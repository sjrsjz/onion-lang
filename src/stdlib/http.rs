use std::{
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinition},
            parameter::LambdaParameter,
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

// 引入所需的辅助函数
use super::{build_dict, wrap_native_function};

// --- Helper functions for robust argument parsing ---

fn get_string_arg(
    arg_map: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<String, RuntimeError> {
    arg_map
        .get(name)
        .ok_or_else(|| {
            RuntimeError::DetailedError(format!("Missing required argument: '{name}'").into())
        })
        .and_then(|obj| match obj.weak() {
            OnionObject::String(s) => Ok(s.to_string()),
            _ => Err(RuntimeError::InvalidType(
                format!("Argument '{name}' must be a string").into(),
            )),
        })
}

fn get_optional_string_arg(
    arg_map: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<Option<String>, RuntimeError> {
    match arg_map.get(name) {
        Some(obj) => match obj.weak() {
            OnionObject::String(s) => Ok(Some(s.to_string())),
            OnionObject::Null | OnionObject::Undefined(_) => Ok(None),
            _ => Err(RuntimeError::InvalidType(
                format!("Argument '{name}' must be a string, null, or undefined").into(),
            )),
        },
        None => Ok(None),
    }
}

fn get_headers_arg(
    arg_map: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<IndexMap<String, String>, RuntimeError> {
    match arg_map.get(name) {
        Some(obj) => match obj.weak() {
            OnionObject::Tuple(tuple) => {
                let mut headers = IndexMap::new();
                for item in tuple.get_elements() {
                    match item {
                        OnionObject::Pair(p) => {
                            let key = p.get_key().to_string(&vec![])?;
                            let value = p.get_value().to_string(&vec![])?;
                            headers.insert(key, value);
                        }
                        _ => {
                            return Err(RuntimeError::InvalidType(
                                "Header elements must be key-value pairs".into(),
                            ));
                        }
                    }
                }
                Ok(headers)
            }
            OnionObject::Null | OnionObject::Undefined(_) => Ok(IndexMap::new()),
            _ => Err(RuntimeError::InvalidType(
                format!("Argument '{name}' must be a tuple of pairs, null, or undefined").into(),
            )),
        },
        None => Ok(IndexMap::new()),
    }
}

#[derive(Debug, Clone)]
enum RequestState {
    Pending,
    InProgress,
    Completed(Result<String, String>),
}

#[derive(Clone)]
pub struct AsyncHttpRequest {
    url: String,
    method: String,
    headers: IndexMap<String, String>,
    body: Option<String>,
    state: Arc<Mutex<RequestState>>,
}

impl AsyncHttpRequest {
    pub fn new(
        url: String,
        method: String,
        headers: IndexMap<String, String>,
        body: Option<String>,
    ) -> Self {
        Self {
            url,
            method,
            headers,
            body,
            state: Arc::new(Mutex::new(RequestState::Pending)),
        }
    }

    fn start_request(&self) {
        let url = self.url.clone();
        let method = self.method.clone();
        let headers = self.headers.clone();
        let body = self.body.clone();
        let state = Arc::clone(&self.state);

        thread::spawn(move || {
            {
                let mut state_guard = state.lock().unwrap();
                *state_guard = RequestState::InProgress;
            }
            let result = Self::perform_http_request(&url, &method, &headers, body.as_deref());
            {
                let mut state_guard = state.lock().unwrap();
                *state_guard = RequestState::Completed(result);
            }
        });
    }

    fn perform_http_request(
        url: &str,
        method: &str,
        headers: &IndexMap<String, String>,
        body: Option<&str>,
    ) -> Result<String, String> {
        let rt = match tokio::runtime::Runtime::new() {
            Ok(rt) => rt,
            Err(e) => return Err(format!("Failed to create async runtime: {e}")),
        };

        rt.block_on(async {
            let client = reqwest::Client::new();
            let mut request_builder = match method.to_uppercase().as_str() {
                "GET" => client.get(url),
                "POST" => client.post(url),
                "PUT" => client.put(url),
                "DELETE" => client.delete(url),
                "PATCH" => client.patch(url),
                "HEAD" => client.head(url),
                _ => return Err(format!("Unsupported HTTP method: {method}")),
            };

            if let Some(body_data) = body {
                request_builder = request_builder.body(body_data.to_string());
            }

            for (key, value) in headers {
                request_builder = request_builder.header(key, value);
            }

            request_builder = request_builder.timeout(Duration::from_secs(30));

            match request_builder.send().await {
                Ok(response) => {
                    let status = response.status();
                    let response_body = response
                        .text()
                        .await
                        .map_err(|e| format!("Failed to read response body: {e}"))?;
                    let response_json = serde_json::json!({
                        "status_code": status.as_u16(),
                        "status_text": status.canonical_reason().unwrap_or("Unknown"),
                        "body": response_body,
                        "success": status.is_success(),
                    });
                    Ok(response_json.to_string())
                }
                Err(e) => {
                    if e.is_timeout() {
                        Err("Request timeout".to_string())
                    } else if e.is_connect() {
                        Err(format!("Connection error: {e}"))
                    } else {
                        Err(format!("HTTP request failed: {e}"))
                    }
                }
            }
        })
    }
}

impl Runnable for AsyncHttpRequest {
    fn step(&mut self, _gc: &mut GC<OnionObjectCell>) -> StepResult {
        let state = self.state.lock().unwrap().clone();
        match state {
            RequestState::Pending => {
                self.start_request();
                StepResult::Continue
            }
            RequestState::InProgress => StepResult::Continue,
            RequestState::Completed(result) => match result {
                Ok(response) => {
                    StepResult::Return(OnionObject::String(response.into()).stabilize().into())
                }
                Err(error) => {
                    let response_json = serde_json::json!({
                        "status_code": 500, // Generic server error
                        "status_text": "ClientError",
                        "body": error,
                        "success": false,
                    });
                    StepResult::Return(
                        OnionObject::String(response_json.to_string().into())
                            .stabilize()
                            .into(),
                    )
                }
            },
        }
    }

    fn receive(
        &mut self,
        _step_result: &StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Ok(())
    }

    fn format_context(&self) -> String {
        let state = self.state.lock().unwrap().clone();
        let headers_str = self
            .headers
            .iter()
            .map(|(k, v)| format!("    - {k}: {v}"))
            .collect::<Vec<String>>()
            .join("\n");
        let body_info = self
            .body
            .as_ref()
            .map_or("No".to_string(), |b| format!("Yes ({} bytes)", b.len()));
        format!(
            "-> Performing Async HTTP Request:\n   - State: {:?}\n   - Method: {}\n   - URL: {}\n   - Body Sent: {}\n   - Headers:\n{}",
            state,
            self.method,
            self.url,
            body_info,
            if headers_str.is_empty() {
                "    (None)".to_string()
            } else {
                headers_str
            }
        )
    }
}

fn http_request(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = get_string_arg(argument, "url")?;
    let method = get_optional_string_arg(argument, "method")?.unwrap_or_else(|| "GET".to_string());
    let headers = get_headers_arg(argument, "headers")?;
    let body = get_optional_string_arg(argument, "body")?;

    let request = AsyncHttpRequest::new(url, method, headers, body);
    let lambda_body = LambdaBody::NativeFunction((
        Arc::new(move |_, _, _, _| Box::new(request.clone())),
        OnionKeyPool::create(vec![]),
    ));

    Ok(OnionLambdaDefinition::new_static(
        LambdaParameter::Multiple([].into()),
        lambda_body,
        OnionFastMap::default(),
        "http::async_request".into(),
        LambdaType::Normal,
    ))
}

fn http_get(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = get_string_arg(argument, "url")?;
    let request = AsyncHttpRequest::new(url, "GET".to_string(), IndexMap::new(), None);
    let lambda_body = LambdaBody::NativeFunction((
        Arc::new(move |_, _, _, _| Box::new(request.clone())),
        OnionKeyPool::create(vec![]),
    ));
    Ok(OnionLambdaDefinition::new_static(
        LambdaParameter::Multiple([].into()),
        lambda_body,
        OnionFastMap::default(),
        "http::async_get".into(),
        LambdaType::Normal,
    ))
}

fn http_post(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = get_string_arg(argument, "url")?;
    let body = get_optional_string_arg(argument, "body")?;
    let request = AsyncHttpRequest::new(url, "POST".to_string(), IndexMap::new(), body);
    let lambda_body = LambdaBody::NativeFunction((
        Arc::new(move |_, _, _, _| Box::new(request.clone())),
        OnionKeyPool::create(vec![]),
    ));
    Ok(OnionLambdaDefinition::new_static(
        LambdaParameter::Multiple([].into()),
        lambda_body,
        OnionFastMap::default(),
        "http::async_post".into(),
        LambdaType::Normal,
    ))
}

fn http_put(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = get_string_arg(argument, "url")?;
    let body = get_optional_string_arg(argument, "body")?;
    let request = AsyncHttpRequest::new(url, "PUT".to_string(), IndexMap::new(), body);
    let lambda_body = LambdaBody::NativeFunction((
        Arc::new(move |_, _, _, _| Box::new(request.clone())),
        OnionKeyPool::create(vec![]),
    ));
    Ok(OnionLambdaDefinition::new_static(
        LambdaParameter::Multiple([].into()),
        lambda_body,
        OnionFastMap::default(),
        "http::async_put".into(),
        LambdaType::Normal,
    ))
}

fn http_delete(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = get_string_arg(argument, "url")?;
    let request = AsyncHttpRequest::new(url, "DELETE".to_string(), IndexMap::new(), None);
    let lambda_body = LambdaBody::NativeFunction((
        Arc::new(move |_, _, _, _| Box::new(request.clone())),
        OnionKeyPool::create(vec![]),
    ));
    Ok(OnionLambdaDefinition::new_static(
        LambdaParameter::Multiple([].into()),
        lambda_body,
        OnionFastMap::default(),
        "http::async_delete".into(),
        LambdaType::Normal,
    ))
}

fn http_patch(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = get_string_arg(argument, "url")?;
    let body = get_optional_string_arg(argument, "body")?;
    let request = AsyncHttpRequest::new(url, "PATCH".to_string(), IndexMap::new(), body);
    let lambda_body = LambdaBody::NativeFunction((
        Arc::new(move |_, _, _, _| Box::new(request.clone())),
        OnionKeyPool::create(vec![]),
    ));
    Ok(OnionLambdaDefinition::new_static(
        LambdaParameter::Multiple([].into()),
        lambda_body,
        OnionFastMap::default(),
        "http::async_patch".into(),
        LambdaType::Normal,
    ))
}

fn http_get_sync(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = get_string_arg(argument, "url")?;
    let result = AsyncHttpRequest::perform_http_request(&url, "GET", &IndexMap::new(), None);
    Ok(OnionObject::String(result.unwrap_or_else(|e| e).into()).stabilize())
}

fn http_post_sync(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = get_string_arg(argument, "url")?;
    let body = get_optional_string_arg(argument, "body")?;
    let result =
        AsyncHttpRequest::perform_http_request(&url, "POST", &IndexMap::new(), body.as_deref());
    Ok(OnionObject::String(result.unwrap_or_else(|e| e).into()).stabilize())
}

/// 构建HTTP模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // --- Async Functions ---
    module.insert(
        "get".to_string(),
        wrap_native_function(
            LambdaParameter::top("url"),
            OnionFastMap::default(),
            "http::get",
            OnionKeyPool::create(vec!["url".into()]),
            &http_get,
        ),
    );
    module.insert(
        "post".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("url"), LambdaParameter::top("body")].into(),
            ),
            OnionFastMap::default(),
            "http::post",
            OnionKeyPool::create(vec!["url".into(), "body".into()]),
            &http_post,
        ),
    );
    module.insert(
        "put".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("url"), LambdaParameter::top("body")].into(),
            ),
            OnionFastMap::default(),
            "http::put",
            OnionKeyPool::create(vec!["url".into(), "body".into()]),
            &http_put,
        ),
    );
    module.insert(
        "delete".to_string(),
        wrap_native_function(
            LambdaParameter::top("url"),
            OnionFastMap::default(),
            "http::delete",
            OnionKeyPool::create(vec!["url".into()]),
            &http_delete,
        ),
    );
    module.insert(
        "patch".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("url"), LambdaParameter::top("body")].into(),
            ),
            OnionFastMap::default(),
            "http::patch",
            OnionKeyPool::create(vec!["url".into(), "body".into()]),
            &http_patch,
        ),
    );

    // --- Sync Functions (for testing/simple cases) ---
    module.insert(
        "get_sync".to_string(),
        wrap_native_function(
            LambdaParameter::top("url"),
            OnionFastMap::default(),
            "http::get_sync",
            OnionKeyPool::create(vec!["url".into()]),
            &http_get_sync,
        ),
    );
    module.insert(
        "post_sync".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("url"), LambdaParameter::top("body")].into(),
            ),
            OnionFastMap::default(),
            "http::post_sync",
            OnionKeyPool::create(vec!["url".into(), "body".into()]),
            &http_post_sync,
        ),
    );

    // --- Full Request Function ---
    module.insert(
        "request".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("url"),
                    LambdaParameter::top("method"),
                    LambdaParameter::top("headers"),
                    LambdaParameter::top("body"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "http::request",
            OnionKeyPool::create(vec![
                "url".into(),
                "method".into(),
                "headers".into(),
                "body".into(),
            ]),
            &http_request,
        ),
    );

    build_dict(module)
}
