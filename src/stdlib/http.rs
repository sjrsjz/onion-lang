use std::{
    cell::RefCell,
    collections::HashMap,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use onion_vm::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    onion_tuple,
    types::{
        lambda::definition::{LambdaBody, OnionLambdaDefinition},
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    unwrap_object, GC,
};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

/// HTTP请求的状态
#[derive(Debug, Clone)]
enum RequestState {
    Pending,
    InProgress,
    Completed(Result<String, String>),
}

/// 异步HTTP请求实现
pub struct AsyncHttpRequest {
    url: String,
    method: String,
    headers: HashMap<String, String>,
    body: Option<String>,
    state: Arc<Mutex<RequestState>>,
}

impl AsyncHttpRequest {
    pub fn new(
        url: String,
        method: String,
        headers: HashMap<String, String>,
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
            // 将状态设置为进行中
            {
                let mut state_guard = state.lock().unwrap();
                *state_guard = RequestState::InProgress;
            }

            // 执行HTTP请求（这里是一个简化的实现，在实际中应该使用真正的HTTP客户端）
            let result = Self::perform_http_request(&url, &method, &headers, body.as_deref());

            // 更新状态为完成
            {
                let mut state_guard = state.lock().unwrap();
                *state_guard = RequestState::Completed(result);
            }
        });
    }
    fn perform_http_request(
        url: &str,
        method: &str,
        headers: &HashMap<String, String>,
        body: Option<&str>,
    ) -> Result<String, String> {
        // 创建Tokio runtime来处理异步请求
        let rt = match tokio::runtime::Runtime::new() {
            Ok(rt) => rt,
            Err(e) => return Err(format!("Failed to create async runtime: {}", e)),
        };

        // 在异步runtime中执行HTTP请求
        rt.block_on(async {
            let client = reqwest::Client::new();

            // 构建请求
            let request_builder = match method.to_uppercase().as_str() {
                "GET" => client.get(url),
                "POST" => {
                    let mut builder = client.post(url);
                    if let Some(body_data) = body {
                        builder = builder.body(body_data.to_string());
                    }
                    builder
                }
                "PUT" => {
                    let mut builder = client.put(url);
                    if let Some(body_data) = body {
                        builder = builder.body(body_data.to_string());
                    }
                    builder
                }
                "DELETE" => client.delete(url),
                "PATCH" => {
                    let mut builder = client.patch(url);
                    if let Some(body_data) = body {
                        builder = builder.body(body_data.to_string());
                    }
                    builder
                }
                "HEAD" => client.head(url),
                _ => return Err(format!("Unsupported HTTP method: {}", method)),
            };

            // 添加headers
            let mut final_builder = request_builder;
            for (key, value) in headers {
                final_builder = final_builder.header(key, value);
            }

            // 设置超时时间
            final_builder = final_builder.timeout(Duration::from_secs(30));

            // 执行请求
            match final_builder.send().await {
                Ok(response) => {
                    let status = response.status();
                    let status_code = status.as_u16();

                    match response.text().await {
                        Ok(text) => {
                            // 构建包含状态码和响应体的JSON响应
                            let response_json = serde_json::json!({
                                "status_code": status_code,
                                "status": status.canonical_reason().unwrap_or("Unknown"),
                                "method": method.to_uppercase(),
                                "url": url,
                                "body": text,
                                "success": status.is_success()
                            });
                            Ok(response_json.to_string())
                        }
                        Err(e) => Err(format!("Failed to read response body: {}", e)),
                    }
                }
                Err(e) => {
                    // 处理不同类型的错误
                    if e.is_timeout() {
                        Err("Request timeout".to_string())
                    } else if e.is_connect() {
                        Err(format!("Connection error: {}", e))
                    } else if e.is_request() {
                        Err(format!("Request error: {}", e))
                    } else {
                        Err(format!("HTTP request failed: {}", e))
                    }
                }
            }
        })
    }
}

impl Runnable for AsyncHttpRequest {
    fn set_argument(
        &mut self,
        _argument: OnionStaticObject,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Ok(())
    }

    fn step(&mut self, _gc: &mut GC<OnionObjectCell>) -> Result<StepResult, RuntimeError> {
        let state = {
            let state_guard = self.state.lock().unwrap();
            state_guard.clone()
        };

        match state {
            RequestState::Pending => {
                // 启动请求
                self.start_request();
                Ok(StepResult::Continue)
            }
            RequestState::InProgress => {
                // 请求正在进行中，继续等待
                Ok(StepResult::Continue)
            }
            RequestState::Completed(result) => {
                // 请求完成，返回结果
                match result {
                    Ok(response) => {
                        let response_obj = OnionObject::String(response).stabilize();
                        Ok(StepResult::Return(response_obj))
                    }
                    Err(error) => {
                        let error_obj =
                            OnionObject::String(format!("HTTP Error: {}", error)).stabilize();
                        Ok(StepResult::Return(error_obj))
                    }
                }
            }
        }
    }

    fn receive(
        &mut self,
        _step_result: StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "AsyncHttpRequest does not support receive".to_string(),
        ))
    }

    fn copy(&self, _gc: &mut GC<OnionObjectCell>) -> Box<dyn Runnable> {
        Box::new(AsyncHttpRequest {
            url: self.url.clone(),
            method: self.method.clone(),
            headers: self.headers.clone(),
            body: self.body.clone(),
            state: Arc::clone(&self.state),
        })
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        let state = {
            let state_guard = self.state.lock().unwrap();
            match &*state_guard {
                RequestState::Pending => "Pending",
                RequestState::InProgress => "In Progress",
                RequestState::Completed(_) => "Completed",
            }
        };

        Ok(serde_json::json!({
            "url": self.url,
            "method": self.method,
            "headers": self.headers,
            "body": self.body,
            "state": state,
        }))
    }
}

/// 解析请求参数并创建HTTP请求
fn parse_request_params(
    argument: &OnionStaticObject,
) -> Result<(String, String, HashMap<String, String>, Option<String>), RuntimeError> {
    argument.weak().with_data(|data| {
        let _request_obj = unwrap_object!(data, OnionObject::Tuple)?;

        // 获取URL
        let url = get_attr_direct(data, "url".to_string())?
            .weak()
            .try_borrow()?
            .to_string(&vec![])
            .map_err(|e| RuntimeError::InvalidType(format!("Invalid URL: {}", e)))?;

        // 获取方法，默认为GET
        let method = get_attr_direct(data, "method".to_string())
            .unwrap_or_else(|_| OnionObject::String("GET".to_string()).stabilize())
            .weak()
            .try_borrow()?
            .to_string(&vec![])
            .unwrap_or_else(|_| "GET".to_string());

        // 获取headers，如果有的话
        let mut headers = HashMap::new();
        if let Ok(headers_obj) = get_attr_direct(data, "headers".to_string()) {
            if let OnionObject::Tuple(headers_tuple) = &*headers_obj.weak().try_borrow()? {
                for element in &headers_tuple.elements {
                    if let OnionObject::Named(named) = &*element.try_borrow()? {
                        let key = named
                            .get_key()
                            .try_borrow()?
                            .to_string(&vec![])
                            .unwrap_or_default();
                        let value = named
                            .get_value()
                            .try_borrow()?
                            .to_string(&vec![])
                            .unwrap_or_default();
                        headers.insert(key, value);
                    }
                }
            }
        }

        // 获取body，如果有的话
        let body = get_attr_direct(data, "body".to_string())
            .ok()
            .and_then(|body_obj| body_obj.weak().try_borrow().ok()?.to_string(&vec![]).ok());

        Ok((url, method, headers, body))
    })
}

/// 创建异步HTTP GET请求
fn http_get(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = argument.weak().with_data(|data| {
        get_attr_direct(data, "url".to_string())?
            .weak()
            .try_borrow()?
            .to_string(&vec![])
            .map_err(|e| RuntimeError::InvalidType(format!("Invalid URL: {}", e)))
    })?;

    let headers = HashMap::new();
    let request = AsyncHttpRequest::new(url, "GET".to_string(), headers, None);

    // 将调度器包装成Lambda返回
    let lambda_body = LambdaBody::NativeFunction(Arc::new(RefCell::new(request)));
    let lambda_def = OnionLambdaDefinition::new_static(
        &onion_tuple!(),
        lambda_body,
        None,
        None,
        "http::async_get".to_string(),
    );

    Ok(lambda_def)
}

/// 创建异步HTTP POST请求
fn http_post(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let (url, body) = argument.weak().with_data(|data| {
        let url = get_attr_direct(data, "url".to_string())?
            .weak()
            .try_borrow()?
            .to_string(&vec![])
            .map_err(|e| RuntimeError::InvalidType(format!("Invalid URL: {}", e)))?;

        let body = get_attr_direct(data, "body".to_string())
            .ok()
            .and_then(|body_obj| body_obj.weak().try_borrow().ok()?.to_string(&vec![]).ok());

        Ok((url, body))
    })?;

    let headers = HashMap::new();
    let request = AsyncHttpRequest::new(url, "POST".to_string(), headers, body);
    // 将调度器包装成Lambda返回
    let lambda_body = LambdaBody::NativeFunction(Arc::new(RefCell::new(request)));
    let lambda_def = OnionLambdaDefinition::new_static(
        &onion_tuple!(),
        lambda_body,
        None,
        None,
        "http::async_post".to_string(),
    );

    Ok(lambda_def)
}

/// 创建异步HTTP PUT请求
fn http_put(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let (url, body) = argument.weak().with_data(|data| {
        let url = get_attr_direct(data, "url".to_string())?
            .weak()
            .try_borrow()?
            .to_string(&vec![])
            .map_err(|e| RuntimeError::InvalidType(format!("Invalid URL: {}", e)))?;

        let body = get_attr_direct(data, "body".to_string())
            .ok()
            .and_then(|body_obj| body_obj.weak().try_borrow().ok()?.to_string(&vec![]).ok());

        Ok((url, body))
    })?;

    let headers = HashMap::new();
    let request = AsyncHttpRequest::new(url, "PUT".to_string(), headers, body);

    let lambda_body = LambdaBody::NativeFunction(Arc::new(RefCell::new(request)));
    let lambda_def = OnionLambdaDefinition::new_static(
        &onion_tuple!(),
        lambda_body,
        None,
        None,
        "http::async_put".to_string(),
    );

    Ok(lambda_def)
}

/// 创建异步HTTP DELETE请求
fn http_delete(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = argument.weak().with_data(|data| {
        get_attr_direct(data, "url".to_string())?
            .weak()
            .try_borrow()?
            .to_string(&vec![])
            .map_err(|e| RuntimeError::InvalidType(format!("Invalid URL: {}", e)))
    })?;

    let headers = HashMap::new();
    let request = AsyncHttpRequest::new(url, "DELETE".to_string(), headers, None);

    let lambda_body = LambdaBody::NativeFunction(Arc::new(RefCell::new(request)));
    let lambda_def = OnionLambdaDefinition::new_static(
        &onion_tuple!(),
        lambda_body,
        None,
        None,
        "http::async_delete".to_string(),
    );

    Ok(lambda_def)
}

/// 创建异步HTTP PATCH请求
fn http_patch(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let (url, body) = argument.weak().with_data(|data| {
        let url = get_attr_direct(data, "url".to_string())?
            .weak()
            .try_borrow()?
            .to_string(&vec![])
            .map_err(|e| RuntimeError::InvalidType(format!("Invalid URL: {}", e)))?;

        let body = get_attr_direct(data, "body".to_string())
            .ok()
            .and_then(|body_obj| body_obj.weak().try_borrow().ok()?.to_string(&vec![]).ok());

        Ok((url, body))
    })?;

    let headers = HashMap::new();
    let request = AsyncHttpRequest::new(url, "PATCH".to_string(), headers, body);

    let lambda_body = LambdaBody::NativeFunction(Arc::new(RefCell::new(request)));
    let lambda_def = OnionLambdaDefinition::new_static(
        &onion_tuple!(),
        lambda_body,
        None,
        None,
        "http::async_patch".to_string(),
    );

    Ok(lambda_def)
}
fn http_request(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let (url, method, headers, body) = parse_request_params(&argument)?;

    let request = AsyncHttpRequest::new(url, method, headers, body);

    // 将调度器包装成Lambda返回
    let lambda_body = LambdaBody::NativeFunction(Arc::new(RefCell::new(request)));
    let lambda_def = OnionLambdaDefinition::new_static(
        &onion_tuple!(),
        lambda_body,
        None,
        None,
        "http::async_request".to_string(),
    );

    Ok(lambda_def)
}

/// 创建同步HTTP GET请求（用于简单测试）
fn http_get_sync(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = argument.weak().with_data(|data| {
        get_attr_direct(data, "url".to_string())?
            .weak()
            .try_borrow()?
            .to_string(&vec![])
            .map_err(|e| RuntimeError::InvalidType(format!("Invalid URL: {}", e)))
    })?;

    // 直接执行HTTP请求并返回结果
    let headers = HashMap::new();
    match AsyncHttpRequest::perform_http_request(&url, "GET", &headers, None) {
        Ok(response) => Ok(OnionObject::String(response).stabilize()),
        Err(error) => Ok(OnionObject::String(format!("HTTP Error: {}", error)).stabilize()),
    }
}

/// 创建同步HTTP POST请求（用于简单测试）
fn http_post_sync(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let (url, body) = argument.weak().with_data(|data| {
        let url = get_attr_direct(data, "url".to_string())?
            .weak()
            .try_borrow()?
            .to_string(&vec![])
            .map_err(|e| RuntimeError::InvalidType(format!("Invalid URL: {}", e)))?;

        let body = get_attr_direct(data, "body".to_string())
            .ok()
            .and_then(|body_obj| body_obj.weak().try_borrow().ok()?.to_string(&vec![]).ok());

        Ok((url, body))
    })?;

    let headers = HashMap::new();
    match AsyncHttpRequest::perform_http_request(&url, "POST", &headers, body.as_deref()) {
        Ok(response) => Ok(OnionObject::String(response).stabilize()),
        Err(error) => Ok(OnionObject::String(format!("HTTP Error: {}", error)).stabilize()),
    }
}

/// 构建HTTP模块
pub fn build_module() -> OnionStaticObject {
    let mut module = HashMap::new();

    // GET请求参数
    let mut get_params = HashMap::new();
    get_params.insert(
        "url".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );

    module.insert(
        "get".to_string(),
        wrap_native_function(
            &build_named_dict(get_params),
            None,
            None,
            "http::get".to_string(),
            http_get,
        ),
    );

    // POST请求参数
    let mut post_params = HashMap::new();
    post_params.insert(
        "url".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    post_params.insert(
        "body".to_string(),
        OnionObject::String("{}".to_string()).stabilize(),
    );

    module.insert(
        "post".to_string(),
        wrap_native_function(
            &build_named_dict(post_params),
            None,
            None,
            "http::post".to_string(),
            http_post,
        ),
    );

    // PUT请求参数
    let mut put_params = HashMap::new();
    put_params.insert(
        "url".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    put_params.insert(
        "body".to_string(),
        OnionObject::String("{}".to_string()).stabilize(),
    );

    module.insert(
        "put".to_string(),
        wrap_native_function(
            &build_named_dict(put_params),
            None,
            None,
            "http::put".to_string(),
            http_put,
        ),
    );

    // DELETE请求参数
    let mut delete_params = HashMap::new();
    delete_params.insert(
        "url".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );

    module.insert(
        "delete".to_string(),
        wrap_native_function(
            &build_named_dict(delete_params),
            None,
            None,
            "http::delete".to_string(),
            http_delete,
        ),
    ); // PATCH请求参数
    let mut patch_params = HashMap::new();
    patch_params.insert(
        "url".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    patch_params.insert(
        "body".to_string(),
        OnionObject::String("{}".to_string()).stabilize(),
    );

    module.insert(
        "patch".to_string(),
        wrap_native_function(
            &build_named_dict(patch_params),
            None,
            None,
            "http::patch".to_string(),
            http_patch,
        ),
    );

    // 同步GET请求 (用于测试)
    let mut get_sync_params = HashMap::new();
    get_sync_params.insert(
        "url".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );

    module.insert(
        "get_sync".to_string(),
        wrap_native_function(
            &build_named_dict(get_sync_params),
            None,
            None,
            "http::get_sync".to_string(),
            http_get_sync,
        ),
    );

    // 同步POST请求 (用于测试)
    let mut post_sync_params = HashMap::new();
    post_sync_params.insert(
        "url".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    post_sync_params.insert(
        "body".to_string(),
        OnionObject::String("{}".to_string()).stabilize(),
    );

    module.insert(
        "post_sync".to_string(),
        wrap_native_function(
            &build_named_dict(post_sync_params),
            None,
            None,
            "http::post_sync".to_string(),
            http_post_sync,
        ),
    );

    // 通用请求参数
    let mut request_params = HashMap::new();
    request_params.insert(
        "url".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    request_params.insert(
        "method".to_string(),
        OnionObject::String("GET".to_string()).stabilize(),
    );
    request_params.insert("headers".to_string(), onion_tuple!());
    request_params.insert(
        "body".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );

    module.insert(
        "request".to_string(),
        wrap_native_function(
            &build_named_dict(request_params),
            None,
            None,
            "http::request".to_string(),
            http_request,
        ),
    );

    build_named_dict(module)
}
