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
        object::{ObjectError, OnionObject, OnionStaticObject},
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
        _headers: &HashMap<String, String>,
        body: Option<&str>,
    ) -> Result<String, String> {
        // 这里是一个模拟的HTTP请求实现
        // 在实际应用中，这里应该使用真正的HTTP客户端库

        // 模拟网络延迟
        thread::sleep(Duration::from_millis(100 + (url.len() % 1000) as u64));

        // 简单的URL验证
        if !url.starts_with("http://") && !url.starts_with("https://") {
            return Err("Invalid URL: must start with http:// or https://".to_string());
        }

        // 根据方法和URL模拟不同的响应
        match method.to_uppercase().as_str() {
            "GET" => Ok(format!(
                r#"{{"status": "success", "method": "GET", "url": "{}", "data": "Mock GET response"}}"#,
                url
            )),
            "POST" => {
                let response_body = body.unwrap_or("{}");
                Ok(format!(
                    r#"{{"status": "success", "method": "POST", "url": "{}", "request_body": {}, "data": "Mock POST response"}}"#,
                    url, response_body
                ))
            }
            "PUT" => {
                let response_body = body.unwrap_or("{}");
                Ok(format!(
                    r#"{{"status": "success", "method": "PUT", "url": "{}", "request_body": {}, "data": "Mock PUT response"}}"#,
                    url, response_body
                ))
            }
            "DELETE" => Ok(format!(
                r#"{{"status": "success", "method": "DELETE", "url": "{}", "data": "Mock DELETE response"}}"#,
                url
            )),
            _ => Err(format!("Unsupported HTTP method: {}", method)),
        }
    }
}

impl Runnable for AsyncHttpRequest {
    fn set_argument(
        &mut self,
        _argument: OnionStaticObject,
        _gc: &mut GC<OnionObject>,
    ) -> Result<(), ObjectError> {
        Ok(())
    }

    fn step(&mut self, _gc: &mut GC<OnionObject>) -> Result<StepResult, RuntimeError> {
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
        _gc: &mut GC<OnionObject>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "AsyncHttpRequest does not support receive".to_string(),
        ))
    }

    fn copy(&self, _gc: &mut GC<OnionObject>) -> Box<dyn Runnable> {
        Box::new(AsyncHttpRequest {
            url: self.url.clone(),
            method: self.method.clone(),
            headers: self.headers.clone(),
            body: self.body.clone(),
            state: Arc::clone(&self.state),
        })
    }
}

/// 解析请求参数并创建HTTP请求
fn parse_request_params(
    argument: &OnionStaticObject,
) -> Result<(String, String, HashMap<String, String>, Option<String>), ObjectError> {
    argument.weak().with_data(|data| {
        let _request_obj = unwrap_object!(data, OnionObject::Tuple)?;

        // 获取URL
        let url = get_attr_direct(data, "url".to_string())?
            .weak()
            .to_string()
            .map_err(|e| ObjectError::InvalidType(format!("Invalid URL: {}", e)))?;

        // 获取方法，默认为GET
        let method = get_attr_direct(data, "method".to_string())
            .unwrap_or_else(|_| OnionObject::String("GET".to_string()).stabilize())
            .weak()
            .to_string()
            .unwrap_or_else(|_| "GET".to_string());

        // 获取headers，如果有的话
        let mut headers = HashMap::new();
        if let Ok(headers_obj) = get_attr_direct(data, "headers".to_string()) {
            if let OnionObject::Tuple(headers_tuple) = headers_obj.weak() {
                for element in &headers_tuple.elements {
                    if let OnionObject::Named(named) = element {
                        let key = named.get_key().to_string().unwrap_or_default();
                        let value = named.get_value().to_string().unwrap_or_default();
                        headers.insert(key, value);
                    }
                }
            }
        }

        // 获取body，如果有的话
        let body = get_attr_direct(data, "body".to_string())
            .ok()
            .and_then(|body_obj| body_obj.weak().to_string().ok());

        Ok((url, method, headers, body))
    })
}

/// 创建异步HTTP GET请求
fn http_get(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    let url = argument
        .weak()
        .with_data(|data| {
            get_attr_direct(data, "url".to_string())?
                .weak()
                .to_string()
                .map_err(|e| ObjectError::InvalidType(format!("Invalid URL: {}", e)))
        })
        .map_err(RuntimeError::ObjectError)?;

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
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    let (url, body) = argument
        .weak()
        .with_data(|data| {
            let url = get_attr_direct(data, "url".to_string())?
                .weak()
                .to_string()
                .map_err(|e| ObjectError::InvalidType(format!("Invalid URL: {}", e)))?;

            let body = get_attr_direct(data, "body".to_string())
                .ok()
                .and_then(|body_obj| body_obj.weak().to_string().ok());

            Ok((url, body))
        })
        .map_err(RuntimeError::ObjectError)?;

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

/// 创建通用的异步HTTP请求
fn http_request(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    let (url, method, headers, body) =
        parse_request_params(&argument).map_err(RuntimeError::ObjectError)?;

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
