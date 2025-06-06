use std::collections::{HashMap, HashSet}; // 添加 HashSet
use std::io::{BufRead, Write};
use std::panic;
use std::sync::{Arc, Mutex}; // 添加 panic 模块

use log::{debug, error, info, warn};
use onion_frontend::utils::cycle_detector;
use serde_json::Value;
use url::Url;

use crate::lsp::semantic::encode_semantic_tokens;
use onion_frontend::dir_stack::DirStack;
use onion_frontend::parser::analyzer::{self, auto_capture_and_rebuild};
use onion_frontend::parser::ast::build_ast;
use onion_frontend::parser::lexer;

use super::capabilities::initialize_capabilities;
use super::document::TextDocument;
use super::protocol::*;
use super::semantic::SemanticTokenTypes;

/// LSP 服务器状态
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ServerState {
    Uninitialized,
    Initializing,
    Initialized,
    ShutDown,
}

/// LSP 服务器数据结构
pub struct LspServer {
    /// 服务器当前状态
    state: ServerState,
    /// 打开的文档映射
    documents: HashMap<String, TextDocument>,
    /// 客户端能力
    client_capabilities: Option<ClientCapabilities>,
    /// 工作空间根目录
    root_uri: Option<String>,
}

impl LspServer {
    /// 创建新的LSP服务器实例
    pub fn new() -> Self {
        Self {
            state: ServerState::Uninitialized,
            documents: HashMap::new(),
            client_capabilities: None,
            root_uri: None,
        }
    }

    /// 处理初始化请求
    pub fn initialize(
        &mut self,
        params: InitializeParams,
    ) -> Result<InitializeResult, ResponseError> {
        if self.state == ServerState::Initialized {
            return Err(ResponseError {
                code: error_codes::INVALID_REQUEST,
                message: "Server is already initialized".to_string(),
                data: None,
            });
        }

        self.state = ServerState::Initializing;
        self.client_capabilities = params.capabilities;
        self.root_uri = params.root_uri;

        Ok(InitializeResult {
            capabilities: initialize_capabilities(),
            server_info: Some(ServerInfo {
                name: "Onion Language Server".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        })
    }

    /// 处理初始化完成通知
    pub fn initialized(&mut self) {
        self.state = ServerState::Initialized;
    }

    /// 处理关闭请求
    pub fn shutdown(&mut self) -> Result<Value, ResponseError> {
        self.state = ServerState::ShutDown;
        Ok(Value::Null)
    }

    /// 处理退出通知
    pub fn exit(&mut self) -> bool {
        self.state == ServerState::ShutDown
    }

    /// 处理文档打开通知
    pub fn did_open(&mut self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let document = TextDocument::new(
            params.text_document.uri,
            params.text_document.language_id,
            params.text_document.version,
            params.text_document.text,
        );
        self.documents.insert(uri.clone(), document);

        // 触发文档验证
        self.validate_document(&uri);
        info!("Document opened: {}", uri);
    }

    /// 处理文档变更通知
    pub fn did_change(&mut self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        info!("文档变更: {} (版本 {})", uri, params.text_document.version);

        // 获取或创建文档
        let document = if let Some(doc) = self.documents.get_mut(&uri) {
            info!("更新现有文档");
            doc
        } else {
            info!("文档不存在，创建新文档: {}", uri);
            // 确定初始内容
            let initial_content = if !params.content_changes.is_empty() {
                match &params.content_changes[0] {
                    TextDocumentContentChangeEvent::Full { text } => text.clone(),
                    TextDocumentContentChangeEvent::Incremental { .. } => String::new(),
                }
            } else {
                String::new()
            };

            let doc = TextDocument::new(
                uri.clone(),
                "onion".to_string(), // 假设语言为 onion
                params.text_document.version,
                initial_content,
            );

            self.documents.insert(uri.clone(), doc);
            self.documents.get_mut(&uri).unwrap()
        };

        // 应用所有变更
        for (i, change) in params.content_changes.iter().enumerate() {
            match change {
                TextDocumentContentChangeEvent::Full { text } => {
                    info!("应用全量变更 #{}: 内容长度 {} 字节", i, text.len());
                    document.update_content(text.clone());
                }
                TextDocumentContentChangeEvent::Incremental { range, text } => {
                    info!(
                        "应用增量变更 #{}: 范围 [{},{}]-[{},{}], 文本长度 {} 字节",
                        i,
                        range.start.line,
                        range.start.character,
                        range.end.line,
                        range.end.character,
                        text.len()
                    );
                    document.apply_incremental_change(range.clone(), text.clone());
                }
            }
        }

        // 更新文档版本
        let old_version = document.version;
        document.version = params.text_document.version;
        info!("文档版本从 {} 更新到 {}", old_version, document.version);

        // 打印当前文档内容 (调试用)
        debug!(
            "更新后的文档内容 (前100字符): {}",
            &document.content.chars().take(100).collect::<String>()
        );
    }
    /// 处理文档关闭通知
    pub fn did_close(&mut self, params: DidCloseTextDocumentParams) {
        info!("Document closed: {}", params.text_document.uri);
        let uri = params.text_document.uri.clone();
        if self.documents.remove(&uri).is_none() {
            warn!("Tried to close non-existent document: {}", uri);
        }
    }

    // 验证文档并发送诊断消息
    fn validate_document(
        &self,
        uri: &str,
    ) -> Option<(Vec<Diagnostic>, Option<Vec<SemanticTokenTypes>>)> {
        info!("验证文档: {} 开始", uri);
        if let Some(document) = self.documents.get(uri) {
            info!(
                "找到文档，版本 {}, 内容长度 {} 字节",
                document.version,
                document.content.len()
            );

            // 生成诊断信息和语义着色
            match std::panic::catch_unwind(|| super::diagnostics::validate_document(document)) {
                Ok((diagnostics, semantic_tokens)) => {
                    info!("诊断完成: 生成了 {} 个诊断信息", diagnostics.len());
                    if let Some(tokens) = &semantic_tokens {
                        info!("语义着色: 生成了 {} 个标记", tokens.len());
                    } else {
                        info!("语义着色: 未能生成标记");
                    }
                    Some((diagnostics, semantic_tokens))
                }
                Err(e) => {
                    error!("诊断过程中发生崩溃: {:?}", e);
                    Some((vec![], None)) // 返回空诊断列表和无语义标记
                }
            }
        } else {
            warn!("找不到要验证的文档: {}", uri);
            None
        }
    }
    /// 处理自动完成请求
    pub fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<CompletionResponse, ResponseError> {
        let uri = params.text_document.uri.clone();

        if let Some(document) = self.documents.get(&uri) {
            // 从文档内容和位置计算补全项
            let items = self.calculate_completions(document, params.position);

            Ok(CompletionResponse::List(items))
        } else {
            Err(ResponseError {
                code: error_codes::INVALID_PARAMS,
                message: format!("Document not found: {}", uri),
                data: None,
            })
        }
    }

    /// 计算给定位置的补全项
    fn calculate_completions(
        &self,
        document: &TextDocument,
        position: Position,
    ) -> Vec<CompletionItem> {
        // 基于Onion语法，提供关键字、运算符等的完成项
        let mut items = Vec::new();
        let mut unique_vars = HashSet::new(); // 用于存储唯一的变量名

        // 添加Onion关键字
        let keywords = vec![
            "if",
            "else",
            "while",
            "return",
            "break",
            "continue",
            "and",
            "or",
            "not",
            "in",
            "async",
            "await",
            "emit",
            "deepcopy",
            "import",
            "keyof",
            "valueof",
            "typeof",
            "dyn",
            "copy",
            "mut",
            "const",
            "assert",
            "xor",
            "lengthof",
            "arguments",
            "is",
            "sync",
        ];
        for keyword in keywords {
            items.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::Keyword),
                detail: Some("Onion keyword".to_string()),
                documentation: None,
                insert_text: Some(keyword.to_string()),
                other: HashMap::new(),
            });
        }

        // 添加常量
        let constants = vec!["true", "false", "null", "undefined"];
        for constant in constants {
            items.push(CompletionItem {
                label: constant.to_string(),
                kind: Some(CompletionItemKind::Constant), // 使用 Constant 类型
                detail: Some("Onion constant".to_string()),
                documentation: None,
                insert_text: Some(constant.to_string()),
                other: HashMap::new(),
            });
        }

        let annotations = vec![
            "dynamic", "static", "compile", "required", "macro", "import",
        ];
        for annotation in annotations {
            items.push(CompletionItem {
                label: annotation.to_string(),
                kind: Some(CompletionItemKind::Enum),
                detail: Some("Onion annotation".to_string()),
                documentation: None,
                insert_text: Some(annotation.to_string()),
                other: HashMap::new(),
            });
        }

        // --- 使用分析器获取变量上下文 ---
        // 1. 解析 AST
        let lex_result = lexer::lexer::tokenize(&document.content);
        let lex_result = lexer::lexer::reject_comment(&lex_result);
        let parse_result = build_ast(&lex_result);
        if let Ok(ast) = parse_result {
            let ast = auto_capture_and_rebuild(&ast).1;
            // 2. 将 LSP Position 转换为字节偏移
            if let Some(byte_offset) = position_to_byte_offset(&document.content, position.clone())
            {
                // 1. 解析 URI
                let file_path = match Url::parse(&document.uri) {
                    Ok(url) if url.scheme() == "file" => {
                        match url.to_file_path() {
                            Ok(path) => path,
                            Err(_) => {
                                error!("Failed to convert URI to file path: {}", document.uri);
                                return vec![]; // 返回空列表
                            }
                        }
                    }
                    _ => {
                        error!("Invalid URI scheme: {}", document.uri);
                        return vec![]; // 返回空列表
                    }
                };

                // 2. 获取父目录
                let parent_dir = match file_path.parent() {
                    Some(dir) => dir.to_path_buf(),
                    None => {
                        error!(
                            "Failed to get parent directory for file: {}",
                            file_path.display()
                        );
                        return vec![]; // 返回空列表
                    }
                };
                let mut dir_stack = DirStack::new(Some(&parent_dir)).unwrap();

                let mut cycle_detector = cycle_detector::CycleDetector::new();
                let mut visit_result = match cycle_detector.visit(
                    match dir_stack.get_absolute_path(match file_path.to_str() {
                        Some(path) => path,
                        None => {
                            error!(
                                "Failed to convert file path to string: {}",
                                file_path.display()
                            );
                            return vec![]; // 返回空列表
                        }
                    }) {
                        Ok(path) => path.to_str().unwrap_or("").to_string(),
                        Err(e) => {
                            error!("Failed to get absolute path: {}", e);
                            return vec![]; // 返回空列表
                        }
                    }
                    .to_string(),
                ) {
                    Ok(result) => result,
                    Err(e) => {
                        error!("Cycle detection failed: {}", e);
                        return vec![]; // 返回空列表
                    }
                };

                let macro_result =
                    analyzer::expand_macro(&ast, visit_result.get_detector_mut(), &mut dir_stack);
                if !macro_result.errors.is_empty() {
                    error!(
                        "Macro expansion errors: {}",
                        macro_result
                            .errors
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
                if !macro_result.warnings.is_empty() {
                    warn!(
                        "Macro expansion warnings: {}",
                        macro_result
                            .warnings
                            .iter()
                            .map(|w| w.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }

                // 3. 调用分析器获取特定位置的上下文
                let analysis_output = analyzer::analyze_ast(
                    &macro_result.result_node,
                    Some(byte_offset),
                    visit_result.get_detector_mut(),
                    &mut dir_stack,
                );

                // 4. 如果分析器在断点处捕获了上下文，则提取变量
                if let Some(context) = analysis_output.context_at_break {
                    // 从内到外遍历作用域帧
                    for context in context.all_contexts().iter().rev() {
                        for frame in context.iter().rev() {
                            for var in &frame.variables {
                                // 添加到 HashSet 以确保唯一性
                                if unique_vars.insert(var.name.clone()) {
                                    // 根据变量类型确定 CompletionItemKind 和 detail
                                    let (kind, detail) = match var.assumed_type {
                                        analyzer::AssumedType::Lambda => (
                                            CompletionItemKind::Function,
                                            format!("Function: {}", var.name),
                                        ),
                                        analyzer::AssumedType::String => (
                                            CompletionItemKind::Variable,
                                            format!("String: {}", var.name),
                                        ),
                                        analyzer::AssumedType::Number => (
                                            CompletionItemKind::Variable,
                                            format!("Number: {}", var.name),
                                        ),
                                        analyzer::AssumedType::Boolean => (
                                            CompletionItemKind::Variable,
                                            format!("Boolean: {}", var.name),
                                        ),
                                        analyzer::AssumedType::Base64 => (
                                            CompletionItemKind::Variable,
                                            format!("Base64: {}", var.name),
                                        ),
                                        analyzer::AssumedType::Null => (
                                            CompletionItemKind::Variable,
                                            format!("Null: {}", var.name),
                                        ),
                                        analyzer::AssumedType::Undefined => (
                                            CompletionItemKind::Variable,
                                            format!("Undefined: {}", var.name),
                                        ),
                                        analyzer::AssumedType::Tuple => (
                                            CompletionItemKind::Class,
                                            format!("Tuple: {}", var.name),
                                        ),
                                        analyzer::AssumedType::KeyVal => (
                                            CompletionItemKind::Variable,
                                            format!("KeyValue: {}", var.name),
                                        ),
                                        analyzer::AssumedType::NamedArgument => (
                                            CompletionItemKind::Interface,
                                            format!("NamedArgument: {}", var.name),
                                        ),
                                        // 可以为其他类型添加更多分支
                                        _ => (
                                            CompletionItemKind::Variable,
                                            format!(
                                                "Variable: {} (type: {:?})",
                                                var.name, var.assumed_type
                                            ),
                                        ),
                                    };

                                    items.push(CompletionItem {
                                        label: var.name.clone(),
                                        kind: Some(kind),
                                        detail: Some(detail),
                                        documentation: None, // 可以考虑未来添加基于类型的文档
                                        insert_text: Some(var.name.clone()), // 对于函数，可能需要添加 '()'
                                        other: HashMap::new(),
                                    });
                                }
                            }
                        }
                    }
                } else {
                    info!(
                        "Analyzer did not capture context at break position {}",
                        byte_offset
                    );
                }
            } else {
                warn!("Could not convert position {:?} to byte offset", position);
            }
        } else {
            warn!("Failed to parse document for completion: {}", document.uri);
            // 解析失败，仅返回关键字和内置函数
        }

        items
    }
}

/// 将 LSP 的 Position (0-based line, 0-based UTF-16 character offset) 转换为字节偏移量
fn position_to_byte_offset(text: &str, position: Position) -> Option<usize> {
    let mut byte_offset = 0;
    let mut current_line = 0;

    for line in text.lines() {
        if current_line == position.line {
            // 找到目标行，计算字符偏移对应的字节偏移
            let mut char_offset = 0;
            for (i, ch) in line.char_indices() {
                if char_offset == position.character {
                    return Some(byte_offset + i);
                }
                // LSP 使用 UTF-16 编码单位计数，Rust 字符串迭代器使用 Unicode 标量值 (char)
                // 对于 BMP 字符，两者长度相同。对于代理对，UTF-16 长度为 2，char 长度为 1。
                // 这里简化处理，假设大部分是 BMP 字符，或者不严格要求精确处理代理对。
                // 更精确的方法需要处理 UTF-16 编码。
                char_offset += ch.len_utf16() as u32; // 使用 UTF-16 长度计数
                if char_offset > position.character {
                    // 如果超过了目标字符偏移，说明位置在字符内部或无效，返回当前字节偏移
                    return Some(byte_offset + i);
                }
            }
            // 如果循环结束还没找到，说明位置在行尾
            return Some(byte_offset + line.len());
        }
        // 加上行内容和换行符的字节长度
        // 需要处理 \n 和 \r\n 两种情况
        byte_offset += line.len();
        if text[byte_offset..].starts_with("\r\n") {
            byte_offset += 2;
        } else if text[byte_offset..].starts_with('\n') {
            byte_offset += 1;
        } else {
            // 文件末尾没有换行符
            if current_line == position.line {
                // 如果是最后一行且位置在行尾
                return Some(byte_offset);
            }
            // 否则可能 position.line 超出范围
            break;
        }
        current_line += 1;
    }

    // 如果行号超出范围，或者文本为空
    if current_line == position.line && position.character == 0 {
        return Some(byte_offset); // 可能是空文件或最后一行之后的位置
    }

    None // 位置无效
}

/// 运行LSP服务器
pub fn run_lsp_server<R: BufRead, W: Write>(
    server: Arc<Mutex<LspServer>>,
    mut reader: R,
    mut writer: W,
) -> Result<(), String> {
    loop {
        // 读取消息头
        let mut header = String::new();
        let mut content_length = 0;

        loop {
            header.clear();
            if reader.read_line(&mut header).map_err(|e| e.to_string())? == 0 {
                debug!("Connection closed by client");
                return Ok(()); // 连接已关闭
            }

            header = header.trim().to_string();
            if header.is_empty() {
                break; // 头部结束
            }

            // 解析Content-Length头
            if header.to_lowercase().starts_with("content-length: ") {
                content_length = header[16..]
                    .parse::<usize>()
                    .map_err(|e| format!("Invalid Content-Length: {}", e))?;
            }
        }

        if content_length == 0 {
            return Err("Missing Content-Length header".to_string());
        }

        // 读取消息体
        let mut buffer = vec![0; content_length];
        reader
            .read_exact(&mut buffer)
            .map_err(|e| format!("Failed to read message: {}", e))?;

        let message =
            String::from_utf8(buffer).map_err(|e| format!("Invalid UTF-8 in message: {}", e))?;

        // 解析消息类型并处理
        if message.contains("\"id\":") {
            // 请求
            match serde_json::from_str::<RequestMessage>(&message) {
                Ok(request) => {
                    // 使用 catch_unwind 包裹请求处理
                    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                        handle_request(server.clone(), request.clone())
                    }));

                    match result {
                        Ok(response) => {
                            if let Err(e) = send_message(&mut writer, &response) {
                                error!("Failed to send response: {}", e);
                                // Consider breaking or returning error if sending fails critically
                            }

                            // 如果是退出请求并且服务器状态是关闭，则退出循环
                            if request.method == "exit" {
                                let server_guard = server.lock().unwrap();
                                if server_guard.state == ServerState::ShutDown {
                                    info!("Exit request received and server is shutdown. Exiting.");
                                    return Ok(());
                                }
                            }
                        }
                        Err(panic_payload) => {
                            error!(
                                "Panic occurred while handling request (ID: {:?} Method: {}): {:?}",
                                request.id, request.method, panic_payload
                            );
                            // 发送内部错误响应
                            let error_response = ResponseMessage {
                                jsonrpc: "2.0".to_string(),
                                id: request.id.clone(),
                                result: None,
                                error: Some(ResponseError {
                                    code: error_codes::INTERNAL_ERROR,
                                    message: "Internal server error during request handling."
                                        .to_string(),
                                    data: None,
                                }),
                            };
                            if let Err(e) = send_message(&mut writer, &error_response) {
                                error!("Failed to send internal error response: {}", e);
                            }
                        }
                    }
                }
                Err(e) => {
                    error!("Failed to parse request: {}", e);
                    // 尝试从原始 JSON 中提取 ID
                    let id = serde_json::from_str::<Value>(&message)
                        .ok()
                        .and_then(|v| v.get("id").cloned())
                        .and_then(|id_val| serde_json::from_value::<RequestId>(id_val).ok())
                        .unwrap_or(RequestId::Null); // 如果无法解析ID，则使用 Null

                    let error_response = ResponseMessage {
                        jsonrpc: "2.0".to_string(),
                        id, // 使用提取的或默认的 ID
                        result: None,
                        error: Some(ResponseError {
                            code: error_codes::PARSE_ERROR,
                            message: format!("Failed to parse request: {}", e),
                            data: None,
                        }),
                    };
                    if let Err(e) = send_message(&mut writer, &error_response) {
                        error!("Failed to send parse error response: {}", e);
                    }
                }
            }
        } else {
            // 通知
            match serde_json::from_str::<NotificationMessage>(&message) {
                Ok(notification) => {
                    // 使用 catch_unwind 包裹通知处理
                    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                        // 需要传递 writer 的可变引用，但 catch_unwind 要求闭包是 FnOnce
                        // 解决方案：克隆 Arc<Mutex<LspServer>>，但在闭包外部处理 writer
                        // 或者，如果 handle_notification 不需要 writer，则移除它
                        // 假设 handle_notification 可能需要 writer 发送诊断等，我们需要一种方法
                        // 暂时将 writer 移出闭包，如果 handle_notification 确实需要，需要重构
                        handle_notification(server.clone(), notification.clone(), &mut writer)
                    }));

                    match result {
                        Ok(Ok(())) => {
                            // 通知处理成功
                        }
                        Ok(Err(e)) => {
                            // 通知处理函数返回错误
                            error!(
                                "Error handling notification (Method: {}): {}",
                                notification.method, e
                            );
                        }
                        Err(panic_payload) => {
                            // 通知处理函数发生 Panic
                            error!(
                                "Panic occurred while handling notification (Method: {}): {:?}",
                                notification.method, panic_payload
                            );
                            // 通知没有响应，所以只记录错误
                        }
                    }
                }
                Err(e) => {
                    error!("Failed to parse notification: {}", e);
                    // 通知解析失败，只记录日志
                }
            }
        }
    }
    // 注意：原始代码的 loop 永不退出，除非 read_line 返回 0 或 exit 请求被正确处理。
    // 添加 catch_unwind 后，即使发生 panic，循环也会继续。
}

/// 处理LSP请求
fn handle_request(server: Arc<Mutex<LspServer>>, request: RequestMessage) -> ResponseMessage {
    let mut response = ResponseMessage {
        jsonrpc: "2.0".to_string(),
        id: request.id.clone(),
        result: None,
        error: None,
    };

    match request.method.as_str() {
        "initialize" => match serde_json::from_value::<InitializeParams>(request.params) {
            Ok(params) => {
                let mut server = server.lock().unwrap();
                match server.initialize(params) {
                    Ok(result) => {
                        response.result = Some(serde_json::to_value(result).unwrap());
                    }
                    Err(err) => {
                        response.error = Some(err);
                    }
                }
            }
            Err(e) => {
                response.error = Some(ResponseError {
                    code: error_codes::INVALID_PARAMS,
                    message: format!("Invalid initialize params: {}", e),
                    data: None,
                });
            }
        },
        "shutdown" => {
            let mut server = server.lock().unwrap();
            match server.shutdown() {
                Ok(result) => {
                    response.result = Some(result);
                }
                Err(err) => {
                    response.error = Some(err);
                }
            }
        }
        "textDocument/completion" => {
            match serde_json::from_value::<CompletionParams>(request.params) {
                Ok(params) => {
                    let server = server.lock().unwrap();
                    match server.completion(params) {
                        Ok(result) => {
                            response.result = Some(serde_json::to_value(result).unwrap());
                        }
                        Err(err) => {
                            response.error = Some(err);
                        }
                    }
                }
                Err(e) => {
                    response.error = Some(ResponseError {
                        code: error_codes::INVALID_PARAMS,
                        message: format!("Invalid completion params: {}", e),
                        data: None,
                    });
                }
            }
        }
        "textDocument/semanticTokens/full" => {
            match serde_json::from_value::<TextDocumentIdentifier>(
                request
                    .params
                    .get("textDocument")
                    .cloned()
                    .unwrap_or(serde_json::Value::Null),
            ) {
                Ok(text_doc) => {
                    let uri = text_doc.uri.clone();
                    let server = server.lock().unwrap();

                    if let Some(document) = server.documents.get(&uri) {
                        // 生成语义标记
                        if let Some((_, semantic_tokens)) = server.validate_document(&uri) {
                            if let Some(tokens) = semantic_tokens {
                                // 编码标记
                                let encoded_tokens =
                                    encode_semantic_tokens(&tokens, &document.content);

                                // 创建结果
                                let result = serde_json::json!({
                                    "data": encoded_tokens
                                });

                                response.result = Some(result);
                            } else {
                                // 如果没有标记，返回空数组
                                response.result = Some(serde_json::json!({
                                    "data": []
                                }));
                            }
                        } else {
                            // 如果没有验证结果，返回空数组
                            response.result = Some(serde_json::json!({
                                "data": []
                            }));
                        }
                    } else {
                        response.error = Some(ResponseError {
                            code: error_codes::INVALID_PARAMS,
                            message: format!("Document not found: {}", uri),
                            data: None,
                        });
                    }
                }
                Err(e) => {
                    response.error = Some(ResponseError {
                        code: error_codes::INVALID_PARAMS,
                        message: format!("Invalid semantic tokens params: {}", e),
                        data: None,
                    });
                }
            }
        }
        _ => {
            response.error = Some(ResponseError {
                code: error_codes::METHOD_NOT_FOUND,
                message: format!("Method not found: {}", request.method),
                data: None,
            });
        }
    }

    response
}

/// 处理LSP通知
fn handle_notification<W: Write>(
    server: Arc<Mutex<LspServer>>,
    notification: NotificationMessage,
    writer: &mut W,
) -> Result<(), String> {
    debug!("Notification: {}", notification.method);

    match notification.method.as_str() {
        "initialized" => {
            let mut server = server.lock().unwrap();
            server.initialized();
        }
        "exit" => {
            let mut server = server.lock().unwrap();
            if server.exit() {
                return Ok(());
            }
        }
        "textDocument/didOpen" => {
            match serde_json::from_value::<DidOpenTextDocumentParams>(notification.params) {
                Ok(params) => {
                    let uri = params.text_document.uri.clone();
                    let mut server_locked = server.lock().unwrap();
                    server_locked.did_open(params);

                    // 发送诊断通知和语义着色信息
                    if let Some((diagnostics, semantic_tokens)) =
                        server_locked.validate_document(&uri)
                    {
                        // 获取文档内容
                        let content = if let Some(doc) = server_locked.documents.get(&uri) {
                            doc.content.clone()
                        } else {
                            String::new()
                        };

                        drop(server_locked); // 释放锁

                        // 发送诊断信息
                        send_diagnostics(writer, &uri, diagnostics)?;

                        // 如果有语义着色信息，则发送语义着色通知
                        if let Some(tokens) = semantic_tokens {
                            // 编码标记并发送
                            let encoded_tokens = encode_semantic_tokens(&tokens, &content);
                            send_semantic_tokens_encoded(writer, &uri, encoded_tokens)?;
                        }
                    }
                }
                Err(e) => {
                    error!("Invalid didOpen params: {}", e);
                }
            }
        }
        "textDocument/didChange" => {
            match serde_json::from_value::<DidChangeTextDocumentParams>(notification.params) {
                Ok(params) => {
                    let uri = params.text_document.uri.clone();
                    let mut server = server.lock().unwrap();
                    server.did_change(params);

                    // 发送诊断通知和语义着色信息
                    if let Some((diagnostics, _)) = server.validate_document(&uri) {
                        // 获取文档内容

                        drop(server); // 释放锁

                        // 发送诊断信息
                        send_diagnostics(writer, &uri, diagnostics)?;
                    }
                }
                Err(e) => {
                    error!("Invalid didChange params: {}", e);
                }
            }
        }
        "textDocument/didClose" => {
            match serde_json::from_value::<DidCloseTextDocumentParams>(notification.params) {
                Ok(params) => {
                    let mut server = server.lock().unwrap();
                    server.did_close(params);
                }
                Err(e) => {
                    error!("Invalid didClose params: {}", e);
                }
            }
        }
        // 添加对 setTrace 通知的处理
        "$/setTrace" => {
            // 仅记录日志，不需要其他操作
            debug!("Trace level set to: {}", notification.params);
        }
        // 添加对 cancelRequest 通知的处理
        "$/cancelRequest" => {
            debug!("Request cancellation received: {}", notification.params);
            // 未实现请求取消逻辑，因为当前实现是同步的
        }
        // 添加对 progress 通知的处理
        "$/progress" => {
            debug!("Progress notification: {}", notification.params);
        }
        // 添加对 logTrace 通知的处理
        "$/logTrace" => {
            debug!("Log trace notification: {}", notification.params);
        }
        // 添加对 telemetry/event 通知的处理
        "telemetry/event" => {
            debug!("Telemetry event: {}", notification.params);
        }
        // 添加对 workspace/didChangeConfiguration 通知的处理
        "workspace/didChangeConfiguration" => {
            debug!("Configuration changed: {}", notification.params);
        }
        // 添加对 workspace/didChangeWatchedFiles 通知的处理
        "workspace/didChangeWatchedFiles" => {
            debug!("Watched files changed: {}", notification.params);
        }
        _ => {
            // 改为 info 级别，减少错误日志
            info!("Unhandled notification: {}", notification.method);
        }
    }

    Ok(())
}

/// 发送诊断通知
fn send_diagnostics<W: Write>(
    writer: &mut W,
    uri: &str,
    diagnostics: Vec<Diagnostic>,
) -> Result<(), String> {
    let params = PublishDiagnosticsParams {
        uri: uri.to_string(),
        diagnostics,
    };

    let notification = NotificationMessage {
        jsonrpc: "2.0".to_string(),
        method: "textDocument/publishDiagnostics".to_string(),
        params: serde_json::to_value(params).unwrap(),
    };

    send_message(writer, &notification)
}

/// 发送已编码的语义着色通知
fn send_semantic_tokens_encoded<W: Write>(
    writer: &mut W,
    uri: &str,
    encoded_tokens: Vec<u32>,
) -> Result<(), String> {
    let params = serde_json::json!({
        "textDocument": {
            "uri": uri
        },
        "tokens": encoded_tokens
    });

    let notification = NotificationMessage {
        jsonrpc: "2.0".to_string(),
        method: "textDocument/semanticTokens/full".to_string(),
        params,
    };
    send_message(writer, &notification)
}

/// 发送LSP消息
fn send_message<T: serde::Serialize, W: Write>(writer: &mut W, message: &T) -> Result<(), String> {
    let json = serde_json::to_string(message).map_err(|e| e.to_string())?;
    let content = json.as_bytes();

    debug!("Sending: {}", json);

    write!(writer, "Content-Length: {}\r\n\r\n", content.len())
        .map_err(|e| format!("Failed to write header: {}", e))?;

    writer
        .write_all(content)
        .map_err(|e| format!("Failed to write content: {}", e))?;

    writer
        .flush()
        .map_err(|e| format!("Failed to flush: {}", e))?;

    Ok(())
}
