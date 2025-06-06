use std::collections::{HashMap, HashSet}; // Add HashSet
use std::io::{BufRead, Write};
use std::panic;
use std::sync::{Arc, Mutex}; // Add panic module

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

/// LSP Server State
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ServerState {
    Uninitialized,
    Initializing,
    Initialized,
    ShutDown,
}

/// LSP Server data structure
pub struct LspServer {
    /// Server's current state
    state: ServerState,
    /// Opened document map
    documents: HashMap<String, TextDocument>,
    /// Client capabilities
    client_capabilities: Option<ClientCapabilities>,
    /// Workspace root directory
    root_uri: Option<String>,
}

impl LspServer {
    /// Creates a new LSP server instance
    pub fn new() -> Self {
        Self {
            state: ServerState::Uninitialized,
            documents: HashMap::new(),
            client_capabilities: None,
            root_uri: None,
        }
    }

    /// Handles initialization request
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

    /// Handles initialized notification
    pub fn initialized(&mut self) {
        self.state = ServerState::Initialized;
    }

    /// Handles shutdown request
    pub fn shutdown(&mut self) -> Result<Value, ResponseError> {
        self.state = ServerState::ShutDown;
        Ok(Value::Null)
    }

    /// Handles exit notification
    pub fn exit(&mut self) -> bool {
        self.state == ServerState::ShutDown
    }

    /// Handles document open notification
    pub fn did_open(&mut self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let document = TextDocument::new(
            params.text_document.uri,
            params.text_document.language_id,
            params.text_document.version,
            params.text_document.text,
        );
        self.documents.insert(uri.clone(), document);

        // Trigger document validation
        self.validate_document(&uri);
        info!("Document opened: {}", uri);
    }

    /// Handles document change notification
    pub fn did_change(&mut self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        info!("Document changed: {} (version {})", uri, params.text_document.version);

        // Get or create document
        let document = if let Some(doc) = self.documents.get_mut(&uri) {
            info!("Updating existing document");
            doc
        } else {
            info!("Document not found, creating new document: {}", uri);
            // Determine initial content
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
                "onion".to_string(), // Assuming language is onion
                params.text_document.version,
                initial_content,
            );

            self.documents.insert(uri.clone(), doc);
            self.documents.get_mut(&uri).unwrap()
        };

        // Apply all changes
        for (i, change) in params.content_changes.iter().enumerate() {
            match change {
                TextDocumentContentChangeEvent::Full { text } => {
                    info!("Applying full change #{}: content length {} bytes", i, text.len());
                    document.update_content(text.clone());
                }
                TextDocumentContentChangeEvent::Incremental { range, text } => {
                    info!(
                        "Applying incremental change #{}: range [{},{}]-[{},{}], text length {} bytes",
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

        // Update document version
        let old_version = document.version;
        document.version = params.text_document.version;
        info!("Document version updated from {} to {}", old_version, document.version);

        // Print current document content (for debugging)
        debug!(
            "Updated document content (first 100 chars): {}",
            &document.content.chars().take(100).collect::<String>()
        );
    }
    /// Handles document close notification
    pub fn did_close(&mut self, params: DidCloseTextDocumentParams) {
        info!("Document closed: {}", params.text_document.uri);
        let uri = params.text_document.uri.clone();
        if self.documents.remove(&uri).is_none() {
            warn!("Tried to close non-existent document: {}", uri);
        }
    }

    // Validates document and sends diagnostic messages
    fn validate_document(
        &self,
        uri: &str,
    ) -> Option<(Vec<Diagnostic>, Option<Vec<SemanticTokenTypes>>)> {
        info!("Validating document: {} started", uri);
        if let Some(document) = self.documents.get(uri) {
            info!(
                "Found document, version {}, content length {} bytes",
                document.version,
                document.content.len()
            );

            // Generate diagnostics and semantic tokens
            match std::panic::catch_unwind(|| super::diagnostics::validate_document(document)) {
                Ok((diagnostics, semantic_tokens)) => {
                    info!("Validation complete: {} diagnostics generated", diagnostics.len());
                    if let Some(tokens) = &semantic_tokens {
                        info!("Semantic highlighting: {} tokens generated", tokens.len());
                    } else {
                        info!("Semantic highlighting: Failed to generate tokens");
                    }
                    Some((diagnostics, semantic_tokens))
                }
                Err(e) => {
                    error!("Panic during validation: {:?}", e);
                    Some((vec![], None)) // Return empty diagnostics list and no semantic tokens
                }
            }
        } else {
            warn!("Document not found for validation: {}", uri);
            None
        }
    }
    /// Handles auto-completion request
    pub fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<CompletionResponse, ResponseError> {
        let uri = params.text_document.uri.clone();

        if let Some(document) = self.documents.get(&uri) {
            // Calculate completion items from document content and position
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

    /// Calculates completion items for the given position
    fn calculate_completions(
        &self,
        document: &TextDocument,
        position: Position,
    ) -> Vec<CompletionItem> {
        // Provide completion items for keywords, operators, etc., based on Onion syntax
        let mut items = Vec::new();
        let mut unique_vars = HashSet::new(); // Used to store unique variable names

        // Add Onion keywords
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

        // Add constants
        let constants = vec!["true", "false", "null", "undefined"];
        for constant in constants {
            items.push(CompletionItem {
                label: constant.to_string(),
                kind: Some(CompletionItemKind::Constant), // Use Constant type
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

        // --- Use analyzer to get variable context ---
        // 1. Parse AST
        let lex_result = lexer::lexer::tokenize(&document.content);
        let lex_result = lexer::lexer::reject_comment(&lex_result);
        let parse_result = build_ast(&lex_result);
        if let Ok(ast) = parse_result {
            // 2. Convert LSP Position to byte offset
            if let Some(byte_offset) = position_to_byte_offset(&document.content, position.clone())
            {
                // 1. Parse URI
                let file_path = match Url::parse(&document.uri) {
                    Ok(url) if url.scheme() == "file" => {
                        match url.to_file_path() {
                            Ok(path) => path,
                            Err(_) => {
                                error!("Failed to convert URI to file path: {}", document.uri);
                                return vec![]; // Return empty list
                            }
                        }
                    }
                    _ => {
                        error!("Invalid URI scheme: {}", document.uri);
                        return vec![]; // Return empty list
                    }
                };

                // 2. Get parent directory
                let parent_dir = match file_path.parent() {
                    Some(dir) => dir.to_path_buf(),
                    None => {
                        error!(
                            "Failed to get parent directory for file: {}",
                            file_path.display()
                        );
                        return vec![]; // Return empty list
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
                            return vec![]; // Return empty list
                        }
                    }) {
                        Ok(path) => path.to_str().unwrap_or("").to_string(),
                        Err(e) => {
                            error!("Failed to get absolute path: {}", e);
                            return vec![]; // Return empty list
                        }
                    }
                    .to_string(),
                ) {
                    Ok(result) => result,
                    Err(e) => {
                        error!("Cycle detection failed: {}", e);
                        return vec![]; // Return empty list
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
                let ast = auto_capture_and_rebuild(&macro_result.result_node).1;

                // 3. Call analyzer to get context at specific position
                let analysis_output = analyzer::analyze_ast(
                    &ast,
                    Some(byte_offset),
                    visit_result.get_detector_mut(),
                    &mut dir_stack,
                );

                // 4. If analyzer captured context at breakpoint, extract variables
                if let Some(context) = analysis_output.context_at_break {
                    // Iterate scope frames from inner to outer
                    for context in context.all_contexts().iter().rev() {
                        for frame in context.iter().rev() {
                            for var in &frame.variables {
                                // Add to HashSet to ensure uniqueness
                                if unique_vars.insert(var.name.clone()) {
                                    // Determine CompletionItemKind and detail based on variable type
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
                                        // Add more branches for other types
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
                                        documentation: None, // Consider adding type-based documentation in the future
                                        insert_text: Some(var.name.clone()), // For functions, might need to add '()'
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
            // Parsing failed, return only keywords and built-in functions
        }

        items
    }
}

/// Converts LSP Position (0-based line, 0-based UTF-16 character offset) to byte offset
fn position_to_byte_offset(text: &str, position: Position) -> Option<usize> {
    let mut byte_offset = 0;
    let mut current_line = 0;

    for line in text.lines() {
        if current_line == position.line {
            // Found target line, calculate byte offset for character offset
            let mut char_offset = 0;
            for (i, ch) in line.char_indices() {
                if char_offset == position.character {
                    return Some(byte_offset + i);
                }
                // LSP uses UTF-16 code units, Rust string iterators use Unicode scalar values (char)
                // For BMP characters, both lengths are the same. For surrogate pairs, UTF-16 length is 2, char length is 1.
                // Simplified handling here, assuming most are BMP characters, or precise handling of surrogate pairs is not strictly required.
                // A more precise method needs to handle UTF-16 encoding.
                char_offset += ch.len_utf16() as u32; // Count using UTF-16 length
                if char_offset > position.character {
                    // If target character offset is exceeded, position is inside character or invalid, return current byte offset
                    return Some(byte_offset + i);
                }
            }
            // If loop ends and not found, position is at end of line
            return Some(byte_offset + line.len());
        }
        // Add byte length of line content and newline character
        // Need to handle both \n and \r\n cases
        byte_offset += line.len();
        if text[byte_offset..].starts_with("\r\n") {
            byte_offset += 2;
        } else if text[byte_offset..].starts_with('\n') {
            byte_offset += 1;
        } else {
            // No newline character at end of file
            if current_line == position.line {
                // If it's the last line and position is at end of line
                return Some(byte_offset);
            }
            // Otherwise, position.line might be out of range
            break;
        }
        current_line += 1;
    }

    // If line number is out of range, or text is empty
    if current_line == position.line && position.character == 0 {
        return Some(byte_offset); // Might be an empty file or position after the last line
    }

    None // Position invalid
}

/// Runs the LSP server
pub fn run_lsp_server<R: BufRead, W: Write>(
    server: Arc<Mutex<LspServer>>,
    mut reader: R,
    mut writer: W,
) -> Result<(), String> {
    loop {
        // Read message header
        let mut header = String::new();
        let mut content_length = 0;

        loop {
            header.clear();
            if reader.read_line(&mut header).map_err(|e| e.to_string())? == 0 {
                debug!("Connection closed by client");
                return Ok(()); // Connection closed
            }

            header = header.trim().to_string();
            if header.is_empty() {
                break; // Header end
            }

            // Parse Content-Length header
            if header.to_lowercase().starts_with("content-length: ") {
                content_length = header[16..]
                    .parse::<usize>()
                    .map_err(|e| format!("Invalid Content-Length: {}", e))?;
            }
        }

        if content_length == 0 {
            return Err("Missing Content-Length header".to_string());
        }

        // Read message body
        let mut buffer = vec![0; content_length];
        reader
            .read_exact(&mut buffer)
            .map_err(|e| format!("Failed to read message: {}", e))?;

        let message =
            String::from_utf8(buffer).map_err(|e| format!("Invalid UTF-8 in message: {}", e))?;

        // Parse message type and handle
        if message.contains("\"id\":") {
            // Request
            match serde_json::from_str::<RequestMessage>(&message) {
                Ok(request) => {
                    // Wrap request handling with catch_unwind
                    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                        handle_request(server.clone(), request.clone())
                    }));

                    match result {
                        Ok(response) => {
                            if let Err(e) = send_message(&mut writer, &response) {
                                error!("Failed to send response: {}", e);
                                // Consider breaking or returning error if sending fails critically
                            }

                            // If it's an exit request and server state is shutdown, exit loop
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
                            // Send internal error response
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
                    // Try to extract ID from raw JSON
                    let id = serde_json::from_str::<Value>(&message)
                        .ok()
                        .and_then(|v| v.get("id").cloned())
                        .and_then(|id_val| serde_json::from_value::<RequestId>(id_val).ok())
                        .unwrap_or(RequestId::Null); // Use Null if ID cannot be parsed

                    let error_response = ResponseMessage {
                        jsonrpc: "2.0".to_string(),
                        id, // Use extracted or default ID
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
            // Notification
            match serde_json::from_str::<NotificationMessage>(&message) {
                Ok(notification) => {
                    // Wrap notification handling with catch_unwind
                    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                        // Need to pass mutable reference of writer, but catch_unwind requires closure to be FnOnce
                        // Solution: Clone Arc<Mutex<LspServer>>, but handle writer outside the closure
                        // Or, if handle_notification doesn't need writer, remove it
                        // Assuming handle_notification might need writer to send diagnostics etc., we need a way
                        // Temporarily move writer out of closure, if handle_notification indeed needs it, refactor
                        handle_notification(server.clone(), notification.clone(), &mut writer)
                    }));

                    match result {
                        Ok(Ok(())) => {
                            // Notification handled successfully
                        }
                        Ok(Err(e)) => {
                            // Notification handler returned error
                            error!(
                                "Error handling notification (Method: {}): {}",
                                notification.method, e
                            );
                        }
                        Err(panic_payload) => {
                            // Notification handler panicked
                            error!(
                                "Panic occurred while handling notification (Method: {}): {:?}",
                                notification.method, panic_payload
                            );
                            // Notifications don't have responses, so just log error
                        }
                    }
                }
                Err(e) => {
                    error!("Failed to parse notification: {}", e);
                    // Notification parsing failed, just log
                }
            }
        }
    }
    // Note: Original code's loop never exits unless read_line returns 0 or exit request is handled correctly.
    // After adding catch_unwind, loop continues even if panic occurs.
}

/// Handles LSP request
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
                        // Generate semantic tokens
                        if let Some((_, semantic_tokens)) = server.validate_document(&uri) {
                            if let Some(tokens) = semantic_tokens {
                                // Encode tokens
                                let encoded_tokens =
                                    encode_semantic_tokens(&tokens, &document.content);

                                // Create result
                                let result = serde_json::json!({
                                    "data": encoded_tokens
                                });

                                response.result = Some(result);
                            } else {
                                // If no tokens, return empty array
                                response.result = Some(serde_json::json!({
                                    "data": []
                                }));
                            }
                        } else {
                            // If no validation result, return empty array
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

/// Handles LSP notification
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

                    // Send diagnostic notification and semantic highlighting information
                    if let Some((diagnostics, semantic_tokens)) =
                        server_locked.validate_document(&uri)
                    {
                        // Get document content
                        let content = if let Some(doc) = server_locked.documents.get(&uri) {
                            doc.content.clone()
                        } else {
                            String::new()
                        };

                        drop(server_locked); // Release lock

                        // Send diagnostics
                        send_diagnostics(writer, &uri, diagnostics)?;

                        // If semantic highlighting information exists, send semantic highlighting notification
                        if let Some(tokens) = semantic_tokens {
                            // Encode tokens and send
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

                    // Send diagnostic notification and semantic highlighting information
                    if let Some((diagnostics, _)) = server.validate_document(&uri) {
                        // Get document content

                        drop(server); // Release lock

                        // Send diagnostics
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
        // Add handling for setTrace notification
        "$/setTrace" => {
            // Log only, no other action needed
            debug!("Trace level set to: {}", notification.params);
        }
        // Add handling for cancelRequest notification
        "$/cancelRequest" => {
            debug!("Request cancellation received: {}", notification.params);
            // Request cancellation logic not implemented as current implementation is synchronous
        }
        // Add handling for progress notification
        "$/progress" => {
            debug!("Progress notification: {}", notification.params);
        }
        // Add handling for logTrace notification
        "$/logTrace" => {
            debug!("Log trace notification: {}", notification.params);
        }
        // Add handling for telemetry/event notification
        "telemetry/event" => {
            debug!("Telemetry event: {}", notification.params);
        }
        // Add handling for workspace/didChangeConfiguration notification
        "workspace/didChangeConfiguration" => {
            debug!("Configuration changed: {}", notification.params);
        }
        // Add handling for workspace/didChangeWatchedFiles notification
        "workspace/didChangeWatchedFiles" => {
            debug!("Watched files changed: {}", notification.params);
        }
        _ => {
            // Change to info level to reduce error logs
            info!("Unhandled notification: {}", notification.method);
        }
    }

    Ok(())
}

/// Sends diagnostic notification
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

/// Sends semantic highlighting notification (encoded)
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
