use log::{debug, error, info};
use onion_frontend::parser::analyzer::auto_capture_and_rebuild;
use onion_frontend::parser::analyzer::expand_macro;
use onion_frontend::utils::cycle_detector;
use url::Url;

use super::document::TextDocument;
use super::protocol::*;
use super::semantic::SemanticTokenTypes;
use crate::lsp::semantic::do_semantic;
use onion_frontend::dir_stack::DirectoryStack;
use onion_frontend::parser::analyzer::analyze_ast;
use onion_frontend::parser::ast::ast_token_stream;
use onion_frontend::parser::ast::build_ast;
use onion_frontend::parser::ast::ParserError;
use onion_frontend::parser::lexer::lexer;
use onion_frontend::parser::lexer::Token;
/// Validates the document and generates diagnostics.
/// Returns diagnostics and optional semantic highlighting results.
pub fn validate_document(
    document: &TextDocument,
) -> (Vec<Diagnostic>, Option<Vec<SemanticTokenTypes>>) {
    let mut diagnostics = Vec::new();
    let mut semantic_tokens = None;

    // Check if the document is empty
    if document.content.is_empty() {
        info!("Document content is empty, skipping validation: {}", document.uri);
        return (diagnostics, semantic_tokens);
    }

    info!("Performing lexical analysis: {}", document.uri);

    // Perform lexical analysis
    let tokens = match std::panic::catch_unwind(|| lexer::tokenize(&document.content)) {
        Ok(tokens) => tokens,
        Err(e) => {
            error!("Error during lexical analysis: {:?}", e);
            return (
                vec![Diagnostic {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 1,
                        },
                    },
                    severity: Some(DiagnosticSeverity::Error),
                    code: None,
                    source: Some("onion-lsp".to_string()),
                    message: "Lexical analysis failed: May contain unrecognized tokens".to_string(),
                    related_information: None,
                }],
                None,
            );
        }
    };

    info!("Lexical analysis complete, {} tokens found", tokens.len());

    let filtered_tokens = lexer::reject_comment(&tokens);
    debug!("{} tokens after filtering comments", filtered_tokens.len());

    if filtered_tokens.is_empty() {
        info!("No valid tokens after filtering, possibly only comments or whitespace: {}", document.uri);
        return (diagnostics, semantic_tokens);
    }

    // Try to parse AST
    info!("Starting syntax analysis");
    let gathered =
        match std::panic::catch_unwind(|| ast_token_stream::from_stream(&filtered_tokens)) {
            Ok(gathered) => gathered,
            Err(e) => {
                error!("Token stream processing failed: {:?}", e);
                return (
                    vec![Diagnostic {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 1,
                            },
                        },
                        severity: Some(DiagnosticSeverity::Error),
                        code: None,
                        source: Some("onion-lsp".to_string()),
                        message: "Error processing token stream".to_string(),
                        related_information: None,
                    }],
                    None,
                );
            }
        };

    match build_ast(gathered) {
        Ok(ast) => {
            // Parsing successful, no errors
            info!("Document parsed successfully: {}", document.uri);
            info!("Analyzing variable definitions: {}", document.uri);
            // 1. Parse URI
            let file_path = match Url::parse(&document.uri) {
                Ok(url) if url.scheme() == "file" => match url.to_file_path() {
                    Ok(path) => path,
                    Err(_) => {
                        error!("Failed to convert URI to file path: {}", document.uri);
                        return (vec![], None);
                    }
                },
                _ => {
                    error!("Invalid URI scheme: {}", document.uri);
                    return (vec![], None);
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
                    return (vec![], None);
                }
            };
            let dir_stack = DirectoryStack::new(Some(&parent_dir));
            if let Err(err) = &dir_stack {
                let err_msg = err.to_string();
                error!("Directory stack initialization failed: {}", err_msg);
                return (
                    vec![Diagnostic {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 1,
                            },
                        },
                        severity: Some(DiagnosticSeverity::Error),
                        code: None,
                        source: Some("onion-lsp".to_string()),
                        message: format!("Directory stack initialization failed: {}", err_msg),
                        related_information: None,
                    }],
                    None,
                );
            }
            let mut dir_stack = dir_stack.unwrap();

            let mut cycle_detector = cycle_detector::CycleDetector::new();
            let visit_result = cycle_detector.visit(file_path.to_str().unwrap_or("").to_string());
            match visit_result {
                Ok(mut guard) => {
                    let macro_result = expand_macro(&ast, guard.get_detector_mut(), &mut dir_stack);
                    for error in macro_result.errors {
                        match error {
                    onion_frontend::parser::analyzer::AnalyzeError::UndefinedVariable(var) => {
                        if var.start_token.is_none() {
                            continue;
                        }
                        let range = get_token_range(var.start_token.unwrap(), &document.content);
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::Error),
                            code: Some(serde_json::Value::String("VAR-E001".to_string())),
                            source: Some("onion-lsp".to_string()),
                            message: format!(
                                "Variable '{}' is not explicitly defined",
                                var.start_token.unwrap().token
                            ),
                            related_information: None,
                        });
                    }
                    onion_frontend::parser::analyzer::AnalyzeError::InvalidMacroDefinition(
                        node,
                        msg,
                    ) => {
                        if node.start_token.is_none() {
                            continue;
                        }
                        let range = get_token_range(node.start_token.unwrap(), &document.content);
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::Error),
                            code: Some(serde_json::Value::String("MACRO-E001".to_string())),
                            source: Some("onion-lsp".to_string()),
                            message: format!("Macro definition error: {}", msg),
                            related_information: None,
                        });
                    }
                    onion_frontend::parser::analyzer::AnalyzeError::DetailedError(node, msg) => {
                        if node.start_token.is_none() {
                            continue;
                        }
                        let range = get_token_range(node.start_token.unwrap(), &document.content);
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::Error),
                            code: Some(serde_json::Value::String("AST-E001".to_string())),
                            source: Some("onion-lsp".to_string()),
                            message: format!("Error: {}", msg),
                            related_information: None,
                        });
                    }
                    onion_frontend::parser::analyzer::AnalyzeError::DetailedContextLessError(
                        _,
                        msg,
                    ) => {
                        // No range information
                        let range = Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 1,
                            },
                        };
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::Error),
                            code: Some(serde_json::Value::String("AST-E001".to_string())),
                            source: Some("onion-lsp".to_string()),
                            message: format!("Error: {}", msg),
                            related_information: None,
                        });
                    }
                }
                    }
                    for warn in macro_result.warnings {
                        match warn {
                            onion_frontend::parser::analyzer::AnalyzeWarn::CompileError(
                                node,
                                warn,
                            ) => {
                                if node.start_token.is_none() {
                                    continue;
                                }
                                let range =
                                    get_token_range(node.start_token.unwrap(), &document.content);
                                diagnostics.push(Diagnostic {
                                    range,
                                    severity: Some(DiagnosticSeverity::Warning),
                                    code: Some(serde_json::Value::String("COMP-W001".to_string())),
                                    source: Some("onion-lsp".to_string()),
                                    message: format!("@compile: {}", warn),
                                    related_information: None,
                                });
                            }
                        }
                    }
                    let ast = auto_capture_and_rebuild(&macro_result.result_node).1;

                    let result = analyze_ast(&ast, None, guard.get_detector_mut(), &mut dir_stack);
                    for error in result.errors {
                        match error {
                    onion_frontend::parser::analyzer::AnalyzeError::UndefinedVariable(var) => {
                        if var.start_token.is_none() {
                            continue;
                        }
                        let range = get_token_range(var.start_token.unwrap(), &document.content);
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::Error),
                            code: Some(serde_json::Value::String("VAR-E001".to_string())),
                            source: Some("onion-lsp".to_string()),
                            message: format!(
                                "Variable '{}' is not explicitly defined",
                                var.start_token.unwrap().token
                            ),
                            related_information: None,
                        });
                    }
                    onion_frontend::parser::analyzer::AnalyzeError::InvalidMacroDefinition(
                        node,
                        msg,
                    ) => {
                        if node.start_token.is_none() {
                            continue;
                        }
                        let range = get_token_range(node.start_token.unwrap(), &document.content);
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::Error),
                            code: Some(serde_json::Value::String("MACRO-E001".to_string())),
                            source: Some("onion-lsp".to_string()),
                            message: format!("Macro definition error: {}", msg),
                            related_information: None,
                        });
                    }
                    onion_frontend::parser::analyzer::AnalyzeError::DetailedError(node, msg) => {
                        if node.start_token.is_none() {
                            continue;
                        }
                        let range = get_token_range(node.start_token.unwrap(), &document.content);
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::Error),
                            code: Some(serde_json::Value::String("AST-E001".to_string())),
                            source: Some("onion-lsp".to_string()),
                            message: format!("Error: {}", msg),
                            related_information: None,
                        });
                    }
                    onion_frontend::parser::analyzer::AnalyzeError::DetailedContextLessError(
                        _,
                        msg,
                    ) => {
                        // No range information
                        let range = Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 1,
                            },
                        };
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::Error),
                            code: Some(serde_json::Value::String("AST-E001".to_string())),
                            source: Some("onion-lsp".to_string()),
                            message: format!("Error: {}", msg),
                            related_information: None,
                        });
                    }
                }
                    }
                    for warn in result.warnings {
                        match warn {
                            onion_frontend::parser::analyzer::AnalyzeWarn::CompileError(
                                node,
                                warn,
                            ) => {
                                if node.start_token.is_none() {
                                    continue;
                                }
                                let range =
                                    get_token_range(node.start_token.unwrap(), &document.content);
                                diagnostics.push(Diagnostic {
                                    range,
                                    severity: Some(DiagnosticSeverity::Warning),
                                    code: Some(serde_json::Value::String("COMP-W001".to_string())),
                                    source: Some("onion-lsp".to_string()),
                                    message: format!("@compile: {}", warn),
                                    related_information: None,
                                });
                            }
                        }
                    }

                    // Perform semantic highlighting
                    info!("Starting semantic highlighting analysis");
                    match do_semantic(&document.content, ast, &tokens) {
                        Ok(tokens) => {
                            info!("Semantic highlighting successful, {} tokens generated", tokens.len());
                            semantic_tokens = Some(tokens);
                        }
                        Err(e) => {
                            error!("Semantic highlighting failed: {}", e);
                            // Semantic highlighting failure should not affect diagnostic results
                        }
                    }
                }
                Err(err) => {
                    error!("Macro expansion or AST analysis failed: {:?}", err);
                    diagnostics.push(Diagnostic {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 1,
                            },
                        },
                        severity: Some(DiagnosticSeverity::Error),
                        code: None,
                        source: Some("onion-lsp".to_string()),
                        message: "Macro expansion or AST analysis failed".to_string(),
                        related_information: None,
                    });
                }
            }
        }
        Err(parse_error) => {
            // Parsing failed, create diagnostic information
            info!("Document parsing failed: {}", document.uri);

            // Get error message
            let error_message = match std::panic::catch_unwind(|| {
                parse_error.format(&filtered_tokens, document.content.clone())
            }) {
                Ok(msg) => {
                    // Remove ANSI color codes to get plain text
                    strip_ansi_codes(&msg)
                }
                Err(_) => "Parsing error, cannot generate specific information".to_string(),
            };

            // Get corresponding position information based on different types of parsing errors
            let diagnostic = create_diagnostic_from_parser_error(
                &parse_error,
                &filtered_tokens,
                &document.content,
                error_message,
            );

            info!(
                "Adding diagnostic: Range[{},{}]-[{},{}], Message: {}",
                diagnostic.range.start.line,
                diagnostic.range.start.character,
                diagnostic.range.end.line,
                diagnostic.range.end.character,
                &diagnostic.message
            );

            diagnostics.push(diagnostic);
        }
    }

    info!(
        "Diagnostics complete, {} diagnostics generated, semantic highlighting: {}",
        diagnostics.len(),
        if semantic_tokens.is_some() {
            "Success"
        } else {
            "None"
        }
    );

    (diagnostics, semantic_tokens)
}
/// Create diagnostic information based on parsing error type
fn create_diagnostic_from_parser_error<'t>(
    parse_error: &ParserError<'t>,
    _tokens: &Vec<Token<'t>>,
    source_code: &str,
    _error_message: String,
) -> Diagnostic {
    match parse_error {
        ParserError::UnexpectedToken(token) => {
            // Unexpected token error
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E001".to_string())),
                source: Some("onion-lsp".to_string()),
                message: format!("Unexpected token: '{}'", token.token),
                related_information: None,
            }
        }
        ParserError::UnmatchedParenthesis(opening, closing) => {
            // Unmatched parenthesis error
            let opening_range = get_token_range(opening, source_code);
            let closing_range = get_token_range(closing, source_code);

            // Create main diagnostic information
            let main_range = if closing.position > opening.position {
                Range {
                    start: opening_range.clone().start,
                    end: closing_range.clone().end,
                }
            } else {
                closing_range.clone()
            };

            // Create related diagnostic information
            let related = vec![
                DiagnosticRelatedInformation {
                    location: Location {
                        uri: "".to_string(), // Needs to be set to the correct URI
                        range: opening_range,
                    },
                    message: format!("Opening parenthesis: '{}'", opening.token),
                },
                DiagnosticRelatedInformation {
                    location: Location {
                        uri: "".to_string(), // Needs to be set to the correct URI
                        range: closing_range,
                    },
                    message: format!("Closing parenthesis: '{}'", closing.token),
                },
            ];

            Diagnostic {
                range: main_range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E002".to_string())),
                source: Some("onion-lsp".to_string()),
                message: "Unmatched parentheses".to_string(),
                related_information: Some(related),
            }
        }
        ParserError::InvalidSyntax(token) => {
            // Invalid syntax error
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E003".to_string())),
                source: Some("onion-lsp".to_string()),
                message: "Invalid syntax structure".to_string(),
                related_information: None,
            }
        }
        ParserError::NotFullyMatched(start, end) => {
            // Expression not fully matched error
            let start_range = get_token_range(start, source_code);
            let end_range = get_token_range(end, source_code);

            // Create a range that includes the entire expression
            let range = Range {
                start: start_range.start,
                end: end_range.end,
            };

            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E004".to_string())),
                source: Some("onion-lsp".to_string()),
                message: "Expression not fully matched".to_string(),
                related_information: None,
            }
        }
        ParserError::InvalidVariableName(token) => {
            // Invalid variable name error
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E005".to_string())),
                source: Some("onion-lsp".to_string()),
                message: format!("Invalid variable name: '{}'", token.token),
                related_information: None,
            }
        }
        ParserError::UnsupportedStructure(token) => {
            // Unsupported structure error
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E006".to_string())),
                source: Some("onion-lsp".to_string()),
                message: "Unsupported syntax structure".to_string(),
                related_information: None,
            }
        }
        ParserError::MissingStructure(token, expected) => {
            // Missing structure error
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E007".to_string())),
                source: Some("onion-lsp".to_string()),
                message: format!("Missing structure: '{}'", expected),
                related_information: None,
            }
        }
        ParserError::ErrorStructure(token, err) => {
            // Error structure error
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E008".to_string())),
                source: Some("onion-lsp".to_string()),
                message: format!("Erroneous structure: '{}'", err),
                related_information: None,
            }
        }
    }
}

/// Get Range from token position
fn get_token_range(token: &Token, source_code: &str) -> Range {
    let (line, column) = get_line_col(source_code, token.position);
    let token_length = token.origin_token.len();

    Range {
        start: Position {
            line: line as u32,
            character: column as u32,
        },
        end: Position {
            line: line as u32,
            character: (column + token_length) as u32,
        },
    }
}

/// Get line and column number for a byte position
fn get_line_col(text: &str, byte_pos: usize) -> (usize, usize) {
    if byte_pos >= text.len() {
        // If position is out of text bounds, return the last position
        let lines: Vec<&str> = text.lines().collect();
        if lines.is_empty() {
            return (0, 0);
        }
        return (lines.len() - 1, lines.last().unwrap().len());
    }

    let mut line = 0;
    let mut col = 0;
    let mut current_pos = 0;

    for c in text.chars() {
        if current_pos >= byte_pos {
            break;
        }

        if c == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }

        current_pos += c.len_utf8();
    }

    (line, col)
}

/// Remove ANSI color codes from a string
fn strip_ansi_codes(s: &str) -> String {
    // Simple regex to match ANSI color codes
    // This is a basic implementation, might need a dedicated library like strip_ansi_escapes
    let ansi_re = regex::Regex::new(r"\x1b\[\d+(;\d+)*m").unwrap();
    ansi_re.replace_all(s, "").to_string()
}
