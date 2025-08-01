use log::{debug, error, info};
use onion_frontend::parser::analyzer::{
    analyze_ast, auto_capture_and_rebuild, expand_macro, AnalysisResult,
};
use onion_frontend::parser::ast::{ast_token_stream, build_ast, ASTNode, ParserError};
use onion_frontend::parser::lexer::{lexer, Token};
use onion_frontend::utils::cycle_detector;
use url::Url;

use super::document::TextDocument;
use super::protocol::*;
use super::semantic::{do_semantic, SemanticTokenTypes};
use onion_frontend::dir_stack::DirectoryStack;

/// Validates the document, generating diagnostics and semantic tokens.
pub fn validate_document(
    document: &TextDocument,
) -> (Vec<Diagnostic>, Option<Vec<SemanticTokenTypes>>) {
    if document.content.is_empty() {
        info!(
            "Document content is empty, skipping validation: {}",
            document.uri
        );
        return (Vec::new(), None);
    }

    info!("Performing lexical analysis: {}", document.uri);
    let tokens = match std::panic::catch_unwind(|| lexer::tokenize(&document.content)) {
        Ok(tokens) => tokens,
        Err(e) => {
            error!("Panic during lexical analysis: {e:?}");
            let diag = Diagnostic {
                range: Range::default_range(),
                severity: Some(DiagnosticSeverity::Error),
                source: Some("onion-lsp".to_string()),
                message: "Lexical analysis failed unexpectedly. This may be a bug in the lexer."
                    .to_string(),
                ..Default::default()
            };
            return (vec![diag], None);
        }
    };
    info!("Lexical analysis complete, {} tokens found", tokens.len());

    let filtered_tokens = lexer::reject_comment(&tokens);
    debug!("{} tokens after filtering comments", filtered_tokens.len());
    if filtered_tokens.is_empty() {
        return (Vec::new(), None);
    }

    info!("Starting syntax analysis");
    // 注意：ast_token_stream::from_stream 不应 panic，如果它 panic，说明有逻辑错误
    let gathered = ast_token_stream::from_stream(&filtered_tokens);

    match build_ast(gathered) {
        Ok(ast) => {
            info!(
                "Document parsed successfully. Starting semantic analysis: {}",
                document.uri
            );
            // Semantic analysis logic starts here
            let mut diagnostics = Vec::new();

            // Setup directory context for analysis
            let file_path = match Url::parse(&document.uri) {
                Ok(url) if url.scheme() == "file" => url.to_file_path().unwrap(),
                _ => {
                    error!("Invalid or non-file URI: {}", document.uri);
                    return (diagnostics, None);
                }
            };
            let parent_dir = file_path.parent().unwrap();
            let mut dir_stack = match DirectoryStack::new(Some(parent_dir)) {
                Ok(stack) => stack,
                Err(e) => {
                    error!("Directory stack initialization failed: {e}");
                    diagnostics.push(Diagnostic {
                        range: Range::default_range(),
                        severity: Some(DiagnosticSeverity::Error),
                        message: format!("Directory stack failed: {e}"),
                        ..Default::default()
                    });
                    return (diagnostics, None);
                }
            };

            let mut cycle_detector = cycle_detector::CycleDetector::new();
            let file_path_str = file_path.to_str().unwrap_or_default().to_string();
            let visit_result = cycle_detector.visit(file_path_str);
            match visit_result {
                Ok(mut guard) => {
                    // --- Macro Expansion Phase ---
                    let macro_result = expand_macro(&ast, guard.get_detector_mut(), &mut dir_stack);
                    diagnostics.extend(process_analysis_results(&macro_result, document));

                    // --- Main Analysis Phase ---
                    let ast_after_macro = auto_capture_and_rebuild(&macro_result.result_node).1;
                    let analysis_result = analyze_ast(
                        &ast_after_macro,
                        None,
                        guard.get_detector_mut(),
                        &mut dir_stack,
                    );
                    diagnostics.extend(process_analysis_results(&analysis_result, document));

                    // --- Semantic Highlighting ---
                    info!("Starting semantic highlighting analysis");
                    let semantic_tokens =
                        match do_semantic(&document.content, ast_after_macro, &tokens) {
                            Ok(tokens) => {
                                info!(
                                    "Semantic highlighting successful, {} tokens generated",
                                    tokens.len()
                                );
                                Some(tokens)
                            }
                            Err(e) => {
                                error!("Semantic highlighting failed: {e}");
                                None
                            }
                        };
                    (diagnostics, semantic_tokens)
                }
                Err(err) => {
                    error!("Cyclic dependency detected: {err:?}");
                    diagnostics.push(Diagnostic {
                        range: Range::default_range(),
                        severity: Some(DiagnosticSeverity::Error),
                        message: format!("Cyclic dependency detected: {err}"),
                        ..Default::default()
                    });
                    (diagnostics, None)
                }
            }
        }
        Err(parse_error) => {
            info!("Document parsing failed: {}", document.uri);
            // 直接从结构化的 ParserError 创建诊断信息
            let diagnostic = create_diagnostic_from_parser_error(&parse_error, document);
            (vec![diagnostic], None)
        }
    }
}

fn process_analysis_results<T: AnalysisResult>(
    output: &T,
    document: &TextDocument,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Process errors by calling the trait method
    for error in output.errors() {
        let diagnostic = match error {
            // ... the body of this match statement remains exactly the same
            onion_frontend::parser::analyzer::AnalyzeError::UndefinedVariable(node) => Diagnostic {
                range: get_node_range(node, &document.content),
                severity: Some(DiagnosticSeverity::Error),
                message: format!(
                    "Undefined variable: '{}'",
                    node.start_token
                        .as_ref()
                        .map_or("".to_string(), |t| t.origin_token())
                ),
                ..Default::default()
            },
            onion_frontend::parser::analyzer::AnalyzeError::InvalidMacroDefinition(node, msg) => {
                Diagnostic {
                    range: get_node_range(node, &document.content),
                    severity: Some(DiagnosticSeverity::Error),
                    message: format!("Invalid macro definition: {msg}"),
                    ..Default::default()
                }
            }
            onion_frontend::parser::analyzer::AnalyzeError::ParserError(p_err) => {
                create_diagnostic_from_parser_error(p_err, document)
            }
            onion_frontend::parser::analyzer::AnalyzeError::DetailedError(node, msg) => {
                Diagnostic {
                    range: get_node_range(node, &document.content),
                    severity: Some(DiagnosticSeverity::Error),
                    message: msg.clone(),
                    ..Default::default()
                }
            }
        };
        diagnostics.push(diagnostic);
    }

    // Process warnings by calling the trait method
    for warn in output.warnings() {
        let diagnostic = match warn {
            // ... the body of this match statement also remains the same
            onion_frontend::parser::analyzer::AnalyzeWarn::CompileError(node, msg) => Diagnostic {
                range: get_node_range(node, &document.content),
                severity: Some(DiagnosticSeverity::Warning),
                message: format!("@compile warning: {msg}"),
                ..Default::default()
            },
        };
        diagnostics.push(diagnostic);
    }
    diagnostics
}
/// **(Rewritten Helper)** Creates an LSP Diagnostic from a ParserError.
fn create_diagnostic_from_parser_error(
    parse_error: &ParserError,
    document: &TextDocument,
) -> Diagnostic {
    match parse_error {
        ParserError::UnexpectedToken(token)
        | ParserError::InvalidSyntax(token)
        | ParserError::InvalidVariableName(token)
        | ParserError::UnsupportedStructure(token) => Diagnostic {
            range: get_token_range(token, &document.content),
            severity: Some(DiagnosticSeverity::Error),
            message: parse_error.format(), // Use the Display impl for a concise message
            ..Default::default()
        },
        ParserError::MissingStructure(token, expected) => Diagnostic {
            range: get_token_range(token, &document.content),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Missing structure: Expected '{expected}' here"),
            ..Default::default()
        },
        ParserError::ErrorStructure(token, err) => Diagnostic {
            range: get_token_range(token, &document.content),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Erroneous structure: {err}"),
            ..Default::default()
        },
        ParserError::NotFullyMatched(start, end) => Diagnostic {
            range: get_range_between_tokens(start, end, &document.content),
            severity: Some(DiagnosticSeverity::Error),
            message: "Expression not fully matched".to_string(),
            ..Default::default()
        },
        ParserError::UnmatchedParenthesis(opening, closing) => {
            let opening_range = get_token_range(opening, &document.content);
            let closing_range = get_token_range(closing, &document.content);

            Diagnostic {
                range: closing_range.clone(),
                severity: Some(DiagnosticSeverity::Error),
                message: "Unmatched parenthesis".to_string(),
                related_information: Some(vec![DiagnosticRelatedInformation {
                    location: Location {
                        uri: document.uri.clone(),
                        range: opening_range,
                    },
                    message: "This is the corresponding opening parenthesis.".to_string(),
                }]),
                ..Default::default()
            }
        }
    }
}

/// **(Rewritten Helper)** Gets the LSP Range for a single Token.
fn get_token_range(token: &Token, source_code: &str) -> Range {
    get_range_between_tokens(token, token, source_code)
}

/// **(New Helper)** Gets the LSP Range for an entire ASTNode.
fn get_node_range(node: &ASTNode, source_code: &str) -> Range {
    match (&node.start_token, &node.end_token) {
        (Some(start), Some(end)) => get_range_between_tokens(start, end, source_code),
        (Some(token), None) | (None, Some(token)) => get_token_range(token, source_code),
        (None, None) => Range::default_range(), // No token, default to start of file
    }
}

/// **(New Helper)** Gets the LSP Range between a start and end Token.
fn get_range_between_tokens(start: &Token, end: &Token, source_code: &str) -> Range {
    let (start_char, _) = start.origin_token_span();
    let (_, end_char) = end.origin_token_span();

    let start_pos = get_line_col_from_char_pos(source_code, start_char);
    let end_pos = get_line_col_from_char_pos(source_code, end_char);

    Range {
        start: Position {
            line: start_pos.0 as u32,
            character: start_pos.1 as u32,
        },
        end: Position {
            line: end_pos.0 as u32,
            character: end_pos.1 as u32,
        },
    }
}

/// **(Rewritten Helper)** Gets (line, column) from a character position in the text.
fn get_line_col_from_char_pos(text: &str, char_pos: usize) -> (usize, usize) {
    let mut line = 0;
    let mut last_newline_char_pos = 0;

    // Clamp the position to be within bounds
    let char_pos = char_pos.min(text.chars().count());

    for (i, c) in text.chars().enumerate() {
        if i >= char_pos {
            break;
        }
        if c == '\n' {
            line += 1;
            last_newline_char_pos = i + 1;
        }
    }
    let column = char_pos - last_newline_char_pos;
    (line, column)
}

// Add a default implementation for Range to simplify Diagnostic creation
impl Range {
    fn default_range() -> Self {
        Self {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 1,
            },
        }
    }
}
