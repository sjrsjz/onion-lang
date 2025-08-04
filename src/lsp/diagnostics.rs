use std::collections::HashMap;
use std::panic::catch_unwind;

use log::{error, info};
use onion_frontend::parser::analyzer::{
    AnalysisResult, AnalyzeError, AnalyzeWarn, analyze_ast, auto_capture_and_rebuild,
};
use onion_frontend::parser::ast::{ASTNode, ASTNodeType, ParserError, ast_token_stream, build_ast};
use onion_frontend::parser::comptime::solver::{ComptimeError, ComptimeSolver, ComptimeWarning};
use onion_frontend::parser::lexer::{Token, lexer};
use onion_frontend::utils::cycle_detector::CycleDetector;
use url::Url;

use super::document::TextDocument;
use super::protocol::*;
use super::semantic::{SemanticTokenTypes, do_semantic};

// =======================================================================
//                       主验证函数 (Main Validation Function)
// =======================================================================

/// 使用现代化的、感知编译时计算的编译管线来验证文档。
pub fn validate_document(
    document: &TextDocument,
) -> (Vec<Diagnostic>, Option<Vec<SemanticTokenTypes>>) {
    // 返回 SemanticToken
    if document.content.is_empty() {
        info!(
            "Document content is empty, skipping validation: {}",
            document.uri
        );
        return (Vec::new(), None);
    }

    // --- 1. 词法和语法分析 ---
    info!(
        "Performing lexical and syntax analysis for: {}",
        document.uri
    );
    let (ast, tokens) = match parse_source_to_ast_with_tokens(&document.content, document) {
        Ok(res) => res,
        Err(diag) => return (vec![diag], None),
    };
    info!("Syntax analysis successful.");

    // --- 2. 初始化编译时环境 ---
    let mut diagnostics = Vec::new();
    let solver_result = new_solver_for_file(&document.uri);
    let mut comptime_solver = match solver_result {
        Ok(solver) => solver,
        Err(diag) => return (vec![diag], None),
    };

    // --- 3. 编译时求解与宏展开 ---
    info!("Starting compile-time solving for: {}", document.uri);
    let solved_ast = match comptime_solver.solve(&ast) {
        Ok(solved_ast) => solved_ast,
        Err(_) => {
            error!("Comptime solving failed for: {}", document.uri);
            diagnostics.extend(process_comptime_results(&comptime_solver, document));
            // 尽力而为：即使编译时求解失败，也尝试对原始 AST 进行语义高亮。
            let semantic_tokens = do_semantic(&document.content, &ast, &tokens).ok();
            return (diagnostics, semantic_tokens);
        }
    };
    // 收集 comptime 阶段成功后的所有诊断信息（主要是警告）
    diagnostics.extend(process_comptime_results(&comptime_solver, document));
    info!("Compile-time solving successful.");

    // --- 4. 最终语义分析 ---
    info!("Starting final semantic analysis for: {}", document.uri);
    let (_required_vars, final_ast) = auto_capture_and_rebuild(&solved_ast);
    let analysis_result = analyze_ast(&final_ast, None); // 假设 analyze_ast 仍然需要
    diagnostics.extend(process_analysis_results(&analysis_result, document));
    info!("Final semantic analysis complete.");

    // --- 5. 语义高亮 ---
    info!("Starting semantic highlighting analysis.");
    let semantic_tokens = match do_semantic(&document.content, &ast, &tokens) {
        Ok(tokens) => {
            info!(
                "Semantic highlighting successful, {} tokens generated",
                tokens.len()
            );
            Some(tokens)
        }
        Err(e) => {
            error!("Semantic highlighting failed: {}", e);
            None
        }
    };

    (diagnostics, semantic_tokens)
}

// =======================================================================
//                        核心辅助结构与函数
// =======================================================================

// 为 ComptimeSolver 实现一个专门用于 LSP 的构造函数
pub fn new_solver_for_file(uri: &str) -> Result<ComptimeSolver, Diagnostic> {
    let file_path = Url::parse(uri)
        .map_err(|_| Diagnostic {
            range: Range::default_range(),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Invalid URI: {}", uri),
            ..Default::default()
        })?
        .to_file_path()
        .map_err(|_| Diagnostic {
            range: Range::default_range(),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("URI is not a valid file path: {}", uri),
            ..Default::default()
        })?;

    let import_cycle_detector = CycleDetector::new()
        .enter(file_path)
        .expect("Cycle detector should not fail on the first entry");

    // 假设 ComptimeSolver::new 的最终签名是这样
    Ok(ComptimeSolver::new(HashMap::new(), import_cycle_detector))
}

/// 将 ComptimeSolver 的结果转换为 LSP Diagnostics 的统一入口。
fn process_comptime_results(solver: &ComptimeSolver, document: &TextDocument) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for error in solver.errors() {
        diagnostics.extend(comptime_error_to_diagnostics(error, document));
    }
    for warning in solver.warnings() {
        diagnostics.extend(comptime_warning_to_diagnostics(warning, document));
    }

    diagnostics
}

/// 将 ComptimeError 转换为 LSP Diagnostics。
fn comptime_error_to_diagnostics(
    error: &(ComptimeError, ASTNode),
    document: &TextDocument,
) -> Vec<Diagnostic> {
    match &error.0 {
        ComptimeError::AnalysisError(errors) => errors
            .iter()
            .map(|e| diagnostic_from_analyze_error(e, document))
            .collect(),
        ComptimeError::RuntimeError(err) => vec![Diagnostic {
            range: get_node_range(&error.1, &document.content),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Compile-time runtime error: {}", err),
            source: Some("onion-lsp (comptime)".to_string()),
            ..Default::default()
        }],
        ComptimeError::IRGeneratorError(err) => vec![Diagnostic {
            range: get_node_range(&error.1, &document.content),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Internal compiler error (IR generation): {}", err),
            source: Some("onion-lsp (internal)".to_string()),
            ..Default::default()
        }],
        ComptimeError::IRTranslatorError(err) => vec![Diagnostic {
            range: get_node_range(&error.1, &document.content),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Internal compiler error (IR translation): {}", err),
            source: Some("onion-lsp (internal)".to_string()),
            ..Default::default()
        }],
    }
}

/// 将 ComptimeWarning 转换为 LSP Diagnostics。
fn comptime_warning_to_diagnostics(
    warning: &(ComptimeWarning, ASTNode),
    document: &TextDocument,
) -> Vec<Diagnostic> {
    match &warning.0 {
        ComptimeWarning::AnalysisWarning(warnings) => warnings
            .iter()
            .map(|w| diagnostic_from_analyze_warn(w, document))
            .collect(),
    }
}

// =======================================================================
//                已有的、重构后的错误处理与位置计算辅助函数
// =======================================================================

/// **(新)** 将单个 AnalyzeError 转换为 Diagnostic。
fn diagnostic_from_analyze_error(error: &AnalyzeError, document: &TextDocument) -> Diagnostic {
    match error {
        AnalyzeError::UndefinedVariable(node) => Diagnostic {
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
        AnalyzeError::InvalidMacroDefinition(node, msg) => Diagnostic {
            range: get_node_range(node, &document.content),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Invalid macro definition: {}", msg),
            ..Default::default()
        },
        AnalyzeError::ParserError(p_err) => create_diagnostic_from_parser_error(p_err, document),
        AnalyzeError::DetailedError(node, msg) => Diagnostic {
            range: get_node_range(node, &document.content),
            severity: Some(DiagnosticSeverity::Error),
            message: msg.clone(),
            ..Default::default()
        },
    }
}

/// **(新)** 将单个 AnalyzeWarn 转换为 Diagnostic。
fn diagnostic_from_analyze_warn(warn: &AnalyzeWarn, document: &TextDocument) -> Diagnostic {
    match warn {
        AnalyzeWarn::CompileError(node, msg) => Diagnostic {
            range: get_node_range(node, &document.content),
            severity: Some(DiagnosticSeverity::Warning),
            message: format!("@compile warning: {}", msg),
            ..Default::default()
        },
    }
}

/// 将 `AnalysisResult` trait 对象的结果转换为 Diagnostics。
fn process_analysis_results<T: AnalysisResult>(
    output: &T,
    document: &TextDocument,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for error in output.errors() {
        diagnostics.push(diagnostic_from_analyze_error(error, document));
    }
    for warn in output.warnings() {
        diagnostics.push(diagnostic_from_analyze_warn(warn, document));
    }
    diagnostics
}

/// 解析源码文本为 AST，并同时返回原始 Tokens。
pub fn parse_source_to_ast_with_tokens(
    source: &str,
    document: &TextDocument,
) -> Result<(ASTNode, Vec<Token>), Diagnostic> {
    let tokens = match catch_unwind(|| lexer::tokenize(source)) {
        Ok(tokens) => tokens,
        Err(e) => {
            error!("Panic during lexical analysis: {:?}", e);
            return Err(Diagnostic {
                range: Range::default_range(),
                severity: Some(DiagnosticSeverity::Error),
                source: Some("onion-lsp".to_string()),
                message: "Lexical analysis failed unexpectedly (panic). This is a bug.".to_string(),
                ..Default::default()
            });
        }
    };

    let filtered_tokens = lexer::reject_comment(&tokens);
    if filtered_tokens.is_empty() {
        return Ok((
            ASTNode::new(ASTNodeType::Expressions, None, None, Some(vec![])),
            tokens,
        ));
    }

    let gathered = ast_token_stream::from_stream(&filtered_tokens);
    build_ast(gathered)
        .map(|ast| (ast, tokens))
        .map_err(|parse_error| create_diagnostic_from_parser_error(&parse_error, document))
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
