use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use log::{error, info};
use onion_frontend::diagnostics::collector::DiagnosticCollector;
use onion_frontend::diagnostics::{
    Diagnostic as FrontendDiagnostic, ReportSeverity, SourceLocation,
};
use onion_frontend::parser::analyzer::{analyze_ast, auto_capture_and_rebuild};
use onion_frontend::parser::ast::{ASTNode, ASTNodeType, ast_token_stream, build_ast};
use onion_frontend::parser::comptime::solver::ComptimeSolver;
use onion_frontend::parser::lexer::{Source, Token, tokenizer};
use onion_frontend::utils::cycle_detector::CycleDetector;
use url::Url;

use super::document::TextDocument;
use super::protocol::*;
use super::semantic::{SemanticTokenTypes, do_semantic};

// =======================================================================
//                       诊断转换辅助函数
// =======================================================================

/// 将 frontend 的 Diagnostic trait 对象转换为 LSP Diagnostic
fn frontend_diagnostic_to_lsp(
    diag: &dyn FrontendDiagnostic,
    document: &TextDocument,
) -> (Option<String>, Diagnostic) {
    let severity = match diag.severity() {
        ReportSeverity::Error => DiagnosticSeverity::Error,
        ReportSeverity::Warning => DiagnosticSeverity::Warning,
    };

    let range = if let Some(location) = diag.location() {
        source_location_to_lsp_range(&location, &document.content)
    } else {
        Range::default_range()
    };

    let uri = diag
        .location()
        .and_then(|loc| loc.source.file_path().map(|path| path.to_owned()))
        .map(|path| {
            Url::from_file_path(path)
                .map(|url| url.to_string())
                .unwrap_or_else(|_| document.uri.clone())
        })
        .unwrap_or_else(|| document.uri.clone());

    (
        Some(uri),
        Diagnostic {
            range,
            severity: Some(severity),
            message: format!("{}: {}", diag.title(), diag.message()),
            source: Some("onion-lsp".to_string()),
            code: None,
            related_information: None,
        },
    )
}

/// 将 SourceLocation 转换为 LSP Range
fn source_location_to_lsp_range(location: &SourceLocation, document_content: &str) -> Range {
    let (start_char, end_char) = location.span;
    let start_pos = get_line_col_from_char_pos(document_content, start_char);
    let end_pos = get_line_col_from_char_pos(document_content, end_char);

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

/// 将 DiagnosticCollector 中的所有诊断转换为 LSP 诊断
fn collector_to_lsp_diagnostics(
    collector: &DiagnosticCollector,
    document: &TextDocument,
) -> Vec<(Option<String>, Diagnostic)> {
    collector
        .diagnostics()
        .iter()
        .map(|diag| frontend_diagnostic_to_lsp(diag.as_ref(), document))
        .collect()
}

// =======================================================================
//                       主验证函数 (Main Validation Function)
// =======================================================================

/// 使用现代化的、感知编译时计算的编译管线来验证文档。
pub fn validate_document(
    document: &TextDocument,
) -> (
    Vec<(Option<String>, Diagnostic)>,
    Option<Vec<SemanticTokenTypes>>,
) {
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
    let source = Source::from_string_with_file_path(document.content.clone(), &document.uri);
    let (ast, tokens) = match parse_source_to_ast_with_tokens(&source, document) {
        Ok(res) => res,
        Err(diag) => return (diag, None),
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
            // 收集编译时错误
            let diagnostics_guard = comptime_solver.diagnostics().read().unwrap();
            let comptime_diags = collector_to_lsp_diagnostics(&*diagnostics_guard, document);
            diagnostics.extend(comptime_diags);
            // 尽力而为：即使编译时求解失败，也尝试对原始 AST 进行语义高亮。
            let semantic_tokens = do_semantic(&document.content, &ast, &tokens).ok();
            return (diagnostics, semantic_tokens);
        }
    };
    // 收集 comptime 阶段成功后的所有诊断信息（主要是警告）
    let diagnostics_guard = comptime_solver.diagnostics().read().unwrap();
    let comptime_diags = collector_to_lsp_diagnostics(&*diagnostics_guard, document);
    diagnostics.extend(comptime_diags);
    info!("Compile-time solving successful.");

    // --- 4. 最终语义分析 ---
    info!("Starting final semantic analysis for: {}", document.uri);
    let (_required_vars, final_ast) = auto_capture_and_rebuild(&solved_ast);
    let mut diagnostics_collector = DiagnosticCollector::new();
    let _analysis_result = analyze_ast(&final_ast, &mut diagnostics_collector, &None);
    let analysis_diags = collector_to_lsp_diagnostics(&diagnostics_collector, document);
    diagnostics.extend(analysis_diags);
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
pub fn new_solver_for_file(uri: &str) -> Result<ComptimeSolver, (Option<String>, Diagnostic)> {
    let file_path = Url::parse(uri)
        .map_err(|_| {
            (
                None,
                Diagnostic {
                    range: Range::default_range(),
                    severity: Some(DiagnosticSeverity::Error),
                    message: format!("Invalid URI: {}", uri),
                    ..Default::default()
                },
            )
        })?
        .to_file_path()
        .map_err(|_| {
            (
                None,
                Diagnostic {
                    range: Range::default_range(),
                    severity: Some(DiagnosticSeverity::Error),
                    message: format!("URI is not a valid file path: {}", uri),
                    ..Default::default()
                },
            )
        })?;

    let import_cycle_detector = CycleDetector::new()
        .enter(file_path)
        .expect("Cycle detector should not fail on the first entry");

    // 假设 ComptimeSolver::new 的最终签名是这样
    Ok(ComptimeSolver::new(
        Arc::new(RwLock::new(HashMap::new())),
        import_cycle_detector,
    ))
}

/// 解析源码文本为 AST，并同时返回原始 Tokens。
pub fn parse_source_to_ast_with_tokens(
    source: &Source,
    document: &TextDocument,
) -> Result<(ASTNode, Vec<Token>), Vec<(Option<String>, Diagnostic)>> {
    let tokens = tokenizer::tokenize(source);

    let filtered_tokens = tokenizer::reject_comment(&tokens);
    if filtered_tokens.is_empty() {
        return Ok((ASTNode::new(ASTNodeType::Expressions, None, vec![]), tokens));
    }

    let gathered = ast_token_stream::from_stream(&filtered_tokens);
    let mut collector = DiagnosticCollector::new();

    build_ast(&mut collector, gathered)
        .map(|ast| (ast, tokens))
        .map_err(|_| collector_to_lsp_diagnostics(&collector, document))
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
