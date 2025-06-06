use log::{debug, error, info};
use onion_frontend::parser::analyzer::auto_capture_and_rebuild;
use onion_frontend::parser::analyzer::expand_macro;
use onion_frontend::utils::cycle_detector;
use url::Url;

use super::document::TextDocument;
use super::protocol::*;
use super::semantic::SemanticTokenTypes;
use crate::lsp::semantic::do_semantic;
use onion_frontend::dir_stack::DirStack;
use onion_frontend::parser::analyzer::analyze_ast;
use onion_frontend::parser::ast::ast_token_stream;
use onion_frontend::parser::ast::build_ast;
use onion_frontend::parser::ast::ParserError;
use onion_frontend::parser::lexer::lexer;
use onion_frontend::parser::lexer::Token;
/// 验证文档并生成诊断信息
/// 返回诊断信息和可选的语义着色结果
pub fn validate_document(
    document: &TextDocument,
) -> (Vec<Diagnostic>, Option<Vec<SemanticTokenTypes>>) {
    let mut diagnostics = Vec::new();
    let mut semantic_tokens = None;

    // 检查文档是否为空
    if document.content.is_empty() {
        info!("文档内容为空，跳过验证: {}", document.uri);
        return (diagnostics, semantic_tokens);
    }

    info!("正在进行词法分析: {}", document.uri);

    // 进行词法分析
    let tokens = match std::panic::catch_unwind(|| lexer::tokenize(&document.content)) {
        Ok(tokens) => tokens,
        Err(e) => {
            error!("词法分析过程中发生错误: {:?}", e);
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
                    message: "词法分析失败: 可能包含无法识别的标记".to_string(),
                    related_information: None,
                }],
                None,
            );
        }
    };

    info!("词法分析完成，获得 {} 个标记", tokens.len());

    let filtered_tokens = lexer::reject_comment(&tokens);
    debug!("过滤注释后有 {} 个标记", filtered_tokens.len());

    if filtered_tokens.is_empty() {
        info!("过滤后无有效标记，可能只有注释或空白: {}", document.uri);
        return (diagnostics, semantic_tokens);
    }

    // 尝试解析AST
    info!("开始语法分析");
    let gathered =
        match std::panic::catch_unwind(|| ast_token_stream::from_stream(&filtered_tokens)) {
            Ok(gathered) => gathered,
            Err(e) => {
                error!("标记流处理失败: {:?}", e);
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
                        message: "处理标记流时发生错误".to_string(),
                        related_information: None,
                    }],
                    None,
                );
            }
        };

    match build_ast(gathered) {
        Ok(ast) => {
            // 解析成功，没有错误
            info!("文档解析成功: {}", document.uri);
            info!("分析变量定义: {}", document.uri);
            // 1. 解析 URI
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

            // 2. 获取父目录
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
            let dir_stack = DirStack::new(Some(&parent_dir));
            if let Err(err) = &dir_stack {
                let err_msg = err.to_string();
                error!("目录栈初始化失败: {}", err_msg);
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
                        message: format!("目录栈初始化失败: {}", err_msg),
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
                                "变量 '{}' 未明确定义",
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
                            message: format!("宏定义错误: {}", msg),
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
                            message: format!("错误: {}", msg),
                            related_information: None,
                        });
                    }
                    onion_frontend::parser::analyzer::AnalyzeError::DetailedContextLessError(
                        _,
                        msg,
                    ) => {
                        // 没有range信息
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
                            message: format!("错误: {}", msg),
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
                                "变量 '{}' 未明确定义",
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
                            message: format!("宏定义错误: {}", msg),
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
                            message: format!("错误: {}", msg),
                            related_information: None,
                        });
                    }
                    onion_frontend::parser::analyzer::AnalyzeError::DetailedContextLessError(
                        _,
                        msg,
                    ) => {
                        // 没有range信息
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
                            message: format!("错误: {}", msg),
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

                    // 进行语义着色处理
                    info!("开始进行语义着色分析");
                    match do_semantic(&document.content, ast, &tokens) {
                        Ok(tokens) => {
                            info!("语义着色处理成功，生成了 {} 个标记", tokens.len());
                            semantic_tokens = Some(tokens);
                        }
                        Err(e) => {
                            error!("语义着色处理失败: {}", e);
                            // 语义着色失败不应该影响诊断结果
                        }
                    }
                }
                Err(err) => {
                    error!("宏展开或AST分析失败: {:?}", err);
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
                        message: "宏展开或AST分析失败".to_string(),
                        related_information: None,
                    });
                }
            }
        }
        Err(parse_error) => {
            // 解析失败，创建诊断信息
            info!("文档解析失败: {}", document.uri);

            // 获取错误消息
            let error_message = match std::panic::catch_unwind(|| {
                parse_error.format(&filtered_tokens, document.content.clone())
            }) {
                Ok(msg) => {
                    // 移除ANSI颜色代码以获得纯文本
                    strip_ansi_codes(&msg)
                }
                Err(_) => "解析错误，无法生成具体信息".to_string(),
            };

            // 根据不同类型的解析错误获取相应的位置信息
            let diagnostic = create_diagnostic_from_parser_error(
                &parse_error,
                &filtered_tokens,
                &document.content,
                error_message,
            );

            info!(
                "添加诊断信息: 范围[{},{}]-[{},{}], 消息: {}",
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
        "诊断完成，生成了 {} 个诊断信息，语义着色: {}",
        diagnostics.len(),
        if semantic_tokens.is_some() {
            "成功"
        } else {
            "无"
        }
    );

    (diagnostics, semantic_tokens)
}
/// 根据解析错误类型创建诊断信息
fn create_diagnostic_from_parser_error<'t>(
    parse_error: &ParserError<'t>,
    _tokens: &Vec<Token<'t>>,
    source_code: &str,
    _error_message: String,
) -> Diagnostic {
    match parse_error {
        ParserError::UnexpectedToken(token) => {
            // 意外标记错误
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E001".to_string())),
                source: Some("onion-lsp".to_string()),
                message: format!("意外标记: '{}'", token.token),
                related_information: None,
            }
        }
        ParserError::UnmatchedParenthesis(opening, closing) => {
            // 不匹配的括号错误
            let opening_range = get_token_range(opening, source_code);
            let closing_range = get_token_range(closing, source_code);

            // 创建主诊断信息
            let main_range = if closing.position > opening.position {
                Range {
                    start: opening_range.clone().start,
                    end: closing_range.clone().end,
                }
            } else {
                closing_range.clone()
            };

            // 创建相关诊断信息
            let related = vec![
                DiagnosticRelatedInformation {
                    location: Location {
                        uri: "".to_string(), // 需要设置正确的URI
                        range: opening_range,
                    },
                    message: format!("开始括号: '{}'", opening.token),
                },
                DiagnosticRelatedInformation {
                    location: Location {
                        uri: "".to_string(), // 需要设置正确的URI
                        range: closing_range,
                    },
                    message: format!("结束括号: '{}'", closing.token),
                },
            ];

            Diagnostic {
                range: main_range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E002".to_string())),
                source: Some("onion-lsp".to_string()),
                message: "括号不匹配".to_string(),
                related_information: Some(related),
            }
        }
        ParserError::InvalidSyntax(token) => {
            // 无效语法错误
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E003".to_string())),
                source: Some("onion-lsp".to_string()),
                message: "无效的语法结构".to_string(),
                related_information: None,
            }
        }
        ParserError::NotFullyMatched(start, end) => {
            // 表达式未完全匹配错误
            let start_range = get_token_range(start, source_code);
            let end_range = get_token_range(end, source_code);

            // 创建包含整个表达式的范围
            let range = Range {
                start: start_range.start,
                end: end_range.end,
            };

            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E004".to_string())),
                source: Some("onion-lsp".to_string()),
                message: "表达式未完全匹配".to_string(),
                related_information: None,
            }
        }
        ParserError::InvalidVariableName(token) => {
            // 无效变量名错误
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E005".to_string())),
                source: Some("onion-lsp".to_string()),
                message: format!("无效的变量名: '{}'", token.token),
                related_information: None,
            }
        }
        ParserError::UnsupportedStructure(token) => {
            // 不支持的结构错误
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E006".to_string())),
                source: Some("onion-lsp".to_string()),
                message: "不支持的语法结构".to_string(),
                related_information: None,
            }
        }
        ParserError::MissingStructure(token, expected) => {
            // 缺失结构错误
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E007".to_string())),
                source: Some("onion-lsp".to_string()),
                message: format!("缺失的结构: '{}'", expected),
                related_information: None,
            }
        }
        ParserError::ErrorStructure(token, err) => {
            // 错误结构错误
            let range = get_token_range(token, source_code);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                code: Some(serde_json::Value::String("AST-E008".to_string())),
                source: Some("onion-lsp".to_string()),
                message: format!("错误的结构: '{}'", err),
                related_information: None,
            }
        }
    }
}

/// 从token位置获取Range
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

/// 获取字节位置对应的行列号
fn get_line_col(text: &str, byte_pos: usize) -> (usize, usize) {
    if byte_pos >= text.len() {
        // 如果位置超出文本范围，返回最后位置
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

/// 从字符串中移除ANSI颜色代码
fn strip_ansi_codes(s: &str) -> String {
    // 简单的正则表达式匹配ANSI颜色代码
    // 这是一个基本实现，可能需要使用专门的库如 strip_ansi_escapes
    let ansi_re = regex::Regex::new(r"\x1b\[\d+(;\d+)*m").unwrap();
    ansi_re.replace_all(s, "").to_string()
}
