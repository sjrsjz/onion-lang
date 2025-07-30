use onion_frontend::parser::{
    ast::{ASTNode, ASTNodeType},
    lexer::{Token, TokenType},
};

/// 语义着色器
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticTokenTypes {
    Null,
    Variable,
    String,
    Number,
    Boolean,
    Comment,
    LambdaCall,
    Operation,
    Let,
    Assign,
    Return,
    If,
    While,
    Body,
    Tuple,
    KeyValue,
    GetAttr,
    Modifier,
    LambdaDef,
    Expressions,
    AssumeTuple,
    Break,
    Continue,
    Range,
    In,
    Namespace,
    Set,
    Map,
    Annotation,
    Undefined,
    Base64,
}

impl From<ASTNodeType> for SemanticTokenTypes {
    fn from(node_type: ASTNodeType) -> Self {
        match node_type {
            ASTNodeType::Null => Self::Null,
            ASTNodeType::Undefined => Self::Undefined,
            ASTNodeType::String(_) => Self::String,
            ASTNodeType::Boolean(_) => Self::Boolean,
            ASTNodeType::Number(_) => Self::Number,
            ASTNodeType::Base64(_) => Self::Base64,
            ASTNodeType::Variable(_) => Self::Variable,
            ASTNodeType::Let(_) => Self::Let,
            ASTNodeType::Body => Self::Body,
            ASTNodeType::Assign => Self::Assign,
            ASTNodeType::LambdaDef(_, _) => Self::LambdaDef,
            ASTNodeType::Expressions => Self::Expressions,
            ASTNodeType::Apply => Self::LambdaCall,
            ASTNodeType::Operation(_) => Self::Operation,
            ASTNodeType::Tuple => Self::Tuple,
            ASTNodeType::AssumeTuple => Self::AssumeTuple,
            ASTNodeType::KeyValue => Self::KeyValue,
            ASTNodeType::GetAttr => Self::GetAttr,
            ASTNodeType::Return => Self::Return,
            ASTNodeType::If => Self::If,
            ASTNodeType::While => Self::While,
            ASTNodeType::Modifier(_) => Self::Modifier,
            ASTNodeType::Break => Self::Break,
            ASTNodeType::Continue => Self::Continue,
            ASTNodeType::Range => Self::Range,
            ASTNodeType::In => Self::In,
            ASTNodeType::Namespace(_) => Self::Namespace,
            ASTNodeType::Set => Self::Set,
            ASTNodeType::Map => Self::Map,
            ASTNodeType::Annotation(_) => Self::Annotation,
            ASTNodeType::Is => Self::Operation,
            _ => Self::Variable, // 默认情况
        }
    }
}

/// 对 AST 进行语义分析，生成用于着色的标记。
/// 此版本通过字符到字节的映射来解决多字节字符（如中文）的错位问题。
pub fn do_semantic(
    code: &str,
    ast: ASTNode,
    tokens_with_comment: &Vec<Token>,
) -> Result<Vec<SemanticTokenTypes>, String> {
    // 1. 创建字节图：一个与源码字节长度相同的向量，用于标记每个字节的语义类型。
    let mut semantic_tokens = vec![SemanticTokenTypes::Null; code.len()];

    // 2. 创建字符索引到字节索引的映射表。这是确保非 ASCII 字符正确的关键。
    //    lexer 返回的 token span 是基于字符的，我们需要将它转换为字节偏移。
    let mut char_to_byte_map: Vec<usize> =
        code.char_indices().map(|(byte_idx, _)| byte_idx).collect();
    // 添加一个额外的元素，指向字符串的末尾，使得获取最后一个字符的范围成为可能。
    char_to_byte_map.push(code.len());

    // 3. 获取根节点的 Source 对象用于比较
    let root_source = if let Some(token) = &ast.start_token {
        Some(token.source_code().clone().into())
    } else if let Some(token) = &ast.end_token {
        Some(token.source_code().clone().into())
    } else {
        None
    };

    // 4. 严格遵循先序遍历，在字节图上标记类型。
    process_node(
        &ast,
        &mut semantic_tokens,
        code,
        &char_to_byte_map,
        root_source.as_ref(),
    )?;

    // 5. 最后覆盖注释，确保最高优先级。
    for token in tokens_with_comment {
        if token.token_type() == TokenType::COMMENT {
            // 使用字符到字节的映射来获取正确的字节范围
            if let Some((start_byte, end_byte)) =
                get_byte_span(token.origin_token_span(), &char_to_byte_map)
            {
                mark_byte_range_as(
                    start_byte,
                    end_byte,
                    SemanticTokenTypes::Comment,
                    &mut semantic_tokens,
                    false,
                    code,
                );
            }
        }
    }

    Ok(semantic_tokens)
}

/// 递归处理 AST 节点，标记语义类型。
fn process_node(
    node: &ASTNode,
    tokens: &mut Vec<SemanticTokenTypes>,
    code: &str,
    char_map: &Vec<usize>,
    root_source: Option<&std::sync::Arc<Vec<char>>>,
) -> Result<(), String> {
    // 1. 检查节点是否属于当前文件
    if let Some(root_src) = root_source {
        if !is_node_from_current_file(node, root_src) {
            // 如果节点不属于当前文件（来自@macro或@import），跳过着色但继续处理子节点
            for child in &node.children {
                process_node(child, tokens, code, char_map, root_source)?;
            }
            return Ok(());
        }
    }

    // 2. 计算当前节点管辖的字节范围
    if let Some((start_byte, end_byte)) = calculate_node_byte_range(node, char_map) {
        let semantic_type = SemanticTokenTypes::from(node.node_type.clone());

        // 3. 先序处理：首先标记父节点范围 (并跳过空白)
        mark_byte_range_as(start_byte, end_byte, semantic_type, tokens, true, code);
    }

    // 4. 递归处理子节点
    for child in &node.children {
        process_node(child, tokens, code, char_map, root_source)?;
    }

    // 5. 特殊覆盖逻辑：对特定节点类型进行更精确的着色
    match &node.node_type {
        ASTNodeType::Let(_) => {
            if let Some(token) = &node.start_token {
                if let Some((start, end)) = get_byte_span(token.origin_token_span(), char_map) {
                    mark_byte_range_as(
                        start,
                        end,
                        SemanticTokenTypes::Variable,
                        tokens,
                        true,
                        code,
                    );
                }
            }
        }
        ASTNodeType::Apply => {
            if let Some(child) = node.children.get(0) {
                if let ASTNodeType::Variable(_) = child.node_type {
                    if let Some((start, end)) = calculate_node_byte_range(child, char_map) {
                        mark_byte_range_as(
                            start,
                            end,
                            SemanticTokenTypes::LambdaCall,
                            tokens,
                            true,
                            code,
                        );
                    }
                }
            }
        }
        ASTNodeType::GetAttr => {
            if let Some(child) = node.children.get(1) {
                if let ASTNodeType::String(_) = child.node_type {
                    if let Some((start, end)) = calculate_node_byte_range(child, char_map) {
                        mark_byte_range_as(
                            start,
                            end,
                            SemanticTokenTypes::GetAttr,
                            tokens,
                            true,
                            code,
                        );
                    }
                }
            }
        }
        _ => {}
    }

    Ok(())
}

/// 计算节点管辖的字节范围 (start_byte, end_byte)。
fn calculate_node_byte_range(node: &ASTNode, char_map: &Vec<usize>) -> Option<(usize, usize)> {
    let start_token = node.start_token.as_ref()?;
    let end_token = node.end_token.as_ref()?;

    // token span 是基于字符的
    let (start_char, _) = start_token.origin_token_span();
    let (_, end_char) = end_token.origin_token_span();

    // 将字符 span 转换为字节 span
    get_byte_span((start_char, end_char), char_map)
}

/// 将字符索引范围转换为字节索引范围。
fn get_byte_span(char_span: (usize, usize), char_map: &Vec<usize>) -> Option<(usize, usize)> {
    let start_byte = *char_map.get(char_span.0)?;
    let end_byte = *char_map.get(char_span.1)?;
    Some((start_byte, end_byte))
}

/// 将字节图中的一个范围标记为指定的语义类型。
/// 这是正确处理多字节字符的关键。
fn mark_byte_range_as(
    start: usize,
    end: usize,
    semantic_type: SemanticTokenTypes,
    tokens: &mut Vec<SemanticTokenTypes>,
    skip_whitespace: bool,
    code: &str,
) {
    if start >= end {
        return;
    }

    let end = end.min(code.len());
    let start = start.min(end);

    if skip_whitespace {
        // 正确处理UTF-8字符的逻辑
        let mut byte_pos = start;
        while byte_pos < end {
            // 确保我们从一个字符的起始字节开始处理
            if code.is_char_boundary(byte_pos) {
                // 获取从当前位置开始的下一个字符
                if let Some(ch) = code[byte_pos..].chars().next() {
                    let char_len = ch.len_utf8();
                    // 确保整个字符都在指定的范围内
                    if byte_pos + char_len <= end {
                        if !ch.is_whitespace() {
                            // 将组成该字符的所有字节都标记为指定的类型
                            for i in 0..char_len {
                                let absolute_index = byte_pos + i;
                                if absolute_index < tokens.len() {
                                    tokens[absolute_index] = semantic_type;
                                }
                            }
                        }
                        // 移动到下一个字符的起始位置
                        byte_pos += char_len;
                    } else {
                        // 字符超出了范围，结束循环
                        break;
                    }
                } else {
                    // 无法获取字符，结束循环
                    break;
                }
            } else {
                // 如果不在字符边界上（例如，一个多字节字符的中间），
                // 则向前移动一个字节，直到找到下一个字符边界。
                byte_pos += 1;
            }
        }
    } else {
        // 如果不跳过空白，直接标记整个字节范围
        for i in start..end {
            if i < tokens.len() {
                tokens[i] = semantic_type;
            }
        }
    }
}

/// 检查节点是否来自当前文件
/// 通过比较节点的token的Source对象与根节点的Source对象来判断
fn is_node_from_current_file(node: &ASTNode, root_source: &std::sync::Arc<Vec<char>>) -> bool {
    // 检查start_token
    if let Some(start_token) = &node.start_token {
        let node_source: std::sync::Arc<Vec<char>> = start_token.source_code().clone().into();
        if !std::sync::Arc::ptr_eq(&node_source, root_source) {
            return false;
        }
    }

    // 检查end_token
    if let Some(end_token) = &node.end_token {
        let node_source: std::sync::Arc<Vec<char>> = end_token.source_code().clone().into();
        if !std::sync::Arc::ptr_eq(&node_source, root_source) {
            return false;
        }
    }

    true
}

// --- LSP 编码函数（无需修改，保持完整）---

#[allow(dead_code)]
const MOD_NONE: u32 = 0;
#[allow(dead_code)]
const MOD_DECLARATION: u32 = 1 << 0;
#[allow(dead_code)]
const MOD_DEFINITION: u32 = 1 << 1;
#[allow(dead_code)]
const MOD_READONLY: u32 = 1 << 2;
#[allow(dead_code)]
const MOD_STATIC: u32 = 1 << 3;
#[allow(dead_code)]
const MOD_DEPRECATED: u32 = 1 << 4;
#[allow(dead_code)]
const MOD_ABSTRACT: u32 = 1 << 5;
#[allow(dead_code)]
const MOD_ASYNC: u32 = 1 << 6;
#[allow(dead_code)]
const MOD_MODIFICATION: u32 = 1 << 7;
#[allow(dead_code)]
const MOD_DOCUMENTATION: u32 = 1 << 8;
#[allow(dead_code)]
const MOD_DEFAULT: u32 = 1 << 9;

fn get_token_type_index(token_type: &SemanticTokenTypes) -> Option<u32> {
    match token_type {
        SemanticTokenTypes::Null => None,
        SemanticTokenTypes::Undefined => Some(22),
        SemanticTokenTypes::String => Some(18),
        SemanticTokenTypes::Boolean => Some(23),
        SemanticTokenTypes::Number => Some(19),
        SemanticTokenTypes::Base64 => Some(24),
        SemanticTokenTypes::Variable => Some(8),
        SemanticTokenTypes::Let => Some(25),
        SemanticTokenTypes::Body => Some(26),
        SemanticTokenTypes::Assign => Some(28),
        SemanticTokenTypes::LambdaDef => Some(29),
        SemanticTokenTypes::Expressions => Some(30),
        SemanticTokenTypes::LambdaCall => Some(31),
        SemanticTokenTypes::Operation => Some(33),
        SemanticTokenTypes::Tuple => Some(34),
        SemanticTokenTypes::AssumeTuple => Some(35),
        SemanticTokenTypes::KeyValue => Some(36),
        SemanticTokenTypes::GetAttr => Some(38),
        SemanticTokenTypes::Return => Some(39),
        SemanticTokenTypes::If => Some(41),
        SemanticTokenTypes::While => Some(42),
        SemanticTokenTypes::Modifier => Some(15),
        SemanticTokenTypes::Break => Some(44),
        SemanticTokenTypes::Continue => Some(45),
        SemanticTokenTypes::Range => Some(46),
        SemanticTokenTypes::In => Some(47),
        SemanticTokenTypes::Namespace => Some(49),
        SemanticTokenTypes::Set => Some(50),
        SemanticTokenTypes::Map => Some(51),
        SemanticTokenTypes::Comment => Some(17),
        SemanticTokenTypes::Annotation => Some(14),
    }
}

fn get_token_modifiers(token_type: &SemanticTokenTypes) -> u32 {
    match token_type {
        SemanticTokenTypes::KeyValue => MOD_DEFINITION | MOD_DECLARATION,
        SemanticTokenTypes::Let => MOD_DECLARATION | MOD_DEFINITION,
        SemanticTokenTypes::Variable => MOD_NONE,
        SemanticTokenTypes::LambdaDef => MOD_DECLARATION | MOD_DEFINITION,
        SemanticTokenTypes::Assign => MOD_MODIFICATION,
        SemanticTokenTypes::GetAttr => MOD_READONLY,
        _ => MOD_NONE,
    }
}

pub fn encode_semantic_tokens(tokens: &[SemanticTokenTypes], text: &str) -> Vec<u32> {
    assert_eq!(
        tokens.len(),
        text.len(),
        "Tokens length must match text byte length"
    );

    let mut result = Vec::new();
    let mut previous_line = 0;
    let mut previous_char_utf16 = 0;
    let mut current_line = 0;
    let mut current_char_utf16 = 0;
    let mut current_semantic_type: Option<SemanticTokenTypes> = None;
    let mut token_start_line = 0;
    let mut token_start_char_utf16 = 0;
    let mut current_token_len_utf16 = 0;

    let finalize_token = |result: &mut Vec<u32>,
                          token_type_opt: Option<SemanticTokenTypes>,
                          start_line: u32,
                          start_char: u32,
                          len: u32,
                          prev_line: &mut u32,
                          prev_char: &mut u32| {
        if len == 0 {
            return;
        }
        if let Some(token_type) = token_type_opt {
            if let Some(lsp_token_index) = get_token_type_index(&token_type) {
                let delta_line = start_line - *prev_line;
                let delta_start_char = if delta_line == 0 {
                    start_char - *prev_char
                } else {
                    start_char
                };
                let token_modifiers = get_token_modifiers(&token_type);
                result.extend_from_slice(&[
                    delta_line,
                    delta_start_char,
                    len,
                    lsp_token_index,
                    token_modifiers,
                ]);
                *prev_line = start_line;
                *prev_char = start_char;
            }
        }
    };

    for (byte_idx, ch) in text.char_indices() {
        let char_len_utf16 = ch.len_utf16() as u32;
        let char_len_utf8 = ch.len_utf8();

        // 对于多字节字符，我们需要获取这个字符的第一个字节的语义类型
        let semantic_type = tokens
            .get(byte_idx)
            .copied()
            .unwrap_or(SemanticTokenTypes::Null);

        if ch == '\n' {
            finalize_token(
                &mut result,
                current_semantic_type,
                token_start_line,
                token_start_char_utf16,
                current_token_len_utf16,
                &mut previous_line,
                &mut previous_char_utf16,
            );
            current_semantic_type = None;
            current_token_len_utf16 = 0;
            current_line += 1;
            current_char_utf16 = 0;
            continue;
        }

        // 检查类型是否发生变化
        // 对于多字节字符，我们需要检查组成这个字符的所有字节是否都有相同的语义类型
        let unified_semantic_type = semantic_type;

        // 如果是多字节字符，检查所有字节是否有相同的语义类型
        if char_len_utf8 > 1 {
            for i in 1..char_len_utf8 {
                if let Some(byte_type) = tokens.get(byte_idx + i) {
                    if *byte_type != semantic_type {
                        // 如果字符的不同字节有不同的语义类型，使用第一个字节的类型
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        let type_changed = match current_semantic_type {
            Some(current_type) => current_type != unified_semantic_type,
            None => unified_semantic_type != SemanticTokenTypes::Null,
        };

        if type_changed {
            finalize_token(
                &mut result,
                current_semantic_type,
                token_start_line,
                token_start_char_utf16,
                current_token_len_utf16,
                &mut previous_line,
                &mut previous_char_utf16,
            );
            current_semantic_type = Some(unified_semantic_type);
            token_start_line = current_line;
            token_start_char_utf16 = current_char_utf16;
            current_token_len_utf16 = char_len_utf16;
        } else if current_semantic_type.is_some() {
            current_token_len_utf16 += char_len_utf16;
        }

        current_char_utf16 += char_len_utf16;
    }

    finalize_token(
        &mut result,
        current_semantic_type,
        token_start_line,
        token_start_char_utf16,
        current_token_len_utf16,
        &mut previous_line,
        &mut previous_char_utf16,
    );

    result
}
