use onion_frontend::parser::{
    ast::{ASTNode, ASTNodeType},
    lexer::{Token, TokenType},
};

/// 语义着色器

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticTokenTypes {
    Null,        // Null
    String,      // String
    Boolean,     // Boolean
    Number,      // Number (Integer, Float)
    Base64,      // Base64
    Variable,    // Variable
    Let,         // x := expression
    Body,        // {...}
    Assign,      // x = expression
    LambdaDef,   // tuple -> body or tuple -> dyn expression
    Expressions, // expression1; expression2; ...
    LambdaCall,  // x (tuple)
    Operation,   // x + y, x - y, x * y, x / y ...
    Tuple,       // x, y, z, ...
    AssumeTuple, // ...value
    KeyValue,    // x: y
    IndexOf,     // x[y]
    GetAttr,     // x.y
    Return,      // return expression
    If,          // if expression truecondition || if expression truecondition else falsecondition
    While,       // while expression body
    Modifier,    // modifier expression
    NamedTo,     // x => y (x is name of y)
    Break,       // break
    Continue,    // continue
    Range,       // x..y
    In,
    Emit,
    AsyncLambdaCall,
    Namespace, // Type::Value
    Set,   // collection | filter
    Map,   // collection |> map
    Comment,
    Annotation, // 注解
}

pub fn do_semantic(
    code: &str,
    ast: ASTNode,
    tokens_with_comment: &Vec<Token>,
) -> Result<Vec<SemanticTokenTypes>, String> {
    let mut semantic_tokens = vec![SemanticTokenTypes::Comment; code.len()];

    // 递归处理AST节点并标记语义类型
    process_node(&ast, &mut semantic_tokens, code)?;

    // 遍历tokens_with_comment，标记注释
    for token in tokens_with_comment {
        if token.token_type == TokenType::COMMENT {
            let start = token.position;
            let end = start + token.origin_token.len();
            if start < code.len() && end <= code.len() {
                for i in start..end {
                    semantic_tokens[i] = SemanticTokenTypes::Comment;
                }
            }
        }
    }

    Ok(semantic_tokens)
}
fn process_node(
    node: &ASTNode,
    tokens: &mut Vec<SemanticTokenTypes>,
    code: &str,
) -> Result<(), String> {
    // 处理当前节点，计算它的管辖范围
    // 计算当前节点管辖的整个范围（包括子节点）
    let (range_start, range_end) = calculate_node_range(node);

    if range_start < range_end && range_start < tokens.len() && range_end <= tokens.len() {
        let token_type = match &node.node_type {
            ASTNodeType::Null => SemanticTokenTypes::Null,
            ASTNodeType::String(_) => SemanticTokenTypes::String,
            ASTNodeType::Boolean(_) => SemanticTokenTypes::Boolean,
            ASTNodeType::Number(_) => SemanticTokenTypes::Number,
            ASTNodeType::Base64(_) => SemanticTokenTypes::Base64,
            ASTNodeType::Variable(_) => SemanticTokenTypes::Variable,
            ASTNodeType::Let(_) => SemanticTokenTypes::Let,
            ASTNodeType::Body => SemanticTokenTypes::Body,
            ASTNodeType::Assign => SemanticTokenTypes::Assign,
            ASTNodeType::LambdaDef(_, _, _) => SemanticTokenTypes::LambdaDef,
            ASTNodeType::Expressions => SemanticTokenTypes::Expressions,
            ASTNodeType::LambdaCall => SemanticTokenTypes::LambdaCall,
            ASTNodeType::AsyncLambdaCall => SemanticTokenTypes::AsyncLambdaCall,
            ASTNodeType::Operation(_) => SemanticTokenTypes::Operation,
            ASTNodeType::Tuple => SemanticTokenTypes::Tuple,
            ASTNodeType::AssumeTuple => SemanticTokenTypes::AssumeTuple,
            ASTNodeType::KeyValue => SemanticTokenTypes::KeyValue,
            ASTNodeType::IndexOf => SemanticTokenTypes::IndexOf,
            ASTNodeType::GetAttr => SemanticTokenTypes::GetAttr,
            ASTNodeType::Return => SemanticTokenTypes::Return,
            ASTNodeType::If => SemanticTokenTypes::If,
            ASTNodeType::While => SemanticTokenTypes::While,
            ASTNodeType::Modifier(_) => SemanticTokenTypes::Modifier,
            ASTNodeType::NamedTo => SemanticTokenTypes::NamedTo,
            ASTNodeType::Break => SemanticTokenTypes::Break,
            ASTNodeType::Continue => SemanticTokenTypes::Continue,
            ASTNodeType::Range => SemanticTokenTypes::Range,
            ASTNodeType::In => SemanticTokenTypes::In,
            ASTNodeType::Emit => SemanticTokenTypes::Emit,
            ASTNodeType::Namespace(_) => SemanticTokenTypes::Namespace,
            ASTNodeType::Set => SemanticTokenTypes::Set,
            ASTNodeType::Map => SemanticTokenTypes::Map,
            ASTNodeType::Annotation(_) => SemanticTokenTypes::Annotation,
            ASTNodeType::Is => SemanticTokenTypes::Operation,
            _ => SemanticTokenTypes::Variable, // 默认情况
        };

        // 标记节点的token，但跳过空白字符
        for i in range_start..range_end {
            if i < tokens.len() {
                // 检查当前字符是否为空白字符
                if i < code.len() {
                    let ch = code.as_bytes()[i] as char;
                    if !ch.is_whitespace() {
                        tokens[i] = token_type;
                    }
                } else {
                    tokens[i] = token_type;
                }
            }
        }

        match &node.node_type {
            ASTNodeType::Let(_) => {
                // 标记当前token为variable
                let start = node.start_token.as_ref().map_or(0, |t| t.position);
                let end = start
                    + node
                        .start_token
                        .as_ref()
                        .map_or(0, |t| t.origin_token.len());
                for i in start..end {
                    if i < tokens.len() {
                        tokens[i] = SemanticTokenTypes::Variable;
                    }
                }
            }
            _ => {}
        };
    }

    for child in &node.children {
        process_node(child, tokens, code)?;
    }

    match &node.node_type {
        ASTNodeType::LambdaCall => {
            // 如果子节点是简单的变量，标记为LambdaCall
            if let ASTNodeType::Variable(_) = node.children[0].node_type {
                let start = node.children[0]
                    .start_token
                    .as_ref()
                    .map_or(0, |t| t.position);
                let end = start
                    + node.children[0]
                        .end_token
                        .as_ref()
                        .map_or(0, |t| t.origin_token.len());
                for i in start..end {
                    if i < tokens.len() {
                        tokens[i] = SemanticTokenTypes::LambdaCall;
                    }
                }
            }
        }
        ASTNodeType::AsyncLambdaCall => {
            // 如果子节点是简单的变量，标记为AsyncLambdaCall
            if let ASTNodeType::Variable(_) = node.children[0].node_type {
                let start = node.children[0]
                    .start_token
                    .as_ref()
                    .map_or(0, |t| t.position);
                let end = start
                    + node.children[0]
                        .end_token
                        .as_ref()
                        .map_or(0, |t| t.origin_token.len());
                for i in start..end {
                    if i < tokens.len() {
                        tokens[i] = SemanticTokenTypes::AsyncLambdaCall;
                    }
                }
            }
        }
        ASTNodeType::GetAttr => {
            // 如果属性是简单的string，标记为GetAttr
            if let ASTNodeType::String(_) = node.children[1].node_type {
                let start = node.children[1]
                    .start_token
                    .as_ref()
                    .map_or(0, |t| t.position);
                let end = start
                    + node.children[1]
                        .end_token
                        .as_ref()
                        .map_or(0, |t| t.origin_token.len());
                for i in start..end {
                    if i < tokens.len() {
                        tokens[i] = SemanticTokenTypes::GetAttr;
                    }
                }
            }
        }
        _ => {}
    }

    Ok(())
}

// 计算节点管辖的范围
fn calculate_node_range(node: &ASTNode) -> (usize, usize) {
    let mut min_pos = usize::MAX;
    let mut max_pos = 0;

    // 考虑当前节点自身的token
    if let Some(token) = &node.start_token {
        min_pos = min_pos.min(token.position);
    }

    if let Some(token) = &node.end_token {
        max_pos = max_pos.max(token.position + token.origin_token.len());
    }

    // // 递归考虑所有子节点的范围
    // for child in &node.children {
    //     let (child_min, child_max) = calculate_node_range(child);
    //     min_pos = min_pos.min(child_min);
    //     max_pos = max_pos.max(child_max);
    // }

    // 如果没有token，返回默认范围
    if min_pos == usize::MAX {
        return (usize::MAX, 0);
    }

    (min_pos, max_pos)
}
/// 将语义标记编码为LSP协议所需的格式 (Semantic Tokens) - Corrected for Line Breaks
///
/// LSP 语义标记数据格式是一个 `Vec<u32>`，其中每 5 个 u32 代表一个语义标记：
/// 1. delta_line: 相对于前一个标记的行号增量。
/// 2. delta_start_char: 相对于前一个标记起始字符的增量 (UTF-16)。新行则为绝对起始位置。
/// 3. length: 标记的长度 (UTF-16)。
/// 4. token_type: 标记类型的索引 (由 legend 决定)。
/// 5. token_modifiers_bitmask: 标记修饰符位掩码 (默认为 0)。
///
/// 此版本确保语义标记不会跨越换行符进行合并。
///
/// # Arguments
///
/// * `tokens`: 一个 `SemanticTokenTypes` 的 slice，长度等于 `text` 的字节数。
/// * `text`: 需要进行语义着色的原始文本。
///
/// # Returns
///
/// 符合 LSP 规范的 `Vec<u32>` 编码数据。
pub(super) fn encode_semantic_tokens(tokens: &[SemanticTokenTypes], text: &str) -> Vec<u32> {
    assert_eq!(
        tokens.len(),
        text.as_bytes().len(),
        "Tokens length must match text byte length"
    );

    let get_token_type_index = |token_type: &SemanticTokenTypes| -> Option<u32> {
        match token_type {
            SemanticTokenTypes::Null => Some(22),
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
            SemanticTokenTypes::AsyncLambdaCall => Some(32),
            SemanticTokenTypes::Operation => Some(33),
            SemanticTokenTypes::Tuple => Some(34),
            SemanticTokenTypes::AssumeTuple => Some(35),
            SemanticTokenTypes::KeyValue => Some(36),
            SemanticTokenTypes::IndexOf => Some(37),
            SemanticTokenTypes::GetAttr => Some(38),
            SemanticTokenTypes::Return => Some(39),
            SemanticTokenTypes::If => Some(41),
            SemanticTokenTypes::While => Some(42),
            SemanticTokenTypes::Modifier => Some(15),
            SemanticTokenTypes::NamedTo => Some(43),
            SemanticTokenTypes::Break => Some(44),
            SemanticTokenTypes::Continue => Some(45),
            SemanticTokenTypes::Range => Some(46),
            SemanticTokenTypes::In => Some(47),
            SemanticTokenTypes::Emit => Some(48),
            SemanticTokenTypes::Namespace => Some(49),
            SemanticTokenTypes::Set => Some(50),
            SemanticTokenTypes::Map => Some(51),
            SemanticTokenTypes::Comment => Some(17),
            SemanticTokenTypes::Annotation => Some(14),
        }
    };

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

    let get_token_modifiers = |token_type: &SemanticTokenTypes| -> u32 {
        match token_type {
            SemanticTokenTypes::KeyValue => MOD_DEFINITION | MOD_DECLARATION, // KeyValue 通常是定义
            SemanticTokenTypes::Let => MOD_DECLARATION | MOD_DEFINITION,
            SemanticTokenTypes::Variable => MOD_NONE, // 变量使用可能没有，声明时可能有
            SemanticTokenTypes::LambdaDef => MOD_DECLARATION | MOD_DEFINITION,
            SemanticTokenTypes::AsyncLambdaCall => MOD_ASYNC,
            SemanticTokenTypes::Emit => MOD_ASYNC,
            SemanticTokenTypes::Assign => MOD_MODIFICATION,
            SemanticTokenTypes::Operation => MOD_MODIFICATION, // 不确定，看具体操作
            SemanticTokenTypes::Modifier => MOD_NONE,          // 修饰符本身通常不带修饰符
            SemanticTokenTypes::GetAttr => MOD_READONLY,       // 通常是读取
            SemanticTokenTypes::IndexOf => MOD_READONLY,       // 通常是读取
            SemanticTokenTypes::Range => MOD_READONLY,         // 范围通常是只读的
            _ => MOD_NONE,
        }
    };
    // --- State Initialization (保持不变) ---
    let mut result = Vec::new();
    let mut previous_line = 0;
    let mut previous_char_utf16 = 0;

    let mut current_line = 0;
    let mut current_char_utf16 = 0; // Tracks UTF-16 position on the current line

    // --- State for the currently accumulating token ---
    let mut current_original_token_type: Option<SemanticTokenTypes> = None;
    let mut token_start_line = 0;
    let mut token_start_char_utf16 = 0;
    let mut current_token_len_utf16 = 0;

    // Helper closure to finalize and push a token
    let finalize_token = |result: &mut Vec<u32>,
                          token_type_opt: Option<SemanticTokenTypes>,
                          start_line: u32,
                          start_char: u32,
                          len: u32,
                          prev_line: &mut u32,
                          prev_char: &mut u32| {
        if let Some(token_type) = token_type_opt {
            // 仅当 token 类型有效（非Null且有映射）时才生成
            if let Some(lsp_token_index) = get_token_type_index(&token_type) {
                let delta_line = start_line - *prev_line;
                let delta_start_char = if delta_line == 0 {
                    start_char - *prev_char
                } else {
                    start_char // 新行，绝对位置
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

    // --- Iterate through characters ---
    for (byte_idx, ch) in text.char_indices() {
        let char_len_utf16 = ch.len_utf16() as u32;
        // 获取此字符开始处的原始语义类型
        let byte_semantic_type = tokens
            .get(byte_idx)
            .copied()
            .unwrap_or(SemanticTokenTypes::Null); // 默认值处理边界情况

        // --- NEW: Check for Newline FIRST ---
        if ch == '\n' {
            // Finalize any token accumulating *before* the newline
            finalize_token(
                &mut result,
                current_original_token_type,
                token_start_line,
                token_start_char_utf16,
                current_token_len_utf16,
                &mut previous_line,
                &mut previous_char_utf16,
            );

            // Reset accumulation state for the new line
            current_original_token_type = None;
            current_token_len_utf16 = 0;

            // Advance position trackers AFTER finalizing
            current_line += 1;
            current_char_utf16 = 0;
            continue; // Move to the next character, skip normal processing for newline
        }

        // --- Logic for non-newline characters ---

        // Check if the ORIGINAL token type changes
        let type_changed = match current_original_token_type {
            Some(current_type) => current_type != byte_semantic_type,
            None => byte_semantic_type != SemanticTokenTypes::Null, // Start if not null
        };

        if type_changed {
            // Finalize the *previous* token (if one exists)
            finalize_token(
                &mut result,
                current_original_token_type,
                token_start_line,
                token_start_char_utf16,
                current_token_len_utf16,
                &mut previous_line,
                &mut previous_char_utf16,
            );

            current_original_token_type = Some(byte_semantic_type);
            token_start_line = current_line;
            token_start_char_utf16 = current_char_utf16;
            current_token_len_utf16 = char_len_utf16;
        } else {
            // Type is the same, continue the current token (if one is active)
            if current_original_token_type.is_some() {
                current_token_len_utf16 += char_len_utf16;
            }
            // If current_original_token_type is None (e.g., consecutive Nulls), do nothing
        }

        // --- Advance character position (only for non-newline chars) ---
        current_char_utf16 += char_len_utf16;
    }

    // --- Finalize the very last token after the loop ---
    finalize_token(
        &mut result,
        current_original_token_type,
        token_start_line,
        token_start_char_utf16,
        current_token_len_utf16,
        &mut previous_line,
        &mut previous_char_utf16,
    );

    result
}
