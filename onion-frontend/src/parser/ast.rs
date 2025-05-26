use std::{fmt::Debug, vec};

use crate::parser::lexer::{Token, TokenType};

#[derive(Debug)]
pub enum ParserError<'t> {
    UnexpectedToken(&'t Token<'t>),                     // Token
    UnmatchedParenthesis(&'t Token<'t>, &'t Token<'t>), // (opening, closing)
    InvalidSyntax(&'t Token<'t>),
    NotFullyMatched(&'t Token<'t>, &'t Token<'t>),
    InvalidVariableName(&'t Token<'t>),
    UnsupportedStructure(&'t Token<'t>),
    MissingStructure(&'t Token<'t>, String), // (token, expected structure)
    ErrorStructure(&'t Token<'t>, String),   // (token, expected structure)
}

impl ParserError<'_> {
    pub fn format(&self, _tokens: &Vec<Token>, source_code: String) -> String {
        use colored::*;
        use unicode_segmentation::UnicodeSegmentation;

        // Split source code into lines
        let lines: Vec<&str> = source_code.lines().collect();

        // Helper function to find line and column from position
        let find_position = |byte_pos: usize| -> (usize, usize) {
            let mut current_byte = 0;
            for (line_num, line) in lines.iter().enumerate() {
                // 计算行长度（包括换行符）
                // Windows通常使用CRLF (\r\n)，而Unix使用LF (\n)
                // 我们需要检测使用的是哪种换行符
                let eol_len = if source_code.contains("\r\n") { 2 } else { 1 };
                let line_bytes = line.len() + eol_len; // 加上实际的换行符长度

                if current_byte + line_bytes > byte_pos {
                    // 计算行内的字节偏移
                    let line_offset = byte_pos - current_byte;

                    // 边界检查
                    if line_offset > line.len() {
                        return (line_num, line.graphemes(true).count()); // 位置在行尾
                    }

                    // 找到有效的字符边界
                    let valid_offset = line
                        .char_indices()
                        .map(|(i, _)| i)
                        .take_while(|&i| i <= line_offset)
                        .last()
                        .unwrap_or(0);

                    // 使用有效的字节偏移获取文本
                    let column_text = &line[..valid_offset];
                    let column = column_text.graphemes(true).count();
                    return (line_num, column);
                }
                current_byte += line_bytes;
            }
            (lines.len().saturating_sub(1), 0) // Default to last line
        };

        match self {
            ParserError::UnexpectedToken(token) => {
                let (line_num, col) = find_position(token.position);
                let line = if line_num < lines.len() {
                    lines[line_num]
                } else {
                    ""
                };

                let mut error_msg = format!(
                    "{}: {}\n\n",
                    "Parse Error".bright_red().bold(),
                    format!("Unexpected token '{}'", token.token).yellow()
                );
                error_msg.push_str(&format!(
                    "{} {}:{}\n",
                    "Position".bright_blue(),
                    (line_num + 1).to_string().bright_cyan(),
                    (col + 1).to_string().bright_cyan()
                ));
                error_msg.push_str(&format!("{}\n", line.white()));
                error_msg.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(col),
                    "^".repeat(token.origin_token.len()).bright_red().bold()
                ));

                error_msg
            }

            ParserError::UnmatchedParenthesis(opening, closing) => {
                let (opening_line, opening_col) = find_position(opening.position);
                let (closing_line, closing_col) = find_position(closing.position);

                let mut error_msg = format!(
                    "{}: {}\n\n",
                    "Parse Error".bright_red().bold(),
                    "Unmatched parenthesis".yellow()
                );

                // Display opening parenthesis position
                if opening_line < lines.len() {
                    let line = lines[opening_line];
                    error_msg.push_str(&format!(
                        "{} '{}' at {}:{}\n",
                        "Opening".bright_green(),
                        opening.token.bright_white(),
                        (opening_line + 1).to_string().bright_cyan(),
                        (opening_col + 1).to_string().bright_cyan()
                    ));
                    error_msg.push_str(&format!("{}\n", line.white()));
                    error_msg.push_str(&format!(
                        "{}{}\n\n",
                        " ".repeat(opening_col),
                        "^".bright_red().bold()
                    ));
                }

                // Display closing parenthesis position
                if closing_line < lines.len() {
                    let line = lines[closing_line];
                    error_msg.push_str(&format!(
                        "{} '{}' at {}:{}\n",
                        "Closing".bright_green(),
                        closing.token.bright_white(),
                        (closing_line + 1).to_string().bright_cyan(),
                        (closing_col + 1).to_string().bright_cyan()
                    ));
                    error_msg.push_str(&format!("{}\n", line.white()));
                    error_msg.push_str(&format!(
                        "{}{}\n",
                        " ".repeat(closing_col),
                        "^".bright_red().bold()
                    ));
                }

                error_msg
            }

            ParserError::InvalidSyntax(token) => {
                let (line_num, col) = find_position(token.position);
                let line = if line_num < lines.len() {
                    lines[line_num]
                } else {
                    ""
                };

                let mut error_msg = format!(
                    "{}: {}\n\n",
                    "Syntax Error".bright_red().bold(),
                    "Invalid syntax".yellow()
                );
                error_msg.push_str(&format!(
                    "{} {}:{}\n",
                    "Position".bright_blue(),
                    (line_num + 1).to_string().bright_cyan(),
                    (col + 1).to_string().bright_cyan()
                ));
                error_msg.push_str(&format!("{}\n", line.white()));
                error_msg.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(col),
                    "^".repeat(token.origin_token.len()).bright_red().bold()
                ));

                error_msg
            }

            ParserError::NotFullyMatched(start, end) => {
                let (start_line, start_col) = find_position(start.position);
                let (end_line, end_col) = find_position(end.position);

                let mut error_msg = format!(
                    "{}: {}\n\n",
                    "Parse Error".bright_red().bold(),
                    "Expression not fully matched".yellow()
                );

                if start_line == end_line && start_line < lines.len() {
                    // If on the same line
                    let line = lines[start_line];
                    let underline_length = end.position + end.origin_token.len() - start.position;

                    error_msg.push_str(&format!(
                        "{} {}:{}-{}:{}\n",
                        "Position".bright_blue(),
                        (start_line + 1).to_string().bright_cyan(),
                        (start_col + 1).to_string().bright_cyan(),
                        (end_line + 1).to_string().bright_cyan(),
                        (end_col + 1 + end.origin_token.len())
                            .to_string()
                            .bright_cyan()
                    ));
                    error_msg.push_str(&format!("  {}\n", line.white()));
                    error_msg.push_str(&format!(
                        "  {}{}\n",
                        " ".repeat(start_col),
                        "~".repeat(underline_length).bright_red().bold()
                    ));
                } else {
                    // If spans multiple lines
                    error_msg.push_str(&format!(
                        "{} {}:{}\n",
                        "Starting position".bright_blue(),
                        (start_line + 1).to_string().bright_cyan(),
                        (start_col + 1).to_string().bright_cyan()
                    ));
                    if start_line < lines.len() {
                        let line = lines[start_line];
                        error_msg.push_str(&format!("    {}\n", line.white()));
                        error_msg.push_str(&format!("   ┌{}^\n", "─".repeat(start_col),));
                    }

                    for line_num in (start_line + 1)..end_line {
                        if line_num < lines.len() {
                            let line = lines[line_num];
                            error_msg.push_str(&format!("   │{}\n", line.white()));
                        }
                    }

                    if end_line < lines.len() {
                        let line = lines[end_line];
                        error_msg.push_str(&format!("   │{}\n", line.white()));
                        error_msg.push_str(&format!(
                            "   └{}{}^\n",
                            " ".repeat(0),
                            "─"
                                .repeat(end_col + end.origin_token.len())
                                .bright_red()
                                .bold(),
                        ));
                    }

                    error_msg.push_str(&format!(
                        "{} {}:{}\n",
                        "Ending position".bright_blue(),
                        (end_line + 1).to_string().bright_cyan(),
                        (end_col + 1).to_string().bright_cyan()
                    ));
                }

                error_msg
            }

            ParserError::InvalidVariableName(token) => {
                let (line_num, col) = find_position(token.position);
                let line = if line_num < lines.len() {
                    lines[line_num]
                } else {
                    ""
                };

                let mut error_msg = format!(
                    "{}: {}\n\n",
                    "Parse Error".bright_red().bold(),
                    format!("Invalid variable name '{}'", token.origin_token).yellow()
                );
                error_msg.push_str(&format!(
                    "{} {}:{}\n",
                    "Position".bright_blue(),
                    (line_num + 1).to_string().bright_cyan(),
                    (col + 1).to_string().bright_cyan()
                ));
                error_msg.push_str(&format!("{}\n", line.white()));
                error_msg.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(col),
                    "^".repeat(token.origin_token.len()).bright_red().bold()
                ));

                error_msg
            }

            ParserError::UnsupportedStructure(token) => {
                let (line_num, col) = find_position(token.position);
                let line = if line_num < lines.len() {
                    lines[line_num]
                } else {
                    ""
                };

                let mut error_msg = format!(
                    "{}: {}\n\n",
                    "Parse Error".bright_red().bold(),
                    "Unsupported structure".yellow()
                );
                error_msg.push_str(&format!(
                    "{} {}:{}\n",
                    "Position".bright_blue(),
                    (line_num + 1).to_string().bright_cyan(),
                    (col + 1).to_string().bright_cyan()
                ));
                error_msg.push_str(&format!("{}\n", line.white()));
                error_msg.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(col),
                    "^".repeat(token.origin_token.len()).bright_red().bold()
                ));

                // 添加帮助提示
                error_msg.push_str(&format!(
                    "\n{} {}\n",
                    "Hint:".bright_green().bold(),
                    "Check syntax or try breaking down complex expressions"
                        .bright_white()
                        .italic()
                ));

                error_msg
            }

            ParserError::MissingStructure(token, structure) => {
                let (line_num, col) = find_position(token.position);
                let line = if line_num < lines.len() {
                    lines[line_num]
                } else {
                    ""
                };

                let mut error_msg = format!(
                    "{}: {}\n\n",
                    "Parse Error".bright_red().bold(),
                    format!("Missing structure: {}", structure).yellow()
                );
                error_msg.push_str(&format!(
                    "{} {}:{}\n",
                    "Position".bright_blue(),
                    (line_num + 1).to_string().bright_cyan(),
                    (col + 1).to_string().bright_cyan()
                ));
                error_msg.push_str(&format!("{}\n", line.white()));
                error_msg.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(col),
                    "^".repeat(token.origin_token.len()).bright_red().bold()
                ));

                error_msg
            }

            ParserError::ErrorStructure(token, structure) => {
                let (line_num, col) = find_position(token.position);
                let line = if line_num < lines.len() {
                    lines[line_num]
                } else {
                    ""
                };

                let mut error_msg = format!(
                    "{}: {}\n\n",
                    "Parse Error".bright_red().bold(),
                    format!("Error structure: {}", structure).yellow()
                );
                error_msg.push_str(&format!(
                    "{} {}:{}\n",
                    "Position".bright_blue(),
                    (line_num + 1).to_string().bright_cyan(),
                    (col + 1).to_string().bright_cyan()
                ));
                error_msg.push_str(&format!("{}\n", line.white()));
                error_msg.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(col),
                    "^".repeat(token.origin_token.len()).bright_red().bold()
                ));

                error_msg
            }
        }
    }
}

pub type TokenStream<'t> = Vec<Token<'t>>;
pub type GatheredTokens<'t> = &'t [Token<'t>];

pub mod ast_token_stream {
    pub fn from_stream<'t>(stream: &'t super::TokenStream<'t>) -> super::GatheredTokens<'t> {
        stream.as_slice()
    }
}

fn get_next_tokens(
    tokens: GatheredTokens<'_>,
    current: usize,
) -> Result<GatheredTokens<'_>, ParserError<'_>> {
    let mut stack = Vec::<(&str, usize)>::new();
    let mut next_tokens_end = 0usize;
    let mut index = current;
    if index >= (*tokens).len() {
        return Ok(&[]);
    }
    loop {
        if ["{", "[", "("].contains(&tokens[index].token)
            && tokens[index].token_type == TokenType::SYMBOL
        {
            stack.push((tokens[index].token, index));
            next_tokens_end += 1;
        } else if ["}", "]", ")"].contains(&tokens[index].token)
            && tokens[index].token_type == TokenType::SYMBOL
        {
            if stack.is_empty() {
                break;
            }
            let (last, last_position) = stack.pop().unwrap();
            if (last == "{" && tokens[index].token != "}")
                || (last == "[" && tokens[index].token != "]")
                || (last == "(" && tokens[index].token != ")")
            {
                return Err(ParserError::UnmatchedParenthesis(
                    &tokens[last_position],
                    &tokens[index],
                ));
            }

            next_tokens_end += 1;
        } else {
            next_tokens_end += 1;
        }
        index += 1;
        if index >= (tokens).len() || stack.is_empty() {
            break;
        }
    }
    if !stack.is_empty() {
        let (_, last_position) = stack.pop().unwrap();
        return Err(ParserError::UnmatchedParenthesis(
            &tokens[last_position],
            &tokens[index - 1],
        ));
    }
    Ok(&tokens[current..current + next_tokens_end])
}

fn gather(tokens: GatheredTokens<'_>) -> Result<Vec<GatheredTokens<'_>>, ParserError<'_>> {
    let mut current = 0;
    let mut result = Vec::<GatheredTokens>::new();
    while current < tokens.len() {
        let next_tokens = get_next_tokens(tokens, current)?;
        if next_tokens.is_empty() {
            return Err(ParserError::UnsupportedStructure(&tokens[current]));
        }
        current += next_tokens.len();
        result.push(next_tokens);
    }
    Ok(result)
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNodeType {
    None,                        // No expression
    Null,                        // Null
    String(String),              // String
    Boolean(String),             // Boolean
    Number(String),              // Number (Integer, Float)
    Base64(String),              // Base64
    Variable(String),            // Variable
    Let(String),                 // x := expression
    Body,                        // {...}
    Assign,                      // x = expression
    LambdaDef(bool, bool, bool), // tuple -> body or tuple -> dyn expression or tuple -> &capture body or dynamic tuple -> body
    Expressions,                 // expression1; expression2; ...
    LambdaCall,                  // x (tuple)
    Operation(ASTNodeOperation), // x + y, x - y, x * y, x / y ...
    Tuple,                       // x, y, z, ...
    AssumeTuple,                 // ...value
    KeyValue,                    // x: y
    IndexOf,                     // x[y]
    GetAttr,                     // x.y
    Return,                      // return expression
    If,    // if expression truecondition || if expression truecondition else falsecondition
    While, // while expression body
    Modifier(ASTNodeModifier), // modifier expression
    NamedTo, // x => y (x is name of y)
    Break, // break
    Continue, // continue
    Range, // x..y
    In,
    Emit,
    AsyncLambdaCall,
    Namespace(String),      // Type::Value
    Set,                // collection | filter
    Map,                // collection |> map
    Annotation(String), // @annotation expr
    Is, // x is y
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ASTNodeOperation {
    Add,          // +
    Subtract,     // -
    Multiply,     // *
    Divide,       // /
    Modulus,      // %
    Power,        // **
    And,          // and
    Xor,          // xor
    Or,           // or
    Not,          // not
    Equal,        // ==
    NotEqual,     // !=
    Greater,      // >
    Less,         // <
    GreaterEqual, // >=
    LessEqual,    // <=
    LeftShift,    // << (left shift)
    RightShift,   // >> (right shift)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ASTNodeModifier {
    DeepCopy, // DeepCopy
    Copy,     // Copy
    Mut,      // Mut
    Const,    // Const
    KeyOf,    // KeyOf
    ValueOf,  // ValueOf
    Assert,   // Assert
    Import,   // Import
    TypeOf,   // TypeOf
    Await,
    BindSelf,
    Collect,
    LengthOf,  // LengthOf
}

#[derive(Debug, Clone)]
pub struct ASTNode<'t> {
    pub node_type: ASTNodeType,             // Type of the node
    pub start_token: Option<&'t Token<'t>>, // Start Token associated with the node
    pub end_token: Option<&'t Token<'t>>,   // End token associated with the node
    pub children: Vec<ASTNode<'t>>,         // Children of the node
}

impl ASTNode<'_> {
    pub fn new<'t>(
        node_type: ASTNodeType,
        start_token: Option<&'t Token>,
        end_token: Option<&'t Token>,
        children: Option<Vec<ASTNode<'t>>>,
    ) -> ASTNode<'t> {
        ASTNode {
            node_type,
            start_token,
            end_token,
            children: children.unwrap_or_default(),
        }
    }

    pub fn _formatted_print(&self, indent: usize) {
        let indent_str = " ".repeat(indent);
        let output = match &self.node_type {
            node_type @ (ASTNodeType::Variable(v)
            | ASTNodeType::Number(v)
            | ASTNodeType::String(v)
            | ASTNodeType::Boolean(v)) => {
                format!("{}{:?}: {:?}", indent_str, node_type, v)
            }
            node_type => format!("{}{:?}", indent_str, node_type),
        };

        println!("{}", output);

        if !self.children.is_empty() {
            for child in &self.children {
                child._formatted_print(indent + 2);
            }
        }
    }
}

type MatcherFn<'a> = Box<
    dyn Fn(
        &Vec<GatheredTokens<'a>>,
        usize,
    ) -> Result<(Option<ASTNode<'a>>, usize), ParserError<'a>>,
>;

struct NodeMatcher<'a> {
    matchers: Vec<MatcherFn<'a>>,
}

impl<'a> NodeMatcher<'a> {
    fn new() -> NodeMatcher<'a> {
        NodeMatcher {
            matchers: Vec::new(),
        }
    }

    fn add_matcher(&mut self, matcher: MatcherFn<'a>) {
        self.matchers.push(matcher);
    }

    fn match_node<'b>(
        &self,
        tokens: &'b Vec<GatheredTokens<'a>>,
        current: usize,
    ) -> Result<(Option<ASTNode<'a>>, usize), ParserError<'a>> {
        if tokens.is_empty() {
            return Ok((Some(ASTNode::new(ASTNodeType::None, None, None, None)), 0));
        }
        let mut offset = 0;
        let mut matched_nodes = Vec::<ASTNode<'a>>::new();
        let mut current_pos = current;
        while current_pos < tokens.len() {
            let remaining_tokens = &tokens[current_pos..].to_vec();
            let (node, next_offset) = self.try_match_node(&remaining_tokens, 0)?;
            if node.is_none() {
                break;
            }
            matched_nodes.push(node.unwrap());
            offset += next_offset;
            current_pos += next_offset;
        }

        if matched_nodes.is_empty() {
            return Ok((None, 0));
        }
        if matched_nodes.len() == 1 {
            return Ok((Some(matched_nodes.remove(0)), offset));
        }

        Ok((
            Some(ASTNode::new(
                ASTNodeType::Expressions,
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + offset - 1].last().unwrap()),
                Some(matched_nodes),
            )),
            offset,
        ))
    }
    fn try_match_node<'b>(
        &self,
        tokens: &'b Vec<GatheredTokens<'a>>,
        current: usize,
    ) -> Result<(Option<ASTNode<'a>>, usize), ParserError<'a>> {
        if tokens.is_empty() {
            return Ok((Some(ASTNode::new(ASTNodeType::None, None, None, None)), 0));
        }

        if current >= tokens.len() {
            return Ok((None, 0));
        }

        // 使用二分法查找最大成功匹配长度
        let mut low = 0; // 最小尝试长度
        let mut high = 2 * (tokens.len() - current - 1); // 最大可能长度
        let mut best_match: Option<(ASTNode<'a>, usize)> = None;

        while low <= high {
            // high 所在位置绝对为非成功匹配， low 所在位置绝对为成功匹配
            let mid = low + (high - low) / 2;

            // 尝试匹配当前长度
            let test_tokens = &tokens[current..current + mid + 1].to_vec();

            // 对所有匹配器进行尝试
            let mut current_match: Option<(ASTNode<'a>, usize)> = None;

            for matcher in &self.matchers {
                match matcher(test_tokens, 0) {
                    Ok((Some(node), offset)) => {
                        // 只有当匹配器成功匹配了所有提供的 tokens 时才认为是成功的
                        if offset == test_tokens.len() {
                            current_match = Some((node, offset));
                            break;
                        }
                    }
                    _ => continue,
                }
            }
            // 如果当前长度能够匹配成功，尝试更长的长度
            if let Some((node, offset)) = current_match {
                best_match = Some((node, offset));
                if low >= mid {
                    // 如果当前长度已经是最大长度，直接退出
                    break;
                }
                if mid >= tokens.len() - 1 {
                    break;
                }
                low = mid;
            } else {
                if low >= mid {
                    // 如果当前长度已经是最小长度，直接退出
                    break;
                }
                // 否则尝试更短的长度
                high = mid;
            }
        }

        // 返回最佳匹配结果
        match best_match {
            Some((node, offset)) => Ok((Some(node), offset)),
            None => Ok((None, 0)),
        }
    }
}

fn is_symbol(token: &GatheredTokens, symbol: &str) -> bool {
    if token.len() != 1 {
        return false;
    }
    let token = &token[0];
    token.token_type == TokenType::SYMBOL && token.token == symbol
}

fn is_any_symbol(token: &GatheredTokens) -> bool {
    if token.len() != 1 {
        return false;
    }
    let token = &token[0];
    token.token_type == TokenType::SYMBOL
}

fn is_identifier(token: &GatheredTokens, identifier: &str) -> bool {
    if token.len() != 1 {
        return false;
    }
    let token = &token[0];
    token.token_type == TokenType::IDENTIFIER && token.token == identifier
}

fn unwrap_brace<'t>(token: &GatheredTokens<'t>) -> Result<GatheredTokens<'t>, ParserError<'t>> {
    if token.len() < 2 {
        return Err(ParserError::UnexpectedToken(&token[0]));
    }
    if token[0].token_type == TokenType::SYMBOL
        && token[0].token == "{"
        && token[token.len() - 1].token_type == TokenType::SYMBOL
        && token[token.len() - 1].token == "}"
    {
        return Ok(&token[1..token.len() - 1]);
    }
    if token[0].token_type == TokenType::SYMBOL
        && token[0].token == "["
        && token[token.len() - 1].token_type == TokenType::SYMBOL
        && token[token.len() - 1].token == "]"
    {
        return Ok(&token[1..token.len() - 1]);
    }
    if token[0].token_type == TokenType::SYMBOL
        && token[0].token == "("
        && token[token.len() - 1].token_type == TokenType::SYMBOL
        && token[token.len() - 1].token == ")"
    {
        return Ok(&token[1..token.len() - 1]);
    }
    Err(ParserError::UnexpectedToken(&token[0]))
}

fn is_bracket(token: &GatheredTokens) -> bool {
    if token.len() < 2 {
        return false;
    }
    token[0].token_type == TokenType::SYMBOL
        && token[0].token == "("
        && token[token.len() - 1].token_type == TokenType::SYMBOL
        && token[token.len() - 1].token == ")"
}

fn is_brace(token: &GatheredTokens) -> bool {
    if token.len() < 2 {
        return false;
    }
    token[0].token_type == TokenType::SYMBOL
        && token[0].token == "{"
        && token[token.len() - 1].token_type == TokenType::SYMBOL
        && token[token.len() - 1].token == "}"
}

fn is_square_bracket(token: &GatheredTokens) -> bool {
    if token.len() < 2 {
        return false;
    }
    token[0].token_type == TokenType::SYMBOL
        && token[0].token == "["
        && token[token.len() - 1].token_type == TokenType::SYMBOL
        && token[token.len() - 1].token == "]"
}

pub fn build_ast(tokens: GatheredTokens<'_>) -> Result<ASTNode<'_>, ParserError<'_>> {
    let gathered = gather(tokens)?;
    let (matched, offset) = match_all(&gathered, 0)?;
    if matched.is_none() {
        return Ok(ASTNode::new(ASTNodeType::None, None, None, None));
    }
    let matched = matched.unwrap();
    if offset != gathered.len() {
        return Err(ParserError::NotFullyMatched(
            gathered.first().unwrap().first().unwrap(),
            gathered.last().unwrap().last().unwrap(),
        ));
    }
    Ok(matched)
}

fn match_all<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut node_matcher = NodeMatcher::new();
    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_expressions(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_annotation(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_return_emit_raise(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens: &Vec<&[Token<'_>]>,
         current|
         -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_tuple(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_let(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_assign(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_map(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_set_def(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_quick_call(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_lambda_def(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_named_to(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_key_value(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_while(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_control_flow(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_if(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_or(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_and(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_xor(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_not(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_operation_compare(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_operation_add_sub(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_operation_mul_div_mod(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_bitwise_shift(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_unary(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_power(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_range(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_in(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_is(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_modifier(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_quick_named_to(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_assume_tuple(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_alias(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_member_access_and_call(tokens, current)
        },
    ));

    node_matcher.add_matcher(Box::new(
        |tokens, current| -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
            match_variable(tokens, current)
        },
    ));

    node_matcher.match_node(tokens, current)
}

fn match_expressions<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset = 0usize;
    let mut left_tokens = Vec::<GatheredTokens>::new();
    let mut last_offset = 0usize;
    let mut separated = Vec::<ASTNode>::new();
    while current + offset < tokens.len() {
        if is_symbol(&tokens[current + offset], ";") {
            let (node, node_offset) = match_all(&left_tokens, 0)?;
            if node.is_none() {
                return Ok((None, 0));
            }
            if node_offset != left_tokens.len() {
                return Err(ParserError::NotFullyMatched(
                    left_tokens.first().unwrap().first().unwrap(),
                    left_tokens.last().unwrap().last().unwrap(),
                ));
            }

            separated.push(node.unwrap());
            left_tokens.clear();
            offset += 1;
            last_offset = offset;
        } else {
            left_tokens.push(tokens[current + offset]);
            offset += 1;
        }
    }
    if separated.is_empty() {
        return Ok((None, 0));
    }
    let (node, node_offset) = match_all(&left_tokens, 0)?;
    if node.is_none() {
        return Ok((None, 0));
    }
    separated.push(node.unwrap());
    Ok((
        Some(ASTNode::new(
            ASTNodeType::Expressions,
            Some(&tokens[current].first().unwrap()),
            Some(
                &tokens[current + last_offset + node_offset - 1]
                    .last()
                    .unwrap(),
            ),
            Some(separated),
        )),
        last_offset + node_offset,
    ))
}

fn match_annotation<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }

    if !is_symbol(&tokens[current], "@") {
        return Ok((None, 0));
    }

    if current + 1 >= tokens.len() {
        return Err(ParserError::MissingStructure(
            &tokens[current][0],
            "Annotation".to_string(),
        ));
    };

    if tokens[current + 1].len() != 1 || tokens[current + 1][0].token_type != TokenType::IDENTIFIER
    {
        return Err(ParserError::ErrorStructure(
            &tokens[current][0],
            "Annotation requires an Identifer".to_string(),
        ));
    };

    let annotation = tokens[current + 1][0].token.to_string();
    let right_tokens = tokens.get(current + 2..).unwrap_or(&[]).to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }

    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }

    let right = right.unwrap();
    let node = ASTNode::new(
        ASTNodeType::Annotation(annotation),
        Some(&tokens[current].first().unwrap()),
        Some(&tokens[current + right_offset + 1].last().unwrap()),
        Some(vec![right]),
    );
    Ok((Some(node), right_offset + 2))
}

fn match_return_emit_raise<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_identifier(&tokens[current], "return")
        && !is_identifier(&tokens[current], "emit")
    {
        return Ok((None, 0));
    }
    let right_tokens = tokens[current + 1..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right = right.unwrap();

    let node_type = match tokens[current].first().unwrap().token {
        "return" => ASTNodeType::Return,
        "emit" => ASTNodeType::Emit,
        _ => unreachable!(),
    };
    Ok((
        Some(ASTNode::new(
            node_type,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + right_offset].last().unwrap()),
            Some(vec![right]),
        )),
        right_offset + 1,
    ))
}

fn match_tuple<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset = 0usize;
    let mut left_tokens = Vec::<GatheredTokens>::new();
    let mut last_offset = 0usize;
    let mut separated = Vec::<ASTNode>::new();
    while current + offset < tokens.len() {
        if is_symbol(&tokens[current + offset], ",") {
            let (node, node_offset) = match_all(&left_tokens, 0)?;
            if node.is_none() {
                return Ok((None, 0));
            }
            if node_offset != left_tokens.len() {
                return Err(ParserError::NotFullyMatched(
                    left_tokens.first().unwrap().first().unwrap(),
                    left_tokens.last().unwrap().last().unwrap(),
                ));
            }
            separated.push(node.unwrap());
            left_tokens.clear();
            offset += 1;
            last_offset = offset;
        } else {
            left_tokens.push(tokens[current + offset]);
            offset += 1;
        }
    }
    if separated.is_empty() {
        return Ok((None, 0));
    }
    let (node, node_offset) = match_all(&left_tokens, 0)?;
    if node.is_none() {
        return Ok((None, 0));
    }
    separated.push(node.unwrap());
    Ok((
        Some(ASTNode::new(
            ASTNodeType::Tuple,
            Some(&tokens[current].first().unwrap()),
            Some(
                &tokens[current + last_offset + node_offset - 1]
                    .last()
                    .unwrap(),
            ),
            Some(separated),
        )),
        last_offset + node_offset,
    ))
}

fn match_let<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_symbol(&tokens[current + 1], ":=") {
        // x := expression
        return Ok((None, 0));
    }

    let left_tokens = gather(tokens[current])?;

    let right_tokens = tokens[current + 2..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right = right.unwrap();

    let (left, left_offset) = match_all(&left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let left = left.unwrap();

    match left.node_type {
        ASTNodeType::Variable(name) => Ok((
            Some(ASTNode::new(
                ASTNodeType::Let(name),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + right_offset + 1].last().unwrap()),
                Some(vec![right]),
            )),
            right_offset + 2,
        )),
        ASTNodeType::String(name) => Ok((
            Some(ASTNode::new(
                ASTNodeType::Let(name),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + right_offset + 1].last().unwrap()),
                Some(vec![right]),
            )),
            right_offset + 2,
        )),
        _ => Err(ParserError::InvalidVariableName(
            &tokens[current].first().unwrap(),
        )),
    }
}

fn match_assign<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    // 确保有足够的 token 来处理赋值
    if current >= tokens.len() {
        return Ok((None, 0));
    }

    // 向右搜索 = 符号
    let mut offset = 0;
    let mut left_tokens = Vec::new();

    while current + offset < tokens.len() {
        // 找到 = 符号
        if tokens[current + offset].len() == 1
            && tokens[current + offset][0].token_type == TokenType::SYMBOL
            && tokens[current + offset][0].token == "="
        {
            break;
        }

        left_tokens.push(tokens[current + offset]);
        offset += 1;
    }

    // 没找到 = 符号
    if current + offset >= tokens.len() || !is_symbol(&tokens[current + offset], "=") {
        return Ok((None, 0));
    }

    // 解析左侧表达式
    let (left, left_offset) = match_all(&left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let left = left.unwrap();

    // 解析右侧表达式
    if current + offset + 1 >= tokens.len() {
        return Ok((None, 0));
    }
    let right_tokens = tokens[current + offset + 1..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right = right.unwrap();

    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Assign,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + offset + right_offset].last().unwrap()),
            Some(vec![left, right]),
        )),
        offset + right_offset + 1, // +1 是因为 = 符号
    ));
}
fn match_named_to<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_symbol(&tokens[current + 1], "=>") {
        // x => y (x is name of y)
        return Ok((None, 0));
    }

    let left_tokens = gather(tokens[current])?;

    let (left, left_offset) = match_all(&left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let mut left = left.unwrap();

    if let ASTNodeType::Variable(name) = left.node_type {
        left = ASTNode::new(
            ASTNodeType::String(name),
            left.start_token,
            left.start_token,
            Some(left.children),
        );
    }

    let right_tokens = tokens[current + 2..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right = right.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::NamedTo,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + right_offset + 1].last().unwrap()),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}

fn match_key_value<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_symbol(&tokens[current + 1], ":") {
        // x: y
        return Ok((None, 0));
    }

    let left_tokens = gather(tokens[current])?;
    let (left, left_offset) = match_all(&left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let left = left.unwrap();

    let right_tokens = tokens[current + 2..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right = right.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::KeyValue,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + right_offset + 1].last().unwrap()),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}

fn match_while<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_identifier(&tokens[current], "while") {
        // while expression body
        return Ok((None, 0));
    }

    let condition_tokens = gather(tokens[current + 1])?;

    let (condition, condition_offset) = match_all(&condition_tokens, 0)?;
    if condition.is_none() {
        return Ok((None, 0));
    }
    if condition_offset != condition_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            condition_tokens.first().unwrap().first().unwrap(),
            condition_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let condition = condition.unwrap();

    let body_tokens = tokens[current + 2..].to_vec();
    let (body, body_offset) = match_all(&body_tokens, 0)?;
    if body.is_none() {
        return Ok((None, 0));
    }
    if body_offset != body_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            body_tokens.first().unwrap().first().unwrap(),
            body_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let body = body.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::While,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + body_offset + 1].last().unwrap()),
            Some(vec![condition, body]),
        )),
        body_offset + 2,
    ))
}

fn match_if<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_identifier(&tokens[current], "if") {
        // if expression truecondition || if expression truecondition else falsecondition
        return Ok((None, 0));
    }

    let condition_tokens = gather(tokens[current + 1])?;
    let true_condition_tokens = gather(tokens[current + 2])?;

    let (condition, condition_offset) = match_all(&condition_tokens, 0)?;
    if condition.is_none() {
        return Ok((None, 0));
    }
    if condition_offset != condition_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            condition_tokens.first().unwrap().first().unwrap(),
            condition_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let condition = condition.unwrap();

    let (true_condition, true_condition_offset) = match_all(&true_condition_tokens, 0)?;
    if true_condition.is_none() {
        return Ok((None, 0));
    }
    if true_condition_offset != true_condition_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            true_condition_tokens.first().unwrap().first().unwrap(),
            true_condition_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let true_condition = true_condition.unwrap();

    if current + 3 < tokens.len() && is_identifier(&tokens[current + 3], "else") {
        let false_condition_tokens = tokens[current + 4..].to_vec();
        let (false_condition, false_condition_offset) = match_all(&false_condition_tokens, 0)?;
        if false_condition.is_none() {
            return Ok((None, 0));
        }
        if false_condition_offset != false_condition_tokens.len() {
            return Err(ParserError::NotFullyMatched(
                false_condition_tokens.first().unwrap().first().unwrap(),
                false_condition_tokens.last().unwrap().last().unwrap(),
            ));
        }
        let false_condition = false_condition.unwrap();
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::If,
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + false_condition_offset + 3].last().unwrap()),
                Some(vec![condition, true_condition, false_condition]),
            )),
            false_condition_offset + 4,
        ));
    }
    Ok((
        Some(ASTNode::new(
            ASTNodeType::If,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + true_condition_offset + 1].last().unwrap()),
            Some(vec![condition, true_condition]),
        )),
        true_condition_offset + 2,
    ))
}

fn match_control_flow<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }
    if is_identifier(&tokens[current], "break") {
        let right_tokens = tokens[current + 1..].to_vec();
        let (right, right_offset) = match_all(&right_tokens, 0)?;
        if right.is_none() {
            return Ok((None, 0));
        }
        if right_offset != right_tokens.len() {
            return Err(ParserError::NotFullyMatched(
                right_tokens.first().unwrap().first().unwrap(),
                right_tokens.last().unwrap().last().unwrap(),
            ));
        }
        let right = right.unwrap();
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Break,
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + right_offset].last().unwrap()),
                Some(vec![right]),
            )),
            right_offset + 1,
        ));
    } else if is_identifier(&tokens[current], "continue") {
        let right_tokens = tokens[current + 1..].to_vec();
        let (right, right_offset) = match_all(&right_tokens, 0)?;
        if right.is_none() {
            return Ok((None, 0));
        }
        if right_offset != right_tokens.len() {
            return Err(ParserError::NotFullyMatched(
                right_tokens.first().unwrap().first().unwrap(),
                right_tokens.last().unwrap().last().unwrap(),
            ));
        }
        let right = right.unwrap();
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Continue,
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + right_offset].last().unwrap()),
                Some(vec![right]),
            )),
            right_offset + 1,
        ));
    }
    Ok((None, 0))
}

fn match_or<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = current + offset;
        if is_identifier(&tokens[pos], "or") {
            operator = Some("or");
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok((None, 0));
    }

    let left_tokens = &tokens[current..operator_pos].to_vec();
    let right_tokens = &tokens[operator_pos + 1..].to_vec();

    let (left, left_offset) = match_all(left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(ASTNodeOperation::Or),
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

fn match_and<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = current + offset;
        if is_identifier(&tokens[pos], "and") {
            operator = Some("and");
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok((None, 0));
    }

    let left_tokens = &tokens[current..operator_pos].to_vec();
    let right_tokens = &tokens[operator_pos + 1..].to_vec();

    let (left, left_offset) = match_all(left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(ASTNodeOperation::And),
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

fn match_xor<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = current + offset;
        if is_identifier(&tokens[pos], "xor") {
            operator = Some("xor");
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok((None, 0));
    }

    let left_tokens = &tokens[current..operator_pos].to_vec();
    let right_tokens = &tokens[operator_pos + 1..].to_vec();

    let (left, left_offset) = match_all(left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(ASTNodeOperation::Xor),
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

fn match_not<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }
    if is_identifier(&tokens[current], "not") {
        if current + 1 >= tokens.len() {
            return Ok((None, 0));
        };
        let not_expr = tokens[current + 1..].to_vec();
        let (node, node_offset) = match_all(&not_expr, 0)?;
        if node.is_none() {
            return Ok((None, 0));
        }
        if node_offset != not_expr.len() {
            return Err(ParserError::NotFullyMatched(
                not_expr.first().unwrap().first().unwrap(),
                not_expr.last().unwrap().last().unwrap(),
            ));
        }
        let node = node.unwrap();
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Operation(ASTNodeOperation::Not),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + node_offset].last().unwrap()),
                Some(vec![node]),
            )),
            node_offset + 1,
        ));
    }
    Ok((None, 0))
}

// >, <, >=, <=, ==, !=
fn match_operation_compare<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = current + offset;
        if is_symbol(&tokens[pos], ">")
            || is_symbol(&tokens[pos], "<")
            || is_symbol(&tokens[pos], ">=")
            || is_symbol(&tokens[pos], "<=")
            || is_symbol(&tokens[pos], "==")
            || is_symbol(&tokens[pos], "!=")
        {
            operator = Some(tokens[pos][0].token);
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok((None, 0));
    }

    let left_tokens = &tokens[current..operator_pos].to_vec();
    let right_tokens = &tokens[operator_pos + 1..].to_vec();

    let (left, left_offset) = match_all(left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let operation = match operator.unwrap() {
        ">" => ASTNodeOperation::Greater,
        "<" => ASTNodeOperation::Less,
        ">=" => ASTNodeOperation::GreaterEqual,
        "<=" => ASTNodeOperation::LessEqual,
        "==" => ASTNodeOperation::Equal,
        "!=" => ASTNodeOperation::NotEqual,
        _ => unreachable!(),
    };
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(operation),
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

// +, -
fn match_operation_add_sub<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset: usize = tokens.len().saturating_sub(current).saturating_sub(1);
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;

    // 从右往左查找 + 或 - 操作符
    while offset > 0 {
        let pos: usize = current + offset;
        if is_symbol(&tokens[pos], "+") || is_symbol(&tokens[pos], "-") {
            let op_token = tokens[pos][0].token;

            // 检查是否为一元操作符
            let is_unary = if pos == current {
                true // 如果在表达式开始位置，一定是一元操作符
            } else {
                // 检查前一个token是否为运算符或括号等，表明这是一元操作符
                let prev_pos = pos - 1;
                is_any_symbol(&tokens[prev_pos])
            };

            // 如果是一元操作符，继续向左搜索二元操作符
            if is_unary && pos > current {
                offset -= 1;
                continue;
            }

            operator = Some(op_token);
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }

    if operator.is_none() {
        return Ok((None, 0)); // 没有找到操作符
    }

    let op = operator.unwrap();

    // 处理二元操作符情况

    // 解析左侧表达式
    let left_tokens = &tokens[current..operator_pos].to_vec();
    let (left, left_offset) = match_all(left_tokens, 0)?;

    if left.is_none() {
        return Ok((None, 0));
    }

    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }

    // 解析右侧表达式
    let right_tokens = &tokens[operator_pos + 1..].to_vec();
    let (right, right_offset) = match_all(right_tokens, 0)?;

    if right.is_none() {
        return Ok((None, 0));
    }

    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }

    // 确定操作类型
    let operation = if op == "+" {
        ASTNodeOperation::Add
    } else {
        ASTNodeOperation::Subtract
    };

    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(operation),
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // 返回整个匹配长度
    ));
}

fn match_operation_mul_div_mod<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset: usize = tokens.len().saturating_sub(current).saturating_sub(1);
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;

    // 从右往左查找 *, / 或 % 操作符
    while offset > 0 {
        let pos = current + offset;
        if is_symbol(&tokens[pos], "*")
            || is_symbol(&tokens[pos], "/")
            || is_symbol(&tokens[pos], "%")
        {
            operator = Some(tokens[pos][0].token);
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }

    if operator.is_none() {
        return Ok((None, 0)); // 没有找到操作符
    }

    // 解析左侧表达式
    let left_tokens = &tokens[current..operator_pos].to_vec();
    let (left, left_offset) = match_all(left_tokens, 0)?;

    if left.is_none() {
        return Ok((None, 0));
    }

    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }

    // 解析右侧表达式
    let right_tokens = &tokens[operator_pos + 1..].to_vec();
    let (right, right_offset) = match_all(right_tokens, 0)?;

    if right.is_none() {
        return Ok((None, 0));
    }

    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }

    // 确定操作类型
    let operation = match operator.unwrap() {
        "*" => ASTNodeOperation::Multiply,
        "/" => ASTNodeOperation::Divide,
        "%" => ASTNodeOperation::Modulus,
        _ => unreachable!(),
    };

    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(operation),
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // 返回整个匹配长度
    ));
}

fn match_bitwise_shift<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = current + offset;
        if is_symbol(&tokens[pos], "<<") || is_symbol(&tokens[pos], ">>") {
            operator = Some(tokens[pos][0].token);
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok((None, 0));
    }

    let left_tokens = &tokens[current..operator_pos].to_vec();
    let right_tokens = &tokens[operator_pos + 1..].to_vec();

    let (left, left_offset) = match_all(left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(if operator.unwrap() == "<<" {
                ASTNodeOperation::LeftShift
            } else {
                ASTNodeOperation::RightShift
            }),
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

fn match_unary<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }
    if is_symbol(&tokens[current], "-") || is_symbol(&tokens[current], "+") {
        if current + 1 >= tokens.len() {
            return Ok((None, 0));
        };
        let unary_expr = tokens[current + 1..].to_vec();
        let (node, node_offset) = match_all(&unary_expr, 0)?;
        if node.is_none() {
            return Ok((None, 0));
        }
        if node_offset != unary_expr.len() {
            return Err(ParserError::NotFullyMatched(
                unary_expr.first().unwrap().first().unwrap(),
                unary_expr.last().unwrap().last().unwrap(),
            ));
        }
        let node = node.unwrap();
        let operation = match tokens[current].first().unwrap().token {
            "-" => ASTNodeOperation::Subtract,
            "+" => ASTNodeOperation::Add,
            _ => unreachable!(),
        };
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Operation(operation),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + node_offset].last().unwrap()),
                Some(vec![node]),
            )),
            node_offset + 1,
        ));
    }
    Ok((None, 0))
}

fn match_power<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }

    // 正向搜索 **
    let find = tokens[current..]
        .iter()
        .position(|token| is_symbol(token, "**"));
    if find.is_none() {
        return Ok((None, 0));
    }
    let operator_pos = find.unwrap() + current;
    if operator_pos + 1 >= tokens.len() {
        return Ok((None, 0));
    }
    let left_tokens = &tokens[current..operator_pos].to_vec();
    let right_tokens = &tokens[operator_pos + 1..].to_vec();
    let (left, left_offset) = match_all(left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(ASTNodeOperation::Power),
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}
fn match_map<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }

    // 从右向左查找 |> 操作符
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator_pos: usize = 0;
    let mut found = false;

    while offset > 0 {
        let pos = current + offset;
        if is_symbol(&tokens[pos], "|>") {
            operator_pos = pos;
            found = true;
            break;
        }
        offset -= 1;
    }

    if !found {
        return Ok((None, 0));
    }

    // 解析左侧表达式（集合）
    let left_tokens = &tokens[current..operator_pos].to_vec();
    let (left, left_offset) = match_all(left_tokens, 0)?;

    if left.is_none() {
        return Ok((None, 0));
    }

    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }

    // 解析右侧表达式（映射函数）
    let right_tokens = &tokens[operator_pos + 1..].to_vec();
    let (right, right_offset) = match_all(right_tokens, 0)?;

    if right.is_none() {
        return Ok((None, 0));
    }

    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }

    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Map,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // 返回整个匹配长度
    ));
}
fn match_set_def<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }

    // 从右向左查找 | 操作符
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator_pos: usize = 0;
    let mut found = false;

    while offset > 0 {
        let pos = current + offset;
        if is_symbol(&tokens[pos], "|") {
            operator_pos = pos;
            found = true;
            break;
        }
        offset -= 1;
    }

    if !found {
        return Ok((None, 0));
    }

    // 解析左侧表达式（集合）
    let left_tokens = &tokens[current..operator_pos].to_vec();
    let (left, left_offset) = match_all(left_tokens, 0)?;

    if left.is_none() {
        return Ok((None, 0));
    }

    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }

    // 解析右侧表达式（过滤条件）
    let right_tokens = &tokens[operator_pos + 1..].to_vec();
    let (right, right_offset) = match_all(right_tokens, 0)?;

    if right.is_none() {
        return Ok((None, 0));
    }

    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }

    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Set,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens.last().unwrap().last().unwrap()),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // 返回整个匹配长度
    ));
}

fn match_lambda_def<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    // Need at least params, ->, body_start
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }
    // Check for -> symbol
    if !is_symbol(&tokens[current + 1], "->") {
        return Ok((None, 0));
    }

    // Parse parameters (left part)
    let left_tokens = gather(tokens[current])?;
    let (left, left_offset) = match_all(&left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let mut left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    // Ensure parameters are treated as a tuple
    if left.node_type != ASTNodeType::Tuple && left.node_type != ASTNodeType::AssumeTuple {
        left = ASTNode::new(
            ASTNodeType::Tuple,
            left.start_token,
            left.end_token,
            Some(vec![left]),
        );
    }

    let mut body_start_index = current + 2; // Index after params ->
    let mut captures_node: Option<ASTNode> = None;
    let mut is_capture = false;

    // Check for capture list (&xxx)
    if body_start_index < tokens.len() && is_symbol(&tokens[body_start_index], "&") {
        is_capture = true;
        let capture_list_index = body_start_index + 1;
        if capture_list_index >= tokens.len() {
            return Err(ParserError::MissingStructure(
                &tokens[body_start_index][0],
                "Capture list after '&'".to_string(),
            ));
        }

        // Parse the capture list (single token group expected after &)
        let capture_tokens_group = gather(tokens[capture_list_index])?;
        let (captures, captures_parsed_len) = match_all(&capture_tokens_group, 0)?;

        if captures.is_none() {
            return Err(ParserError::ErrorStructure(
                &tokens[capture_list_index][0],
                "Failed to parse capture value after '&'".to_string(),
            ));
        }
        let captures = captures.unwrap();
        if captures_parsed_len != capture_tokens_group.len() {
            return Err(ParserError::NotFullyMatched(
                capture_tokens_group.first().unwrap().first().unwrap(),
                capture_tokens_group.last().unwrap().last().unwrap(),
            ));
        }
        captures_node = Some(captures);

        // Update body_start_index to point after & and the capture list token group
        body_start_index += 2;
    }

    // Check for 'dyn' keyword
    let is_dyn = body_start_index < tokens.len() && is_identifier(&tokens[body_start_index], "dyn");
    if is_dyn {
        body_start_index += 1;
    }

    // Check if there's anything left for the body
    if body_start_index >= tokens.len() {
        return Err(ParserError::MissingStructure(
            tokens.last().unwrap().last().unwrap(), // Point to the last token
            "Lambda body".to_string(),
        ));
    }

    // Parse the lambda body (right part)
    let body_tokens = &tokens[body_start_index..].to_vec();
    let (right, right_offset) = match_all(body_tokens, 0)?;
    if right.is_none() {
        // This might indicate an empty body, which could be valid depending on language rules.
        // If an empty body isn't allowed after ->[&captures][dyn], return error.
        // For now, assume it might be valid or handled by match_all returning None correctly.
        return Err(ParserError::MissingStructure(
            &tokens[body_start_index - 1].last().unwrap(), // Point to token before body start
            "Lambda body expression".to_string(),
        ));
    }
    if right_offset != body_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            body_tokens.first().unwrap().first().unwrap(),
            body_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right = right.unwrap();

    // Construct the AST node
    let mut children = vec![left];
    if let Some(captures) = captures_node {
        children.push(captures); // Add captures node if it exists
    }
    children.push(right); // Add body node

    let total_offset = body_start_index - current + right_offset;

    Ok((
        Some(ASTNode::new(
            // Use the new ASTNodeType variant
            ASTNodeType::LambdaDef(is_dyn, is_capture, false),
            Some(&tokens[current].first().unwrap()), // Start of parameters
            Some(&tokens[current + total_offset - 1].last().unwrap()), // End of body
            Some(children),
        )),
        total_offset, // Total number of token groups consumed
    ))
}

fn match_quick_call<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 1 >= tokens.len() {
        return Ok((None, 0));
    }
    if is_symbol(&tokens[current], "#") {
        // #lambdaobj var
        let left_tokens = gather(tokens[current + 1])?;
        let (left, left_offset) = match_all(&left_tokens, 0)?;
        if left.is_none() {
            return Ok((None, 0));
        }
        let left = left.unwrap();
        if left_offset != left_tokens.len() {
            return Err(ParserError::NotFullyMatched(
                left_tokens.first().unwrap().first().unwrap(),
                left_tokens.last().unwrap().last().unwrap(),
            ));
        }
        let right_tokens = tokens.get(current + 2..).unwrap_or(&[]).to_vec();
        let (right, right_offset) = match_all(&right_tokens, 0)?;
        if right.is_none() {
            return Ok((None, 0));
        }
        if right_offset != right_tokens.len() {
            return Err(ParserError::NotFullyMatched(
                right_tokens.first().unwrap().first().unwrap(),
                right_tokens.last().unwrap().last().unwrap(),
            ));
        }
        let mut right = right.unwrap();
        if right.node_type != ASTNodeType::Tuple && right.node_type != ASTNodeType::AssumeTuple {
            right = ASTNode::new(
                ASTNodeType::Tuple,
                if current + 2 < tokens.len() {
                    Some(&tokens[current + 2].first().unwrap())
                } else {
                    None
                },
                right.end_token,
                Some(vec![right]),
            );
        }
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::LambdaCall,
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + right_offset + 1].last().unwrap()),
                Some(vec![left, right]),
            )),
            right_offset + 2,
        ));
    }
    Ok((None, 0))
}

fn match_modifier<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 1 >= tokens.len() {
        return Ok((None, 0));
    }
    if tokens[current].len() == 1
        && vec![
            "deepcopy",
            "copy",
            "mut",
            "const",
            "keyof",
            "valueof",
            "assert",
            "import",
            "typeof",
            "await",
            "bind",
            "collect",
            "lengthof",
            "dynamic",
            "static",
        ]
        .contains(&tokens[current].first().unwrap().token)
    {
        let modify_expr = tokens[current + 1..].to_vec();
        let (node, node_offset) = match_all(&modify_expr, 0)?;
        if node.is_none() {
            return Ok((None, 0));
        }
        if node_offset != modify_expr.len() {
            return Err(ParserError::NotFullyMatched(
                modify_expr.first().unwrap().first().unwrap(),
                modify_expr.last().unwrap().last().unwrap(),
            ));
        }
        let node = node.unwrap();

        if tokens[current].first().unwrap().token == "dynamic" {
            let ASTNodeType::LambdaDef(is_dyn_gen, is_capture, _) = node.node_type else {
                return Err(ParserError::ErrorStructure(
                    tokens[current].first().unwrap(),
                    "Dynamic modifier can only be used with lambda".to_string(),
                ));
            };
            let node = node.clone();
            return Ok((
                Some(ASTNode::new(
                    ASTNodeType::LambdaDef(is_dyn_gen, is_capture, true),
                    Some(&tokens[current].first().unwrap()),
                    Some(&tokens[current + node_offset].last().unwrap()),
                    Some(node.children),
                )),
                node_offset + 1,
            ));
        }

        if tokens[current].first().unwrap().token == "static" {
            let ASTNodeType::LambdaDef(is_dyn_gen, is_capture, _) = node.node_type else {
                return Err(ParserError::ErrorStructure(
                    tokens[current].first().unwrap(),
                    "Dynamic modifier can only be used with lambda".to_string(),
                ));
            };
            let node = node.clone();
            return Ok((
                Some(ASTNode::new(
                    ASTNodeType::LambdaDef(is_dyn_gen, is_capture, false),
                    Some(&tokens[current].first().unwrap()),
                    Some(&tokens[current + node_offset].last().unwrap()),
                    Some(node.children),
                )),
                node_offset + 1,
            ));
        }

        let modifier = match tokens[current].first().unwrap().token {
            "deepcopy" => ASTNodeModifier::DeepCopy,
            "copy" => ASTNodeModifier::Copy,
            "mut" => ASTNodeModifier::Mut,
            "const" => ASTNodeModifier::Const,
            "keyof" => ASTNodeModifier::KeyOf,
            "valueof" => ASTNodeModifier::ValueOf,
            "assert" => ASTNodeModifier::Assert,
            "import" => ASTNodeModifier::Import,
            "typeof" => ASTNodeModifier::TypeOf,
            "await" => ASTNodeModifier::Await,
            "bind" => ASTNodeModifier::BindSelf,
            "collect" => ASTNodeModifier::Collect,
            "lengthof" => ASTNodeModifier::LengthOf,
            _ => return Ok((None, 0)),
        };
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Modifier(modifier),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current + node_offset].last().unwrap()),
                Some(vec![node]),
            )),
            node_offset + 1,
        ));
    }
    Ok((None, 0))
}

fn match_quick_named_to<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    // expr ?
    if current + 1 >= tokens.len() {
        return Ok((None, 0));
    }
    if is_symbol(&tokens[tokens.len() - 1], "?") {
        let left_tokens = tokens[..tokens.len() - 1].to_vec();
        let (node, node_offset) = match_all(&left_tokens, 0)?;
        if node.is_none() {
            return Ok((None, 0));
        }
        if node_offset != left_tokens.len() {
            return Err(ParserError::NotFullyMatched(
                left_tokens.first().unwrap().first().unwrap(),
                left_tokens.last().unwrap().last().unwrap(),
            ));
        }
        let mut node = node.unwrap();
        if let ASTNodeType::Variable(name) = node.node_type {
            node = ASTNode::new(
                ASTNodeType::String(name),
                node.start_token,
                node.end_token,
                Some(node.children),
            );
        }
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::NamedTo,
                Some(&tokens[current].first().unwrap()),
                Some(&tokens.last().unwrap().last().unwrap()),
                Some(vec![
                    node,
                    ASTNode::new(ASTNodeType::Null, None, None, None),
                ]),
            )),
            node_offset + 1,
        ));
    } else if is_symbol(&tokens[tokens.len() - 1], "!") {
        let left_tokens = tokens[..tokens.len() - 1].to_vec();
        let (node, node_offset) = match_all(&left_tokens, 0)?;
        if node.is_none() {
            return Ok((None, 0));
        }
        if node_offset != left_tokens.len() {
            return Err(ParserError::NotFullyMatched(
                left_tokens.first().unwrap().first().unwrap(),
                left_tokens.last().unwrap().last().unwrap(),
            ));
        }
        let node = node.unwrap();
        let ASTNodeType::Variable(name) = node.node_type else {
            return Ok((None, 0));
        };
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::NamedTo,
                Some(&tokens[current].first().unwrap()),
                Some(&tokens.last().unwrap().last().unwrap()),
                Some(vec![
                    ASTNode::new(
                        ASTNodeType::String(name.clone()),
                        node.start_token,
                        node.start_token,
                        None,
                    ),
                    ASTNode::new(
                        ASTNodeType::Variable(name),
                        node.start_token,
                        node.end_token,
                        None,
                    ),
                ]),
            )),
            node_offset + 1,
        ));
    }
    Ok((None, 0))
}

fn match_assume_tuple<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 1 >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_symbol(&tokens[current], "...") {
        return Ok((None, 0));
    }
    let left_tokens = &tokens[current + 1..].to_vec();
    let (node, node_offset) = match_all(left_tokens, 0)?;
    if node.is_none() {
        return Ok((None, 0));
    }
    let node = node.unwrap();
    if node_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    Ok((
        Some(ASTNode::new(
            ASTNodeType::AssumeTuple,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + node_offset].last().unwrap()),
            Some(vec![node]),
        )),
        node_offset + 1,
    ))
}

fn match_alias<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }

    if !is_symbol(&tokens[current + 1], "::") {
        return Ok((None, 0));
    }

    let type_tokens = gather(tokens[current])?;
    let (type_node, type_offset) = match_all(&type_tokens, 0)?;

    if type_node.is_none() {
        return Ok((None, 0));
    }

    let type_node = type_node.unwrap();
    if type_offset != type_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            type_tokens.first().unwrap().first().unwrap(),
            type_tokens.last().unwrap().last().unwrap(),
        ));
    }

    let type_name = match &type_node.node_type {
        ASTNodeType::Variable(name) => name.clone(),
        ASTNodeType::String(name) => name.clone(),
        _ => {
            return Err(ParserError::InvalidSyntax(
                &tokens[current].first().unwrap(),
            ));
        }
    };

    // 解析右侧值表达式
    let right_tokens = &tokens[current + 2..].to_vec();
    let (value_node, value_offset) = match_all(&right_tokens, 0)?;
    if value_node.is_none() {
        return Ok((None, 0));
    }
    if value_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let value_node = value_node.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::Namespace(type_name),
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + value_offset + 1].last().unwrap()),
            Some(vec![value_node]),
        )),
        value_offset + 2,
    ))
}

fn match_member_access_and_call<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    let mut offset: usize = tokens.len() - current - 1;
    let mut access_type = Option::<&str>::None;
    let mut access_pos: usize = 0;

    // 从右往左搜索访问操作符
    while offset > 0 {
        let pos = current + offset;

        // 检查是否为索引访问 obj[idx]
        if is_square_bracket(&tokens[pos]) {
            access_type = Some("[]");
            access_pos = pos;
            break;
        }
        // 检查是否为属性访问 obj.prop
        else if is_symbol(&tokens[pos], ".") {
            access_type = Some(".");
            access_pos = pos;
            break;
        }
        // 检查是否为函数调用 func(args)
        else if is_bracket(&tokens[pos]) {
            access_type = Some("()");
            access_pos = pos;
            break;
        }

        offset -= 1;
    }

    if access_type.is_none() {
        return Ok((None, 0)); // 没有找到访问操作符
    }

    match access_type.unwrap() {
        // 处理索引访问 obj[idx]
        "[]" => {
            if access_pos != tokens.len() - 1 {
                // 非 xxx[] 形式的访问，直接返回
                return Err(ParserError::InvalidSyntax(
                    &tokens[access_pos].first().unwrap(),
                ));
            }
            // 解析左侧表达式（被访问的对象或被调用的函数）
            let left_tokens = &tokens[current..access_pos].to_vec();
            if left_tokens.is_empty() {
                return Ok((None, 0));
            }

            let (left, left_offset) = match_all(left_tokens, 0)?;
            if left.is_none() {
                return Ok((None, 0));
            }

            let left = left.unwrap();
            if left_offset != left_tokens.len() {
                return Err(ParserError::NotFullyMatched(
                    left_tokens.first().unwrap().first().unwrap(),
                    left_tokens.last().unwrap().last().unwrap(),
                ));
            }
            // 解包索引括号中的内容
            let index_tokens = &tokens[access_pos];
            let gathered_index = gather(index_tokens)?;

            let (index_node, _) = match_all(&gathered_index, 0)?;
            if index_node.is_none() {
                return Ok((None, 0));
            }

            let index_node = index_node.unwrap();

            Ok((
                Some(ASTNode::new(
                    ASTNodeType::IndexOf,
                    Some(&tokens[current].first().unwrap()),
                    Some(&tokens[access_pos].last().unwrap()),
                    Some(vec![left, index_node]),
                )),
                (access_pos - current) + 1,
            ))
        }

        // 处理属性访问 obj.prop
        "." => {
            if access_pos != tokens.len() - 2 {
                // 非 xxx.xx 形式的访问，直接返回
                return Err(ParserError::InvalidSyntax(
                    &tokens[access_pos].first().unwrap(),
                ));
            }
            // 解析左侧表达式（被访问的对象或被调用的函数）
            let left_tokens = &tokens[current..access_pos].to_vec();
            if left_tokens.is_empty() {
                return Ok((None, 0));
            }

            let (left, left_offset) = match_all(left_tokens, 0)?;
            if left.is_none() {
                return Ok((None, 0));
            }

            let left = left.unwrap();
            if left_offset != left_tokens.len() {
                return Err(ParserError::NotFullyMatched(
                    left_tokens.first().unwrap().first().unwrap(),
                    left_tokens.last().unwrap().last().unwrap(),
                ));
            }
            if access_pos + 1 >= tokens.len() {
                return Ok((None, 0));
            }

            // 获取属性名称
            let right_tokens = &tokens[access_pos + 1..].to_vec();
            let (right, right_offset) = match_all(right_tokens, 0)?;
            if right.is_none() {
                return Ok((None, 0));
            }

            let mut right = right.unwrap();

            // 如果右侧是变量，将其视为属性名
            if let ASTNodeType::Variable(var_name) = right.node_type {
                right = ASTNode::new(
                    ASTNodeType::String(var_name),
                    right.start_token,
                    right.end_token,
                    Some(right.children),
                );
                return Ok((
                    Some(ASTNode::new(
                        ASTNodeType::GetAttr,
                        Some(&tokens[current].first().unwrap()),
                        Some(&tokens[access_pos + right_offset].last().unwrap()),
                        Some(vec![left, right]),
                    )),
                    (access_pos - current) + 1 + right_offset,
                ));
            }

            Ok((
                Some(ASTNode::new(
                    ASTNodeType::GetAttr,
                    Some(&tokens[current].first().unwrap()),
                    Some(&tokens[access_pos + right_offset].last().unwrap()),
                    Some(vec![left, right]),
                )),
                (access_pos - current) + 1 + right_offset,
            ))
        }

        // 处理函数调用 func(args)
        "()" => {
            if access_pos != tokens.len() - 1 {
                // 非 xxx() 形式的访问，直接返回
                return Err(ParserError::InvalidSyntax(
                    &tokens[access_pos].first().unwrap(),
                ));
            }
            // 解析左侧表达式（被访问的对象或被调用的函数）
            let mut left_tokens = tokens[current..access_pos].to_vec();
            if left_tokens.is_empty() {
                return Ok((None, 0));
            }

            let is_async = if left_tokens[0].len() == 1 && left_tokens[0][0].token == "async" {
                left_tokens = left_tokens[1..].to_vec();
                true
            } else {
                false
            };
            let (left, left_offset) = match_all(&left_tokens, 0)?;
            if left.is_none() {
                return Ok((None, 0));
            }

            let left = left.unwrap();
            if left_offset != left_tokens.len() {
                return Err(ParserError::NotFullyMatched(
                    left_tokens.first().unwrap().first().unwrap(),
                    left_tokens.last().unwrap().last().unwrap(),
                ));
            }

            // 解包括号中的参数
            let args_tokens = &tokens[access_pos];
            let gathered_args = gather(args_tokens)?;

            // 处理有参数情况
            let (args_node, _) = match_all(&gathered_args, 0)?;
            if args_node.is_none() {
                return Ok((None, 0));
            }

            let args_node = args_node.unwrap();

            // 如果不是元组类型，将其包装为元组
            let args = if args_node.node_type != ASTNodeType::Tuple
                && args_node.node_type != ASTNodeType::AssumeTuple
            {
                ASTNode::new(
                    ASTNodeType::Tuple,
                    args_node.start_token,
                    args_node.end_token,
                    Some(vec![args_node]),
                )
            } else {
                args_node
            };

            Ok((
                Some(ASTNode::new(
                    if is_async {
                        ASTNodeType::AsyncLambdaCall
                    } else {
                        ASTNodeType::LambdaCall
                    },
                    Some(&tokens[current].first().unwrap()),
                    Some(&tokens[access_pos].last().unwrap()),
                    Some(vec![left, args]),
                )),
                (access_pos - current) + 1,
            ))
        }

        _ => unreachable!(),
    }
}

fn match_range<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_symbol(&tokens[current + 1], "..") {
        // x..y
        return Ok((None, 0));
    }

    let left_tokens = gather(tokens[current])?;
    let (left, left_offset) = match_all(&left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right_tokens = &tokens[current + 2..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::Range,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + right_offset + 1].last().unwrap()),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}

fn match_in<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_identifier(&tokens[current + 1], "in") {
        return Ok((None, 0));
    }

    let left_tokens = gather(tokens[current])?;
    let (left, left_offset) = match_all(&left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right_tokens = &tokens[current + 2..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right = right.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::In,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + right_offset + 1].last().unwrap()),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}

fn match_is<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_identifier(&tokens[current + 1], "is") {
        return Ok((None, 0));
    }

    let left_tokens = gather(tokens[current])?;
    let (left, left_offset) = match_all(&left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap(),
            left_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right_tokens = &tokens[current + 2..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap(),
            right_tokens.last().unwrap().last().unwrap(),
        ));
    }
    let right = right.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::Is,
            Some(&tokens[current].first().unwrap()),
            Some(&tokens[current + right_offset + 1].last().unwrap()),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}


fn match_variable<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode<'t>>, usize), ParserError<'t>> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }

    // 匹配括号内容（元组）
    if is_bracket(&tokens[current]) || is_square_bracket(&tokens[current]) {
        let inner_tokens = unwrap_brace(&tokens[current])?;

        // 处理空元组 ()
        if inner_tokens.is_empty() {
            return Ok((
                Some(ASTNode::new(
                    ASTNodeType::Tuple,
                    Some(&tokens[current].first().unwrap()),
                    Some(&tokens[current].last().unwrap()),
                    Some(vec![]),
                )),
                1,
            ));
        }

        let gathered_inner = gather(inner_tokens)?;
        let (node, _) = match_all(&gathered_inner, 0)?;
        if node.is_none() {
            return Ok((None, 0));
        }

        return Ok((Some(node.unwrap()), 1));
    }

    // 匹配函数体 {...}
    if is_brace(&tokens[current]) {
        let body_tokens = unwrap_brace(&tokens[current])?;
        let gathered_body = gather(body_tokens)?;
        let (body, _) = match_all(&gathered_body, 0)?;

        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Body,
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current].last().unwrap()),
                body.map(|b| vec![b]),
            )),
            1,
        ));
    }

    // 匹配字符串常量
    if tokens[current].len() == 1
        && tokens[current].first().unwrap().token_type == TokenType::STRING
    {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::String(tokens[current].first().unwrap().token.to_string()),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current].last().unwrap()),
                None,
            )),
            1,
        ));
    }

    // 匹配数字常量
    if tokens[current].len() == 1
        && tokens[current].first().unwrap().token_type == TokenType::NUMBER
    {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Number(tokens[current].first().unwrap().token.to_string()),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current].last().unwrap()),
                None,
            )),
            1,
        ));
    }

    // 匹配b64
    if tokens[current].len() == 1
        && tokens[current].first().unwrap().token_type == TokenType::BASE64
    {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Base64(tokens[current].first().unwrap().token.to_string()),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current].last().unwrap()),
                None,
            )),
            1,
        ));
    }

    // 匹配布尔值（true）
    if is_identifier(&tokens[current], "true") {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Boolean(String::from("true")),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current].last().unwrap()),
                None,
            )),
            1,
        ));
    }

    // 匹配布尔值（false）
    if is_identifier(&tokens[current], "false") {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Boolean(String::from("false")),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current].last().unwrap()),
                None,
            )),
            1,
        ));
    }

    // 匹配空值（null）
    if is_identifier(&tokens[current], "null") {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Null,
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current].last().unwrap()),
                None,
            )),
            1,
        ));
    }

    // 匹配普通变量名
    if tokens[current].len() == 1
        && tokens[current].first().unwrap().token_type == TokenType::IDENTIFIER
    {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Variable(tokens[current].first().unwrap().token.to_string()),
                Some(&tokens[current].first().unwrap()),
                Some(&tokens[current].last().unwrap()),
                None,
            )),
            1,
        ));
    }
    Ok((None, 0))
}
