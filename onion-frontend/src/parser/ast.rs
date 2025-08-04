use super::diagnostics::find_line_and_col_from_source;
use crate::parser::lexer::{Token, TokenType};
use colored::*;
use std::{collections::HashSet, fmt::Debug, vec};
use unicode_width::UnicodeWidthStr;

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedToken(Token),             // Token
    UnmatchedParenthesis(Token, Token), // (opening, closing)
    InvalidSyntax(Token),
    NotFullyMatched(Token, Token),
    InvalidVariableName(Token),
    UnsupportedStructure(Token),
    MissingStructure(Token, String), // (token, expected structure)
    ErrorStructure(Token, String),   // (token, expected structure)
}

impl ParserError {
    /// 将解析器错误格式化为用户友好的、带颜色的字符串，以便在终端中显示。
    /// 这个实现是自包含的，不需要任何外部诊断库。
    pub fn format(&self) -> String {
        match self {
            // --- 处理指向单个 Token 的错误 ---
            Self::UnexpectedToken(token) => Self::format_single_token_report(
                "Parse Error",
                &format!("Unexpected token '{}'", token.token().yellow()),
                token,
                "Check the syntax rules for this location.",
            ),
            Self::InvalidSyntax(token) => Self::format_single_token_report(
                "Syntax Error",
                "The syntax here is invalid.",
                token,
                "Please correct the statement according to the language grammar.",
            ),
            Self::InvalidVariableName(token) => Self::format_single_token_report(
                "Naming Error",
                &format!("'{}' is not a valid variable name.", token.token().yellow()),
                token,
                "Variable names must start with a letter or underscore.",
            ),
            Self::UnsupportedStructure(token) => Self::format_single_token_report(
                "Parse Error",
                "This language construct is not supported in the current context.",
                token,
                "Try breaking down complex expressions or using a different structure.",
            ),
            Self::MissingStructure(token, expected) => Self::format_single_token_report(
                "Syntax Error",
                &format!("Missing expected structure: {}", expected.yellow()),
                token,
                "An expected element is missing after this token.",
            ),
            Self::ErrorStructure(token, details) => Self::format_single_token_report(
                "Syntax Error",
                &format!("There is an error in this structure: {}", details.yellow()),
                token,
                "Please review the structure of this expression.",
            ),
            Self::UnmatchedParenthesis(opening, closing) => {
                let opening_report = Self::format_single_token_report(
                    "Error",
                    "This opening parenthesis...",
                    opening,
                    "",
                );
                let closing_report = Self::format_single_token_report(
                    "...is not matched by this closing parenthesis",
                    "",
                    closing,
                    "",
                );
                format!(
                    "{}\n\n{}\n\n{}: {}",
                    "Unmatched Parenthesis".bright_red().bold(),
                    opening_report,
                    closing_report,
                    "Ensure every '(', '[', or '{' has a matching closing one.".bright_green()
                )
            }

            // --- 全面重构 NotFullyMatched 的格式化逻辑 ---
            Self::NotFullyMatched(start, end) => {
                let title = "Parse Error".bright_red().bold();
                let message = "Input was not fully parsed. Unprocessed tokens remain.";

                let source_str = start.source_code_str();

                let (start_char_idx, _) = start.origin_token_span();
                let (end_char_idx, _) = end.origin_token_span();

                let (start_line_idx, start_col_char) =
                    find_line_and_col_from_source(start_char_idx, &source_str);
                let (end_line_idx, end_col_char) =
                    find_line_and_col_from_source(end_char_idx, &source_str);

                // 为了生成报告，我们仍然需要分行处理
                let lines: Vec<&str> = source_str.lines().collect();

                // 使用一个可变字符串来构建复杂的输出
                let mut report = String::new();

                // 1. 添加报告头部
                report.push_str(&format!(
                    "{}: {}\n --> {}:{}\n",
                    title,
                    message,
                    (start_line_idx + 1).to_string().bright_cyan(),
                    (start_col_char + 1).to_string().bright_cyan()
                ));

                // 2. 循环遍历所有涉及的行，并为每一行生成代码和高亮
                for i in start_line_idx..=end_line_idx {
                    let line_text = lines.get(i).unwrap_or(&"");

                    // 添加行号和代码
                    report.push_str(&format!(
                        "     |\n{:4} | {}\n     | ",
                        (i + 1).to_string().bright_cyan(),
                        line_text.white()
                    ));

                    // 根据行是第一行、中间行还是最后一行来决定高亮样式
                    if i == start_line_idx && i == end_line_idx {
                        // --- 情况A: 错误范围在同一行内 ---
                        let display_offset = line_text
                            .chars()
                            .take(start_col_char)
                            .collect::<String>()
                            .width();
                        let underline_width = line_text
                            .chars()
                            .skip(start_col_char)
                            .take(end_col_char - start_col_char)
                            .collect::<String>()
                            .width();
                        report.push_str(&format!(
                            "{}{}",
                            " ".repeat(display_offset),
                            "~".repeat(underline_width.max(1)).bright_red().bold()
                        ));
                    } else if i == start_line_idx {
                        // --- 情况B: 错误范围的第一行 ---
                        let display_offset = line_text
                            .chars()
                            .take(start_col_char)
                            .collect::<String>()
                            .width();
                        let underline_width = line_text.width() - display_offset;
                        report.push_str(&format!(
                            "{}{}",
                            " ".repeat(display_offset),
                            "~".repeat(underline_width.max(1)).bright_red().bold()
                        ));
                    } else if i == end_line_idx {
                        // --- 情况C: 错误范围的最后一行 ---
                        let underline_width = line_text
                            .chars()
                            .take(end_col_char)
                            .collect::<String>()
                            .width();
                        report.push_str(&format!(
                            "{}",
                            "~".repeat(underline_width.max(1)).bright_red().bold()
                        ));
                    } else {
                        // --- 情况D: 错误范围的中间行 ---
                        report.push_str(&format!(
                            "{}",
                            "~".repeat(line_text.width()).bright_red().bold()
                        ));
                    }
                    report.push('\n');
                }

                // 3. 添加帮助信息
                report.push_str(&format!(
                    "\n{}: {}",
                    "Help".bright_green(),
                    "Check for missing semicolons or unclosed blocks in this region."
                ));

                report
            }
        }
    }

    /// 内部辅助函数，用于格式化指向单个 Token 的错误报告。
    fn format_single_token_report(
        title: &str,
        message: &str,
        token: &Token,
        help_text: &str,
    ) -> String {
        let source_str = token.source_code_str();
        let (char_index, _) = token.origin_token_span();

        // 找到 Token 所在的行和列（基于字符数）
        let (line_idx, col_char) = find_line_and_col_from_source(char_index, &source_str);

        // 为了生成报告，我们仍然需要分行处理
        let lines: Vec<&str> = source_str.lines().collect();
        let line_text = lines.get(line_idx).unwrap_or(&"");

        // 2. 关键修正：计算正确的显示宽度
        // a. 计算错误 token 之前文本的显示宽度，作为空格填充量
        let display_offset = line_text.chars().take(col_char).collect::<String>().width();
        // b. 计算错误 token 本身的显示宽度，作为 `^` 的重复次数
        let underline_display_width = token.origin_token().width();

        let main_report = format!(
            "{}: {}\n --> {}:{}\n     |\n{:4} | {}\n     | {}{}",
            title.bright_red().bold(),
            message,
            (line_idx + 1).to_string().bright_cyan(),
            (col_char + 1).to_string().bright_cyan(), // 列号仍然按字符数报告，符合编辑器标准
            (line_idx + 1).to_string().bright_cyan(),
            line_text.white(),
            " ".repeat(display_offset), // 使用显示宽度进行填充
            "^".repeat(underline_display_width.max(1))
                .bright_red()
                .bold()  // 使用显示宽度画下划线
        );

        if help_text.is_empty() {
            main_report
        } else {
            format!(
                "{}\n\n{}: {}",
                main_report,
                "Help".bright_green(),
                help_text
            )
        }
    }
}

pub type TokenStream = Vec<Token>;
pub type GatheredTokens<'t> = &'t [Token];

pub mod ast_token_stream {
    pub fn from_stream<'t>(stream: &'t super::TokenStream) -> super::GatheredTokens<'t> {
        stream.as_slice()
    }
}

fn get_next_tokens(
    tokens: GatheredTokens<'_>,
    current: usize,
) -> Result<GatheredTokens<'_>, ParserError> {
    let mut stack = Vec::<(Token, usize)>::new();
    let mut next_tokens_end = 0usize;
    let mut index = current;
    if index >= (*tokens).len() {
        return Ok(&[]);
    }
    loop {
        if ["{", "[", "("].contains(&tokens[index].token().as_str())
            && tokens[index] == TokenType::SYMBOL
        {
            stack.push((tokens[index].clone(), index));
            next_tokens_end += 1;
        } else if ["}", "]", ")"].contains(&tokens[index].token().as_str())
            && tokens[index] == TokenType::SYMBOL
        {
            if stack.is_empty() {
                break;
            }
            let (last, last_position) = stack.pop().unwrap();
            if (last == "{" && tokens[index] != "}")
                || (last == "[" && tokens[index] != "]")
                || (last == "(" && tokens[index] != ")")
            {
                return Err(ParserError::UnmatchedParenthesis(
                    tokens[last_position].clone(),
                    tokens[index].clone(),
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
            tokens[last_position].clone(),
            tokens[index - 1].clone(),
        ));
    }
    Ok(&tokens[current..current + next_tokens_end])
}

fn gather(tokens: GatheredTokens<'_>) -> Result<Vec<GatheredTokens<'_>>, ParserError> {
    let mut current = 0;
    let mut result = Vec::<GatheredTokens>::new();
    while current < tokens.len() {
        let next_tokens = get_next_tokens(tokens, current)?;
        if next_tokens.is_empty() {
            return Err(ParserError::UnsupportedStructure(tokens[current].clone()));
        }
        current += next_tokens.len();
        result.push(next_tokens);
    }
    Ok(result)
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNodeType {
    Null, // Null
    Undefined,
    String(String), // String
    Boolean(bool),  // Boolean
    Number(String), // Number (Integer, Float)
    Base64(String), // Base64

    Variable(String), // Variable
    Required(String), // 变量存在性占位符，我们不直接硬编码对应的AST，我们通过comptime中调用`required "var"`来生成

    Let(String),                      // x := expression
    Frame,                            // {...}
    Assign,                           // x = expression
    LambdaDef(bool, HashSet<String>), // tuple -> body or tuple -> dyn expression
    Expressions,                      // expression1; expression2; ...
    Apply,                            // x y
    Operation(ASTNodeOperation),      // x + y, x - y, x * y, x / y ...
    Tuple,                            // x, y, z, ...
    AssumeTuple,                      // ...value
    Pair,                             // x: y
    GetAttr,                          // x.y
    Return,                           // return expression
    If,    // if expression truecondition || if expression truecondition else falsecondition
    While, // while expression body
    Modifier(ASTNodeModifier), // modifier expression
    Break, // break
    Continue, // continue
    Range, // x..y
    In,
    Namespace(String), // Type::Value
    Set,               // collection | filter
    Map,               // collection |> map
    Is,                // x is y
    Raise,

    Dynamic, // 假设子表达式的所有变量都存在
    Static,  // 假设子表达式的所有变量都在当前上下文中定义

    Comptime, // 编译时计算的表达式
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
    Abs,    // 一元绝对值
    Minus, // 一元负号
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ASTNodeModifier {
    Mut,      // Mut
    Const,    // Const
    KeyOf,    // KeyOf
    ValueOf,  // ValueOf
    Assert,   // Assert
    Import,   // Import
    TypeOf,   // TypeOf
    Await,    // Await
    LengthOf, // LengthOf
    Launch,   // Launch
    Spawn,    // Spawn
    Async,
    Sync,
    Atomic,
}

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub node_type: ASTNodeType,     // Type of the node
    pub start_token: Option<Token>, // Start Token associated with the node
    pub end_token: Option<Token>,   // End token associated with the node
    pub children: Vec<ASTNode>,     // Children of the node
}

impl PartialEq for ASTNode {
    fn eq(&self, other: &Self) -> bool {
        self.node_type == other.node_type && self.children == other.children
    }
}

impl ASTNode {
    pub fn new(
        node_type: ASTNodeType,
        start_token: Option<Token>,
        end_token: Option<Token>,
        children: Option<Vec<ASTNode>>,
    ) -> ASTNode {
        ASTNode {
            node_type,
            start_token,
            end_token,
            children: children.unwrap_or_default(),
        }
    }

    pub fn undefined() -> ASTNode {
        ASTNode::new(ASTNodeType::Undefined, None, None, None)
    }

    pub fn _formatted_print(&self, indent: usize) {
        let indent_str = " ".repeat(indent);
        let output = match &self.node_type {
            node_type @ (ASTNodeType::Variable(v)
            | ASTNodeType::Number(v)
            | ASTNodeType::String(v)) => {
                format!("{}{:?}: {:?}", indent_str, node_type, v)
            }
            node_type @ ASTNodeType::Boolean(v) => {
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

type MatcherFn = fn(&Vec<GatheredTokens>, usize) -> Result<(Option<ASTNode>, usize), ParserError>;

struct NodeMatcher {
    matchers: Vec<MatcherFn>,
}

impl NodeMatcher {
    fn new() -> NodeMatcher {
        NodeMatcher {
            matchers: Vec::new(),
        }
    }

    fn add_matcher(
        &mut self,
        matcher: fn(&Vec<GatheredTokens>, usize) -> Result<(Option<ASTNode>, usize), ParserError>,
    ) {
        self.matchers.push(matcher);
    }

    #[stacksafe::stacksafe]
    fn match_node(
        &self,
        tokens: &Vec<GatheredTokens>,
        current: usize,
    ) -> Result<(Option<ASTNode>, usize), ParserError> {
        if tokens.is_empty() {
            return Ok((
                Some(ASTNode::new(ASTNodeType::Tuple, None, None, Some(vec![]))),
                0,
            ));
        }
        let mut offset = 0;
        let mut matched_nodes = Vec::<ASTNode>::new();
        let mut current_pos = current;
        while current_pos < tokens.len() {
            let remaining_tokens = &tokens[current_pos..].to_vec();
            let (node, next_offset) = self.match_longest_possible(&remaining_tokens, 0)?;
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
                tokens[current].first().cloned(),
                tokens[current + offset - 1].last().cloned(),
                Some(matched_nodes),
            )),
            offset,
        ))
    }

    #[allow(dead_code)]
    #[deprecated]
    fn try_match_node(
        &self,
        tokens: &Vec<GatheredTokens>,
        current: usize,
    ) -> Result<(Option<ASTNode>, usize), ParserError> {
        if tokens.is_empty() {
            return Ok((
                Some(ASTNode::new(ASTNodeType::Tuple, None, None, Some(vec![]))),
                0,
            ));
        }

        if current >= tokens.len() {
            return Ok((None, 0));
        }

        // 使用二分法查找最大成功匹配长度
        let mut low = 0; // 最小尝试长度
        let mut high = 2 * (tokens.len() - current - 1); // 最大可能长度
        let mut best_match: Option<(ASTNode, usize)> = None;

        while low <= high {
            // high 所在位置绝对为非成功匹配， low 所在位置绝对为成功匹配
            let mid = low + (high - low) / 2;

            // 尝试匹配当前长度
            let test_tokens = &tokens[current..current + mid + 1].to_vec();

            // 对所有匹配器进行尝试
            let mut current_match: Option<(ASTNode, usize)> = None;

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

    pub fn match_longest_possible(
        &self,
        tokens: &Vec<GatheredTokens>, // 直接使用切片，避免拥有权和拷贝
        current: usize,
    ) -> Result<(Option<ASTNode>, usize), ParserError> {
        // 如果当前位置已经超出了 token 序列的边界，说明没有东西可以匹配了。
        if current >= tokens.len() {
            return Ok((None, 0));
        }

        let mut best_match: Option<(ASTNode, usize)> = None;
        let remaining_tokens = &tokens[current..].to_vec();

        // 遍历所有注册的语法规则匹配器
        for matcher in &self.matchers {
            match matcher(remaining_tokens, 0) {
                Ok((Some(node), consumed_count)) => {
                    // 更新最佳匹配
                    best_match = Some((node, consumed_count));
                    break;
                }
                Ok((None, _)) => {
                    // 匹配器明确表示不匹配，继续尝试下一个匹配器
                    continue;
                }
                Err(e) => {
                    // 如果一个匹配器内部出错，可以选择立即返回错误，或者忽略并尝试下一个
                    // 这里我们选择立即返回，因为这通常意味着一个严重的、意外的问题
                    return Err(e);
                }
            }
        }

        // 返回找到的最佳匹配
        // 如果没有任何匹配器成功，best_match 会是 None，函数会返回 Ok((None, 0))
        match best_match {
            Some((node, consumed)) => Ok((Some(node), consumed)),
            None => Ok((None, 0)),
        }
    }
}

fn is_symbol(token: &GatheredTokens, symbol: &str) -> bool {
    if token.len() != 1 {
        return false;
    }
    let token = &token[0];
    token == TokenType::SYMBOL && token == symbol
}

fn is_any_symbol(token: &GatheredTokens) -> bool {
    if token.len() != 1 {
        return false;
    }
    let token = &token[0];
    token == TokenType::SYMBOL
}

fn is_identifier(token: &GatheredTokens, identifier: &str) -> bool {
    if token.len() != 1 {
        return false;
    }
    let token = &token[0];
    token == TokenType::IDENTIFIER && token == identifier
}

fn unwrap_brace<'t>(token: &GatheredTokens<'t>) -> Result<GatheredTokens<'t>, ParserError> {
    if token.len() < 2 {
        return Err(ParserError::UnexpectedToken(token[0].clone()));
    }
    if token[0] == TokenType::SYMBOL
        && token[0] == "{"
        && token.last().unwrap() == TokenType::SYMBOL
        && token.last().unwrap() == "}"
    {
        return Ok(&token[1..token.len() - 1]);
    }
    if token[0] == TokenType::SYMBOL
        && token[0] == "["
        && token.last().unwrap() == TokenType::SYMBOL
        && token.last().unwrap() == "]"
    {
        return Ok(&token[1..token.len() - 1]);
    }
    if token[0] == TokenType::SYMBOL
        && token[0] == "("
        && token.last().unwrap() == TokenType::SYMBOL
        && token.last().unwrap() == ")"
    {
        return Ok(&token[1..token.len() - 1]);
    }
    Err(ParserError::UnexpectedToken(token[0].clone()))
}

fn is_bracket(token: &GatheredTokens) -> bool {
    if token.len() < 2 {
        return false;
    }
    token[0] == TokenType::SYMBOL
        && token[0] == "("
        && token.last().unwrap() == TokenType::SYMBOL
        && token.last().unwrap() == ")"
}

fn is_brace(token: &GatheredTokens) -> bool {
    if token.len() < 2 {
        return false;
    }
    token[0] == TokenType::SYMBOL
        && token[0] == "{"
        && token.last().unwrap() == TokenType::SYMBOL
        && token.last().unwrap() == "}"
}

fn is_square_bracket(token: &GatheredTokens) -> bool {
    if token.len() < 2 {
        return false;
    }
    token[0] == TokenType::SYMBOL
        && token[0] == "["
        && token.last().unwrap() == TokenType::SYMBOL
        && token.last().unwrap() == "]"
}

pub fn build_ast(tokens: GatheredTokens<'_>) -> Result<ASTNode, ParserError> {
    let gathered = gather(tokens)?;
    let (matched, offset) = match_all(&gathered, 0)?;
    if matched.is_none() {
        return Ok(ASTNode::new(ASTNodeType::Tuple, None, None, Some(vec![])));
    }
    let matched = matched.unwrap();
    if offset != gathered.len() {
        return Err(ParserError::NotFullyMatched(
            gathered.first().unwrap().first().unwrap().clone(),
            gathered.last().unwrap().last().unwrap().clone(),
        ));
    }
    Ok(matched)
}

fn match_all<'t>(
    tokens: &Vec<GatheredTokens<'t>>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    let mut node_matcher = NodeMatcher::new();
    node_matcher.add_matcher(match_expressions);
    node_matcher.add_matcher(match_dynamic_and_static);
    node_matcher.add_matcher(match_return_emit_raise);
    node_matcher.add_matcher(match_tuple);
    node_matcher.add_matcher(match_comptime);
    node_matcher.add_matcher(match_let);
    node_matcher.add_matcher(match_assign);
    node_matcher.add_matcher(match_map);
    node_matcher.add_matcher(match_set_def);
    node_matcher.add_matcher(match_quick_call);
    node_matcher.add_matcher(match_lambda_def);
    node_matcher.add_matcher(match_named_to);
    node_matcher.add_matcher(match_pair);
    node_matcher.add_matcher(match_while);
    node_matcher.add_matcher(match_break_and_continue);
    node_matcher.add_matcher(match_if);
    node_matcher.add_matcher(match_or);
    node_matcher.add_matcher(match_and);
    node_matcher.add_matcher(match_xor);
    node_matcher.add_matcher(match_not);
    node_matcher.add_matcher(match_operation_compare);
    node_matcher.add_matcher(match_operation_add_sub);
    node_matcher.add_matcher(match_operation_mul_div_mod);
    node_matcher.add_matcher(match_bitwise_shift);
    node_matcher.add_matcher(match_unary);
    node_matcher.add_matcher(match_power);
    node_matcher.add_matcher(match_range);
    node_matcher.add_matcher(match_in);
    node_matcher.add_matcher(match_is);
    node_matcher.add_matcher(match_as);
    node_matcher.add_matcher(match_modifier);
    node_matcher.add_matcher(match_quick_named_to);
    node_matcher.add_matcher(match_assume_tuple);
    node_matcher.add_matcher(match_alias);
    node_matcher.add_matcher(match_member_access_and_apply);
    node_matcher.add_matcher(match_variable);
    node_matcher.match_node(tokens, current)
}

fn match_expressions(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
                    left_tokens.first().unwrap().first().unwrap().clone(),
                    left_tokens.last().unwrap().last().unwrap().clone(),
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
            tokens[current].first().cloned(),
            tokens[current + last_offset + node_offset - 1]
                .last()
                .cloned(),
            Some(separated),
        )),
        last_offset + node_offset,
    ))
}

fn match_comptime(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }

    if !is_symbol(&tokens[current], "@") {
        return Ok((None, 0));
    }

    let right_tokens = tokens[current + 1..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    let node = ASTNode::new(
        ASTNodeType::Comptime,
        tokens[current].first().cloned(),
        tokens[current + right_offset].last().cloned(),
        Some(vec![right]),
    );
    Ok((Some(node), right_offset + 1))
}

fn match_dynamic_and_static(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }

    if !is_symbol(&tokens[current], "dynamic") && !is_identifier(&tokens[current], "static") {
        return Ok((None, 0));
    }

    let right_tokens = tokens[current + 1..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    let node = ASTNode::new(
        if is_identifier(&tokens[current], "dynamic") {
            ASTNodeType::Dynamic
        } else {
            ASTNodeType::Static
        },
        tokens[current].first().cloned(),
        tokens[current + right_offset + 1].last().cloned(),
        Some(vec![right]),
    );
    Ok((Some(node), right_offset + 1))
}

fn match_return_emit_raise(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }
    if !is_identifier(&tokens[current], "return") && !is_identifier(&tokens[current], "raise") {
        return Ok((None, 0));
    }
    let right_tokens = tokens[current + 1..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right = right.unwrap();

    let node_type = match tokens[current].first().unwrap().token().as_str() {
        "return" => ASTNodeType::Return,
        "raise" => ASTNodeType::Raise,
        _ => unreachable!(),
    };
    Ok((
        Some(ASTNode::new(
            node_type,
            tokens[current].first().cloned(),
            tokens[current + right_offset].last().cloned(),
            Some(vec![right]),
        )),
        right_offset + 1,
    ))
}

fn match_tuple(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    let mut offset = 0usize;
    let mut left_tokens = Vec::<GatheredTokens>::new();
    let mut last_offset = 0usize;
    let mut separated = Vec::<ASTNode>::new();
    while current + offset < tokens.len() {
        if is_symbol(&tokens[current + offset], ",") {
            if !left_tokens.is_empty() {
                let (node, node_offset) = match_all(&left_tokens, 0)?;
                if node.is_none() {
                    return Ok((None, 0));
                }
                if node_offset != left_tokens.len() {
                    return Err(ParserError::NotFullyMatched(
                        left_tokens.first().unwrap().first().unwrap().clone(),
                        left_tokens.last().unwrap().last().unwrap().clone(),
                    ));
                }
                separated.push(node.unwrap());
                left_tokens.clear();
            }
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
    if !left_tokens.is_empty() {
        let (node, node_offset) = match_all(&left_tokens, 0)?;
        if node.is_none() {
            return Ok((None, 0));
        }
        separated.push(node.unwrap());
        last_offset += node_offset;
    }
    Ok((
        Some(ASTNode::new(
            ASTNodeType::Tuple,
            tokens[current].first().cloned(),
            tokens[current + last_offset - 1].last().cloned(),
            Some(separated),
        )),
        last_offset,
    ))
}

fn match_let(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right = right.unwrap();

    let (left, left_offset) = match_all(&left_tokens, 0)?;
    if left.is_none() {
        return Ok((None, 0));
    }
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let left = left.unwrap();

    match left.node_type {
        ASTNodeType::Variable(name) => Ok((
            Some(ASTNode::new(
                ASTNodeType::Let(name),
                tokens[current].first().cloned(),
                tokens[current + right_offset + 1].last().cloned(),
                Some(vec![right]),
            )),
            right_offset + 2,
        )),
        ASTNodeType::String(name) => Ok((
            Some(ASTNode::new(
                ASTNodeType::Let(name),
                tokens[current].first().cloned(),
                tokens[current + right_offset + 1].last().cloned(),
                Some(vec![right]),
            )),
            right_offset + 2,
        )),
        _ => Err(ParserError::InvalidVariableName(
            tokens[current].first().unwrap().clone(),
        )),
    }
}

fn match_assign(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            && tokens[current + offset][0] == TokenType::SYMBOL
            && tokens[current + offset][0] == "="
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
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
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right = right.unwrap();

    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Assign,
            tokens[current].first().cloned(),
            tokens[current + offset + right_offset].last().cloned(),
            Some(vec![left, right]),
        )),
        offset + right_offset + 1, // +1 是因为 = 符号
    ));
}
fn match_named_to(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let mut left = left.unwrap();

    if let ASTNodeType::Variable(name) = left.node_type {
        left = ASTNode::new(
            ASTNodeType::String(name),
            left.start_token.clone(),
            left.start_token.clone(),
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
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right = right.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::Pair,
            tokens[current].first().cloned(),
            tokens[current + right_offset + 1].last().cloned(),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}

fn match_pair(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
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
            right_tokens.first().unwrap().first().cloned().unwrap(),
            right_tokens.last().unwrap().last().cloned().unwrap(),
        ));
    }
    let right = right.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::Pair,
            tokens[current].first().cloned(),
            tokens[current + right_offset + 1].last().cloned(),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}

fn match_while(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            condition_tokens.first().unwrap().first().unwrap().clone(),
            condition_tokens.last().unwrap().last().unwrap().clone(),
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
            body_tokens.first().unwrap().first().unwrap().clone(),
            body_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let body = body.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::While,
            tokens[current].first().cloned(),
            tokens[current + body_offset + 1].last().cloned(),
            Some(vec![condition, body]),
        )),
        body_offset + 2,
    ))
}

fn match_if(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            condition_tokens.first().unwrap().first().unwrap().clone(),
            condition_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let condition = condition.unwrap();

    let (true_condition, true_condition_offset) = match_all(&true_condition_tokens, 0)?;
    if true_condition.is_none() {
        return Ok((None, 0));
    }
    if true_condition_offset != true_condition_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            true_condition_tokens
                .first()
                .unwrap()
                .first()
                .unwrap()
                .clone(),
            true_condition_tokens
                .last()
                .unwrap()
                .last()
                .unwrap()
                .clone(),
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
                false_condition_tokens
                    .first()
                    .unwrap()
                    .first()
                    .unwrap()
                    .clone(),
                false_condition_tokens
                    .last()
                    .unwrap()
                    .last()
                    .unwrap()
                    .clone(),
            ));
        }
        let false_condition = false_condition.unwrap();
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::If,
                tokens[current].first().cloned(),
                tokens[current + false_condition_offset + 3].last().cloned(),
                Some(vec![condition, true_condition, false_condition]),
            )),
            false_condition_offset + 4,
        ));
    }
    Ok((
        Some(ASTNode::new(
            ASTNodeType::If,
            tokens[current].first().cloned(),
            tokens[current + true_condition_offset + 1].last().cloned(),
            Some(vec![condition, true_condition]),
        )),
        true_condition_offset + 2,
    ))
}

fn match_break_and_continue(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
                right_tokens.first().unwrap().first().unwrap().clone(),
                right_tokens.last().unwrap().last().unwrap().clone(),
            ));
        }
        let right = right.unwrap();
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Break,
                tokens[current].first().cloned(),
                tokens[current + right_offset].last().cloned(),
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
                right_tokens.first().unwrap().first().unwrap().clone(),
                right_tokens.last().unwrap().last().unwrap().clone(),
            ));
        }
        let right = right.unwrap();
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Continue,
                tokens[current].first().cloned(),
                tokens[current + right_offset].last().cloned(),
                Some(vec![right]),
            )),
            right_offset + 1,
        ));
    }
    Ok((None, 0))
}

fn match_or(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(ASTNodeOperation::Or),
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

fn match_and(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(ASTNodeOperation::And),
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

fn match_xor(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(ASTNodeOperation::Xor),
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

fn match_not(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
                not_expr.first().unwrap().first().unwrap().clone(),
                not_expr.last().unwrap().last().unwrap().clone(),
            ));
        }
        let node = node.unwrap();
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Operation(ASTNodeOperation::Not),
                tokens[current].first().cloned(),
                tokens[current + node_offset].last().cloned(),
                Some(vec![node]),
            )),
            node_offset + 1,
        ));
    }
    Ok((None, 0))
}

// >, <, >=, <=, ==, !=
fn match_operation_compare(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator = None;
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
            operator = Some(tokens[pos][0].token());
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let operation = match operator.unwrap().as_str() {
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
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

// +, -
fn match_operation_add_sub(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    let mut offset: usize = tokens.len().saturating_sub(current).saturating_sub(1);
    let mut operator = None;
    let mut operator_pos: usize = 0;

    // 从右往左查找 + 或 - 操作符
    while offset > 0 {
        let pos: usize = current + offset;
        if is_symbol(&tokens[pos], "+") || is_symbol(&tokens[pos], "-") {
            let op_token = tokens[pos][0].token();

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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
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
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
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
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // 返回整个匹配长度
    ));
}

fn match_operation_mul_div_mod(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    let mut offset: usize = tokens.len().saturating_sub(current).saturating_sub(1);
    let mut operator = None;
    let mut operator_pos: usize = 0;

    // 从右往左查找 *, / 或 % 操作符
    while offset > 0 {
        let pos = current + offset;
        if is_symbol(&tokens[pos], "*")
            || is_symbol(&tokens[pos], "/")
            || is_symbol(&tokens[pos], "%")
        {
            operator = Some(tokens[pos][0].token());
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
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
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }

    // 确定操作类型
    let operation = match operator.unwrap().as_str() {
        "*" => ASTNodeOperation::Multiply,
        "/" => ASTNodeOperation::Divide,
        "%" => ASTNodeOperation::Modulus,
        _ => unreachable!(),
    };

    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(operation),
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // 返回整个匹配长度
    ));
}

fn match_bitwise_shift(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator = None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = current + offset;
        if is_symbol(&tokens[pos], "<<") || is_symbol(&tokens[pos], ">>") {
            operator = Some(tokens[pos][0].token());
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(if operator.unwrap() == "<<" {
                ASTNodeOperation::LeftShift
            } else {
                ASTNodeOperation::RightShift
            }),
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

fn match_unary(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
                unary_expr.first().unwrap().first().unwrap().clone(),
                unary_expr.last().unwrap().last().unwrap().clone(),
            ));
        }
        let node = node.unwrap();
        let operation = match tokens[current].first().unwrap().token().as_str() {
            "-" => ASTNodeOperation::Minus,
            "+" => ASTNodeOperation::Abs,
            _ => unreachable!(),
        };
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Operation(operation),
                tokens[current].first().cloned(),
                tokens[current + node_offset].last().cloned(),
                Some(vec![node]),
            )),
            node_offset + 1,
        ));
    }
    Ok((None, 0))
}

fn match_power(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let (right, right_offset) = match_all(right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Operation(ASTNodeOperation::Power),
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}
fn match_map(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
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
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }

    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Map,
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // 返回整个匹配长度
    ));
}
fn match_set_def(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
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
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }

    return Ok((
        Some(ASTNode::new(
            ASTNodeType::Set,
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left, right]),
        )),
        tokens.len() - current, // 返回整个匹配长度
    ));
}

fn match_lambda_def(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }

    let mut body_start_index = current + 2; // Index after params ->
    let mut capture_vars = HashSet::new();
    if is_symbol(&tokens[body_start_index], "&") {
        // If there's an '&' symbol, it indicates a capture
        body_start_index += 1; // Skip the '&'
        if body_start_index >= tokens.len() {
            return Err(ParserError::MissingStructure(
                tokens.last().unwrap().last().unwrap().clone(), // Point to the last token
                "Lambda capture".to_string(),
            ));
        }
        // then we expect a capture name or tuple
        let capture = gather(tokens[body_start_index])?;
        let (capture_node, capture_offset) = match_all(&capture, 0)?;
        if capture_node.is_none() {
            return Err(ParserError::MissingStructure(
                tokens[body_start_index].first().unwrap().clone(), // Point to the capture token
                "Lambda capture".to_string(),
            ));
        }
        if capture_offset != capture.len() {
            return Err(ParserError::NotFullyMatched(
                capture.first().unwrap().first().unwrap().clone(),
                capture.last().unwrap().last().unwrap().clone(),
            ));
        }
        let capture = capture_node.unwrap();
        match capture.node_type {
            ASTNodeType::String(v) | ASTNodeType::Variable(v) => {
                // Valid capture variable
                capture_vars.insert(v);
            }
            ASTNodeType::Tuple => {
                for child in &capture.children {
                    if let ASTNodeType::String(v) | ASTNodeType::Variable(v) = &child.node_type {
                        capture_vars.insert(v.clone());
                    } else {
                        return Err(ParserError::ErrorStructure(
                            tokens[body_start_index].first().unwrap().clone(),
                            "Capture must be a variable or tuple of variables".to_string(),
                        ));
                    }
                }
            }
            _ => {
                return Err(ParserError::ErrorStructure(
                    tokens[body_start_index].first().unwrap().clone(),
                    "Capture must be a variable or tuple of variables".to_string(),
                ));
            }
        }
        body_start_index += 1;
    }

    // Check for 'dyn' keyword
    let is_dyn = body_start_index < tokens.len() && is_identifier(&tokens[body_start_index], "dyn");
    if is_dyn {
        body_start_index += 1;
    }

    // Check if there's anything left for the body
    if body_start_index >= tokens.len() {
        return Err(ParserError::MissingStructure(
            tokens.last().unwrap().last().unwrap().clone(), // Point to the last token
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
            tokens[body_start_index - 1].last().unwrap().clone(), // Point to token before body start
            "Lambda body expression".to_string(),
        ));
    }
    if right_offset != body_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            body_tokens.first().unwrap().first().unwrap().clone(),
            body_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right = right.unwrap();

    let total_offset = body_start_index - current + right_offset;

    Ok((
        Some(ASTNode::new(
            // Use the new ASTNodeType variant
            ASTNodeType::LambdaDef(is_dyn, capture_vars),
            tokens[current].first().cloned(), // Start of parameters
            tokens[current + total_offset - 1].last().cloned(), // End of body
            Some(vec![left, right]),
        )),
        total_offset, // Total number of token groups consumed
    ))
}

fn match_quick_call(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
                left_tokens.first().unwrap().first().unwrap().clone(),
                left_tokens.last().unwrap().last().unwrap().clone(),
            ));
        }
        let right_tokens = tokens.get(current + 2..).unwrap_or(&[]).to_vec();
        let (right, right_offset) = match_all(&right_tokens, 0)?;
        if right.is_none() {
            return Ok((None, 0));
        }
        if right_offset != right_tokens.len() {
            return Err(ParserError::NotFullyMatched(
                right_tokens.first().unwrap().first().unwrap().clone(),
                right_tokens.last().unwrap().last().unwrap().clone(),
            ));
        }
        let right = right.unwrap();

        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Apply,
                tokens[current].first().cloned(),
                tokens[current + right_offset + 1].last().cloned(),
                Some(vec![left, right]),
            )),
            right_offset + 2,
        ));
    }
    Ok((None, 0))
}

fn match_modifier(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    if current + 1 >= tokens.len() {
        return Ok((None, 0));
    }
    if tokens[current].len() == 1
        && vec![
            "mut", "const", "keyof", "valueof", "assert", "import", "typeof", "await", "lengthof",
            "launch", "spawn", "async", "sync", "atomic",
        ]
        .contains(&tokens[current].first().unwrap().token().as_str())
    {
        let modify_expr = tokens[current + 1..].to_vec();
        let (node, node_offset) = match_all(&modify_expr, 0)?;
        if node.is_none() {
            return Ok((None, 0));
        }
        if node_offset != modify_expr.len() {
            return Err(ParserError::NotFullyMatched(
                modify_expr.first().unwrap().first().unwrap().clone(),
                modify_expr.last().unwrap().last().unwrap().clone(),
            ));
        }
        let node = node.unwrap();

        let modifier = match tokens[current].first().unwrap().token().as_str() {
            "mut" => ASTNodeModifier::Mut,
            "const" => ASTNodeModifier::Const,
            "keyof" => ASTNodeModifier::KeyOf,
            "valueof" => ASTNodeModifier::ValueOf,
            "assert" => ASTNodeModifier::Assert,
            "import" => ASTNodeModifier::Import,
            "typeof" => ASTNodeModifier::TypeOf,
            "await" => ASTNodeModifier::Await,
            "lengthof" => ASTNodeModifier::LengthOf,
            "launch" => ASTNodeModifier::Launch,
            "spawn" => ASTNodeModifier::Spawn,
            "async" => ASTNodeModifier::Async,
            "sync" => ASTNodeModifier::Sync,
            "atomic" => ASTNodeModifier::Atomic,
            _ => return Ok((None, 0)),
        };
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Modifier(modifier),
                tokens[current].first().cloned(),
                tokens[current + node_offset].last().cloned(),
                Some(vec![node]),
            )),
            node_offset + 1,
        ));
    }
    Ok((None, 0))
}

fn match_quick_named_to(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
                left_tokens.first().unwrap().first().unwrap().clone(),
                left_tokens.last().unwrap().last().unwrap().clone(),
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
                ASTNodeType::Pair,
                tokens[current].first().cloned(),
                tokens.last().unwrap().last().cloned(),
                Some(vec![
                    node,
                    ASTNode::new(ASTNodeType::Boolean(true), None, None, None),
                ]),
            )),
            node_offset + 1,
        ));
    }
    Ok((None, 0))
}

fn match_assume_tuple(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    Ok((
        Some(ASTNode::new(
            ASTNodeType::AssumeTuple,
            tokens[current].first().cloned(),
            tokens[current + node_offset].last().cloned(),
            Some(vec![node]),
        )),
        node_offset + 1,
    ))
}

fn match_alias(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            type_tokens.first().unwrap().first().unwrap().clone(),
            type_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }

    let type_name = match &type_node.node_type {
        ASTNodeType::Variable(name) => name.clone(),
        ASTNodeType::String(name) => name.clone(),
        _ => {
            return Err(ParserError::InvalidSyntax(
                tokens[current].first().unwrap().clone(),
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
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let value_node = value_node.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::Namespace(type_name),
            tokens[current].first().cloned(),
            tokens[current + value_offset + 1].last().cloned(),
            Some(vec![value_node]),
        )),
        value_offset + 2,
    ))
}

fn match_member_access_and_apply(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    // x y or x.y
    if current + 1 >= tokens.len() {
        return Ok((None, 0));
    }

    let is_member_access = is_symbol(&tokens[tokens.len() - 2], ".");
    let left = if is_member_access {
        tokens[current..tokens.len() - 2].to_vec()
    } else {
        tokens[current..tokens.len() - 1].to_vec()
    };
    let (left_node, left_offset) = match_all(&left, 0)?;
    if left_node.is_none() {
        return Ok((None, 0));
    }
    let left_node = left_node.unwrap();
    if left_offset != left.len() {
        return Err(ParserError::NotFullyMatched(
            left.first().unwrap().first().unwrap().clone(),
            left.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right_tokens = gather(&tokens.last().unwrap())?;
    let (right_node, right_offset) = match_all(&right_tokens, 0)?;
    if right_node.is_none() {
        return Ok((None, 0));
    }
    let mut right_node = right_node.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }

    if let ASTNodeType::Variable(name) = &right_node.node_type
        && is_member_access
    {
        // 如果是变量名，转换为字符串节点
        right_node = ASTNode {
            node_type: ASTNodeType::String(name.clone()),
            start_token: right_node.start_token,
            end_token: right_node.end_token,
            children: right_node.children,
        };
    }

    return Ok((
        Some(ASTNode::new(
            if is_member_access {
                ASTNodeType::GetAttr
            } else {
                ASTNodeType::Apply
            },
            tokens[current].first().cloned(),
            tokens.last().unwrap().last().cloned(),
            Some(vec![left_node, right_node]),
        )),
        tokens.len() - current, // return full length of the tokens
    ));
}

fn match_range(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
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
            tokens[current].first().cloned(),
            tokens[current + right_offset + 1].last().cloned(),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}

fn match_in(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right_tokens = &tokens[current + 2..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right = right.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::In,
            tokens[current].first().cloned(),
            tokens[current + right_offset + 1].last().cloned(),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}

fn match_is(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
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
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right_tokens = &tokens[current + 2..].to_vec();
    let (right, right_offset) = match_all(&right_tokens, 0)?;
    if right.is_none() {
        return Ok((None, 0));
    }
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }
    let right = right.unwrap();

    Ok((
        Some(ASTNode::new(
            ASTNodeType::Is,
            tokens[current].first().cloned(),
            tokens[current + right_offset + 1].last().cloned(),
            Some(vec![left, right]),
        )),
        right_offset + 2,
    ))
}

fn match_as(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    // 搜索 `as` 关键字
    if current + 2 >= tokens.len() {
        return Ok((None, 0));
    }

    // 从右向左扫描 `as` 关键字所在的位置
    let mut offset: usize = tokens.len() - current - 1;
    let mut operator_pos: usize = 0;
    let mut found = false;

    while offset > 0 {
        let pos = current + offset;
        if is_identifier(&tokens[pos], "as") {
            operator_pos = pos;
            found = true;
            break;
        }
        offset -= 1;
    }

    if !found {
        return Ok((None, 0));
    }

    // 解析左侧表达式（要转换的值）
    let left_tokens = &tokens[current..operator_pos].to_vec();
    let (left, left_offset) = match_all(left_tokens, 0)?;

    if left.is_none() {
        return Ok((None, 0));
    }

    let left = left.unwrap();
    if left_offset != left_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            left_tokens.first().unwrap().first().unwrap().clone(),
            left_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }

    // 解析右侧表达式（目标类型，应该是一个变量如 int, string 等）
    let right_tokens = &tokens[operator_pos + 1..].to_vec();
    let (right, right_offset) = match_all(right_tokens, 0)?;

    if right.is_none() {
        return Ok((None, 0));
    }

    let right = right.unwrap();
    if right_offset != right_tokens.len() {
        return Err(ParserError::NotFullyMatched(
            right_tokens.first().unwrap().first().unwrap().clone(),
            right_tokens.last().unwrap().last().unwrap().clone(),
        ));
    }

    // 将 x as int 转换为 int(x) 的 AST 结构
    // 即：LambdaCall(right, Tuple(left))

    // right 应该是类型名变量，如 int、string 等
    // 将 left 作为参数包装成元组
    let args_tuple = ASTNode::new(
        ASTNodeType::Tuple,
        left.start_token.clone(),
        left.end_token.clone(),
        Some(vec![left]),
    );

    // 创建函数调用 type_name(value)
    let function_call = ASTNode::new(
        ASTNodeType::Apply,
        tokens[current].first().cloned(),
        tokens.last().unwrap().last().cloned(),
        Some(vec![right, args_tuple]),
    );

    Ok((
        Some(function_call),
        tokens.len() - current, // 返回整个匹配长度
    ))
}

fn match_variable(
    tokens: &Vec<GatheredTokens>,
    current: usize,
) -> Result<(Option<ASTNode>, usize), ParserError> {
    if current >= tokens.len() {
        return Ok((None, 0));
    }

    // 匹配括号内容（元组）
    if is_bracket(&tokens[current]) || is_square_bracket(&tokens[current]) {
        let inner_tokens = unwrap_brace(&tokens[current])?;
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
                ASTNodeType::Frame,
                tokens[current].first().cloned(),
                tokens[current].last().cloned(),
                body.map(|b| vec![b]),
            )),
            1,
        ));
    }

    // 匹配字符串常量
    if tokens[current].len() == 1 && tokens[current].first().unwrap() == TokenType::STRING {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::String(tokens[current].first().unwrap().token().clone()),
                tokens[current].first().cloned(),
                tokens[current].last().cloned(),
                None,
            )),
            1,
        ));
    }

    // 匹配数字常量
    if tokens[current].len() == 1 && tokens[current].first().unwrap() == TokenType::NUMBER {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Number(tokens[current].first().unwrap().token().clone()),
                tokens[current].first().cloned(),
                tokens[current].last().cloned(),
                None,
            )),
            1,
        ));
    }

    // 匹配b64
    if tokens[current].len() == 1 && tokens[current].first().unwrap() == TokenType::BASE64 {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Base64(tokens[current].first().unwrap().token().clone()),
                tokens[current].first().cloned(),
                tokens[current].last().cloned(),
                None,
            )),
            1,
        ));
    }

    // 匹配布尔值（true）
    if is_identifier(&tokens[current], "true") {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Boolean(true),
                tokens[current].first().cloned(),
                tokens[current].last().cloned(),
                None,
            )),
            1,
        ));
    }

    // 匹配布尔值（false）
    if is_identifier(&tokens[current], "false") {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Boolean(false),
                tokens[current].first().cloned(),
                tokens[current].last().cloned(),
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
                tokens[current].first().cloned(),
                tokens[current].last().cloned(),
                None,
            )),
            1,
        ));
    }

    // undefined
    if is_identifier(&tokens[current], "undefined") {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Undefined,
                tokens[current].first().cloned(),
                tokens[current].last().cloned(),
                None,
            )),
            1,
        ));
    }

    // 匹配普通变量名
    if tokens[current].len() == 1 && tokens[current].first().unwrap() == TokenType::IDENTIFIER {
        return Ok((
            Some(ASTNode::new(
                ASTNodeType::Variable(tokens[current].first().unwrap().token().clone()),
                tokens[current].first().cloned(),
                tokens[current].last().cloned(),
                None,
            )),
            1,
        ));
    }
    Ok((None, 0))
}
