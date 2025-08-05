use crate::{
    diagnostics::{Diagnostic, SourceLocation, collector::DiagnosticCollector},
    parser::lexer::{Token, TokenType},
};
use std::{collections::HashSet, fmt::Debug, vec};

#[derive(Debug, Clone)]
pub enum ASTParseDiagnostic {
    UnexpectedToken(Option<SourceLocation>),
    UnmatchedParenthesis(Option<SourceLocation>),
    InvalidVariableName(Option<SourceLocation>),
    UnsupportedStructure(Option<SourceLocation>),
    MissingStructure(Option<SourceLocation>, String),
    InvalidSyntax(Option<SourceLocation>, String),
}

impl Diagnostic for ASTParseDiagnostic {
    fn severity(&self) -> crate::diagnostics::ReportSeverity {
        match self {
            ASTParseDiagnostic::UnexpectedToken(_) => crate::diagnostics::ReportSeverity::Error,
            ASTParseDiagnostic::UnmatchedParenthesis(_) => {
                crate::diagnostics::ReportSeverity::Error
            }
            ASTParseDiagnostic::InvalidVariableName(_) => crate::diagnostics::ReportSeverity::Error,
            ASTParseDiagnostic::UnsupportedStructure(_) => {
                crate::diagnostics::ReportSeverity::Error
            }
            ASTParseDiagnostic::MissingStructure(_, _) => crate::diagnostics::ReportSeverity::Error,
            ASTParseDiagnostic::InvalidSyntax(_, _) => crate::diagnostics::ReportSeverity::Error,
        }
    }

    fn title(&self) -> String {
        "AST Parse Error".into()
    }

    fn message(&self) -> String {
        match self {
            ASTParseDiagnostic::UnexpectedToken(_) => "Unexpected token found".into(),
            ASTParseDiagnostic::UnmatchedParenthesis(_) => "Unmatched parenthesis found".into(),
            ASTParseDiagnostic::InvalidVariableName(_) => "Invalid variable name".into(),
            ASTParseDiagnostic::UnsupportedStructure(_) => "Unsupported structure".into(),
            ASTParseDiagnostic::MissingStructure(_, expected) => {
                format!("Missing structure: {}", expected)
            }
            ASTParseDiagnostic::InvalidSyntax(_, expected) => {
                format!("Invalid syntax: {}", expected)
            }
        }
    }

    fn location(&self) -> Option<SourceLocation> {
        match self {
            ASTParseDiagnostic::UnexpectedToken(loc) => loc.clone(),
            ASTParseDiagnostic::UnmatchedParenthesis(loc) => loc.clone(),
            ASTParseDiagnostic::InvalidVariableName(loc) => loc.clone(),
            ASTParseDiagnostic::UnsupportedStructure(loc) => loc.clone(),
            ASTParseDiagnostic::MissingStructure(loc, _) => loc.clone(),
            ASTParseDiagnostic::InvalidSyntax(loc, _) => loc.clone(),
        }
    }

    fn help(&self) -> Option<String> {
        match self {
            ASTParseDiagnostic::UnexpectedToken(_) => Some("Check the syntax near the unexpected token.".into()),
            ASTParseDiagnostic::UnmatchedParenthesis(_) => Some("Ensure all parentheses are properly matched.".into()),
            ASTParseDiagnostic::InvalidVariableName(_) => Some("Variable names must start with a letter and can only contain letters, numbers, and underscores.".into()),
            ASTParseDiagnostic::UnsupportedStructure(_) => None,
            ASTParseDiagnostic::MissingStructure(_, _) => None,
            ASTParseDiagnostic::InvalidSyntax(_, _) => None,
        }
    }

    fn copy(&self) -> Box<dyn Diagnostic> {
        Box::new(self.clone())
    }
}

pub type TokenStream = Vec<Token>;
pub type GatheredTokens<'t> = &'t [Token];

pub mod ast_token_stream {
    pub fn from_stream<'t>(stream: &'t super::TokenStream) -> super::GatheredTokens<'t> {
        stream.as_slice()
    }
}

fn get_next_tokens<'a, 'b>(
    collector: &'a mut DiagnosticCollector,
    tokens: GatheredTokens<'b>,
    offset: usize,
) -> Result<GatheredTokens<'b>, ()> {
    let mut stack = Vec::<(Token, usize)>::new();
    let mut next_tokens_end = 0usize;
    let mut index = offset;
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
                return collector.fatal(ASTParseDiagnostic::UnmatchedParenthesis(
                    span_from_two_token(&tokens[last_position], &tokens[index]),
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
        return collector.fatal(ASTParseDiagnostic::UnmatchedParenthesis(
            span_from_two_token(&tokens[last_position], &tokens[index - 1]),
        ));
    }
    Ok(&tokens[..next_tokens_end])
}

fn gather<'a, 'b>(
    collector: &'a mut DiagnosticCollector,
    tokens: GatheredTokens<'b>,
) -> Result<Vec<GatheredTokens<'b>>, ()> {
    let mut offset = 0;
    let mut result = Vec::<GatheredTokens>::new();
    while offset < tokens.len() {
        let next_tokens = get_next_tokens(collector, tokens, offset)?;
        if next_tokens.is_empty() {
            return collector.fatal(ASTParseDiagnostic::UnsupportedStructure(
                span_from_single_token(&tokens[offset]),
            ));
        }
        offset += next_tokens.len();
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
    Abs,          // 一元绝对值
    Minus,        // 一元负号
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
    LengthOf, // LengthOf
    Launch,   // Launch
    Spawn,    // Spawn
    Async,
    Sync,
    Atomic,
}

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub node_type: ASTNodeType,                  // Type of the node
    pub source_location: Option<SourceLocation>, // Source location for error reporting
    pub children: Vec<ASTNode>,                  // Children of the node
}

impl PartialEq for ASTNode {
    fn eq(&self, other: &Self) -> bool {
        self.node_type == other.node_type && self.children == other.children
    }
}

impl ASTNode {
    pub fn new(
        node_type: ASTNodeType,
        source_location: Option<SourceLocation>,
        children: Vec<ASTNode>,
    ) -> ASTNode {
        ASTNode {
            node_type,
            source_location,
            children,
        }
    }

    pub fn undefined() -> ASTNode {
        ASTNode::new(ASTNodeType::Undefined, None, vec![])
    }

    #[allow(unused)]
    pub fn formatted_print(&self, indent: usize) {
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
                child.formatted_print(indent + 2);
            }
        }
    }
}

type MatcherFn = fn(&mut DiagnosticCollector, &[GatheredTokens]) -> Result<Option<ASTNode>, ()>;

struct NodeMatcher {
    matchers: Vec<MatcherFn>,
}

fn span_from_tokens(tokens: &[GatheredTokens]) -> Option<SourceLocation> {
    if tokens.is_empty() {
        return None;
    }
    let first_token = tokens.first().and_then(|t| t.first());
    let last_token = tokens.last().and_then(|t| t.last());
    if let (Some(start_loc), Some(end_loc)) = (first_token, last_token) {
        if start_loc.source_code().eq(end_loc.source_code()) {
            return Some(SourceLocation {
                span: (
                    start_loc.origin_token_span().0,
                    end_loc.origin_token_span().1,
                ),
                source: start_loc.source_code().clone(),
            });
        }
    }
    None
}

fn span_from_two_token(start: &Token, end: &Token) -> Option<SourceLocation> {
    if start.source_code().eq(end.source_code()) {
        return Some(SourceLocation {
            span: (start.origin_token_span().0, end.origin_token_span().1),
            source: start.source_code().clone(),
        });
    }
    None
}

fn span_from_single_token(token: &Token) -> Option<SourceLocation> {
    Some(SourceLocation {
        span: token.origin_token_span(),
        source: token.source_code().clone(),
    })
}

impl NodeMatcher {
    fn new() -> NodeMatcher {
        NodeMatcher {
            matchers: Vec::new(),
        }
    }

    fn add_matcher(
        &mut self,
        matcher: fn(&mut DiagnosticCollector, &[GatheredTokens]) -> Result<Option<ASTNode>, ()>,
    ) {
        self.matchers.push(matcher);
    }

    #[stacksafe::stacksafe]
    fn match_node(
        &self,
        collector: &mut DiagnosticCollector,
        tokens: &[GatheredTokens],
    ) -> Result<Option<ASTNode>, ()> {
        if tokens.is_empty() {
            return Ok(Some(ASTNode::new(ASTNodeType::Tuple, None, vec![])));
        }
        let mut best_match: Option<ASTNode> = None;
        // 遍历所有注册的语法规则匹配器
        for matcher in &self.matchers {
            match matcher(collector, tokens) {
                Ok(Some(node)) => {
                    // 更新最佳匹配
                    best_match = Some(node);
                    break;
                }
                Ok(None) => {
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
        match best_match {
            Some(node) => Ok(Some(node)),
            None => {
                collector.fatal(ASTParseDiagnostic::UnsupportedStructure(span_from_tokens(
                    tokens,
                )))?;
                Err(())
            }
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
fn is_identifier(token: &GatheredTokens, identifier: &str) -> bool {
    if token.len() != 1 {
        return false;
    }
    let token = &token[0];
    token == TokenType::IDENTIFIER && token == identifier
}

fn unwrap_brace<'t, 'a>(
    collector: &'a mut DiagnosticCollector,
    token: &GatheredTokens<'t>,
) -> Result<GatheredTokens<'t>, ()> {
    if token.len() < 2 {
        return collector.fatal(ASTParseDiagnostic::UnexpectedToken(span_from_single_token(
            &token[0],
        )));
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
    collector.fatal(ASTParseDiagnostic::UnexpectedToken(span_from_single_token(
        &token[0],
    )))
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

pub fn build_ast(
    collector: &mut DiagnosticCollector,
    tokens: GatheredTokens<'_>,
) -> Result<ASTNode, ()> {
    let gathered = gather(collector, tokens)?;
    let matched = match_all(collector, &gathered)?;
    if matched.is_none() {
        return Ok(ASTNode::new(ASTNodeType::Tuple, None, vec![]));
    }
    Ok(matched.unwrap())
}

fn match_all<'t>(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens<'t>],
) -> Result<Option<ASTNode>, ()> {
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
    node_matcher.match_node(collector, tokens)
}

macro_rules! try_match_node {
    ($collector:expr, $tokens:expr) => {
        match match_all($collector, $tokens) {
            Ok(Some(node)) => node,
            Ok(None) => return Ok(None),
            Err(e) => return Err(e),
        }
    };
}
fn match_expressions(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    let mut offset = 0usize;
    let mut left_start = 0usize;
    let mut separated = Vec::<ASTNode>::new();
    while offset < tokens.len() {
        if is_symbol(&tokens[offset], ";") {
            let left_tokens = &tokens[left_start..offset];
            separated.push(try_match_node!(collector, left_tokens));
            left_start = offset + 1;
        }
        offset += 1;
    }
    if separated.is_empty() {
        return Ok(None);
    }
    let left_tokens = &tokens[left_start..offset];
    separated.push(try_match_node!(collector, left_tokens));
    Ok(Some(ASTNode::new(
        ASTNodeType::Expressions,
        span_from_tokens(tokens),
        separated,
    )))
}

fn match_comptime(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.is_empty() || !is_symbol(&tokens[0], "@") {
        return Ok(None);
    }
    let right = try_match_node!(collector, &tokens[1..]);
    let node = ASTNode::new(
        ASTNodeType::Comptime,
        span_from_tokens(&tokens),
        vec![right],
    );
    Ok(Some(node))
}

fn match_dynamic_and_static(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() == 0
        || !is_symbol(&tokens[0], "dynamic") && !is_identifier(&tokens[0], "static")
    {
        return Ok(None);
    }
    let right = try_match_node!(collector, &tokens[1..]);
    let node = ASTNode::new(
        if is_identifier(&tokens[0], "dynamic") {
            ASTNodeType::Dynamic
        } else {
            ASTNodeType::Static
        },
        span_from_tokens(&tokens),
        vec![right],
    );
    Ok(Some(node))
}

fn match_return_emit_raise(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() == 0
        || !is_identifier(&tokens[0], "return") && !is_identifier(&tokens[0], "raise")
    {
        return Ok(None);
    }
    let right = try_match_node!(collector, &tokens[1..]);
    Ok(Some(ASTNode::new(
        if is_identifier(&tokens[0], "return") {
            ASTNodeType::Return
        } else {
            ASTNodeType::Raise
        },
        span_from_tokens(&tokens),
        vec![right],
    )))
}

fn match_tuple(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    let mut offset = 0usize;
    let mut left_tokens = Vec::<GatheredTokens>::new();
    let mut separated = Vec::<ASTNode>::new();
    while offset < tokens.len() {
        if is_symbol(&tokens[offset], ",") {
            if !left_tokens.is_empty() {
                separated.push(try_match_node!(collector, &left_tokens));
                left_tokens.clear();
            }
        } else {
            left_tokens.push(tokens[offset]);
        }
        offset += 1;
    }
    if separated.is_empty() {
        return Ok(None);
    }
    if !left_tokens.is_empty() {
        separated.push(try_match_node!(collector, &left_tokens));
    }
    Ok(Some(ASTNode::new(
        ASTNodeType::Tuple,
        span_from_tokens(&tokens),
        separated,
    )))
}

fn match_let(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() <= 2 || !is_symbol(&tokens[1], ":=") {
        return Ok(None);
    }
    let left_tokens = gather(collector, tokens[0])?;
    let left = try_match_node!(collector, &left_tokens);
    let right = try_match_node!(collector, &tokens[2..]);

    match left.node_type {
        ASTNodeType::Variable(name) | ASTNodeType::String(name) => Ok(Some(ASTNode::new(
            ASTNodeType::Let(name),
            span_from_tokens(&tokens),
            vec![right],
        ))),
        _ => {
            return collector.fatal(ASTParseDiagnostic::InvalidVariableName(span_from_tokens(
                &left_tokens,
            )));
        }
    }
}

fn match_assign(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    // 确保有足够的 token 来处理赋值
    if tokens.len() == 0 {
        return Ok(None);
    }

    // 向右搜索 = 符号
    let mut offset = 0;
    let mut left_tokens: Vec<&[Token]> = Vec::new();

    while offset < tokens.len() {
        // 找到 = 符号
        if is_symbol(&tokens[offset], "=") {
            break;
        }
        left_tokens.push(tokens[offset]);
        offset += 1;
    }

    // 没找到 = 符号
    if offset >= tokens.len() {
        return Ok(None);
    }

    let left = try_match_node!(collector, &left_tokens);
    let right = try_match_node!(collector, &tokens[offset + 1..]);

    return Ok(Some(ASTNode::new(
        ASTNodeType::Assign,
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}

fn match_named_to(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() <= 2 || !is_symbol(&tokens[1], "=>") {
        return Ok(None);
    }

    let left_tokens = gather(collector, tokens[0])?;
    let mut left = try_match_node!(collector, &left_tokens);

    if let ASTNodeType::Variable(name) = left.node_type {
        left = ASTNode::new(
            ASTNodeType::String(name),
            left.source_location,
            left.children,
        );
    }
    let right = try_match_node!(collector, &tokens[2..]);
    Ok(Some(ASTNode::new(
        ASTNodeType::Pair,
        span_from_tokens(&tokens),
        vec![left, right],
    )))
}

fn match_pair(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() <= 2 || !is_symbol(&tokens[1], ":") {
        return Ok(None);
    }

    let left_tokens = gather(collector, tokens[0])?;
    let left = try_match_node!(collector, &left_tokens);
    let right = try_match_node!(collector, &tokens[2..]);
    Ok(Some(ASTNode::new(
        ASTNodeType::Pair,
        span_from_tokens(&tokens),
        vec![left, right],
    )))
}

fn match_while(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() <= 2 || !is_identifier(&tokens[0], "while") {
        return Ok(None);
    }
    let condition_tokens = gather(collector, tokens[1])?;
    let condition = try_match_node!(collector, &condition_tokens);
    let body = try_match_node!(collector, &tokens[2..]);
    Ok(Some(ASTNode::new(
        ASTNodeType::While,
        span_from_tokens(&tokens),
        vec![condition, body],
    )))
}

fn match_if(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() <= 2 || !is_identifier(&tokens[0], "if") {
        return Ok(None);
    }

    let condition_tokens = gather(collector, tokens[1])?;
    let true_condition_tokens = gather(collector, tokens[2])?;

    let condition = try_match_node!(collector, &condition_tokens);
    let true_condition = try_match_node!(collector, &true_condition_tokens);

    if 3 < tokens.len() && is_identifier(&tokens[3], "else") {
        let false_condition = try_match_node!(collector, &tokens[4..]);
        return Ok(Some(ASTNode::new(
            ASTNodeType::If,
            span_from_tokens(&tokens),
            vec![condition, true_condition, false_condition],
        )));
    }
    Ok(Some(ASTNode::new(
        ASTNodeType::If,
        span_from_tokens(&tokens),
        vec![condition, true_condition],
    )))
}

fn match_break_and_continue(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() == 0 {
        return Ok(None);
    }
    if is_identifier(&tokens[0], "break") {
        let right = try_match_node!(collector, &tokens[1..]);
        return Ok(Some(ASTNode::new(
            ASTNodeType::Break,
            span_from_tokens(&tokens),
            vec![right],
        )));
    } else if is_identifier(&tokens[0], "continue") {
        let right = try_match_node!(collector, &tokens[1..]);
        return Ok(Some(ASTNode::new(
            ASTNodeType::Continue,
            span_from_tokens(&tokens),
            vec![right],
        )));
    }
    Ok(None)
}

fn match_or(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    let mut offset: usize = tokens.len() - 1;
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = offset;
        if is_identifier(&tokens[pos], "or") {
            operator = Some("or");
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok(None);
    }

    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);

    return Ok(Some(ASTNode::new(
        ASTNodeType::Operation(ASTNodeOperation::Or),
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}

fn match_and(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    let mut offset: usize = tokens.len() - 1;
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = offset;
        if is_identifier(&tokens[pos], "and") {
            operator = Some("and");
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok(None);
    }

    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);

    return Ok(Some(ASTNode::new(
        ASTNodeType::Operation(ASTNodeOperation::And),
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}

fn match_xor(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() == 0 {
        return Ok(None);
    }
    let mut offset: usize = tokens.len() - 1;
    let mut operator = Option::<&str>::None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = offset;
        if is_identifier(&tokens[pos], "xor") {
            operator = Some("xor");
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok(None);
    }

    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);

    return Ok(Some(ASTNode::new(
        ASTNodeType::Operation(ASTNodeOperation::Xor),
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}

fn match_not(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() <= 1 || !is_identifier(&tokens[0], "not") {
        return Ok(None);
    }
    let node = try_match_node!(collector, &tokens[1..]);
    return Ok(Some(ASTNode::new(
        ASTNodeType::Operation(ASTNodeOperation::Not),
        span_from_tokens(&tokens),
        vec![node],
    )));
}

// >, <, >=, <=, ==, !=
fn match_operation_compare(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() == 0 {
        return Ok(None);
    }
    let mut offset: usize = tokens.len() - 1;
    let mut operator = None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = offset;
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
        return Ok(None);
    }

    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);

    let operation = match operator.unwrap().as_str() {
        ">" => ASTNodeOperation::Greater,
        "<" => ASTNodeOperation::Less,
        ">=" => ASTNodeOperation::GreaterEqual,
        "<=" => ASTNodeOperation::LessEqual,
        "==" => ASTNodeOperation::Equal,
        "!=" => ASTNodeOperation::NotEqual,
        _ => unreachable!(),
    };
    return Ok(Some(ASTNode::new(
        ASTNodeType::Operation(operation),
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}

// +, -
fn match_operation_add_sub(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.is_empty() {
        return Ok(None);
    }
    let mut offset: usize = tokens.len() - 1;
    let mut operator = None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = offset;
        if is_symbol(&tokens[pos], "+") || is_symbol(&tokens[pos], "-") {
            operator = Some(tokens[pos][0].token());
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok(None);
    }
    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);
    let op = operator.unwrap();
    let operation = if op == "+" {
        ASTNodeOperation::Add
    } else {
        ASTNodeOperation::Subtract
    };
    return Ok(Some(ASTNode::new(
        ASTNodeType::Operation(operation),
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}

fn match_operation_mul_div_mod(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.is_empty() {
        return Ok(None);
    }
    let mut offset: usize = tokens.len() - 1;
    let mut operator = None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = offset;
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
        return Ok(None);
    }
    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);
    let operation = match operator.unwrap().as_str() {
        "*" => ASTNodeOperation::Multiply,
        "/" => ASTNodeOperation::Divide,
        "%" => ASTNodeOperation::Modulus,
        _ => unreachable!(),
    };
    return Ok(Some(ASTNode::new(
        ASTNodeType::Operation(operation),
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}

fn match_bitwise_shift(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.is_empty() {
        return Ok(None);
    }
    let mut offset: usize = tokens.len() - 1;
    let mut operator = None;
    let mut operator_pos: usize = 0;
    while offset > 0 {
        let pos = offset;
        if is_symbol(&tokens[pos], "<<") || is_symbol(&tokens[pos], ">>") {
            operator = Some(tokens[pos][0].token());
            operator_pos = pos;
            break;
        }
        offset -= 1;
    }
    if operator.is_none() {
        return Ok(None);
    }

    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);

    return Ok(Some(ASTNode::new(
        ASTNodeType::Operation(if operator.unwrap() == "<<" {
            ASTNodeOperation::LeftShift
        } else {
            ASTNodeOperation::RightShift
        }),
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}

fn match_unary(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.is_empty() {
        return Ok(None);
    }
    if is_symbol(&tokens[0], "-") || is_symbol(&tokens[0], "+") {
        if tokens.len() <= 1 {
            return Ok(None);
        };
        let right = try_match_node!(collector, &tokens[1..]);
        let operation = match tokens[0][0].token().as_str() {
            "-" => ASTNodeOperation::Minus,
            "+" => ASTNodeOperation::Abs,
            _ => unreachable!(),
        };
        return Ok(Some(ASTNode::new(
            ASTNodeType::Operation(operation),
            span_from_tokens(&tokens),
            vec![right],
        )));
    }
    Ok(None)
}

fn match_power(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() < 3 {
        return Ok(None);
    }

    // 正向搜索 **
    let find = tokens.iter().position(|token| is_symbol(token, "**"));
    if find.is_none() {
        return Ok(None);
    }
    let operator_pos = find.unwrap();
    if operator_pos + 1 >= tokens.len() {
        return Ok(None);
    }
    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);

    return Ok(Some(ASTNode::new(
        ASTNodeType::Operation(ASTNodeOperation::Power),
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}
fn match_map(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.is_empty() {
        return Ok(None);
    }

    // 从右向左查找 |> 操作符
    let mut offset: usize = tokens.len() - 1;
    let mut operator_pos: usize = 0;
    let mut found = false;

    while offset > 0 {
        let pos = offset;
        if is_symbol(&tokens[pos], "|>") {
            operator_pos = pos;
            found = true;
            break;
        }
        offset -= 1;
    }

    if !found {
        return Ok(None);
    }

    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);

    return Ok(Some(ASTNode::new(
        ASTNodeType::Map,
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}
fn match_set_def(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.is_empty() {
        return Ok(None);
    }

    // 从右向左查找 | 操作符
    let mut offset: usize = tokens.len() - 1;
    let mut operator_pos: usize = 0;
    let mut found = false;

    while offset > 0 {
        let pos = offset;
        if is_symbol(&tokens[pos], "|") {
            operator_pos = pos;
            found = true;
            break;
        }
        offset -= 1;
    }

    if !found {
        return Ok(None);
    }

    let left = try_match_node!(collector, &tokens[..operator_pos]);
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);

    return Ok(Some(ASTNode::new(
        ASTNodeType::Set,
        span_from_tokens(&tokens),
        vec![left, right],
    )));
}

fn match_lambda_def(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    // Need at least params, ->, body_start
    if tokens.len() < 3 {
        return Ok(None);
    }
    // Check for -> symbol
    if !is_symbol(&tokens[1], "->") {
        return Ok(None);
    }

    // Parse parameters (left part)
    let left_tokens = gather(collector, tokens[0])?;
    let left = try_match_node!(collector, &left_tokens);

    let mut body_start_index = 2; // Index after params ->
    let mut capture_vars = HashSet::new();
    if body_start_index < tokens.len() && is_symbol(&tokens[body_start_index], "&") {
        // If there's an '&' symbol, it indicates a capture
        body_start_index += 1; // Skip the '&'
        if body_start_index >= tokens.len() {
            return collector.fatal(ASTParseDiagnostic::MissingStructure(
                span_from_single_token(tokens.last().unwrap().last().unwrap()), // Point to the last token
                "Lambda capture".to_string(),
            ));
        }
        // then we expect a capture name or tuple
        let capture = gather(collector, tokens[body_start_index])?;
        let capture_node = try_match_node!(collector, &capture);
        match capture_node.node_type {
            ASTNodeType::String(v) | ASTNodeType::Variable(v) => {
                // Valid capture variable
                capture_vars.insert(v);
            }
            ASTNodeType::Tuple => {
                for child in &capture_node.children {
                    if let ASTNodeType::String(v) | ASTNodeType::Variable(v) = &child.node_type {
                        capture_vars.insert(v.clone());
                    } else {
                        return collector.fatal(ASTParseDiagnostic::InvalidSyntax(
                            span_from_single_token(tokens[body_start_index].first().unwrap()),
                            "Capture must be a variable or tuple of variables".to_string(),
                        ));
                    }
                }
            }
            _ => {
                return collector.fatal(ASTParseDiagnostic::InvalidSyntax(
                    span_from_single_token(tokens[body_start_index].first().unwrap()),
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
        return collector.fatal(ASTParseDiagnostic::MissingStructure(
            span_from_single_token(tokens.last().unwrap().last().unwrap()), // Point to the last token
            "Lambda body".to_string(),
        ));
    }

    // Parse the lambda body (right part)
    let right = try_match_node!(collector, &tokens[body_start_index..]);

    Ok(Some(ASTNode::new(
        // Use the new ASTNodeType variant
        ASTNodeType::LambdaDef(is_dyn, capture_vars),
        span_from_tokens(&tokens),
        vec![left, right],
    )))
}

fn match_modifier(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() < 2 {
        return Ok(None);
    }
    if tokens[0].len() == 1
        && vec![
            "mut", "const", "keyof", "valueof", "assert", "import", "typeof", "lengthof",
            "launch", "spawn", "async", "sync", "atomic",
        ]
        .contains(&tokens[0].first().unwrap().token().as_str())
    {
        let right = try_match_node!(collector, &tokens[1..]);
        let modifier = match tokens[0].first().unwrap().token().as_str() {
            "mut" => ASTNodeModifier::Mut,
            "const" => ASTNodeModifier::Const,
            "keyof" => ASTNodeModifier::KeyOf,
            "valueof" => ASTNodeModifier::ValueOf,
            "assert" => ASTNodeModifier::Assert,
            "import" => ASTNodeModifier::Import,
            "typeof" => ASTNodeModifier::TypeOf,
            "lengthof" => ASTNodeModifier::LengthOf,
            "launch" => ASTNodeModifier::Launch,
            "spawn" => ASTNodeModifier::Spawn,
            "async" => ASTNodeModifier::Async,
            "sync" => ASTNodeModifier::Sync,
            "atomic" => ASTNodeModifier::Atomic,
            _ => return Ok(None),
        };
        return Ok(Some(ASTNode::new(
            ASTNodeType::Modifier(modifier),
            span_from_tokens(&tokens),
            vec![right],
        )));
    }
    Ok(None)
}

fn match_quick_named_to(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    // expr ?
    if tokens.len() <= 1 {
        return Ok(None);
    }
    if is_symbol(&tokens[tokens.len() - 1], "?") {
        let left_tokens = tokens[..tokens.len() - 1].to_vec();
        let mut node = try_match_node!(collector, &left_tokens);
        if let ASTNodeType::Variable(name) = node.node_type {
            node = ASTNode::new(
                ASTNodeType::String(name),
                node.source_location,
                node.children,
            );
        }
        return Ok(Some(ASTNode::new(
            ASTNodeType::Pair,
            span_from_tokens(&tokens),
            vec![node, ASTNode::new(ASTNodeType::Boolean(true), None, vec![])],
        )));
    }
    Ok(None)
}

fn match_assume_tuple(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() < 2 {
        return Ok(None);
    }
    if !is_symbol(&tokens[0], "...") {
        return Ok(None);
    }
    let right = try_match_node!(collector, &tokens[1..]);
    Ok(Some(ASTNode::new(
        ASTNodeType::AssumeTuple,
        span_from_tokens(&tokens),
        vec![right],
    )))
}

fn match_alias(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() < 3 {
        return Ok(None);
    }

    if !is_symbol(&tokens[1], "::") {
        return Ok(None);
    }

    let type_tokens = gather(collector, tokens[0])?;
    let type_node = try_match_node!(collector, &type_tokens);

    let type_name = match &type_node.node_type {
        ASTNodeType::Variable(name) => name.clone(),
        ASTNodeType::String(name) => name.clone(),
        _ => {
            return collector.fatal(ASTParseDiagnostic::InvalidSyntax(
                span_from_single_token(tokens[0].first().unwrap()),
                "Expected identifier".to_string(),
            ));
        }
    };

    // 解析右侧值表达式
    let right = try_match_node!(collector, &tokens[2..]);

    Ok(Some(ASTNode::new(
        ASTNodeType::Namespace(type_name),
        span_from_tokens(&tokens),
        vec![right],
    )))
}

fn match_member_access_and_apply(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    // x y or x.y
    if tokens.len() <= 1 {
        return Ok(None);
    }

    let is_member_access = is_symbol(&tokens[tokens.len() - 2], ".");
    let left = if is_member_access {
        tokens[..tokens.len() - 2].to_vec()
    } else {
        tokens[..tokens.len() - 1].to_vec()
    };
    let left_node = try_match_node!(collector, &left);
    let right_tokens = gather(collector, &tokens.last().unwrap())?;
    let mut right_node = try_match_node!(collector, &right_tokens);

    if let ASTNodeType::Variable(name) = &right_node.node_type {
        if is_member_access {
            // 如果是变量名，转换为字符串节点
            right_node = ASTNode::new(
                ASTNodeType::String(name.clone()),
                right_node.source_location.clone(),
                right_node.children,
            );
        }
    }

    return Ok(Some(ASTNode::new(
        if is_member_access {
            ASTNodeType::GetAttr
        } else {
            ASTNodeType::Apply
        },
        span_from_tokens(&tokens),
        vec![left_node, right_node],
    )));
}

fn match_range(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() < 3 {
        return Ok(None);
    }
    if !is_symbol(&tokens[1], "..") {
        // x..y
        return Ok(None);
    }

    let left_tokens = gather(collector, tokens[0])?;
    let left = try_match_node!(collector, &left_tokens);
    let right = try_match_node!(collector, &tokens[2..]);

    Ok(Some(ASTNode::new(
        ASTNodeType::Range,
        span_from_tokens(&tokens),
        vec![left, right],
    )))
}

fn match_in(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if 2 >= tokens.len() {
        return Ok(None);
    }
    if !is_identifier(&tokens[1], "in") {
        return Ok(None);
    }

    let left_tokens = gather(collector, tokens[0])?;
    let left = try_match_node!(collector, &left_tokens);
    let right = try_match_node!(collector, &tokens[2..]);

    Ok(Some(ASTNode::new(
        ASTNodeType::In,
        span_from_tokens(&tokens),
        vec![left, right],
    )))
}

fn match_is(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.len() <= 2 {
        return Ok(None);
    }
    if !is_identifier(&tokens[1], "is") {
        return Ok(None);
    }

    let left_tokens = gather(collector, tokens[0])?;
    let left = try_match_node!(collector, &left_tokens);
    let right = try_match_node!(collector, &tokens[2..]);

    Ok(Some(ASTNode::new(
        ASTNodeType::Is,
        span_from_tokens(&tokens),
        vec![left, right],
    )))
}

fn match_as(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    // 搜索 `as` 关键字
    if tokens.len() <= 2 {
        return Ok(None);
    }

    // 从右向左扫描 `as` 关键字所在的位置
    let mut offset: usize = tokens.len() - 1;
    let mut operator_pos: usize = 0;
    let mut found = false;

    while offset > 0 {
        let pos = offset;
        if is_identifier(&tokens[pos], "as") {
            operator_pos = pos;
            found = true;
            break;
        }
        offset -= 1;
    }

    if !found {
        return Ok(None);
    }

    // 解析左侧表达式（要转换的值）
    let left = try_match_node!(collector, &tokens[..operator_pos]);

    // 解析右侧表达式（目标类型，应该是一个变量如 int, string 等）
    let right = try_match_node!(collector, &tokens[operator_pos + 1..]);

    // 将 x as int 转换为 int(x) 的 AST 结构
    // 即：LambdaCall(right, Tuple(left))

    // right 应该是类型名变量，如 int、string 等
    // 将 left 作为参数包装成元组
    let args_tuple = ASTNode::new(
        ASTNodeType::Tuple,
        span_from_tokens(&tokens[..operator_pos]),
        vec![left],
    );

    Ok(Some(ASTNode::new(
        ASTNodeType::Apply,
        span_from_tokens(&tokens),
        vec![right, args_tuple],
    )))
}

fn match_variable(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.is_empty() {
        return Ok(None);
    }

    // 匹配括号内容（元组）
    if is_bracket(&tokens[0]) || is_square_bracket(&tokens[0]) {
        let inner_tokens = unwrap_brace(collector, &tokens[0])?;
        let gathered_inner = gather(collector, inner_tokens)?;
        let node = try_match_node!(collector, &gathered_inner);
        return Ok(Some(node));
    }

    // 匹配函数体 {...}
    if is_brace(&tokens[0]) {
        let body_tokens = unwrap_brace(collector, &tokens[0])?;
        let gathered_body = gather(collector, body_tokens)?;
        let body = try_match_node!(collector, &gathered_body);

        return Ok(Some(ASTNode::new(
            ASTNodeType::Frame,
            span_from_tokens(&tokens),
            vec![body],
        )));
    }

    // 匹配字符串常量
    if tokens[0].len() == 1 && tokens[0].first().unwrap() == TokenType::STRING {
        return Ok(Some(ASTNode::new(
            ASTNodeType::String(tokens[0].first().unwrap().token().clone()),
            span_from_tokens(&tokens),
            vec![],
        )));
    }

    // 匹配数字常量
    if tokens[0].len() == 1 && tokens[0].first().unwrap() == TokenType::NUMBER {
        return Ok(Some(ASTNode::new(
            ASTNodeType::Number(tokens[0].first().unwrap().token().clone()),
            span_from_tokens(&tokens),
            vec![],
        )));
    }

    // 匹配b64
    if tokens[0].len() == 1 && tokens[0].first().unwrap() == TokenType::BASE64 {
        return Ok(Some(ASTNode::new(
            ASTNodeType::Base64(tokens[0].first().unwrap().token().clone()),
            span_from_tokens(&tokens),
            vec![],
        )));
    }

    // 匹配布尔值（true）
    if is_identifier(&tokens[0], "true") {
        return Ok(Some(ASTNode::new(
            ASTNodeType::Boolean(true),
            span_from_tokens(&tokens),
            vec![],
        )));
    }

    // 匹配布尔值（false）
    if is_identifier(&tokens[0], "false") {
        return Ok(Some(ASTNode::new(
            ASTNodeType::Boolean(false),
            span_from_tokens(&tokens),
            vec![],
        )));
    }

    // 匹配空值（null）
    if is_identifier(&tokens[0], "null") {
        return Ok(Some(ASTNode::new(
            ASTNodeType::Null,
            span_from_tokens(&tokens),
            vec![],
        )));
    }

    // undefined
    if is_identifier(&tokens[0], "undefined") {
        return Ok(Some(ASTNode::new(
            ASTNodeType::Undefined,
            span_from_tokens(&tokens),
            vec![],
        )));
    }

    // 匹配普通变量名
    if tokens[0].len() == 1 && tokens[0][0] == TokenType::IDENTIFIER {
        return Ok(Some(ASTNode::new(
            ASTNodeType::Variable(tokens[0][0].token().clone()),
            span_from_tokens(&tokens),
            vec![],
        )));
    }
    Ok(None)
}
