//! AST 解析模块：将 Token 流转换为抽象语法树（AST）。
//!
//! 本模块实现了 Onion 语言的语法分析器，将词法分析产生的 Token 流转换为
//! 结构化的抽象语法树。支持丰富的语法结构，包括表达式、控制流、函数定义、
//! 类型系统、模式匹配等。
//!
//! # Onion 语言语法结构
//!
//! ## 基础语法
//! - **字面量**：`42`, `"hello"`, `true`, `false`, `null`, `undefined`
//! - **变量**：`variable_name`
//! - **元组**：`x, y, z` 或 `(x, y, z)`
//! - **对象**：`{...}` 块作用域
//!
//! ## 表达式
//! - **算术运算**：`+`, `-`, `*`, `/`, `%`, `**` (幂运算)
//! - **比较运算**：`==`, `!=`, `>`, `<`, `>=`, `<=`
//! - **逻辑运算**：`and`, `or`, `xor`, `not`
//! - **位运算**：`<<`, `>>` (位移)
//! - **一元运算**：`-x` (负号), `+x` (绝对值)
//!
//! ## 变量与赋值
//! - **变量定义**：`x := value` (let binding)
//! - **变量赋值**：`x = value` (assignment)
//! - **模式匹配**：`key: value` (pair), `key => value` (named pair)
//!
//! ## 函数与 Lambda
//! - **函数定义**：`params -> body` (静态函数)
//! - **动态函数**：`params -> dyn body` (动态函数)
//! - **捕获变量**：`params -> & captured_vars body`
//! - **函数调用**：`func args`
//!
//! ## 控制流
//! - **条件判断**：`if condition body` 或 `if condition body else else_body`
//! - **循环**：`while condition body`
//! - **跳转**：`break value`, `continue value`, `return value`, `raise error`
//!
//! ## 高级特性
//! - **范围**：`start..end`
//! - **成员访问**：`object.member`
//! - **包含检查**：`element in container`
//! - **同一性检查**：`x is y`
//! - **类型转换**：`value as type`
//! - **集合操作**：`collection | filter` (set), `collection |> map` (map)
//!
//! ## 修饰符
//! - **可变性**：`mut`, `const`
//! - **类型查询**：`typeof`, `keyof`, `valueof`, `lengthof`
//! - **断言**：`assert condition`
//! - **导入**：`import module`
//! - **并发**：`launch`, `spawn`, `async`, `sync`, `atomic`
//!
//! ## 编译时特性
//! - **编译时求值**：`@ expression`
//! - **AST 序列化**：`$ expression`
//! - **动态/静态模式**：`dynamic expression`, `static expression`
//! - **命名空间**：`Type::value`
//! - **假设元组**：`...value`
//! - **快速命名**：`expression ?`
//!
//! # 用法示例
//! ```ignore
//! let tokens = tokenize(source);
//! let gathered = ast_token_stream::from_stream(&tokens);
//! let ast = build_ast(&mut diagnostics, gathered)?;
//! ```

use base64::Engine;
use serde::{Deserialize, Serialize};

use crate::{
    diagnostics::{Diagnostic, SourceLocation, collector::DiagnosticCollector},
    parser::lexer::{Token, TokenType},
};
use std::{
    collections::HashSet,
    fmt::{self, Debug, Display, Formatter},
    vec,
};

/// AST 解析诊断信息类型。
///
/// 封装语法分析过程中可能出现的各种错误类型。
#[derive(Debug, Clone)]
pub enum ASTParseDiagnostic {
    /// 意外的 Token。
    UnexpectedToken(Option<SourceLocation>),
    /// 括号不匹配。
    UnmatchedParenthesis(Option<SourceLocation>),
    /// 无效的变量名。
    InvalidVariableName(Option<SourceLocation>),
    /// 不支持的语法结构。
    UnsupportedStructure(Option<SourceLocation>),
    /// 缺少必要的语法结构。
    MissingStructure(Option<SourceLocation>, String),
    /// 无效的语法。
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

/// Token 流类型别名。
pub type TokenStream = Vec<Token>;
/// 收集的 Token 片段类型别名。
pub type GatheredTokens<'t> = &'t [Token];

/// AST Token 流处理模块。
pub mod ast_token_stream {
    /// 从 Token 流创建收集的 Token 片段。
    pub fn from_stream<'t>(stream: &'t super::TokenStream) -> super::GatheredTokens<'t> {
        stream.as_slice()
    }
}

/// 获取下一个平衡的 Token 组。
///
/// 处理括号、方括号、花括号的匹配，确保返回的 Token 组语法完整。
fn get_next_tokens<'a, 'b>(
    collector: &'a mut DiagnosticCollector,
    tokens: GatheredTokens<'b>,
    offset: usize,
) -> Result<GatheredTokens<'b>, ()> {
    let mut stack = Vec::<&Token>::new();
    let mut next_tokens_end = 0usize;
    let mut index = offset;
    if index >= tokens.len() {
        return Ok(&[]);
    }
    loop {
        if ["{", "[", "("].contains(&tokens[index].token().as_str())
            && tokens[index] == TokenType::SYMBOL
        {
            stack.push(&tokens[index]);
            next_tokens_end += 1;
        } else if ["}", "]", ")"].contains(&tokens[index].token().as_str())
            && tokens[index] == TokenType::SYMBOL
        {
            if stack.is_empty() {
                break;
            }
            let last = stack.pop().unwrap();
            if (last == "{" && tokens[index] != "}")
                || (last == "[" && tokens[index] != "]")
                || (last == "(" && tokens[index] != ")")
            {
                return collector.fatal(ASTParseDiagnostic::UnmatchedParenthesis(
                    span_from_two_token(&last, &tokens[index]),
                ));
            }

            next_tokens_end += 1;
        } else {
            next_tokens_end += 1;
        }
        index += 1;
        if index >= tokens.len() || stack.is_empty() {
            break;
        }
    }
    if !stack.is_empty() {
        let last = stack.pop().unwrap();
        return collector.fatal(ASTParseDiagnostic::UnmatchedParenthesis(
            span_from_two_token(&last, &tokens[offset + next_tokens_end - 1]),
        ));
    }
    Ok(&tokens[offset..offset + next_tokens_end])
}

/// 将 Token 流分割为语法单元组。
///
/// 根据括号匹配将 Token 流分割为独立的语法单元，每个单元是一个完整的表达式。
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

/// AST 节点类型枚举。
///
/// 定义了 Onion 语言中所有可能的 AST 节点类型，对应不同的语法结构。
///
/// # 语法结构说明
///
/// ## 字面量类型
/// - `Null`：空值 `null`
/// - `Undefined`：未定义值 `undefined`
/// - `String(String)`：字符串字面量 `"text"`
/// - `Boolean(bool)`：布尔值 `true`/`false`
/// - `Number(String)`：数值字面量 `42`, `3.14`, `0xFF`
/// - `Base64(String)`：Base64 编码数据
///
/// ## 变量与标识符
/// - `Variable(String)`：变量引用 `variable_name`
/// - `Required(String)`：必需变量占位符 `@required "var"`
///
/// ## 变量操作
/// - `Let(String)`：变量绑定 `x := value`
/// - `Assign`：变量赋值 `x = value`
///
/// ## 函数与应用
/// - `LambdaDef(bool, HashSet<String>)`：Lambda 定义 `params -> body`
///   - 第一个参数：是否为动态函数 (`dyn`)
///   - 第二个参数：捕获的变量集合 (`& captured_vars`)
/// - `Apply`：函数应用 `func args`
///
/// ## 数据结构
/// - `Tuple`：元组 `x, y, z`
/// - `Pair`：键值对 `key: value`
/// - `AssumeTuple`：假设元组 `...value`
///
/// ## 控制流
/// - `If`：条件分支 `if condition body [else else_body]`
/// - `While`：循环 `while condition body`
/// - `Break`：跳出循环 `break value`
/// - `Continue`：继续循环 `continue value`
/// - `Return`：函数返回 `return value`
/// - `Raise`：抛出异常 `raise error`
///
/// ## 运算符
/// - `Operation(ASTNodeOperation)`：各种运算 `+`, `-`, `*`, `/`, `==`, 等
///
/// ## 高级特性
/// - `Range`：范围 `start..end`
/// - `In`：包含检查 `element in container`
/// - `Is`：同一性检查 `x is y`
/// - `GetAttr`：成员访问 `object.member`
/// - `Set`：集合过滤 `collection | filter`
/// - `Map`：集合映射 `collection |> map`
/// - `Namespace(String)`：命名空间 `Type::value`
///
/// ## 修饰符与元操作
/// - `Modifier(ASTNodeModifier)`：修饰符 `mut`, `const`, `typeof`, 等
/// - `Frame`：作用域块 `{...}`
/// - `Expressions`：表达式序列 `expr1; expr2; ...`
///
/// ## 编译时特性
/// - `Dynamic`：动态模式 `dynamic expression`
/// - `Static`：静态模式 `static expression`
/// - `Comptime`：编译时求值 `@ expression`
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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
    In,    // x in y
    Namespace(String), // Type::Value
    LazySet, // collection | filter
    Map,   // collection |> map
    Is,    // x is y
    Raise, // raise expression

    Dynamic, // 假设子表达式的所有变量都存在
    Static,  // 假设子表达式的所有变量都在当前上下文中定义

    Comptime, // 编译时计算的表达式
}

impl Display for ASTNodeType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ASTNodeType::Null => "Null",
                ASTNodeType::Undefined => "Undefined",
                ASTNodeType::String(_) => "String",
                ASTNodeType::Boolean(_) => "Boolean",
                ASTNodeType::Number(_) => "Number",
                ASTNodeType::Base64(_) => "Base64",
                ASTNodeType::Variable(_) => "Variable",
                ASTNodeType::Required(_) => "Required",
                ASTNodeType::Let(_) => "Let",
                ASTNodeType::Frame => "Frame",
                ASTNodeType::Assign => "Assign",
                ASTNodeType::LambdaDef(_, _) => "LambdaDef",
                ASTNodeType::Expressions => "Expressions",
                ASTNodeType::Apply => "Apply",
                ASTNodeType::Operation(_) => "Operation",
                ASTNodeType::Tuple => "Tuple",
                ASTNodeType::AssumeTuple => "AssumeTuple",
                ASTNodeType::Pair => "Pair",
                ASTNodeType::GetAttr => "GetAttr",
                ASTNodeType::Return => "Return",
                ASTNodeType::If => "If",
                ASTNodeType::While => "While",
                ASTNodeType::Modifier(_) => "Modifier",
                ASTNodeType::Break => "Break",
                ASTNodeType::Continue => "Continue",
                ASTNodeType::Range => "Range",
                ASTNodeType::In => "In",
                ASTNodeType::Namespace(_) => "Namespace",
                ASTNodeType::LazySet => "LazySet",
                ASTNodeType::Map => "Map",
                ASTNodeType::Is => "Is",
                ASTNodeType::Raise => "Raise",
                ASTNodeType::Dynamic => "Dynamic",
                ASTNodeType::Static => "Static",
                ASTNodeType::Comptime => "Comptime",
            }
        )
    }
}

/// AST 运算符枚举。
///
/// 定义了 Onion 语言中所有支持的运算符及其操作类型。
///
/// # 运算符类型
///
/// ## 算术运算符 (二元)
/// - `Add`：加法 `+`
/// - `Subtract`：减法 `-`
/// - `Multiply`：乘法 `*`
/// - `Divide`：除法 `/`
/// - `Modulus`：取模 `%`
/// - `Power`：幂运算 `**`
///
/// ## 位运算符 (二元)
/// - `And`：按位与 `and`
/// - `Or`：按位或 `or`
/// - `Xor`：按位异或 `xor`
/// - `LeftShift`：左移 `<<`
/// - `RightShift`：右移 `>>`
///
/// ## 比较运算符 (二元)
/// - `Equal`：等于 `==`
/// - `NotEqual`：不等于 `!=`
/// - `Less`：小于 `<`
/// - `LessEqual`：小于等于 `<=`
/// - `Greater`：大于 `>`
/// - `GreaterEqual`：大于等于 `>=`
///
/// ## 一元运算符
/// - `Not`：逻辑非 `not`
/// - `Minus`：数值取负 `-`
/// - `Abs`：绝对值 `abs`
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
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

/// AST 修饰符枚举。
///
/// 定义了 Onion 语言中所有可用的修饰符，用于修改表达式的语义。
///
/// # 修饰符类型
///
/// ## 变量修饰符
/// - `Mut`：可变修饰符 `mut variable`，声明变量可以被修改
/// - `Const`：常量修饰符 `const variable`，声明变量为常量
///
/// ## 类型操作修饰符
/// - `KeyOf`：键类型提取 `keyof object`，获取对象的键类型
/// - `ValueOf`：值类型提取 `valueof object`，获取对象的值类型
/// - `TypeOf`：类型提取 `typeof expression`，获取表达式的类型
/// - `LengthOf`：长度提取 `lengthof collection`，获取集合的长度
///
/// ## 函数调度修饰符
/// - `Launch`：并行启动 `launch expression`，在新线程中执行表达式
/// - `Spawn`：异步启动 `spawn expression`，在异步上下文中执行表达式
/// - `Async`：异步修饰符 `async function`，声明函数为异步调度入口
/// - `Sync`：同步修饰符 `sync function`，声明函数为同步调度入口
/// - `Atomic`：原子操作修饰符 `atomic expression`，移除表达式的调度修饰作为标准函数调用
///
/// ## 其他修饰符
/// - `Assert`：断言 `assert condition`，在运行时检查条件
/// - `Import`：导入模块 `import module`，引入外部字节码
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
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
    Async,    // Async
    Sync,     // Sync
    Atomic,   // Atomic
}

/// AST 节点结构体。
///
/// 表示 Onion 语言的抽象语法树节点，是构成整个语法树的基本单元。
///
/// # 字段说明
/// - `node_type`：节点类型，定义了该节点的语法角色和语义
/// - `source_location`：源码位置信息，用于错误报告和调试
/// - `children`：子节点列表，构成树形结构
///
/// # 设计原则
///
/// ## 递归结构
/// AST 节点通过 `children` 字段形成递归的树形结构，每个节点可以包含任意数量的子节点。
///
/// ## 位置信息
/// 每个节点都可以携带源码位置信息，便于在编译时提供准确的错误定位。
///
/// ## 类型安全
/// 通过 `ASTNodeType` 枚举确保节点类型的类型安全，避免无效的语法树结构。
#[derive(Debug, Clone, Serialize, Deserialize)]
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
    /// 创建新的 AST 节点。
    ///
    /// # 参数
    /// - `node_type`：节点类型
    /// - `source_location`：源码位置信息（可选）
    /// - `children`：子节点列表
    ///
    /// # 返回
    /// 新创建的 AST 节点实例
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

    /// 格式化打印 AST 节点树。
    ///
    /// 以缩进的形式递归打印整个 AST 结构，用于调试和可视化。
    ///
    /// # 参数
    /// - `indent`：当前缩进级别
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

/// 匹配器函数类型。
///
/// 定义了语法解析匹配器的函数签名。
///
/// # 参数
/// - `&mut DiagnosticCollector`：诊断信息收集器
/// - `&[GatheredTokens]`：待解析的 Token 组
///
/// # 返回
/// - `Ok(Some(ASTNode))`：成功匹配并生成 AST 节点
/// - `Ok(None)`：无法匹配当前模式
/// - `Err(())`：解析出现致命错误
type MatcherFn = fn(&mut DiagnosticCollector, &[GatheredTokens]) -> Result<Option<ASTNode>, ()>;

/// 节点匹配器结构体。
///
/// 包含多个匹配器函数，用于按优先级尝试解析不同的语法模式。
struct NodeMatcher {
    matchers: Vec<MatcherFn>,
}

/// 从 Token 组生成源码位置信息。
///
/// 计算一组 Token 片段覆盖的源码范围，用于错误报告。
///
/// # 参数
/// - `tokens`：Token 组数组
///
/// # 返回
/// 包含起始和结束位置的源码位置信息
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

/// 从两个 Token 生成源码位置信息。
///
/// 计算从起始 Token 到结束 Token 的源码范围。
///
/// # 参数
/// - `start`：起始 Token
/// - `end`：结束 Token
///
/// # 返回
/// 包含两个 Token 之间范围的源码位置信息
fn span_from_two_token(start: &Token, end: &Token) -> Option<SourceLocation> {
    if start.source_code().eq(end.source_code()) {
        return Some(SourceLocation {
            span: (start.origin_token_span().0, end.origin_token_span().1),
            source: start.source_code().clone(),
        });
    }
    None
}

/// 从单个 Token 生成源码位置信息。
///
/// 获取单个 Token 的源码位置信息。
///
/// # 参数
/// - `token`：目标 Token
///
/// # 返回
/// Token 的源码位置信息
fn span_from_single_token(token: &Token) -> Option<SourceLocation> {
    Some(SourceLocation {
        span: token.origin_token_span(),
        source: token.source_code().clone(),
    })
}

impl NodeMatcher {
    /// 创建新的节点匹配器。
    ///
    /// # 返回
    /// 空的节点匹配器实例
    fn new() -> NodeMatcher {
        NodeMatcher {
            matchers: Vec::new(),
        }
    }

    /// 添加匹配器函数。
    ///
    /// 将新的匹配器函数添加到匹配器列表中。匹配器按添加顺序进行尝试。
    ///
    /// # 参数
    /// - `matcher`：匹配器函数
    fn add_matcher(
        &mut self,
        matcher: fn(&mut DiagnosticCollector, &[GatheredTokens]) -> Result<Option<ASTNode>, ()>,
    ) {
        self.matchers.push(matcher);
    }

    /// 尝试匹配 Token 组生成 AST 节点。
    ///
    /// 按优先级顺序尝试所有匹配器，直到找到可以处理当前 Token 组的匹配器。
    ///
    /// # 参数
    /// - `collector`：诊断信息收集器
    /// - `tokens`：待解析的 Token 组
    ///
    /// # 返回
    /// - `Ok(ASTNode)`：成功生成的 AST 节点
    /// - `Err(())`：所有匹配器都无法处理或出现错误
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
                return collector.fatal(ASTParseDiagnostic::UnsupportedStructure(
                    span_from_tokens(tokens),
                ));
            }
        }
    }
}

/// 检查 Token 组是否为指定符号。
///
/// 验证 Token 组是否包含单个符号 Token 且匹配指定的符号字符串。
///
/// # 参数
/// - `token`：Token 组
/// - `symbol`：要匹配的符号字符串
///
/// # 返回
/// 如果匹配则返回 `true`，否则返回 `false`
fn is_symbol(token: &GatheredTokens, symbol: &str) -> bool {
    if token.len() != 1 {
        return false;
    }
    let token = &token[0];
    token == TokenType::SYMBOL && token == symbol
}

/// 检查 Token 组是否为指定标识符。
///
/// 验证 Token 组是否包含单个标识符 Token 且匹配指定的标识符字符串。
///
/// # 参数
/// - `token`：Token 组
/// - `identifier`：要匹配的标识符字符串
///
/// # 返回
/// 如果匹配则返回 `true`，否则返回 `false`
fn is_identifier(token: &GatheredTokens, identifier: &str) -> bool {
    if token.len() != 1 {
        return false;
    }
    let token = &token[0];
    token == TokenType::IDENTIFIER && token == identifier
}

/// 解包括号包围的 Token 组。
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

/// 检查 Token 组是否为括号包围的内容。
///
/// 验证 Token 组是否以 `(` 开始并以 `)` 结束。
///
/// # 参数
/// - `token`：Token 组
///
/// # 返回
/// 如果是括号包围则返回 `true`，否则返回 `false`
fn is_bracket(token: &GatheredTokens) -> bool {
    if token.len() < 2 {
        return false;
    }
    token[0] == TokenType::SYMBOL
        && token[0] == "("
        && token.last().unwrap() == TokenType::SYMBOL
        && token.last().unwrap() == ")"
}

/// 检查 Token 组是否为花括号包围的内容。
///
/// 验证 Token 组是否以 `{` 开始并以 `}` 结束。
///
/// # 参数
/// - `token`：Token 组
///
/// # 返回
/// 如果是花括号包围则返回 `true`，否则返回 `false`
fn is_brace(token: &GatheredTokens) -> bool {
    if token.len() < 2 {
        return false;
    }
    token[0] == TokenType::SYMBOL
        && token[0] == "{"
        && token.last().unwrap() == TokenType::SYMBOL
        && token.last().unwrap() == "}"
}

/// 检查 Token 组是否为方括号包围的内容。
///
/// 验证 Token 组是否以 `[` 开始并以 `]` 结束。
///
/// # 参数
/// - `token`：Token 组
///
/// # 返回
/// 如果是方括号包围则返回 `true`，否则返回 `false`
fn is_square_bracket(token: &GatheredTokens) -> bool {
    if token.len() < 2 {
        return false;
    }
    token[0] == TokenType::SYMBOL
        && token[0] == "["
        && token.last().unwrap() == TokenType::SYMBOL
        && token.last().unwrap() == "]"
}

/// 构建抽象语法树。
///
/// 这是 AST 构建的主入口函数，将词法分析后的 Token 流转换为 AST。
///
/// # 参数
/// - `collector`：诊断信息收集器，用于收集解析过程中的错误和警告
/// - `tokens`：词法分析后的 Token 流
///
/// # 返回
/// - `Ok(ASTNode)`：成功构建的 AST 根节点
/// - `Err(())`：解析失败，错误信息已添加到收集器中
///
/// # 解析流程
/// 1. 使用 `gather` 函数将 Token 流分组为语法单元
/// 2. 使用 `match_all` 函数尝试匹配各种语法模式
/// 3. 如果没有匹配到任何模式，返回空元组节点
/// 4. 否则返回匹配成功的 AST 节点
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

/// 尝试匹配所有可能的语法模式。
///
/// 创建并配置节点匹配器，按优先级尝试所有支持的语法模式。
///
/// # 参数
/// - `collector`：诊断信息收集器
/// - `tokens`：分组后的 Token 组数组
///
/// # 返回
/// - `Ok(Some(ASTNode))`：成功匹配的 AST 节点
/// - `Ok(None)`：没有找到匹配的模式
/// - `Err(())`：解析出现错误
///
/// # 支持的语法模式（按优先级）
/// - 表达式序列：`expr1; expr2; ...`
/// - 动态/静态模式：`dynamic expr`, `static expr`
/// - 控制流：`return`, `break`, `continue`, `raise`
/// - 元组：`a, b, c`
/// - 编译时求值：`@ expr`
/// - Base64 序列化
/// - 变量绑定：`x := expr`
/// - 变量赋值：`x = expr`
/// - 集合操作：`collection |> map`, `collection | filter`
/// - Lambda 函数：`args -> body`
/// - 命名空间：`Type::value`
/// - 键值对：`key: value`
/// - 循环：`while condition body`
/// - 条件：`if condition body [else else_body]`
/// - 范围：`start..end`
/// - 包含检查：`elem in collection`
/// - 同一性检查：`x is y`
/// - 成员访问：`obj.member`
/// - 修饰符：`mut expr`, `const expr`, 等
/// - 运算符：`+`, `-`, `*`, `/`, `==`, 等
/// - 函数应用：`func args`
/// - 变量、字面量等原子表达式
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
    node_matcher.add_matcher(match_serialize_to_base64);
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

/// 尝试匹配子节点，如果失败则返回 None 或错误。
/// 这是一个简化的宏，用于在匹配子节点时处理结果。
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

fn match_serialize_to_base64(
    collector: &mut DiagnosticCollector,
    tokens: &[GatheredTokens],
) -> Result<Option<ASTNode>, ()> {
    if tokens.is_empty() || !is_symbol(&tokens[0], "$") {
        return Ok(None);
    }
    let ast = try_match_node!(collector, &tokens[1..]);

    let serialized = match bincode::serde::encode_to_vec(&ast, bincode::config::standard()) {
        Ok(data) => data,
        Err(err) => {
            return collector.fatal(ASTParseDiagnostic::InvalidSyntax(
                span_from_tokens(&tokens),
                format!("Failed to serialize AST: {}", err),
            ));
        }
    };
    let base64_encoded = base64::engine::general_purpose::STANDARD.encode(serialized);

    // 将 `$expr` 解糖为 `ast.deserialize("base64...")` 的AST结构

    // 1. 创建 `ast.deserialize` 节点 (GetAttr)
    let get_attr_node = ASTNode::new(
        ASTNodeType::GetAttr,
        None, // 这是一个生成的节点，没有直接的源码位置
        vec![
            ASTNode::new(ASTNodeType::Variable("ast".to_string()), None, vec![]),
            ASTNode::new(ASTNodeType::String("deserialize".to_string()), None, vec![]),
        ],
    );

    // 2. 创建参数节点 (Base64编码的字符串)
    let arg_node = ASTNode::new(ASTNodeType::Base64(base64_encoded), None, vec![]);

    // 3. 创建函数调用节点 (Apply)
    let apply_node = ASTNode::new(
        ASTNodeType::Apply,
        span_from_tokens(&tokens), // 整个调用节点的源码位置对应于原始的 `$expr`
        vec![get_attr_node, arg_node],
    );

    Ok(Some(apply_node))
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
    if operator_pos == 0 {
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
        ASTNodeType::LazySet,
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
            "mut", "const", "keyof", "valueof", "assert", "import", "typeof", "lengthof", "launch",
            "spawn", "async", "sync", "atomic",
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

    Ok(Some(ASTNode::new(
        ASTNodeType::Apply,
        span_from_tokens(&tokens),
        vec![right, left],
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
