//! 词法分析器模块：将源代码字符流转换为 Token 流。
//!
//! 本模块实现了 Onion 语言的词法分析器（Lexer），负责将原始的源代码字符串
//! 分解为结构化的 Token 序列，为后续的语法分析阶段提供输入。
//!
//! # 主要功能
//!
//! ## Token 类型识别
//! - **数字**：整数、浮点数、十六进制、八进制、二进制
//! - **字符串**：单引号、双引号、反引号、多行字符串、原始字符串
//! - **标识符**：变量名、函数名、关键字（支持 Unicode 字符）
//! - **符号**：运算符、分隔符、括号等
//! - **注释**：单行注释 `//` 和多行注释 `/* */`
//! - **Base64**：特殊的 Base64 编码字符串 `$"..."`
//!
//! ## 字符串处理特性
//! - **转义序列**：`\n`, `\t`, `\r`, `\\`, `\"`, `\u1234`, `\x41` 等
//! - **原始字符串**：`R"delimiter(content)delimiter"` 格式
//! - **多行字符串**：`"""content"""` 或 `'''content'''`
//! - **Unicode 支持**：完整的 Unicode 字符和转义序列支持
//!
//! ## 数字格式支持
//! - **十进制**：`123`, `3.14`, `1.23e-4`
//! - **十六进制**：`0xFF`, `0x1A2B`
//! - **八进制**：`0o777`, `0O123`
//! - **二进制**：`0b1010`, `0B0011`
//! - **科学计数法**：`1.23e10`, `4.56E-7`
//!
//! # 使用示例
//! ```ignore
//! let source: Source = source_code.chars().collect();
//! let tokens = tokenizer::tokenize(&source);
//! let filtered_tokens = tokenizer::reject_comment(&tokens);
//! ```

use crate::parser::Source;
use std::fmt::Debug;

/// Token 类型枚举。
///
/// 定义了 Onion 语言中所有可能的 Token 类型。每种类型对应不同的词法单元。
///
/// # Token 类型说明
///
/// - `NUMBER`：数字字面量，包括整数、浮点数、十六进制、八进制、二进制
/// - `STRING`：字符串字面量，支持多种引号格式和转义序列
/// - `IDENTIFIER`：标识符，包括变量名、函数名、关键字等
/// - `SYMBOL`：符号，包括运算符、分隔符、括号等
/// - `COMMENT`：注释，包括单行注释和多行注释
/// - `BASE64`：特殊的 Base64 编码字符串，格式为 `$"..."`
#[derive(Debug, Clone)]
pub enum TokenType {
    NUMBER,
    STRING,
    IDENTIFIER,
    SYMBOL,
    COMMENT,
    BASE64,
}

impl PartialEq for TokenType {
    /// 比较两个 TokenType 是否相等。
    ///
    /// 使用内存判别式进行比较，只比较枚举变体，不比较关联数据。
    fn eq(&self, other: &Self) -> bool {
        // Deriving PartialEq would be simpler, but this works too.
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

/// Token 结构体。
///
/// 表示词法分析过程中识别出的一个词法单元（Token）。
/// 包含 Token 的内容、类型、源码位置等信息。
///
/// # 字段说明
/// - `token`：处理后的 Token 内容字符串
/// - `origin_token_span`：Token 在原始源码中的位置范围 (start, end)
/// - `source_code`：共享的源码引用，用于错误报告和调试
/// - `token_type`：Token 的类型分类
#[derive(Debug, Clone)]
pub struct Token {
    token: String,                     // The processed token value
    origin_token_span: (usize, usize), // The original token span in source
    source_code: Source,               // The shared source code
    token_type: TokenType,             // The type of the token
}

impl Token {
    /// 创建新的 Token 实例。
    ///
    /// # 参数
    /// - `token`：处理后的 Token 内容字符串
    /// - `origin_token_span`：Token 在源码中的位置范围
    /// - `source_code`：源码的共享引用
    /// - `token_type`：Token 类型
    ///
    /// # 返回
    /// 新创建的 Token 实例
    pub fn new(
        token: String,
        origin_token_span: (usize, usize),
        source_code: Source,
        token_type: TokenType,
    ) -> Token {
        Token {
            token,
            origin_token_span,
            source_code,
            token_type,
        }
    }

    /// 获取 Token 的内容字符串。
    ///
    /// # 返回
    /// Token 处理后的内容字符串引用
    pub fn token(&self) -> &String {
        &self.token
    }

    /// 获取 Token 在原始源码中的位置范围。
    ///
    /// # 返回
    /// 包含起始和结束位置的元组 (start, end)
    pub fn origin_token_span(&self) -> (usize, usize) {
        self.origin_token_span
    }

    /// 获取 Token 在原始源码中的原始内容。
    ///
    /// 从源码中提取 Token 的原始字符串，可能与处理后的内容不同
    /// （例如字符串可能包含转义序列）。
    ///
    /// # 返回
    /// Token 的原始源码字符串
    pub fn origin_token(&self) -> String {
        let (start, end) = self.origin_token_span;
        self.source_code[start..end].iter().collect()
    }

    /// 获取源码的共享引用。
    ///
    /// # 返回
    /// 源码的引用，可用于访问完整的源码内容
    pub fn source_code(&self) -> &Source {
        &self.source_code
    }

    /// 获取完整源码的字符串表示。
    ///
    /// 将整个源码转换为字符串，主要用于调试和错误报告。
    ///
    /// # 返回
    /// 完整源码的字符串
    pub fn source_code_str(&self) -> String {
        self.source_code.iter().collect()
    }

    /// 获取 Token 的类型。
    ///
    /// # 返回
    /// Token 的类型枚举值
    pub fn token_type(&self) -> TokenType {
        self.token_type.clone()
    }
}

impl PartialEq<&str> for Token {
    /// 比较 Token 内容与字符串切片。
    ///
    /// 允许直接将 Token 与字符串字面量进行比较。
    fn eq(&self, other: &&str) -> bool {
        self.token() == *other
    }
}

impl PartialEq<str> for Token {
    /// 比较 Token 内容与字符串。
    ///
    /// 允许直接将 Token 与字符串进行比较。
    fn eq(&self, other: &str) -> bool {
        self.token() == other
    }
}

impl PartialEq<TokenType> for Token {
    /// 比较 Token 类型与 TokenType。
    ///
    /// 允许直接检查 Token 是否为特定类型。
    fn eq(&self, other: &TokenType) -> bool {
        self.token_type == *other
    }
}

impl PartialEq<TokenType> for &Token {
    /// 比较 Token 引用的类型与 TokenType。
    ///
    /// 允许对 Token 引用直接进行类型检查。
    fn eq(&self, other: &TokenType) -> bool {
        self.token_type == *other
    }
}

/// 词法分析器模块。
///
/// 包含词法分析的核心功能，将源代码字符流转换为 Token 序列。
pub mod tokenizer {
    use std::cell::RefCell;

    // Use the Token and Source from the parent module.
    use super::{Source, Token};

    /// 检查字符串是否为运算符。
    ///
    /// 判断给定的字符串是否属于 Onion 语言支持的运算符集合。
    ///
    /// # 支持的运算符
    ///
    /// ## 算术运算符
    /// - `+`, `-`, `*`, `/`, `%`：基本算术运算
    /// - `**`：幂运算
    /// - `++`：自增运算
    ///
    /// ## 比较运算符
    /// - `==`, `!=`：相等/不等比较
    /// - `>`, `<`, `>=`, `<=`：大小比较
    /// - `?=`：可选相等
    ///
    /// ## 逻辑运算符
    /// - `&&`, `||`：逻辑与/或
    /// - `!`：逻辑非
    ///
    /// ## 位运算符
    /// - `&`, `|`, `^`, `~`：位与/或/异或/取反
    /// - `<<`, `>>`：位移
    ///
    /// ## 赋值运算符
    /// - `=`：赋值
    /// - `:=`：变量绑定
    ///
    /// ## 函数与流控制
    /// - `->`：函数定义箭头
    /// - `|>`：管道运算符
    /// - `<|`：反向管道
    /// - `::`：命名空间
    /// - `=>`：命名对
    ///
    /// ## 分隔符与括号
    /// - `(`, `)`, `[`, `]`, `{`, `}`：各种括号
    /// - `,`, `;`, `:`：分隔符
    /// - `.`, `..`, `...`：点运算符和范围
    ///
    /// ## 特殊符号
    /// - `@`, `$`：特殊前缀
    /// - `?`：可选标记
    /// - `#`：井号
    /// - `"""`, `'''`：多行字符串标记
    /// - `/*`, `*/`：多行注释标记
    ///
    /// # 参数
    /// - `symbol`：要检查的字符串
    ///
    /// # 返回
    /// 如果是支持的运算符则返回 `true`，否则返回 `false`
    pub fn is_operator(symbol: &str) -> bool {
        let operators = vec![
            "+", "-", "*", "**", "/", "\\", "%", "&", "!", "^", "~", "=", "==", ">", "<", "<=",
            ">=", "!=", "?=", "|", "?", ":>", "#", "&&", ",", ".", "\n", ":", "->", "<<", ">>",
            "/*", "*/", ";", " ", ":=", "|>", "<|", "::", "=>", "++", "||", ">>", "<<", "\"\"\"",
            "'''", "(", ")", "[", "]", "{", "}", "..", "...", "@", "$",
        ];
        operators.contains(&symbol)
    }

    /// 对源代码进行词法分析。
    ///
    /// 这是词法分析器的主入口函数，将源代码字符序列转换为 Token 序列。
    ///
    /// # 处理流程
    /// 1. **空白符跳过**：自动跳过空白字符（空格、制表符、换行符等）
    /// 2. **Token 类型识别**：按优先级顺序尝试识别不同类型的 Token
    /// 3. **错误处理**：遇到无法识别的字符时跳过，避免无限循环
    ///
    /// # Token 识别优先级
    /// 1. **注释**：`//` 单行注释，`/* */` 多行注释
    /// 2. **数字**：各种进制的数字字面量
    /// 3. **Base64 字符串**：`$"..."` 格式的特殊字符串
    /// 4. **普通字符串**：各种引号格式的字符串
    /// 5. **运算符**：符号和操作符
    /// 6. **标识符**：变量名、函数名、关键字等
    ///
    /// # 特殊处理
    /// - **Unicode 支持**：完整支持 Unicode 字符作为标识符
    /// - **中文支持**：支持中文字符作为标识符的一部分
    /// - **转义序列**：完整的转义序列处理
    /// - **多种字符串格式**：支持单引号、双引号、反引号、多行字符串、原始字符串
    ///
    /// # 参数
    /// - `source`：源代码字符序列的引用
    ///
    /// # 返回
    /// Token 向量，按源码中出现的顺序排列
    ///
    /// # 示例
    /// ```ignore
    /// let source: Source = "let x = 42 + \"hello\"".chars().collect();
    /// let tokens = tokenize(&source);
    /// // 生成: [IDENTIFIER("let"), IDENTIFIER("x"), SYMBOL("="),
    /// //       NUMBER("42"), SYMBOL("+"), STRING("hello")]
    /// ```
    pub fn tokenize(source: &Source) -> Vec<Token> {
        let chars: &Vec<char> = &source;

        let tokens = RefCell::new(Vec::<super::Token>::new());
        let curr_pos = RefCell::new(0usize);

        // 跳过空白字符。
        //
        // 从当前位置开始跳过所有连续的空白字符，包括空格、制表符、换行符等。
        let skip_space = || {
            let mut curr_pos = curr_pos.borrow_mut();
            while *curr_pos < chars.len() && chars[*curr_pos].is_whitespace() {
                *curr_pos += 1;
            }
        };

        // 测试指定位置是否匹配给定字符串。
        //
        // 检查从指定位置开始的字符序列是否与测试字符串完全匹配。
        //
        // # 参数
        // - `test_str`：要匹配的字符串
        // - `pos`：起始位置
        //
        // # 返回
        // 如果匹配则返回 `true`，否则返回 `false`
        let test_string = |test_str: &str, pos| -> bool {
            let test_chars: Vec<char> = test_str.chars().collect();
            if pos + test_chars.len() > chars.len() {
                return false;
            }

            for i in 0..test_chars.len() {
                if chars[pos + i] != test_chars[i] {
                    return false;
                }
            }
            true
        };

        // 测试指定位置是否为数字的开始，并返回数字的长度。
        //
        // 识别各种数字格式，包括：
        // - 十六进制：`0x1234`, `0X5678`
        // - 八进制：`0o777`, `0O123`
        // - 二进制：`0b1010`, `0B0011`
        // - 十进制：`123`, `3.14`, `1.23e-4`, `4.56E+7`
        //
        // # 参数
        // - `pos`：起始位置
        //
        // # 返回
        // 如果是数字则返回数字的字符长度，否则返回 0
        let test_number = |pos| -> usize {
            if pos >= chars.len() {
                return 0;
            }
            let substring: String = chars[pos..].iter().collect();

            if substring.len() >= 2 {
                let first_two_chars: String = substring.chars().take(2).collect();
                if first_two_chars.to_lowercase() == "0x" {
                    let hex_pattern = r"^0[xX][0-9a-fA-F]+";
                    if let Some(matched) = regex::Regex::new(hex_pattern).unwrap().find(&substring)
                    {
                        return matched.end();
                    }
                }
            }
            if substring.len() >= 2 {
                let first_two_chars: String = substring.chars().take(2).collect();
                if first_two_chars.to_lowercase() == "0o" {
                    let oct_pattern = r"^0[oO][0-7]+";
                    if let Some(matched) = regex::Regex::new(oct_pattern).unwrap().find(&substring)
                    {
                        return matched.end();
                    }
                }
            }
            if substring.len() >= 2 {
                let first_two_chars: String = substring.chars().take(2).collect();
                if first_two_chars.to_lowercase() == "0b" {
                    let bin_pattern = r"^0[bB][01]+";
                    if let Some(matched) = regex::Regex::new(bin_pattern).unwrap().find(&substring)
                    {
                        return matched.end();
                    }
                }
            }

            let number_pattern = r"^\d*\.?\d+([eE][-+]?\d+)?";
            if let Some(matched) = regex::Regex::new(number_pattern).unwrap().find(&substring) {
                return matched.end();
            }

            0
        };

        // 读取数字 Token。
        //
        // 尝试从当前位置读取一个完整的数字 Token，支持所有数字格式。
        //
        // # 返回
        // - `Some((token, span))`：成功读取数字时返回处理后的字符串和位置范围
        // - `None`：当前位置不是数字开始
        let read_number = || -> Option<(String, (usize, usize))> {
            let start_pos = *curr_pos.borrow();
            let mut pos = curr_pos.borrow_mut();
            let len = test_number(*pos);
            if len == 0 {
                return None;
            }
            let token: String = chars[*pos..*pos + len].iter().collect();
            *pos += len;
            Some((token, (start_pos, *pos)))
        };

        // 读取 Base64 字符串 Token。
        //
        // 处理 `$"..."` 格式的特殊字符串，这种格式用于 Base64 编码的数据。
        // 支持转义序列处理，包括 Unicode 转义。
        //
        // # 字符串格式
        // - 以 `$"` 开始，以 `"` 结束
        // - 支持转义序列：`\n`, `\t`, `\r`, `\\`, `\"`, `\u1234` 等
        // - 处理后的内容作为 Token 值
        //
        // # 返回
        // - `Some((token, span))`：成功读取时返回处理后的字符串和位置范围
        // - `None`：不是 Base64 字符串格式或格式错误
        let read_base64 = || -> Option<(String, (usize, usize))> {
            let start_pos = *curr_pos.borrow();
            let mut pos = curr_pos.borrow_mut();
            if !test_string("$\"", start_pos) {
                return None;
            }

            let mut current_token = String::new();
            *pos += 2; // Skip $"

            while *pos < chars.len() {
                if chars[*pos] == '\\' {
                    *pos += 1;
                    if *pos < chars.len() {
                        let escape_char = chars[*pos];
                        match escape_char {
                            'n' => current_token.push('\n'),
                            'r' => current_token.push('\r'),
                            't' => current_token.push('\t'),
                            'b' => current_token.push('\x08'),
                            'f' => current_token.push('\x0C'),
                            'v' => current_token.push('\x0B'),
                            'a' => current_token.push('\x07'),
                            '"' | '\\' => current_token.push(escape_char),
                            'u' => {
                                *pos += 1;
                                if *pos + 4 <= chars.len() {
                                    let unicode_str: String =
                                        chars[*pos..*pos + 4].iter().collect();
                                    if let Ok(unicode_char) = u32::from_str_radix(&unicode_str, 16)
                                    {
                                        current_token
                                            .push(std::char::from_u32(unicode_char).unwrap_or('?'));
                                        *pos += 3;
                                    }
                                }
                            }
                            _ => {
                                current_token.push('\\');
                                current_token.push(escape_char);
                            }
                        }
                        *pos += 1;
                    } else {
                        *pos = start_pos; // backtrack
                        return None;
                    }
                } else if chars[*pos] == '"' {
                    *pos += 1; // consume closing quote
                    return Some((current_token, (start_pos, *pos)));
                } else {
                    current_token.push(chars[*pos]);
                    *pos += 1;
                }
            }
            *pos = start_pos; // unclosed string, backtrack
            None
        };

        // 读取字符串 Token。
        //
        // 处理各种格式的字符串字面量，支持多种引号类型和转义序列。
        //
        // # 支持的字符串格式
        //
        // ## 原始字符串
        // - `R"delimiter(content)delimiter"`：不处理转义序列的原始字符串
        // - delimiter 可以是任意字符序列，用于避免内容中的括号冲突
        //
        // ## 多行字符串
        // - `"""content"""`：三重双引号多行字符串
        // - `'''content'''`：三重单引号多行字符串
        // - 支持跨行内容和转义序列
        //
        // ## 普通字符串
        // - `"content"`：双引号字符串
        // - `'content'`：单引号字符串
        // - `` `content` ``：反引号字符串
        //
        // # 转义序列支持
        // - `\n`, `\r`, `\t`：标准控制字符
        // - `\\`, `\"`, `\'`：字面字符
        // - `\0`, `\b`, `\f`, `\v`, `\a`：其他控制字符
        // - `\x41`：十六进制字符编码（2位）
        // - `\u1234`：Unicode 字符编码（4位）
        // - `\U12345678`：扩展 Unicode 字符编码（8位）
        //
        // # 返回
        // - `Some((token, span))`：成功读取时返回处理后的字符串内容和位置范围
        // - `None`：不是字符串格式或格式错误（如未闭合）
        let read_string = || -> Option<(String, (usize, usize))> {
            let mut current_token = String::new();
            let start_char_pos = *curr_pos.borrow();

            let process_escape = |curr_pos: &mut usize, current_token: &mut String| -> bool {
                if *curr_pos >= chars.len() {
                    return false;
                }
                let escape_char = chars[*curr_pos];
                match escape_char {
                    'n' => current_token.push('\n'),
                    'r' => current_token.push('\r'),
                    't' => current_token.push('\t'),
                    '\\' | '"' | '\'' | '`' => current_token.push(escape_char),
                    '0' => current_token.push('\0'),
                    'b' => current_token.push('\x08'),
                    'f' => current_token.push('\x0C'),
                    'v' => current_token.push('\x0B'),
                    'a' => current_token.push('\x07'),
                    'x' => {
                        *curr_pos += 1;
                        if *curr_pos + 1 < chars.len() {
                            let hex_str: String = chars[*curr_pos..*curr_pos + 2].iter().collect();
                            if let Ok(hex_val) = u8::from_str_radix(&hex_str, 16) {
                                current_token.push(hex_val as char);
                                *curr_pos += 1;
                            } else {
                                current_token.push_str("\\x");
                                *curr_pos -= 1;
                            }
                        } else {
                            current_token.push_str("\\x");
                            *curr_pos -= 1;
                        }
                    }
                    'u' => {
                        *curr_pos += 1;
                        if *curr_pos + 3 < chars.len() {
                            let unicode_str: String =
                                chars[*curr_pos..*curr_pos + 4].iter().collect();
                            if let Ok(val) = u32::from_str_radix(&unicode_str, 16) {
                                if let Some(c) = std::char::from_u32(val) {
                                    current_token.push(c);
                                    *curr_pos += 3;
                                } else {
                                    current_token.push_str("\\u");
                                    *curr_pos -= 1;
                                }
                            } else {
                                current_token.push_str("\\u");
                                *curr_pos -= 1;
                            }
                        } else {
                            current_token.push_str("\\u");
                            *curr_pos -= 1;
                        }
                    }
                    'U' => {
                        *curr_pos += 1;
                        if *curr_pos + 7 < chars.len() {
                            let unicode_str: String =
                                chars[*curr_pos..*curr_pos + 8].iter().collect();
                            if let Ok(val) = u32::from_str_radix(&unicode_str, 16) {
                                if let Some(c) = std::char::from_u32(val) {
                                    current_token.push(c);
                                    *curr_pos += 7;
                                } else {
                                    current_token.push_str("\\U");
                                    *curr_pos -= 1;
                                }
                            } else {
                                current_token.push_str("\\U");
                                *curr_pos -= 1;
                            }
                        } else {
                            current_token.push_str("\\U");
                            *curr_pos -= 1;
                        }
                    }
                    _ => {
                        current_token.push('\\');
                        current_token.push(escape_char);
                    }
                }
                *curr_pos += 1;
                true
            };
            if test_string("R\"", start_char_pos) {
                let mut pos = curr_pos.borrow_mut();
                *pos += 2;
                let mut divider = String::new();
                while *pos < chars.len() && chars[*pos] != '(' {
                    divider.push(chars[*pos]);
                    *pos += 1;
                }
                if *pos < chars.len() && chars[*pos] == '(' {
                    *pos += 1;
                    let end_sequence = format!("){}\"", divider);
                    while *pos < chars.len() && !test_string(&end_sequence, *pos) {
                        current_token.push(chars[*pos]);
                        *pos += 1;
                    }
                    if *pos < chars.len() {
                        *pos += end_sequence.len();
                        return Some((current_token, (start_char_pos, *pos)));
                    }
                }
                *pos = start_char_pos;
                return None;
            }
            if test_string("\"\"\"", start_char_pos) || test_string("'''", start_char_pos) {
                let quote_seq: String = chars[start_char_pos..start_char_pos + 3].iter().collect();
                let mut pos = curr_pos.borrow_mut();
                *pos += 3;
                while *pos < chars.len() {
                    if test_string(&quote_seq, *pos) {
                        *pos += 3;
                        return Some((current_token, (start_char_pos, *pos)));
                    }
                    if chars[*pos] == '\\' {
                        *pos += 1;
                        if !process_escape(&mut pos, &mut current_token) {
                            *pos = start_char_pos;
                            return None;
                        }
                    } else {
                        current_token.push(chars[*pos]);
                        *pos += 1;
                    }
                }
                *pos = start_char_pos;
                return None;
            }
            let quote_pairs: std::collections::HashMap<char, char> =
                [('"', '"'), ('\'', '\''), ('`', '`')]
                    .iter()
                    .cloned()
                    .collect();
            if start_char_pos < chars.len() {
                let start_char = chars[start_char_pos];
                if quote_pairs.contains_key(&start_char) {
                    let mut pos = curr_pos.borrow_mut();
                    *pos += 1;
                    while *pos < chars.len() {
                        if chars[*pos] == '\\' {
                            *pos += 1;
                            if !process_escape(&mut pos, &mut current_token) {
                                *pos = start_char_pos;
                                return None;
                            }
                        } else if chars[*pos] == start_char {
                            *pos += 1;
                            return Some((current_token, (start_char_pos, *pos)));
                        } else {
                            current_token.push(chars[*pos]);
                            *pos += 1;
                        }
                    }
                    *pos = start_char_pos;
                }
            }
            None
        };

        // 读取注释 Token。
        //
        // 识别并处理单行注释和多行注释。
        //
        // # 支持的注释格式
        //
        // ## 单行注释
        // - `// comment content`：从 `//` 开始到行末的所有内容
        // - 不包括行结束符在注释内容中
        //
        // ## 多行注释
        // - `/* comment content */`：块注释，可以跨越多行
        // - 必须有闭合的 `*/`，否则读取失败
        // - 注释内容不包括开始和结束标记
        //
        // # 返回
        // - `Some((content, span))`：成功读取时返回注释内容和位置范围
        // - `None`：不是注释格式或多行注释未闭合
        let read_comment = || -> Option<(String, (usize, usize))> {
            let start_pos = *curr_pos.borrow();
            let mut pos = curr_pos.borrow_mut();
            if test_string("//", *pos) {
                *pos += 2;
                let mut current_token = String::new();
                while *pos < chars.len() && !['\n', '\r'].contains(&chars[*pos]) {
                    current_token.push(chars[*pos]);
                    *pos += 1;
                }
                return Some((current_token, (start_pos, *pos)));
            }
            if test_string("/*", *pos) {
                *pos += 2;
                let mut current_token = String::new();
                while *pos < chars.len() && !test_string("*/", *pos) {
                    current_token.push(chars[*pos]);
                    *pos += 1;
                }
                if *pos < chars.len() {
                    *pos += 2;
                    return Some((current_token, (start_pos, *pos)));
                }
            }
            *pos = start_pos;
            None
        };

        // 读取运算符 Token。
        //
        // 识别各种长度的运算符，采用最长匹配策略。
        //
        // # 匹配策略
        // 1. 首先尝试匹配 3 字符运算符（如 `"""`, `'''`）
        // 2. 然后尝试匹配 2 字符运算符（如 `==`, `!=`, `->`, `::` 等）
        // 3. 最后匹配单字符运算符（如 `+`, `-`, `*`, `/` 等）
        //
        // # 运算符优先级
        // 采用贪婪匹配，优先匹配更长的运算符，确保 `==` 不会被识别为两个 `=`。
        //
        // # 返回
        // - `Some((operator, span))`：成功读取时返回运算符字符串和位置范围
        // - `None`：当前位置不是运算符
        let read_operator = || -> Option<(String, (usize, usize))> {
            let start_pos = *curr_pos.borrow();
            let mut pos = curr_pos.borrow_mut();
            if *pos + 2 < chars.len() {
                let three_chars: String = chars[*pos..*pos + 3].iter().collect();
                if is_operator(&three_chars) {
                    *pos += 3;
                    return Some((three_chars, (start_pos, *pos)));
                }
            }
            if *pos + 1 < chars.len() {
                let two_chars: String = chars[*pos..*pos + 2].iter().collect();
                if is_operator(&two_chars) {
                    *pos += 2;
                    return Some((two_chars, (start_pos, *pos)));
                }
            }
            if *pos < chars.len() {
                let one_char = chars[*pos].to_string();
                if is_operator(&one_char) {
                    *pos += 1;
                    return Some((one_char, (start_pos, *pos)));
                }
            }
            None
        };

        // 读取标识符 Token。
        //
        // 识别变量名、函数名、关键字等标识符。
        //
        // # 标识符规则
        //
        // ## 首字符要求
        // - 字母字符（a-z, A-Z）
        // - 下划线 `_`
        // - Unicode 字符（支持中文、日文、韩文等）
        //   - 中日韩统一表意文字：U+4E00-U+9FFF
        //   - 中日韩扩展A：U+3400-U+4DBF
        //   - 中日韩兼容表意文字：U+F900-U+FAFF
        //
        // ## 后续字符要求
        // - 首字符的所有要求
        // - 数字字符（0-9）
        //
        // # Unicode 支持
        // 完整支持 Unicode 标识符，允许使用各种语言的字符作为变量名，
        // 提高代码的国际化支持。
        //
        // # 返回
        // - `Some((identifier, span))`：成功读取时返回标识符字符串和位置范围
        // - `None`：当前位置不是标识符的开始
        let read_token = || -> Option<(String, (usize, usize))> {
            let start_pos = *curr_pos.borrow();
            let mut pos = curr_pos.borrow_mut();
            if *pos < chars.len() {
                let first_char = chars[*pos];
                if first_char.is_alphabetic()
                    || first_char == '_'
                    || (first_char as u32 >= 0x4E00 && first_char as u32 <= 0x9FFF)
                    || (first_char as u32 >= 0x3400 && first_char as u32 <= 0x4DBF)
                    || (first_char as u32 >= 0xF900 && first_char as u32 <= 0xFAFF)
                {
                    *pos += 1;
                    while *pos < chars.len() {
                        let c = chars[*pos];
                        if c.is_alphanumeric()
                            || c == '_'
                            || (c as u32 >= 0x4E00 && c as u32 <= 0x9FFF)
                            || (c as u32 >= 0x3400 && c as u32 <= 0x4DBF)
                            || (c as u32 >= 0xF900 && c as u32 <= 0xFAFF)
                        {
                            *pos += 1;
                        } else {
                            break;
                        }
                    }
                    let token: String = chars[start_pos..*pos].iter().collect();
                    return Some((token, (start_pos, *pos)));
                }
            }
            None
        };

        // 主词法分析循环
        //
        // 按优先级顺序尝试识别不同类型的 Token：
        // 1. 注释 - 必须优先处理，避免与运算符冲突
        // 2. 数字 - 各种进制和格式的数字字面量
        // 3. Base64 字符串 - 特殊格式的字符串
        // 4. 普通字符串 - 各种引号格式的字符串
        // 5. 运算符 - 符号和操作符
        // 6. 标识符 - 变量名、函数名、关键字
        //
        // 如果所有解析器都失败，跳过当前字符以避免无限循环
        loop {
            skip_space();
            if *curr_pos.borrow() >= chars.len() {
                break;
            }
            if let Some((token, origin_span)) = read_comment() {
                tokens.borrow_mut().push(super::Token::new(
                    token,
                    origin_span,
                    source.clone(),
                    super::TokenType::COMMENT,
                ));
                continue;
            }

            if let Some((token, origin_span)) = read_number() {
                tokens.borrow_mut().push(super::Token::new(
                    token,
                    origin_span,
                    source.clone(),
                    super::TokenType::NUMBER,
                ));
                continue;
            }

            if let Some((token, origin_span)) = read_base64() {
                tokens.borrow_mut().push(super::Token::new(
                    token,
                    origin_span,
                    source.clone(),
                    super::TokenType::BASE64,
                ));
                continue;
            }

            if let Some((token, origin_span)) = read_string() {
                tokens.borrow_mut().push(super::Token::new(
                    token,
                    origin_span,
                    source.clone(),
                    super::TokenType::STRING,
                ));
                continue;
            }

            if let Some((token, origin_span)) = read_operator() {
                tokens.borrow_mut().push(super::Token::new(
                    token,
                    origin_span,
                    source.clone(),
                    super::TokenType::SYMBOL,
                ));
                continue;
            }

            if let Some((token, origin_span)) = read_token() {
                tokens.borrow_mut().push(super::Token::new(
                    token,
                    origin_span,
                    source.clone(),
                    super::TokenType::IDENTIFIER,
                ));
                continue;
            } else {
                // 如果所有解析器都失败，向前移动一个字符以避免无限循环
                // 这处理了无法识别的字符
                // If all parsers fail, advance by one character to avoid an infinite loop.
                // This handles unrecognized characters.
                let mut curr_pos = curr_pos.borrow_mut();
                if *curr_pos < chars.len() {
                    *curr_pos += 1;
                }
            }
        }

        tokens.into_inner()
    }

    /// 从 Token 列表中过滤掉注释。
    ///
    /// 移除所有类型为 `COMMENT` 的 Token，返回不包含注释的 Token 列表。
    /// 这在语法分析阶段通常是必要的，因为注释不参与语法结构。
    ///
    /// # 使用场景
    /// - 语法分析前的预处理
    /// - 生成不包含注释的代码
    /// - 代码格式化工具
    ///
    /// # 参数
    /// - `tokens`：包含所有 Token 的向量引用
    ///
    /// # 返回
    /// 不包含注释 Token 的新向量
    ///
    /// # 示例
    /// ```ignore
    /// let all_tokens = tokenize(&source);
    /// let code_tokens = reject_comment(&all_tokens);
    /// // code_tokens 不包含任何注释 Token
    /// ```
    pub fn reject_comment(tokens: &Vec<super::Token>) -> Vec<super::Token> {
        tokens
            .iter()
            .filter(|token| token.token_type() != super::TokenType::COMMENT)
            .cloned()
            .collect()
    }
}
