#[derive(Debug, Clone)]
pub enum TokenType {
    NUMBER,
    STRING,
    IDENTIFIER,
    SYMBOL,
    COMMENT,
    BASE64,
}
impl TokenType {
    pub fn _to_string(&self) -> String {
        match self {
            TokenType::NUMBER => "NUMBER".to_string(),
            TokenType::STRING => "STRING".to_string(),
            TokenType::IDENTIFIER => "IDENTIFIER".to_string(),
            TokenType::SYMBOL => "SYMBOL".to_string(),
            TokenType::COMMENT => "COMMENT".to_string(),
            TokenType::BASE64 => "BASE64".to_string(),
        }
    }
}


impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TokenType::NUMBER, TokenType::NUMBER) => true,
            (TokenType::STRING, TokenType::STRING) => true,
            (TokenType::IDENTIFIER, TokenType::IDENTIFIER) => true,
            (TokenType::SYMBOL, TokenType::SYMBOL) => true,
            (TokenType::COMMENT, TokenType::COMMENT) => true,
            (TokenType::BASE64, TokenType::BASE64) => true,
            _ => false,
        }
    }
    
}


#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub token: &'a str,         // The token string
    pub token_type: TokenType, // The type of the token
    pub origin_token: String,  // The original token string
    pub position: usize,       // The position of the token in the input string
}

impl<'a> Token<'a> {
    pub fn new(
        token: String,
        token_type: TokenType,
        origin_token: String,
        position: usize,
    ) -> Token<'a> {
        let token_str = Box::leak(token.into_boxed_str());
        Token {
            token: token_str,
            token_type,
            origin_token,
            position,
        }
    }

    pub fn _to_string(&self) -> String {
        format!(
            "{} <{}, Token: {}, Type: {}>",
            self.origin_token,
            self.position,
            self.token,
            self.token_type._to_string()
        )
    }
}


pub mod lexer {
    use std::cell::RefCell;

    pub fn is_operator(symbol: &str) -> bool {
        let operators = vec![
            "+", "-", "*", "**", "/", "\\", "%", "&", "!", "^", "~", "=", "==", ">", "<", "<=", ">=",
            "!=", "?=", "|", "?", ":>", "#", "&&", ",", ".", "\n", ":", "->", "<<", ">>", "/*",
            "*/", ";", " ", ":=", "|>", "<|", "::", "=>", "++", "||", ">>", "<<", "\"\"\"", "'''",
            "(", ")", "[", "]", "{", "}", "..", "...", "@", "$"
        ];
        operators.contains(&symbol)
    }

    // Tokenize the input code
    pub fn tokenize(code: &str) -> Vec<super::Token> {
        let chars: Vec<char> = code.chars().collect(); // 预处理，将字符串转换为字符数组
        let tokens = RefCell::new(Vec::<super::Token>::new());
        let curr_pos = RefCell::new(0usize);
        
        // 源代码字符串
        let code_str = RefCell::new(code.to_string());

        // 辅助函数：获取字符的字节索引
        let get_byte_index = |char_index: usize| -> usize {
            if char_index == 0 {
                return 0;
            }
            
            let code_ref = code_str.borrow();
            let s = &code_ref[..];
            let mut char_count = 0;
            let mut byte_index = 0;
            
            for (idx, _) in s.char_indices() {
                if char_count == char_index {
                    byte_index = idx;
                    break;
                }
                char_count += 1;
            }
            
            byte_index
        };

        // Skip whitespace
        let skip_space = || {
            let mut curr_pos = curr_pos.borrow_mut();
            while *curr_pos < chars.len() && chars[*curr_pos].is_whitespace() {
                *curr_pos += 1;
            }
        };

        // 测试字符串匹配的闭包
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

        // 测试数字模式的闭包
        let test_number = |pos| -> usize {
            if pos >= chars.len() {
                return 0;
            }
            
            let substring: String = chars[pos..].iter().collect();
            let number_pattern = r"^\d*\.?\d+([eE][-+]?\d+)?";
            let re = regex::Regex::new(number_pattern).unwrap();
            
            if let Some(matched) = re.find(&substring) {
                let end_pos = matched.end();
                // 字节索引转换为字符索引
                let char_count = substring[..end_pos].chars().count();
                char_count
            } else {
                0
            }
        };

        // 读取数字的闭包，同时可以更新 curr_pos
        let read_number = || -> Option<(String, String)> {
            let mut pos = curr_pos.borrow_mut();
            let len = test_number(*pos);
            if len == 0 {
                return None;
            }
            let token: String = chars[*pos..*pos + len].iter().collect();
            *pos += len;
            Some((token.clone(), token))// token, original token
        };

        let read_base64 = || -> Option<(String, String)> {
            let mut curr_pos = curr_pos.borrow_mut();
            let mut current_token = String::new();
            
            if test_string("$\"", *curr_pos) {
                *curr_pos += 2;
                while *curr_pos < chars.len() {
                    if chars[*curr_pos] == '\\' {
                        *curr_pos += 1;
                        if *curr_pos < chars.len() {
                            let escape_char = chars[*curr_pos];
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
                                    *curr_pos += 1;
                                    if *curr_pos + 4 <= chars.len() {
                                        let unicode_str: String = chars[*curr_pos..*curr_pos + 4].iter().collect();
                                        if let Ok(unicode_char) = u32::from_str_radix(&unicode_str, 16) {
                                            current_token.push(std::char::from_u32(unicode_char).unwrap());
                                            *curr_pos += 3;
                                        }
                                    }
                                }
                                _ => {
                                    current_token.push('\\');
                                    current_token.push(escape_char);
                                }
                            }
                            *curr_pos += 1;
                        } else {
                            return None;
                        }
                    } else if chars[*curr_pos] == '"' {
                        *curr_pos += 1;
                        return Some((
                            current_token.clone(),
                            String::from("$\"") + &current_token + "\"",
                        ));
                    } else {
                        current_token.push(chars[*curr_pos]);
                        *curr_pos += 1;
                    }
                }
            }
            None
        };

        let read_string = || -> Option<(String, String)> {
            let mut curr_pos = curr_pos.borrow_mut();
            let mut current_token = String::new();
            let mut original_token = String::new();

            // 处理 R"..." 原始字符串
            if test_string("R\"", *curr_pos) {
                *curr_pos += 2;
                let mut divider = String::new();

                // 读取分隔符
                while *curr_pos < chars.len() && chars[*curr_pos] != '(' {
                    divider.push(chars[*curr_pos]);
                    *curr_pos += 1;
                }

                if *curr_pos < chars.len() {
                    *curr_pos += 1; // 跳过 '('
                    let end_divider = format!(")){}", divider);

                    while *curr_pos < chars.len() && !test_string(&(end_divider.clone() + "\""), *curr_pos) {
                        current_token.push(chars[*curr_pos]);
                        *curr_pos += 1;
                    }

                    if *curr_pos < chars.len() {
                        *curr_pos += end_divider.len() + 1; // +1 for the closing quote
                        original_token = format!("R\"{}({}){}", divider, current_token, end_divider);
                        return Some((current_token, original_token));
                    }
                }
                return None;
            }

            // 处理 """...""" 三引号字符串
            if test_string("\"\"\"", *curr_pos) {
                *curr_pos += 3;
                original_token.push_str("\"\"\"");

                while *curr_pos < chars.len() {
                    if test_string("\"\"\"", *curr_pos) {
                        *curr_pos += 3;
                        original_token.push_str("\"\"\"");
                        return Some((current_token, original_token));
                    }

                    if chars[*curr_pos] == '\\' {
                        original_token.push('\\');
                        *curr_pos += 1;
                        if *curr_pos < chars.len() {
                            let escape_char = chars[*curr_pos];
                            original_token.push(escape_char);

                            match escape_char {
                                'n' => current_token.push('\n'),
                                't' => current_token.push('\t'),
                                '"' | '\\' => current_token.push(escape_char),
                                'u' => {
                                    *curr_pos += 1;
                                    if *curr_pos + 4 <= chars.len() {
                                        let hex_str: String = chars[*curr_pos..*curr_pos + 4].iter().collect();
                                        if let Ok(unicode_value) = u32::from_str_radix(&hex_str, 16) {
                                            if let Some(unicode_char) = std::char::from_u32(unicode_value) {
                                                current_token.push(unicode_char);
                                                original_token.push_str(&hex_str);
                                                *curr_pos += 3;
                                            } else {
                                                return None;
                                            }
                                        } else {
                                            return None;
                                        }
                                    } else {
                                        return None;
                                    }
                                }
                                _ => {
                                    current_token.push('\\');
                                    current_token.push(escape_char);
                                }
                            }
                            *curr_pos += 1;
                        } else {
                            return None;
                        }
                    } else {
                        let c = chars[*curr_pos];
                        current_token.push(c);
                        original_token.push(c);
                        *curr_pos += 1;
                    }
                }
                return None;
            }

            // 处理普通引号字符串
            let quote_pairs: std::collections::HashMap<char, char> =
                [('"', '"'), ('\'', '\''), ('"', '"')]
                    .iter()
                    .cloned()
                    .collect();

            if *curr_pos < chars.len() {
                let start_char = chars[*curr_pos];
                if let Some(&end_char) = quote_pairs.get(&start_char) {
                    *curr_pos += 1;
                    original_token.push(start_char);

                    while *curr_pos < chars.len() {
                        let current_char = chars[*curr_pos];

                        if current_char == '\\' {
                            original_token.push('\\');
                            *curr_pos += 1;
                            if *curr_pos < chars.len() {
                                let escape_char = chars[*curr_pos];
                                original_token.push(escape_char);

                                match escape_char {
                                    'n' => current_token.push('\n'),
                                    't' => current_token.push('\t'),
                                    '"' | '\'' | '\\' => current_token.push(escape_char),
                                    'u' => {
                                        *curr_pos += 1;
                                        if *curr_pos + 4 <= chars.len() {
                                            let hex_str: String = chars[*curr_pos..*curr_pos + 4].iter().collect();
                                            if let Ok(unicode_value) = u32::from_str_radix(&hex_str, 16) {
                                                if let Some(unicode_char) = std::char::from_u32(unicode_value) {
                                                    current_token.push(unicode_char);
                                                    original_token.push_str(&hex_str);
                                                    *curr_pos += 3;
                                                } else {
                                                    return None;
                                                }
                                            } else {
                                                return None;
                                            }
                                        } else {
                                            return None;
                                        }
                                    }
                                    _ => {
                                        current_token.push('\\');
                                        current_token.push(escape_char);
                                    }
                                }
                                *curr_pos += 1;
                            } else {
                                return None;
                            }
                        } else if current_char == end_char {
                            *curr_pos += 1;
                            original_token.push(end_char);
                            return Some((current_token, original_token));
                        } else {
                            current_token.push(current_char);
                            original_token.push(current_char);
                            *curr_pos += 1;
                        }
                    }
                }
            }

            None
        };

        let read_comment = || -> Option<(String, String)> {
            let mut curr_pos = curr_pos.borrow_mut();
            if test_string("//", *curr_pos) {
                *curr_pos += 2;
                let mut current_token = String::new();
                while *curr_pos < chars.len() && !['\n', '\r'].contains(&chars[*curr_pos]) {
                    current_token.push(chars[*curr_pos]);
                    *curr_pos += 1;
                }
                return Some((current_token.clone(), format!("//{}", current_token)));
            }
            if test_string("/*", *curr_pos) {
                *curr_pos += 2;
                let mut current_token = String::new();
                while *curr_pos < chars.len() && !test_string("*/", *curr_pos) {
                    current_token.push(chars[*curr_pos]);
                    *curr_pos += 1;
                }
                if *curr_pos < chars.len() {
                    *curr_pos += 2;
                    return Some((current_token.clone(), format!("/*{}*/", current_token)));
                }
            }
            None
        };

        let read_operator = || -> Option<(String, String)> {
            let mut curr_pos = curr_pos.borrow_mut();
            if *curr_pos + 2 < chars.len() {
                let three_chars: String = chars[*curr_pos..*curr_pos + 3].iter().collect();
                if is_operator(&three_chars) {
                    *curr_pos += 3;
                    return Some((three_chars.clone(), three_chars));
                }
            }
            if *curr_pos + 1 < chars.len() {
                let two_chars: String = chars[*curr_pos..*curr_pos + 2].iter().collect();
                if is_operator(&two_chars) {
                    *curr_pos += 2;
                    return Some((two_chars.clone(), two_chars));
                }
            }
            if *curr_pos < chars.len() {
                let one_char = chars[*curr_pos].to_string();
                if is_operator(&one_char) {
                    *curr_pos += 1;
                    return Some((one_char.clone(), one_char));
                }
            }
            None
        };

        let read_token = || -> Option<(String, String)> {
            let mut curr_pos = curr_pos.borrow_mut();
            let mut curr_token = String::new();
            
            // Unicode友好的标识符读取
            // 标识符允许字母、数字、下划线，但首字符不能是数字
            if *curr_pos < chars.len() {
                let first_char = chars[*curr_pos];
                if first_char.is_alphabetic() || first_char == '_' {
                    curr_token.push(first_char);
                    *curr_pos += 1;
                    
                    // 读取剩余字符
                    while *curr_pos < chars.len() {
                        let c = chars[*curr_pos];
                        // 支持中文字符作为标识符的一部分
                        if c.is_alphanumeric() || c == '_' || 
                           // 添加对CJK字符的支持
                           (c as u32 >= 0x4E00 && c as u32 <= 0x9FFF) ||
                           (c as u32 >= 0x3400 && c as u32 <= 0x4DBF) ||
                           (c as u32 >= 0xF900 && c as u32 <= 0xFAFF) {
                            curr_token.push(c);
                            *curr_pos += 1;
                        } else {
                            break;
                        }
                    }
                    
                    return Some((curr_token.clone(), curr_token));
                }
            }
            
            None
        };

        loop {
            skip_space();
            let curr_pos_value = *curr_pos.borrow();
            if curr_pos_value >= chars.len() {
                break;
            }

            // 获取当前字符位置对应的字节位置
            let byte_pos = get_byte_index(curr_pos_value);

            if let Some((token, origin_token)) = read_number() {
                let mut tokens = tokens.borrow_mut();
                tokens.push(super::Token::new(
                    token,
                    super::TokenType::NUMBER,
                    origin_token,
                    byte_pos,
                ));
                continue;
            }

            if let Some((token, origin_token)) = read_base64() {
                let mut tokens = tokens.borrow_mut();
                tokens.push(super::Token::new(
                    token,
                    super::TokenType::BASE64,
                    origin_token,
                    byte_pos,
                ));
                continue;
            }

            if let Some((token, origin_token)) = read_string() {
                let mut tokens = tokens.borrow_mut();
                tokens.push(super::Token::new(
                    token,
                    super::TokenType::STRING,
                    origin_token,
                    byte_pos,
                ));
                continue;
            }

            if let Some((token, origin_token)) = read_comment() {
                let mut tokens = tokens.borrow_mut();
                tokens.push(super::Token::new(
                    token,
                    super::TokenType::COMMENT,
                    origin_token,
                    byte_pos,
                ));
                continue;
            }

            if let Some((token, origin_token)) = read_operator() {
                let mut tokens = tokens.borrow_mut();
                tokens.push(super::Token::new(
                    token,
                    super::TokenType::SYMBOL,
                    origin_token,
                    byte_pos,
                ));
                continue;
            }

            if let Some((token, origin_token)) = read_token() {
                let mut tokens = tokens.borrow_mut();
                tokens.push(super::Token::new(
                    token,
                    super::TokenType::IDENTIFIER,
                    origin_token,
                    byte_pos,
                ));
                continue;
            } else {
                // 如果所有解析器都失败，跳过当前字符
                let mut curr_pos = curr_pos.borrow_mut();
                *curr_pos += 1;
            }
        }

        tokens.into_inner()
    }

    // Reject comments from the token list
    pub fn reject_comment<'t>(tokens: &'t Vec<super::Token>) -> Vec<super::Token<'t>> {
        let mut result = Vec::new();
        for token in tokens {
            if token.token_type != super::TokenType::COMMENT {
                result.push(token.clone());
            }
        }
        result
    }
}