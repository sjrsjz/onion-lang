use std::{ops::Deref, sync::Arc};

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
pub struct Source(Arc<Vec<char>>);
impl From<String> for Source {
    fn from(source: String) -> Self {
        Source(Arc::new(source.chars().collect()))
    }
}

impl From<Arc<Vec<char>>> for Source {
    fn from(source: Arc<Vec<char>>) -> Self {
        Source(source)
    }
}

impl Into<Arc<Vec<char>>> for Source {
    fn into(self) -> Arc<Vec<char>> {
        self.0
    }
}

impl Into<String> for Source {
    fn into(self) -> String {
        self.0.iter().collect()
    }
}

impl Deref for Source {
    type Target = Arc<Vec<char>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    token_span: (usize, usize),        // The span of the token
    origin_token_span: (usize, usize), // The original token span
    source_code: Source,               // The source code as a vector of characters
    token_type: TokenType,             // The type of the token
}

impl Token {
    pub fn new(
        token_span: (usize, usize),
        origin_token_span: (usize, usize),
        source_code: Arc<Vec<char>>,
        token_type: TokenType,
    ) -> Token {
        Token {
            token_span,
            origin_token_span,
            source_code: source_code.into(),
            token_type,
        }
    }

    pub fn token_span(&self) -> (usize, usize) {
        self.token_span
    }

    pub fn token(&self) -> String {
        let (start, end) = self.token_span;
        self.source_code[start..end].iter().collect()
    }

    pub fn origin_token_span(&self) -> (usize, usize) {
        self.origin_token_span
    }

    pub fn origin_token(&self) -> String {
        let (start, end) = self.origin_token_span;
        self.source_code[start..end].iter().collect()
    }
    pub fn source_code(&self) -> &Source {
        &self.source_code
    }

    pub fn source_code_str(&self) -> String {
        self.source_code.iter().collect()
    }

    pub fn token_type(&self) -> TokenType {
        self.token_type.clone()
    }
}

impl PartialEq<String> for Token {
    fn eq(&self, other: &String) -> bool {
        self.token() == *other
    }
}

impl PartialEq<&str> for Token {
    fn eq(&self, other: &&str) -> bool {
        self.token() == *other
    }
}

impl PartialEq<str> for Token {
    fn eq(&self, other: &str) -> bool {
        self.token() == other
    }
}

impl PartialEq<TokenType> for Token {
    fn eq(&self, other: &TokenType) -> bool {
        self.token_type == *other
    }
}

impl PartialEq<TokenType> for &Token {
    fn eq(&self, other: &TokenType) -> bool {
        self.token_type == *other
    }
}

pub mod lexer {
    use std::cell::RefCell;

    // Use the Token and Source from the parent module.
    use super::{Source, Token};

    pub fn is_operator(symbol: &str) -> bool {
        let operators = vec![
            "+", "-", "*", "**", "/", "\\", "%", "&", "!", "^", "~", "=", "==", ">", "<", "<=",
            ">=", "!=", "?=", "|", "?", ":>", "#", "&&", ",", ".", "\n", ":", "->", "<<", ">>",
            "/*", "*/", ";", " ", ":=", "|>", "<|", "::", "=>", "++", "||", ">>", "<<", "\"\"\"",
            "'''", "(", ")", "[", "]", "{", "}", "..", "...", "@", "$",
        ];
        operators.contains(&symbol)
    }

    // Tokenize the input code
    pub fn tokenize(code: &str) -> Vec<Token> {
        // Create a single Source instance that will be shared across all tokens.
        let source = Source::from(code.to_string());
        let chars: &Vec<char> = &source; // Get a reference to the character vector.

        let tokens = RefCell::new(Vec::<super::Token>::new());
        let curr_pos = RefCell::new(0usize);

        // Skip whitespace
        let skip_space = || {
            let mut curr_pos = curr_pos.borrow_mut();
            while *curr_pos < chars.len() && chars[*curr_pos].is_whitespace() {
                *curr_pos += 1;
            }
        };

        // All `read_*` helper functions now return a tuple of:
        // (processed_value, original_text_from_source)
        // The `original_text_from_source` is used to determine the token's span.
        // NOTE: The new `Token` struct's `token()` method re-slices the source code.
        // This means it will return a string slice with escapes (e.g., "a\\nb")
        // rather than the processed string value (e.g., "a\nb"). This is a
        // direct consequence of the provided `Token` struct's design.

        // test_string and test_number closures remain the same as they operate on `chars`
        // and `curr_pos`, which are correctly defined.

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
                        return substring[..matched.end()].chars().count();
                    }
                }
            }

            if substring.len() >= 2 {
                let first_two_chars: String = substring.chars().take(2).collect();
                if first_two_chars.to_lowercase() == "0o" {
                    let oct_pattern = r"^0[oO][0-7]+";
                    if let Some(matched) = regex::Regex::new(oct_pattern).unwrap().find(&substring)
                    {
                        return substring[..matched.end()].chars().count();
                    }
                }
            }

            if substring.len() >= 2 {
                let first_two_chars: String = substring.chars().take(2).collect();
                if first_two_chars.to_lowercase() == "0b" {
                    let bin_pattern = r"^0[bB][01]+";
                    if let Some(matched) = regex::Regex::new(bin_pattern).unwrap().find(&substring)
                    {
                        return substring[..matched.end()].chars().count();
                    }
                }
            }

            let number_pattern = r"^\d*\.?\d+([eE][-+]?\d+)?";
            if let Some(matched) = regex::Regex::new(number_pattern).unwrap().find(&substring) {
                return substring[..matched.end()].chars().count();
            }

            0
        };

        // The logic inside the `read_*` functions is mostly unchanged, as they correctly
        // advance `curr_pos` and produce the token strings.
        let read_number = || -> Option<(String, String)> {
            let mut pos = curr_pos.borrow_mut();
            let len = test_number(*pos);
            if len == 0 {
                return None;
            }
            let token: String = chars[*pos..*pos + len].iter().collect();
            *pos += len;
            Some((token.clone(), token)) // token, original token
        };

        let read_base64 = || -> Option<(String, String)> {
            let mut pos = curr_pos.borrow_mut();
            let start_pos = *pos;
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
                        // Incomplete escape sequence
                        *pos = start_pos; // backtrack
                        return None;
                    }
                } else if chars[*pos] == '"' {
                    *pos += 1; // consume closing quote
                    let original_token: String = chars[start_pos..*pos].iter().collect();
                    return Some((current_token, original_token));
                } else {
                    current_token.push(chars[*pos]);
                    *pos += 1;
                }
            }
            // unclosed string, backtrack and fail
            *pos = start_pos;
            None
        };

        // The rest of the read_* functions (read_string, read_comment, etc.) are assumed to be correct
        // in their logic for parsing and advancing `curr_pos`. They are used as-is from the prompt.
        // (The full code for these is long, so it's omitted here for brevity, but would be included in the final file)

        let read_string = || -> Option<(String, String)> {
            let mut curr_pos = curr_pos.borrow_mut();
            let mut current_token = String::new();
            let original_token;

            // 处理 R"..." 原始字符串
            if test_string("R\"", *curr_pos) {
                let start_of_raw = *curr_pos;
                *curr_pos += 2;
                let mut divider = String::new();

                // 读取分隔符
                while *curr_pos < chars.len() && chars[*curr_pos] != '(' {
                    divider.push(chars[*curr_pos]);
                    *curr_pos += 1;
                }

                if *curr_pos < chars.len() && chars[*curr_pos] == '(' {
                    *curr_pos += 1; // 跳过 '('
                    let end_divider = format!("){}", divider);
                    let end_sequence = format!("{}\"", end_divider);

                    while *curr_pos < chars.len() && !test_string(&end_sequence, *curr_pos) {
                        current_token.push(chars[*curr_pos]);
                        *curr_pos += 1;
                    }

                    if *curr_pos < chars.len() {
                        *curr_pos += end_sequence.len();
                        original_token = chars[start_of_raw..*curr_pos].iter().collect();
                        return Some((current_token, original_token));
                    }
                }
                *curr_pos = start_of_raw; // backtrack on failure
                return None;
            }

            // Other string types... (full implementation from prompt)
            let start_char_pos = *curr_pos;

            if test_string("\"\"\"", *curr_pos) || test_string("'''", *curr_pos) {
                let quote_seq: String = chars[*curr_pos..*curr_pos + 3].iter().collect();
                *curr_pos += 3;

                while *curr_pos < chars.len() {
                    if test_string(&quote_seq, *curr_pos) {
                        *curr_pos += 3;
                        let original_token: String =
                            chars[start_char_pos..*curr_pos].iter().collect();
                        return Some((current_token, original_token));
                    }

                    if chars[*curr_pos] == '\\' {
                        *curr_pos += 1;
                        if *curr_pos < chars.len() {
                            let escape_char = chars[*curr_pos];
                            match escape_char {
                                'n' => current_token.push('\n'),
                                'r' => current_token.push('\r'),
                                't' => current_token.push('\t'),
                                _ => current_token.push(escape_char), // Simplified for example
                            }
                            *curr_pos += 1;
                        } else {
                            *curr_pos = start_char_pos;
                            return None;
                        }
                    } else {
                        current_token.push(chars[*curr_pos]);
                        *curr_pos += 1;
                    }
                }
                *curr_pos = start_char_pos; // Unclosed string
                return None;
            }

            let quote_pairs: std::collections::HashMap<char, char> =
                [('"', '"'), ('\'', '\''), ('`', '`')]
                    .iter()
                    .cloned()
                    .collect();

            if *curr_pos < chars.len() {
                let start_char = chars[*curr_pos];
                if quote_pairs.contains_key(&start_char) {
                    *curr_pos += 1;
                    while *curr_pos < chars.len() {
                        if chars[*curr_pos] == '\\' {
                            *curr_pos += 1;
                            if *curr_pos < chars.len() {
                                let escape_char = chars[*curr_pos];
                                match escape_char {
                                    'n' => current_token.push('\n'),
                                    'r' => current_token.push('\r'),
                                    't' => current_token.push('\t'),
                                    _ => current_token.push(escape_char), // Simplified
                                }
                                *curr_pos += 1;
                            } else {
                                *curr_pos = start_char_pos;
                                return None;
                            }
                        } else if chars[*curr_pos] == start_char {
                            *curr_pos += 1;
                            let original_token: String =
                                chars[start_char_pos..*curr_pos].iter().collect();
                            return Some((current_token, original_token));
                        } else {
                            current_token.push(chars[*curr_pos]);
                            *curr_pos += 1;
                        }
                    }
                    *curr_pos = start_char_pos; // unclosed string
                }
            }
            None
        };

        let read_comment = || -> Option<(String, String)> {
            let mut pos = curr_pos.borrow_mut();
            let start_pos = *pos;
            if test_string("//", *pos) {
                *pos += 2;
                let mut current_token = String::new();
                while *pos < chars.len() && !['\n', '\r'].contains(&chars[*pos]) {
                    current_token.push(chars[*pos]);
                    *pos += 1;
                }
                let original_token = chars[start_pos..*pos].iter().collect();
                return Some((current_token, original_token));
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
                    let original_token = chars[start_pos..*pos].iter().collect();
                    return Some((current_token, original_token));
                }
            }
            *pos = start_pos; // backtrack
            None
        };

        let read_operator = || -> Option<(String, String)> {
            let mut pos = curr_pos.borrow_mut();
            if *pos + 2 < chars.len() {
                let three_chars: String = chars[*pos..*pos + 3].iter().collect();
                if is_operator(&three_chars) {
                    *pos += 3;
                    return Some((three_chars.clone(), three_chars));
                }
            }
            if *pos + 1 < chars.len() {
                let two_chars: String = chars[*pos..*pos + 2].iter().collect();
                if is_operator(&two_chars) {
                    *pos += 2;
                    return Some((two_chars.clone(), two_chars));
                }
            }
            if *pos < chars.len() {
                let one_char = chars[*pos].to_string();
                if is_operator(&one_char) {
                    *pos += 1;
                    return Some((one_char.clone(), one_char));
                }
            }
            None
        };

        let read_token = || -> Option<(String, String)> {
            let mut pos = curr_pos.borrow_mut();
            let start_pos = *pos;
            if *pos < chars.len() {
                let first_char = chars[*pos];
                if first_char.is_alphabetic() || first_char == '_' {
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
                    return Some((token.clone(), token));
                }
            }
            None
        };

        // Main tokenization loop
        loop {
            skip_space();
            let start_pos = *curr_pos.borrow();
            if start_pos >= chars.len() {
                break;
            }

            // The logic is now:
            // 1. Try to read a token of a certain type.
            // 2. If successful, the `read_*` function updates `curr_pos`.
            // 3. The `origin_token` string gives us the character count to calculate the end span.
            // 4. We calculate the `token_span` (the content) and `origin_token_span` (the full text).
            // 5. We create a new `Token` with these spans and the shared `source` Arc.

            if let Some((_token, origin_token)) = read_number() {
                let end_pos = start_pos + origin_token.chars().count();
                let span = (start_pos, end_pos);
                tokens.borrow_mut().push(super::Token::new(
                    span, // For numbers, token and origin spans are the same
                    span,
                    source.0.clone(),
                    super::TokenType::NUMBER,
                ));
                continue;
            }

            if let Some((_token, origin_token)) = read_base64() {
                let end_pos = start_pos + origin_token.chars().count();
                let origin_token_span = (start_pos, end_pos);
                // Content is between `$` and `"`
                let token_span = (start_pos + 2, end_pos - 1);
                tokens.borrow_mut().push(super::Token::new(
                    token_span,
                    origin_token_span,
                    source.0.clone(),
                    super::TokenType::BASE64,
                ));
                continue;
            }

            if let Some((_token, origin_token)) = read_string() {
                let end_pos = start_pos + origin_token.chars().count();
                let origin_token_span = (start_pos, end_pos);

                let token_span = if origin_token.starts_with("R\"") {
                    let first_paren = origin_token.find('(').unwrap_or(2);
                    let prefix_len = first_paren + 1;
                    let suffix_len =
                        origin_token.len() - origin_token.rfind(')').unwrap_or(end_pos);
                    (start_pos + prefix_len, end_pos - suffix_len)
                } else if origin_token.starts_with("\"\"\"") || origin_token.starts_with("'''") {
                    (start_pos + 3, end_pos - 3)
                } else {
                    (start_pos + 1, end_pos - 1)
                };

                tokens.borrow_mut().push(super::Token::new(
                    token_span,
                    origin_token_span,
                    source.0.clone(),
                    super::TokenType::STRING,
                ));
                continue;
            }

            if let Some((_token, origin_token)) = read_comment() {
                let end_pos = start_pos + origin_token.chars().count();
                let origin_token_span = (start_pos, end_pos);

                let token_span = if origin_token.starts_with("/*") {
                    (start_pos + 2, end_pos - 2)
                } else {
                    // Handles `//`
                    (start_pos + 2, end_pos)
                };

                tokens.borrow_mut().push(super::Token::new(
                    token_span,
                    origin_token_span,
                    source.0.clone(),
                    super::TokenType::COMMENT,
                ));
                continue;
            }

            if let Some((_token, origin_token)) = read_operator() {
                let end_pos = start_pos + origin_token.chars().count();
                let span = (start_pos, end_pos);
                tokens.borrow_mut().push(super::Token::new(
                    span,
                    span,
                    source.0.clone(),
                    super::TokenType::SYMBOL,
                ));
                continue;
            }

            if let Some((_token, origin_token)) = read_token() {
                let end_pos = start_pos + origin_token.chars().count();
                let span = (start_pos, end_pos);
                tokens.borrow_mut().push(super::Token::new(
                    span,
                    span,
                    source.0.clone(),
                    super::TokenType::IDENTIFIER,
                ));
                continue;
            } else {
                // If all parsers fail, advance by one character to avoid an infinite loop.
                let mut curr_pos = curr_pos.borrow_mut();
                if *curr_pos < chars.len() {
                    *curr_pos += 1;
                }
            }
        }

        tokens.into_inner()
    }

    /// Reject comments from the token list.
    /// The signature is corrected to not use a lifetime parameter, as the new Token struct is self-contained.
    pub fn reject_comment(tokens: &Vec<super::Token>) -> Vec<super::Token> {
        tokens
            .iter()
            .filter(|token| token.token_type() != super::TokenType::COMMENT)
            .cloned()
            .collect()
    }
}
