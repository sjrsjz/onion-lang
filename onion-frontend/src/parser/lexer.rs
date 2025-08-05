use std::{
    fmt::{Debug, Display},
    ops::Deref,
    path::Path,
    sync::Arc,
};

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
        // Deriving PartialEq would be simpler, but this works too.
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

/// Represents a source code input, including its content and optionally its file path.
///
/// This struct uses `Arc` internally to make cloning cheap, allowing the source
/// content to be shared across different parts of the compiler/VM without multiple
/// allocations.
#[derive(Clone)]
pub struct Source {
    content: Arc<Vec<char>>,
    file_path: Option<Arc<Path>>,
}

impl Source {
    pub fn from_string(source: String) -> Self {
        Self {
            content: Arc::new(source.chars().collect()),
            file_path: None,
        }
    }

    pub fn from_string_with_file_path<P: AsRef<Path>>(source: String, path: P) -> Self {
        Self {
            content: Arc::new(source.chars().collect()),
            file_path: Some(Arc::from(path.as_ref())),
        }
    }

    pub fn from_file<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let path_ref = path.as_ref();
        let content_string = std::fs::read_to_string(path_ref)?;
        Ok(Self {
            content: Arc::new(content_string.chars().collect()),
            file_path: Some(Arc::from(path_ref)),
        })
    }

    pub fn file_path(&self) -> Option<&Path> {
        self.file_path.as_deref()
    }

    pub fn content(&self) -> &Arc<Vec<char>> {
        &self.content
    }

    pub fn content_str(&self) -> String {
        self.content.iter().collect()
    }
}

impl Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let path_str = self
            .file_path
            .as_ref()
            .map(|p| p.to_string_lossy())
            .unwrap_or_else(|| "<in-memory>".into());

        let content_snippet: String = self.content.iter().take(40).collect();
        write!(
            f,
            "Source(file: \"{}\", content: \"{}...\")",
            path_str, content_snippet
        )
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.file_path {
            Some(path) => write!(f, "{}", path.display()),
            None => write!(f, "<anonymous>"),
        }
    }
}

impl Deref for Source {
    type Target = Arc<Vec<char>>;

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl From<String> for Source {
    fn from(source: String) -> Self {
        Self::from_string(source)
    }
}

impl From<&str> for Source {
    fn from(source: &str) -> Self {
        Self::from_string(source.to_string())
    }
}
#[derive(Debug, Clone)]
pub struct Token {
    token: String,                     // The processed token value
    origin_token_span: (usize, usize), // The original token span in source
    source_code: Source,               // The shared source code
    token_type: TokenType,             // The type of the token
}

impl Token {
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

    pub fn token(&self) -> &String {
        &self.token
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

pub mod tokenizer {
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
    pub fn tokenize(source: &Source) -> Vec<Token> {
        let chars: &Vec<char> = &source;

        let tokens = RefCell::new(Vec::<super::Token>::new());
        let curr_pos = RefCell::new(0usize);

        let skip_space = || {
            let mut curr_pos = curr_pos.borrow_mut();
            while *curr_pos < chars.len() && chars[*curr_pos].is_whitespace() {
                *curr_pos += 1;
            }
        };

        // All `read_*` helper functions now return a tuple of:
        // (processed_token_string, origin_token_span)
        // This makes the main loop cleaner and encapsulates span calculation.

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

        // Main tokenization loop
        loop {
            skip_space();
            if *curr_pos.borrow() >= chars.len() {
                break;
            }

            // The logic now is:
            // 1. Try to read a token of a certain type.
            // 2. If successful, the `read_*` function returns the processed string and its original span.
            // 3. Create a new `Token` with these values and the shared `source` Arc.

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

    /// Reject comments from the token list.
    pub fn reject_comment(tokens: &Vec<super::Token>) -> Vec<super::Token> {
        tokens
            .iter()
            .filter(|token| token.token_type() != super::TokenType::COMMENT)
            .cloned()
            .collect()
    }
}
