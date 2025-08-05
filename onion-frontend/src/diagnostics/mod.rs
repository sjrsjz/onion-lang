use std::fmt::Debug;

use crate::parser::lexer::Source;
use colored::*;
use unicode_width::UnicodeWidthStr;
pub mod collector;

/// 定义诊断信息的严重级别
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReportSeverity {
    Error,
    Warning,
}

/// 表示错误在源代码中的精确位置
#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub span: (usize, usize), // (start, end) character offset
    pub source: Source,       // The source object containing content and file path
}

/// 一个统一的接口，用于表示任何可以报告给用户的诊断信息。
pub trait Diagnostic: Debug + Send + Sync {
    fn severity(&self) -> ReportSeverity;
    fn title(&self) -> String;
    fn message(&self) -> String;
    fn location(&self) -> Option<SourceLocation>;
    fn help(&self) -> Option<String>;
    fn  copy(&self) -> Box<dyn Diagnostic>;

    /// 默认的格式化方法，将诊断信息转换为彩色的、人类可读的字符串。
    fn format_report(&self) -> String {
        let (title_color, primary_color) = match self.severity() {
            ReportSeverity::Error => (Color::BrightRed, Color::BrightRed),
            ReportSeverity::Warning => (Color::Yellow, Color::Yellow),
        };

        let mut report = String::new();

        // --- 1. 打印标题和主要信息 ---
        report.push_str(&format!(
            "{}: {}",
            self.title().color(title_color).bold(),
            self.message()
        ));

        // --- 2. 打印带代码上下文的位置信息 ---
        if let Some(loc) = self.location() {
            let source_content: String = loc.source.iter().collect();
            let lines: Vec<&str> = source_content.lines().collect();

            let (start_char, end_char) = loc.span;

            let (start_line_idx, start_col_char) = find_line_and_col(&source_content, start_char);
            let (end_line_idx, end_col_char) = find_line_and_col(&source_content, end_char);

            // 打印文件位置箭头
            report.push_str(&format!(
                "\n  {} {}:{}\n",
                "-->".bright_blue().bold(),
                (start_line_idx + 1).to_string().bright_cyan(),
                (start_col_char + 1).to_string().bright_cyan()
            ));

            // 打印代码行和下划线
            for i in start_line_idx..=end_line_idx {
                if let Some(line_text) = lines.get(i) {
                    report.push_str(&format!(
                        " {:>4} {} {}\n",
                        (i + 1).to_string().bright_cyan(),
                        "|".bright_blue().bold(),
                        line_text.white()
                    ));

                    // 计算下划线
                    let underline = build_underline(
                        i,
                        start_line_idx,
                        end_line_idx,
                        start_col_char,
                        end_col_char,
                        line_text,
                    );

                    report.push_str(&format!(
                        "      {} {}\n",
                        "|".bright_blue().bold(),
                        underline.color(primary_color).bold()
                    ));
                }
            }
        } else {
            // 如果没有位置信息，给一个提示
            report.push_str(&format!(
                "\n{}\n",
                "Note: Location information not available for this diagnostic.".italic()
            ));
        }

        // --- 3. 打印帮助信息 ---
        if let Some(help_text) = self.help() {
            report.push_str(&format!("\n{}: {}", "Help".bright_green(), help_text));
        }

        report
    }
}

/// 辅助函数：构建单行的下划线字符串
fn build_underline(
    current_line_idx: usize,
    start_line_idx: usize,
    end_line_idx: usize,
    start_col_char: usize,
    end_col_char: usize,
    line_text: &str,
) -> String {
    if current_line_idx == start_line_idx && current_line_idx == end_line_idx {
        // --- 情况A: 单行错误 ---
        let prefix_width = line_text
            .chars()
            .take(start_col_char)
            .collect::<String>()
            .width();
        let error_width = if end_col_char > start_col_char {
            line_text
                .chars()
                .skip(start_col_char)
                .take(end_col_char - start_col_char)
                .collect::<String>()
                .width()
        } else {
            1
        };
        format!(
            "{}{}",
            " ".repeat(prefix_width),
            "^".repeat(error_width.max(1))
        )
    } else if current_line_idx == start_line_idx {
        // --- 情况B: 多行的第一行 ---
        let prefix_width = line_text
            .chars()
            .take(start_col_char)
            .collect::<String>()
            .width();
        let error_width = line_text.width() - prefix_width;
        format!(
            "{}{}",
            " ".repeat(prefix_width),
            "^".repeat(error_width.max(1))
        )
    } else if current_line_idx == end_line_idx {
        // --- 情况C: 多行的最后一行 ---
        let error_width = line_text
            .chars()
            .take(end_col_char)
            .collect::<String>()
            .width();
        "^".repeat(error_width.max(1)).to_string()
    } else {
        // --- 情况D: 多行的中间行 ---
        "^".repeat(line_text.width()).to_string()
    }
}

/// 从源码字符串中根据字符偏移查找行号和列号 (0-indexed)
fn find_line_and_col(source: &str, char_pos: usize) -> (usize, usize) {
    let mut current_pos = 0;
    for (line_idx, line) in source.lines().enumerate() {
        let line_start_pos = current_pos;
        let line_end_pos = line_start_pos + line.len() + 1; // +1 for the newline character

        if char_pos >= line_start_pos && char_pos < line_end_pos {
            return (line_idx, char_pos - line_start_pos);
        }
        current_pos = line_end_pos;
    }
    // If position is at the very end of the file
    (source.lines().count().saturating_sub(1), 0)
}

// pub fn format_node_based_report(
//     severity: ReportSeverity,
//     title: &str,
//     message: &str,
//     node: &ASTNode,
//     help_text: &str,
// ) -> String {
//     let (title_color, primary_color) = match severity {
//         ReportSeverity::Error => (Color::BrightRed, Color::BrightRed),
//         ReportSeverity::Warning => (Color::Yellow, Color::Yellow),
//     };

//     let (start_token, end_token) = match (&node.start_token, &node.end_token) {
//         (Some(s), Some(e)) => (s, e),
//         (Some(s), None) => (s, s),
//         _ => {
//             return format!(
//                 "{}: {}\n{}",
//                 title.color(title_color).bold(),
//                 message,
//                 "Error location not available.".italic()
//             );
//         }
//     };

//     let source_str = start_token.source_code_str();
//     let lines: Vec<&str> = source_str.lines().collect();

//     let (start_char, end_char) = {
//         let (s, _) = start_token.origin_token_span();
//         let (_, e) = end_token.origin_token_span();
//         (s, e)
//     };

//     // 直接使用原始源码字符串进行位置计算
//     let (start_line_idx, start_col_char) = find_line_and_col_from_source(start_char, &source_str);
//     let (end_line_idx, end_col_char) = find_line_and_col_from_source(end_char, &source_str);

//     let mut report = String::new();

//     report.push_str(&format!(
//         "{}: {}\n --> {}:{}\n",
//         title.color(title_color).bold(),
//         message,
//         (start_line_idx + 1).to_string().bright_cyan(),
//         (start_col_char + 1).to_string().bright_cyan()
//     ));

//     for i in start_line_idx..=end_line_idx {
//         let line_text = lines.get(i).unwrap_or(&"");

//         report.push_str(&format!(
//             "     |\n{:4} | {}\n     | ",
//             (i + 1).to_string().bright_cyan(),
//             line_text.white()
//         ));

//         if i == start_line_idx && i == end_line_idx {
//             // --- 情况A: 单行 ---
//             let display_offset = line_text
//                 .chars()
//                 .take(start_col_char)
//                 .collect::<String>()
//                 .width();
//             let underline_width = if end_col_char > start_col_char {
//                 line_text
//                     .chars()
//                     .skip(start_col_char)
//                     .take(end_col_char - start_col_char)
//                     .collect::<String>()
//                     .width()
//             } else {
//                 1 // 至少显示一个字符宽度
//             };
//             report.push_str(&format!(
//                 "{}{}",
//                 " ".repeat(display_offset),
//                 "~".repeat(underline_width.max(1))
//                     .color(primary_color)
//                     .bold()
//             ));
//         } else if i == start_line_idx {
//             // --- 情况B: 多行的第一行 ---
//             let display_offset = line_text
//                 .chars()
//                 .take(start_col_char)
//                 .collect::<String>()
//                 .width();
//             let underline_width = line_text.width() - display_offset;
//             report.push_str(&format!(
//                 "{}{}",
//                 " ".repeat(display_offset),
//                 "~".repeat(underline_width.max(1))
//                     .color(primary_color)
//                     .bold()
//             ));
//         } else if i == end_line_idx {
//             // --- 情况C: 多行的最后一行 ---
//             let underline_width = line_text
//                 .chars()
//                 .take(end_col_char)
//                 .collect::<String>()
//                 .width();
//             report.push_str(&format!(
//                 "{}",
//                 "~".repeat(underline_width.max(1))
//                     .color(primary_color)
//                     .bold()
//             ));
//         } else {
//             // --- 情况D: 多行的中间行 ---
//             report.push_str(&format!(
//                 "{}",
//                 "~".repeat(line_text.width()).color(primary_color).bold()
//             ));
//         }
//         report.push('\n');
//     }

//     if !help_text.is_empty() {
//         report.push_str(&format!("\n{}: {}", "Help".bright_green(), help_text));
//     }

//     report
// }

// /// 从原始源码字符串中根据字符索引查找行号和列号
// /// 这是更准确的版本，直接使用原始源码而不是重构的文本
// pub fn find_line_and_col_from_source(char_pos: usize, source: &str) -> (usize, usize) {
//     let chars: Vec<char> = source.chars().collect();

//     // 如果位置超出范围，返回最后位置
//     if char_pos >= chars.len() {
//         let line_count = source.lines().count();
//         return (line_count.saturating_sub(1), 0);
//     }

//     // 逐字符遍历，正确处理换行符
//     let mut current_line = 0;
//     let mut current_col = 0;

//     for (i, &ch) in chars.iter().enumerate() {
//         if i == char_pos {
//             return (current_line, current_col);
//         }

//         if ch == '\n' {
//             current_line += 1;
//             current_col = 0;
//         } else if ch == '\r' {
//             // 检查是否是 \r\n (Windows)
//             if i + 1 < chars.len() && chars[i + 1] == '\n' {
//                 // 这是 \r\n，跳过 \r，让 \n 处理换行
//                 continue;
//             } else {
//                 // 这是单独的 \r (旧Mac风格)
//                 current_line += 1;
//                 current_col = 0;
//             }
//         } else {
//             current_col += 1;
//         }
//     }

//     // 如果到达这里，说明 char_pos 就是最后一个字符
//     (current_line, current_col)
// }
