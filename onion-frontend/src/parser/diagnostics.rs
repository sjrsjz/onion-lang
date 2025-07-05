// In diagnostics.rs

use colored::*;
use unicode_width::UnicodeWidthStr; // 确保导入
use crate::parser::ast::ASTNode;

#[derive(Clone, Copy)]
pub enum ReportSeverity {
    Error,
    Warning,
}

pub fn format_node_based_report(
    severity: ReportSeverity,
    title: &str,
    message: &str,
    node: &ASTNode,
    help_text: &str,
) -> String {
    let (title_color, primary_color) = match severity {
        ReportSeverity::Error => (Color::BrightRed, Color::BrightRed),
        ReportSeverity::Warning => (Color::Yellow, Color::Yellow),
    };

    let (start_token, end_token) = match (&node.start_token, &node.end_token) {
        (Some(s), Some(e)) => (s, e),
        (Some(s), None) => (s, s),
        _ => {
            return format!(
                "{}: {}\n{}",
                title.color(title_color).bold(),
                message,
                "Error location not available.".italic()
            );
        }
    };

    let source_str = start_token.source_code_str();
    let lines: Vec<&str> = source_str.lines().collect();

    let (start_char, end_char) = {
        let (s, _) = start_token.origin_token_span();
        let (_, e) = end_token.origin_token_span();
        (s, e)
    };

    let (start_line_idx, start_col_char) = find_line_and_col(start_char, &lines);
    let (end_line_idx, end_col_char) = find_line_and_col(end_char, &lines);
    
    let mut report = String::new();

    report.push_str(&format!(
        "{}: {}\n --> {}:{}\n",
        title.color(title_color).bold(),
        message,
        (start_line_idx + 1).to_string().bright_cyan(),
        (start_col_char + 1).to_string().bright_cyan()
    ));

    for i in start_line_idx..=end_line_idx {
        let line_text = lines.get(i).unwrap_or(&"");

        report.push_str(&format!(
            "     |\n{:4} | {}\n     | ",
            (i + 1).to_string().bright_cyan(),
            line_text.white()
        ));

        if i == start_line_idx && i == end_line_idx {
            // --- 情况A: 单行 ---
            let display_offset = line_text.chars().take(start_col_char).collect::<String>().width();
            let underline_width = if end_col_char > start_col_char {
                line_text.chars().skip(start_col_char).take(end_col_char - start_col_char).collect::<String>().width()
            } else {
                1 // 至少显示一个字符宽度
            };
            report.push_str(&format!(
                "{}{}",
                " ".repeat(display_offset),
                "~".repeat(underline_width.max(1)).color(primary_color).bold()
            ));

        } else if i == start_line_idx {
            // --- 情况B: 多行的第一行 ---
            let display_offset = line_text.chars().take(start_col_char).collect::<String>().width();
            let underline_width = line_text.width() - display_offset;
            report.push_str(&format!(
                "{}{}",
                " ".repeat(display_offset),
                "~".repeat(underline_width.max(1)).color(primary_color).bold()
            ));

        } else if i == end_line_idx {
            // --- 情况C: 多行的最后一行 ---
            let underline_width = line_text.chars().take(end_col_char).collect::<String>().width();
            report.push_str(&format!("{}", "~".repeat(underline_width.max(1)).color(primary_color).bold()));
            
        } else {
            // --- 情况D: 多行的中间行 ---
            report.push_str(&format!("{}", "~".repeat(line_text.width()).color(primary_color).bold()));
        }
        report.push('\n');
    }

    if !help_text.is_empty() {
        report.push_str(&format!(
            "\n{}: {}",
            "Help".bright_green(),
            help_text
        ));
    }

    report
}


pub fn find_line_and_col(char_pos: usize, lines: &[&str]) -> (usize, usize) {
    let mut processed_chars = 0;
    for (line_idx, line) in lines.iter().enumerate() {
        let line_len_with_newline = line.chars().count() + 1;
        if processed_chars + line_len_with_newline > char_pos {
            let col = char_pos - processed_chars;
            return (line_idx, col);
        }
        processed_chars += line_len_with_newline;
    }
    (lines.len().saturating_sub(1), 0)
}