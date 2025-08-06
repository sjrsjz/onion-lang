
//! 诊断信息与错误报告核心模块。
//!
//! 提供统一的诊断对象接口、错误定位、分级报告与彩色格式化输出，
//! 支持编译器前端、分析器等场景下的高质量错误与警告展示。
//!
//! # 主要功能
//! - 诊断对象的统一 trait（Diagnostic）
//! - 错误/警告分级（ReportSeverity）
//! - 源码精确定位（SourceLocation）
//! - 彩色人类可读报告格式化
//! - 诊断信息的批量收集与报告（见 collector 子模块）
//!
//! # 典型用法
//! ```ignore
//! let diag: Box<dyn Diagnostic> = ...;
//! println!("{}", diag.format_report());
//! ```

use std::fmt::Debug;

use colored::*;
use serde::{Deserialize, Serialize};
use unicode_width::UnicodeWidthStr;

use crate::parser::Source;
pub mod collector;

/// 诊断信息的严重级别。
///
/// 用于区分错误（Error）与警告（Warning），影响报告格式与处理逻辑。
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReportSeverity {
    Error,
    Warning,
}

/// 源码错误的精确定位信息。
///
/// 包含字符偏移、源码内容与文件路径等信息，支持多文件、多行定位。
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceLocation {
    pub span: (usize, usize), // (start, end) character offset
    pub source: Source,       // The source object containing content and file path
}

/// 诊断对象统一 trait。
///
/// 任何可报告给用户的诊断信息都应实现该 trait，
/// 支持分级、标题、消息、定位、帮助与深拷贝。
///
/// # 关键方法
/// - `severity()`: 严重级别
/// - `title()`: 简要标题
/// - `message()`: 详细消息
/// - `location()`: 源码定位
/// - `help()`: 可选帮助信息
/// - `copy()`: 深拷贝
/// - `format_report()`: 彩色格式化输出
pub trait Diagnostic: Debug + Send + Sync {
    fn severity(&self) -> ReportSeverity;
    fn title(&self) -> String;
    fn message(&self) -> String;
    fn location(&self) -> Option<SourceLocation>;
    fn help(&self) -> Option<String>;
    fn copy(&self) -> Box<dyn Diagnostic>;

    /// 默认的格式化方法，将诊断信息转换为彩色的、人类可读的字符串。
    ///
    /// 支持多行源码高亮、文件路径、行号、下划线标记与帮助信息。
    /// 适用于终端输出与日志记录。
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
            match loc.source.file_path() {
                Some(path) => {
                    report.push_str(&format!(
                        "\n  {} {}:{} in {}\n",
                        "-->".bright_blue().bold(),
                        (start_line_idx + 1).to_string().bright_cyan(),
                        (start_col_char + 1).to_string().bright_cyan(),
                        path.display().to_string().bright_yellow().underline()
                    ));
                }
                None => {
                    report.push_str(&format!(
                        "\n  {} {}:{}\n",
                        "-->".bright_blue().bold(),
                        (start_line_idx + 1).to_string().bright_cyan(),
                        (start_col_char + 1).to_string().bright_cyan()
                    ));
                }
            }
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

/// 辅助函数：构建单行的下划线字符串。
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

/// 从源码字符串中根据字符偏移查找行号和列号 (0-indexed)。
///
/// 直接使用原始源码，支持多平台换行符。
fn find_line_and_col(source: &str, char_pos: usize) -> (usize, usize) {
    let chars: Vec<char> = source.chars().collect();

    // 如果位置超出范围，返回最后位置
    if char_pos >= chars.len() {
        let line_count = source.lines().count();
        return (line_count.saturating_sub(1), 0);
    }

    // 逐字符遍历，正确处理换行符
    let mut current_line = 0;
    let mut current_col = 0;

    for (i, &ch) in chars.iter().enumerate() {
        if i == char_pos {
            return (current_line, current_col);
        }

        if ch == '\n' {
            current_line += 1;
            current_col = 0;
        } else if ch == '\r' {
            // 检查是否是 \r\n (Windows)
            if i + 1 < chars.len() && chars[i + 1] == '\n' {
                // 这是 \r\n，跳过 \r，让 \n 处理换行
                continue;
            } else {
                // 这是单独的 \r (旧Mac风格)
                current_line += 1;
                current_col = 0;
            }
        } else {
            current_col += 1;
        }
    }

    // 如果到达这里，说明 char_pos 就是最后一个字符
    (current_line, current_col)
}
