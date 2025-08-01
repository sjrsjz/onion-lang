use crate::types::object::OnionObject;
pub mod fastmap;

const MAX_DEBUG_LEN: usize = 120; // 定义一个合理的单行最大长度

/// 一个辅助函数，用于格式化 OnionObject，如果太长则截断。
pub fn format_object_summary(obj: &OnionObject) -> String {
    // 使用非 pretty 的 debug 格式化，这样可以得到一个单行的表示
    let debug_str = obj.repr(&vec![]).unwrap_or("<unknown>".to_string());

    if debug_str.len() > MAX_DEBUG_LEN {
        // 如果字符串太长，截断并加上 "..."
        // 我们需要小心处理多字节字符，所以使用 chars()
        let mut truncated: String = debug_str.chars().take(MAX_DEBUG_LEN).collect();
        truncated.push_str("...");
        truncated
    } else {
        // 如果长度合适，直接返回
        debug_str
    }
}


/// 从原始源码字符串中根据字符索引查找行号和列号
/// 这是更准确的版本，直接使用原始源码而不是重构的文本
pub fn find_line_and_col_from_source(char_pos: usize, source: &str) -> (usize, usize) {
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