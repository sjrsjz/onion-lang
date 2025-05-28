use super::protocol::*;

/// 文本文档结构
#[allow(dead_code)]
pub struct TextDocument {
    /// 文档URI
    pub uri: String,
    /// 文档语言ID
    pub language_id: String,
    /// 文档版本号
    pub version: i32,
    /// 文档内容
    pub content: String,
    /// 文档的行数组
    lines: Vec<String>,
}

impl TextDocument {
    /// 创建新文档
    pub fn new(uri: String, language_id: String, version: i32, content: String) -> Self {
        let lines = content.lines().map(|l| l.to_string()).collect();
        Self {
            uri,
            language_id,
            version,
            content,
            lines,
        }
    }

    /// 更新文档内容
    pub fn update_content(&mut self, content: String) {
        self.content = content;
        self.lines = self.content.lines().map(|l| l.to_string()).collect();
    }

    /// 应用增量变更
    pub fn apply_incremental_change(&mut self, range: Range, text: String) {
        // 简单实现 - 完全重建内容，实际LSP应该做更高效的修改
        let mut new_content = String::new();
        
        // 添加range前的内容
        let start_line = range.start.line as usize;
        let end_line = range.end.line as usize;
        
        for i in 0..start_line {
            if i > 0 {
                new_content.push('\n');
            }
            new_content.push_str(&self.lines[i]);
        }
        
        // 添加start_line的开始部分
        if start_line < self.lines.len() {
            if start_line > 0 {
                new_content.push('\n');
            }
            
            let start_char = range.start.character as usize;
            if start_char <= self.lines[start_line].len() {
                new_content.push_str(&self.lines[start_line][..start_char]);
            }
        }
        
        // 添加新文本
        new_content.push_str(&text);
        
        // 添加end_line的结束部分
        if end_line < self.lines.len() {
            let end_char = range.end.character as usize;
            if end_char <= self.lines[end_line].len() {
                new_content.push_str(&self.lines[end_line][end_char..]);
            }
        }
        
        // 添加剩余行
        for i in (end_line + 1)..self.lines.len() {
            new_content.push('\n');
            new_content.push_str(&self.lines[i]);
        }
        
        self.update_content(new_content);
    }
}