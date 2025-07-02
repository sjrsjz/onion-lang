// src/lib.rs

use std::io;
use std::path::{Path, PathBuf};
use std::fs;

/// 自定义错误类型，用于封装可能发生的错误。
#[derive(Debug)]
pub enum StackError {
    /// 包装了标准的 I/O 错误，例如路径不存在。
    Io(io::Error),
    /// 当栈为空时尝试获取当前目录，理论上不会发生，因为我们保证栈底总有一个元素。
    StackEmpty,
    /// 当提供的路径没有父目录时（例如根路径 "/"）。
    NoParentDirectory(PathBuf),
}

// 为我们的自定义错误实现 Display trait，以便能友好地打印出来。
impl std::fmt::Display for StackError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackError::Io(err) => write!(f, "IO error: {}", err),
            StackError::StackEmpty => write!(f, "Directory stack is empty"),
            StackError::NoParentDirectory(path) => write!(f, "Path '{}' has no parent directory", path.display()),
        }
    }
}

// 实现 Error trait，这是 Rust 错误处理的最佳实践。
impl std::error::Error for StackError {}

// 允许从 std::io::Error 自动转换为我们的 StackError。
impl From<io::Error> for StackError {
    fn from(err: io::Error) -> Self {
        StackError::Io(err)
    }
}

/// 目录栈结构体，用于维护一组绝对路径。
#[derive(Debug)]
pub struct DirectoryStack {
    // 使用 Vec<PathBuf>作为栈，存储的是绝对路径。
    stack: Vec<PathBuf>,
}

// 新增：实现 Default trait
impl Default for DirectoryStack {
    /// 从当前工作目录创建一个新的 `DirectoryStack`。
    ///
    /// 这是 `DirectoryStack::new().unwrap()` 的便捷方式。
    ///
    /// # Panics
    ///
    /// 如果无法获取当前工作目录，此方法会 panic。
    fn default() -> Self {
        Self::new(None).expect("Failed to create DirectoryStack from the current working directory.")
    }
}

impl DirectoryStack {
    /// 创建一个新的目录栈。
    ///
    /// 初始时，它会将当前工作目录作为第一个也是唯一的元素压入栈中。
    ///
    /// # Errors
    ///
    /// 如果无法获取当前工作目录，将返回一个 I/O 错误。
    pub fn new(path: Option<&Path>) -> Result<Self, io::Error> {
        // let current_dir = std::env::current_dir()?;
        // Ok(Self {
        //     stack: vec![current_dir],
        // })
        let initial_path = match path {
            Some(p) => fs::canonicalize(p)?,
            None => fs::canonicalize(".")?,
        };
        Ok(Self {
            stack: vec![initial_path],
        })
    }

    /// 将一个新目录压入栈顶。
    ///
    /// # Arguments
    /// * `path` - 一个目录路径，可以是相对路径或绝对路径。
    pub fn push(&mut self, path: &Path) -> Result<(), StackError> {
        let base_dir = self.stack.last().ok_or(StackError::StackEmpty)?;
        let new_path = base_dir.join(path);
        let absolute_path = fs::canonicalize(new_path)?;

        self.stack.push(absolute_path);
        Ok(())
    }
    
    /// 新增：根据文件路径将其父目录压入栈中。
    ///
    /// # Arguments
    /// * `file_path` - 一个文件的路径，可以是相对或绝对的。
    ///
    /// # Errors
    /// * `StackError::NoParentDirectory` - 如果解析后的文件路径没有父目录。
    pub fn push_file(&mut self, file_path: &Path) -> Result<(), StackError> {
        let base_dir = self.stack.last().ok_or(StackError::StackEmpty)?;
        let resolved_file_path = base_dir.join(file_path);

        // 获取文件的父目录
        let parent_dir = resolved_file_path.parent()
            .ok_or_else(|| StackError::NoParentDirectory(resolved_file_path.clone()))?;

        // 规范化父目录的路径并压入栈中
        let absolute_parent_path = fs::canonicalize(parent_dir)?;
        self.stack.push(absolute_parent_path);

        Ok(())
    }

    /// 从栈顶弹出一个目录。
    pub fn pop(&mut self) -> Option<PathBuf> {
        if self.stack.len() > 1 {
            self.stack.pop()
        } else {
            None
        }
    }

    /// 将给定的路径（相对或绝对）翻译成一个绝对路径。
    pub fn translate(&self, path: &Path) -> Result<PathBuf, StackError> {
        let base_dir = self.stack.last().ok_or(StackError::StackEmpty)?;
        let resolved_path = base_dir.join(path);
        let absolute_path = fs::canonicalize(resolved_path)?;
        Ok(absolute_path)
    }

    /// 返回对栈顶当前目录的引用。
    pub fn current_base(&self) -> Option<&Path> {
        self.stack.last().map(|p| p.as_path())
    }
}