use std::env;
use std::path::PathBuf;
use std::io; // Import io for error handling

#[derive(Debug, Clone)]
pub struct DirStack {
    stack: Vec<PathBuf>, // Use PathBuf for better path handling
}

impl DirStack {
    /// Creates a new DirStack, initializing it with the current working directory.
    pub fn new(path: Option<&PathBuf>) -> io::Result<Self> {
        let initial_dir = match path {
            Some(p) => p.clone(),
            None => env::current_dir().map_err(|e| {
                io::Error::new(
                    e.kind(),
                    format!("Failed to get current directory: {}", e),
                )
            })?,
        };

        // Ensure the initial directory is canonical and exists
        let canonical_path = initial_dir.canonicalize().map_err(
            |e| {
                io::Error::new(
                    e.kind(),
                    format!("Failed to find or access directory '{}': {}", initial_dir.display(), e),
                )
            },
        )?;
        if !canonical_path.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("Path '{}' is not a directory", canonical_path.display()),
            ));
        }

        env::set_current_dir(&canonical_path).map_err(|e| {
            io::Error::new(
                e.kind(),
                format!("Failed to set current directory to '{}': {}", canonical_path.display(), e),
            )
        })? ; // Set the current working directory
        Ok(Self {
            stack: vec![canonical_path], // Initialize stack with the canonical path
        })
    }

    /// Pushes a new directory onto the stack and changes the current working directory.
    /// The `dir` path is interpreted relative to the *current* directory on the stack.
    pub fn push(&mut self, dir: &str) -> io::Result<()> {
        let target_dir = if let Some(current) = self.stack.last() {
             // Resolve the new path relative to the current top of the stack
             current.join(dir)
        } else {
             // Should not happen if initialized correctly, but handle defensively
             PathBuf::from(dir)
        };

        // Attempt to canonicalize to get an absolute path and verify existence
        let canonical_path = match target_dir.canonicalize() {
             Ok(p) => p,
             Err(e) => {
                 // Provide more context on error
                 return Err(io::Error::new(
                     e.kind(),
                     format!("(Push)Failed to find or access directory '{}': {}", target_dir.display(), e),
                 ));
             }
        };

        // Check if it's actually a directory
        if !canonical_path.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("(Push)Path '{}' is not a directory", canonical_path.display()),
            ));
        }


        env::set_current_dir(&canonical_path).map_err(
            |e| io::Error::new(
                e.kind(),
                format!("(Push)Failed to set current directory to '{}': {}", canonical_path.display(), e),
            )
        )?; // Change the working directory
        self.stack.push(canonical_path); // Push the canonical path onto the stack
        Ok(())
    }

    pub fn push_file(&mut self, file: &str) -> io::Result<()> {
        // Use the parent directory of the file to push onto the stack
        let target_dir = if let Some(current) = self.stack.last() {
            current.join(file).parent().unwrap_or(current).to_path_buf()
        } else {
            PathBuf::from(file)
        };

        // Call push with the directory of the file
        self.push(target_dir.to_str().unwrap_or(""))
    }

    /// Pops the current directory from the stack and changes the working directory
    /// back to the previous one. Returns the directory popped, or None if only
    /// the initial directory remains.
    pub fn pop(&mut self) -> io::Result<Option<PathBuf>> {
        if self.stack.len() <= 1 {
            // Cannot pop the initial directory
            return Ok(None);
        }

        let popped_dir = self.stack.pop(); // Remove the top directory

        if let Some(previous_dir) = self.stack.last() {
            env::set_current_dir(previous_dir).map_err(|e| {
                io::Error::new(
                    e.kind(),
                    format!("(Pop)Failed to set current directory to '{}': {}", previous_dir.display(), e),
                )
            })?; // Change the working directory to the new top of the stack
        }
        // If stack somehow became empty (shouldn't happen with the check above),
        // we don't change dir, but still return the popped one.

        Ok(popped_dir)
    }

    /// 根据给定的文件路径获得绝对路径
    /// 如果路径是相对路径，则相对于栈顶目录解析
    /// 如果路径已经是绝对路径，则直接返回规范化后的路径
    pub fn get_absolute_path(&self, path: &str) -> io::Result<PathBuf> {
        let target_path = PathBuf::from(path);
        
        let resolved_path = if target_path.is_absolute() {
            // 已经是绝对路径，直接使用
            target_path
        } else {
            // 相对路径，基于栈顶目录解析
            if let Some(current) = self.stack.last() {
                current.join(path)
            } else {
                // 栈为空时（不应该发生），使用当前工作目录
                env::current_dir()
                    .map_err(|e| io::Error::new(e.kind(), format!("Failed to get current directory: {}", e)))?
                    .join(path)
            }
        };

        // 规范化路径并验证存在性
        resolved_path.canonicalize().map_err(|e| {
            io::Error::new(
                e.kind(),
                format!("Failed to resolve path '{}': {}", resolved_path.display(), e),
            )
        })
    }
}

// Optional: Add a default implementation
impl Default for DirStack {
    fn default() -> Self {
        // In Default, we might panic on error or return a basic state
        // For simplicity here, we'll use "." if getting current_dir fails.
        let initial_dir = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        Self {
            stack: vec![initial_dir],
        }
    }
}