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