use std::{fmt::{Debug, Display}, ops::Deref, path::Path, sync::Arc};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

pub mod analyzer;
pub mod ast;
pub mod comptime;
pub mod lexer;

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

impl Serialize for Source {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // 我们只关心 content。将 Vec<char> 转换成 String
        let content_str: String = self.content.iter().collect();
        // 直接序列化这个 String
        serializer.serialize_str(&content_str)
    }
}

impl<'de> Deserialize<'de> for Source {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let content_str = String::deserialize(deserializer)?;

        Ok(Source {
            content: Arc::new(content_str.chars().collect()),
            file_path: None,
        })
    }
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
