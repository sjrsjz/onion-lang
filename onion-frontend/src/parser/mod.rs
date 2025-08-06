//! 语法分析器模块：Onion 语言的完整语法分析工具链。
//!
//! 本模块包含了 Onion 语言编译器前端的所有语法分析组件，从词法分析到语义分析，
//! 提供了完整的源代码处理流水线。
//!
//! # 模块架构
//!
//! ## 核心模块
//! - **lexer**：词法分析器，将源代码字符流转换为 Token 序列
//! - **ast**：抽象语法树模块，定义 AST 节点和语法解析逻辑
//! - **analyzer**：语义分析器，进行变量作用域和语义检查
//! - **comptime**：编译时求值模块，处理编译期计算和元编程
//!
//! ## 源码表示
//! - **Source**：统一的源码表示结构，支持文件和内存中的代码
//!
//! # 处理流程
//! 1. **词法分析**：源码 → Token 流
//! 2. **语法分析**：Token 流 → AST
//! 3. **编译时求值**：处理 `@` 表达式和元编程特性
//! 4. **语义分析**：AST → 处理过变量捕获等信息的 AST

use std::{
    fmt::{Debug, Display},
    ops::Deref,
    path::Path,
    sync::Arc,
};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// 语义分析器模块。
///
/// 负责语义检查、变量作用域分析、类型推导等高级语言特性的处理。
pub mod analyzer;

/// 抽象语法树模块。
///
/// 定义 AST 节点结构和语法解析逻辑，将 Token 流转换为结构化的语法树。
pub mod ast;

/// 编译时求值模块。
///
/// 处理编译期计算、元编程、`@` 表达式等编译时特性。
pub mod comptime;

/// 词法分析器模块。
///
/// 将源代码字符流分解为 Token 序列，识别关键字、标识符、字面量等词法单元。
pub mod lexer;

/// 源代码表示结构。
///
/// 统一的源代码抽象，支持从文件、字符串等多种来源加载代码，
/// 并提供完整的源码位置追踪和错误报告能力。
///
/// # 设计特点
///
/// ## 内存共享
/// 使用 `Arc<Vec<char>>` 来共享源码内容，避免重复拷贝，
/// 多个 Token 和 AST 节点可以安全地引用同一份源码。
///
/// ## 路径追踪
/// 可选的文件路径信息，用于错误报告和调试时显示文件来源。
///
/// ## Unicode 友好
/// 内部使用 `Vec<char>` 存储，完整支持 Unicode 字符，
/// 便于处理多语言源码和字符边界计算。
///
/// ## 序列化支持
/// 实现了 Serde 序列化，支持 AST 的持久化存储和网络传输。
///
/// # 使用示例
/// ```ignore
/// // 从字符串创建
/// let source = Source::from_string("let x = 42".to_string());
///
/// // 从文件创建
/// let source = Source::from_file("example.onion")?;
///
/// // 带路径信息创建
/// let source = Source::from_string_with_file_path(
///     "let y = x + 1".to_string(),
///     "inline.onion"
/// );
/// ```
#[derive(Clone)]
pub struct Source {
    /// 源代码内容，以字符向量形式存储。
    ///
    /// 使用 `Arc` 进行共享，避免在多个位置重复存储相同的源码内容。
    /// 使用 `Vec<char>` 而非 `String` 以便于 Unicode 字符的索引和边界处理。
    content: Arc<Vec<char>>,

    /// 可选的文件路径信息。
    ///
    /// 当源码来自文件时包含文件路径，用于错误报告和调试信息显示。
    /// 对于内存中的代码片段，此字段为 `None`。
    file_path: Option<Arc<Path>>,
}

impl Serialize for Source {
    /// 序列化 Source 为字符串。
    ///
    /// 只序列化源码内容，忽略文件路径信息。这样可以避免路径信息在不同环境间的兼容性问题，同时减少序列化数据的大小。
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
    /// 从字符串反序列化 Source。
    ///
    /// 将序列化的字符串转换回 Source 结构，文件路径信息设置为 `None`，
    /// 因为序列化时未包含路径信息。
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
    /// 从字符串创建 Source。
    ///
    /// 创建不包含文件路径信息的源码实例，适用于内存中的代码片段、
    /// 动态生成的代码或测试用例。
    ///
    /// # 参数
    /// - `source`：源代码字符串
    ///
    /// # 返回
    /// 新的 Source 实例
    ///
    /// # 示例
    /// ```ignore
    /// let source = Source::from_string("let x = 42".to_string());
    /// ```
    pub fn from_string(source: String) -> Self {
        Self {
            content: Arc::new(source.chars().collect()),
            file_path: None,
        }
    }

    /// 从字符串和文件路径创建 Source。
    ///
    /// 创建包含文件路径信息的源码实例，适用于需要保留来源信息的场景，
    /// 如编辑器插件、调试器或错误报告。
    ///
    /// # 参数
    /// - `source`：源代码字符串
    /// - `path`：文件路径，可以是任何可转换为 `Path` 的类型
    ///
    /// # 返回
    /// 新的 Source 实例，包含路径信息
    ///
    /// # 示例
    /// ```ignore
    /// let source = Source::from_string_with_file_path(
    ///     "let y = x + 1".to_string(),
    ///     "math.onion"
    /// );
    /// ```
    pub fn from_string_with_file_path<P: AsRef<Path>>(source: String, path: P) -> Self {
        Self {
            content: Arc::new(source.chars().collect()),
            file_path: Some(Arc::from(path.as_ref())),
        }
    }

    /// 从文件创建 Source。
    ///
    /// 读取文件内容并创建 Source 实例，自动设置文件路径信息。
    /// 这是从磁盘文件加载源码的标准方法。
    ///
    /// # 参数
    /// - `path`：文件路径，可以是任何可转换为 `Path` 的类型
    ///
    /// # 返回
    /// - `Ok(Source)`：成功时返回包含文件内容和路径的 Source
    /// - `Err(std::io::Error)`：文件读取失败时返回 IO 错误
    ///
    /// # 错误
    /// 当文件不存在、权限不足或读取过程中发生 IO 错误时返回错误。
    ///
    /// # 示例
    /// ```ignore
    /// let source = Source::from_file("example.onion")?;
    /// ```
    pub fn from_file<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let path_ref = path.as_ref();
        let content_string = std::fs::read_to_string(path_ref)?;
        Ok(Self {
            content: Arc::new(content_string.chars().collect()),
            file_path: Some(Arc::from(path_ref)),
        })
    }

    /// 获取文件路径。
    ///
    /// 返回源码的文件路径，如果源码不是来自文件则返回 `None`。
    /// 主要用于错误报告和调试信息显示。
    ///
    /// # 返回
    /// - `Some(&Path)`：如果源码来自文件
    /// - `None`：如果源码来自内存字符串
    pub fn file_path(&self) -> Option<&Path> {
        self.file_path.as_deref()
    }

    /// 获取源码内容的共享引用。
    ///
    /// 返回内部字符向量的 Arc 引用，允许多个地方共享同一份源码内容。
    /// 主要供内部模块使用，如词法分析器和 AST 节点。
    ///
    /// # 返回
    /// 源码字符向量的共享引用
    pub fn content(&self) -> &Arc<Vec<char>> {
        &self.content
    }

    /// 获取源码内容的字符串表示。
    ///
    /// 将内部的字符向量转换为字符串，主要用于显示、打印或与
    /// 需要字符串的 API 交互。
    ///
    /// # 返回
    /// 源码的字符串表示
    pub fn content_str(&self) -> String {
        self.content.iter().collect()
    }
}

impl Debug for Source {
    /// Debug 格式化输出。
    ///
    /// 提供源码的调试信息，包括文件路径（或标记为内存中）和
    /// 内容的前 40 个字符预览。有助于调试和日志输出。
    ///
    /// # 输出格式
    /// `Source(file: "path/to/file.onion", content: "let x = 42...")`
    ///
    /// 对于内存中的源码：
    /// `Source(file: "<in-memory>", content: "let x = 42...")`
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
    /// Display 格式化输出。
    ///
    /// 提供简洁的源码标识，主要用于用户友好的错误消息和日志输出。
    /// 只显示文件路径或匿名标记，不包含源码内容。
    ///
    /// # 输出格式
    /// - 有文件路径：`/path/to/file.onion`
    /// - 无文件路径：`<anonymous>`
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.file_path {
            Some(path) => write!(f, "{}", path.display()),
            None => write!(f, "<anonymous>"),
        }
    }
}

impl Deref for Source {
    type Target = Arc<Vec<char>>;

    /// 解引用到内部的字符向量。
    ///
    /// 允许 Source 像 `Arc<Vec<char>>` 一样使用，提供对字符向量的直接访问。
    /// 这使得可以直接在 Source 上使用索引、切片等操作。
    ///
    /// # 示例
    /// ```ignore
    /// let source = Source::from_string("hello".to_string());
    /// let first_char = source[0];  // 等价于 source.content()[0]
    /// let slice = &source[1..3];   // 等价于 &source.content()[1..3]
    /// ```
    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl From<String> for Source {
    /// 从 String 转换为 Source。
    ///
    /// 提供便捷的转换方法，等价于 `Source::from_string()`。
    fn from(source: String) -> Self {
        Self::from_string(source)
    }
}

impl From<&str> for Source {
    /// 从字符串切片转换为 Source。
    ///
    /// 提供便捷的转换方法，会将字符串切片复制为 String 再创建 Source。
    fn from(source: &str) -> Self {
        Self::from_string(source.to_string())
    }
}
