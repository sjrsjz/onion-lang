//! Onion 语言前端主库。
//!
//! 本 crate 提供 Onion 语言的完整前端实现，包括：
//! - 词法分析（lexer）
//! - 语法分析（parser/ast）
//! - 语义分析（analyzer）
//! - 编译时求值与宏系统（comptime）
//! - IR 生成与优化（ir_generator）
//! - 诊断与错误报告（diagnostics）
//! - 编译主流程（compile）
//! - 实用工具（utils）
//!
//! # 主要模块
//! - `ir_generator`：IR 生成与命名空间管理
//! - `parser`：词法、语法、语义分析与源码管理
//! - `diagnostics`：错误与警告收集、源码定位
//! - `compile`：一站式编译入口，源码到字节码
//! - `utils`：通用辅助工具（如循环检测等）
//!
//! # 用法示例
//! ```ignore
//! use onion_frontend::compile::build_code;
//! use onion_frontend::parser::Source;
//! let source = Source::from_file("main.onion").unwrap();
//! let mut collector = ...;
//! let ir_package = build_code(&mut collector, &source).unwrap();
//! ```

pub mod compile;
pub mod diagnostics;
pub mod ir_generator;
pub mod parser;
pub mod utils;
