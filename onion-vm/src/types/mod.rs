//! Onion 运行时核心类型模块。
//!
//! 本模块聚合了 Onion 语言虚拟机的所有核心数据类型，
//! 包括对象系统、元组、键值对、lambda 表达式、惰性集合、
//! 线程与异步句柄等。
//!
//! # 子模块
//! - `object`：通用对象系统与类型枚举
//! - `tuple`：元组类型与相关操作
//! - `pair`：键值对类型
//! - `lambda`：lambda 表达式定义与执行
//! - `lazy_set`：惰性集合与过滤机制
//! - `thread_handle`：线程句柄类型
//! - `async_handle`：异步任务句柄类型
//!
//! 这些类型共同构成 Onion 运行时的类型系统与数据结构基础。

pub mod async_handle;
pub mod lambda;
pub mod lazy_set;
pub mod object;
pub mod pair;
pub mod thread_handle;
pub mod tuple;
