//! OnionVM 虚拟机核心库。
//!
//! 本 crate 提供 Onion 语言运行时的核心类型、虚拟机执行引擎、
//! Lambda 表达式、垃圾回收、类型系统与高性能工具集。
//!
//! # 主要模块
//! - `types`：Onion 运行时核心类型系统（对象、元组、lambda、集合等）
//! - `lambda`：抽象可调度匿名函数对象及调度器实现
//! - `utils`：高性能工具与辅助结构（如 FastMap）
//!
//! # 主要导出
//! - `GC`：基于 arc_gc 的垃圾回收器
//! - `GCTraceable`：GC 跟踪 trait
//! - `GCArc`/`GCArcWeak`：GC 智能指针

pub mod types;
pub mod lambda;
pub use arc_gc::gc::GC as GC;
pub use arc_gc::traceable::GCTraceable as GCTraceable;
pub use arc_gc::arc::GCArc as GCArc;
pub use arc_gc::arc::GCArcWeak as GCArcWeak;
pub mod utils;