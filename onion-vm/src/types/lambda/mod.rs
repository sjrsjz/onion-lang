//! Onion Lambda 表达式与虚拟机相关模块。
//!
//! 本模块包含 Onion 语言中 Lambda 表达式的定义、参数、执行、
//! 虚拟机指令集等核心实现。
//!
//! # 子模块
//! - `context`：Lambda 执行上下文与作用域管理
//! - `definition`：Lambda 类型与定义结构
//! - `launcher`：Lambda 启动器与运行环境
//! - `native`：原生函数适配与扩展
//! - `parameter`：参数结构与约束系统
//! - `runnable`：可执行对象与虚拟机主循环
//! - `vm_instructions`：虚拟机指令集与操作码
//!
//! 这些模块共同实现了 Onion 语言的高阶函数与运行时执行能力。

pub mod context;
pub mod definition;
pub mod launcher;
pub(crate) mod native;
pub mod parameter;
pub mod runnable;
pub mod vm_instructions;
