//! Onion 虚拟机调度器模块。
//!
//! 包含三种调度器：
//! - `scheduler`：通用基于栈的任务调度器，支持嵌套与协作式任务。
//! - `async_scheduler`：异步/协作式多任务调度器，支持优先级与动态降级。
//! - `map_scheduler`：用于 map 操作的专用调度器，支持容器元素批量处理。

pub mod scheduler;
pub mod async_scheduler;
pub mod map_scheduler;