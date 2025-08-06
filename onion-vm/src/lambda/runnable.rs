
//! Onion 虚拟机运行时调度核心 trait 和类型定义。
//! 
//! 包含任务调度的错误类型、步骤结果类型、调度 trait 及辅助宏。
use std::fmt::Display;

use arc_gc::gc::GC;

use crate::{
    lambda::scheduler::async_scheduler::Task,
    types::object::{OnionObjectCell, OnionStaticObject},
};


/// 虚拟机运行时错误类型。
/// 
/// 用于描述调度和执行过程中可能出现的各种错误。
#[derive(Clone, Debug)]
pub enum RuntimeError {
    /// 当前指令需要重复检查直到条件满足（如异步等待）
    Pending,
    /// 引用失效或悬空
    BrokenReference,
    /// 步骤执行错误，带详细信息
    StepError(Box<str>),
    /// 详细错误信息
    DetailedError(Box<str>),
    /// 类型错误
    InvalidType(Box<str>),
    /// 非法操作
    InvalidOperation(Box<str>),
    /// 自定义错误值（可携带任意对象）
    CustomValue(Box<OnionStaticObject>),
    /// 借用相关错误
    BorrowError(Box<str>),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Pending => write!(f, "Pending: The operation is not yet complete"),
            RuntimeError::StepError(msg) => write!(f, "Step Error: {}", msg),
            RuntimeError::DetailedError(msg) => write!(f, "{}", msg),
            RuntimeError::InvalidType(msg) => write!(f, "Invalid type: {}", msg),
            RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
            RuntimeError::BrokenReference => write!(f, "Broken reference encountered"),
            RuntimeError::BorrowError(msg) => write!(f, "Borrow error: {}", msg),
            RuntimeError::CustomValue(value) => write!(f, "Custom value error: {}", value),
        }
    }
}


/// 步骤执行结果类型。
/// 
/// 用于描述一次 step 调用后的调度决策。
pub enum StepResult {
    /// 继续当前任务
    Continue,
    /// 生成一个新的 Runnable 并压栈
    NewRunnable(Box<dyn Runnable>),
    /// 替换当前 Runnable
    ReplaceRunnable(Box<dyn Runnable>),
    /// 生成一个新的异步任务（如协程）
    SpawnRunnable(Box<Task>),
    /// 返回结果并结束当前任务
    Return(Box<OnionStaticObject>),
    /// 发生错误
    Error(RuntimeError),
}

/// 辅助宏：用于简化 Result 到 StepResult 的转换。
/// 
/// 若 Result 为 Ok，返回值；若为 Err，直接返回 StepResult::Error。
#[macro_export]
macro_rules! unwrap_step_result {
    ($result:expr) => {
        match $result {
            Ok(value) => value,
            Err(error) => return StepResult::Error(error),
        }
    };
}


/// Onion 虚拟机调度核心 trait。
/// 
/// 所有可调度对象需实现 Runnable trait，支持 step、receive、format_context 三大接口。
pub trait Runnable: Send + Sync + 'static {
    /// 推进任务执行一步。
    /// 
    /// # 参数
    /// * `gc` - 垃圾收集器引用
    /// 
    /// # 返回值
    /// 返回 StepResult，指示调度器下一步动作
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult;

    /// 接收子任务的结果。
    /// 
    /// # 参数
    /// * `step_result` - 子任务的执行结果
    /// * `gc` - 垃圾收集器引用
    /// 
    /// # 返回值
    /// * Ok(()) - 成功处理
    /// * Err(RuntimeError) - 默认未实现
    #[allow(unused_variables)]
    fn receive(
        &mut self,
        step_result: &StepResult,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "receive not implemented".into(),
        ))
    }

    /// 格式化当前任务的上下文信息，便于调试。
    fn format_context(&self) -> String;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_step_result_size() {
        println!("Size of StepResult: {}", std::mem::size_of::<StepResult>());
    }
}
