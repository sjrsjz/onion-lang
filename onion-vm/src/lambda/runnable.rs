use std::fmt::Display;

use arc_gc::gc::GC;

use crate::{
    lambda::scheduler::async_scheduler::Task,
    types::object::{OnionObjectCell, OnionStaticObject},
};

#[derive(Clone, Debug)]
pub enum RuntimeError {
    Pending, // 当前指令需要重复检查直到条件满足，考虑到大部分函数返回值是 `Result`，因此 Pending 只是取代 StepResult::Pending 的一个方式，告诉VM需要
    // 重复检查当前指令直到条件满足
    StepError(Box<String>),
    DetailedError(Box<String>),
    InvalidType(Box<String>),
    InvalidOperation(Box<String>),
    CustomValue(Box<OnionStaticObject>),
    BrokenReference,
    BorrowError(Box<String>),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Pending => write!(f, "Pending: The operation is not yet complete"),
            RuntimeError::StepError(msg) => write!(f, "Step Error: {}", msg),
            RuntimeError::DetailedError(msg) => write!(f, "Detailed Error: {}", msg),
            RuntimeError::InvalidType(msg) => write!(f, "Invalid type: {}", msg),
            RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
            RuntimeError::BrokenReference => write!(f, "Broken reference encountered"),
            RuntimeError::BorrowError(msg) => write!(f, "Borrow error: {}", msg),
            RuntimeError::CustomValue(value) => write!(f, "Custom value error: {}", value),
        }
    }
}

pub enum StepResult {
    Continue,
    NewRunnable(Box<dyn Runnable>),
    ReplaceRunnable(Box<dyn Runnable>),
    SpawnRunnable(Box<Task>),
    Return(Box<OnionStaticObject>),
    SetSelfObject(Box<OnionStaticObject>),
    Error(RuntimeError),
}

impl StepResult {
    pub fn unwrap_error(self) -> RuntimeError {
        match self {
            StepResult::Error(error) => error,
            _ => RuntimeError::StepError(
                "Expected an error, but got a different result"
                    .to_string()
                    .into(),
            ),
        }
    }
}

#[macro_export]
macro_rules! unwrap_step_result {
    ($result:expr) => {
        match $result {
            Ok(value) => value,
            Err(error) => return StepResult::Error(error),
        }
    };
}

#[allow(unused_variables)]
pub trait Runnable: Send + Sync + 'static {
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult;
    fn receive(
        &mut self,
        step_result: &StepResult,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "receive not implemented".to_string().into(),
        ))
    }

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
