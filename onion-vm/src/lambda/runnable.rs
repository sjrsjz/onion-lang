use std::fmt::Display;

use arc_gc::gc::GC;
use serde_json::Value;

use crate::types::object::{OnionObjectCell, OnionStaticObject};

#[derive(Clone, Debug)]
pub enum RuntimeError {
    StepError(String),
    DetailedError(String),
    InvalidType(String),
    InvalidOperation(String),
    CustomValue(Box<OnionStaticObject>),
    BrokenReference,
    BorrowError(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
    Return(Box<OnionStaticObject>),
}

#[allow(unused_variables)]
pub trait Runnable {
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> Result<StepResult, RuntimeError>;
    fn receive(
        &mut self,
        step_result: &StepResult,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "receive not implemented".to_string(),
        ))
    }
    fn copy_with_gc(&self, gc: &mut GC<OnionObjectCell>) -> Box<dyn Runnable> {
        panic!("copy_with_gc is not implemented for this Runnable")
    }
    fn copy(&self) -> Box<dyn Runnable>;

    fn format_context(&self) -> Result<Value, RuntimeError>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_step_result_size() {
        println!("Size of StepResult: {}", std::mem::size_of::<StepResult>());
        println!(
            "Size of Result<StepResult, RuntimeError>: {}",
            std::mem::size_of::<Result<StepResult, RuntimeError>>()
        );
    }
}
