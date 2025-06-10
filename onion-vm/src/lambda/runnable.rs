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
    CustomValue(OnionStaticObject),
    BrokenReference,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::StepError(msg) => write!(f, "Step Error: {}", msg),
            RuntimeError::DetailedError(msg) => write!(f, "Detailed Error: {}", msg),
            RuntimeError::InvalidType(msg) => write!(f, "Invalid type: {}", msg),
            RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
            RuntimeError::BrokenReference => write!(f, "Broken reference encountered"),
            RuntimeError::CustomValue(value) => write!(f, "Custom value error: {}", value),
        }
    }
}

pub enum StepResult {
    Continue,
    NewRunnable(Box<dyn Runnable>),
    ReplaceRunnable(Box<dyn Runnable>),
    Return(OnionStaticObject),
}

#[allow(unused_variables)]
pub trait Runnable {
    fn set_argument(
        &mut self,
        argument: OnionStaticObject,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "set_argument not implemented".to_string(),
        ))
    }
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> Result<StepResult, RuntimeError>;
    fn receive(
        &mut self,
        step_result: StepResult,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "receive not implemented".to_string(),
        ))
    }
    fn copy(&self, gc: &mut GC<OnionObjectCell>) -> Box<dyn Runnable>;

    fn format_context(&self) -> Result<Value, RuntimeError>;
}
