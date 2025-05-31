use std::fmt::Display;

use arc_gc::gc::GC;

use crate::types::object::{ObjectError, OnionObject, OnionObjectCell, OnionStaticObject};

#[derive(Clone, Debug)]
pub enum RuntimeError {
    StepError(String),
    DetailedError(String),
    ObjectError(ObjectError),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::StepError(msg) => write!(f, "Step Error: {}", msg),
            RuntimeError::DetailedError(msg) => write!(f, "Detailed Error: {}", msg),
            RuntimeError::ObjectError(err) => write!(f, "Object Error: {}", err),
        }
    }
}

pub enum StepResult {
    Continue,
    NewRunnable(Box<dyn Runnable>),
    Return(OnionStaticObject),
    Error(RuntimeError),
}

#[allow(unused_variables)]
pub trait Runnable {
    fn set_argument(
        &mut self,
        argument: OnionStaticObject,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), ObjectError> {
        Err(ObjectError::InvalidOperation(
            "set_argument not implemented".to_string(),
        ))
    }
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> Result<StepResult, RuntimeError>;
    fn receive(&mut self, step_result: StepResult, gc: &mut GC<OnionObjectCell>) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "receive not implemented".to_string(),
        ))
    }
    fn copy(&self, gc: &mut GC<OnionObjectCell>) -> Box<dyn Runnable>;
}
