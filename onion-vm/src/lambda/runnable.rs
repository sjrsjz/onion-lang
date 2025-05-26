use crate::types::object::{ObjectError, OnionStaticObject};

#[derive(Clone, Debug)]
pub enum RuntimeError {
    StepError(String),
    DetailedError(String),
    ObjectError(ObjectError)
}

pub enum StepResult {
    Continue,
    NewRunnable(Box<dyn Runnable>),
    Return(OnionStaticObject),
    Error(RuntimeError),
}

pub trait Runnable {
    fn set_argument(&mut self, argument: OnionStaticObject) -> Result<(), ObjectError> {
        Err(ObjectError::InvalidOperation("set_argument not implemented".to_string()))
    }
    fn step(&mut self) -> Result<StepResult, RuntimeError>;
    fn receive(&mut self, step_result: StepResult) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError("receive not implemented".to_string()))
    }
    fn copy(&self) -> Box<dyn Runnable>;
}