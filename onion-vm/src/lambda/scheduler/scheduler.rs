use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::object::OnionStaticObject,
};

pub struct Scheduler {
    pub(crate) argument: OnionStaticObject,
    pub(crate) runnable_stack: Vec<Box<dyn Runnable>>,
    pub(crate) result: OnionStaticObject,
}

impl Scheduler {
    pub fn new(argument: OnionStaticObject) -> Self {
        Scheduler {
            argument,
            runnable_stack: Vec::new(),
            result: OnionStaticObject::default(),
        }
    }
}

impl Runnable for Scheduler {
    fn step(&mut self) -> Result<StepResult, RuntimeError> {
        if let Some(runnable) = self.runnable_stack.last_mut() {
            match runnable.step()? {
                StepResult::Continue => Ok(StepResult::Continue),
                StepResult::NewRunnable(new_runnable) => {
                    self.runnable_stack.push(new_runnable);
                    Ok(StepResult::Continue)
                }
                StepResult::Return(result) => {
                    self.runnable_stack.pop();
                    if let Some(top_runnable) = self.runnable_stack.last_mut() {
                        top_runnable.receive(StepResult::Return(result.clone()))?;
                        Ok(StepResult::Continue)
                    } else {
                        self.result = result;
                        Ok(StepResult::Return(self.result.clone()))
                    }
                }
                StepResult::Error(error) => Ok(StepResult::Error(error)),
            }
        } else {
            Ok(StepResult::Return(self.result.clone()))
        }
    }
    fn receive(&mut self, step_result: StepResult) -> Result<(), RuntimeError> {
        if let Some(runnable) = self.runnable_stack.last_mut() {
            runnable.receive(step_result)
        } else {
            Err(RuntimeError::DetailedError(
                "No runnable in stack".to_string(),
            ))
        }
    }
}
