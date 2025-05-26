use std::{cell::{Ref, RefCell}, sync::Arc};

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::object::OnionStaticObject,
};

use super::{context::Context, vm_instructions::instruction_set::VMInstructionPackage};

pub struct OnionLambdaRunnable {
    pub(crate) argument: OnionStaticObject,
    pub(crate) context: Context,
    pub(crate) result: OnionStaticObject,
    pub(crate) error: Option<RuntimeError>,
    pub(crate) ip: isize, // Instruction pointer
    pub(crate) instruction: Arc<RefCell<VMInstructionPackage>>,
}

impl OnionLambdaRunnable {
    pub fn borrow_instruction(&self) -> Result<Ref<VMInstructionPackage>, RuntimeError> {
        self.instruction
            .try_borrow()
            .map_err(|_| RuntimeError::DetailedError("Failed to borrow instruction".to_string()))
    }
}

impl Runnable for OnionLambdaRunnable {
    fn receive(&mut self, step_result: StepResult) -> Result<(), RuntimeError> {
        if let StepResult::Return(result) = step_result {
            self.context.push_object(result)?;
            Ok(())
        } else {
            Err(RuntimeError::DetailedError(
                "receive not implemented for cases except `Return`".to_string(),
            ))
        }
    }
    fn step(&mut self) -> Result<StepResult, RuntimeError> {
        if let Some(error) = &self.error {
            return Ok(StepResult::Error(error.clone()));
        }


        Ok(StepResult::Continue)
    }
}
