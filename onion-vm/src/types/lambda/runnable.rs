use std::{
    cell::{Ref, RefCell},
    sync::Arc,
};

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::object::{ObjectError, OnionObject, OnionStaticObject},
};

use super::{
    context::{Context, Frame},
    vm_instructions::instruction_set::VMInstructionPackage,
};

pub struct OnionLambdaRunnable {
    pub(crate) argument: OnionStaticObject,
    pub(crate) context: Context,
    pub(crate) result: OnionStaticObject,
    pub(crate) error: Option<RuntimeError>,
    pub(crate) ip: isize, // Instruction pointer
    pub(crate) instruction: Arc<RefCell<VMInstructionPackage>>,
}

impl OnionLambdaRunnable {
    pub fn new(
        argument: OnionStaticObject,
        instruction: Arc<RefCell<VMInstructionPackage>>,
    ) -> Result<Self, ObjectError> {
        let mut new_context = Context::new();
        Context::push_frame(
            &mut new_context,
            Frame::Normal(std::collections::HashMap::new(), Vec::new()),
        );

        let OnionObject::Tuple(tuple) = argument.weak() else {
            return Err(ObjectError::InvalidOperation(
                "Argument must be a tuple".to_string(),
            ));
        };

        for (_, item) in tuple.elements.iter().enumerate() {
            match item {
                OnionObject::Named(named) => {
                    named.get_key().with_data(|key| match key {
                        OnionObject::String(key_str) => {
                            new_context.let_variable(key_str, named.get_value().clone().stabilize())
                        }
                        _ => Ok(()),
                    })?;
                }
                _ => {}
            }
        }

        Ok(OnionLambdaRunnable {
            argument,
            context: new_context,
            result: OnionStaticObject::default(),
            error: None,
            ip: 0,
            instruction,
        })
    }
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

    fn copy(&self) -> Box<dyn Runnable> {
        Box::new(OnionLambdaRunnable {
            argument: self.argument.clone(),
            context: self.context.clone(),
            result: self.result.clone(),
            error: self.error.clone(),
            ip: self.ip,
            instruction: Arc::clone(&self.instruction),
        })
    }
}
