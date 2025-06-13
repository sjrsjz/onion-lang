use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
    sync::Arc,
};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::vm_instructions::instruction_set::VMInstructionPackage,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
};

use super::runnable::OnionLambdaRunnable;

pub enum LambdaBody {
    Instruction(Arc<VMInstructionPackage>),
    NativeFunction(Box<dyn Runnable>),
}

impl Clone for LambdaBody {
    fn clone(&self) -> Self {
        match self {
            LambdaBody::Instruction(instruction) => LambdaBody::Instruction(instruction.clone()),
            LambdaBody::NativeFunction(native_function) => {
                LambdaBody::NativeFunction(native_function.copy())
            }
        }
    }
}

impl Debug for LambdaBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LambdaBody::Instruction(_) => write!(f, "Instruction(...)"),
            LambdaBody::NativeFunction(_) => write!(f, "NativeFunction"),
        }
    }
}

impl Display for LambdaBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LambdaBody::Instruction(_) => write!(f, "Instruction(...)"),
            LambdaBody::NativeFunction(_) => write!(f, "NativeFunction"),
        }
    }
}

#[derive(Clone)]
pub struct OnionLambdaDefinition {
    pub(crate) parameter: Box<OnionObject>,
    pub(crate) body: LambdaBody,
    pub(crate) capture: Box<OnionObject>,
    pub(crate) self_object: Box<OnionObject>,
    pub(crate) signature: String,
}

impl OnionLambdaDefinition {
    pub fn new_static(
        parameter: &OnionStaticObject,
        body: LambdaBody,
        capture: Option<&OnionStaticObject>,
        self_object: Option<&OnionStaticObject>,
        signature: String,
    ) -> OnionStaticObject {
        OnionObject::Lambda(Box::new(OnionLambdaDefinition {
            parameter: Box::new(parameter.weak().clone()),
            body,
            capture: Box::new(match capture {
                Some(capture) => capture.weak().clone(),
                None => OnionObject::Undefined(None),
            }),
            self_object: Box::new(match self_object {
                Some(self_object) => self_object.weak().clone(),
                None => OnionObject::Undefined(None),
            }),
            signature,
        }))
        .stabilize()
    }

    pub fn create_runnable(
        &self,
        argument: OnionStaticObject,
        this_lambda: &OnionStaticObject,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<Box<dyn Runnable>, RuntimeError> {
        match &self.body {
            LambdaBody::Instruction(instruction) => {
                let runnable = OnionLambdaRunnable::new(
                    argument,
                    self.self_object.as_ref(),
                    this_lambda,
                    instruction.clone(),
                    match instruction.get_table().get(&self.signature) {
                        Some(ip) => *ip as isize,
                        None => {
                            return Err(RuntimeError::InvalidOperation(format!(
                                "Signature '{}' not found in instruction package",
                                self.signature
                            )));
                        }
                    },
                )?;
                Ok(Box::new(runnable))
            }
            LambdaBody::NativeFunction(native_function) => {
                let mut runnable = native_function.copy();
                runnable.receive(&StepResult::Return(argument.into()), gc)?;
                Ok(runnable)
            }
        }
    }

    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.parameter.upgrade(collected);
        self.capture.upgrade(collected);
        self.self_object.upgrade(collected);
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        match key {
            OnionObject::String(s) if s.as_str() == "parameter" => f(&self.parameter),
            OnionObject::String(s) if s.as_str() == "capture" => f(&self.capture),
            OnionObject::String(s) if s.as_str() == "self" => f(&self.self_object),
            OnionObject::String(s) if s.as_str() == "signature" => {
                f(&OnionObject::String(Arc::new(self.signature.clone())))
            }
            _ => Err(RuntimeError::InvalidOperation(format!(
                "Attribute '{:?}' not found in lambda definition",
                key
            ))),
        }
    }

    pub fn with_attribute_mut<F, R>(&mut self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&mut OnionObject) -> Result<R, RuntimeError>,
    {
        match key {
            OnionObject::String(s) if s.as_str() == "parameter" => f(&mut self.parameter),
            OnionObject::String(s) if s.as_str() == "capture" => f(&mut self.capture),
            OnionObject::String(s) if s.as_str() == "self" => f(&mut self.self_object),
            _ => Err(RuntimeError::InvalidOperation(format!(
                "Attribute '{:?}' not found in lambda definition",
                key
            ))),
        }
    }

    pub fn with_parameter<F, R>(&self, f: F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        f(&self.parameter)
    }
}

impl GCTraceable<OnionObjectCell> for OnionLambdaDefinition {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        self.parameter.collect(queue);
        self.capture.collect(queue);
        self.self_object.collect(queue);
    }
}

impl Debug for OnionLambdaDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "OnionLambdaDefinition {{ parameter: {:?}, body: {:?}, capture: {:?}, self_object: {:?} }}",
            self.parameter, self.body, self.capture, self.self_object
        )
    }
}
