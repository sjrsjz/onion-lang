use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
};

use super::runnable::OnionLambdaRunnable;

pub enum LambdaBody {
    Instruction(Box<OnionObject>),
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
            LambdaBody::Instruction(instruction) => write!(f, "Instruction({:?})", instruction),
            LambdaBody::NativeFunction(_) => write!(f, "NativeFunction"),
        }
    }
}

impl Display for LambdaBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LambdaBody::Instruction(instruction) => write!(f, "Instruction({:?})", instruction),
            LambdaBody::NativeFunction(_) => write!(f, "NativeFunction"),
        }
    }
}

#[derive(Clone)]
pub struct OnionLambdaDefinition {
    pub(crate) parameter: Box<OnionObjectCell>,
    pub(crate) body: LambdaBody,
    pub(crate) capture: Box<OnionObjectCell>,
    pub(crate) self_object: Box<OnionObjectCell>,
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
                None => OnionObject::Undefined(None).to_cell(),
            }),
            self_object: Box::new(match self_object {
                Some(self_object) => self_object.weak().clone(),
                None => OnionObject::Undefined(None).to_cell(),
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
                let runnable = instruction.with_data(|instruction| match instruction {
                    OnionObject::InstructionPackage(package) => OnionLambdaRunnable::new(
                        argument,
                        self.capture.clone().stabilize(),
                        self.self_object.clone().stabilize(),
                        this_lambda.clone(),
                        package.clone(),
                        match package.get_table().get(&self.signature) {
                            Some(ip) => *ip as isize,
                            None => {
                                return Err(RuntimeError::InvalidOperation(format!(
                                    "Signature '{}' not found in instruction package",
                                    self.signature
                                )));
                            }
                        },
                    ),
                    _ => {
                        return Err(RuntimeError::InvalidOperation(
                            "Lambda body must be an instruction package".to_string(),
                        ));
                    }
                })?;
                Ok(Box::new(runnable))
            }
            LambdaBody::NativeFunction(native_function) => {
                let mut runnable = native_function.copy();
                runnable.receive(StepResult::Return(argument), gc)?;
                Ok(runnable)
            }
        }
    }

    pub fn upgrade(&self) -> Option<Vec<GCArc<OnionObjectCell>>> {
        let mut arcs = Vec::new();
        if let Some(param_arcs) = self.parameter.upgrade() {
            arcs.extend(param_arcs);
        }
        if let Some(capture_arcs) = self.capture.upgrade() {
            arcs.extend(capture_arcs);
        }
        if let Some(self_object_arcs) = self.self_object.upgrade() {
            arcs.extend(self_object_arcs);
        }
        match &self.body {
            LambdaBody::Instruction(instruction) => {
                if let Some(instruction_arcs) = instruction.upgrade() {
                    arcs.extend(instruction_arcs);
                }
            }
            LambdaBody::NativeFunction(_) => {}
        }
        if arcs.is_empty() {
            None
        } else {
            Some(arcs)
        }
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        match key {
            OnionObject::String(s) if s.as_str() == "parameter" => {
                f(&*self.parameter.try_borrow()?)
            }
            OnionObject::String(s) if s.as_str() == "capture" => f(&*self.capture.try_borrow()?),
            OnionObject::String(s) if s.as_str() == "self" => f(&*self.self_object.try_borrow()?),
            OnionObject::String(s) if s.as_str() == "signature" => {
                f(&OnionObject::String(self.signature.clone()))
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
            OnionObject::String(s) if s.as_str() == "parameter" => {
                f(&mut *self.parameter.try_borrow_mut()?)
            }
            OnionObject::String(s) if s.as_str() == "capture" => {
                f(&mut *self.capture.try_borrow_mut()?)
            }
            OnionObject::String(s) if s.as_str() == "self" => {
                f(&mut *self.self_object.try_borrow_mut()?)
            }
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
        f(&*self.parameter.try_borrow()?)
    }
}

impl GCTraceable<OnionObjectCell> for OnionLambdaDefinition {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        self.parameter.collect(queue);
        match &self.body {
            LambdaBody::Instruction(instruction) => instruction.collect(queue),
            LambdaBody::NativeFunction(_) => {}
        }
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
