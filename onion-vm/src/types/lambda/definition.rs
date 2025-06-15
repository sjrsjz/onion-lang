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
    parameter: Arc<OnionObject>,
    body: LambdaBody,
    capture: Arc<OnionObject>,
    self_object: Arc<OnionObject>,
    signature: String,
}

impl OnionLambdaDefinition {
    pub fn new_static(
        parameter: &OnionStaticObject,
        body: LambdaBody,
        capture: Option<&OnionStaticObject>,
        self_object: Option<&OnionStaticObject>,
        signature: String,
    ) -> OnionStaticObject {
        OnionObject::Lambda(
            OnionLambdaDefinition {
                parameter: parameter.weak().clone().into(),
                body,
                capture: (match capture {
                    Some(capture) => capture.weak().clone(),
                    None => OnionObject::Undefined(None),
                })
                .into(),
                self_object: (match self_object {
                    Some(self_object) => self_object.weak().clone(),
                    None => OnionObject::Undefined(None),
                })
                .into(),
                signature,
            }
            .into(),
        )
        .consume_and_stabilize()
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
                            return Err(RuntimeError::InvalidOperation(
                                format!(
                                    "Signature '{}' not found in instruction package",
                                    self.signature
                                )
                                .into(),
                            ));
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

    pub fn get_signature(&self) -> &str {
        &self.signature
    }

    pub fn get_parameter(&self) -> &OnionObject {
        &self.parameter
    }

    pub fn get_capture(&self) -> &OnionObject {
        &self.capture
    }

    pub fn get_self_object(&self) -> &OnionObject {
        &self.self_object
    }

    pub fn get_body(&self) -> &LambdaBody {
        &self.body
    }

    pub fn clone_and_replace_self_object(
        &self,
        new_self_object: &OnionStaticObject,
    ) -> OnionStaticObject {
        let new_definition = OnionLambdaDefinition {
            parameter: self.parameter.clone(),
            body: self.body.clone(),
            capture: self.capture.clone(),
            self_object: new_self_object.weak().clone().into(),
            signature: self.signature.clone(),
        };
        OnionObject::Lambda(new_definition.into()).consume_and_stabilize()
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
            _ => Err(RuntimeError::InvalidOperation(
                format!("Attribute '{:?}' not found in lambda definition", key).into(),
            )),
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

impl OnionLambdaDefinition {
    pub fn reconstruct_container(&self) -> Result<OnionObject, RuntimeError> {
        let parameter = self.parameter.clone();
        let body = self.body.clone();
        let capture = self.capture.clone();
        let self_object = self.self_object.clone();
        let signature = self.signature.clone();
        Ok(OnionObject::Lambda(
            OnionLambdaDefinition {
                parameter: parameter,
                body,
                capture,
                self_object,
                signature,
            }
            .into(),
        ))
    }
}
