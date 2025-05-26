use std::{cell::RefCell, fmt::Debug, sync::Arc};

use arc_gc::{gc::GC, traceable::GCTraceable};

use crate::{
    lambda::runnable::Runnable,
    types::object::{ObjectError, OnionObject, OnionStaticObject},
};

use super::runnable::OnionLambdaRunnable;

#[derive(Clone)]
pub enum LambdaBody {
    Instruction(Box<OnionObject>),
    NativeFunction(Arc<RefCell<dyn Runnable>>),
}

impl Debug for LambdaBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LambdaBody::Instruction(instruction) => write!(f, "Instruction({:?})", instruction),
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
        OnionObject::Lambda(OnionLambdaDefinition {
            parameter: Box::new(parameter.weak().clone()),
            body,
            capture: Box::new(match capture {
                Some(capture) => capture.weak().clone(),
                None => OnionObject::Undefined("No capture".to_string()),
            }),
            self_object: Box::new(match self_object {
                Some(self_object) => self_object.weak().clone(),
                None => OnionObject::Undefined("No self object".to_string()),
            }),
            signature,
        })
        .stabilize()
    }

    pub fn create_runnable(
        &self,
        argument: OnionStaticObject,
        gc: &mut GC,
    ) -> Result<Box<dyn Runnable>, ObjectError> {
        match &self.body {
            LambdaBody::Instruction(instruction) => match instruction.as_ref() {
                OnionObject::InstructionPackage(package) => {
                    let runnable = OnionLambdaRunnable::new(
                        argument,
                        Arc::new(RefCell::new(package.clone())),
                        match package.get_table().get(&self.signature) {
                            Some(ip) => *ip as isize,
                            None => {
                                return Err(ObjectError::InvalidOperation(format!(
                                    "Signature '{}' not found in instruction package",
                                    self.signature
                                )));
                            }
                        },
                    )?;
                    Ok(Box::new(runnable))
                }
                _ => {
                    return Err(ObjectError::InvalidOperation(
                        "Lambda body must be an instruction package".to_string(),
                    ));
                }
            },
            LambdaBody::NativeFunction(native_function) => {
                let mut runnable = native_function.borrow().copy(gc);
                runnable.set_argument(argument, gc)?;
                Ok(runnable)
            }
        }
    }
}

impl GCTraceable for OnionLambdaDefinition {
    fn visit(&self) {
        self.parameter.visit();
        match &self.body {
            LambdaBody::Instruction(instruction) => instruction.visit(),
            LambdaBody::NativeFunction(_) => (),
        }
        self.capture.visit();
        self.self_object.visit();
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
