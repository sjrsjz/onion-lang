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
    lambda::runnable::{Runnable, RuntimeError},
    types::{
        lambda::{
            parameter::LambdaParameter, vm_instructions::instruction_set::VMInstructionPackage,
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

use super::runnable::OnionLambdaRunnable;

pub enum LambdaBody {
    Instruction(Arc<VMInstructionPackage>),
    NativeFunction(
        (
            Arc<dyn Fn() -> Box<dyn Runnable> + Send + Sync>,
            OnionKeyPool<String>,
        ),
    ),
}

impl Clone for LambdaBody {
    fn clone(&self) -> Self {
        match self {
            LambdaBody::Instruction(instruction) => LambdaBody::Instruction(instruction.clone()),
            LambdaBody::NativeFunction(native_function) => {
                LambdaBody::NativeFunction(native_function.clone())
            }
        }
    }
}

impl LambdaBody {
    fn create_string_pool(&self) -> OnionKeyPool<String> {
        match self {
            LambdaBody::Instruction(instruction) => instruction.create_key_pool(),
            LambdaBody::NativeFunction((_, key_pool)) => key_pool.clone(),
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

pub enum LambdaType {
    Normal,
    AsyncLauncher,
    SyncLauncher,
}

pub struct OnionLambdaDefinition {
    parameter: LambdaParameter,
    flatten_param_keys: Vec<String>,
    flatten_param_constraints: Vec<OnionObject>,
    body: LambdaBody,
    capture: OnionFastMap<String, OnionObject>,
    signature: String,
    lambda_type: LambdaType,
}

impl OnionLambdaDefinition {
    pub fn new_static(
        parameter: LambdaParameter,
        body: LambdaBody,
        capture: OnionFastMap<String, OnionObject>,
        signature: String,
        lambda_type: LambdaType,
    ) -> OnionStaticObject {
        let flatten_param_keys = parameter.flatten_keys();
        let flatten_param_constraints = parameter.flatten_constraints();
        OnionObject::Lambda((
            OnionLambdaDefinition {
                parameter,
                flatten_param_keys,
                flatten_param_constraints,
                body,
                capture,
                signature,
                lambda_type,
            }
            .into(),
            OnionObject::Undefined(None).into(),
        ))
        .consume_and_stabilize()
    }

    pub fn new_static_with_self(
        parameter: LambdaParameter,
        body: LambdaBody,
        capture: OnionFastMap<String, OnionObject>,
        self_object: &OnionObject,
        signature: String,
        lambda_type: LambdaType,
    ) -> OnionStaticObject {
        let flatten_param_keys = parameter.flatten_keys();
        let flatten_param_constraints = parameter.flatten_constraints();
        OnionObject::Lambda((
            OnionLambdaDefinition {
                parameter,
                flatten_param_keys,
                flatten_param_constraints,
                body,
                capture,
                signature,
                lambda_type,
            }
            .into(),
            self_object.clone().into(),
        ))
        .consume_and_stabilize()
    }

    // 从定义创建可用字符串池，Lambda自身只能使用这个字符串池中的字符串
    pub fn create_key_pool(&self) -> OnionKeyPool<String> {
        self.body.create_string_pool()
    }

    pub fn with_lambda_type(&self, lambda_type: LambdaType) -> Self {
        OnionLambdaDefinition {
            parameter: self.parameter.clone(),
            flatten_param_keys: self.flatten_param_keys.clone(),
            flatten_param_constraints: self.flatten_param_constraints.clone(),
            body: self.body.clone(),
            capture: self.capture.clone(),
            signature: self.signature.clone(),
            lambda_type,
        }
    }

    pub fn lambda_type(&self) -> &LambdaType {
        &self.lambda_type
    }

    // 显然我们在Launcher里已经严格保证argument所使用的字符串池是Lambda定义的字符串池
    pub fn create_runnable(
        &self,
        argument: &OnionFastMap<String, OnionStaticObject>,
        this_lambda: &OnionStaticObject,
        self_object: &OnionObject,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<Box<dyn Runnable>, RuntimeError> {
        match &self.body {
            LambdaBody::Instruction(instruction) => {
                let runnable = OnionLambdaRunnable::new(
                    argument,
                    &self.capture,
                    self_object,
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
            LambdaBody::NativeFunction((native_function, _)) => {
                let mut runnable = native_function();
                runnable.capture(argument, &self.capture, gc)?;
                runnable.bind_self_object(self_object, gc)?;
                Ok(runnable)
            }
        }
    }

    pub fn get_signature(&self) -> &str {
        &self.signature
    }

    pub fn get_parameter(&self) -> &LambdaParameter {
        &self.parameter
    }

    pub fn get_flatten_param_keys(&self) -> &[String] {
        &self.flatten_param_keys
    }

    pub fn get_flatten_param_constraints(&self) -> &[OnionObject] {
        &self.flatten_param_constraints
    }

    pub fn get_capture(&self) -> &OnionFastMap<String, OnionObject> {
        &self.capture
    }

    pub fn get_body(&self) -> &LambdaBody {
        &self.body
    }

    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.parameter.upgrade(collected);
        for (_, obj) in self.capture.pairs() {
            obj.upgrade(collected);
        }
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        match key {
            OnionObject::String(s) if s.as_str() == "$parameter" => {
                let parameter = self.parameter.to_onion();
                f(parameter.weak())
            }
            OnionObject::String(s) if s.as_str() == "$signature" => {
                f(&OnionObject::String(Arc::new(self.signature.clone())))
            }
            OnionObject::String(s) => {
                if let Some(value) = self.capture.get(s.as_ref()) {
                    f(value)
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Attribute '{:?}' not found in lambda definition", key).into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                format!("Attribute '{:?}' not found in lambda definition", key).into(),
            )),
        }
    }
}

impl GCTraceable<OnionObjectCell> for OnionLambdaDefinition {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        self.parameter.collect(queue);
        for (_, obj) in self.capture.pairs() {
            obj.collect(queue);
        }
    }
}

impl Debug for OnionLambdaDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "OnionLambdaDefinition {{ parameter: {:?}, body: {:?}, capture: {:?} }}",
            self.parameter, self.body, self.capture
        )
    }
}
