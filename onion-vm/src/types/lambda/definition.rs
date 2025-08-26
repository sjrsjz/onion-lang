//! Onion 虚拟机 Lambda 定义与实现。
//!
//! - `OnionLambdaDefinition`：完整的 Lambda 定义，支持参数、捕获、原生与字节码体。
//! - `LambdaBody`：Lambda 体类型，支持字节码与原生函数。
//! - `LambdaType`：Lambda 类型（普通、异步、同步）。
//! - 提供 Lambda 的构造、运行、属性访问、GC 跟踪等。

use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
    ops::Deref,
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
        object::{
            OnionObject, OnionObjectCell, OnionObjectProtocol, OnionObjectProtocolStatic,
            OnionStaticObject,
        },
        string_value::OnionStringValue,
        undefined::OnionUndefined,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

use super::runnable::OnionLambdaRunnable;

/// Lambda 体类型。
///
/// 支持字节码（Instruction）和原生函数（NativeFunction）。
pub enum LambdaBody {
    /// 字节码实现
    Instruction(Arc<VMInstructionPackage>),
    /// 原生函数实现（带字符串池），用于启动一个 Runnable
    NativeFunction(
        (
            Arc<
                dyn Fn(
                        &OnionObject,                               // self_object
                        &OnionFastMap<Box<str>, OnionStaticObject>, // argument
                        &OnionFastMap<Box<str>, OnionObject>,       // captured_vars
                        &mut GC<OnionObjectCell>,                   // gc
                    ) -> Box<dyn Runnable>
                    + Send
                    + Sync,
            >,
            OnionKeyPool<Box<str>>,
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
    fn create_string_pool(&self) -> OnionKeyPool<Box<str>> {
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

/// Lambda 类型。
pub enum LambdaType {
    /// 普通 Lambda
    Atomic,
    /// 异步调度器
    AsyncLauncher,
    /// 同步调度器
    SyncLauncher,
}

/// Onion 虚拟机 Lambda 定义。
///
/// 封装参数、捕获、Lambda 体、签名、类型等。
#[derive(Clone)]
pub struct OnionLambdaDefinition {
    inner: Arc<OnionLambdaDefinitionInner>,
    temp_self_object: Arc<OnionObject>, // 临时 self 对象
}

impl OnionLambdaDefinition {
    pub fn new(inner: OnionLambdaDefinitionInner, temp_self_object: &OnionObject) -> Self {
        Self {
            inner: Arc::new(inner),
            temp_self_object: Arc::new(temp_self_object.clone()),
        }
    }

    pub fn with_lambda_type(&self, lambda_type: LambdaType) -> OnionLambdaDefinition {
        OnionLambdaDefinition::new(
            OnionLambdaDefinitionInner {
                parameter: self.inner.parameter.clone(),
                flatten_param_keys: self.inner.flatten_param_keys.clone(),
                flatten_param_constraints: self.inner.flatten_param_constraints.clone(),
                body: self.inner.body.clone(),
                capture: self.inner.capture.clone(),
                signature: self.inner.signature.clone(),
                lambda_type,
            },
            &self.temp_self_object,
        )
    }

    #[inline(always)]
    pub fn temp_self_object(&self) -> &OnionObject {
        &self.temp_self_object
    }
}

impl Deref for OnionLambdaDefinition {
    type Target = OnionLambdaDefinitionInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub struct OnionLambdaDefinitionInner {
    /// 参数定义
    parameter: LambdaParameter,
    /// 展平后的参数名
    flatten_param_keys: Box<[Box<str>]>,
    /// 展平后的参数约束
    flatten_param_constraints: Box<[OnionObject]>,
    /// Lambda 体
    body: LambdaBody,
    /// 捕获变量
    capture: OnionFastMap<Box<str>, OnionObject>,
    /// 签名字符串
    signature: Box<str>,
    /// Lambda 类型
    lambda_type: LambdaType,
}

impl OnionLambdaDefinitionInner {
    pub fn new_static(
        parameter: LambdaParameter,
        body: LambdaBody,
        capture: OnionFastMap<Box<str>, OnionObject>,
        signature: Box<str>,
        lambda_type: LambdaType,
    ) -> OnionStaticObject {
        let flatten_param_keys = parameter.flatten_keys();
        let flatten_param_constraints = parameter.flatten_constraints();
        OnionObject::Lambda(OnionLambdaDefinition::new(
            OnionLambdaDefinitionInner {
                parameter,
                flatten_param_keys,
                flatten_param_constraints,
                body,
                capture,
                signature,
                lambda_type,
            },
            &OnionObject::Undefined(OnionUndefined::new(None)),
        ))
        .consume_and_stabilize()
    }

    pub fn new_static_with_self(
        parameter: LambdaParameter,
        body: LambdaBody,
        capture: OnionFastMap<Box<str>, OnionObject>,
        self_object: &OnionObject,
        signature: Box<str>,
        lambda_type: LambdaType,
    ) -> OnionStaticObject {
        let flatten_param_keys = parameter.flatten_keys();
        let flatten_param_constraints = parameter.flatten_constraints();
        OnionObject::Lambda(OnionLambdaDefinition::new(
            OnionLambdaDefinitionInner {
                parameter,
                flatten_param_keys,
                flatten_param_constraints,
                body,
                capture,
                signature,
                lambda_type,
            },
            self_object,
        ))
        .consume_and_stabilize()
    }

    // 从定义创建可用字符串池，Lambda自身只能使用这个字符串池中的字符串
    pub fn create_key_pool(&self) -> OnionKeyPool<Box<str>> {
        self.body.create_string_pool()
    }
    pub fn lambda_type(&self) -> &LambdaType {
        &self.lambda_type
    }

    // 显然我们在Launcher里已经严格保证argument所使用的字符串池是Lambda定义的字符串池
    pub fn create_runnable(
        &self,
        argument: &OnionFastMap<Box<str>, OnionStaticObject>,
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
                    match instruction.get_table().get(self.signature.as_ref()) {
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
                Ok(native_function(self_object, argument, &self.capture, gc))
            }
        }
    }

    pub fn get_signature(&self) -> &str {
        &self.signature
    }

    pub fn get_parameter(&self) -> &LambdaParameter {
        &self.parameter
    }

    pub fn get_flatten_param_keys(&self) -> &[Box<str>] {
        &self.flatten_param_keys
    }

    pub fn get_flatten_param_constraints(&self) -> &[OnionObject] {
        &self.flatten_param_constraints
    }

    pub fn get_capture(&self) -> &OnionFastMap<Box<str>, OnionObject> {
        &self.capture
    }

    pub fn get_body(&self) -> &LambdaBody {
        &self.body
    }
}

impl GCTraceable<OnionObjectCell> for OnionLambdaDefinition {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        self.temp_self_object.collect(queue);
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

impl OnionObjectProtocol for OnionLambdaDefinition {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.temp_self_object.upgrade(collected);
        self.parameter.upgrade(collected);
        for (_, obj) in self.capture.pairs() {
            obj.upgrade(collected);
        }
    }

    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!(
            "({:?}) -> &{:?})",
            self.parameter,
            self.capture.keys()
        ))
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("Lambda".into())
    }

    fn equals(&self, _other: &OnionObject) -> Result<bool, RuntimeError> {
        Ok(false)
    }
}

impl OnionObjectProtocolStatic for OnionLambdaDefinition {
    fn with_attribute<F, R>(
        &self,
        _self_object: &OnionObject,
        key: &OnionObject,
        f: &F,
    ) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        match key {
            OnionObject::StringValue(s) if s.value() == "$parameter" => {
                let parameter = self.parameter.to_onion();
                f(parameter.weak())
            }
            OnionObject::StringValue(s) if s.value() == "$signature" => f(
                &OnionObject::StringValue(OnionStringValue::new(&self.signature)),
            ),
            OnionObject::StringValue(s) => {
                if let Some(value) = self.capture.get(s.value()) {
                    f(value)
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Attribute {:?} not found in lambda definition", key).into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                format!("Attribute {:?} not found in lambda definition", key).into(),
            )),
        }
    }
}
