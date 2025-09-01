use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{arc::GCArcWeak, traceable::GCTraceable};
use smallvec::SmallVec;

use crate::{
    lambda::runnable::RuntimeError,
    types::{
        lambda::{
            native::{
                native_bool_converter, native_bytes_converter, native_float_converter,
                native_int_converter, native_string_converter, wrap_native_function,
            },
            parameter::LambdaParameter,
        },
        object::{
            OnionObject, OnionObjectCell, OnionObjectProtocol, OnionObjectProtocolStatic,
            OnionStaticObject,
        },
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

#[derive(Clone)]
pub struct OnionBooleanValue {
    value: bool,
}

impl GCTraceable<OnionObjectCell> for OnionBooleanValue {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}

impl Debug for OnionBooleanValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl OnionBooleanValue {
    #[inline(always)]
    pub fn new(value: bool) -> Self {
        OnionBooleanValue { value }
    }

    #[inline(always)]
    pub fn new_static(value: bool) -> OnionStaticObject {
        OnionObject::BooleanValue(Self::new(value)).consume_and_stabilize()
    }

    #[inline(always)]
    pub fn value(&self) -> bool {
        self.value
    }
}

impl OnionObjectProtocol for OnionBooleanValue {
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("{:?}", self.value))
    }

    fn display(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("{}", self.value))
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("Boolean".into())
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, _collected: &mut Vec<arc_gc::arc::GCArc<OnionObjectCell>>) {}

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::BooleanValue(other) => Ok(self.value == other.value),
            _ => Ok(false),
        }
    }

    fn binary_and(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::BooleanValue(other) => {
                let result = self.value && other.value;
                Ok(OnionBooleanValue::new_static(result))
            }
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_and operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }

    fn binary_or(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::BooleanValue(other) => {
                let result = self.value || other.value;
                Ok(OnionBooleanValue::new_static(result))
            }
            _ => Err(RuntimeError::InvalidOperation(
                std::format!("Invalid binary_or operation for {:?} and {:?}", self, other).into(),
            )),
        }
    }

    fn binary_xor(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::BooleanValue(other) => {
                let result = self.value ^ other.value;
                Ok(OnionBooleanValue::new_static(result))
            }
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_xor operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }

    fn unary_not(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(OnionBooleanValue::new_static(!self.value))
    }
}

impl OnionObjectProtocolStatic for OnionBooleanValue {
    fn with_attribute<F, R>(
        &self,
        self_object: &OnionObject,
        key: &OnionObject,
        _path: &mut SmallVec<[*const (); 8]>,
        f: &F,
    ) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject, &OnionObject) -> Result<R, RuntimeError>,
    {
        if let OnionObject::StringValue(key_str) = key {
            match key_str.value() {
                "int" => {
                    let converter = wrap_native_function(
                        LambdaParameter::Multiple(Box::new([])),
                        OnionFastMap::new(OnionKeyPool::create(vec![])),
                        self_object,
                        "converter::int",
                        OnionKeyPool::create(vec![]),
                        &native_int_converter,
                    );
                    return f(self_object, converter.weak());
                }
                "float" => {
                    let converter = wrap_native_function(
                        LambdaParameter::Multiple(Box::new([])),
                        OnionFastMap::new(OnionKeyPool::create(vec![])),
                        self_object,
                        "converter::float",
                        OnionKeyPool::create(vec![]),
                        &native_float_converter,
                    );
                    return f(self_object, converter.weak());
                }
                "string" => {
                    let converter = wrap_native_function(
                        LambdaParameter::Multiple(Box::new([])),
                        OnionFastMap::new(OnionKeyPool::create(vec![])),
                        self_object,
                        "converter::string",
                        OnionKeyPool::create(vec![]),
                        &native_string_converter,
                    );
                    return f(self_object, converter.weak());
                }
                "bool" => {
                    let converter = wrap_native_function(
                        LambdaParameter::Multiple(Box::new([])),
                        OnionFastMap::new(OnionKeyPool::create(vec![])),
                        self_object,
                        "converter::bool",
                        OnionKeyPool::create(vec![]),
                        &native_bool_converter,
                    );
                    return f(self_object, converter.weak());
                }
                "bytes" => {
                    let converter = wrap_native_function(
                        LambdaParameter::Multiple(Box::new([])),
                        OnionFastMap::new(OnionKeyPool::create(vec![])),
                        self_object,
                        "converter::bytes",
                        OnionKeyPool::create(vec![]),
                        &native_bytes_converter,
                    );
                    return f(self_object, converter.weak());
                }
                _ => {}
            }
        }
        Err(RuntimeError::InvalidOperation(
            format!("Attribute {} not found for Boolean", key.repr(&vec![])?).into(),
        ))
    }
}
