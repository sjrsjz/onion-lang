use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{arc::GCArcWeak, traceable::GCTraceable};

use crate::{
    lambda::runnable::RuntimeError,
    types::{
        lambda::{
            native::{
                native_bool_converter, native_bytes_converter, native_elements_method,
                native_float_converter, native_int_converter, native_length_method,
                native_string_converter, wrap_native_function,
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
pub struct OnionRange {
    start: i64,
    end: i64,
}

impl GCTraceable<OnionObjectCell> for OnionRange {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}

impl Debug for OnionRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl OnionRange {
    #[inline(always)]
    pub fn new(start: i64, end: i64) -> Self {
        OnionRange { start, end }
    }

    #[inline(always)]
    pub fn new_static(start: i64, end: i64) -> OnionStaticObject {
        OnionObject::Range(OnionRange::new(start, end)).consume_and_stabilize()
    }

    #[inline(always)]
    pub fn start(&self) -> i64 {
        self.start
    }

    #[inline(always)]
    pub fn end(&self) -> i64 {
        self.end
    }
}

impl OnionObjectProtocol for OnionRange {
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("{}..{}", self.start, self.end))
    }

    fn display(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("{}..{}", self.start, self.end))
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("Range".into())
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, _collected: &mut Vec<arc_gc::arc::GCArc<OnionObjectCell>>) {}

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::Range(other) => Ok(self.start == other.start && self.end == other.end),
            _ => Ok(false),
        }
    }

    fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::Range(other) => {
                let new_start = self.start + other.start;
                let new_end = self.end + other.end;
                Ok(Self::new_static(new_start, new_end))
            }
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_add operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }

    fn binary_sub(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::Range(other) => {
                let new_start = self.start - other.start;
                let new_end = self.end - other.end;
                Ok(Self::new_static(new_start, new_end))
            }
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_sub operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }
}

impl OnionObjectProtocolStatic for OnionRange {
    fn with_attribute<F, R>(
        &self,
        self_object: &OnionObject,
        key: &OnionObject,
        f: &F,
    ) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
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
                    return f(converter.weak());
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
                    return f(converter.weak());
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
                    return f(converter.weak());
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
                    return f(converter.weak());
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
                    return f(converter.weak());
                }
                "length" => {
                    let length_method = wrap_native_function(
                        LambdaParameter::Multiple(Box::new([])),
                        OnionFastMap::new(OnionKeyPool::create(vec![])),
                        self_object,
                        "builtin::length",
                        OnionKeyPool::create(vec![]),
                        &native_length_method,
                    );
                    return f(length_method.weak());
                }
                "elements" => {
                    let elements_method = wrap_native_function(
                        LambdaParameter::Multiple(Box::new([])),
                        OnionFastMap::new(OnionKeyPool::create(vec![])),
                        self_object,
                        "builtin::elements",
                        OnionKeyPool::create(vec![]),
                        &native_elements_method,
                    );
                    return f(elements_method.weak());
                }
                _ => {}
            }
        }
        Err(RuntimeError::InvalidOperation(
            format!("Attribute {} not found for Bytes", key.repr(&vec![])?).into(),
        ))
    }
}
