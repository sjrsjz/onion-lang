use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{arc::GCArcWeak, traceable::GCTraceable};

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
pub struct OnionNull {}

impl GCTraceable<OnionObjectCell> for OnionNull {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}

impl Debug for OnionNull {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}

impl OnionNull {
    #[inline(always)]
    pub fn new() -> Self {
        OnionNull {}
    }

    #[inline(always)]
    pub fn new_static() -> OnionStaticObject {
        OnionObject::Null(OnionNull {}).consume_and_stabilize()
    }
}

impl OnionObjectProtocol for OnionNull {
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok("null".into())
    }

    fn display(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok("null".into())
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("Null".into())
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, _collected: &mut Vec<arc_gc::arc::GCArc<OnionObjectCell>>) {}

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::Null(_) => Ok(true),
            _ => Ok(false),
        }
    }
}

impl OnionObjectProtocolStatic for OnionNull {
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
                _ => {}
            }
        }
        Err(RuntimeError::InvalidOperation(
            format!("Attribute {} not found for Null", key.repr(&vec![])?).into(),
        ))
    }
}
