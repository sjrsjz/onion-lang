use std::{collections::VecDeque, fmt::Debug, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};
use base64::Engine;
use smallvec::SmallVec;

use crate::{
    lambda::runnable::{RuntimeError, StepResult},
    types::{
        integer_value::OnionIntegerValue,
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
pub struct OnionBytesValue {
    value: Arc<[u8]>,
}

impl GCTraceable<OnionObjectCell> for OnionBytesValue {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}

impl Debug for OnionBytesValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let b64 = base64::engine::general_purpose::STANDARD.encode(&self.value);
        write!(f, "${:?}", b64)
    }
}

impl OnionBytesValue {
    #[inline(always)]
    pub fn new(value: &[u8]) -> Self {
        OnionBytesValue {
            value: Arc::from(value),
        }
    }

    #[inline(always)]
    pub fn new_static(value: &[u8]) -> OnionStaticObject {
        OnionObject::BytesValue(Self::new(value)).consume_and_stabilize()
    }

    #[inline(always)]
    pub fn value(&self) -> &[u8] {
        &self.value
    }

    #[inline(always)]
    pub fn as_arc(&self) -> &Arc<[u8]> {
        &self.value
    }
}

impl OnionObjectProtocol for OnionBytesValue {
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        let b64 = base64::engine::general_purpose::STANDARD.encode(&self.value);
        Ok(format!("${}", b64))
    }

    fn display(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        let b64 = base64::engine::general_purpose::STANDARD.encode(&self.value);
        Ok(format!("${}", b64))
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("Bytes".into())
    }

    fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(OnionIntegerValue::new_static(self.value.len() as i64))
    }

    fn contains(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::BytesValue(b) => {
                let needle = b.value();
                if needle.is_empty() {
                    Ok(true)
                } else {
                    Ok(self
                        .value
                        .windows(needle.len())
                        .any(|window| window == needle))
                }
            }
            _ => Ok(false),
        }
    }

    fn binary_eq(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        self.equals(other)
    }
    fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::BytesValue(s2) => {
                let mut new_bytes = Vec::with_capacity(self.value().len() + s2.value().len());
                new_bytes.extend_from_slice(self.value().as_ref());
                new_bytes.extend_from_slice(s2.value().as_ref());
                Ok(Self::new_static(new_bytes.as_slice()))
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {}

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::BytesValue(other) => Ok(self.value() == other.value()),
            _ => Ok(false),
        }
    }

    fn apply(
        &self,
        _this_object: &OnionObject,
        _self_object: Option<&OnionObject>,
        value: &OnionObject,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<Result<StepResult, OnionStaticObject>, RuntimeError> {
        value.with_data(|value| match value {
            OnionObject::IntegerValue(i) => {
                let idx = i.value();
                let len = self.value.len();

                if len == 0 {
                    return Err(RuntimeError::InvalidOperation(
                        format!("Index {} out of bounds for empty Bytes", idx,).into(),
                    ));
                }

                let wrapped_idx = ((idx % len as i64) + len as i64) % len as i64;

                Ok(Err(OnionIntegerValue::new_static(
                    self.value[wrapped_idx as usize] as i64,
                )))
            }
            OnionObject::Range(range) => {
                let start = range.start();
                let end = range.end();
                let len = self.value.len();
                if start < 0 || end < 0 || start >= len as i64 || end > len as i64 || start > end {
                    return Err(RuntimeError::InvalidOperation(
                        format!("Range {}..{} out of bounds for Bytes", start, end).into(),
                    ));
                }

                Ok(Err(OnionBytesValue::new_static(
                    &self.value[start as usize..end as usize],
                )))
            }
            _ => Err(RuntimeError::InvalidOperation(
                format!("Cannot apply {} to Bytes", value.repr(&vec![])?).into(),
            )),
        })
    }
}

impl OnionObjectProtocolStatic for OnionBytesValue {
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
                "length" => {
                    let length_method = wrap_native_function(
                        LambdaParameter::Multiple(Box::new([])),
                        OnionFastMap::new(OnionKeyPool::create(vec![])),
                        self_object,
                        "builtin::length",
                        OnionKeyPool::create(vec![]),
                        &native_length_method,
                    );
                    return f(self_object, length_method.weak());
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
                    return f(self_object, elements_method.weak());
                }
                _ => {}
            }
        }
        Err(RuntimeError::InvalidOperation(
            format!("Attribute {} not found for Bytes", key.repr(&vec![])?).into(),
        ))
    }
}
