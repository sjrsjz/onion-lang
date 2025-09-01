use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};
use smallvec::SmallVec;

use crate::{
    lambda::runnable::RuntimeError,
    types::{
        float_value::OnionFloatValue,
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
pub struct OnionIntegerValue {
    value: i64,
}

impl GCTraceable<OnionObjectCell> for OnionIntegerValue {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}

impl Debug for OnionIntegerValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl OnionIntegerValue {
    #[inline(always)]
    pub fn new(value: i64) -> Self {
        OnionIntegerValue { value }
    }

    #[inline(always)]
    pub fn new_static(value: i64) -> OnionStaticObject {
        OnionObject::IntegerValue(Self::new(value)).consume_and_stabilize()
    }

    #[inline(always)]
    pub fn value(&self) -> i64 {
        self.value
    }
}

impl OnionObjectProtocol for OnionIntegerValue {
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(std::format!("{:?}", self.value))
    }

    fn display(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(std::format!("{}", self.value))
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("Integer".into())
    }

    fn binary_eq(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        self.equals(other)
    }

    fn binary_lt(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(self.value < i2.value()),
            OnionObject::FloatValue(f2) => Ok((self.value as f64) < f2.value()),
            _ => Err(RuntimeError::InvalidOperation(
                std::format!("Invalid binary_lt operation for {:?} and {:?}", self, other).into(),
            )),
        }
    }

    fn binary_gt(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(self.value > i2.value()),
            OnionObject::FloatValue(f2) => Ok((self.value as f64) > f2.value()),
            _ => Err(RuntimeError::InvalidOperation(
                std::format!("Invalid binary_gt operation for {:?} and {:?}", self, other).into(),
            )),
        }
    }

    fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(Self::new_static(self.value + i2.value())),
            OnionObject::FloatValue(f2) => {
                Ok(OnionFloatValue::new_static(self.value as f64 + f2.value()))
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
            OnionObject::IntegerValue(i2) => Ok(Self::new_static(self.value - i2.value())),
            OnionObject::FloatValue(f2) => {
                Ok(OnionFloatValue::new_static(self.value as f64 - f2.value()))
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

    fn binary_mul(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(Self::new_static(self.value * i2.value())),
            OnionObject::FloatValue(f2) => {
                Ok(OnionFloatValue::new_static(self.value as f64 * f2.value()))
            }
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_mul operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }

    fn binary_div(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => {
                if i2.value() == 0 {
                    return Err(RuntimeError::InvalidOperation("Division by zero".into()));
                }
                Ok(Self::new_static(self.value / i2.value()))
            }
            OnionObject::FloatValue(f2) => {
                Ok(OnionFloatValue::new_static(self.value as f64 / f2.value()))
            }
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_div operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }

    fn binary_mod(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => {
                if i2.value() == 0 {
                    return Err(RuntimeError::InvalidOperation("Division by zero".into()));
                }
                Ok(Self::new_static(self.value % i2.value()))
            }
            OnionObject::FloatValue(f2) => {
                Ok(OnionFloatValue::new_static(self.value as f64 % f2.value()))
            }
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_mod operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }

    fn binary_pow(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => {
                Ok(Self::new_static(self.value.pow(i2.value() as u32)))
            }
            OnionObject::FloatValue(f2) => Ok(OnionFloatValue::new_static(
                (self.value as f64).powf(f2.value()),
            )),
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_pow operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }

    fn binary_and(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(Self::new_static(self.value & i2.value())),
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
            OnionObject::IntegerValue(i2) => Ok(Self::new_static(self.value | i2.value())),
            _ => Err(RuntimeError::InvalidOperation(
                std::format!("Invalid binary_or operation for {:?} and {:?}", self, other).into(),
            )),
        }
    }

    fn binary_xor(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(Self::new_static(self.value ^ i2.value())),
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

    fn binary_shl(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(Self::new_static(self.value << i2.value())),
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_shl operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }

    fn binary_shr(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(Self::new_static(self.value >> i2.value())),
            _ => Err(RuntimeError::InvalidOperation(
                std::format!(
                    "Invalid binary_shr operation for {:?} and {:?}",
                    self,
                    other
                )
                .into(),
            )),
        }
    }

    fn unary_neg(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(Self::new_static(-self.value))
    }

    fn unary_plus(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(Self::new_static(self.value.abs()))
    }

    fn unary_not(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(Self::new_static(!self.value))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {}

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::IntegerValue(iv) => Ok(self.value == iv.value),
            OnionObject::FloatValue(fv) => Ok((self.value as f64) == fv.value()),
            _ => Ok(false),
        }
    }
}

impl OnionObjectProtocolStatic for OnionIntegerValue {
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
            format!("Attribute {} not found for Integer", key.repr(&vec![])?).into(),
        ))
    }
}
