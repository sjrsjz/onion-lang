use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};

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
        object::{OnionObject, OnionObjectCell, OnionObjectProtocol, OnionObjectProtocolStatic, OnionStaticObject},
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

#[derive(Clone)]
pub struct OnionFloatValue {
    value: f64,
}

impl GCTraceable<OnionObjectCell> for OnionFloatValue {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}

impl Debug for OnionFloatValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl OnionFloatValue {
    #[inline(always)]
    pub fn new(value: f64) -> Self {
        OnionFloatValue { value }
    }

    #[inline(always)]
    pub fn new_static(value: f64) -> OnionStaticObject {
        OnionObject::FloatValue(Self::new(value)).consume_and_stabilize()
    }

    #[inline(always)]
    pub fn value(&self) -> f64 {
        self.value
    }
}

impl OnionObjectProtocol for OnionFloatValue {
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(std::format!("{:?}", self))
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("Float".into())
    }

    fn binary_eq(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        self.equals(other)
    }

    fn binary_lt(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(self.value < (i2.value() as f64)),
            OnionObject::FloatValue(f2) => Ok(self.value < f2.value()),
            _ => Err(RuntimeError::InvalidOperation(
                std::format!("Invalid binary_lt operation for {:?} and {:?}", self, other).into(),
            )),
        }
    }

    fn binary_gt(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(self.value > (i2.value() as f64)),
            OnionObject::FloatValue(f2) => Ok(self.value > f2.value()),
            _ => Err(RuntimeError::InvalidOperation(
                std::format!("Invalid binary_gt operation for {:?} and {:?}", self, other).into(),
            )),
        }
    }

    fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::IntegerValue(i2) => Ok(OnionStaticObject::new(OnionObject::FloatValue(
                OnionFloatValue::new(self.value + (i2.value() as f64)),
            ))),
            OnionObject::FloatValue(f2) => Ok(OnionStaticObject::new(OnionObject::FloatValue(
                OnionFloatValue::new(self.value + f2.value()),
            ))),
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
            OnionObject::IntegerValue(i2) => Ok(OnionStaticObject::new(OnionObject::FloatValue(
                OnionFloatValue::new(self.value - (i2.value() as f64)),
            ))),
            OnionObject::FloatValue(f2) => Ok(OnionStaticObject::new(OnionObject::FloatValue(
                OnionFloatValue::new(self.value - f2.value()),
            ))),
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
            OnionObject::IntegerValue(i2) => Ok(OnionStaticObject::new(OnionObject::FloatValue(
                OnionFloatValue::new(self.value * (i2.value() as f64)),
            ))),
            OnionObject::FloatValue(f2) => Ok(OnionStaticObject::new(OnionObject::FloatValue(
                OnionFloatValue::new(self.value * f2.value()),
            ))),
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
                let divisor = i2.value() as f64;
                if divisor == 0.0 {
                    return Err(RuntimeError::InvalidOperation("Division by zero".into()));
                }
                Ok(OnionStaticObject::new(OnionObject::FloatValue(
                    OnionFloatValue::new(self.value / divisor),
                )))
            }
            OnionObject::FloatValue(f2) => {
                if f2.value() == 0.0 {
                    return Err(RuntimeError::InvalidOperation("Division by zero".into()));
                }
                Ok(OnionStaticObject::new(OnionObject::FloatValue(
                    OnionFloatValue::new(self.value / f2.value()),
                )))
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
                let divisor = i2.value() as f64;
                if divisor == 0.0 {
                    return Err(RuntimeError::InvalidOperation("Division by zero".into()));
                }
                Ok(OnionStaticObject::new(OnionObject::FloatValue(
                    OnionFloatValue::new(self.value % divisor),
                )))
            }
            OnionObject::FloatValue(f2) => {
                if f2.value() == 0.0 {
                    return Err(RuntimeError::InvalidOperation("Division by zero".into()));
                }
                Ok(OnionStaticObject::new(OnionObject::FloatValue(
                    OnionFloatValue::new(self.value % f2.value()),
                )))
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
            OnionObject::IntegerValue(i2) => Ok(OnionStaticObject::new(OnionObject::FloatValue(
                OnionFloatValue::new(self.value.powf(i2.value() as f64)),
            ))),
            OnionObject::FloatValue(f2) => Ok(OnionStaticObject::new(OnionObject::FloatValue(
                OnionFloatValue::new(self.value.powf(f2.value())),
            ))),
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

    fn binary_and(&self, _other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "Bitwise operations not supported for Float".into(),
        ))
    }

    fn binary_or(&self, _other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "Bitwise operations not supported for Float".into(),
        ))
    }

    fn binary_xor(&self, _other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "Bitwise operations not supported for Float".into(),
        ))
    }

    fn binary_shl(&self, _other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "Shift operations not supported for Float".into(),
        ))
    }

    fn binary_shr(&self, _other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "Shift operations not supported for Float".into(),
        ))
    }

    fn unary_neg(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(OnionStaticObject::new(OnionObject::FloatValue(
            OnionFloatValue::new(-self.value),
        )))
    }

    fn unary_plus(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(OnionStaticObject::new(OnionObject::FloatValue(
            OnionFloatValue::new(self.value.abs()),
        )))
    }

    fn unary_not(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "Bitwise NOT not supported for Float".into(),
        ))
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {}

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::FloatValue(fv) => Ok((self.value - fv.value()).abs() < f64::EPSILON),
            OnionObject::IntegerValue(iv) => {
                Ok((self.value - (iv.value() as f64)).abs() < f64::EPSILON)
            }
            _ => Ok(false),
        }
    }
}



impl OnionObjectProtocolStatic for OnionFloatValue {
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
            format!("Attribute {} not found for Float", key.repr(&vec![])?).into(),
        ))
    }
}
