//! Onion 虚拟机原生方法与类型转换工具。
//!
//! - 提供原生方法包装、类型转换（int/float/string/bool/bytes）等常用原生函数。
//! - 支持 Rust 闭包/函数指针与虚拟机 Lambda 的无缝集成。
//! - 支持元组、字符串、字节数组等多种类型的自动展开与转换。

use std::sync::Arc;

use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        boolean_value::OnionBooleanValue,
        bytes_value::OnionBytesValue,
        float_value::OnionFloatValue,
        integer_value::OnionIntegerValue,
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinitionInner},
            parameter::LambdaParameter,
        },
        object::{OnionObject, OnionObjectCell, OnionObjectProtocol, OnionStaticObject},
        string_value::OnionStringValue,
        tuple::OnionTuple,
    },
    unwrap_step_result,
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

/// 原生方法生成器（NativeMethodGenerator）。
///
/// 用于将 Rust 闭包/函数指针包装为虚拟机可调度的 Runnable。
/// 支持自动捕获参数和 self，便于原生方法与虚拟机无缝集成。
pub struct NativeMethodGenerator<F>
where
    F: Fn(
            &OnionStaticObject,
            &OnionFastMap<Box<str>, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + 'static,
{
    /// 捕获参数（包含调用参数和捕获变量，自动合并）
    captured: OnionFastMap<Box<str>, OnionStaticObject>,
    /// self 对象（方法调用时的 this）
    self_object: OnionStaticObject,
    /// 原生函数指针（静态生命周期，便于高效复用）
    function: &'static F,
}

impl<F> Runnable for NativeMethodGenerator<F>
where
    F: Fn(
            &OnionStaticObject,
            &OnionFastMap<Box<str>, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + Send
        + Sync
        + 'static,
{
    /// 执行原生方法，自动将捕获和 self 传递给底层闭包。
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        unwrap_step_result!(
            (self.function)(&self.self_object, &self.captured, gc)
                .map(|result| StepResult::Return(result.into()))
        )
    }
    /// 格式化当前原生方法的上下文信息。
    /// 展示函数类型、self、参数等，便于调试定位。
    fn format_context(&self) -> String {
        let full_type_name = std::any::type_name_of_val(self.function);
        let short_type_name = full_type_name.split("::").last().unwrap_or(full_type_name);
        let self_info = format!("{:?}", self.self_object);
        format!(
            "-> Executing Native Method:\n   - Function: {} (Full Type: {})\n   - Self: {}\n   - Captured Args: {:?}",
            short_type_name, full_type_name, self_info, self.captured
        )
    }
}

/// int 类型转换器。
///
/// 支持从整数、浮点、字符串、布尔、字节数组等多种类型转换为整数。
/// 字符串/字节数组需能被正确解析，否则报错。
pub(crate) fn native_int_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| match obj {
        OnionObject::IntegerValue(v) => Ok(OnionIntegerValue::new_static(v.value())),
        OnionObject::FloatValue(v) => Ok(OnionIntegerValue::new_static(v.value() as i64)),
        OnionObject::StringValue(s) => match s.value().parse::<i64>() {
            Ok(parsed) => Ok(OnionIntegerValue::new_static(parsed)),
            Err(_) => Err(RuntimeError::DetailedError(
                format!("Cannot convert string '{}' to integer", s.value()).into(),
            )),
        },
        OnionObject::BooleanValue(b) => {
            Ok(OnionIntegerValue::new_static(if b.value() { 1 } else { 0 }))
        }
        OnionObject::BytesValue(b) => {
            if b.value().len() < 8 {
                return Err(RuntimeError::DetailedError(
                    "Cannot convert bytes to integer: insufficient length".into(),
                ));
            }
            let mut arr = [0u8; 8];
            let bytes = b.value();
            let len = bytes.len().min(8);
            arr[8 - len..].copy_from_slice(&bytes[..len]);
            Ok(OnionIntegerValue::new_static(i64::from_be_bytes(arr)))
        }
        _ => Err(RuntimeError::DetailedError(
            format!(
                "Cannot convert {} to integer",
                obj.type_of().unwrap_or("<unknown>".to_string())
            )
            .into(),
        )),
    })
}

/// float 类型转换器。
///
/// 支持从浮点、整数、字符串、布尔、字节数组等多种类型转换为浮点数。
/// 字符串/字节数组需能被正确解析，否则报错。
pub(crate) fn native_float_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| match obj {
        OnionObject::FloatValue(v) => Ok(OnionFloatValue::new_static(v.value())),
        OnionObject::IntegerValue(v) => Ok(OnionFloatValue::new_static(v.value() as f64)),
        OnionObject::StringValue(s) => match s.value().parse::<f64>() {
            Ok(parsed) => Ok(OnionFloatValue::new_static(parsed)),
            Err(_) => Err(RuntimeError::DetailedError(
                format!("Cannot convert string '{}' to float", s.value()).into(),
            )),
        },
        OnionObject::BooleanValue(b) => Ok(OnionFloatValue::new_static(if b.value() {
            1.0
        } else {
            0.0
        })),
        OnionObject::BytesValue(b) => {
            if b.value().len() < 8 {
                return Err(RuntimeError::DetailedError(
                    "Cannot convert bytes to float: insufficient length".into(),
                ));
            }
            let mut arr = [0u8; 8];
            let bytes = b.value();
            let len = bytes.len().min(8);
            arr[8 - len..].copy_from_slice(&bytes[..len]);
            Ok(OnionFloatValue::new_static(f64::from_be_bytes(arr)))
        }
        _ => Err(RuntimeError::DetailedError(
            format!(
                "Cannot convert {} to float",
                obj.type_of().unwrap_or("<unknown>".to_string())
            )
            .into(),
        )),
    })
}

/// string 类型转换器。
///
/// 支持从字符串、整数、浮点、布尔、字节数组、null、undefined、range、复杂对象等多种类型转换为字符串。
pub(crate) fn native_string_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object
        .weak()
        .display(&vec![])
        .map(|s| OnionStringValue::new_static(s))
}

/// bool 类型转换器。
///
/// 支持从布尔、整数、浮点、字符串、字节数组、null、undefined、元组等多种类型转换为布尔值。
/// 0/空/Null/Undefined 均为 false，其他为 true。
pub(crate) fn native_bool_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| match obj {
        OnionObject::BooleanValue(b) => Ok(OnionBooleanValue::new_static(b.value())),
        OnionObject::IntegerValue(v) => Ok(OnionBooleanValue::new_static(v.value() != 0)),
        OnionObject::FloatValue(v) => Ok(OnionBooleanValue::new_static(v.value() != 0.0)),
        OnionObject::StringValue(s) => Ok(OnionBooleanValue::new_static(!s.value().is_empty())),
        OnionObject::BytesValue(bytes) => {
            Ok(OnionBooleanValue::new_static(!bytes.value().is_empty()))
        }
        OnionObject::Null(_) => Ok(OnionBooleanValue::new_static(false)),
        OnionObject::Undefined(_) => Ok(OnionBooleanValue::new_static(false)),
        OnionObject::Tuple(tuple) => Ok(OnionBooleanValue::new_static(
            !tuple.get_elements().is_empty(),
        )),
        _ => Ok(OnionBooleanValue::new_static(true)),
    })
}

/// bytes 类型转换器。
///
/// 支持从字节数组、字符串、整数、浮点、布尔、元组等多种类型转换为字节数组。
/// 元组元素需为 0-255 的整数，否则报错。
pub(crate) fn native_bytes_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| {
        match obj {
            OnionObject::BytesValue(bytes) => {
                Ok(OnionBytesValue::new_static(bytes.value()))
            }
            OnionObject::StringValue(s) => {
                Ok(OnionBytesValue::new_static(s.value().as_bytes()))
            }
            OnionObject::IntegerValue(v) => {
                let bytes = v.value().to_be_bytes();
                Ok(OnionBytesValue::new_static(&bytes))
            }
            OnionObject::FloatValue(v) => {
                // 浮点数转字节数组（大端序）
                let bytes = v.value().to_be_bytes();
                Ok(OnionBytesValue::new_static(&bytes))
            }
            OnionObject::BooleanValue(b) => {
                let byte_value = if b.value() { 1u8 } else { 0u8 };
                Ok(OnionBytesValue::new_static(&[byte_value]))
            }
            OnionObject::Tuple(tuple) => {
                let mut result = Vec::new();
                for element in tuple.get_elements().iter() {
                    match element {
                        OnionObject::IntegerValue(v) => {
                            if v.value() >= 0 && v.value() <= 255 {
                                result.push(v.value() as u8);
                            } else {
                                return Err(RuntimeError::DetailedError(
                                    format!("Integer {} is out of byte range (0-255)", v.value()).into(),
                                ));
                            }
                        }
                        _ => {
                            return Err(RuntimeError::DetailedError(
                                "Tuple elements must be integers in range 0-255 to convert to bytes".into(),
                            ));
                        }
                    }
                }
                Ok(OnionBytesValue::new_static(&result))
            }
            _ => Err(RuntimeError::DetailedError(
                format!("Cannot convert {} to bytes", obj.type_of().unwrap_or("<unknown>".to_string())).into(),
            )),
        }
    })
}

/// length 方法。
///
/// 获取字符串、字节数组、区间、元组等对象的长度。
/// 其他类型报错。
pub(crate) fn native_length_method(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| match obj {
        OnionObject::IntegerValue(v) => v.len(),
        OnionObject::FloatValue(v) => v.len(),
        OnionObject::StringValue(v) => v.len(),
        OnionObject::BytesValue(v) => v.len(),
        OnionObject::Range(v) => v.len(),
        OnionObject::Tuple(v) => v.len(),
        OnionObject::BooleanValue(v) => v.len(),
        OnionObject::Null(v) => v.len(),
        OnionObject::Undefined(v) => v.len(),
        OnionObject::InstructionPackage(v) => v.len(),
        OnionObject::Pair(v) => v.len(),
        OnionObject::LazySet(v) => v.len(),
        OnionObject::Lambda(v) => v.len(),
        OnionObject::Custom(v) => v.len(),
        OnionObject::Mut(_) => unreachable!(),
    })
}

/// elements 方法。
///
/// 获取字符串、字节数组、区间、元组等对象的元素集合。
/// 字符串按字符分割，元组返回自身。
pub(crate) fn native_elements_method(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| {
        match obj {
            OnionObject::StringValue(s) => {
                let elements = OnionTuple::new_static_no_ref(
                    &s.value()
                        .chars()
                        .map(|c| OnionStringValue::new_static(c.to_string()))
                        .collect::<Vec<_>>(),
                );
                Ok(elements)
            }
            OnionObject::BytesValue(b) => {
                let elements = OnionTuple::new_static_no_ref(
                    &b.value()
                        .iter()
                        .map(|byte| OnionIntegerValue::new_static(*byte as i64))
                        .collect::<Vec<_>>(),
                );
                Ok(elements)
            }
            OnionObject::Range(range) => {
                let elements = OnionTuple::new_static_no_ref(
                    &(range.start()..range.end())
                        .map(|i| OnionIntegerValue::new_static(i as i64))
                        .collect::<Vec<_>>(),
                );
                Ok(elements)
            }
            OnionObject::Tuple(_) => {
                // 对于元组，直接返回自己
                Ok(self_object.clone())
            }
            _ => Err(RuntimeError::DetailedError(
                format!(
                    "Cannot get elements of {}",
                    obj.type_of().unwrap_or("<unknown>".to_string())
                )
                .into(),
            )),
        }
    })
}

/// 将 Rust 原生函数包装为虚拟机 Lambda。
///
/// 支持参数、捕获、self、签名、字符串池等元信息，返回可直接用于虚拟机的 Lambda 对象。
/// 捕获参数与调用参数自动合并，便于原生方法与虚拟机交互。
pub(crate) fn wrap_native_function<F>(
    params: LambdaParameter,
    capture: OnionFastMap<Box<str>, OnionObject>,
    self_object: &OnionObject,
    signature: &'static str,
    string_pool: OnionKeyPool<Box<str>>,
    function: &'static F,
) -> OnionStaticObject
where
    F: Fn(
            &OnionStaticObject,
            &OnionFastMap<Box<str>, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + Send
        + Sync
        + 'static,
{
    let cloned_pool = string_pool.clone();
    OnionLambdaDefinitionInner::new_static_with_self(
        params,
        LambdaBody::NativeFunction((
            Arc::new(
                move |self_object: &OnionObject,
                      argument: &OnionFastMap<Box<str>, OnionStaticObject>,
                      capture: &OnionFastMap<Box<str>, OnionObject>,
                      _gc: &mut GC<OnionObjectCell>| {
                    let mut captured = argument.clone();
                    for (key, value) in capture.pairs() {
                        captured.push_with_index(*key, value.stabilize());
                    }
                    Box::new(NativeMethodGenerator {
                        captured,
                        self_object: self_object.stabilize(),
                        function: function,
                    })
                },
            ),
            cloned_pool,
        )),
        capture,
        self_object,
        Box::from(signature),
        LambdaType::Atomic,
    )
}
