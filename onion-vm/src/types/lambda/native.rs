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
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinition},
            parameter::LambdaParameter,
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
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
        OnionObject::Integer(v) => Ok(OnionObject::Integer(*v).stabilize()),
        OnionObject::Float(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
        OnionObject::String(s) => match s.parse::<i64>() {
            Ok(parsed) => Ok(OnionObject::Integer(parsed).stabilize()),
            Err(_) => Err(RuntimeError::DetailedError(
                format!("Cannot convert string '{}' to integer", s).into(),
            )),
        },
        OnionObject::Boolean(b) => Ok(OnionObject::Integer(if *b { 1 } else { 0 }).stabilize()),
        OnionObject::Bytes(bytes) => match std::str::from_utf8(bytes) {
            Ok(s) => match s.parse::<i64>() {
                Ok(parsed) => Ok(OnionObject::Integer(parsed).stabilize()),
                Err(_) => Err(RuntimeError::DetailedError(
                    format!("Cannot convert bytes to integer: invalid format").into(),
                )),
            },
            Err(_) => Err(RuntimeError::DetailedError(
                "Cannot convert non-UTF8 bytes to integer"
                    .to_string()
                    .into(),
            )),
        },
        _ => Err(RuntimeError::DetailedError(
            format!(
                "Cannot convert {} to integer",
                obj.type_of().unwrap_or("unknown".to_string())
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
        OnionObject::Float(v) => Ok(OnionObject::Float(*v).stabilize()),
        OnionObject::Integer(v) => Ok(OnionObject::Float(*v as f64).stabilize()),
        OnionObject::String(s) => match s.parse::<f64>() {
            Ok(parsed) => Ok(OnionObject::Float(parsed).stabilize()),
            Err(_) => Err(RuntimeError::DetailedError(
                format!("Cannot convert string '{}' to float", s).into(),
            )),
        },
        OnionObject::Boolean(b) => Ok(OnionObject::Float(if *b { 1.0 } else { 0.0 }).stabilize()),
        OnionObject::Bytes(bytes) => match std::str::from_utf8(bytes) {
            Ok(s) => match s.parse::<f64>() {
                Ok(parsed) => Ok(OnionObject::Float(parsed).stabilize()),
                Err(_) => Err(RuntimeError::DetailedError(
                    format!("Cannot convert bytes to float: invalid format").into(),
                )),
            },
            Err(_) => Err(RuntimeError::DetailedError(
                "Cannot convert non-UTF8 bytes to float".into(),
            )),
        },
        _ => Err(RuntimeError::DetailedError(
            format!(
                "Cannot convert {} to float",
                obj.type_of().unwrap_or("unknown".to_string())
            )
            .into(),
        )),
    })
}

/// string 类型转换器。
///
/// 支持从字符串、整数、浮点、布尔、字节数组、null、undefined、range、复杂对象等多种类型转换为字符串。
/// 对于复杂对象，优先使用 repr，否则输出类型名。
pub(crate) fn native_string_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| {
        match obj {
            OnionObject::String(s) => Ok(OnionObject::String(s.clone()).stabilize()),
            OnionObject::Integer(v) => {
                Ok(OnionObject::String(Arc::from(v.to_string())).stabilize())
            }
            OnionObject::Float(v) => Ok(OnionObject::String(Arc::from(v.to_string())).stabilize()),
            OnionObject::Boolean(b) => {
                Ok(OnionObject::String(Arc::from(b.to_string())).stabilize())
            }
            OnionObject::Bytes(bytes) => {
                match std::str::from_utf8(bytes) {
                    Ok(s) => Ok(OnionObject::String(Arc::from(s.to_string())).stabilize()),
                    Err(_) => {
                        // 如果不是有效UTF-8，使用lossy转换
                        let s = String::from_utf8_lossy(bytes);
                        Ok(OnionObject::String(Arc::from(s.to_string())).stabilize())
                    }
                }
            }
            OnionObject::Null => Ok(OnionObject::String(Arc::from("null")).stabilize()),
            OnionObject::Undefined(_) => {
                Ok(OnionObject::String(Arc::from("undefined")).stabilize())
            }
            OnionObject::Range(start, end) => {
                Ok(OnionObject::String(Arc::from(format!("{}..{}", start, end))).stabilize())
            }
            _ => {
                // 对于复杂对象，使用 repr 方法
                match obj.repr(&vec![]) {
                    Ok(repr_str) => Ok(OnionObject::String(Arc::from(repr_str)).stabilize()),
                    Err(_) => Ok(OnionObject::String(Arc::from(format!(
                        "<{}>",
                        obj.type_of().unwrap_or("object".to_string())
                    )))
                    .stabilize()),
                }
            }
        }
    })
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
        OnionObject::Boolean(b) => Ok(OnionObject::Boolean(*b).stabilize()),
        OnionObject::Integer(v) => Ok(OnionObject::Boolean(*v != 0).stabilize()),
        OnionObject::Float(v) => Ok(OnionObject::Boolean(*v != 0.0).stabilize()),
        OnionObject::String(s) => Ok(OnionObject::Boolean(!s.is_empty()).stabilize()),
        OnionObject::Bytes(bytes) => Ok(OnionObject::Boolean(!bytes.is_empty()).stabilize()),
        OnionObject::Null => Ok(OnionObject::Boolean(false).stabilize()),
        OnionObject::Undefined(_) => Ok(OnionObject::Boolean(false).stabilize()),
        OnionObject::Tuple(tuple) => {
            Ok(OnionObject::Boolean(!tuple.get_elements().is_empty()).stabilize())
        }
        _ => Ok(OnionObject::Boolean(true).stabilize()),
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
            OnionObject::Bytes(bytes) => {
                Ok(OnionObject::Bytes(bytes.clone()).stabilize())
            }
            OnionObject::String(s) => {
                Ok(OnionObject::Bytes(Arc::from(s.as_bytes().to_vec())).stabilize())
            }
            OnionObject::Integer(v) => {
                Ok(OnionObject::Bytes(Arc::from(v.to_be_bytes().to_vec())).stabilize())
            }
            OnionObject::Float(v) => {
                // 浮点数转字节数组（大端序）
                Ok(OnionObject::Bytes(Arc::from(v.to_be_bytes().to_vec())).stabilize())
            }
            OnionObject::Boolean(b) => {
                Ok(OnionObject::Bytes(Arc::from(vec![if *b { 1 } else { 0 }])).stabilize())
            }
            OnionObject::Tuple(tuple) => {
                let mut result = Vec::new();
                for element in tuple.get_elements().iter() {
                    match element {
                        OnionObject::Integer(v) => {
                            if *v >= 0 && *v <= 255 {
                                result.push(*v as u8);
                            } else {
                                return Err(RuntimeError::DetailedError(
                                    format!("Integer {} is out of byte range (0-255)", v).into(),
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
                Ok(OnionObject::Bytes(Arc::from(result)).stabilize())
            }
            _ => Err(RuntimeError::DetailedError(
                format!("Cannot convert {} to bytes", obj.type_of().unwrap_or("unknown".to_string())).into(),
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
        OnionObject::String(s) => Ok(OnionObject::Integer(s.len() as i64).stabilize()),
        OnionObject::Bytes(b) => Ok(OnionObject::Integer(b.len() as i64).stabilize()),
        OnionObject::Range(start, end) => {
            Ok(OnionObject::Integer((end - start) as i64).stabilize())
        }
        OnionObject::Tuple(tuple) => {
            Ok(OnionObject::Integer(tuple.get_elements().len() as i64).stabilize())
        }
        _ => Err(RuntimeError::DetailedError(
            format!(
                "Object of type {} does not have a length",
                obj.type_of().unwrap_or("unknown".to_string())
            )
            .into(),
        )),
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
            OnionObject::String(s) => {
                let elements = OnionTuple::new_static_no_ref(
                    &s.chars()
                        .map(|c| {
                            OnionStaticObject::new(OnionObject::String(Arc::from(c.to_string())))
                        })
                        .collect::<Vec<_>>(),
                );
                Ok(elements)
            }
            OnionObject::Bytes(b) => {
                let elements = OnionTuple::new_static_no_ref(
                    &b.iter()
                        .map(|byte| OnionStaticObject::new(OnionObject::Integer(*byte as i64)))
                        .collect::<Vec<_>>(),
                );
                Ok(elements)
            }
            OnionObject::Range(start, end) => {
                let elements = OnionTuple::new_static_no_ref(
                    &(*start..*end)
                        .map(|i| OnionStaticObject::new(OnionObject::Integer(i as i64)))
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
                    "Object of type {} does not have elements",
                    obj.type_of().unwrap_or("unknown".to_string())
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
    OnionLambdaDefinition::new_static_with_self(
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
