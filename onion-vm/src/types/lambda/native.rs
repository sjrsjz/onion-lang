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

pub struct NativeMethodGenerator<F>
where
    F: Fn(
            &OnionStaticObject,
            &OnionFastMap<Box<str>, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + 'static,
{
    captured: OnionFastMap<Box<str>, OnionStaticObject>,
    self_object: OnionStaticObject,
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
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        unwrap_step_result!(
            (self.function)(&self.self_object, &self.captured, gc)
                .map(|result| StepResult::Return(result.into()))
        )
    }
    fn format_context(&self) -> String {
        // Get the type name of the closure/function pointer. This is the best
        // static information we have to identify which native code is running.
        let full_type_name = std::any::type_name_of_val(self.function);

        // Often, the full type name is very long (e.g., my_app::modules::math::add::{{closure}}).
        // Let's try to show just the most specific part for readability.
        let short_type_name = full_type_name.split("::").last().unwrap_or(full_type_name);

        let self_info = format!("{:?}", self.self_object);

        // Assemble all the information into a clear, multi-line string.
        format!(
            "-> Executing Native Method:\n   - Function: {} (Full Type: {})\n   - Self (this): {}\n   - Argument: {:?}",
            short_type_name,
            full_type_name, // It's good to include the full name for detailed debugging
            self_info,
            self.captured
        )
    }
}

pub(crate) fn native_int_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| {
        match obj {
            OnionObject::Integer(v) => {
                // 如果已经是整数，直接返回
                Ok(OnionObject::Integer(*v).stabilize())
            }
            OnionObject::Float(v) => {
                // 浮点数转整数（截断）
                Ok(OnionObject::Integer(*v as i64).stabilize())
            }
            OnionObject::String(s) => {
                // 字符串解析为整数
                match s.parse::<i64>() {
                    Ok(parsed) => Ok(OnionObject::Integer(parsed).stabilize()),
                    Err(_) => Err(RuntimeError::DetailedError(
                        format!("Cannot convert string '{}' to integer", s).into(),
                    )),
                }
            }
            OnionObject::Boolean(b) => {
                // 布尔值转整数：true->1, false->0
                Ok(OnionObject::Integer(if *b { 1 } else { 0 }).stabilize())
            }
            OnionObject::Bytes(bytes) => {
                // 如果字节数组可以解析为UTF-8字符串，再转整数
                match std::str::from_utf8(bytes) {
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
                }
            }
            _ => Err(RuntimeError::DetailedError(
                format!(
                    "Cannot convert {} to integer",
                    obj.type_of().unwrap_or("unknown".to_string())
                )
                .into(),
            )),
        }
    })
}

pub(crate) fn native_float_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| {
        match obj {
            OnionObject::Float(v) => {
                // 如果已经是浮点数，直接返回
                Ok(OnionObject::Float(*v).stabilize())
            }
            OnionObject::Integer(v) => {
                // 整数转浮点数
                Ok(OnionObject::Float(*v as f64).stabilize())
            }
            OnionObject::String(s) => {
                // 字符串解析为浮点数
                match s.parse::<f64>() {
                    Ok(parsed) => Ok(OnionObject::Float(parsed).stabilize()),
                    Err(_) => Err(RuntimeError::DetailedError(
                        format!("Cannot convert string '{}' to float", s).into(),
                    )),
                }
            }
            OnionObject::Boolean(b) => {
                // 布尔值转浮点数：true->1.0, false->0.0
                Ok(OnionObject::Float(if *b { 1.0 } else { 0.0 }).stabilize())
            }
            OnionObject::Bytes(bytes) => {
                // 如果字节数组可以解析为UTF-8字符串，再转浮点数
                match std::str::from_utf8(bytes) {
                    Ok(s) => match s.parse::<f64>() {
                        Ok(parsed) => Ok(OnionObject::Float(parsed).stabilize()),
                        Err(_) => Err(RuntimeError::DetailedError(
                            format!("Cannot convert bytes to float: invalid format").into(),
                        )),
                    },
                    Err(_) => Err(RuntimeError::DetailedError(
                        "Cannot convert non-UTF8 bytes to float".into(),
                    )),
                }
            }
            _ => Err(RuntimeError::DetailedError(
                format!(
                    "Cannot convert {} to float",
                    obj.type_of().unwrap_or("unknown".to_string())
                )
                .into(),
            )),
        }
    })
}

pub(crate) fn native_string_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| {
        match obj {
            OnionObject::String(s) => {
                // 如果已经是字符串，直接返回
                Ok(OnionObject::String(s.clone()).stabilize())
            }
            OnionObject::Integer(v) => {
                // 整数转字符串
                Ok(OnionObject::String(Arc::from(v.to_string())).stabilize())
            }
            OnionObject::Float(v) => {
                // 浮点数转字符串
                Ok(OnionObject::String(Arc::from(v.to_string())).stabilize())
            }
            OnionObject::Boolean(b) => {
                // 布尔值转字符串
                Ok(OnionObject::String(Arc::from(b.to_string())).stabilize())
            }
            OnionObject::Bytes(bytes) => {
                // 字节数组转字符串（UTF-8）
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

pub(crate) fn native_bool_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| {
        match obj {
            OnionObject::Boolean(b) => {
                // 如果已经是布尔值，直接返回
                Ok(OnionObject::Boolean(*b).stabilize())
            }
            OnionObject::Integer(v) => {
                // 整数转布尔值：0为false，非0为true
                Ok(OnionObject::Boolean(*v != 0).stabilize())
            }
            OnionObject::Float(v) => {
                // 浮点数转布尔值：0.0为false，非0为true
                Ok(OnionObject::Boolean(*v != 0.0).stabilize())
            }
            OnionObject::String(s) => {
                // 字符串转布尔值：空字符串为false，非空为true
                Ok(OnionObject::Boolean(!s.is_empty()).stabilize())
            }
            OnionObject::Bytes(bytes) => {
                // 字节数组转布尔值：空数组为false，非空为true
                Ok(OnionObject::Boolean(!bytes.is_empty()).stabilize())
            }
            OnionObject::Null => {
                // null 为 false
                Ok(OnionObject::Boolean(false).stabilize())
            }
            OnionObject::Undefined(_) => {
                // undefined 为 false
                Ok(OnionObject::Boolean(false).stabilize())
            }
            OnionObject::Tuple(tuple) => {
                // 元组转布尔值：空元组为false，非空为true
                Ok(OnionObject::Boolean(!tuple.get_elements().is_empty()).stabilize())
            }
            _ => {
                // 其他对象都为 true（存在即为真）
                Ok(OnionObject::Boolean(true).stabilize())
            }
        }
    })
}

pub(crate) fn native_bytes_converter(
    self_object: &OnionStaticObject,
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    self_object.weak().with_data(|obj: &OnionObject| {
        match obj {
            OnionObject::Bytes(bytes) => {
                // 如果已经是字节数组，直接返回
                Ok(OnionObject::Bytes(bytes.clone()).stabilize())
            }
            OnionObject::String(s) => {
                // 字符串转字节数组（UTF-8编码）
                Ok(OnionObject::Bytes(Arc::from(s.as_bytes().to_vec())).stabilize())
            }
            OnionObject::Integer(v) => {
                // 整数转字节数组（大端序）
                Ok(OnionObject::Bytes(Arc::from(v.to_be_bytes().to_vec())).stabilize())
            }
            OnionObject::Float(v) => {
                // 浮点数转字节数组（大端序）
                Ok(OnionObject::Bytes(Arc::from(v.to_be_bytes().to_vec())).stabilize())
            }
            OnionObject::Boolean(b) => {
                // 布尔值转字节数组：true->1, false->0
                Ok(OnionObject::Bytes(Arc::from(vec![if *b { 1 } else { 0 }])).stabilize())
            }
            OnionObject::Tuple(tuple) => {
                // 元组转字节数组：尝试将每个元素转换为字节
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
        LambdaType::Normal,
    )
}
