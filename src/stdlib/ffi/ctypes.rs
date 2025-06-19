use std::{collections::VecDeque, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};
use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionObjectExt, OnionStaticObject},
    GC,
};

use crate::stdlib::{build_named_dict, get_attr_direct, wrap_native_function};

#[derive(Clone, Debug)]
pub enum CTypes {
    // 现有的类型
    CInt16(i16),
    CInt32(i32),
    CInt64(i64),
    CFloat(f32),
    CDouble(f64),
    CBool(bool),
    CString(String),
    CBuffer(Vec<u8>),
    CPointer(usize),

    // 缺失的重要类型
    CInt8(i8),
    CUInt8(u8),
    CUInt16(u16),
    CUInt32(u32),
    CUInt64(u64),
    CChar(i8),     // C char (通常是有符号的)
    CUChar(u8),    // C unsigned char
    CSize(usize),  // size_t
    CSSize(isize), // ssize_t
    CVoid,         // void (单元类型)
    CNull,         // NULL 指针
}

impl GCTraceable<OnionObjectCell> for CTypes {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}

impl OnionObjectExt for CTypes {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        match self {
            CTypes::CInt16(value) => Ok(format!("CInt16({})", value)),
            CTypes::CInt32(value) => Ok(format!("CInt32({})", value)),
            CTypes::CInt64(value) => Ok(format!("CInt64({})", value)),
            CTypes::CFloat(value) => Ok(format!("CFloat({})", value)),
            CTypes::CDouble(value) => Ok(format!("CDouble({})", value)),
            CTypes::CBool(value) => Ok(format!("CBool({})", value)),
            CTypes::CString(value) => Ok(format!("CString(\"{}\")", value)),
            CTypes::CBuffer(value) => Ok(format!("CBuffer({:?})", value)),
            CTypes::CPointer(value) => Ok(format!("CPointer({:#x})", value)),
            CTypes::CInt8(value) => Ok(format!("CInt8({})", value)),
            CTypes::CUInt8(value) => Ok(format!("CUInt8({})", value)),
            CTypes::CUInt16(value) => Ok(format!("CUInt16({})", value)),
            CTypes::CUInt32(value) => Ok(format!("CUInt32({})", value)),
            CTypes::CUInt64(value) => Ok(format!("CUInt64({})", value)),
            CTypes::CChar(value) => Ok(format!("CChar({})", value)),
            CTypes::CUChar(value) => Ok(format!("CUChar({})", value)),
            CTypes::CSize(value) => Ok(format!("CSize({:#x})", value)),
            CTypes::CSSize(value) => Ok(format!("CSSize({:#x})", value)),
            CTypes::CVoid => Ok("CVoid".to_string()),
            CTypes::CNull => Ok("CNull".to_string()),
        }
    }

    fn to_string(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        match self {
            CTypes::CInt16(value) => Ok(format!("CInt16({})", value)),
            CTypes::CInt32(value) => Ok(format!("CInt32({})", value)),
            CTypes::CInt64(value) => Ok(format!("CInt64({})", value)),
            CTypes::CFloat(value) => Ok(format!("CFloat({})", value)),
            CTypes::CDouble(value) => Ok(format!("CDouble({})", value)),
            CTypes::CBool(value) => Ok(format!("CBool({})", value)),
            CTypes::CString(value) => Ok(format!("CString(\"{}\")", value)),
            CTypes::CBuffer(value) => Ok(format!("CBuffer({:?})", value)),
            CTypes::CPointer(value) => Ok(format!("CPointer({:#x})", value)),
            CTypes::CInt8(value) => Ok(format!("CInt8({})", value)),
            CTypes::CUInt8(value) => Ok(format!("CUInt8({})", value)),
            CTypes::CUInt16(value) => Ok(format!("CUInt16({})", value)),
            CTypes::CUInt32(value) => Ok(format!("CUInt32({})", value)),
            CTypes::CUInt64(value) => Ok(format!("CUInt64({})", value)),
            CTypes::CChar(value) => Ok(format!("CChar({})", value)),
            CTypes::CUChar(value) => Ok(format!("CUChar({})", value)),
            CTypes::CSize(value) => Ok(format!("CSize({:#x})", value)),
            CTypes::CSSize(value) => Ok(format!("CSSize({:#x})", value)),
            CTypes::CVoid => Ok("CVoid".to_string()),
            CTypes::CNull => Ok("CNull".to_string()),
        }
    }
    fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        match self {
            CTypes::CInt16(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CInt32(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CInt64(value) => Ok(OnionObject::Integer(*value).stabilize()),
            CTypes::CFloat(value) => Ok(OnionObject::Float(*value as f64).stabilize()),
            CTypes::CDouble(value) => Ok(OnionObject::Float(*value).stabilize()),
            CTypes::CBool(value) => Ok(OnionObject::Boolean(*value).stabilize()),
            CTypes::CString(value) => Ok(OnionObject::String(value.clone().into()).stabilize()),
            CTypes::CBuffer(value) => Ok(OnionObject::Bytes(value.clone().into()).stabilize()),
            CTypes::CPointer(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CInt8(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CUInt8(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CUInt16(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CUInt32(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CUInt64(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CChar(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CUChar(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CSize(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CSSize(value) => Ok(OnionObject::Integer(*value as i64).stabilize()),
            CTypes::CVoid => Err(RuntimeError::InvalidOperation(
                "CVoid has no value".to_string().into(),
            )),
            CTypes::CNull => Err(RuntimeError::InvalidOperation(
                "CNull has no value".to_string().into(),
            )),
        }
    }

    fn equals(&self, _other: &OnionObject) -> Result<bool, RuntimeError> {
        Ok(false)
    }

    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {
        // CTypes 不需要特殊的升级逻辑，因为它们是基本类型
    }

    fn reconstruct_container(&self) -> Result<OnionObject, RuntimeError> {
        return Ok(OnionObject::Custom(Arc::new(self.clone())));
    }

    fn is_same(&self, _other: &OnionObject) -> Result<bool, RuntimeError> {
        Ok(false)
    }

    fn with_attribute(
        &self,
        key: &OnionObject,
        f: &mut dyn FnMut(&OnionObject) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        if let OnionObject::String(attr) = key {
            match attr.as_ref().as_str() {
                "pointer" => {
                    if let CTypes::CPointer(addr) = self {
                        f(&OnionObject::Integer(*addr as i64))
                    } else {
                        Err(RuntimeError::InvalidOperation(
                            "Attribute 'pointer' is not applicable".to_string().into(),
                        ))
                    }
                }
                _ => Err(RuntimeError::InvalidOperation(
                    format!("Attribute '{}' is not applicable for CTypes", attr).into(),
                )),
            }
        } else {
            Err(RuntimeError::InvalidOperation(
                "Attribute key must be a string".to_string().into(),
            ))
        }
    }

    fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        match self {
            CTypes::CBuffer(bytes) => Ok(OnionObject::Integer(bytes.len() as i64).stabilize()),
            CTypes::CString(s) => Ok(OnionObject::Integer(s.len() as i64).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "len() is not applicable for this CTypes variant"
                    .to_string()
                    .into(),
            )),
        }
    }
}

// 构造函数实现

// 有符号整数类型
fn c_int8(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= i8::MIN as i64 && *n <= i8::MAX as i64 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CInt8(*n as i8))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Value {} out of range for i8", n).into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_int8 requires integer value".to_string().into(),
            )),
        })
    })
}

fn c_int16(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= i16::MIN as i64 && *n <= i16::MAX as i64 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CInt16(*n as i16))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Value {} out of range for i16", n).into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_int16 requires integer value".to_string().into(),
            )),
        })
    })
}

fn c_int32(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CInt32(*n as i32))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Value {} out of range for i32", n).into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_int32 requires integer value".to_string().into(),
            )),
        })
    })
}

fn c_int64(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                Ok(OnionObject::Custom(Arc::new(CTypes::CInt64(*n))).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_int64 requires integer value".to_string().into(),
            )),
        })
    })
}

// 无符号整数类型
fn c_uint8(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= 0 && *n <= u8::MAX as i64 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CUInt8(*n as u8))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Value {} out of range for u8", n).into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_uint8 requires integer value".to_string().into(),
            )),
        })
    })
}

fn c_uint16(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= 0 && *n <= u16::MAX as i64 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CUInt16(*n as u16))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Value {} out of range for u16", n).into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_uint16 requires integer value".to_string().into(),
            )),
        })
    })
}

fn c_uint32(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= 0 && *n <= u32::MAX as i64 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CUInt32(*n as u32))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Value {} out of range for u32", n).into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_uint32 requires integer value".to_string().into(),
            )),
        })
    })
}

fn c_uint64(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= 0 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CUInt64(*n as u64))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "c_uint64 requires non-negative value".to_string().into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_uint64 requires integer value".to_string().into(),
            )),
        })
    })
}

// 浮点数类型
fn c_float(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Float(f) => {
                Ok(OnionObject::Custom(Arc::new(CTypes::CFloat(*f as f32))).stabilize())
            }
            OnionObject::Integer(n) => {
                Ok(OnionObject::Custom(Arc::new(CTypes::CFloat(*n as f32))).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_float requires numeric value".to_string().into(),
            )),
        })
    })
}

fn c_double(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Float(f) => {
                Ok(OnionObject::Custom(Arc::new(CTypes::CDouble(*f))).stabilize())
            }
            OnionObject::Integer(n) => {
                Ok(OnionObject::Custom(Arc::new(CTypes::CDouble(*n as f64))).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_double requires numeric value".to_string().into(),
            )),
        })
    })
}

// 字符类型
fn c_char(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= i8::MIN as i64 && *n <= i8::MAX as i64 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CChar(*n as i8))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Value {} out of range for char", n).into(),
                    ))
                }
            }
            OnionObject::String(s) => {
                if let Some(c) = s.chars().next() {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CChar(c as i8))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Empty string cannot be converted to char"
                            .to_string()
                            .into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_char requires integer or string value".to_string().into(),
            )),
        })
    })
}

fn c_uchar(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= 0 && *n <= u8::MAX as i64 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CUChar(*n as u8))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Value {} out of range for unsigned char", n).into(),
                    ))
                }
            }
            OnionObject::String(s) => {
                if let Some(c) = s.chars().next() {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CUChar(c as u8))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Empty string cannot be converted to unsigned char"
                            .to_string()
                            .into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_uchar requires integer or string value"
                    .to_string()
                    .into(),
            )),
        })
    })
}

// 布尔类型
fn c_bool(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Boolean(b) => {
                Ok(OnionObject::Custom(Arc::new(CTypes::CBool(*b))).stabilize())
            }
            OnionObject::Integer(n) => {
                Ok(OnionObject::Custom(Arc::new(CTypes::CBool(*n != 0))).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_bool requires boolean or integer value"
                    .to_string()
                    .into(),
            )),
        })
    })
}

// 字符串类型
fn c_string(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::String(s) => {
                Ok(OnionObject::Custom(Arc::new(CTypes::CString(s.as_ref().clone()))).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_string requires string value".to_string().into(),
            )),
        })
    })
}

// 缓冲区类型
fn c_buffer(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Bytes(bytes) => Ok(OnionObject::Custom(Arc::new(CTypes::CBuffer(
                bytes.as_ref().clone(),
            )))
            .stabilize()),
            OnionObject::String(s) => {
                // 将字符串转换为 UTF-8 字节
                Ok(
                    OnionObject::Custom(Arc::new(CTypes::CBuffer(s.as_bytes().to_vec())))
                        .stabilize(),
                )
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_buffer requires bytes or string value".to_string().into(),
            )),
        })
    })
}

// 指针类型
fn c_pointer(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= 0 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CPointer(*n as usize))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Pointer address must be non-negative".to_string().into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_pointer requires integer value".to_string().into(),
            )),
        })
    })
}

// 大小类型
fn c_size(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n >= 0 {
                    Ok(OnionObject::Custom(Arc::new(CTypes::CSize(*n as usize))).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "size_t must be non-negative".to_string().into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_size requires integer value".to_string().into(),
            )),
        })
    })
}

fn c_ssize(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                Ok(OnionObject::Custom(Arc::new(CTypes::CSSize(*n as isize))).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "c_ssize requires integer value".to_string().into(),
            )),
        })
    })
}

// 空类型和空指针
fn c_void(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    Ok(OnionObject::Custom(Arc::new(CTypes::CVoid)).stabilize())
}

fn c_null(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    Ok(OnionObject::Custom(Arc::new(CTypes::CNull)).stabilize())
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // 有符号整数类型构造函数
    let mut int8_params = IndexMap::new();
    int8_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some(
            "Integer value for i8 (-128 to 127)".to_string().into(),
        ))
        .stabilize(),
    );
    module.insert(
        "i8".to_string(),
        wrap_native_function(
            &build_named_dict(int8_params),
            None,
            None,
            "ctypes::i8".to_string(),
            &c_int8,
        ),
    );

    let mut int16_params = IndexMap::new();
    int16_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some(
            "Integer value for i16 (-32768 to 32767)".to_string().into(),
        ))
        .stabilize(),
    );
    module.insert(
        "i16".to_string(),
        wrap_native_function(
            &build_named_dict(int16_params),
            None,
            None,
            "ctypes::i16".to_string(),
            &c_int16,
        ),
    );

    let mut int32_params = IndexMap::new();
    int32_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Integer value for i32".to_string().into())).stabilize(),
    );
    module.insert(
        "i32".to_string(),
        wrap_native_function(
            &build_named_dict(int32_params),
            None,
            None,
            "ctypes::i32".to_string(),
            &c_int32,
        ),
    );

    let mut int64_params = IndexMap::new();
    int64_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Integer value for i64".to_string().into())).stabilize(),
    );
    module.insert(
        "i64".to_string(),
        wrap_native_function(
            &build_named_dict(int64_params),
            None,
            None,
            "ctypes::i64".to_string(),
            &c_int64,
        ),
    );

    // 无符号整数类型构造函数
    let mut uint8_params = IndexMap::new();
    uint8_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Integer value for u8 (0 to 255)".to_string().into()))
            .stabilize(),
    );
    module.insert(
        "u8".to_string(),
        wrap_native_function(
            &build_named_dict(uint8_params),
            None,
            None,
            "ctypes::u8".to_string(),
            &c_uint8,
        ),
    );

    let mut uint16_params = IndexMap::new();
    uint16_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some(
            "Integer value for u16 (0 to 65535)".to_string().into(),
        ))
        .stabilize(),
    );
    module.insert(
        "u16".to_string(),
        wrap_native_function(
            &build_named_dict(uint16_params),
            None,
            None,
            "ctypes::u16".to_string(),
            &c_uint16,
        ),
    );

    let mut uint32_params = IndexMap::new();
    uint32_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Integer value for u32".to_string().into())).stabilize(),
    );
    module.insert(
        "u32".to_string(),
        wrap_native_function(
            &build_named_dict(uint32_params),
            None,
            None,
            "ctypes::u32".to_string(),
            &c_uint32,
        ),
    );

    let mut uint64_params = IndexMap::new();
    uint64_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Integer value for u64".to_string().into())).stabilize(),
    );
    module.insert(
        "u64".to_string(),
        wrap_native_function(
            &build_named_dict(uint64_params),
            None,
            None,
            "ctypes::u64".to_string(),
            &c_uint64,
        ),
    );

    // 浮点数类型构造函数
    let mut float_params = IndexMap::new();
    float_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Numeric value for f32".to_string().into())).stabilize(),
    );
    module.insert(
        "f32".to_string(),
        wrap_native_function(
            &build_named_dict(float_params),
            None,
            None,
            "ctypes::f32".to_string(),
            &c_float,
        ),
    );

    let mut double_params = IndexMap::new();
    double_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Numeric value for f64".to_string().into())).stabilize(),
    );
    module.insert(
        "f64".to_string(),
        wrap_native_function(
            &build_named_dict(double_params),
            None,
            None,
            "ctypes::f64".to_string(),
            &c_double,
        ),
    );

    // 字符类型构造函数
    let mut char_params = IndexMap::new();
    char_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Integer or string value for char".to_string().into()))
            .stabilize(),
    );
    module.insert(
        "char".to_string(),
        wrap_native_function(
            &build_named_dict(char_params),
            None,
            None,
            "ctypes::char".to_string(),
            &c_char,
        ),
    );

    let mut uchar_params = IndexMap::new();
    uchar_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some(
            "Integer or string value for unsigned char"
                .to_string()
                .into(),
        ))
        .stabilize(),
    );
    module.insert(
        "uchar".to_string(),
        wrap_native_function(
            &build_named_dict(uchar_params),
            None,
            None,
            "ctypes::uchar".to_string(),
            &c_uchar,
        ),
    );

    // 布尔类型构造函数
    let mut bool_params = IndexMap::new();
    bool_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Boolean or integer value".to_string().into())).stabilize(),
    );
    module.insert(
        "bool".to_string(),
        wrap_native_function(
            &build_named_dict(bool_params),
            None,
            None,
            "ctypes::bool".to_string(),
            &c_bool,
        ),
    );

    // 字符串类型构造函数
    let mut string_params = IndexMap::new();
    string_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("String value".to_string().into())).stabilize(),
    );
    module.insert(
        "string".to_string(),
        wrap_native_function(
            &build_named_dict(string_params),
            None,
            None,
            "ctypes::string".to_string(),
            &c_string,
        ),
    ); // 缓冲区类型构造函数
    let mut buffer_params = IndexMap::new();
    buffer_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Bytes or string value".to_string().into())).stabilize(),
    );
    module.insert(
        "buffer".to_string(),
        wrap_native_function(
            &build_named_dict(buffer_params),
            None,
            None,
            "ctypes::buffer".to_string(),
            &c_buffer,
        ),
    );

    // 指针类型构造函数
    let mut pointer_params = IndexMap::new();
    pointer_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Integer address value".to_string().into())).stabilize(),
    );
    module.insert(
        "pointer".to_string(),
        wrap_native_function(
            &build_named_dict(pointer_params),
            None,
            None,
            "ctypes::pointer".to_string(),
            &c_pointer,
        ),
    );

    // 大小类型构造函数
    let mut size_params = IndexMap::new();
    size_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Non-negative integer for size_t".to_string().into()))
            .stabilize(),
    );
    module.insert(
        "size".to_string(),
        wrap_native_function(
            &build_named_dict(size_params),
            None,
            None,
            "ctypes::size".to_string(),
            &c_size,
        ),
    );

    let mut ssize_params = IndexMap::new();
    ssize_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Integer for ssize_t".to_string().into())).stabilize(),
    );
    module.insert(
        "ssize".to_string(),
        wrap_native_function(
            &build_named_dict(ssize_params),
            None,
            None,
            "ctypes::ssize".to_string(),
            &c_ssize,
        ),
    );

    // 特殊类型构造函数
    module.insert(
        "void".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "ctypes::void".to_string(),
            &c_void,
        ),
    );

    module.insert(
        "null".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "ctypes::null".to_string(),
            &c_null,
        ),
    );

    build_named_dict(module)
}
