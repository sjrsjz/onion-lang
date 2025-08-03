use std::{collections::VecDeque, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};
use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionObjectExt, OnionStaticObject},
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

// 引入所需的辅助函数
use crate::stdlib::{build_dict, wrap_native_function};

// --- Core CTypes Enum and OnionObjectExt Impl (Unchanged) ---
// This part defines the behavior of the custom types and is already well-designed.
#[derive(Clone, Debug)]
pub enum CTypes {
    CInt16(i16),
    CInt32(i32),
    CInt64(i64),
    CFloat(f32),
    CDouble(f64),
    CBool(bool),
    CString(String),
    CBuffer(Vec<u8>),
    CPointer(usize),
    CInt8(i8),
    CUInt8(u8),
    CUInt16(u16),
    CUInt32(u32),
    CUInt64(u64),
    CChar(i8),
    CUChar(u8),
    CSize(usize),
    CSSize(isize),
    CVoid,
    CNull,
}

impl GCTraceable<OnionObjectCell> for CTypes {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}

// The OnionObjectExt implementation remains the same.
impl OnionObjectExt for CTypes {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        match self {
            CTypes::CInt8(v) => Ok(format!("ctypes.i8({v})")),
            CTypes::CInt16(v) => Ok(format!("ctypes.i16({v})")),
            CTypes::CInt32(v) => Ok(format!("ctypes.i32({v})")),
            CTypes::CInt64(v) => Ok(format!("ctypes.i64({v})")),
            CTypes::CUInt8(v) => Ok(format!("ctypes.u8({v})")),
            CTypes::CUInt16(v) => Ok(format!("ctypes.u16({v})")),
            CTypes::CUInt32(v) => Ok(format!("ctypes.u32({v})")),
            CTypes::CUInt64(v) => Ok(format!("ctypes.u64({v})")),
            CTypes::CFloat(v) => Ok(format!("ctypes.f32({v})")),
            CTypes::CDouble(v) => Ok(format!("ctypes.f64({v})")),
            CTypes::CBool(v) => Ok(format!("ctypes.bool({v})")),
            CTypes::CString(v) => Ok(format!("ctypes.string({v:?})")),
            CTypes::CBuffer(v) => Ok(format!("ctypes.buffer(len={})", v.len())),
            CTypes::CPointer(v) => Ok(format!("ctypes.pointer({v:#x})")),
            CTypes::CChar(v) => Ok(format!("ctypes.char({v})")),
            CTypes::CUChar(v) => Ok(format!("ctypes.uchar({v})")),
            CTypes::CSize(v) => Ok(format!("ctypes.size({v})")),
            CTypes::CSSize(v) => Ok(format!("ctypes.ssize({v})")),
            CTypes::CVoid => Ok("ctypes.void".to_string()),
            CTypes::CNull => Ok("ctypes.null".to_string()),
        }
    }
    // value_of, equals, etc. also remain unchanged.
    fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        match self {
            CTypes::CInt8(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CInt16(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CInt32(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CInt64(v) => Ok(OnionObject::Integer(*v).stabilize()),
            CTypes::CUInt8(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CUInt16(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CUInt32(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CUInt64(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CFloat(v) => Ok(OnionObject::Float(*v as f64).stabilize()),
            CTypes::CDouble(v) => Ok(OnionObject::Float(*v).stabilize()),
            CTypes::CBool(v) => Ok(OnionObject::Boolean(*v).stabilize()),
            CTypes::CString(v) => Ok(OnionObject::String(v.clone().into()).stabilize()),
            CTypes::CBuffer(v) => Ok(OnionObject::Bytes(v.clone().into()).stabilize()),
            CTypes::CPointer(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CChar(v) => Ok(OnionObject::String(
                std::str::from_utf8(&[*v as u8])
                    .unwrap_or("")
                    .to_string()
                    .into(),
            )
            .stabilize()),
            CTypes::CUChar(v) => Ok(OnionObject::String(
                std::str::from_utf8(&[*v]).unwrap_or("").into(),
            )
            .stabilize()),
            CTypes::CSize(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CSSize(v) => Ok(OnionObject::Integer(*v as i64).stabilize()),
            CTypes::CVoid | CTypes::CNull => Ok(OnionObject::Null.stabilize()),
        }
    }
    fn equals(&self, _other: &OnionObject) -> Result<bool, RuntimeError> {
        Ok(false)
    }
    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {}
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
                "value" => {
                    let v = self.value_of()?;
                    f(v.weak())
                }
                _ => Err(RuntimeError::InvalidOperation(
                    format!("Attribute '{attr}' not found on CTypes object").into(),
                )),
            }
        } else {
            Err(RuntimeError::InvalidType(
                "Attribute key must be a string".into(),
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

// --- Argument Parsing Helper Functions ---

fn get_value_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
) -> Result<&OnionStaticObject, RuntimeError> {
    argument.get(&"value".to_string()).ok_or_else(|| {
        RuntimeError::DetailedError("Function requires a 'value' argument".into())
    })
}

fn get_integer_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
) -> Result<i64, RuntimeError> {
    let obj = get_value_arg(argument)?;
    match obj.weak() {
        OnionObject::Integer(i) => Ok(*i),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' must be an integer".into(),
        )),
    }
}

enum NumericArg {
    Int(i64),
    Float(f64),
}

fn get_numeric_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
) -> Result<NumericArg, RuntimeError> {
    let obj = get_value_arg(argument)?;
    match obj.weak() {
        OnionObject::Integer(i) => Ok(NumericArg::Int(*i)),
        OnionObject::Float(f) => Ok(NumericArg::Float(*f)),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' must be a numeric type (integer or float)"
                .to_string()
                .into(),
        )),
    }
}

// --- Refactored Constructor Functions ---

macro_rules! c_int_constructor {
    ($name:ident, $type:ty, $ctype_variant:ident) => {
        fn $name(
            argument: &OnionFastMap<Box<str>, OnionStaticObject>,
            _gc: &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError> {
            let n = get_integer_arg(argument)?;
            let min = <$type>::MIN as i64;
            let max = <$type>::MAX as i64;
            if n >= min && n <= max {
                Ok(OnionObject::Custom(Arc::new(CTypes::$ctype_variant(n as $type))).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    format!("Value {} out of range for {}", n, stringify!($type)).into(),
                ))
            }
        }
    };
}

macro_rules! c_uint_constructor {
    ($name:ident, $type:ty, $ctype_variant:ident) => {
        fn $name(
            argument: &OnionFastMap<Box<str>, OnionStaticObject>,
            _gc: &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError> {
            let n = get_integer_arg(argument)?;
            if n >= 0 && n <= <$type>::MAX as i64 {
                Ok(OnionObject::Custom(Arc::new(CTypes::$ctype_variant(n as $type))).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    format!("Value {} out of range for {}", n, stringify!($type)).into(),
                ))
            }
        }
    };
}

c_int_constructor!(c_int8, i8, CInt8);
c_int_constructor!(c_int16, i16, CInt16);
c_int_constructor!(c_int32, i32, CInt32);

c_uint_constructor!(c_uint8, u8, CUInt8);
c_uint_constructor!(c_uint16, u16, CUInt16);
c_uint_constructor!(c_uint32, u32, CUInt32);

fn c_int64(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let n = get_integer_arg(argument)?;
    Ok(OnionObject::Custom(Arc::new(CTypes::CInt64(n))).stabilize())
}

fn c_uint64(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let n = get_integer_arg(argument)?;
    if n >= 0 {
        Ok(OnionObject::Custom(Arc::new(CTypes::CUInt64(n as u64))).stabilize())
    } else {
        Err(RuntimeError::InvalidOperation(
            "c_uint64 requires a non-negative value".into(),
        ))
    }
}

fn c_float(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let val = match get_numeric_arg(argument)? {
        NumericArg::Int(i) => i as f32,
        NumericArg::Float(f) => f as f32,
    };
    Ok(OnionObject::Custom(Arc::new(CTypes::CFloat(val))).stabilize())
}

fn c_double(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let val = match get_numeric_arg(argument)? {
        NumericArg::Int(i) => i as f64,
        NumericArg::Float(f) => f,
    };
    Ok(OnionObject::Custom(Arc::new(CTypes::CDouble(val))).stabilize())
}

fn c_char(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value_obj = get_value_arg(argument)?;
    match value_obj.weak() {
        OnionObject::Integer(n) => {
            if *n >= i8::MIN as i64 && *n <= i8::MAX as i64 {
                Ok(OnionObject::Custom(Arc::new(CTypes::CChar(*n as i8))).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    format!("Value {n} out of range for char").into(),
                ))
            }
        }
        OnionObject::String(s) if s.chars().count() == 1 => Ok(OnionObject::Custom(Arc::new(
            CTypes::CChar(s.chars().next().unwrap() as i8),
        ))
        .stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "c_char requires an integer or a single-character string"
                .to_string()
                .into(),
        )),
    }
}

fn c_uchar(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value_obj = get_value_arg(argument)?;
    match value_obj.weak() {
        OnionObject::Integer(n) => {
            if *n >= 0 && *n <= u8::MAX as i64 {
                Ok(OnionObject::Custom(Arc::new(CTypes::CUChar(*n as u8))).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    format!("Value {n} out of range for unsigned char").into(),
                ))
            }
        }
        OnionObject::String(s) if s.chars().count() == 1 => Ok(OnionObject::Custom(Arc::new(
            CTypes::CUChar(s.chars().next().unwrap() as u8),
        ))
        .stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "c_uchar requires a non-negative integer or a single-character string"
                .to_string()
                .into(),
        )),
    }
}

fn c_bool(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value_obj = get_value_arg(argument)?;
    let b = value_obj.weak().to_boolean()?;
    Ok(OnionObject::Custom(Arc::new(CTypes::CBool(b))).stabilize())
}

fn c_string(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value_obj = get_value_arg(argument)?;
    let s = value_obj.weak().to_string(&vec![])?;
    Ok(OnionObject::Custom(Arc::new(CTypes::CString(s))).stabilize())
}

fn c_buffer(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value_obj = get_value_arg(argument)?;
    match value_obj.weak() {
        OnionObject::Bytes(b) => {
            Ok(OnionObject::Custom(Arc::new(CTypes::CBuffer(b.to_vec()))).stabilize())
        }
        OnionObject::String(s) => {
            Ok(OnionObject::Custom(Arc::new(CTypes::CBuffer(s.as_bytes().to_vec()))).stabilize())
        }
        _ => Err(RuntimeError::InvalidType(
            "c_buffer requires bytes or a string".into(),
        )),
    }
}

fn c_pointer(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let n = get_integer_arg(argument)?;
    if n >= 0 {
        Ok(OnionObject::Custom(Arc::new(CTypes::CPointer(n as usize))).stabilize())
    } else {
        Err(RuntimeError::InvalidOperation(
            "Pointer address must be non-negative".into(),
        ))
    }
}

fn c_size(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let n = get_integer_arg(argument)?;
    if n >= 0 {
        Ok(OnionObject::Custom(Arc::new(CTypes::CSize(n as usize))).stabilize())
    } else {
        Err(RuntimeError::InvalidOperation(
            "size_t must be non-negative".into(),
        ))
    }
}

fn c_ssize(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let n = get_integer_arg(argument)?;
    Ok(OnionObject::Custom(Arc::new(CTypes::CSSize(n as isize))).stabilize())
}

fn c_void(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    Ok(OnionObject::Custom(Arc::new(CTypes::CVoid)).stabilize())
}

fn c_null(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    Ok(OnionObject::Custom(Arc::new(CTypes::CNull)).stabilize())
}

// --- Module Build Function ---

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    macro_rules! register_ctype {
        ($name:expr, $func:ident) => {
            module.insert(
                $name.to_string(),
                wrap_native_function(
                    LambdaParameter::top("value"),
                    OnionFastMap::default(),
                    concat!("ctypes::", $name).to_string(),
                    OnionKeyPool::create(vec!["value".to_string()]),
                    &$func,
                ),
            );
        };
    }
    register_ctype!("i8", c_int8);
    register_ctype!("i16", c_int16);
    register_ctype!("i32", c_int32);
    register_ctype!("i64", c_int64);
    register_ctype!("u8", c_uint8);
    register_ctype!("u16", c_uint16);
    register_ctype!("u32", c_uint32);
    register_ctype!("u64", c_uint64);
    register_ctype!("f32", c_float);
    register_ctype!("f64", c_double);
    register_ctype!("char", c_char);
    register_ctype!("uchar", c_uchar);
    register_ctype!("bool", c_bool);
    register_ctype!("string", c_string);
    register_ctype!("buffer", c_buffer);
    register_ctype!("pointer", c_pointer);
    register_ctype!("size", c_size);
    register_ctype!("ssize", c_ssize);

    module.insert(
        "void".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(vec![]),
            OnionFastMap::default(),
            "ctypes::void".to_string(),
            OnionKeyPool::create(vec![]),
            &c_void,
        ),
    );
    module.insert(
        "null".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(vec![]),
            OnionFastMap::default(),
            "ctypes::null".to_string(),
            OnionKeyPool::create(vec![]),
            &c_null,
        ),
    );

    build_dict(module)
}
