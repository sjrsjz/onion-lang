use std::{collections::VecDeque, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};
use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError, smallvec::SmallVec, types::{
        boolean_value::OnionBooleanValue,
        bytes_value::OnionBytesValue,
        float_value::OnionFloatValue,
        integer_value::OnionIntegerValue,
        lambda::parameter::LambdaParameter,
        null::OnionNull,
        object::{
            OnionObject, OnionObjectCell, OnionObjectProtocol, OnionObjectProtocolAny,
            OnionStaticObject,
        },
        string_value::OnionStringValue,
    }, utils::fastmap::{OnionFastMap, OnionKeyPool}, GC
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
impl OnionObjectProtocol for CTypes {
    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("CTypes".into())
    }

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

    fn display(&self, ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        self.repr(ptrs)
    }

    // value_of, equals, etc. also remain unchanged.
    fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        match self {
            CTypes::CInt8(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CInt16(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CInt32(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CInt64(v) => Ok(OnionIntegerValue::new_static(*v)),
            CTypes::CUInt8(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CUInt16(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CUInt32(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CUInt64(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CFloat(v) => Ok(OnionFloatValue::new_static(*v as f64)),
            CTypes::CDouble(v) => Ok(OnionFloatValue::new_static(*v)),
            CTypes::CBool(v) => Ok(OnionBooleanValue::new_static(*v)),
            CTypes::CString(v) => Ok(OnionStringValue::new_static(v)),
            CTypes::CBuffer(v) => Ok(OnionBytesValue::new_static(v)),
            CTypes::CPointer(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CChar(v) => Ok(OnionStringValue::new_static(
                std::str::from_utf8(&[*v as u8]).unwrap_or(""),
            )),
            CTypes::CUChar(v) => Ok(OnionStringValue::new_static(
                std::str::from_utf8(&[*v]).unwrap_or(""),
            )),
            CTypes::CSize(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CSSize(v) => Ok(OnionIntegerValue::new_static(*v as i64)),
            CTypes::CVoid | CTypes::CNull => Ok(OnionNull::new_static()),
        }
    }
    fn equals(&self, _other: &OnionObject) -> Result<bool, RuntimeError> {
        Ok(false)
    }
    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {}
    fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        match self {
            CTypes::CBuffer(bytes) => Ok(OnionIntegerValue::new_static(bytes.len() as i64)),
            CTypes::CString(s) => Ok(OnionIntegerValue::new_static(s.len() as i64)),
            _ => Err(RuntimeError::InvalidOperation(
                "len() is not applicable for this CTypes variant"
                    .to_string()
                    .into(),
            )),
        }
    }
}

impl OnionObjectProtocolAny for CTypes {
    fn with_attribute(
        &self,
        self_object: &OnionObject,
        key: &OnionObject,
        _path: &mut SmallVec<[*const (); 8]>,
        f: &mut dyn FnMut(&OnionObject, &OnionObject) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        if let OnionObject::StringValue(attr) = key {
            match attr.value() {
                "value" => {
                    let v = self.value_of()?;
                    f(self_object, v.weak())
                }
                _ => Err(RuntimeError::InvalidOperation(
                    format!("Attribute {attr:?} not found on CTypes object").into(),
                )),
            }
        } else {
            Err(RuntimeError::InvalidType(
                "Attribute key must be a string".into(),
            ))
        }
    }
}

// --- Argument Parsing Helper Functions ---

fn get_value_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
) -> Result<&OnionStaticObject, RuntimeError> {
    argument
        .get("value")
        .ok_or_else(|| RuntimeError::DetailedError("Function requires a 'value' argument".into()))
}

fn get_integer_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
) -> Result<i64, RuntimeError> {
    let obj = get_value_arg(argument)?;
    match obj.weak() {
        OnionObject::IntegerValue(i) => Ok(i.value()),
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
        OnionObject::IntegerValue(i) => Ok(NumericArg::Int(i.value())),
        OnionObject::FloatValue(f) => Ok(NumericArg::Float(f.value())),
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
        OnionObject::IntegerValue(n) => {
            if n.value() >= i8::MIN as i64 && n.value() <= i8::MAX as i64 {
                Ok(OnionObject::Custom(Arc::new(CTypes::CChar(n.value() as i8))).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    format!("Value {} out of range for char", n.value()).into(),
                ))
            }
        }
        OnionObject::StringValue(s) if s.value().chars().count() == 1 => Ok(OnionObject::Custom(
            Arc::new(CTypes::CChar(s.value().chars().next().unwrap() as i8)),
        )
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
        OnionObject::IntegerValue(n) => {
            if n.value() >= 0 && n.value() <= u8::MAX as i64 {
                Ok(OnionObject::Custom(Arc::new(CTypes::CUChar(n.value() as u8))).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    format!("Value {} out of range for unsigned char", n.value()).into(),
                ))
            }
        }
        OnionObject::StringValue(s) if s.value().chars().count() == 1 => Ok(OnionObject::Custom(
            Arc::new(CTypes::CUChar(s.value().chars().next().unwrap() as u8)),
        )
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
    match value_obj.weak() {
        OnionObject::BooleanValue(b) => {
            Ok(OnionObject::Custom(Arc::new(CTypes::CBool(b.value()))).stabilize())
        }
        _ => Err(RuntimeError::InvalidType(
            "c_bool requires a boolean".into(),
        )),
    }
}

fn c_string(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value_obj = get_value_arg(argument)?;
    match value_obj.weak() {
        OnionObject::StringValue(s) => {
            Ok(OnionObject::Custom(Arc::new(CTypes::CString(s.value().to_string()))).stabilize())
        }
        _ => Err(RuntimeError::InvalidType(
            "c_string requires a string".into(),
        )),
    }
}

fn c_buffer(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value_obj = get_value_arg(argument)?;
    match value_obj.weak() {
        OnionObject::BytesValue(b) => {
            Ok(OnionObject::Custom(Arc::new(CTypes::CBuffer(b.value().to_vec()))).stabilize())
        }
        OnionObject::StringValue(s) => Ok(OnionObject::Custom(Arc::new(CTypes::CBuffer(
            s.value().as_bytes().to_vec(),
        )))
        .stabilize()),
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
                    concat!("ctypes::", $name),
                    OnionKeyPool::create(vec!["value".into()]),
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
            LambdaParameter::Multiple(Box::new([])),
            OnionFastMap::default(),
            "ctypes::void",
            OnionKeyPool::create(vec![]),
            &c_void,
        ),
    );
    module.insert(
        "null".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(Box::new([])),
            OnionFastMap::default(),
            "ctypes::null",
            OnionKeyPool::create(vec![]),
            &c_null,
        ),
    );

    build_dict(module)
}
