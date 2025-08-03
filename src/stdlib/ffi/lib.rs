use std::{collections::VecDeque, ffi::CString, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};
use indexmap::IndexMap;
use libffi::middle::{Arg, Cif, Type};
use libloading::Library;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionObjectExt, OnionStaticObject},
        tuple::OnionTuple,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

// 引入所需的辅助函数和类型
use super::ctypes::CTypes;
use crate::stdlib::{build_dict, wrap_native_function};

// --- Core Structs and Implementations (CLib, CFunctionHandle, etc.) ---
// The internal logic of these structs is sound and remains largely unchanged.

fn string_to_utf16(s: &str) -> Vec<u16> {
    s.encode_utf16().collect()
}
enum Argument {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    USize(usize),
    ISize(isize),
    #[allow(unused)]
    Ptr(*const u8, Option<Box<CString>>),
    #[allow(unused)]
    WidePtr(*const u16, Option<Box<Vec<u16>>>),
}
impl Argument {
    fn as_arg(&self) -> Arg {
        match self {
            Argument::I8(v) => Arg::new(v),
            Argument::I16(v) => Arg::new(v),
            Argument::I32(v) => Arg::new(v),
            Argument::I64(v) => Arg::new(v),
            Argument::U8(v) => Arg::new(v),
            Argument::U16(v) => Arg::new(v),
            Argument::U32(v) => Arg::new(v),
            Argument::U64(v) => Arg::new(v),
            Argument::F32(v) => Arg::new(v),
            Argument::F64(v) => Arg::new(v),
            Argument::USize(v) => Arg::new(v),
            Argument::ISize(v) => Arg::new(v),
            Argument::Ptr(p, _) => Arg::new(p),
            Argument::WidePtr(p, _) => Arg::new(p),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CLib {
    pub path: String,
    pub library: Arc<Library>,
}

impl CLib {
    pub fn new(path: String) -> Result<Self, RuntimeError> {
        unsafe {
            Library::new(&path)
                .map(|lib| CLib {
                    path,
                    library: Arc::new(lib),
                })
                .map_err(|e| {
                    RuntimeError::InvalidOperation(format!("Failed to load library: {e}").into())
                })
        }
    }
    pub fn get_symbol(&self, name: &str) -> Result<*const u8, RuntimeError> {
        unsafe {
            self.library
                .get(name.as_bytes())
                .map(|sym: libloading::Symbol<unsafe extern "C" fn()>| *sym as *const u8)
                .map_err(|e| {
                    RuntimeError::InvalidOperation(
                        format!("Function '{name}' not found: {e}").into(),
                    )
                })
        }
    }
}
impl GCTraceable<OnionObjectCell> for CLib {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}
impl OnionObjectExt for CLib {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("CLib(path=\"{}\")", self.path))
    }
    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        if let OnionObject::Custom(other_custom) = other {
            if let Some(other_lib) = other_custom.as_any().downcast_ref::<CLib>() {
                return Ok(self.path == other_lib.path);
            }
        }
        Ok(false)
    }
    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {}
    fn is_same(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        self.equals(other)
    }
    fn with_attribute(
        &self,
        key: &OnionObject,
        f: &mut dyn FnMut(&OnionObject) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        if let OnionObject::String(attr) = key {
            match attr.as_ref() {
                "path" => f(&OnionObject::String(self.path.clone().into())),
                _ => Err(RuntimeError::InvalidOperation(
                    format!("CLib has no attribute '{attr}'").into(),
                )),
            }
        } else {
            Err(RuntimeError::InvalidType(
                "Attribute key must be a string".into(),
            ))
        }
    }
    fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation("CLib has no length".into()))
    }
}

#[derive(Clone, Debug)]
pub struct CFunctionHandle {
    pub library: Arc<CLib>,
    pub function_name: String,
    pub return_type: String,
    pub param_types: Vec<String>,
    pub function_ptr: *const u8,
}
unsafe impl Send for CFunctionHandle {}
unsafe impl Sync for CFunctionHandle {}
impl CFunctionHandle {
    pub fn new(
        library: Arc<CLib>,
        function_name: String,
        return_type: String,
        param_types: Vec<String>,
    ) -> Result<Self, RuntimeError> {
        let function_ptr = library.get_symbol(&function_name)?;
        Ok(Self {
            library,
            function_name,
            return_type,
            param_types,
            function_ptr,
        })
    }

    pub fn call(&self, args: &[CTypes]) -> Result<CTypes, RuntimeError> {
        if args.len() != self.param_types.len() {
            return Err(RuntimeError::InvalidOperation(
                format!(
                    "Expected {} arguments, got {}",
                    self.param_types.len(),
                    args.len()
                )
                .into(),
            ));
        }
        unsafe { self.call_with_libffi(args) }
    }
    unsafe fn call_with_libffi(&self, args: &[CTypes]) -> Result<CTypes, RuntimeError> {
        let mut arg_types = Vec::with_capacity(args.len());
        let mut arg_values = Vec::with_capacity(args.len());

        for (i, arg) in args.iter().enumerate() {
            let (ffi_type, value) =
                unsafe { self.prepare_arg_for_call(arg, &self.param_types[i])? };
            arg_types.push(ffi_type);
            arg_values.push(value);
        }

        let return_ffi_type = self.get_ffi_type(&self.return_type)?;
        let cif = Cif::new(arg_types, return_ffi_type);
        let ffi_args: Vec<Arg> = arg_values.iter().map(|v| v.as_arg()).collect();
        let code_ptr = libffi::middle::CodePtr(self.function_ptr as *mut _);

        unsafe {
            match self.return_type.as_str() {
                "void" => {
                    cif.call::<()>(code_ptr, &ffi_args);
                    Ok(CTypes::CVoid)
                }
                "i8" => Ok(CTypes::CInt8(cif.call::<i8>(code_ptr, &ffi_args))),
                "i16" => Ok(CTypes::CInt16(cif.call::<i16>(code_ptr, &ffi_args))),
                "i32" => Ok(CTypes::CInt32(cif.call::<i32>(code_ptr, &ffi_args))),
                "i64" => Ok(CTypes::CInt64(cif.call::<i64>(code_ptr, &ffi_args))),
                "u8" => Ok(CTypes::CUInt8(cif.call::<u8>(code_ptr, &ffi_args))),
                "u16" => Ok(CTypes::CUInt16(cif.call::<u16>(code_ptr, &ffi_args))),
                "u32" => Ok(CTypes::CUInt32(cif.call::<u32>(code_ptr, &ffi_args))),
                "u64" => Ok(CTypes::CUInt64(cif.call::<u64>(code_ptr, &ffi_args))),
                "f32" => Ok(CTypes::CFloat(cif.call::<f32>(code_ptr, &ffi_args))),
                "f64" => Ok(CTypes::CDouble(cif.call::<f64>(code_ptr, &ffi_args))),
                "bool" => Ok(CTypes::CBool(cif.call::<u8>(code_ptr, &ffi_args) != 0)),
                "char" => Ok(CTypes::CChar(cif.call::<i8>(code_ptr, &ffi_args))),
                "uchar" => Ok(CTypes::CUChar(cif.call::<u8>(code_ptr, &ffi_args))),
                "size" => Ok(CTypes::CSize(cif.call::<usize>(code_ptr, &ffi_args))),
                "ssize" => Ok(CTypes::CSSize(cif.call::<isize>(code_ptr, &ffi_args))),
                "pointer" => Ok(CTypes::CPointer(
                    cif.call::<*const u8>(code_ptr, &ffi_args) as usize
                )),
                _ => Err(RuntimeError::InvalidOperation(
                    format!("Unsupported return type: {}", self.return_type).into(),
                )),
            }
        }
    }
    unsafe fn prepare_arg_for_call(
        &self,
        ctype: &CTypes,
        expected_type: &str,
    ) -> Result<(Type, Argument), RuntimeError> {
        match (ctype, expected_type) {
            (CTypes::CInt8(v), "i8") => Ok((Type::i8(), Argument::I8(*v))),
            (CTypes::CInt16(v), "i16") => Ok((Type::i16(), Argument::I16(*v))),
            (CTypes::CInt32(v), "i32") => Ok((Type::i32(), Argument::I32(*v))),
            (CTypes::CInt64(v), "i64") => Ok((Type::i64(), Argument::I64(*v))),
            (CTypes::CUInt8(v), "u8") => Ok((Type::u8(), Argument::U8(*v))),
            (CTypes::CUInt16(v), "u16") => Ok((Type::u16(), Argument::U16(*v))),
            (CTypes::CUInt32(v), "u32") => Ok((Type::u32(), Argument::U32(*v))),
            (CTypes::CUInt64(v), "u64") => Ok((Type::u64(), Argument::U64(*v))),
            (CTypes::CFloat(v), "f32") => Ok((Type::f32(), Argument::F32(*v))),
            (CTypes::CDouble(v), "f64") => Ok((Type::f64(), Argument::F64(*v))),
            (CTypes::CBool(v), "bool") => Ok((Type::u8(), Argument::U8(if *v { 1 } else { 0 }))),
            (CTypes::CChar(v), "char") => Ok((Type::i8(), Argument::I8(*v))),
            (CTypes::CUChar(v), "uchar") => Ok((Type::u8(), Argument::U8(*v))),
            (CTypes::CSize(v), "size") => Ok((Type::usize(), Argument::USize(*v))),
            (CTypes::CSSize(v), "ssize") => Ok((Type::isize(), Argument::ISize(*v))),
            (CTypes::CPointer(v), "pointer") => {
                Ok((Type::pointer(), Argument::Ptr(*v as *const u8, None)))
            }
            (CTypes::CString(s), "string") => {
                let c_string = CString::new(s.as_str()).map_err(|_| {
                    RuntimeError::InvalidOperation("String contains null byte".into())
                })?;
                let ptr = c_string.as_ptr();
                Ok((
                    Type::pointer(),
                    Argument::Ptr(ptr as *const u8, Some(Box::new(c_string))),
                ))
            }
            (CTypes::CString(s), "wstring") => {
                let mut wide_chars = string_to_utf16(s);
                wide_chars.push(0);
                let ptr = wide_chars.as_ptr();
                Ok((
                    Type::pointer(),
                    Argument::WidePtr(ptr, Some(Box::new(wide_chars))),
                ))
            }
            (CTypes::CBuffer(b), "buffer") => {
                Ok((Type::pointer(), Argument::Ptr(b.as_ptr(), None)))
            }
            (CTypes::CNull, "pointer") => {
                Ok((Type::pointer(), Argument::Ptr(std::ptr::null(), None)))
            }
            _ => Err(RuntimeError::InvalidOperation(
                format!("Type mismatch for FFI call: expected {expected_type}, got {ctype:?}")
                    .into(),
            )),
        }
    }
    fn get_ffi_type(&self, type_name: &str) -> Result<Type, RuntimeError> {
        match type_name {
            "void" => Ok(Type::void()),
            "i8" => Ok(Type::i8()),
            "i16" => Ok(Type::i16()),
            "i32" => Ok(Type::i32()),
            "i64" => Ok(Type::i64()),
            "u8" => Ok(Type::u8()),
            "u16" => Ok(Type::u16()),
            "u32" => Ok(Type::u32()),
            "u64" => Ok(Type::u64()),
            "f32" => Ok(Type::f32()),
            "f64" => Ok(Type::f64()),
            "bool" => Ok(Type::u8()),
            "char" => Ok(Type::i8()),
            "uchar" => Ok(Type::u8()),
            "size" => Ok(Type::usize()),
            "ssize" => Ok(Type::isize()),
            "pointer" | "string" | "buffer" | "wstring" => Ok(Type::pointer()),
            _ => Err(RuntimeError::InvalidOperation(
                format!("Unsupported FFI type: {type_name}").into(),
            )),
        }
    }
}
impl GCTraceable<OnionObjectCell> for CFunctionHandle {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}
impl OnionObjectExt for CFunctionHandle {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!(
            "CFunctionHandle({}::{})",
            self.library.path, self.function_name
        ))
    }
    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        if let OnionObject::Custom(other_custom) = other {
            if let Some(other_handle) = other_custom.as_any().downcast_ref::<CFunctionHandle>() {
                return Ok(
                    Arc::ptr_eq(&self.library.library, &other_handle.library.library)
                        && self.function_name == other_handle.function_name,
                );
            }
        }
        Ok(false)
    }
    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {}
    fn is_same(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        self.equals(other)
    }
    fn with_attribute(
        &self,
        key: &OnionObject,
        f: &mut dyn FnMut(&OnionObject) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        if let OnionObject::String(attr) = key {
            match attr.as_ref() {
                "name" => f(&OnionObject::String(self.function_name.clone().into())),
                "library" => f(&OnionObject::Custom(Arc::new(
                    self.library.as_ref().clone(),
                ))),
                "return_type" => f(&OnionObject::String(self.return_type.clone().into())),
                "param_types" => {
                    let types = self
                        .param_types
                        .iter()
                        .map(|t| OnionObject::String(t.clone().into()))
                        .collect();
                    f(&OnionObject::Tuple(OnionTuple::new(types).into()))
                }
                _ => Err(RuntimeError::InvalidOperation(
                    format!("CFunctionHandle has no attribute '{attr}'").into(),
                )),
            }
        } else {
            Err(RuntimeError::InvalidType(
                "Attribute key must be a string".into(),
            ))
        }
    }
    fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "CFunctionHandle has no length".into(),
        ))
    }
}

// --- Argument Parsing Helper Functions ---

fn get_clib_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<Arc<CLib>, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(format!("Missing required argument: '{name}'").into())
    })?;
    match obj.weak() {
        OnionObject::Custom(custom) => custom
            .as_any()
            .downcast_ref::<CLib>()
            .map(|clib| Arc::new(clib.clone()))
            .ok_or_else(|| {
                RuntimeError::InvalidType(format!("Argument '{name}' must be a CLib object").into())
            }),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be a CLib object").into(),
        )),
    }
}

fn get_string_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<String, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(format!("Missing required argument: '{name}'").into())
    })?;
    match obj.weak() {
        OnionObject::String(s) => Ok(s.to_string()),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be a string").into(),
        )),
    }
}

fn get_string_tuple_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<Vec<String>, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(format!("Missing required argument: '{name}'").into())
    })?;
    match obj.weak() {
        OnionObject::Tuple(tuple) => tuple
            .get_elements()
            .iter()
            .map(|item| match item {
                OnionObject::String(s) => Ok(s.to_string()),
                _ => Err(RuntimeError::InvalidType(
                    "All elements in the parameter types tuple must be strings"
                        .to_string()
                        .into(),
                )),
            })
            .collect(),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be a tuple of strings").into(),
        )),
    }
}

fn get_ctypes_tuple_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<Vec<CTypes>, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(format!("Missing required argument: '{name}'").into())
    })?;
    match obj.weak() {
        OnionObject::Tuple(tuple) => tuple
            .get_elements()
            .iter()
            .map(|item| match item {
                OnionObject::Custom(custom) => custom
                    .as_any()
                    .downcast_ref::<CTypes>()
                    .cloned()
                    .ok_or_else(|| {
                        RuntimeError::InvalidType(
                            "All arguments for call must be CTypes objects"
                                .to_string()
                                .into(),
                        )
                    }),
                _ => Err(RuntimeError::InvalidType(
                    "All arguments for call must be CTypes objects"
                        .to_string()
                        .into(),
                )),
            })
            .collect(),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be a tuple of CTypes").into(),
        )),
    }
}

// --- Refactored Native Functions ---

fn lib_load(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let path = get_string_arg(argument, "path")?;
    let clib = CLib::new(path)?;
    Ok(OnionObject::Custom(Arc::new(clib)).stabilize())
}

fn lib_get_function(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let library = get_clib_arg(argument, "library")?;
    let func_name = get_string_arg(argument, "function")?;
    let ret_type = get_string_arg(argument, "return_type")?;
    let param_types = get_string_tuple_arg(argument, "param_types")?;

    let handle = CFunctionHandle::new(library, func_name, ret_type, param_types)?;
    Ok(OnionObject::Custom(Arc::new(handle)).stabilize())
}

fn lib_call_function(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let handle_obj = argument
        .get("handle")
        .ok_or_else(|| RuntimeError::DetailedError("Missing required argument: 'handle'".into()))?;
    let handle = match handle_obj.weak() {
        OnionObject::Custom(custom) => custom
            .as_any()
            .downcast_ref::<CFunctionHandle>()
            .ok_or_else(|| {
                RuntimeError::InvalidType(
                    "Argument 'handle' must be a CFunctionHandle"
                        .to_string()
                        .into(),
                )
            }),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'handle' must be a CFunctionHandle"
                .to_string()
                .into(),
        )),
    }?;

    let args = get_ctypes_tuple_arg(argument, "args")?;

    // The call method now takes a slice of owned CTypes
    let result = handle.call(&args)?;
    Ok(OnionObject::Custom(Arc::new(result)).stabilize())
}

// --- Module Build Function ---

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // lib.load(path)
    module.insert(
        "load".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "lib::load",
            OnionKeyPool::create(vec!["path".into()]),
            &lib_load,
        ),
    );

    // lib.get_function(library, function, return_type, param_types)
    module.insert(
        "get_function".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("library"),
                    LambdaParameter::top("function"),
                    LambdaParameter::top("return_type"),
                    LambdaParameter::top("param_types"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "lib::get_function",
            OnionKeyPool::create(vec![
                "library".into(),
                "function".into(),
                "return_type".into(),
                "param_types".into(),
            ]),
            &lib_get_function,
        ),
    );

    // lib.call(handle, args)
    module.insert(
        "call".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("handle"), LambdaParameter::top("args")].into(),
            ),
            OnionFastMap::default(),
            "lib::call",
            OnionKeyPool::create(vec!["handle".into(), "args".into()]),
            &lib_call_function,
        ),
    );

    build_dict(module)
}
