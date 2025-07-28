use std::{
    collections::VecDeque, ffi::CString, sync::Arc
};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};
use indexmap::IndexMap;
use libffi::middle::{Arg, Cif, Type};
use libloading::Library;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::{
        object::{OnionObject, OnionObjectCell, OnionObjectExt, OnionStaticObject},
        tuple::OnionTuple,
    },
    GC,
};

use crate::stdlib::{build_named_dict, get_attr_direct, wrap_native_function};

use super::ctypes::CTypes;

/// 跨平台的字符串到 UTF-16 转换函数
fn string_to_utf16(s: &str) -> Vec<u16> {
    s.encode_utf16().collect()
}

// Helper enum to manage argument lifetimes and types for libffi calls
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
    #[allow(dead_code)]
    Ptr(*const u8, Option<Box<CString>>),
    #[allow(dead_code)]
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

/// C 动态库对象
#[derive(Debug)]
pub struct CLib {
    pub path: String,
    pub library: Arc<Library>,
}

impl Clone for CLib {
    fn clone(&self) -> Self {
        CLib {
            path: self.path.clone(),
            library: self.library.clone(),
        }
    }
}

impl CLib {
    pub fn new(path: String) -> Result<Self, RuntimeError> {
        unsafe {
            match Library::new(&path) {
                Ok(lib) => Ok(CLib {
                    path,
                    library: Arc::new(lib),
                }),
                Err(e) => Err(RuntimeError::InvalidOperation(
                    format!("Failed to load library: {}", e).into(),
                )),
            }
        }
    }

    /// 获取函数符号
    pub fn get_symbol(&self, name: &str) -> Result<*const u8, RuntimeError> {
        unsafe {
            let symbol: Result<libloading::Symbol<unsafe extern "C" fn()>, _> = 
                self.library.get(name.as_bytes());
            match symbol {
                Ok(sym) => Ok(*sym as *const u8),
                Err(e) => Err(RuntimeError::InvalidOperation(
                    format!("Function '{}' not found: {}", name, e).into(),
                )),
            }
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
        Ok(format!(
            "CLib(path: \"{}\")",
            self.path
        ))
    }

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        if let OnionObject::Custom(other_custom) = other {
            if let Some(other_lib) = other_custom.as_any().downcast_ref::<CLib>() {
                Ok(self.path == other_lib.path)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {}

    fn is_same(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        if let OnionObject::Custom(other_custom) = other {
            if let Some(other_lib) = other_custom.as_any().downcast_ref::<CLib>() {
                Ok(self.path == other_lib.path)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    fn with_attribute(
        &self,
        key: &OnionObject,
        f: &mut dyn FnMut(&OnionObject) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        if let OnionObject::String(attr) = key {
            match attr.as_ref().as_str() {
                "path" => f(&OnionObject::String(Arc::new(self.path.clone()))),
                _ => Err(RuntimeError::InvalidOperation(
                    format!("CLib has no attribute '{}'", attr).into(),
                )),
            }
        } else {
            Err(RuntimeError::InvalidOperation(
                "Attribute key must be a string".to_string().into(),
            ))
        }
    }

    fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "CLib has no length".to_string().into(),
        ))
    }
}

/// C 函数句柄
#[derive(Clone, Debug)]
pub struct CFunctionHandle {
    pub library: Arc<CLib>,
    pub function_name: String,
    pub return_type: String,
    pub param_types: Vec<String>,
    pub function_ptr: *const u8,
}

// 确保 CFunctionHandle 可以安全地在线程间传递
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
        
        Ok(CFunctionHandle {
            library,
            function_name,
            return_type,
            param_types,
            function_ptr,
        })
    }

    /// 调用 C 函数
    pub fn call(&self, args: &[&CTypes]) -> Result<CTypes, RuntimeError> {
        if args.len() != self.param_types.len() {
            return Err(RuntimeError::InvalidOperation(
                format!(
                    "Expected {} arguments, got {}",
                    self.param_types.len(),
                    args.len()
                ).into(),
            ));
        }

        unsafe {
            self.call_with_libffi(args)
        }
    }    /// 使用 libffi 调用函数
    unsafe fn call_with_libffi(&self, args: &[&CTypes]) -> Result<CTypes, RuntimeError> {
        // 准备参数类型和值
        let mut arg_types = Vec::new();
        let mut arg_values = Vec::new(); // Vec<Argument>

        for (i, arg) in args.iter().enumerate() {
            let (ffi_type, value) = unsafe {
                self.prepare_arg_for_call(arg, &self.param_types[i])
            }?;
            arg_types.push(ffi_type);
            arg_values.push(value);
        }

        // 准备返回类型
        let return_ffi_type = self.get_ffi_type(&self.return_type)?;

        // 创建 CIF
        let cif = Cif::new(arg_types, return_ffi_type);
        // 准备参数指针
        let args_for_ffi: Vec<Arg> = arg_values.iter().map(|val| val.as_arg()).collect();

        // 调用函数并处理返回值
        unsafe {
            match self.return_type.as_str() {
                "void" => {
                    cif.call::<()>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CVoid)
                }
                "i8" => {
                    let result = cif.call::<i8>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CInt8(result))
                }
                "i16" => {
                    let result = cif.call::<i16>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CInt16(result))
                }
                "i32" => {
                    let result = cif.call::<i32>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CInt32(result))
                }
                "i64" => {
                    let result = cif.call::<i64>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CInt64(result))
                }
                "u8" => {
                    let result = cif.call::<u8>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CUInt8(result))
                }
                "u16" => {
                    let result = cif.call::<u16>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CUInt16(result))
                }
                "u32" => {
                    let result = cif.call::<u32>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CUInt32(result))
                }
                "u64" => {
                    let result = cif.call::<u64>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CUInt64(result))
                }
                "f32" => {
                    let result = cif.call::<f32>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CFloat(result))
                }
                "f64" => {
                    let result = cif.call::<f64>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CDouble(result))
                }
                "bool" => {
                    let result = cif.call::<u8>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CBool(result != 0))
                }
                "char" => {
                    let result = cif.call::<i8>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CChar(result))
                }
                "uchar" => {
                    let result = cif.call::<u8>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CUChar(result))
                }
                "size" => {
                    let result = cif.call::<usize>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CSize(result))
                }
                "ssize" => {
                    let result = cif.call::<isize>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CSSize(result))
                }
                "pointer" => {
                    let result = cif.call::<*const u8>(libffi::middle::CodePtr(self.function_ptr as *mut std::ffi::c_void), &args_for_ffi);
                    Ok(CTypes::CPointer(result as usize))
                }
                _ => Err(RuntimeError::InvalidOperation(
                    format!("Unsupported return type: {}", self.return_type).into(),
                )),
            }
        }
    }    /// 准备参数用于调用
    unsafe fn prepare_arg_for_call(&self, ctype: &CTypes, expected_type: &str) -> Result<(Type, Argument), RuntimeError> {
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
            (CTypes::CBool(v), "bool") => Ok((Type::u8(), Argument::U8(if *v { 1u8 } else { 0u8 }))),
            (CTypes::CChar(v), "char") => Ok((Type::i8(), Argument::I8(*v))),
            (CTypes::CUChar(v), "uchar") => Ok((Type::u8(), Argument::U8(*v))),
            (CTypes::CSize(v), "size") => Ok((Type::usize(), Argument::USize(*v))),
            (CTypes::CSSize(v), "ssize") => Ok((Type::isize(), Argument::ISize(*v))),
            (CTypes::CPointer(v), "pointer") => Ok((Type::pointer(), Argument::Ptr(*v as *const u8, None))),
            (CTypes::CString(s), "string") => {
                let c_string = CString::new(s.as_str()).map_err(|_| {
                    RuntimeError::InvalidOperation("String contains null byte".to_string().into())
                })?;
                let ptr = c_string.as_ptr();
                let boxed_cstring = Box::new(c_string);
                Ok((Type::pointer(), Argument::Ptr(ptr as *const u8, Some(boxed_cstring))))
            }
            (CTypes::CString(s), "wstring") => {
                let mut wide_chars: Vec<u16> = string_to_utf16(s.as_str());
                wide_chars.push(0); // 添加 null 终止符
                let ptr = wide_chars.as_ptr();
                let boxed_wstr = Box::new(wide_chars);
                Ok((Type::pointer(), Argument::WidePtr(ptr, Some(boxed_wstr))))
            }
            (CTypes::CBuffer(b), "buffer") => {
                let ptr = b.as_ptr();
                Ok((Type::pointer(), Argument::Ptr(ptr, None)))
            }
            (CTypes::CNull, "pointer") => Ok((Type::pointer(), Argument::Ptr(std::ptr::null::<u8>(), None))),
            _ => Err(RuntimeError::InvalidOperation(
                format!("Type mismatch: expected {}, got {:?}", expected_type, ctype).into(),
            )),
        }
    }    /// 获取 libffi 类型
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
                format!("Unsupported type: {}", type_name).into(),
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
            "CFunctionHandle({}::{}, ret: {}, params: {:?})",
            self.library.path, self.function_name, self.return_type, self.param_types
        ))
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
                "name" => f(&OnionObject::String(Arc::new(self.function_name.clone()))),
                "library" => f(&OnionObject::String(Arc::new(self.library.path.clone()))),
                "return_type" => f(&OnionObject::String(Arc::new(self.return_type.clone()))),
                "param_types" => {
                    let types: Vec<OnionObject> = self
                        .param_types
                        .iter()
                        .map(|t| OnionObject::String(Arc::new(t.clone())))
                        .collect();
                    let tuple = OnionTuple::new(types);
                    f(&OnionObject::Tuple(Arc::new(tuple)))
                }
                _ => Err(RuntimeError::InvalidOperation(
                    format!("CFunctionHandle has no attribute '{}'", attr).into(),
                )),
            }
        } else {
            Err(RuntimeError::InvalidOperation(
                "Attribute key must be a string".to_string().into(),
            ))
        }
    }

    fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            "CFunctionHandle has no length".to_string().into(),
        ))
    }
}

/// 加载动态库
fn lib_load(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                let path_str = path_str.as_ref();
                
                match CLib::new(path_str.to_string()) {
                    Ok(clib) => Ok(OnionObject::Custom(Arc::new(clib)).stabilize()),
                    Err(e) => Err(e),
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "lib_load requires string path".to_string().into(),
            )),
        })
    })
}

/// 获取函数句柄
fn lib_get_function(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let library_arg = get_attr_direct(data, "library".to_string())?;
        let function_name = get_attr_direct(data, "function".to_string())?;
        let return_type = get_attr_direct(data, "return_type".to_string())?;
        let param_types = get_attr_direct(data, "param_types".to_string())?;

        library_arg.weak().with_data(|lib_data| {
            function_name.weak().with_data(|func_data| {
                return_type.weak().with_data(|ret_data| {
                    param_types.weak().with_data(|params_data| {
                        let (library, func_name, ret_type) = match (lib_data, func_data, ret_data) {
                            (OnionObject::String(lib_path), OnionObject::String(func_name), OnionObject::String(ret_type)) => {
                                // 从路径创建新的库
                                let library = CLib::new(lib_path.as_ref().clone())?;
                                (Arc::new(library), func_name.as_ref().clone(), ret_type.as_ref().clone())
                            }
                            (OnionObject::Custom(custom), OnionObject::String(func_name), OnionObject::String(ret_type)) => {
                                if let Some(clib) = custom.as_any().downcast_ref::<CLib>() {
                                    (Arc::new(clib.clone()), func_name.as_ref().clone(), ret_type.as_ref().clone())
                                } else {
                                    return Err(RuntimeError::InvalidOperation(
                                        "Expected CLib object or string path".to_string().into(),
                                    ));
                                }
                            }
                            _ => return Err(RuntimeError::InvalidOperation(
                                "lib_get_function requires library (CLib or string), function name, and return type".to_string().into(),
                            )),
                        };

                        if let OnionObject::Tuple(params_tuple) = params_data {
                            // 解析参数类型
                            let mut param_type_strs = Vec::new();
                            let tuple_len = params_tuple.len()?.weak().with_data(|data| {
                                if let OnionObject::Integer(len) = data {
                                    Ok(*len)
                                } else {
                                    Err(RuntimeError::InvalidOperation("Invalid tuple length".to_string().into()))
                                }
                            })?;
                            
                            for i in 0..tuple_len {
                                let param = params_tuple.at(i)?;
                                param.weak().with_data(|param_data| {
                                    if let OnionObject::String(type_str) = param_data {
                                        param_type_strs.push(type_str.as_ref().clone());
                                        Ok(())
                                    } else {
                                        Err(RuntimeError::InvalidOperation(
                                            "Parameter types must be strings".to_string().into(),
                                        ))
                                    }
                                })?;
                            }
                            
                            // 创建函数句柄
                            let handle = CFunctionHandle::new(
                                library,
                                func_name,
                                ret_type,
                                param_type_strs,
                            )?;
                            
                            Ok(OnionObject::Custom(Arc::new(handle)).stabilize())
                        } else {
                            Err(RuntimeError::InvalidOperation(
                                "Parameter types must be a tuple".to_string().into(),
                            ))
                        }
                    })
                })
            })
        })
    })
}

/// 调用 C 函数
fn lib_call_function(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let handle_arg = get_attr_direct(data, "handle".to_string())?;
        let args_arg = get_attr_direct(data, "args".to_string())?;

        handle_arg.weak().with_data(|handle_data| {
            args_arg.weak().with_data(|args_data| {
                match (handle_data, args_data) {
                    (OnionObject::Custom(handle_custom), OnionObject::Tuple(args_tuple)) => {
                        // 尝试转换为 CFunctionHandle
                        let handle = handle_custom
                            .as_any()
                            .downcast_ref::<CFunctionHandle>()
                            .ok_or_else(|| {
                                RuntimeError::InvalidOperation(
                                    "Expected CFunctionHandle".to_string().into(),
                                )
                            })?;

                        // 验证参数数量
                        let args_len = args_tuple.len()?.weak().with_data(|data| {
                            if let OnionObject::Integer(len) = data {
                                Ok(*len)
                            } else {
                                Err(RuntimeError::InvalidOperation("Invalid args length".to_string().into()))
                            }
                        })?;
                        
                        if args_len != handle.param_types.len() as i64 {
                            return Err(RuntimeError::InvalidOperation(
                                format!(
                                    "Expected {} arguments, got {}",
                                    handle.param_types.len(),
                                    args_len
                                )
                                .into(),
                            ));
                        }

                        // 收集所有参数的 CTypes，避免生命周期问题
                        let mut owned_ctypes = Vec::new();
                        for i in 0..args_len {
                            let arg = args_tuple.at(i)?;
                            arg.weak().with_data(|arg_data| {
                                if let OnionObject::Custom(custom) = arg_data {
                                    if let Some(ctype) = custom.as_any().downcast_ref::<CTypes>() {
                                        owned_ctypes.push(ctype.clone());
                                        Ok(())
                                    } else {
                                        Err(RuntimeError::InvalidOperation(
                                            format!("Argument {} is not a valid C type", i).into(),
                                        ))
                                    }
                                } else {
                                    Err(RuntimeError::InvalidOperation(
                                        format!("Argument {} is not a C type object", i).into(),
                                    ))
                                }
                            })?;
                        }

                        // 创建引用向量用于调用
                        let ctype_refs: Vec<&CTypes> = owned_ctypes.iter().collect();

                        // 调用函数
                        let result = handle.call(&ctype_refs)?;
                        Ok(OnionObject::Custom(Arc::new(result)).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "lib_call_function requires handle and args tuple".to_string().into(),
                    )),
                }
            })
        })
    })
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();    // load 函数
    let mut load_params = IndexMap::new();
    load_params.insert(
        "path".to_string(),
        OnionObject::Undefined(Some("Path to the dynamic library".to_string().into())).stabilize(),
    );
    module.insert(
        "load".to_string(),
        wrap_native_function(
            &build_named_dict(load_params),
            &OnionObject::Undefined(None),
            "lib::load".to_string(),
            &lib_load,
        ),    );

    // get_function 函数
    let mut get_function_params = IndexMap::new();
    get_function_params.insert(
        "library".to_string(),
        OnionObject::Undefined(Some("CLib object or library path string".to_string().into())).stabilize(),
    );
    get_function_params.insert(
        "function".to_string(),
        OnionObject::Undefined(Some("Function name".to_string().into())).stabilize(),
    );
    get_function_params.insert(
        "return_type".to_string(),
        OnionObject::Undefined(Some("Return type string".to_string().into())).stabilize(),
    );    get_function_params.insert(
        "param_types".to_string(),
        OnionObject::Undefined(Some("Tuple of parameter type strings".to_string().into())).stabilize(),
    );
    module.insert(
        "get_function".to_string(),
        wrap_native_function(
            &build_named_dict(get_function_params),
            &OnionObject::Undefined(None),
            "lib::get_function".to_string(),
            &lib_get_function,
        ),
    );

    // call_function 函数
    let mut call_function_params = IndexMap::new();
    call_function_params.insert(
        "handle".to_string(),
        OnionObject::Undefined(Some("CFunctionHandle object".to_string().into())).stabilize(),
    );    call_function_params.insert(
        "args".to_string(),
        OnionObject::Undefined(Some("Tuple of C type arguments".to_string().into())).stabilize(),
    );
    module.insert(
        "call".to_string(),
        wrap_native_function(
            &build_named_dict(call_function_params),
            &OnionObject::Undefined(None),
            "lib::call".to_string(),
            &lib_call_function,
        ),    );

    build_named_dict(module)
}
