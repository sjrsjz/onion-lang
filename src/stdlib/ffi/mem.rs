use std::{
    alloc::{Layout, alloc, dealloc},
    sync::Arc,
};

use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
};
use rustc_hash::FxHashMap;

// 引入所需的辅助函数
use crate::stdlib::{build_dict, build_string_tuple, wrap_native_function};

// --- Helper function for robust argument parsing ---

fn get_integer_arg(
    argument: &FxHashMap<String, OnionStaticObject>,
    name: &str,
) -> Result<i64, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires an '{}' argument", name)
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::Integer(i) => Ok(*i),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{}' must be an integer", name)
                .to_string()
                .into(),
        )),
    }
}

// --- Refactored Native Functions ---

/// 分配内存
fn mem_alloc(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let size = get_integer_arg(argument, "size")?;
    if size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Size must be positive".to_string().into(),
        ));
    }

    let layout = Layout::from_size_align(size as usize, 1)
        .map_err(|_| RuntimeError::InvalidOperation("Invalid memory layout".to_string().into()))?;

    unsafe {
        let ptr = alloc(layout);
        if ptr.is_null() {
            Err(RuntimeError::InvalidOperation(
                "Memory allocation failed".to_string().into(),
            ))
        } else {
            Ok(OnionObject::Integer(ptr as usize as i64).stabilize())
        }
    }
}

/// 释放内存
fn mem_free(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let ptr = get_integer_arg(argument, "ptr")?;
    let size = get_integer_arg(argument, "size")?;

    if ptr == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Cannot free a null pointer".to_string().into(),
        ));
    }
    if size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Size must be positive".to_string().into(),
        ));
    }

    let layout = Layout::from_size_align(size as usize, 1)
        .map_err(|_| RuntimeError::InvalidOperation("Invalid memory layout".to_string().into()))?;

    unsafe {
        dealloc(ptr as usize as *mut u8, layout);
    }

    Ok(OnionObject::Null.stabilize())
}

/// 分配并清零内存
fn mem_calloc(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let count = get_integer_arg(argument, "count")?;
    let size = get_integer_arg(argument, "size")?;

    if count <= 0 || size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Count and size must be positive".to_string().into(),
        ));
    }

    let total_size = (count as usize).saturating_mul(size as usize);
    if total_size == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Total allocation size cannot be zero".to_string().into(),
        ));
    }

    let layout = Layout::from_size_align(total_size, 1)
        .map_err(|_| RuntimeError::InvalidOperation("Invalid memory layout".to_string().into()))?;

    unsafe {
        let ptr = alloc(layout);
        if ptr.is_null() {
            Err(RuntimeError::InvalidOperation(
                "Memory allocation failed".to_string().into(),
            ))
        } else {
            std::ptr::write_bytes(ptr, 0, total_size);
            Ok(OnionObject::Integer(ptr as usize as i64).stabilize())
        }
    }
}

/// 读取内存内容到缓冲区
fn mem_read(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let ptr = get_integer_arg(argument, "ptr")?;
    let size = get_integer_arg(argument, "size")?;

    if ptr == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Cannot read from a null pointer".to_string().into(),
        ));
    }
    if size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Size must be positive".to_string().into(),
        ));
    }

    unsafe {
        let slice = std::slice::from_raw_parts(ptr as usize as *const u8, size as usize);
        Ok(OnionObject::Bytes(Arc::new(slice.to_vec())).stabilize())
    }
}

/// 写入缓冲区内容到内存
fn mem_write(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let ptr = get_integer_arg(argument, "ptr")?;
    let buffer_obj = argument.get("buffer").ok_or_else(|| {
        RuntimeError::DetailedError("mem_write requires a 'buffer' argument".to_string().into())
    })?;

    if ptr == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Cannot write to a null pointer".to_string().into(),
        ));
    }

    let bytes_to_write = match buffer_obj.weak() {
        OnionObject::Bytes(b) => b.clone(),
        OnionObject::String(s) => Arc::new(s.as_bytes().to_vec()),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Argument 'buffer' must be bytes or a string"
                    .to_string()
                    .into(),
            ));
        }
    };

    unsafe {
        std::ptr::copy_nonoverlapping(
            bytes_to_write.as_ptr(),
            ptr as usize as *mut u8,
            bytes_to_write.len(),
        );
    }

    Ok(OnionObject::Integer(bytes_to_write.len() as i64).stabilize())
}

/// 复制内存
fn mem_copy(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let dest = get_integer_arg(argument, "dest")?;
    let src = get_integer_arg(argument, "src")?;
    let size = get_integer_arg(argument, "size")?;

    if dest == 0 || src == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Cannot copy from or to a null pointer".to_string().into(),
        ));
    }
    if size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Size must be positive".to_string().into(),
        ));
    }

    unsafe {
        std::ptr::copy_nonoverlapping(
            src as usize as *const u8,
            dest as usize as *mut u8,
            size as usize,
        );
    }

    Ok(OnionObject::Null.stabilize())
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    module.insert(
        "alloc".to_string(),
        wrap_native_function(
            &OnionObject::String("size".to_string().into()).stabilize(),
            &FxHashMap::default(),
            "mem::alloc".to_string(),
            &mem_alloc,
        ),
    );
    module.insert(
        "free".to_string(),
        wrap_native_function(
            &build_string_tuple(&["ptr", "size"]),
            &FxHashMap::default(),
            "mem::free".to_string(),
            &mem_free,
        ),
    );
    module.insert(
        "calloc".to_string(),
        wrap_native_function(
            &build_string_tuple(&["count", "size"]),
            &FxHashMap::default(),
            "mem::calloc".to_string(),
            &mem_calloc,
        ),
    );
    module.insert(
        "read".to_string(),
        wrap_native_function(
            &build_string_tuple(&["ptr", "size"]),
            &FxHashMap::default(),
            "mem::read".to_string(),
            &mem_read,
        ),
    );
    module.insert(
        "write".to_string(),
        wrap_native_function(
            &build_string_tuple(&["ptr", "buffer"]),
            &FxHashMap::default(),
            "mem::write".to_string(),
            &mem_write,
        ),
    );
    module.insert(
        "copy".to_string(),
        wrap_native_function(
            &build_string_tuple(&["dest", "src", "size"]),
            &FxHashMap::default(),
            "mem::copy".to_string(),
            &mem_copy,
        ),
    );

    build_dict(module)
}
