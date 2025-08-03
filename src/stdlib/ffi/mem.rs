use std::{
    alloc::{Layout, alloc, dealloc},
    sync::Arc,
};

use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

// 引入所需的辅助函数
use crate::stdlib::{build_dict, wrap_native_function};

// --- Helper function for robust argument parsing ---

fn get_integer_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<i64, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires an '{name}' argument")
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::Integer(i) => Ok(*i),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be an integer")
                .to_string()
                .into(),
        )),
    }
}

// --- Refactored Native Functions ---

/// 分配内存
fn mem_alloc(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let size = get_integer_arg(argument, "size")?;
    if size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Size must be positive".into(),
        ));
    }

    let layout = Layout::from_size_align(size as usize, 1)
        .map_err(|_| RuntimeError::InvalidOperation("Invalid memory layout".into()))?;

    unsafe {
        let ptr = alloc(layout);
        if ptr.is_null() {
            Err(RuntimeError::InvalidOperation(
                "Memory allocation failed".into(),
            ))
        } else {
            Ok(OnionObject::Integer(ptr as usize as i64).stabilize())
        }
    }
}

/// 释放内存
fn mem_free(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let ptr = get_integer_arg(argument, "ptr")?;
    let size = get_integer_arg(argument, "size")?;

    if ptr == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Cannot free a null pointer".into(),
        ));
    }
    if size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Size must be positive".into(),
        ));
    }

    let layout = Layout::from_size_align(size as usize, 1)
        .map_err(|_| RuntimeError::InvalidOperation("Invalid memory layout".into()))?;

    unsafe {
        dealloc(ptr as usize as *mut u8, layout);
    }

    Ok(OnionObject::Null.stabilize())
}

/// 分配并清零内存
fn mem_calloc(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let count = get_integer_arg(argument, "count")?;
    let size = get_integer_arg(argument, "size")?;

    if count <= 0 || size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Count and size must be positive".into(),
        ));
    }

    let total_size = (count as usize).saturating_mul(size as usize);
    if total_size == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Total allocation size cannot be zero".into(),
        ));
    }

    let layout = Layout::from_size_align(total_size, 1)
        .map_err(|_| RuntimeError::InvalidOperation("Invalid memory layout".into()))?;

    unsafe {
        let ptr = alloc(layout);
        if ptr.is_null() {
            Err(RuntimeError::InvalidOperation(
                "Memory allocation failed".into(),
            ))
        } else {
            std::ptr::write_bytes(ptr, 0, total_size);
            Ok(OnionObject::Integer(ptr as usize as i64).stabilize())
        }
    }
}

/// 读取内存内容到缓冲区
fn mem_read(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let ptr = get_integer_arg(argument, "ptr")?;
    let size = get_integer_arg(argument, "size")?;

    if ptr == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Cannot read from a null pointer".into(),
        ));
    }
    if size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Size must be positive".into(),
        ));
    }

    unsafe {
        let slice = std::slice::from_raw_parts(ptr as usize as *const u8, size as usize);
        Ok(OnionObject::Bytes(Arc::from(slice)).stabilize())
    }
}

/// 写入缓冲区内容到内存
fn mem_write(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let ptr = get_integer_arg(argument, "ptr")?;
    let buffer_obj = argument.get("buffer").ok_or_else(|| {
        RuntimeError::DetailedError("mem_write requires a 'buffer' argument".into())
    })?;

    if ptr == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Cannot write to a null pointer".into(),
        ));
    }

    let bytes_to_write = match buffer_obj.weak() {
        OnionObject::Bytes(b) => b.clone(),
        OnionObject::String(s) => Arc::from(s.as_bytes()),
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
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let dest = get_integer_arg(argument, "dest")?;
    let src = get_integer_arg(argument, "src")?;
    let size = get_integer_arg(argument, "size")?;

    if dest == 0 || src == 0 {
        return Err(RuntimeError::InvalidOperation(
            "Cannot copy from or to a null pointer".into(),
        ));
    }
    if size <= 0 {
        return Err(RuntimeError::InvalidOperation(
            "Size must be positive".into(),
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

    // mem.alloc(size)
    module.insert(
        "alloc".to_string(),
        wrap_native_function(
            LambdaParameter::top("size"),
            OnionFastMap::default(),
            "mem::alloc",
            OnionKeyPool::create(vec!["size".into()]),
            &mem_alloc,
        ),
    );

    // mem.free(ptr, size)
    module.insert(
        "free".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("ptr"), LambdaParameter::top("size")].into(),
            ),
            OnionFastMap::default(),
            "mem::free",
            OnionKeyPool::create(vec!["ptr".into(), "size".into()]),
            &mem_free,
        ),
    );

    // mem.calloc(count, size)
    module.insert(
        "calloc".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("count"), LambdaParameter::top("size")].into(),
            ),
            OnionFastMap::default(),
            "mem::calloc",
            OnionKeyPool::create(vec!["count".into(), "size".into()]),
            &mem_calloc,
        ),
    );

    // mem.read(ptr, size)
    module.insert(
        "read".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("ptr"), LambdaParameter::top("size")].into(),
            ),
            OnionFastMap::default(),
            "mem::read",
            OnionKeyPool::create(vec!["ptr".into(), "size".into()]),
            &mem_read,
        ),
    );

    // mem.write(ptr, buffer)
    module.insert(
        "write".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("ptr"), LambdaParameter::top("buffer")].into(),
            ),
            OnionFastMap::default(),
            "mem::write",
            OnionKeyPool::create(vec!["ptr".into(), "buffer".into()]),
            &mem_write,
        ),
    );

    // mem.copy(dest, src, size)
    module.insert(
        "copy".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("dest"),
                    LambdaParameter::top("src"),
                    LambdaParameter::top("size"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "mem::copy",
            OnionKeyPool::create(vec!["dest".into(), "src".into(), "size".into()]),
            &mem_copy,
        ),
    );

    build_dict(module)
}
