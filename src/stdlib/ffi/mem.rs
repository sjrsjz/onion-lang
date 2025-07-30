use std::{alloc::{alloc, dealloc, Layout}, sync::Arc};

use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
    GC,
};
use rustc_hash::FxHashMap;

use crate::stdlib::{build_dict, build_string_tuple, get_attr_direct, wrap_native_function};

/// 分配内存
fn mem_alloc(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let size = get_attr_direct(data, "size".to_string())?;
        size.weak().with_data(|size_data| match size_data {
            OnionObject::Integer(n) => {
                if *n <= 0 {
                    return Err(RuntimeError::InvalidOperation(
                        "Size must be positive".to_string().into(),
                    ));
                }
                
                let size = *n as usize;
                
                // 创建内存布局
                let layout = match Layout::from_size_align(size, 1) {
                    Ok(layout) => layout,
                    Err(_) => {
                        return Err(RuntimeError::InvalidOperation(
                            "Invalid memory layout".to_string().into(),
                        ))
                    }
                };
                
                // 分配内存
                unsafe {
                    let ptr = alloc(layout);
                    if ptr.is_null() {
                        Err(RuntimeError::InvalidOperation(
                            "Memory allocation failed".to_string().into(),
                        ))
                    } else {
                        // 返回指针地址作为整数
                        Ok(OnionObject::Integer(ptr as usize as i64).stabilize())
                    }
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "mem_alloc requires integer size".to_string().into(),
            )),
        })
    })
}

/// 释放内存
fn mem_free(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let ptr_arg = get_attr_direct(data, "ptr".to_string())?;
        let size_arg = get_attr_direct(data, "size".to_string())?;
        
        ptr_arg.weak().with_data(|ptr_data| {
            size_arg.weak().with_data(|size_data| {
                match (ptr_data, size_data) {
                    (OnionObject::Integer(addr), OnionObject::Integer(size)) => {
                        if *addr == 0 {
                            return Err(RuntimeError::InvalidOperation(
                                "Cannot free null pointer".to_string().into(),
                            ));
                        }
                        
                        if *size <= 0 {
                            return Err(RuntimeError::InvalidOperation(
                                "Size must be positive".to_string().into(),
                            ));
                        }
                        
                        let size = *size as usize;
                        
                        // 创建内存布局
                        let layout = match Layout::from_size_align(size, 1) {
                            Ok(layout) => layout,
                            Err(_) => {
                                return Err(RuntimeError::InvalidOperation(
                                    "Invalid memory layout".to_string().into(),
                                ))
                            }
                        };
                        
                        // 释放内存
                        unsafe {
                            dealloc(*addr as usize as *mut u8, layout);
                        }
                        
                        Ok(OnionObject::Null.stabilize())
                    }
                    (_, _) => Err(RuntimeError::InvalidOperation(
                        "mem_free requires integer pointer address and size arguments".to_string().into(),
                    )),
                }
            })
        })
    })
}

/// 分配并清零内存
fn mem_calloc(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let count = get_attr_direct(data, "count".to_string())?;
        let size = get_attr_direct(data, "size".to_string())?;
        
        count.weak().with_data(|count_data| {
            size.weak().with_data(|size_data| {
                match (count_data, size_data) {
                    (OnionObject::Integer(count), OnionObject::Integer(size)) => {
                        if *count <= 0 || *size <= 0 {
                            return Err(RuntimeError::InvalidOperation(
                                "Count and size must be positive".to_string().into(),
                            ));
                        }
                        
                        let total_size = (*count as usize) * (*size as usize);
                        
                        // 创建内存布局
                        let layout = match Layout::from_size_align(total_size, 1) {
                            Ok(layout) => layout,
                            Err(_) => {
                                return Err(RuntimeError::InvalidOperation(
                                    "Invalid memory layout".to_string().into(),
                                ))
                            }
                        };
                        
                        // 分配内存
                        unsafe {
                            let ptr = alloc(layout);
                            if ptr.is_null() {
                                Err(RuntimeError::InvalidOperation(
                                    "Memory allocation failed".to_string().into(),
                                ))
                            } else {
                                // 清零内存
                                std::ptr::write_bytes(ptr, 0, total_size);
                                // 返回指针地址作为整数
                                Ok(OnionObject::Integer(ptr as usize as i64).stabilize())
                            }
                        }
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "mem_calloc requires integer count and size".to_string().into(),
                    )),
                }
            })
        })
    })
}

/// 读取内存内容到缓冲区
fn mem_read(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let ptr_arg = get_attr_direct(data, "ptr".to_string())?;
        let size_arg = get_attr_direct(data, "size".to_string())?;
        
        ptr_arg.weak().with_data(|ptr_data| {
            size_arg.weak().with_data(|size_data| {
                match (ptr_data, size_data) {
                    (OnionObject::Integer(addr), OnionObject::Integer(size)) => {
                        if *addr == 0 {
                            return Err(RuntimeError::InvalidOperation(
                                "Cannot read from null pointer".to_string().into(),
                            ));
                        }
                        
                        if *size <= 0 {
                            return Err(RuntimeError::InvalidOperation(
                                "Size must be positive".to_string().into(),
                            ));
                        }
                        
                        let size = *size as usize;
                        
                        // 读取内存内容
                        unsafe {
                            let slice = std::slice::from_raw_parts(*addr as usize as *const u8, size);
                            let buffer = slice.to_vec();
                            Ok(OnionObject::Bytes(Arc::new(buffer)).stabilize())
                        }
                    }
                    (_, _) => Err(RuntimeError::InvalidOperation(
                        "mem_read requires integer pointer address and size arguments".to_string().into(),
                    )),
                }
            })
        })
    })
}

/// 写入缓冲区内容到内存
fn mem_write(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let ptr_arg = get_attr_direct(data, "ptr".to_string())?;
        let buffer_arg = get_attr_direct(data, "buffer".to_string())?;
        
        ptr_arg.weak().with_data(|ptr_data| {
            buffer_arg.weak().with_data(|buffer_data| {
                match (ptr_data, buffer_data) {
                    (OnionObject::Integer(addr), OnionObject::Bytes(buffer)) => {
                        if *addr == 0 {
                            return Err(RuntimeError::InvalidOperation(
                                "Cannot write to null pointer".to_string().into(),
                            ));
                        }
                        
                        // 写入内存
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                buffer.as_ptr(),
                                *addr as usize as *mut u8,
                                buffer.len()
                            );
                        }
                        
                        Ok(OnionObject::Integer(buffer.len() as i64).stabilize())
                    }
                    (OnionObject::Integer(addr), OnionObject::String(s)) => {
                        if *addr == 0 {
                            return Err(RuntimeError::InvalidOperation(
                                "Cannot write to null pointer".to_string().into(),
                            ));
                        }
                        
                        let bytes = s.as_bytes();
                        // 写入内存
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                bytes.as_ptr(),
                                *addr as usize as *mut u8,
                                bytes.len()
                            );
                        }
                        
                        Ok(OnionObject::Integer(bytes.len() as i64).stabilize())
                    }
                    (_, _) => Err(RuntimeError::InvalidOperation(
                        "mem_write requires integer pointer address and bytes/string arguments".to_string().into(),
                    )),
                }
            })
        })
    })
}

/// 复制内存
fn mem_copy(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let dest_arg = get_attr_direct(data, "dest".to_string())?;
        let src_arg = get_attr_direct(data, "src".to_string())?;
        let size_arg = get_attr_direct(data, "size".to_string())?;
        
        dest_arg.weak().with_data(|dest_data| {
            src_arg.weak().with_data(|src_data| {
                size_arg.weak().with_data(|size_data| {
                    match (dest_data, src_data, size_data) {
                        (OnionObject::Integer(dest_addr), OnionObject::Integer(src_addr), OnionObject::Integer(size)) => {
                            if *dest_addr == 0 || *src_addr == 0 {
                                return Err(RuntimeError::InvalidOperation(
                                    "Cannot copy from/to null pointer".to_string().into(),
                                ));
                            }
                            
                            if *size <= 0 {
                                return Err(RuntimeError::InvalidOperation(
                                    "Size must be positive".to_string().into(),
                                ));
                            }
                            
                            let size = *size as usize;
                            
                            // 复制内存
                            unsafe {
                                std::ptr::copy_nonoverlapping(
                                    *src_addr as usize as *const u8,
                                    *dest_addr as usize as *mut u8,
                                    size
                                );
                            }
                            
                            Ok(OnionObject::Null.stabilize())
                        }
                        (_, _, _) => Err(RuntimeError::InvalidOperation(
                            "mem_copy requires integer dest pointer, src pointer, and size arguments".to_string().into(),
                        )),
                    }
                })
            })
        })
    })
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    module.insert(
        "alloc".to_string(),
        wrap_native_function(
            &OnionObject::String("size".to_string().into()).stabilize(), // 这里不使用build_string_tuple是因为我们期望其能直接接收一个元组而非通过单元素元组传入
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
