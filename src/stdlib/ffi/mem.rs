use std::{alloc::{alloc, dealloc, Layout}, sync::Arc};

use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
    GC,
};

use crate::stdlib::{build_named_dict, get_attr_direct, wrap_native_function};

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

    // alloc 函数
    let mut alloc_params = IndexMap::new();
    alloc_params.insert(
        "size".to_string(),
        OnionObject::Undefined(Some("Size in bytes to allocate".to_string().into())).stabilize(),
    );
    module.insert(
        "alloc".to_string(),
        wrap_native_function(
            &build_named_dict(alloc_params),
            None,
            None,
            "mem::alloc".to_string(),
            &mem_alloc,
        ),
    );

    // free 函数
    let mut free_params = IndexMap::new();
    free_params.insert(
        "ptr".to_string(),
        OnionObject::Undefined(Some("Integer pointer address to memory to free".to_string().into())).stabilize(),
    );
    free_params.insert(
        "size".to_string(),
        OnionObject::Undefined(Some("Size of memory block".to_string().into())).stabilize(),
    );
    module.insert(
        "free".to_string(),
        wrap_native_function(
            &build_named_dict(free_params),
            None,
            None,
            "mem::free".to_string(),
            &mem_free,
        ),
    );

    // calloc 函数
    let mut calloc_params = IndexMap::new();
    calloc_params.insert(
        "count".to_string(),
        OnionObject::Undefined(Some("Number of elements".to_string().into())).stabilize(),
    );
    calloc_params.insert(
        "size".to_string(),
        OnionObject::Undefined(Some("Size of each element".to_string().into())).stabilize(),
    );
    module.insert(
        "calloc".to_string(),
        wrap_native_function(
            &build_named_dict(calloc_params),
            None,
            None,
            "mem::calloc".to_string(),
            &mem_calloc,
        ),
    );

    // read 函数
    let mut read_params = IndexMap::new();
    read_params.insert(
        "ptr".to_string(),
        OnionObject::Undefined(Some("Integer pointer address to memory to read".to_string().into())).stabilize(),
    );
    read_params.insert(
        "size".to_string(),
        OnionObject::Undefined(Some("Number of bytes to read".to_string().into())).stabilize(),
    );
    module.insert(
        "read".to_string(),
        wrap_native_function(
            &build_named_dict(read_params),
            None,
            None,
            "mem::read".to_string(),
            &mem_read,
        ),
    );

    // write 函数
    let mut write_params = IndexMap::new();
    write_params.insert(
        "ptr".to_string(),
        OnionObject::Undefined(Some("Integer pointer address to memory to write".to_string().into())).stabilize(),
    );
    write_params.insert(
        "buffer".to_string(),
        OnionObject::Undefined(Some("Bytes or string containing data to write".to_string().into())).stabilize(),
    );
    module.insert(
        "write".to_string(),
        wrap_native_function(
            &build_named_dict(write_params),
            None,
            None,
            "mem::write".to_string(),
            &mem_write,
        ),
    );

    // copy 函数
    let mut copy_params = IndexMap::new();
    copy_params.insert(
        "dest".to_string(),
        OnionObject::Undefined(Some("Integer destination pointer address".to_string().into())).stabilize(),
    );
    copy_params.insert(
        "src".to_string(),
        OnionObject::Undefined(Some("Integer source pointer address".to_string().into())).stabilize(),
    );
    copy_params.insert(
        "size".to_string(),
        OnionObject::Undefined(Some("Number of bytes to copy".to_string().into())).stabilize(),
    );
    module.insert(
        "copy".to_string(),
        wrap_native_function(
            &build_named_dict(copy_params),
            None,
            None,
            "mem::copy".to_string(),
            &mem_copy,
        ),
    );

    build_named_dict(module)
}
