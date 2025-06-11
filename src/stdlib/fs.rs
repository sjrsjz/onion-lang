use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
    GC,
};
use std::{fs, io::Write, path::Path};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

/// 读取文件内容作为字节
fn read_file(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match fs::read(path_str) {
                    Ok(content) => Ok(OnionObject::Bytes(content).stabilize()),
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to read file '{}': {}", path_str, e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 写入文件内容作为字节
fn write_file(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        let content = get_attr_direct(data, "content".to_string())?;
        
        path.weak().with_data(|path_data| {
            content.weak().with_data(|content_data| {
                match (path_data, content_data) {
                    (OnionObject::String(path_str), OnionObject::Bytes(content_bytes)) => {
                        match fs::write(path_str, content_bytes) {
                            Ok(_) => Ok(OnionObject::Null.stabilize()),
                            Err(e) => Err(RuntimeError::DetailedError(format!(
                                "Failed to write file '{}': {}", path_str, e
                            ))),
                        }
                    }
                    (OnionObject::String(path_str), OnionObject::String(content_str)) => {
                        // 为了向后兼容，仍然支持字符串输入，但转换为字节
                        match fs::write(path_str, content_str.as_bytes()) {
                            Ok(_) => Ok(OnionObject::Null.stabilize()),
                            Err(e) => Err(RuntimeError::DetailedError(format!(
                                "Failed to write file '{}': {}", path_str, e
                            ))),
                        }
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Path must be a string and content must be bytes or string".to_string(),
                    )),
                }
            })
        })
    })
}

/// 追加文件内容作为字节
fn append_file(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        let content = get_attr_direct(data, "content".to_string())?;
        
        path.weak().with_data(|path_data| {
            content.weak().with_data(|content_data| {
                match (path_data, content_data) {
                    (OnionObject::String(path_str), OnionObject::Bytes(content_bytes)) => {
                        match fs::OpenOptions::new().create(true).append(true).open(path_str) {
                            Ok(mut file) => {
                                match file.write_all(content_bytes) {
                                    Ok(_) => Ok(OnionObject::Null.stabilize()),
                                    Err(e) => Err(RuntimeError::DetailedError(format!(
                                        "Failed to append to file '{}': {}", path_str, e
                                    ))),
                                }
                            }
                            Err(e) => Err(RuntimeError::DetailedError(format!(
                                "Failed to open file '{}' for appending: {}", path_str, e
                            ))),
                        }
                    }
                    (OnionObject::String(path_str), OnionObject::String(content_str)) => {
                        // 为了向后兼容，仍然支持字符串输入，但转换为字节
                        match fs::OpenOptions::new().create(true).append(true).open(path_str) {
                            Ok(mut file) => {
                                match file.write_all(content_str.as_bytes()) {
                                    Ok(_) => Ok(OnionObject::Null.stabilize()),
                                    Err(e) => Err(RuntimeError::DetailedError(format!(
                                        "Failed to append to file '{}': {}", path_str, e
                                    ))),
                                }
                            }
                            Err(e) => Err(RuntimeError::DetailedError(format!(
                                "Failed to open file '{}' for appending: {}", path_str, e
                            ))),
                        }
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Path must be a string and content must be bytes or string".to_string(),
                    )),
                }
            })
        })
    })
}

/// 删除文件
fn remove_file(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match fs::remove_file(path_str) {
                    Ok(_) => Ok(OnionObject::Null.stabilize()),
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to remove file '{}': {}", path_str, e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 复制文件
fn copy_file(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let src = get_attr_direct(data, "src".to_string())?;
        let dest = get_attr_direct(data, "dest".to_string())?;
        
        src.weak().with_data(|src_data| {
            dest.weak().with_data(|dest_data| {
                match (src_data, dest_data) {
                    (OnionObject::String(src_str), OnionObject::String(dest_str)) => {
                        match fs::copy(src_str, dest_str) {
                            Ok(_) => Ok(OnionObject::Null.stabilize()),
                            Err(e) => Err(RuntimeError::DetailedError(format!(
                                "Failed to copy file from '{}' to '{}': {}", src_str, dest_str, e
                            ))),
                        }
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Source and destination must be strings".to_string(),
                    )),
                }
            })
        })
    })
}

/// 重命名/移动文件
fn rename_file(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let src = get_attr_direct(data, "src".to_string())?;
        let dest = get_attr_direct(data, "dest".to_string())?;
        
        src.weak().with_data(|src_data| {
            dest.weak().with_data(|dest_data| {
                match (src_data, dest_data) {
                    (OnionObject::String(src_str), OnionObject::String(dest_str)) => {
                        match fs::rename(src_str, dest_str) {
                            Ok(_) => Ok(OnionObject::Null.stabilize()),
                            Err(e) => Err(RuntimeError::DetailedError(format!(
                                "Failed to rename file from '{}' to '{}': {}", src_str, dest_str, e
                            ))),
                        }
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Source and destination must be strings".to_string(),
                    )),
                }
            })
        })
    })
}

/// 创建目录
fn create_dir(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match fs::create_dir(path_str) {
                    Ok(_) => Ok(OnionObject::Null.stabilize()),
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to create directory '{}': {}", path_str, e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 递归创建目录
fn create_dir_all(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match fs::create_dir_all(path_str) {
                    Ok(_) => Ok(OnionObject::Null.stabilize()),
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to create directory '{}': {}", path_str, e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 删除空目录
fn remove_dir(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match fs::remove_dir(path_str) {
                    Ok(_) => Ok(OnionObject::Null.stabilize()),
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to remove directory '{}': {}", path_str, e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 递归删除目录
fn remove_dir_all(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match fs::remove_dir_all(path_str) {
                    Ok(_) => Ok(OnionObject::Null.stabilize()),
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to remove directory '{}': {}", path_str, e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 列出目录内容
fn read_dir(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match fs::read_dir(path_str) {
                    Ok(entries) => {
                        let mut files = Vec::new();
                        for entry in entries {
                            match entry {
                                Ok(entry) => {
                                    let file_name = entry.file_name().to_string_lossy().to_string();
                                    files.push(OnionObject::String(file_name).to_cell());
                                }
                                Err(e) => {
                                    return Err(RuntimeError::DetailedError(format!(
                                        "Error reading directory entry: {}", e
                                    )));
                                }
                            }
                        }
                        Ok(OnionObject::Tuple(onion_vm::types::tuple::OnionTuple::new(files)).stabilize())
                    }
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to read directory '{}': {}", path_str, e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 获取文件元数据
fn file_metadata(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match fs::metadata(path_str) {
                    Ok(metadata) => {
                        let mut meta = IndexMap::new();
                        meta.insert("size".to_string(), OnionObject::Integer(metadata.len() as i64).stabilize());
                        meta.insert("is_file".to_string(), OnionObject::Boolean(metadata.is_file()).stabilize());
                        meta.insert("is_dir".to_string(), OnionObject::Boolean(metadata.is_dir()).stabilize());
                        meta.insert("readonly".to_string(), OnionObject::Boolean(metadata.permissions().readonly()).stabilize());
                        
                        if let Ok(modified) = metadata.modified() {
                            if let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH) {
                                meta.insert("modified".to_string(), OnionObject::Integer(duration.as_secs() as i64).stabilize());
                            }
                        }
                        
                        Ok(build_named_dict(meta))
                    }
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to get metadata for '{}': {}", path_str, e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 检查文件是否存在
fn exists(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                let exists = Path::new(path_str).exists();
                Ok(OnionObject::Boolean(exists).stabilize())
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 读取文本文件内容（UTF-8编码）
fn read_text(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match fs::read(path_str) {
                    Ok(bytes) => {
                        match String::from_utf8(bytes) {
                            Ok(text) => Ok(OnionObject::String(text).stabilize()),
                            Err(e) => Err(RuntimeError::DetailedError(format!(
                                "Failed to decode file '{}' as UTF-8: {}", path_str, e
                            ))),
                        }
                    }
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to read file '{}': {}", path_str, e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 写入文本文件内容（UTF-8编码）
fn write_text(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        let content = get_attr_direct(data, "content".to_string())?;
        
        path.weak().with_data(|path_data| {
            content.weak().with_data(|content_data| {
                match (path_data, content_data) {
                    (OnionObject::String(path_str), OnionObject::String(content_str)) => {
                        match fs::write(path_str, content_str.as_bytes()) {
                            Ok(_) => Ok(OnionObject::Null.stabilize()),
                            Err(e) => Err(RuntimeError::DetailedError(format!(
                                "Failed to write text file '{}': {}", path_str, e
                            ))),
                        }
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Path and content must be strings".to_string(),
                    )),
                }
            })
        })
    })
}

/// 追加文本文件内容（UTF-8编码）
fn append_text(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        let content = get_attr_direct(data, "content".to_string())?;
        
        path.weak().with_data(|path_data| {
            content.weak().with_data(|content_data| {
                match (path_data, content_data) {
                    (OnionObject::String(path_str), OnionObject::String(content_str)) => {
                        match fs::OpenOptions::new().create(true).append(true).open(path_str) {
                            Ok(mut file) => {
                                match file.write_all(content_str.as_bytes()) {
                                    Ok(_) => Ok(OnionObject::Null.stabilize()),
                                    Err(e) => Err(RuntimeError::DetailedError(format!(
                                        "Failed to append to text file '{}': {}", path_str, e
                                    ))),
                                }
                            }
                            Err(e) => Err(RuntimeError::DetailedError(format!(
                                "Failed to open text file '{}' for appending: {}", path_str, e
                            ))),
                        }
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Path and content must be strings".to_string(),
                    )),
                }
            })
        })
    })
}

/// 构建文件系统模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();    // read_file 函数 - 读取文件（字节）
    let mut read_file_params = IndexMap::new();
    read_file_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "read_file".to_string(),
        wrap_native_function(
            &build_named_dict(read_file_params),
            None,
            None,
            "fs::read_file".to_string(),
            &read_file,
        ),
    );

    // write_file 函数 - 写入文件（字节）
    let mut write_file_params = IndexMap::new();
    write_file_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    write_file_params.insert(
        "content".to_string(),
        OnionObject::Bytes(vec![]).stabilize(),
    );
    module.insert(
        "write_file".to_string(),
        wrap_native_function(
            &build_named_dict(write_file_params),
            None,
            None,
            "fs::write_file".to_string(),
            &write_file,
        ),
    );

    // append_file 函数 - 追加文件（字节）
    let mut append_file_params = IndexMap::new();
    append_file_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    append_file_params.insert(
        "content".to_string(),
        OnionObject::Bytes(vec![]).stabilize(),
    );
    module.insert(
        "append_file".to_string(),
        wrap_native_function(
            &build_named_dict(append_file_params),
            None,
            None,
            "fs::append_file".to_string(),
            &append_file,
        ),
    );

    // read_text 函数 - 读取文本文件
    let mut read_text_params = IndexMap::new();
    read_text_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "read_text".to_string(),
        wrap_native_function(
            &build_named_dict(read_text_params),
            None,
            None,
            "fs::read_text".to_string(),
            &read_text,
        ),
    );

    // write_text 函数 - 写入文本文件
    let mut write_text_params = IndexMap::new();
    write_text_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    write_text_params.insert(
        "content".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "write_text".to_string(),
        wrap_native_function(
            &build_named_dict(write_text_params),
            None,
            None,
            "fs::write_text".to_string(),
            &write_text,
        ),
    );

    // append_text 函数 - 追加文本文件
    let mut append_text_params = IndexMap::new();
    append_text_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    append_text_params.insert(
        "content".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "append_text".to_string(),
        wrap_native_function(
            &build_named_dict(append_text_params),
            None,
            None,
            "fs::append_text".to_string(),
            &append_text,
        ),
    );

    // remove_file 函数 - 删除文件
    let mut remove_file_params = IndexMap::new();
    remove_file_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "remove_file".to_string(),
        wrap_native_function(
            &build_named_dict(remove_file_params),
            None,
            None,
            "fs::remove_file".to_string(),
            &remove_file,
        ),
    );

    // copy_file 函数 - 复制文件
    let mut copy_file_params = IndexMap::new();
    copy_file_params.insert(
        "src".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    copy_file_params.insert(
        "dest".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "copy_file".to_string(),
        wrap_native_function(
            &build_named_dict(copy_file_params),
            None,
            None,
            "fs::copy_file".to_string(),
            &copy_file,
        ),
    );

    // rename_file 函数 - 重命名文件
    let mut rename_file_params = IndexMap::new();
    rename_file_params.insert(
        "src".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    rename_file_params.insert(
        "dest".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "rename_file".to_string(),
        wrap_native_function(
            &build_named_dict(rename_file_params),
            None,
            None,
            "fs::rename_file".to_string(),
            &rename_file,
        ),
    );

    // create_dir 函数 - 创建目录
    let mut create_dir_params = IndexMap::new();
    create_dir_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "create_dir".to_string(),
        wrap_native_function(
            &build_named_dict(create_dir_params),
            None,
            None,
            "fs::create_dir".to_string(),
            &create_dir,
        ),
    );

    // create_dir_all 函数 - 递归创建目录
    let mut create_dir_all_params = IndexMap::new();
    create_dir_all_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "create_dir_all".to_string(),
        wrap_native_function(
            &build_named_dict(create_dir_all_params),
            None,
            None,
            "fs::create_dir_all".to_string(),
            &create_dir_all,
        ),
    );

    // remove_dir 函数 - 删除空目录
    let mut remove_dir_params = IndexMap::new();
    remove_dir_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "remove_dir".to_string(),
        wrap_native_function(
            &build_named_dict(remove_dir_params),
            None,
            None,
            "fs::remove_dir".to_string(),
            &remove_dir,
        ),
    );

    // remove_dir_all 函数 - 递归删除目录
    let mut remove_dir_all_params = IndexMap::new();
    remove_dir_all_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "remove_dir_all".to_string(),
        wrap_native_function(
            &build_named_dict(remove_dir_all_params),
            None,
            None,
            "fs::remove_dir_all".to_string(),
            &remove_dir_all,
        ),
    );

    // read_dir 函数 - 列出目录内容
    let mut read_dir_params = IndexMap::new();
    read_dir_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "read_dir".to_string(),
        wrap_native_function(
            &build_named_dict(read_dir_params),
            None,
            None,
            "fs::read_dir".to_string(),
            &read_dir,
        ),
    );

    // file_metadata 函数 - 获取文件元数据
    let mut file_metadata_params = IndexMap::new();
    file_metadata_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "file_metadata".to_string(),
        wrap_native_function(
            &build_named_dict(file_metadata_params),
            None,
            None,
            "fs::file_metadata".to_string(),
            &file_metadata,
        ),
    );

    // exists 函数 - 检查文件是否存在
    let mut exists_params = IndexMap::new();
    exists_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string()).stabilize(),
    );
    module.insert(
        "exists".to_string(),
        wrap_native_function(
            &build_named_dict(exists_params),
            None,
            None,
            "fs::exists".to_string(),
            &exists,
        ),
    );

    build_named_dict(module)
}
