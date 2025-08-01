use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};
use std::{fs, io::Write, path::Path};

// 引入所需的辅助函数
use super::{build_dict, build_string_tuple, wrap_native_function};

/// 读取文件内容作为字节
fn read_file(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "read_file requires a 'path' argument".to_string().into(),
        ));
    };

    path_obj.weak().with_data(|path_data| match path_data {
        OnionObject::String(path_str) => match fs::read(path_str.as_ref()) {
            Ok(content) => Ok(OnionObject::Bytes(content.into()).stabilize()),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to read file '{path_str}': {e}").into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType(
            "Path must be a string".to_string().into(),
        )),
    })
}

/// 写入文件内容作为字节
fn write_file(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "write_file requires a 'path' argument".to_string().into(),
        ));
    };
    let Some(content_obj) = argument.get(&"content".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "write_file requires a 'content' argument"
                .to_string()
                .into(),
        ));
    };

    let path_str = match path_obj.weak() {
        OnionObject::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Path must be a string".to_string().into(),
            ));
        }
    };

    let content_bytes = match content_obj.weak() {
        OnionObject::Bytes(b) => b.as_ref().to_vec(),
        OnionObject::String(s) => s.as_bytes().to_vec(),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Content must be bytes or a string".to_string().into(),
            ));
        }
    };

    match fs::write(path_str.as_ref(), &content_bytes) {
        Ok(_) => Ok(OnionObject::Null.stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to write file '{path_str}': {e}").into(),
        )),
    }
}

/// 追加文件内容作为字节
fn append_file(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "append_file requires a 'path' argument".to_string().into(),
        ));
    };
    let Some(content_obj) = argument.get(&"content".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "append_file requires a 'content' argument"
                .to_string()
                .into(),
        ));
    };

    let path_str = match path_obj.weak() {
        OnionObject::String(s) => s.clone(),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Path must be a string".to_string().into(),
            ));
        }
    };

    let bytes_to_append = match content_obj.weak() {
        OnionObject::Bytes(b) => b.as_ref().to_vec(),
        OnionObject::String(s) => s.as_bytes().to_vec(),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Content must be bytes or a string".to_string().into(),
            ));
        }
    };

    match fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path_str.as_ref())
    {
        Ok(mut file) => match file.write_all(&bytes_to_append) {
            Ok(_) => Ok(OnionObject::Null.stabilize()),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to append to file '{path_str}': {e}").into(),
            )),
        },
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to open file '{path_str}' for appending: {e}").into(),
        )),
    }
}

/// 删除文件
fn remove_file(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "remove_file requires a 'path' argument".to_string().into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::String(path_str) => match fs::remove_file(path_str.as_ref()) {
            Ok(_) => Ok(OnionObject::Null.stabilize()),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to remove file '{path_str}': {e}").into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType(
            "Path must be a string".to_string().into(),
        )),
    }
}

/// 复制文件
fn copy_file(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(src_obj) = argument.get(&"src".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "copy_file requires a 'src' argument".to_string().into(),
        ));
    };
    let Some(dest_obj) = argument.get(&"dest".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "copy_file requires a 'dest' argument".to_string().into(),
        ));
    };

    let (src_str, dest_str) = match (src_obj.weak(), dest_obj.weak()) {
        (OnionObject::String(s), OnionObject::String(d)) => (s, d),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Source and destination must be strings".to_string().into(),
            ));
        }
    };

    match fs::copy(src_str.as_ref(), dest_str.as_ref()) {
        Ok(_) => Ok(OnionObject::Null.stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(
            format!(
                "Failed to copy file from '{src_str}' to '{dest_str}': {e}"
            )
            .into(),
        )),
    }
}

/// 重命名/移动文件
fn rename_file(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(src_obj) = argument.get(&"src".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "rename_file requires a 'src' argument".to_string().into(),
        ));
    };
    let Some(dest_obj) = argument.get(&"dest".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "rename_file requires a 'dest' argument".to_string().into(),
        ));
    };

    let (src_str, dest_str) = match (src_obj.weak(), dest_obj.weak()) {
        (OnionObject::String(s), OnionObject::String(d)) => (s, d),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Source and destination must be strings".to_string().into(),
            ));
        }
    };

    match fs::rename(src_str.as_ref(), dest_str.as_ref()) {
        Ok(_) => Ok(OnionObject::Null.stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(
            format!(
                "Failed to rename file from '{src_str}' to '{dest_str}': {e}"
            )
            .into(),
        )),
    }
}

/// 创建目录
fn create_dir(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "create_dir requires a 'path' argument".to_string().into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::String(path_str) => match fs::create_dir(path_str.as_ref()) {
            Ok(_) => Ok(OnionObject::Null.stabilize()),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to create directory '{path_str}': {e}").into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType(
            "Path must be a string".to_string().into(),
        )),
    }
}

/// 递归创建目录
fn create_dir_all(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "create_dir_all requires a 'path' argument"
                .to_string()
                .into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::String(path_str) => match fs::create_dir_all(path_str.as_ref()) {
            Ok(_) => Ok(OnionObject::Null.stabilize()),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to create directories '{path_str}': {e}").into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType(
            "Path must be a string".to_string().into(),
        )),
    }
}

/// 删除空目录
fn remove_dir(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "remove_dir requires a 'path' argument".to_string().into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::String(path_str) => match fs::remove_dir(path_str.as_ref()) {
            Ok(_) => Ok(OnionObject::Null.stabilize()),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to remove directory '{path_str}': {e}").into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType(
            "Path must be a string".to_string().into(),
        )),
    }
}

/// 递归删除目录
fn remove_dir_all(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "remove_dir_all requires a 'path' argument"
                .to_string()
                .into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::String(path_str) => match fs::remove_dir_all(path_str.as_ref()) {
            Ok(_) => Ok(OnionObject::Null.stabilize()),
            Err(e) => Err(RuntimeError::DetailedError(
                format!(
                    "Failed to remove directory and its contents '{path_str}': {e}"
                )
                .into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType(
            "Path must be a string".to_string().into(),
        )),
    }
}

/// 列出目录内容
fn read_dir(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "read_dir requires a 'path' argument".to_string().into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::String(path_str) => match fs::read_dir(path_str.as_ref()) {
            Ok(entries) => {
                let mut files = Vec::new();
                for entry in entries {
                    match entry {
                        Ok(entry) => {
                            let file_name = entry.file_name().to_string_lossy().to_string();
                            files.push(OnionObject::String(file_name.into()));
                        }
                        Err(e) => {
                            return Err(RuntimeError::DetailedError(
                                format!("Error reading directory entry in '{path_str}': {e}")
                                    .into(),
                            ));
                        }
                    }
                }
                Ok(OnionObject::Tuple(OnionTuple::new(files).into()).stabilize())
            }
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to read directory '{path_str}': {e}").into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType(
            "Path must be a string".to_string().into(),
        )),
    }
}

/// 获取文件元数据
fn file_metadata(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "file_metadata requires a 'path' argument"
                .to_string()
                .into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::String(path_str) => match fs::metadata(path_str.as_ref()) {
            Ok(metadata) => {
                let mut meta = IndexMap::new();
                meta.insert(
                    "size".to_string(),
                    OnionObject::Integer(metadata.len() as i64).stabilize(),
                );
                meta.insert(
                    "is_file".to_string(),
                    OnionObject::Boolean(metadata.is_file()).stabilize(),
                );
                meta.insert(
                    "is_dir".to_string(),
                    OnionObject::Boolean(metadata.is_dir()).stabilize(),
                );
                meta.insert(
                    "readonly".to_string(),
                    OnionObject::Boolean(metadata.permissions().readonly()).stabilize(),
                );

                if let Ok(modified) = metadata.modified() {
                    if let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH) {
                        meta.insert(
                            "modified".to_string(),
                            OnionObject::Integer(duration.as_secs() as i64).stabilize(),
                        );
                    }
                }
                Ok(build_dict(meta))
            }
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to get metadata for '{path_str}': {e}").into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType(
            "Path must be a string".to_string().into(),
        )),
    }
}

/// 检查文件是否存在
fn exists(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "exists requires a 'path' argument".to_string().into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::String(path_str) => {
            let exists = Path::new(path_str.as_ref()).exists();
            Ok(OnionObject::Boolean(exists).stabilize())
        }
        _ => Err(RuntimeError::InvalidType(
            "Path must be a string".to_string().into(),
        )),
    }
}

/// 读取文本文件内容（UTF-8编码）
fn read_text(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "read_text requires a 'path' argument".to_string().into(),
        ));
    };

    let path_str = match path_obj.weak() {
        OnionObject::String(s) => s,
        _ => {
            return Err(RuntimeError::InvalidType(
                "Path must be a string".to_string().into(),
            ));
        }
    };

    match fs::read(path_str.as_ref()) {
        Ok(bytes) => match String::from_utf8(bytes) {
            Ok(text) => Ok(OnionObject::String(text.into()).stabilize()),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to decode file '{path_str}' as UTF-8: {e}").into(),
            )),
        },
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to read file '{path_str}': {e}").into(),
        )),
    }
}

/// 写入文本文件内容（UTF-8编码）
fn write_text(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "write_text requires a 'path' argument".to_string().into(),
        ));
    };
    let Some(content_obj) = argument.get(&"content".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "write_text requires a 'content' argument"
                .to_string()
                .into(),
        ));
    };

    let (path_str, content_str) = match (path_obj.weak(), content_obj.weak()) {
        (OnionObject::String(p), OnionObject::String(c)) => (p, c),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Path and content must be strings".to_string().into(),
            ));
        }
    };

    match fs::write(path_str.as_ref(), content_str.as_bytes()) {
        Ok(_) => Ok(OnionObject::Null.stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to write text file '{path_str}': {e}").into(),
        )),
    }
}

/// 追加文本文件内容（UTF-8编码）
fn append_text(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get(&"path".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "append_text requires a 'path' argument".to_string().into(),
        ));
    };
    let Some(content_obj) = argument.get(&"content".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "append_text requires a 'content' argument"
                .to_string()
                .into(),
        ));
    };

    let (path_str, content_str) = match (path_obj.weak(), content_obj.weak()) {
        (OnionObject::String(p), OnionObject::String(c)) => (p, c),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Path and content must be strings".to_string().into(),
            ));
        }
    };

    match fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path_str.as_ref())
    {
        Ok(mut file) => match file.write_all(content_str.as_bytes()) {
            Ok(_) => Ok(OnionObject::Null.stabilize()),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to append to text file '{path_str}': {e}").into(),
            )),
        },
        Err(e) => Err(RuntimeError::DetailedError(
            format!(
                "Failed to open text file '{path_str}' for appending: {e}"
            )
            .into(),
        )),
    }
}

/// 构建文件系统模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    let single_path_param = OnionObject::String("path".to_string().into()).stabilize();
    let path_content_params = build_string_tuple(&["path", "content"]);
    let src_dest_params = build_string_tuple(&["src", "dest"]);

    module.insert(
        "read_file".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::read_file".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &read_file,
        ),
    );
    module.insert(
        "write_file".to_string(),
        wrap_native_function(
            &path_content_params,
            &OnionFastMap::default(),
            "fs::write_file".to_string(),
            OnionKeyPool::create(vec!["path".to_string(), "content".to_string()]),
            &write_file,
        ),
    );
    module.insert(
        "append_file".to_string(),
        wrap_native_function(
            &path_content_params,
            &OnionFastMap::default(),
            "fs::append_file".to_string(),
            OnionKeyPool::create(vec!["path".to_string(), "content".to_string()]),
            &append_file,
        ),
    );
    module.insert(
        "read_text".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::read_text".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &read_text,
        ),
    );
    module.insert(
        "write_text".to_string(),
        wrap_native_function(
            &path_content_params,
            &OnionFastMap::default(),
            "fs::write_text".to_string(),
            OnionKeyPool::create(vec!["path".to_string(), "content".to_string()]),
            &write_text,
        ),
    );
    module.insert(
        "append_text".to_string(),
        wrap_native_function(
            &path_content_params,
            &OnionFastMap::default(),
            "fs::append_text".to_string(),
            OnionKeyPool::create(vec!["path".to_string(), "content".to_string()]),
            &append_text,
        ),
    );
    module.insert(
        "remove_file".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::remove_file".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &remove_file,
        ),
    );
    module.insert(
        "copy_file".to_string(),
        wrap_native_function(
            &src_dest_params,
            &OnionFastMap::default(),
            "fs::copy_file".to_string(),
            OnionKeyPool::create(vec!["src".to_string(), "dest".to_string()]),
            &copy_file,
        ),
    );
    module.insert(
        "rename_file".to_string(),
        wrap_native_function(
            &src_dest_params,
            &OnionFastMap::default(),
            "fs::rename_file".to_string(),
            OnionKeyPool::create(vec!["src".to_string(), "dest".to_string()]),
            &rename_file,
        ),
    );
    module.insert(
        "create_dir".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::create_dir".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &create_dir,
        ),
    );
    module.insert(
        "create_dir_all".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::create_dir_all".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &create_dir_all,
        ),
    );
    module.insert(
        "remove_dir".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::remove_dir".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &remove_dir,
        ),
    );
    module.insert(
        "remove_dir_all".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::remove_dir_all".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &remove_dir_all,
        ),
    );
    module.insert(
        "read_dir".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::read_dir".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &read_dir,
        ),
    );
    module.insert(
        "file_metadata".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::file_metadata".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &file_metadata,
        ),
    );
    module.insert(
        "exists".to_string(),
        wrap_native_function(
            &single_path_param,
            &OnionFastMap::default(),
            "fs::exists".to_string(),
            OnionKeyPool::create(vec!["path".to_string()]),
            &exists,
        ),
    );

    build_dict(module)
}
