use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        boolean_value::OnionBooleanValue,
        bytes_value::OnionBytesValue,
        integer_value::OnionIntegerValue,
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        string_value::OnionStringValue,
        tuple::OnionTuple,
        undefined::OnionUndefined,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};
use std::{fs, io::Write, path::Path};

// 引入所需的辅助函数
use super::{build_dict, wrap_native_function};

// --- Helper functions for robust argument parsing ---
fn get_string_arg<'a>(
    argument: &'a OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<&'a str, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires a '{name}' argument")
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::StringValue(s) => Ok(s.value()),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be a string")
                .to_string()
                .into(),
        )),
    }
}

fn get_content_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<Vec<u8>, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires a '{name}' argument")
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::BytesValue(b) => Ok(b.value().to_vec()),
        OnionObject::StringValue(s) => Ok(s.value().as_bytes().to_vec()),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be bytes or a string")
                .to_string()
                .into(),
        )),
    }
}

/// 读取文件内容作为字节
fn read_file(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let path = get_string_arg(argument, "path")?;
    match fs::read(path) {
        Ok(content) => Ok(OnionBytesValue::new_static(&content)),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to read file '{path}': {e}").into(),
        )),
    }
}

/// 写入文件内容作为字节
fn write_file(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let path = get_string_arg(argument, "path")?;
    let content = get_content_arg(argument, "content")?;

    match fs::write(path, &content) {
        Ok(_) => Ok(OnionUndefined::new_static(None)),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to write file '{path}': {e}").into(),
        )),
    }
}

/// 追加文件内容作为字节
fn append_file(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let path = get_string_arg(argument, "path")?;
    let content = get_content_arg(argument, "content")?;

    match fs::OpenOptions::new().create(true).append(true).open(path) {
        Ok(mut file) => match file.write_all(&content) {
            Ok(_) => Ok(OnionUndefined::new_static(None)),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to append to file '{path}': {e}").into(),
            )),
        },
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to open file '{path}' for appending: {e}").into(),
        )),
    }
}

/// 删除文件
fn remove_file(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let path = get_string_arg(argument, "path")?;
    match fs::remove_file(path) {
        Ok(_) => Ok(OnionUndefined::new_static(None)),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to remove file '{path}': {e}").into(),
        )),
    }
}

/// 复制文件
fn copy_file(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let src = get_string_arg(argument, "src")?;
    let dest = get_string_arg(argument, "dest")?;

    match fs::copy(src, dest) {
        Ok(_) => Ok(OnionUndefined::new_static(None)),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to copy file from '{src}' to '{dest}': {e}").into(),
        )),
    }
}

/// 重命名/移动文件
fn rename_file(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(src_obj) = argument.get("src") else {
        return Err(RuntimeError::DetailedError(
            "rename_file requires a 'src' argument".into(),
        ));
    };
    let Some(dest_obj) = argument.get("dest") else {
        return Err(RuntimeError::DetailedError(
            "rename_file requires a 'dest' argument".into(),
        ));
    };

    let (src_str, dest_str) = match (src_obj.weak(), dest_obj.weak()) {
        (OnionObject::StringValue(s), OnionObject::StringValue(d)) => (s, d),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Source and destination must be strings".into(),
            ));
        }
    };

    match fs::rename(src_str.value(), dest_str.value()) {
        Ok(_) => Ok(OnionUndefined::new_static(None)),
        Err(e) => Err(RuntimeError::DetailedError(
            format!(
                "Failed to rename file from '{}' to '{}': {e}",
                src_str.value(),
                dest_str.value()
            )
            .into(),
        )),
    }
}

/// 创建目录
fn create_dir(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "create_dir requires a 'path' argument".into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::StringValue(path_str) => match fs::create_dir(path_str.value()) {
            Ok(_) => Ok(OnionUndefined::new_static(None)),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to create directory '{}': {e}", path_str.value()).into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType("Path must be a string".into())),
    }
}

/// 递归创建目录
fn create_dir_all(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "create_dir_all requires a 'path' argument"
                .to_string()
                .into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::StringValue(path_str) => match fs::create_dir_all(path_str.value()) {
            Ok(_) => Ok(OnionUndefined::new_static(None)),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to create directories '{}': {e}", path_str.value()).into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType("Path must be a string".into())),
    }
}

/// 删除空目录
fn remove_dir(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "remove_dir requires a 'path' argument".into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::StringValue(path_str) => match fs::remove_dir(path_str.value()) {
            Ok(_) => Ok(OnionUndefined::new_static(None)),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to remove directory '{}': {e}", path_str.value()).into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType("Path must be a string".into())),
    }
}

/// 递归删除目录
fn remove_dir_all(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "remove_dir_all requires a 'path' argument"
                .to_string()
                .into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::StringValue(path_str) => match fs::remove_dir_all(path_str.value()) {
            Ok(_) => Ok(OnionUndefined::new_static(None)),
            Err(e) => Err(RuntimeError::DetailedError(
                format!(
                    "Failed to remove directory and its contents '{}': {e}",
                    path_str.value()
                )
                .into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType("Path must be a string".into())),
    }
}

/// 列出目录内容
fn read_dir(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "read_dir requires a 'path' argument".into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::StringValue(path_str) => match fs::read_dir(path_str.value()) {
            Ok(entries) => {
                let mut files = Vec::new();
                for entry in entries {
                    match entry {
                        Ok(entry) => {
                            let file_name = entry.file_name().to_string_lossy().to_string();
                            files.push(OnionObject::StringValue(OnionStringValue::new(file_name)));
                        }
                        Err(e) => {
                            return Err(RuntimeError::DetailedError(
                                format!(
                                    "Error reading directory entry in '{}': {e}",
                                    path_str.value()
                                )
                                .into(),
                            ));
                        }
                    }
                }
                Ok(OnionObject::Tuple(OnionTuple::new(files).into()).stabilize())
            }
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to read directory '{}': {e}", path_str.value()).into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType("Path must be a string".into())),
    }
}

/// 获取文件元数据
fn file_metadata(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "file_metadata requires a 'path' argument"
                .to_string()
                .into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::StringValue(path_str) => match fs::metadata(path_str.value()) {
            Ok(metadata) => {
                let mut meta = IndexMap::new();
                meta.insert(
                    "size".to_string(),
                    OnionIntegerValue::new_static(metadata.len() as i64),
                );
                meta.insert(
                    "is_file".to_string(),
                    OnionBooleanValue::new_static(metadata.is_file()),
                );
                meta.insert(
                    "is_dir".to_string(),
                    OnionBooleanValue::new_static(metadata.is_dir()),
                );
                meta.insert(
                    "readonly".to_string(),
                    OnionBooleanValue::new_static(metadata.permissions().readonly()),
                );

                if let Ok(modified) = metadata.modified() {
                    if let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH) {
                        meta.insert(
                            "modified".to_string(),
                            OnionIntegerValue::new_static(duration.as_secs() as i64),
                        );
                    }
                }
                Ok(build_dict(meta))
            }
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to get metadata for '{}': {e}", path_str.value()).into(),
            )),
        },
        _ => Err(RuntimeError::InvalidType("Path must be a string".into())),
    }
}

/// 检查文件是否存在
fn exists(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "exists requires a 'path' argument".into(),
        ));
    };

    match path_obj.weak() {
        OnionObject::StringValue(path_str) => {
            let exists = Path::new(path_str.value()).exists();
            Ok(OnionBooleanValue::new_static(exists))
        }
        _ => Err(RuntimeError::InvalidType("Path must be a string".into())),
    }
}

/// 读取文本文件内容（UTF-8编码）
fn read_text(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "read_text requires a 'path' argument".into(),
        ));
    };

    let path_str = match path_obj.weak() {
        OnionObject::StringValue(s) => s,
        _ => {
            return Err(RuntimeError::InvalidType("Path must be a string".into()));
        }
    };

    match fs::read(path_str.value()) {
        Ok(bytes) => match String::from_utf8(bytes) {
            Ok(text) => Ok(OnionStringValue::new_static(&text)),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to decode file '{}': {e}", path_str.value()).into(),
            )),
        },
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to read file '{}': {e}", path_str.value()).into(),
        )),
    }
}

/// 写入文本文件内容（UTF-8编码）
fn write_text(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "write_text requires a 'path' argument".into(),
        ));
    };
    let Some(content_obj) = argument.get("content") else {
        return Err(RuntimeError::DetailedError(
            "write_text requires a 'content' argument"
                .to_string()
                .into(),
        ));
    };

    let (path_str, content_str) = match (path_obj.weak(), content_obj.weak()) {
        (OnionObject::StringValue(p), OnionObject::StringValue(c)) => (p, c),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Path and content must be strings".into(),
            ));
        }
    };

    match fs::write(path_str.value(), content_str.value().as_bytes()) {
        Ok(_) => Ok(OnionUndefined::new_static(None)),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to write text file '{}': {e}", path_str.value()).into(),
        )),
    }
}

/// 追加文本文件内容（UTF-8编码）
fn append_text(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(path_obj) = argument.get("path") else {
        return Err(RuntimeError::DetailedError(
            "append_text requires a 'path' argument".into(),
        ));
    };
    let Some(content_obj) = argument.get("content") else {
        return Err(RuntimeError::DetailedError(
            "append_text requires a 'content' argument"
                .to_string()
                .into(),
        ));
    };

    let (path_str, content_str) = match (path_obj.weak(), content_obj.weak()) {
        (OnionObject::StringValue(p), OnionObject::StringValue(c)) => (p, c),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Path and content must be strings".into(),
            ));
        }
    };

    match fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path_str.value())
    {
        Ok(mut file) => match file.write_all(content_str.value().as_bytes()) {
            Ok(_) => Ok(OnionUndefined::new_static(None)),
            Err(e) => Err(RuntimeError::DetailedError(
                format!("Failed to append to text file '{}': {e}", path_str.value()).into(),
            )),
        },
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to open text file '{}': {e}", path_str.value()).into(),
        )),
    }
}

/// 构建文件系统模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // --- Single-argument functions: (path) ---
    module.insert(
        "read_file".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::read_file",
            OnionKeyPool::create(vec!["path".into()]),
            &read_file,
        ),
    );
    module.insert(
        "read_text".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::read_text",
            OnionKeyPool::create(vec!["path".into()]),
            &read_text,
        ),
    );
    module.insert(
        "remove_file".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::remove_file",
            OnionKeyPool::create(vec!["path".into()]),
            &remove_file,
        ),
    );
    module.insert(
        "create_dir".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::create_dir",
            OnionKeyPool::create(vec!["path".into()]),
            &create_dir,
        ),
    );
    module.insert(
        "create_dir_all".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::create_dir_all",
            OnionKeyPool::create(vec!["path".into()]),
            &create_dir_all,
        ),
    );
    module.insert(
        "remove_dir".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::remove_dir",
            OnionKeyPool::create(vec!["path".into()]),
            &remove_dir,
        ),
    );
    module.insert(
        "remove_dir_all".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::remove_dir_all",
            OnionKeyPool::create(vec!["path".into()]),
            &remove_dir_all,
        ),
    );
    module.insert(
        "read_dir".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::read_dir",
            OnionKeyPool::create(vec!["path".into()]),
            &read_dir,
        ),
    );
    module.insert(
        "file_metadata".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::file_metadata",
            OnionKeyPool::create(vec!["path".into()]),
            &file_metadata,
        ),
    );
    module.insert(
        "exists".to_string(),
        wrap_native_function(
            LambdaParameter::top("path"),
            OnionFastMap::default(),
            "fs::exists",
            OnionKeyPool::create(vec!["path".into()]),
            &exists,
        ),
    );

    // --- Two-argument functions: (path, content) ---
    let path_content_params = LambdaParameter::Multiple(
        [
            LambdaParameter::top("path"),
            LambdaParameter::top("content"),
        ]
        .into(),
    );
    module.insert(
        "write_file".to_string(),
        wrap_native_function(
            path_content_params.clone(),
            OnionFastMap::default(),
            "fs::write_file",
            OnionKeyPool::create(vec!["path".into(), "content".into()]),
            &write_file,
        ),
    );
    module.insert(
        "append_file".to_string(),
        wrap_native_function(
            path_content_params.clone(),
            OnionFastMap::default(),
            "fs::append_file",
            OnionKeyPool::create(vec!["path".into(), "content".into()]),
            &append_file,
        ),
    );
    module.insert(
        "write_text".to_string(),
        wrap_native_function(
            path_content_params.clone(),
            OnionFastMap::default(),
            "fs::write_text",
            OnionKeyPool::create(vec!["path".into(), "content".into()]),
            &write_text,
        ),
    );
    module.insert(
        "append_text".to_string(),
        wrap_native_function(
            path_content_params, // Can move on the last use
            OnionFastMap::default(),
            "fs::append_text",
            OnionKeyPool::create(vec!["path".into(), "content".into()]),
            &append_text,
        ),
    );

    // --- Two-argument functions: (src, dest) ---
    let src_dest_params = LambdaParameter::Multiple(
        [LambdaParameter::top("src"), LambdaParameter::top("dest")].into(),
    );
    module.insert(
        "copy_file".to_string(),
        wrap_native_function(
            src_dest_params.clone(),
            OnionFastMap::default(),
            "fs::copy_file",
            OnionKeyPool::create(vec!["src".into(), "dest".into()]),
            &copy_file,
        ),
    );
    module.insert(
        "rename_file".to_string(),
        wrap_native_function(
            src_dest_params, // Can move on the last use
            OnionFastMap::default(),
            "fs::rename_file",
            OnionKeyPool::create(vec!["src".into(), "dest".into()]),
            &rename_file,
        ),
    );

    build_dict(module)
}
