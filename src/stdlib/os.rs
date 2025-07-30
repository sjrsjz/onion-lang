use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError, onion_tuple, types::{object::{OnionObject, OnionObjectCell, OnionStaticObject}, tuple::OnionTuple}, GC
};
use rustc_hash::FxHashMap;
use std::{env, process::Command};

// 引入所需的辅助函数
use super::{build_dict, build_string_tuple, wrap_native_function};

fn get_string_arg<'a>(
    argument: &'a FxHashMap<String, OnionStaticObject>,
    name: &str,
) -> Result<&'a str, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires a '{}' argument", name)
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::String(s) => Ok(s.as_ref()),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{}' must be a string", name)
                .to_string()
                .into(),
        )),
    }
}

/// 执行系统命令
fn system(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let cmd_str = get_string_arg(argument, "command")?;

    let (shell, flag) = if cfg!(target_os = "windows") {
        ("cmd", "/C")
    } else {
        ("sh", "-c")
    };

    match Command::new(shell).arg(flag).arg(cmd_str).output() {
        Ok(output) => {
            #[cfg(target_os = "windows")]
            fn decode_bytes(bytes: &[u8]) -> String {
                use encoding_rs::GBK;
                let (cow, _, had_errors) = GBK.decode(bytes);
                if !had_errors {
                    cow.into_owned()
                } else {
                    String::from_utf8_lossy(bytes).to_string()
                }
            }
            #[cfg(not(target_os = "windows"))]
            fn decode_bytes(bytes: &[u8]) -> String {
                String::from_utf8_lossy(bytes).to_string()
            }

            let stdout = decode_bytes(&output.stdout);
            let stderr = decode_bytes(&output.stderr);
            let status = output.status.code().unwrap_or(-1);

            let mut result = IndexMap::new();
            result.insert(
                "stdout".to_string(),
                OnionObject::String(stdout.into()).stabilize(),
            );
            result.insert(
                "stderr".to_string(),
                OnionObject::String(stderr.into()).stabilize(),
            );
            result.insert(
                "status".to_string(),
                OnionObject::Integer(status as i64).stabilize(),
            );
            result.insert(
                "success".to_string(),
                OnionObject::Boolean(output.status.success()).stabilize(),
            );

            Ok(build_dict(result))
        }
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to execute command: {}", e).into(),
        )),
    }
}

/// 执行命令并返回退出码
fn system_code(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let cmd_str = get_string_arg(argument, "command")?;
    let (shell, flag) = if cfg!(target_os = "windows") {
        ("cmd", "/C")
    } else {
        ("sh", "-c")
    };

    match Command::new(shell).arg(flag).arg(cmd_str).status() {
        Ok(status) => {
            let code = status.code().unwrap_or(-1);
            Ok(OnionObject::Integer(code as i64).stabilize())
        }
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to execute command: {}", e).into(),
        )),
    }
}

/// 改变当前工作目录
fn chdir(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let path_str = get_string_arg(argument, "path")?;
    match env::set_current_dir(path_str) {
        Ok(_) => Ok(OnionObject::Null.stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to change directory to '{}': {}", path_str, e).into(),
        )),
    }
}

/// 获取当前用户名
fn username(
    _argument: &FxHashMap<String, OnionStaticObject>, // No arguments needed
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    match env::var("USERNAME").or_else(|_| env::var("USER")) {
        Ok(user) => Ok(OnionObject::String(user.into()).stabilize()),
        Err(_) => Ok(OnionObject::String("unknown".to_string().into()).stabilize()),
    }
}

/// 获取主目录路径
fn home_dir(
    _argument: &FxHashMap<String, OnionStaticObject>, // No arguments needed
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    match dirs::home_dir() {
        Some(path) => {
            Ok(OnionObject::String(path.to_string_lossy().to_string().into()).stabilize())
        }
        None => Err(RuntimeError::DetailedError(
            "Could not determine home directory".to_string().into(),
        )),
    }
}

/// 获取临时目录路径
fn temp_dir(
    _argument: &FxHashMap<String, OnionStaticObject>, // No arguments needed
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let temp_path = env::temp_dir();
    Ok(OnionObject::String(temp_path.to_string_lossy().to_string().into()).stabilize())
}

/// 检查文件或目录是否存在
fn path_exists(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let path_str = get_string_arg(argument, "path")?;
    let exists = std::path::Path::new(path_str).exists();
    Ok(OnionObject::Boolean(exists).stabilize())
}

/// 检查路径是否是目录
fn is_dir(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let path_str = get_string_arg(argument, "path")?;
    let is_directory = std::path::Path::new(path_str).is_dir();
    Ok(OnionObject::Boolean(is_directory).stabilize())
}

/// 检查路径是否是文件
fn is_file(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let path_str = get_string_arg(argument, "path")?;
    let is_file = std::path::Path::new(path_str).is_file();
    Ok(OnionObject::Boolean(is_file).stabilize())
}

/// 连接路径
fn path_join(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let base_str = get_string_arg(argument, "base")?;
    let path_str = get_string_arg(argument, "path")?;
    let joined = std::path::Path::new(base_str).join(path_str);
    Ok(OnionObject::String(joined.to_string_lossy().to_string().into()).stabilize())
}

/// 构建操作系统模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    let command_arg = OnionObject::String("command".to_string().into()).stabilize();
    let path_arg = OnionObject::String("path".to_string().into()).stabilize();
    let no_args = onion_tuple!(); // Standard way to define no parameters

    module.insert(
        "system".to_string(),
        wrap_native_function(
            &command_arg,
            &FxHashMap::default(),
            "os::system".to_string(),
            &system,
        ),
    );
    module.insert(
        "system_code".to_string(),
        wrap_native_function(
            &command_arg,
            &FxHashMap::default(),
            "os::system_code".to_string(),
            &system_code,
        ),
    );
    module.insert(
        "chdir".to_string(),
        wrap_native_function(
            &path_arg,
            &FxHashMap::default(),
            "os::chdir".to_string(),
            &chdir,
        ),
    );
    module.insert(
        "username".to_string(),
        wrap_native_function(
            &no_args,
            &FxHashMap::default(),
            "os::username".to_string(),
            &username,
        ),
    );
    module.insert(
        "home_dir".to_string(),
        wrap_native_function(
            &no_args,
            &FxHashMap::default(),
            "os::home_dir".to_string(),
            &home_dir,
        ),
    );
    module.insert(
        "temp_dir".to_string(),
        wrap_native_function(
            &no_args,
            &FxHashMap::default(),
            "os::temp_dir".to_string(),
            &temp_dir,
        ),
    );
    module.insert(
        "path_exists".to_string(),
        wrap_native_function(
            &path_arg,
            &FxHashMap::default(),
            "os::path_exists".to_string(),
            &path_exists,
        ),
    );
    module.insert(
        "is_dir".to_string(),
        wrap_native_function(
            &path_arg,
            &FxHashMap::default(),
            "os::is_dir".to_string(),
            &is_dir,
        ),
    );
    module.insert(
        "is_file".to_string(),
        wrap_native_function(
            &path_arg,
            &FxHashMap::default(),
            "os::is_file".to_string(),
            &is_file,
        ),
    );
    module.insert(
        "path_join".to_string(),
        wrap_native_function(
            &build_string_tuple(&["base", "path"]),
            &FxHashMap::default(),
            "os::path_join".to_string(),
            &path_join,
        ),
    );

    build_dict(module)
}
