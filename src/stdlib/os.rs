use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
    GC,
};
use std::{env, process::Command};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

/// 执行系统命令
fn system(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let command = get_attr_direct(data, "command".to_string())?;
        command.weak().with_data(|command_data| match command_data {
            OnionObject::String(cmd_str) => {
                // 根据操作系统选择不同的shell
                let (shell, flag) = if cfg!(target_os = "windows") {
                    ("cmd", "/C")
                } else {
                    ("sh", "-c")
                };

                match Command::new(shell).arg(flag).arg(cmd_str.as_ref()).output() {
                    Ok(output) => {
                        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                        let status = output.status.code().unwrap_or(-1);

                        let mut result = IndexMap::new();
                        result.insert("stdout".to_string(), OnionObject::String(stdout.into()).stabilize());
                        result.insert("stderr".to_string(), OnionObject::String(stderr.into()).stabilize());
                        result.insert("status".to_string(), OnionObject::Integer(status as i64).stabilize());
                        result.insert("success".to_string(), OnionObject::Boolean(output.status.success()).stabilize());

                        Ok(build_named_dict(result))
                    }
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to execute command: {}", e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Command must be a string".to_string(),
            )),
        })
    })
}

/// 执行命令并返回退出码
fn system_code(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let command = get_attr_direct(data, "command".to_string())?;
        command.weak().with_data(|command_data| match command_data {
            OnionObject::String(cmd_str) => {
                let (shell, flag) = if cfg!(target_os = "windows") {
                    ("cmd", "/C")
                } else {
                    ("sh", "-c")
                };

                match Command::new(shell).arg(flag).arg(cmd_str.as_ref()).status() {
                    Ok(status) => {
                        let code = status.code().unwrap_or(-1);
                        Ok(OnionObject::Integer(code as i64).stabilize())
                    }
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to execute command: {}", e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Command must be a string".to_string(),
            )),
        })
    })
}

/// 改变当前工作目录
fn chdir(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                match env::set_current_dir(path_str.as_ref()) {
                    Ok(_) => Ok(OnionObject::Null.stabilize()),
                    Err(e) => Err(RuntimeError::DetailedError(format!(
                        "Failed to change directory: {}", e
                    ))),
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 获取当前用户名
fn username(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    match env::var("USERNAME").or_else(|_| env::var("USER")) {
        Ok(user) => Ok(OnionObject::String(user.into()).stabilize()),
        Err(_) => Ok(OnionObject::String("unknown".to_string().into()).stabilize()),
    }
}

/// 获取主目录路径
fn home_dir(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    match dirs::home_dir() {
        Some(path) => Ok(OnionObject::String(path.to_string_lossy().to_string().into()).stabilize()),
        None => Err(RuntimeError::DetailedError(
            "Could not determine home directory".to_string(),
        )),
    }
}

/// 获取临时目录路径
fn temp_dir(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let temp_path = env::temp_dir();
    Ok(OnionObject::String(temp_path.to_string_lossy().to_string().into()).stabilize())
}

/// 检查文件或目录是否存在
fn path_exists(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                let exists = std::path::Path::new(path_str.as_ref()).exists();
                Ok(OnionObject::Boolean(exists).stabilize())
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 检查路径是否是目录
fn is_dir(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                let is_directory = std::path::Path::new(path_str.as_ref()).is_dir();
                Ok(OnionObject::Boolean(is_directory).stabilize())
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 检查路径是否是文件
fn is_file(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let path = get_attr_direct(data, "path".to_string())?;
        path.weak().with_data(|path_data| match path_data {
            OnionObject::String(path_str) => {
                let is_file = std::path::Path::new(path_str.as_ref()).is_file();
                Ok(OnionObject::Boolean(is_file).stabilize())
            }
            _ => Err(RuntimeError::InvalidType(
                "Path must be a string".to_string(),
            )),
        })
    })
}

/// 连接路径
fn path_join(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let base = get_attr_direct(data, "base".to_string())?;
        let path = get_attr_direct(data, "path".to_string())?;
        
        base.weak().with_data(|base_data| {
            path.weak().with_data(|path_data| {
                match (base_data, path_data) {
                    (OnionObject::String(base_str), OnionObject::String(path_str)) => {
                        let joined = std::path::Path::new(base_str.as_ref()).join(path_str.as_ref());
                        Ok(OnionObject::String(joined.to_string_lossy().to_string().into()).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Base and path must be strings".to_string(),
                    )),
                }
            })
        })
    })
}

/// 构建操作系统模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // system 函数 - 执行系统命令
    let mut system_params = IndexMap::new();
    system_params.insert(
        "command".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "system".to_string(),
        wrap_native_function(
            &build_named_dict(system_params),
            None,
            None,
            "os::system".to_string(),
            &system,
        ),
    );

    // system_code 函数 - 执行命令并返回退出码
    let mut system_code_params = IndexMap::new();
    system_code_params.insert(
        "command".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "system_code".to_string(),
        wrap_native_function(
            &build_named_dict(system_code_params),
            None,
            None,
            "os::system_code".to_string(),
            &system_code,
        ),
    );

    // chdir 函数 - 改变当前工作目录
    let mut chdir_params = IndexMap::new();
    chdir_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "chdir".to_string(),
        wrap_native_function(
            &build_named_dict(chdir_params),
            None,
            None,
            "os::chdir".to_string(),
            &chdir,
        ),
    );

    // username 函数 - 获取当前用户名
    module.insert(
        "username".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "os::username".to_string(),
            &username,
        ),
    );

    // home_dir 函数 - 获取主目录
    module.insert(
        "home_dir".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "os::home_dir".to_string(),
            &home_dir,
        ),
    );

    // temp_dir 函数 - 获取临时目录
    module.insert(
        "temp_dir".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "os::temp_dir".to_string(),
            &temp_dir,
        ),
    );

    // path_exists 函数 - 检查路径是否存在
    let mut path_exists_params = IndexMap::new();
    path_exists_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "path_exists".to_string(),
        wrap_native_function(
            &build_named_dict(path_exists_params),
            None,
            None,
            "os::path_exists".to_string(),
            &path_exists,
        ),
    );

    // is_dir 函数 - 检查是否是目录
    let mut is_dir_params = IndexMap::new();
    is_dir_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "is_dir".to_string(),
        wrap_native_function(
            &build_named_dict(is_dir_params),
            None,
            None,
            "os::is_dir".to_string(),
            &is_dir,
        ),
    );

    // is_file 函数 - 检查是否是文件
    let mut is_file_params = IndexMap::new();
    is_file_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "is_file".to_string(),
        wrap_native_function(
            &build_named_dict(is_file_params),
            None,
            None,
            "os::is_file".to_string(),
            &is_file,
        ),
    );

    // path_join 函数 - 连接路径
    let mut path_join_params = IndexMap::new();
    path_join_params.insert(
        "base".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    path_join_params.insert(
        "path".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "path_join".to_string(),
        wrap_native_function(
            &build_named_dict(path_join_params),
            None,
            None,
            "os::path_join".to_string(),
            &path_join,
        ),
    );

    build_named_dict(module)
}
