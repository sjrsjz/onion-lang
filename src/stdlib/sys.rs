use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        pair::OnionPair,
        tuple::OnionTuple,
    },
};
use rustc_hash::FxHashMap;
use std::env;

use super::{build_dict, get_attr_direct, wrap_native_function};

/// 获取系统命令行参数
fn argv(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let args: Vec<_> = env::args()
        .map(|arg| OnionObject::String(arg.into()))
        .collect();

    Ok(OnionObject::Tuple(OnionTuple::new(args).into()).stabilize())
}

/// 获取环境变量
fn getenv(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let key = get_attr_direct(data, "key".to_string())?;
        key.weak().with_data(|key_data| match key_data {
            OnionObject::String(key_str) => match env::var(key_str.as_ref()) {
                Ok(value) => Ok(OnionObject::String(value.into()).stabilize()),
                Err(_) => Ok(OnionObject::Null.stabilize()),
            },
            _ => Err(RuntimeError::InvalidType(
                "Key must be a string".to_string().into(),
            )),
        })
    })
}

/// 设置环境变量
fn setenv(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let key = get_attr_direct(data, "key".to_string())?;
        let value = get_attr_direct(data, "value".to_string())?;

        key.weak().with_data(|key_data| {
            value
                .weak()
                .with_data(|value_data| match (key_data, value_data) {
                    (OnionObject::String(key_str), OnionObject::String(value_str)) => {
                        unsafe {
                            env::set_var(key_str.as_ref(), value_str.as_ref());
                        }
                        Ok(OnionObject::Null.stabilize())
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Key and value must be strings".to_string().into(),
                    )),
                })
        })
    })
}

/// 删除环境变量
fn unsetenv(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let key = get_attr_direct(data, "key".to_string())?;
        key.weak().with_data(|key_data| match key_data {
            OnionObject::String(key_str) => {
                unsafe {
                    env::remove_var(key_str.as_ref());
                }
                Ok(OnionObject::Null.stabilize())
            }
            _ => Err(RuntimeError::InvalidType(
                "Key must be a string".to_string().into(),
            )),
        })
    })
}

/// 获取所有环境变量
fn environ(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let env_vars: Vec<_> = env::vars()
        .map(|(key, value)| {
            let key_obj = OnionObject::String(key.into());
            let value_obj = OnionObject::String(value.into());
            OnionObject::Pair(OnionPair::new(key_obj, value_obj).into())
        })
        .collect();

    Ok(OnionObject::Tuple(OnionTuple::new(env_vars).into()).stabilize())
}

/// 获取当前工作目录
fn getcwd(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    match env::current_dir() {
        Ok(path) => Ok(OnionObject::String(path.to_string_lossy().to_string().into()).stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to get current directory: {}", e).into(),
        )),
    }
}

/// 退出程序
fn exit(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let exit_code = argument.weak().with_data(|data| {
        get_attr_direct(data, "code".to_string())?
            .weak()
            .with_data(|code_data| match code_data {
                OnionObject::Integer(code) => Ok(*code as i32),
                _ => Err(RuntimeError::InvalidType(
                    "Exit code must be an integer".to_string().into(),
                )),
            })
    })?;

    std::process::exit(exit_code);
}

/// 获取系统平台信息
fn platform(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let platform = if cfg!(target_os = "windows") {
        "windows"
    } else if cfg!(target_os = "macos") {
        "macos"
    } else if cfg!(target_os = "linux") {
        "linux"
    } else {
        "unknown"
    };

    Ok(OnionObject::String(platform.to_string().into()).stabilize())
}

/// 获取系统架构信息
fn arch(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let arch = if cfg!(target_arch = "x86_64") {
        "x86_64"
    } else if cfg!(target_arch = "x86") {
        "x86"
    } else if cfg!(target_arch = "aarch64") {
        "aarch64"
    } else if cfg!(target_arch = "arm") {
        "arm"
    } else {
        "unknown"
    };

    Ok(OnionObject::String(arch.to_string().into()).stabilize())
}

/// 获取程序执行路径
fn executable(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    match env::current_exe() {
        Ok(path) => Ok(OnionObject::String(path.to_string_lossy().to_string().into()).stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to get executable path: {}", e).into(),
        )),
    }
}

/// 构建系统模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // 统一参数定义
    let key_arg = &OnionObject::String("key".to_string().into()).stabilize();
    let setenv_args = &["key", "value"];
    let exit_arg = &OnionObject::String("code".to_string().into()).stabilize();

    module.insert(
        "argv".to_string(),
        wrap_native_function(
            &build_dict(IndexMap::new()),
            &FxHashMap::default(),
            "sys::argv".to_string(),
            &argv,
        ),
    );
    module.insert(
        "getenv".to_string(),
        wrap_native_function(
            key_arg,
            &FxHashMap::default(),
            "sys::getenv".to_string(),
            &getenv,
        ),
    );
    module.insert(
        "setenv".to_string(),
        wrap_native_function(
            &super::build_string_tuple(setenv_args),
            &FxHashMap::default(),
            "sys::setenv".to_string(),
            &setenv,
        ),
    );
    module.insert(
        "unsetenv".to_string(),
        wrap_native_function(
            key_arg,
            &FxHashMap::default(),
            "sys::unsetenv".to_string(),
            &unsetenv,
        ),
    );
    module.insert(
        "environ".to_string(),
        wrap_native_function(
            &build_dict(IndexMap::new()),
            &FxHashMap::default(),
            "sys::environ".to_string(),
            &environ,
        ),
    );
    module.insert(
        "getcwd".to_string(),
        wrap_native_function(
            &build_dict(IndexMap::new()),
            &FxHashMap::default(),
            "sys::getcwd".to_string(),
            &getcwd,
        ),
    );
    module.insert(
        "exit".to_string(),
        wrap_native_function(
            exit_arg,
            &FxHashMap::default(),
            "sys::exit".to_string(),
            &exit,
        ),
    );
    module.insert(
        "platform".to_string(),
        wrap_native_function(
            &build_dict(IndexMap::new()),
            &FxHashMap::default(),
            "sys::platform".to_string(),
            &platform,
        ),
    );
    module.insert(
        "arch".to_string(),
        wrap_native_function(
            &build_dict(IndexMap::new()),
            &FxHashMap::default(),
            "sys::arch".to_string(),
            &arch,
        ),
    );
    module.insert(
        "executable".to_string(),
        wrap_native_function(
            &build_dict(IndexMap::new()),
            &FxHashMap::default(),
            "sys::executable".to_string(),
            &executable,
        ),
    );

    build_dict(module)
}
