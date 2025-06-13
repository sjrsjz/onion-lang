use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
    GC,
};
use std::env;

use super::{build_named_dict, get_attr_direct, wrap_native_function};

/// 获取系统命令行参数
fn argv(
    _argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let args: Vec<_> = env::args()
        .map(|arg| OnionObject::String(arg.into()))
        .collect();

    Ok(OnionObject::Tuple(onion_vm::types::tuple::OnionTuple::new(args)).stabilize())
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
                        env::set_var(key_str.as_ref(), value_str.as_ref());
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
                env::remove_var(key_str.as_ref());
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
            OnionObject::Pair(onion_vm::types::pair::OnionPair::new(key_obj, value_obj))
        })
        .collect();

    Ok(OnionObject::Tuple(onion_vm::types::tuple::OnionTuple::new(env_vars)).stabilize())
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

    // argv 函数 - 获取命令行参数
    module.insert(
        "argv".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "sys::argv".to_string(),
            &argv,
        ),
    );

    // getenv 函数 - 获取环境变量
    let mut getenv_params = IndexMap::new();
    getenv_params.insert(
        "key".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "getenv".to_string(),
        wrap_native_function(
            &build_named_dict(getenv_params),
            None,
            None,
            "sys::getenv".to_string(),
            &getenv,
        ),
    );

    // setenv 函数 - 设置环境变量
    let mut setenv_params = IndexMap::new();
    setenv_params.insert(
        "key".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    setenv_params.insert(
        "value".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "setenv".to_string(),
        wrap_native_function(
            &build_named_dict(setenv_params),
            None,
            None,
            "sys::setenv".to_string(),
            &setenv,
        ),
    );

    // unsetenv 函数 - 删除环境变量
    let mut unsetenv_params = IndexMap::new();
    unsetenv_params.insert(
        "key".to_string(),
        OnionObject::String("".to_string().into()).stabilize(),
    );
    module.insert(
        "unsetenv".to_string(),
        wrap_native_function(
            &build_named_dict(unsetenv_params),
            None,
            None,
            "sys::unsetenv".to_string(),
            &unsetenv,
        ),
    );

    // environ 函数 - 获取所有环境变量
    module.insert(
        "environ".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "sys::environ".to_string(),
            &environ,
        ),
    );

    // getcwd 函数 - 获取当前工作目录
    module.insert(
        "getcwd".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "sys::getcwd".to_string(),
            &getcwd,
        ),
    );

    // exit 函数 - 退出程序
    let mut exit_params = IndexMap::new();
    exit_params.insert("code".to_string(), OnionObject::Integer(0).stabilize());
    module.insert(
        "exit".to_string(),
        wrap_native_function(
            &build_named_dict(exit_params),
            None,
            None,
            "sys::exit".to_string(),
            &exit,
        ),
    );

    // platform 函数 - 获取系统平台
    module.insert(
        "platform".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "sys::platform".to_string(),
            &platform,
        ),
    );

    // arch 函数 - 获取系统架构
    module.insert(
        "arch".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "sys::arch".to_string(),
            &arch,
        ),
    );

    // executable 函数 - 获取程序执行路径
    module.insert(
        "executable".to_string(),
        wrap_native_function(
            &build_named_dict(IndexMap::new()),
            None,
            None,
            "sys::executable".to_string(),
            &executable,
        ),
    );

    build_named_dict(module)
}
