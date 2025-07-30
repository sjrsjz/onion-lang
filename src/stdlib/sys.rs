use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError, onion_tuple, types::{
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        pair::OnionPair,
        tuple::OnionTuple,
    }, GC
};
use rustc_hash::FxHashMap;
use std::env;

// 引入所需的辅助函数
use super::{build_dict, build_string_tuple, wrap_native_function};

// 辅助函数，用于获取并验证字符串参数
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

// 辅助函数，用于获取并验证整数参数
fn get_integer_arg(
    argument: &FxHashMap<String, OnionStaticObject>,
    name: &str,
) -> Result<i64, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires an '{}' argument", name)
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::Integer(i) => Ok(*i),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{}' must be an integer", name)
                .to_string()
                .into(),
        )),
    }
}

/// 获取系统命令行参数
fn argv(
    _argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let args: Vec<_> = env::args()
        .map(|arg| OnionObject::String(arg.into()))
        .collect();
    Ok(OnionObject::Tuple(OnionTuple::new(args).into()).stabilize())
}

/// 获取环境变量
fn getenv(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let key_str = get_string_arg(argument, "key")?;
    match env::var(key_str) {
        Ok(value) => Ok(OnionObject::String(value.into()).stabilize()),
        Err(_) => Ok(OnionObject::Null.stabilize()),
    }
}

/// 设置环境变量
fn setenv(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let key_str = get_string_arg(argument, "key")?;
    let value_str = get_string_arg(argument, "value")?;
    unsafe { env::set_var(key_str, value_str) }
    Ok(OnionObject::Null.stabilize())
}

/// 删除环境变量
fn unsetenv(
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let key_str = get_string_arg(argument, "key")?;
    unsafe { env::remove_var(key_str) }
    Ok(OnionObject::Null.stabilize())
}

/// 获取所有环境变量
fn environ(
    _argument: &FxHashMap<String, OnionStaticObject>,
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
    _argument: &FxHashMap<String, OnionStaticObject>,
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
    argument: &FxHashMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let exit_code = get_integer_arg(argument, "code")?;
    std::process::exit(exit_code as i32);
}

/// 获取系统平台信息
fn platform(
    _argument: &FxHashMap<String, OnionStaticObject>,
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
    _argument: &FxHashMap<String, OnionStaticObject>,
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
    _argument: &FxHashMap<String, OnionStaticObject>,
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

    let no_args = onion_tuple!();
    let key_arg = OnionObject::String("key".to_string().into()).stabilize();
    let exit_arg = OnionObject::String("code".to_string().into()).stabilize();

    module.insert(
        "argv".to_string(),
        wrap_native_function(
            &no_args,
            &FxHashMap::default(),
            "sys::argv".to_string(),
            &argv,
        ),
    );
    module.insert(
        "getenv".to_string(),
        wrap_native_function(
            &key_arg,
            &FxHashMap::default(),
            "sys::getenv".to_string(),
            &getenv,
        ),
    );
    module.insert(
        "setenv".to_string(),
        wrap_native_function(
            &build_string_tuple(&["key", "value"]),
            &FxHashMap::default(),
            "sys::setenv".to_string(),
            &setenv,
        ),
    );
    module.insert(
        "unsetenv".to_string(),
        wrap_native_function(
            &key_arg,
            &FxHashMap::default(),
            "sys::unsetenv".to_string(),
            &unsetenv,
        ),
    );
    module.insert(
        "environ".to_string(),
        wrap_native_function(
            &no_args,
            &FxHashMap::default(),
            "sys::environ".to_string(),
            &environ,
        ),
    );
    module.insert(
        "getcwd".to_string(),
        wrap_native_function(
            &no_args,
            &FxHashMap::default(),
            "sys::getcwd".to_string(),
            &getcwd,
        ),
    );
    module.insert(
        "exit".to_string(),
        wrap_native_function(
            &exit_arg,
            &FxHashMap::default(),
            "sys::exit".to_string(),
            &exit,
        ),
    );
    module.insert(
        "platform".to_string(),
        wrap_native_function(
            &no_args,
            &FxHashMap::default(),
            "sys::platform".to_string(),
            &platform,
        ),
    );
    module.insert(
        "arch".to_string(),
        wrap_native_function(
            &no_args,
            &FxHashMap::default(),
            "sys::arch".to_string(),
            &arch,
        ),
    );
    module.insert(
        "executable".to_string(),
        wrap_native_function(
            &no_args,
            &FxHashMap::default(),
            "sys::executable".to_string(),
            &executable,
        ),
    );

    build_dict(module)
}
