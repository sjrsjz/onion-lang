use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        pair::OnionPair,
        tuple::OnionTuple,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};
use std::env;

// 引入所需的辅助函数
use super::{build_dict, wrap_native_function};

// 辅助函数，用于获取并验证字符串参数
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
        OnionObject::String(s) => Ok(s.as_ref()),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be a string")
                .to_string()
                .into(),
        )),
    }
}

// 辅助函数，用于获取并验证整数参数
fn get_integer_arg(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<i64, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires an '{name}' argument")
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::Integer(i) => Ok(*i),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be an integer")
                .to_string()
                .into(),
        )),
    }
}

/// 获取系统命令行参数
fn argv(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let args: Vec<_> = env::args()
        .map(|arg| OnionObject::String(arg.into()))
        .collect();
    Ok(OnionObject::Tuple(OnionTuple::new(args).into()).stabilize())
}

/// 获取环境变量
fn getenv(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
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
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let key_str = get_string_arg(argument, "key")?;
    let value_str = get_string_arg(argument, "value")?;
    unsafe { env::set_var(key_str, value_str) }
    Ok(OnionObject::Null.stabilize())
}

/// 删除环境变量
fn unsetenv(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let key_str = get_string_arg(argument, "key")?;
    unsafe { env::remove_var(key_str) }
    Ok(OnionObject::Null.stabilize())
}

/// 获取所有环境变量
fn environ(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
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
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    match env::current_dir() {
        Ok(path) => Ok(OnionObject::String(path.to_string_lossy().into()).stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to get current directory: {e}").into(),
        )),
    }
}

/// 退出程序
fn exit(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let exit_code = get_integer_arg(argument, "code")?;
    std::process::exit(exit_code as i32);
}

/// 获取系统平台信息
fn platform(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
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
    Ok(OnionObject::String(platform.into()).stabilize())
}

/// 获取系统架构信息
fn arch(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
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
    Ok(OnionObject::String(arch.into()).stabilize())
}

/// 获取程序执行路径
fn executable(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    match env::current_exe() {
        Ok(path) => Ok(OnionObject::String(path.to_string_lossy().into()).stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to get executable path: {e}").into(),
        )),
    }
}

/// 构建系统模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // sys.argv()
    module.insert(
        "argv".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "sys::argv",
            OnionKeyPool::create(vec![]),
            &argv,
        ),
    );

    // sys.getenv(key)
    module.insert(
        "getenv".to_string(),
        wrap_native_function(
            LambdaParameter::top("key"),
            OnionFastMap::default(),
            "sys::getenv",
            OnionKeyPool::create(vec!["key".into()]),
            &getenv,
        ),
    );

    // sys.setenv(key, value)
    module.insert(
        "setenv".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("key"), LambdaParameter::top("value")].into(),
            ),
            OnionFastMap::default(),
            "sys::setenv",
            OnionKeyPool::create(vec!["key".into(), "value".into()]),
            &setenv,
        ),
    );

    // sys.unsetenv(key)
    module.insert(
        "unsetenv".to_string(),
        wrap_native_function(
            LambdaParameter::top("key"),
            OnionFastMap::default(),
            "sys::unsetenv",
            OnionKeyPool::create(vec!["key".into()]),
            &unsetenv,
        ),
    );

    // sys.environ()
    module.insert(
        "environ".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "sys::environ",
            OnionKeyPool::create(vec![]),
            &environ,
        ),
    );

    // sys.getcwd()
    module.insert(
        "getcwd".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "sys::getcwd",
            OnionKeyPool::create(vec![]),
            &getcwd,
        ),
    );

    // sys.exit(code)
    module.insert(
        "exit".to_string(),
        wrap_native_function(
            LambdaParameter::top("code"),
            OnionFastMap::default(),
            "sys::exit",
            OnionKeyPool::create(vec!["code".into()]),
            &exit,
        ),
    );

    // sys.platform()
    module.insert(
        "platform".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "sys::platform",
            OnionKeyPool::create(vec![]),
            &platform,
        ),
    );

    // sys.arch()
    module.insert(
        "arch".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "sys::arch",
            OnionKeyPool::create(vec![]),
            &arch,
        ),
    );

    // sys.executable()
    module.insert(
        "executable".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "sys::executable",
            OnionKeyPool::create(vec![]),
            &executable,
        ),
    );

    build_dict(module)
}
