use std::{
    sync::Arc,
    thread,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinition},
            parameter::LambdaParameter,
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    unwrap_step_result,
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

// 引入所需的辅助函数
use super::{build_dict, wrap_native_function};

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

/// 获取当前时间戳（秒）
fn timestamp(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| OnionObject::Integer(d.as_secs() as i64).stabilize())
        .map_err(|e| RuntimeError::DetailedError(format!("Failed to get timestamp: {e}").into()))
}

/// 获取当前时间戳（毫秒）
fn timestamp_millis(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| OnionObject::Integer(d.as_millis() as i64).stabilize())
        .map_err(|e| RuntimeError::DetailedError(format!("Failed to get timestamp: {e}").into()))
}

/// 获取当前时间戳（纳秒）
fn timestamp_nanos(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| OnionObject::Integer(d.as_nanos() as i64).stabilize()) // Note: might overflow on 32-bit systems in the future
        .map_err(|e| RuntimeError::DetailedError(format!("Failed to get timestamp: {e}").into()))
}

/// 睡眠指定的秒数
fn sleep_seconds(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let seconds = get_integer_arg(argument, "seconds")?;
    if seconds < 0 {
        return Err(RuntimeError::DetailedError(
            "Sleep duration cannot be negative".into(),
        ));
    }
    thread::sleep(Duration::from_secs(seconds as u64));
    Ok(OnionObject::Null.stabilize())
}

/// 睡眠指定的毫秒数
fn sleep_millis(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let millis = get_integer_arg(argument, "millis")?;
    if millis < 0 {
        return Err(RuntimeError::DetailedError(
            "Sleep duration cannot be negative".into(),
        ));
    }
    thread::sleep(Duration::from_millis(millis as u64));
    Ok(OnionObject::Null.stabilize())
}

/// 睡眠指定的微秒数
fn sleep_micros(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let micros = get_integer_arg(argument, "micros")?;
    if micros < 0 {
        return Err(RuntimeError::DetailedError(
            "Sleep duration cannot be negative".into(),
        ));
    }
    thread::sleep(Duration::from_micros(micros as u64));
    Ok(OnionObject::Null.stabilize())
}

/// 将时间戳转换为日期时间字符串（简单实现）
fn format_timestamp(timestamp: u64) -> String {
    const SECS_PER_DAY: u64 = 86400;
    let days = timestamp / SECS_PER_DAY;
    let rem_secs = timestamp % SECS_PER_DAY;
    let year = 1970 + days / 365; // Simplified calculation
    let rem_days = days % 365;
    let month = rem_days / 30 + 1; // Simplified calculation
    let day = rem_days % 30 + 1; // Simplified calculation
    let hour = rem_secs / 3600;
    let minute = (rem_secs % 3600) / 60;
    let second = rem_secs % 60;
    format!("{year:04}-{month:02}-{day:02} {hour:02}:{minute:02}:{second:02} UTC")
}

/// 获取格式化的当前时间字符串（UTC）
fn now_utc(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let duration = SystemTime::now().duration_since(UNIX_EPOCH).map_err(|e| {
        RuntimeError::DetailedError(format!("Failed to get current time: {e}").into())
    })?;
    let datetime = format_timestamp(duration.as_secs());
    Ok(OnionObject::String(datetime.into()).stabilize())
}

/// 从时间戳格式化时间字符串
fn format_time(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let timestamp = get_integer_arg(argument, "timestamp")?;
    if timestamp < 0 {
        return Err(RuntimeError::DetailedError(
            "Timestamp cannot be negative".into(),
        ));
    }
    let datetime = format_timestamp(timestamp as u64);
    Ok(OnionObject::String(datetime.into()).stabilize())
}

/// 计算两个时间戳之间的差值（秒）
fn time_diff(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let start = get_integer_arg(argument, "start")?;
    let end = get_integer_arg(argument, "end")?;
    Ok(OnionObject::Integer(end - start).stabilize())
}

#[derive(Clone)]
pub struct AsyncSleep {
    pub(crate) millis: i64,
    pub(crate) start_time: SystemTime,
}
impl Runnable for AsyncSleep {
    fn step(&mut self, _gc: &mut GC<OnionObjectCell>) -> StepResult {
        let elapsed = unwrap_step_result!(self.start_time.elapsed().map_err(|e| {
            RuntimeError::DetailedError(format!("Failed to get elapsed time: {e}").into())
        }));
        if elapsed.as_millis() >= self.millis as u128 {
            StepResult::Return(OnionObject::Null.stabilize().into())
        } else {
            StepResult::Continue
        }
    }

    fn receive(
        &mut self,
        _step_result: &StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Ok(())
    }
    fn format_context(&self) -> String {
        let elapsed = self.start_time.elapsed().unwrap_or_default();
        let elapsed_ms = elapsed.as_millis();
        let total_duration_ms = self.millis as u128;
        let remaining_ms = total_duration_ms.saturating_sub(elapsed_ms);
        format!(
            "-> Pausing execution (sleep): {elapsed_ms}ms / {total_duration_ms}ms (~{remaining_ms}ms remaining)"
        )
    }
}

fn async_sleep(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let millis = get_integer_arg(argument, "millis")?;
    if millis < 0 {
        return Err(RuntimeError::DetailedError(
            "Sleep duration cannot be negative".into(),
        ));
    }

    Ok(OnionLambdaDefinition::new_static(
        LambdaParameter::Multiple([].into()),
        LambdaBody::NativeFunction((
            Arc::new(move |_, _, _, _| {
                Box::new(AsyncSleep {
                    millis,
                    start_time: SystemTime::now(),
                })
            }),
            OnionKeyPool::create(vec![]),
        )),
        OnionFastMap::default(),
        "time::async_sleep".into(),
        LambdaType::Atomic,
    ))
}

/// 构建时间模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // --- Functions with no arguments ---
    module.insert(
        "timestamp".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "time::timestamp",
            OnionKeyPool::create(vec![]),
            &timestamp,
        ),
    );
    module.insert(
        "timestamp_millis".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "time::timestamp_millis",
            OnionKeyPool::create(vec![]),
            &timestamp_millis,
        ),
    );
    module.insert(
        "timestamp_nanos".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "time::timestamp_nanos",
            OnionKeyPool::create(vec![]),
            &timestamp_nanos,
        ),
    );
    module.insert(
        "now_utc".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple([].into()),
            OnionFastMap::default(),
            "time::now_utc",
            OnionKeyPool::create(vec![]),
            &now_utc,
        ),
    );

    // --- Functions with one argument ---
    module.insert(
        "sleep_seconds".to_string(),
        wrap_native_function(
            LambdaParameter::top("seconds"),
            OnionFastMap::default(),
            "time::sleep_seconds",
            OnionKeyPool::create(vec!["seconds".into()]),
            &sleep_seconds,
        ),
    );

    module.insert(
        "sleep_millis".to_string(),
        wrap_native_function(
            LambdaParameter::top("millis"),
            OnionFastMap::default(),
            "time::sleep_millis",
            OnionKeyPool::create(vec!["millis".into()]),
            &sleep_millis,
        ),
    );

    module.insert(
        "sleep_micros".to_string(),
        wrap_native_function(
            LambdaParameter::top("micros"),
            OnionFastMap::default(),
            "time::sleep_micros",
            OnionKeyPool::create(vec!["micros".into()]),
            &sleep_micros,
        ),
    );

    module.insert(
        "format_time".to_string(),
        wrap_native_function(
            LambdaParameter::top("timestamp"),
            OnionFastMap::default(),
            "time::format_time",
            OnionKeyPool::create(vec!["timestamp".into()]),
            &format_time,
        ),
    );

    module.insert(
        "async_sleep".to_string(),
        wrap_native_function(
            LambdaParameter::top("millis"),
            OnionFastMap::default(),
            "time::async_sleep",
            OnionKeyPool::create(vec!["millis".into()]),
            &async_sleep,
        ),
    );

    // --- Functions with multiple arguments ---
    module.insert(
        "time_diff".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("start"), LambdaParameter::top("end")].into(),
            ),
            OnionFastMap::default(),
            "time::time_diff",
            OnionKeyPool::create(vec!["start".into(), "end".into()]),
            &time_diff,
        ),
    );

    build_dict(module)
}
