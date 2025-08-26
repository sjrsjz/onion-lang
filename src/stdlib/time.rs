use std::{
    sync::Arc,
    thread,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use chrono::DateTime;
use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        integer_value::OnionIntegerValue,
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinitionInner},
            parameter::LambdaParameter,
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        string_value::OnionStringValue,
        undefined::OnionUndefined,
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
        OnionObject::IntegerValue(i) => Ok(i.value()),
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
        .map(|d| OnionIntegerValue::new_static(d.as_secs() as i64))
        .map_err(|e| RuntimeError::DetailedError(format!("Failed to get timestamp: {e}").into()))
}

/// 获取当前时间戳（毫秒）
fn timestamp_millis(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| OnionIntegerValue::new_static(d.as_millis() as i64))
        .map_err(|e| RuntimeError::DetailedError(format!("Failed to get timestamp: {e}").into()))
}

/// 获取当前时间戳（纳秒）
fn timestamp_nanos(
    _argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| OnionIntegerValue::new_static(d.as_nanos() as i64))
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
    Ok(OnionUndefined::new_static(None))
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
    Ok(OnionUndefined::new_static(None))
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
    Ok(OnionUndefined::new_static(None))
}

/// 将时间戳转换为日期时间字符串（使用chrono精确实现）
fn format_timestamp(timestamp: u64) -> String {
    // 将timestamp转换为DateTime<Utc>
    if let Some(dt) = DateTime::from_timestamp(timestamp as i64, 0) {
        dt.format("%Y-%m-%d %H:%M:%S UTC").to_string()
    } else {
        // 如果时间戳无效，回退到错误信息
        format!("Invalid timestamp: {}", timestamp)
    }
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
    Ok(OnionStringValue::new_static(datetime))
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
    Ok(OnionStringValue::new_static(datetime))
}

/// 从时间戳格式化时间字符串（自定义格式）
fn format_time_custom(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let timestamp = get_integer_arg(argument, "timestamp")?;
    if timestamp < 0 {
        return Err(RuntimeError::DetailedError(
            "Timestamp cannot be negative".into(),
        ));
    }

    // 获取格式字符串
    let format_obj = argument.get("format").ok_or_else(|| {
        RuntimeError::DetailedError("Function requires a 'format' argument".into())
    })?;
    let format_str = match format_obj.weak() {
        OnionObject::StringValue(s) => s.value(),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Argument 'format' must be a string".into(),
            ));
        }
    };

    if let Some(dt) = DateTime::from_timestamp(timestamp, 0) {
        let formatted = dt.format(&format_str).to_string();
        Ok(OnionStringValue::new_static(formatted))
    } else {
        Err(RuntimeError::DetailedError(
            format!("Invalid timestamp: {}", timestamp).into(),
        ))
    }
}

/// 解析时间字符串为时间戳
fn parse_time(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    // 获取时间字符串
    let time_obj = argument.get("time_str").ok_or_else(|| {
        RuntimeError::DetailedError("Function requires a 'time_str' argument".into())
    })?;
    let time_str = match time_obj.weak() {
        OnionObject::StringValue(s) => s.value(),
        _ => {
            return Err(RuntimeError::InvalidType(
                "Argument 'time_str' must be a string".into(),
            ));
        }
    };

    // 获取格式字符串（可选，默认为 "%Y-%m-%d %H:%M:%S %Z"）
    let format_str = if let Some(format_obj) = argument.get("format") {
        match format_obj.weak() {
            OnionObject::StringValue(s) => s.value().to_string(),
            _ => {
                return Err(RuntimeError::InvalidType(
                    "Argument 'format' must be a string".into(),
                ));
            }
        }
    } else {
        "%Y-%m-%d %H:%M:%S %Z".to_string()
    };

    // 尝试解析时间
    match DateTime::parse_from_str(&time_str, &format_str) {
        Ok(dt) => Ok(OnionIntegerValue::new_static(dt.timestamp())),
        Err(e) => Err(RuntimeError::DetailedError(
            format!("Failed to parse time string: {}", e).into(),
        )),
    }
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
            StepResult::Return(OnionUndefined::new_static(None).into())
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

    Ok(OnionLambdaDefinitionInner::new_static(
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
        "format_time_custom".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("timestamp"),
                    LambdaParameter::top("format"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "time::format_time_custom",
            OnionKeyPool::create(vec!["timestamp".into(), "format".into()]),
            &format_time_custom,
        ),
    );

    module.insert(
        "parse_time".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("time_str"),
                    LambdaParameter::top("format"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "time::parse_time",
            OnionKeyPool::create(vec!["time_str".into(), "format".into()]),
            &parse_time,
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

    build_dict(module)
}
