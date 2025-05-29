use std::{
    cell::RefCell,
    collections::HashMap,
    sync::Arc,
    thread,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use onion_vm::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    onion_tuple,
    types::{
        lambda::definition::{LambdaBody, OnionLambdaDefinition},
        object::{ObjectError, OnionObject, OnionStaticObject},
        tuple::OnionTuple,
    },
    GC,
};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

/// 获取当前时间戳（秒）
fn timestamp(
    _argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(duration) => Ok(OnionObject::Integer(duration.as_secs() as i64).stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(format!(
            "Failed to get timestamp: {}",
            e
        ))),
    }
}

/// 获取当前时间戳（毫秒）
fn timestamp_millis(
    _argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(duration) => Ok(OnionObject::Integer(duration.as_millis() as i64).stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(format!(
            "Failed to get timestamp: {}",
            e
        ))),
    }
}

/// 获取当前时间戳（纳秒）
fn timestamp_nanos(
    _argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(duration) => Ok(OnionObject::Integer(duration.as_nanos() as i64).stabilize()),
        Err(e) => Err(RuntimeError::DetailedError(format!(
            "Failed to get timestamp: {}",
            e
        ))),
    }
}

/// 睡眠指定的秒数
fn sleep_seconds(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    let seconds = argument
        .weak()
        .with_data(|data| {
            get_attr_direct(data, "seconds".to_string())?
                .weak()
                .to_integer()
                .map_err(|e| ObjectError::InvalidType(format!("Invalid seconds: {}", e)))
        })
        .map_err(RuntimeError::ObjectError)?;

    if seconds < 0 {
        return Err(RuntimeError::DetailedError(
            "Sleep duration cannot be negative".to_string(),
        ));
    }

    thread::sleep(Duration::from_secs(seconds as u64));
    Ok(OnionObject::Null.stabilize())
}

/// 睡眠指定的毫秒数
fn sleep_millis(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    let millis = argument
        .weak()
        .with_data(|data| {
            get_attr_direct(data, "millis".to_string())?
                .weak()
                .to_integer()
                .map_err(|e| ObjectError::InvalidType(format!("Invalid milliseconds: {}", e)))
        })
        .map_err(RuntimeError::ObjectError)?;

    if millis < 0 {
        return Err(RuntimeError::DetailedError(
            "Sleep duration cannot be negative".to_string(),
        ));
    }

    thread::sleep(Duration::from_millis(millis as u64));
    Ok(OnionObject::Null.stabilize())
}

/// 睡眠指定的微秒数
fn sleep_micros(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    let micros = argument
        .weak()
        .with_data(|data| {
            get_attr_direct(data, "micros".to_string())?
                .weak()
                .to_integer()
                .map_err(|e| ObjectError::InvalidType(format!("Invalid microseconds: {}", e)))
        })
        .map_err(RuntimeError::ObjectError)?;

    if micros < 0 {
        return Err(RuntimeError::DetailedError(
            "Sleep duration cannot be negative".to_string(),
        ));
    }

    thread::sleep(Duration::from_micros(micros as u64));
    Ok(OnionObject::Null.stabilize())
}

/// 获取格式化的当前时间字符串（UTC）
fn now_utc(
    _argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(duration) => {
            let secs = duration.as_secs();
            let datetime = format_timestamp(secs);
            Ok(OnionObject::String(datetime).stabilize())
        }
        Err(e) => Err(RuntimeError::DetailedError(format!(
            "Failed to get current time: {}",
            e
        ))),
    }
}

/// 将时间戳转换为日期时间字符串（简单实现）
fn format_timestamp(timestamp: u64) -> String {
    // 简单的时间戳转换实现
    const SECONDS_PER_DAY: u64 = 86400;
    const SECONDS_PER_HOUR: u64 = 3600;
    const SECONDS_PER_MINUTE: u64 = 60;

    // 1970年1月1日是星期四
    let days_since_epoch = timestamp / SECONDS_PER_DAY;

    // 简化的年月日计算（不考虑闰年等复杂情况）
    let year = 1970 + (days_since_epoch / 365);
    let day_of_year = days_since_epoch % 365;
    let month = (day_of_year / 30) + 1;
    let day = (day_of_year % 30) + 1;

    let remaining_seconds = timestamp % SECONDS_PER_DAY;
    let hour = remaining_seconds / SECONDS_PER_HOUR;
    let minute = (remaining_seconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE;
    let second = remaining_seconds % SECONDS_PER_MINUTE;

    format!(
        "{:04}-{:02}-{:02} {:02}:{:02}:{:02} UTC",
        year, month, day, hour, minute, second
    )
}

/// 从时间戳格式化时间字符串
fn format_time(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    let timestamp = argument
        .weak()
        .with_data(|data| {
            get_attr_direct(data, "timestamp".to_string())?
                .weak()
                .to_integer()
                .map_err(|e| ObjectError::InvalidType(format!("Invalid timestamp: {}", e)))
        })
        .map_err(RuntimeError::ObjectError)?;

    if timestamp < 0 {
        return Err(RuntimeError::DetailedError(
            "Timestamp cannot be negative".to_string(),
        ));
    }

    let datetime = format_timestamp(timestamp as u64);
    Ok(OnionObject::String(datetime).stabilize())
}

/// 计算两个时间戳之间的差值（秒）
fn time_diff(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    let (start, end) = argument
        .weak()
        .with_data(|data| {
            let start = get_attr_direct(data, "start".to_string())?
                .weak()
                .to_integer()
                .map_err(|e| ObjectError::InvalidType(format!("Invalid start timestamp: {}", e)))?;

            let end = get_attr_direct(data, "end".to_string())?
                .weak()
                .to_integer()
                .map_err(|e| ObjectError::InvalidType(format!("Invalid end timestamp: {}", e)))?;

            Ok((start, end))
        })
        .map_err(RuntimeError::ObjectError)?;

    let diff = end - start;
    Ok(OnionObject::Integer(diff).stabilize())
}

#[derive(Clone)]
pub struct AsyncSleep {
    pub(crate) millis: i64,
    pub(crate) start_time: SystemTime,
}

impl AsyncSleep {
    pub fn get() -> OnionStaticObject {
        let params =
            HashMap::from([("millis".to_string(), OnionObject::Integer(1000).stabilize())]);
        let params = build_named_dict(params);
        OnionLambdaDefinition::new_static(
            &params,
            LambdaBody::NativeFunction(Arc::new(RefCell::new(AsyncSleep::default()))),
            None,
            None,
            "time::AsyncSleep".to_string(),
        )
    }
}

impl Default for AsyncSleep {
    fn default() -> Self {
        AsyncSleep {
            millis: 1000,
            start_time: SystemTime::now(),
        }
    }
}

impl Runnable for AsyncSleep {
    fn set_argument(
        &mut self,
        argument: OnionStaticObject,
        _gc: &mut GC<OnionObject>,
    ) -> Result<(), ObjectError> {
        self.millis = argument.weak().with_data(|data| {
            get_attr_direct(data, "millis".to_string())?
                .weak()
                .to_integer()
                .map_err(|e| ObjectError::InvalidType(format!("Invalid millis: {}", e)))
        })?;
        self.start_time = SystemTime::now();
        Ok(())
    }

    fn step(&mut self, _gc: &mut GC<OnionObject>) -> Result<StepResult, RuntimeError> {
        let elapsed = self.start_time.elapsed().map_err(|e| {
            RuntimeError::DetailedError(format!("Failed to get elapsed time: {}", e))
        })?;
        if elapsed.as_millis() >= self.millis as u128 {
            Ok(StepResult::Return(OnionObject::Null.stabilize()))
        } else {
            Ok(StepResult::Continue)
        }
    }

    fn receive(
        &mut self,
        _step_result: StepResult,
        _gc: &mut GC<OnionObject>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "receive not implemented".to_string(),
        ))
    }

    fn copy(&self, _gc: &mut onion_vm::GC<OnionObject>) -> Box<dyn Runnable> {
        Box::new(AsyncSleep {
            millis: self.millis,
            start_time: self.start_time,
        })
    }
}

/// 构建时间模块
pub fn build_module() -> OnionStaticObject {
    let mut module = HashMap::new(); // timestamp 函数 - 获取当前时间戳（秒）
    module.insert(
        "timestamp".to_string(),
        wrap_native_function(
            &onion_tuple!(),
            None,
            None,
            "time::timestamp".to_string(),
            timestamp,
        ),
    );

    // timestamp_millis 函数 - 获取当前时间戳（毫秒）
    module.insert(
        "timestamp_millis".to_string(),
        wrap_native_function(
            &onion_tuple!(),
            None,
            None,
            "time::timestamp_millis".to_string(),
            timestamp_millis,
        ),
    );

    // timestamp_nanos 函数 - 获取当前时间戳（纳秒）
    module.insert(
        "timestamp_nanos".to_string(),
        wrap_native_function(
            &onion_tuple!(),
            None,
            None,
            "time::timestamp_nanos".to_string(),
            timestamp_nanos,
        ),
    );

    // sleep_seconds 函数 - 睡眠秒数
    let mut sleep_seconds_params = HashMap::new();
    sleep_seconds_params.insert("seconds".to_string(), OnionObject::Integer(1).stabilize());
    module.insert(
        "sleep_seconds".to_string(),
        wrap_native_function(
            &build_named_dict(sleep_seconds_params),
            None,
            None,
            "time::sleep_seconds".to_string(),
            sleep_seconds,
        ),
    );

    // sleep_millis 函数 - 睡眠毫秒数
    let mut sleep_millis_params = HashMap::new();
    sleep_millis_params.insert("millis".to_string(), OnionObject::Integer(1000).stabilize());
    module.insert(
        "sleep_millis".to_string(),
        wrap_native_function(
            &build_named_dict(sleep_millis_params),
            None,
            None,
            "time::sleep_millis".to_string(),
            sleep_millis,
        ),
    );

    // sleep_micros 函数 - 睡眠微秒数
    let mut sleep_micros_params = HashMap::new();
    sleep_micros_params.insert(
        "micros".to_string(),
        OnionObject::Integer(1000000).stabilize(),
    );
    module.insert(
        "sleep_micros".to_string(),
        wrap_native_function(
            &build_named_dict(sleep_micros_params),
            None,
            None,
            "time::sleep_micros".to_string(),
            sleep_micros,
        ),
    ); // now_utc 函数 - 获取格式化的当前时间
    module.insert(
        "now_utc".to_string(),
        wrap_native_function(
            &onion_tuple!(),
            None,
            None,
            "time::now_utc".to_string(),
            now_utc,
        ),
    );

    // format_time 函数 - 格式化时间戳
    let mut format_time_params = HashMap::new();
    format_time_params.insert("timestamp".to_string(), OnionObject::Integer(0).stabilize());
    module.insert(
        "format_time".to_string(),
        wrap_native_function(
            &build_named_dict(format_time_params),
            None,
            None,
            "time::format_time".to_string(),
            format_time,
        ),
    );

    // time_diff 函数 - 计算时间差
    let mut time_diff_params = HashMap::new();
    time_diff_params.insert("start".to_string(), OnionObject::Integer(0).stabilize());
    time_diff_params.insert("end".to_string(), OnionObject::Integer(0).stabilize());
    module.insert(
        "time_diff".to_string(),
        wrap_native_function(
            &build_named_dict(time_diff_params),
            None,
            None,
            "time::time_diff".to_string(),
            time_diff,
        ),
    );

    module.insert(
        "async_sleep".to_string(),
        AsyncSleep::get(),
    );

    build_named_dict(module)
}
