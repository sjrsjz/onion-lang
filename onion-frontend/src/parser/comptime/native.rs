//! 原生函数包装模块：将 Rust 函数包装为 Onion VM 可调用的 Lambda。
//!
//! 本模块提供将 Rust 原生函数转换为 Onion VM Lambda 对象的机制，
//! 支持参数绑定、捕获变量、以及在 VM 中的执行。主要用于编译时或运行时
//! 向 VM 注入原生功能。

use std::sync::Arc;

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

/// 原生函数生成器：将 Rust 函数包装为 VM 可执行的 Runnable。
///
/// 封装原生 Rust 函数，使其能在 Onion VM 中作为 Lambda 执行。
/// 保存捕获的参数和函数引用，在 VM 调用时执行原生逻辑。
pub struct NativeFunctionGenerator<F>
where
    F: Fn(
        &OnionFastMap<Box<str>, OnionStaticObject>,
        &mut GC<OnionObjectCell>,
    ) -> Result<OnionStaticObject, RuntimeError>,
{
    captured: OnionFastMap<Box<str>, OnionStaticObject>,
    function: Arc<F>,
}

impl<F> Runnable for NativeFunctionGenerator<F>
where
    F: Fn(
            &OnionFastMap<Box<str>, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + Send
        + Sync
        + 'static,
{
    /// 执行原生函数，返回计算结果。
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        unwrap_step_result!(
            (self.function)(&self.captured, gc).map(|result| StepResult::Return(result.into()))
        )
    }

    /// 格式化执行上下文信息，用于调试和错误报告。
    fn format_context(&self) -> String {
        // Use the type name of the function/closure to identify the native code.
        let full_type_name = std::any::type_name_of_val(&self.function);

        // Provide a shorter, more readable version of the type name.
        let short_type_name = full_type_name.split("::").last().unwrap_or(full_type_name);

        // Assemble all the information into a clear, structured block.
        format!(
            "-> Executing Native Function:\n   - Function: {} (Full Type: {})\n   - Argument: {:?}",
            short_type_name,
            full_type_name, // Include full name for disambiguation
            self.captured,
        )
    }
}

/// 将 Rust 原生函数包装为 Onion VM 可调用的 Lambda 定义。
///
/// # 参数
/// - `params`：Lambda 参数定义
/// - `capture`：捕获的变量映射
/// - `signature`：函数签名字符串
/// - `string_pool`：字符串池
/// - `function`：要包装的原生函数
///
/// # 返回
/// 包装后的 Lambda 定义静态对象，可在 VM 中调用。
pub fn wrap_native_function<F>(
    params: LambdaParameter,
    capture: OnionFastMap<Box<str>, OnionObject>,
    signature: &str,
    string_pool: OnionKeyPool<Box<str>>,
    function: Arc<F>,
) -> OnionStaticObject
where
    F: Fn(
            &OnionFastMap<Box<str>, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + Send
        + Sync
        + 'static,
{
    let cloned_pool = string_pool.clone();
    OnionLambdaDefinition::new_static(
        params,
        LambdaBody::NativeFunction((
            Arc::new(
                move |_,
                      argument: &OnionFastMap<Box<str>, OnionStaticObject>,
                      capture: &OnionFastMap<Box<str>, OnionObject>,
                      _| {
                    let mut captured = argument.clone();
                    for (key, value) in capture.pairs() {
                        captured.push_with_index(*key, value.stabilize());
                    }
                    Box::new(NativeFunctionGenerator {
                        captured,
                        function: function.clone(),
                    })
                },
            ),
            cloned_pool,
        )),
        capture,
        signature.into(),
        LambdaType::Atomic,
    )
}
