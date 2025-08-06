//! Onion 语言标准库模块。
//!
//! 本模块提供 Onion 语言的标准库实现，包含各种内置功能和实用程序。
//! 标准库以原生函数的形式集成到虚拟机中，为 Onion 程序提供丰富的功能支持。
//!
//! # 模块结构
//! - `bytes`：字节串处理功能
//! - `ffi`：外部函数接口（FFI）支持
//! - `fs`：文件系统操作
//! - `http`：HTTP 客户端功能
//! - `io`：输入输出操作
//! - `json`：JSON 数据处理
//! - `math`：数学计算函数
//! - `os`：操作系统相关功能
//! - `string`：字符串处理
//! - `sys`：系统级功能
//! - `time`：时间相关操作
//! - `tuple`：元组处理
//! - `types`：类型相关工具
//!
//! # 使用方式
//!
//! 标准库通过 `build_module()` 函数构建为一个包含所有子模块的对象，
//! 然后在VM执行时注入到程序的捕获变量中：
//!
//! ```rust
//! use crate::stdlib;
//! use onion_vm::utils::fastmap::OnionFastMap;
//!
//! // 构建标准库
//! let stdlib = stdlib::build_module();
//!
//! // 创建捕获变量映射
//! let mut capture = OnionFastMap::new(key_pool);
//! capture.push("stdlib", stdlib.weak().clone());
//!
//! // 现在 Onion 程序中可以使用 stdlib.io.print() 等功能
//! ```
//!
//! # 原生函数包装
//!
//! 本模块提供了将 Rust 函数包装为 Onion Lambda 的工具：
//! - `NativeFunctionGenerator`：原生函数生成器，实现 Runnable trait
//! - `wrap_native_function`：将 Rust 闭包包装为 Onion Lambda 定义
//! - `build_dict`：将键值对映射转换为 Onion 元组对象

use std::sync::Arc;

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
        pair::OnionPair,
        tuple::OnionTuple,
    },
    unwrap_step_result,
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};
mod bytes;
mod ffi;
mod fs;
mod http;
mod io;
mod json;
mod math;
mod os;
mod string;
mod sys;
mod time;
mod tuple;
mod types;

/// 将键值对字典转换为 Onion 元组对象。
///
/// 此函数接收一个 IndexMap 并将其转换为包含键值对的 Onion 元组。
/// 每个键值对都被包装为 OnionPair，然后所有对被收集到一个元组中。
///
/// # 参数
/// - `dict`：要转换的字符串到 OnionStaticObject 的映射
///
/// # 返回
/// 包含所有键值对的 OnionStaticObject（元组类型）
pub fn build_dict(dict: IndexMap<String, OnionStaticObject>) -> OnionStaticObject {
    let mut pairs = vec![];
    for (key, value) in dict {
        pairs.push(OnionPair::new_static(
            &OnionObject::String(key.into()).stabilize(),
            &value,
        ));
    }
    OnionTuple::new_static_no_ref(&pairs)
}

/// 原生函数生成器。
///
/// 此结构体包装 Rust 闭包，使其可以作为 Onion Lambda 在虚拟机中执行。
/// 它实现了 Runnable trait，可以被调度器调度执行。
///
/// # 泛型参数
/// - `F`：闭包类型，接收捕获变量和GC，返回执行结果
pub struct NativeFunctionGenerator<F>
where
    F: Fn(
        &OnionFastMap<Box<str>, OnionStaticObject>,
        &mut GC<OnionObjectCell>,
    ) -> Result<OnionStaticObject, RuntimeError>,
{
    captured: OnionFastMap<Box<str>, OnionStaticObject>,
    function: F,
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
    /// 执行原生函数的一个步骤。
    ///
    /// 调用包装的 Rust 闭包并将结果转换为 StepResult。
    /// 如果执行成功，返回 StepResult::Return；如果出错，返回相应的错误。
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        unwrap_step_result!(
            (self.function)(&self.captured, gc).map(|result| StepResult::Return(result.into()))
        )
    }
    
    /// 格式化当前执行上下文，用于调试和错误报告。
    ///
    /// 返回包含函数类型名称和捕获参数的详细信息。
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

/// 将 Rust 闭包包装为 Onion Lambda 定义。
///
/// 此函数是标准库函数注册的核心工具，它将 Rust 原生函数转换为
/// Onion 虚拟机可以执行的 Lambda 对象。
///
/// # 参数
/// - `params`：Lambda 参数定义
/// - `capture`：捕获的变量映射
/// - `signature`：函数签名字符串（用于调试）
/// - `string_pool`：字符串池，用于优化内存使用
/// - `function`：要包装的 Rust 闭包
///
/// # 返回
/// 包装后的 OnionLambdaDefinition 静态对象
///
/// # 示例
/// ```rust
/// let add_fn = wrap_native_function(
///     LambdaParameter::Multiple(vec![
///         ("a".into(), None),
///         ("b".into(), None)
///     ].into()),
///     OnionFastMap::new(pool.clone()),
///     "add(a, b)",
///     pool,
///     &|args, _gc| {
///         // 实现加法逻辑
///         Ok(/* 返回结果 */)
///     }
/// );
/// ```
pub fn wrap_native_function<F>(
    params: LambdaParameter,
    capture: OnionFastMap<Box<str>, OnionObject>,
    signature: &str,
    string_pool: OnionKeyPool<Box<str>>,
    function: &'static F,
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
                    Box::new(NativeFunctionGenerator { captured, function })
                },
            ),
            cloned_pool,
        )),
        capture,
        signature.into(),
        LambdaType::Atomic,
    )
}

/// 构建完整的标准库模块。
///
/// 此函数创建包含所有子模块的标准库对象，每个子模块都通过其对应的
/// `build_module()` 函数构建。返回的对象可以直接注入到 Onion 程序的
/// 执行环境中。
///
/// # 包含的模块
/// - `bytes`：字节串处理
/// - `fs`：文件系统操作  
/// - `io`：输入输出
/// - `types`：类型工具
/// - `math`：数学函数
/// - `string`：字符串处理
/// - `http`：HTTP 客户端
/// - `time`：时间操作
/// - `json`：JSON 处理
/// - `os`：操作系统接口
/// - `sys`：系统功能
/// - `ffi`：外部函数接口
///
/// # 返回
/// 包含所有标准库功能的 OnionStaticObject
///
/// # 使用示例
/// ```rust
/// let stdlib = build_module();
/// // 将标准库注入到程序执行环境中
/// capture.push("stdlib", stdlib.weak().clone());
/// ```
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();
    module.insert("bytes".to_string(), bytes::build_module());
    module.insert("fs".to_string(), fs::build_module());
    module.insert("io".to_string(), io::build_module());
    module.insert("types".to_string(), types::build_module());
    module.insert("math".to_string(), math::build_module());
    module.insert("string".to_string(), string::build_module());
    module.insert("http".to_string(), http::build_module());
    module.insert("time".to_string(), time::build_module());
    module.insert("json".to_string(), json::build_module());
    module.insert("os".to_string(), os::build_module());
    module.insert("sys".to_string(), sys::build_module());
    module.insert("ffi".to_string(), ffi::build_module());
    build_dict(module)
}
