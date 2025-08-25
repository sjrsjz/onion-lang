//! Onion 虚拟机 Lambda 启动器（Launcher）。
//!
//! 该模块实现了 `OnionLambdaRunnableLauncher`，用于将 Lambda 对象与参数、约束、捕获等进行动态绑定，
//! 并自动处理参数展开、约束检查、runnable 构造与映射等复杂流程。
//!
//! 主要特性：
//! - 支持参数自动展开与类型检查
//! - 支持参数约束（布尔/嵌套 Lambda）自动递归校验
//! - 支持自定义 runnable_mapper 对生成的 runnable 进行包装或变换
//! - 兼容原生与字节码 Lambda
//! - 便于与异步/同步调度器集成

use std::sync::Arc;

use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::definition::OnionLambdaDefinition,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    unwrap_step_result,
    utils::{
        fastmap::{OnionFastMap, OnionKeyPool},
        format_object_summary,
    },
};

/// Lambda 启动器。
///
/// 负责将 Lambda 对象与参数、约束、捕获等进行动态绑定，
/// 并自动处理参数展开、约束检查、runnable 构造与映射等复杂流程。
///
/// ## 字段说明
/// - `lambda`：被调用的 Lambda 对象（OnionObject::Lambda）
/// - `lambda_ref`：Lambda 定义的强引用
/// - `lambda_self_object`：Lambda 的 self 对象
/// - `argument`：原始参数对象（用于参数展开）
/// - `flatten_argument`：展平后的参数列表
/// - `string_pool`：参数名字符串池，保证参数名唯一性与高效查找
/// - `current_argument_index`：当前参数处理进度
/// - `runnable_mapper`：对生成的 runnable 进行包装/变换的回调
#[allow(unused)]
pub struct OnionLambdaRunnableLauncher {
    /// 被调用的 Lambda 对象（OnionObject::Lambda）
    lambda: OnionStaticObject,
    /// Lambda 定义的强引用
    lambda_ref: Arc<OnionLambdaDefinition>,
    /// Lambda 的 self 对象
    lambda_self_object: OnionStaticObject,

    /// 原始参数对象（用于参数展开）
    argument: OnionStaticObject,
    /// 展平后的参数列表
    flatten_argument: Vec<OnionObject>,

    /// 参数名字符串池，保证参数名唯一性与高效查找
    string_pool: OnionKeyPool<Box<str>>,
    /// 当前参数处理进度
    current_argument_index: usize,

    /// 对生成的 runnable 进行包装/变换的回调
    runnable_mapper:
        Box<dyn Fn(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError> + Sync + Send>,
}

impl OnionLambdaRunnableLauncher {
    /// 构造新的 Lambda 启动器。
    ///
    /// # 参数
    /// - `lambda`：被调用的 Lambda 对象（OnionObject::Lambda）
    /// - `argument`：参数对象（可为元组、字典等）
    /// - `runnable_mapper`：对生成的 runnable 进行包装/变换的回调
    ///
    /// # 返回值
    /// 返回新的 OnionLambdaRunnableLauncher 实例，参数已自动展开，约束待校验
    ///
    /// # 细节说明
    /// - string_pool 由被调用 Lambda 决定，保证参数名唯一性
    /// - 参数展开失败或类型不符会直接返回错误
    /// - runnable_mapper 可用于包装异步、同步等调度器
    pub fn new<F: Sync + Send + 'static>(
        lambda: &OnionObject,
        self_object: OnionStaticObject,
        argument: OnionStaticObject,
        runnable_mapper: F,
    ) -> Result<OnionLambdaRunnableLauncher, RuntimeError>
    where
        F: Fn(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError> + Sync + Send + 'static,
    {
        let OnionObject::Lambda((lambda_ref, _)) = lambda else {
            return Err(RuntimeError::InvalidType(
                "Cannot launch non-lambda object".into(),
            ));
        };
        let key_pool = lambda_ref.create_key_pool();

        // 参数自动展开，支持元组/字典等多种形式
        let flatten_argument = lambda_ref
            .get_parameter()
            .unpack_arguments(argument.weak())?;

        Ok(Self {
            lambda: lambda.stabilize(),
            lambda_ref: lambda_ref.clone(),
            lambda_self_object: self_object,
            argument,
            flatten_argument,
            string_pool: key_pool.clone(),
            current_argument_index: 0,
            runnable_mapper: Box::new(runnable_mapper),
        })
    }
}

impl Runnable for OnionLambdaRunnableLauncher {
    /// 接收子任务的执行结果。
    ///
    /// - StepResult::Continue：参数约束校验通过，继续
    /// - StepResult::Return：布尔约束校验，false 则报错
    /// - 其他类型（NewRunnable/ReplaceRunnable/SpawnRunnable）：均为非法，直接报错
    /// - StepResult::Error：直接转发错误
    fn receive(
        &mut self,
        step_result: &StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        match step_result {
            StepResult::Continue => Ok(()),
            StepResult::NewRunnable(_) => Err(RuntimeError::DetailedError(
                "OnionLambdaRunnableLauncher cannot yield new runnables"
                    .to_string()
                    .into(),
            )),
            StepResult::Return(constraint_result) => {
                // 约束校验结果，必须为 true
                if constraint_result.weak().to_boolean()? {
                    Ok(())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Constraint check failed".into(),
                    ))
                }
            }
            StepResult::ReplaceRunnable(_) => Err(RuntimeError::DetailedError(
                "OnionLambdaRunnableLauncher cannot replace runnables"
                    .to_string()
                    .into(),
            )),
            StepResult::Error(e) => Err(e.clone()),
            StepResult::SpawnRunnable(_) => Err(RuntimeError::DetailedError(
                "OnionLambdaRunnableLauncher cannot spawn new runnables"
                    .to_string()
                    .into(),
            )),
        }
    }

    /// 推进参数约束校验与 runnable 构造。
    ///
    /// - 首先依次校验所有参数约束（布尔/嵌套 Lambda）
    ///   - 若为布尔约束且为 false，立即报错
    ///   - 若为 Lambda 约束，递归生成新 Launcher 进行校验
    ///   - 其他类型直接报错
    /// - 所有约束校验通过后，收集参数，调用 lambda_ref.create_runnable 构造 runnable
    /// - 通过 runnable_mapper 包装后，替换当前 runnable
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        if self.current_argument_index == self.lambda_ref.get_flatten_param_keys().len() {
            // 所有参数约束校验通过，收集参数并构造 runnable
            let mut collected_arguments = OnionFastMap::new(self.string_pool.clone());
            for i in 0..self.current_argument_index {
                collected_arguments.push(
                    &self.lambda_ref.get_flatten_param_keys()[i],
                    self.flatten_argument[i].stabilize(),
                );
            }

            let runnable = unwrap_step_result!(self.lambda_ref.create_runnable(
                &collected_arguments,
                &self.lambda,
                self.lambda_self_object.weak(),
                gc,
            ));

            // 通过 runnable_mapper 包装（如异步/同步调度器）
            let mapped = unwrap_step_result!((self.runnable_mapper)(runnable));
            return StepResult::ReplaceRunnable(mapped);
        }

        // 依次校验参数约束
        let mut index = self.current_argument_index;
        while index < self.lambda_ref.get_flatten_param_keys().len() {
            match &self.lambda_ref.get_flatten_param_constraints()[index] {
                OnionObject::Boolean(v) => {
                    if !*v {
                        self.current_argument_index = index + 1;
                        return StepResult::Error(RuntimeError::InvalidOperation(
                            "Constraint check failed".into(),
                        ));
                    }
                }
                lambda @ OnionObject::Lambda((_, self_object)) => {
                    // 嵌套 Lambda 约束，递归生成新 Launcher
                    self.current_argument_index = index + 1;
                    return StepResult::NewRunnable(Box::new(unwrap_step_result!(
                        OnionLambdaRunnableLauncher::new(
                            lambda,
                            self_object.stabilize(),
                            self.flatten_argument[index].stabilize(),
                            |r| Ok(r),
                        )
                    )));
                }
                v => {
                    self.current_argument_index = index + 1;
                    return StepResult::Error(
                        RuntimeError::InvalidType(
                            format!(
                                "Expect boolean or lambda for constraint, but found: {:?}",
                                v
                            )
                            .into(),
                        )
                        .into(),
                    );
                }
            }
            index += 1;
        }
        self.current_argument_index = index;
        StepResult::Continue
    }

    /// 格式化当前 Launcher 的上下文信息。
    ///
    /// 输出当前参数处理进度、Lambda 信息等，便于调试。
    fn format_context(&self) -> String {
        "-> At lambda runnable launcher".to_string()
            + &format!(
                " (current index: {}, expected: {})",
                self.current_argument_index,
                self.lambda_ref.get_flatten_param_keys().len()
            )
            + &format!(", lambda: {}", format_object_summary(self.lambda.weak()))
    }
}
