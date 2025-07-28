use std::sync::Arc;

use arc_gc::{arc::GCArc, gc::GC};

use crate::{
    lambda::{
        runnable::{Runnable, RuntimeError, StepResult},
        scheduler::scheduler::Scheduler,
    },
    types::{
        named::OnionNamed,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    unwrap_step_result,
    util::format_object_summary,
};

#[derive(Debug, Clone)]
enum ArgumentProcessingPhase {
    NamedArguments,
    PositionalArguments,
    Done,
}

#[allow(unused)]
pub struct OnionLambdaRunnableLauncher {
    lambda: OnionStaticObject, // The OnionObject::Lambda itself

    parameter_tuple: Arc<OnionTuple>, // The parameter tuple from the lambda definition
    parameter_arcs: Vec<GCArc<OnionObjectCell>>, // The parameter arcs from the lambda definition

    argument_tuple: Arc<OnionTuple>, // The provided argument tuple object
    argument_arcs: Vec<GCArc<OnionObjectCell>>, // The provided argument arcs from the argument tuple

    collected_arguments: Vec<OnionStaticObject>, // Arguments collected during processing
    assigned: Vec<bool>, // Tracks assignment for parameter slots, then for appended args
    current_argument_index: usize, // Index into argument_elements for current phase

    phase: ArgumentProcessingPhase,

    runnable_mapper:
        Arc<dyn Fn(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError> + Sync + Send>,
    constrain_runnable: Option<Box<dyn Runnable>>,
}

impl OnionLambdaRunnableLauncher {
    pub fn new_static<F: Sync + Send + 'static>(
        lambda_obj: &OnionStaticObject,
        argument_tuple_obj: &OnionStaticObject,
        runnable_mapper: F,
    ) -> Result<OnionLambdaRunnableLauncher, RuntimeError>
    where
        F: Fn(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError> + Sync + Send + 'static,
    {
        // Initialize collected_arguments based on parameter count
        let mut collected_arguments = Vec::new();
        let mut assigned = Vec::new();

        let (parameter_tuple, parameter_arcs) = lambda_obj.weak().with_data(|obj_ref| {
            if let OnionObject::Lambda((definition, _)) = obj_ref {
                definition.get_parameter().with_data(|p_obj| {
                    if let OnionObject::Tuple(tuple) = p_obj {
                        // Initialize collected_arguments and assigned based on parameter count
                        for param in tuple.get_elements() {
                            param.with_data(|param_obj| {
                                match param_obj {
                                    OnionObject::LazySet(lazy_set) => {
                                        // If the parameter is a LazySet, we collect its container.
                                        collected_arguments
                                            .push(lazy_set.get_container().stabilize());
                                    }
                                    _ => {
                                        // For other types, we just clone the parameter as is.
                                        collected_arguments.push(param.stabilize());
                                    }
                                }
                                Ok(())
                            })?;
                        }
                        assigned = vec![false; tuple.get_elements().len()];

                        let parameter_tuple = tuple.clone();
                        let mut arcs = vec![];
                        tuple.upgrade(&mut arcs);

                        Ok((parameter_tuple, arcs))
                    } else {
                        Err(RuntimeError::DetailedError(
                            "Lambda parameters must be a Tuple".to_string().into(),
                        ))
                    }
                })
            } else {
                Err(RuntimeError::DetailedError(
                    "Expected a Lambda definition object".to_string().into(),
                ))
            }
        })?;

        let (argument_tuple, argument_arcs) = argument_tuple_obj.weak().with_data(|arg_obj| {
            if let OnionObject::Tuple(tuple) = arg_obj {
                // Initialize argument_tuple and its arcs(simulate `stabilize` behavior)
                let argument_tuple = tuple.clone();
                let mut arcs = vec![];
                tuple.upgrade(&mut arcs);
                Ok((argument_tuple, arcs))
            } else {
                Err(RuntimeError::DetailedError(
                    "Lambda arguments must be a Tuple".to_string().into(),
                ))
            }
        })?;

        Ok(OnionLambdaRunnableLauncher {
            lambda: lambda_obj.clone(),
            parameter_tuple: parameter_tuple,
            parameter_arcs: parameter_arcs,
            argument_tuple: argument_tuple,
            argument_arcs: argument_arcs,
            collected_arguments,
            assigned,
            phase: ArgumentProcessingPhase::NamedArguments,
            current_argument_index: 0,
            runnable_mapper: Arc::new(runnable_mapper),
            constrain_runnable: None,
        })
    }
}

impl Runnable for OnionLambdaRunnableLauncher {
    fn receive(
        &mut self,
        step_result: &StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        match step_result {
            StepResult::Continue => Ok(()),
            StepResult::NewRunnable(_) => {
                // This should not happen, as this launcher is not designed to yield new runnables.
                Err(RuntimeError::DetailedError(
                    "OnionLambdaRunnableLauncher cannot yield new runnables"
                        .to_string()
                        .into(),
                ))
            }
            StepResult::Return(_) => {
                // This should not happen, as this launcher is not designed to return values.
                Err(RuntimeError::DetailedError(
                    "OnionLambdaRunnableLauncher cannot return values"
                        .to_string()
                        .into(),
                ))
            }
            StepResult::ReplaceRunnable(_) => {
                // This should not happen, as this launcher is not designed to replace runnables.
                Err(RuntimeError::DetailedError(
                    "OnionLambdaRunnableLauncher cannot replace runnables"
                        .to_string()
                        .into(),
                ))
            }
            StepResult::Error(e) => {
                // Propagate any errors received from the runnable.
                Err(e.clone())
            }
            StepResult::SetSelfObject(_) => {
                // This should not happen, as this launcher is not designed to set self objects.
                Err(RuntimeError::DetailedError(
                    "OnionLambdaRunnableLauncher cannot set self objects"
                        .to_string()
                        .into(),
                ))
            }
            StepResult::SpawnRunnable(_) => {
                // This should not happen, as this launcher is not designed to spawn new runnables.
                Err(RuntimeError::DetailedError(
                    "OnionLambdaRunnableLauncher cannot spawn new runnables"
                        .to_string()
                        .into(),
                ))
            }
        }
    }
    /// 执行 Lambda 启动器的下一步操作。
    ///
    /// 此方法通过一个状态机来处理参数的收集、验证（通过约束）并最终启动 Lambda。
    /// 它的主要职责是：
    /// 1. 如果存在参数约束 (`constrain_runnable`)，则先执行约束。
    /// 2. 根据当前的 `phase`（命名参数、位置参数、完成）处理传入的参数。
    /// 3. 收集和整理参数，直到所有参数处理完毕。
    /// 4. 创建并返回目标 Lambda 的可运行实例。
    ///
    /// # 参数
    /// * `gc`: 垃圾收集器的可变引用，用于内存管理。
    ///
    /// # 返回
    /// * `Ok(StepResult)`: 表示操作成功，并指示调度器下一步应该做什么。
    ///   - `StepResult::Continue`: 表示启动器需要更多步骤来完成参数处理或约束执行。
    ///   - `StepResult::ReplaceRunnable`: 表示参数处理完成，启动器应被新的 Lambda 可运行实例替换。
    /// * `Err(RuntimeError)`: 表示在执行过程中发生错误。
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        // 阶段一：执行参数约束 (如果存在)
        // 如果 `constrain_runnable` (通常是一个由 LazySet 的 filter 创建的 Lambda 启动器) 存在，
        // 意味着当前正在处理的参数有一个关联的约束需要被满足。
        if let Some(constrain_runnable) = self.constrain_runnable.as_mut() {
            // 执行约束可运行对象的 `step` 方法
            match constrain_runnable.step(gc) {
                // 约束尚未完成，需要继续执行。启动器也返回 Continue。
                StepResult::Continue => {
                    return StepResult::Continue;
                }
                // 约束执行过程中不应产生新的可运行对象或替换自身。
                v @ StepResult::NewRunnable(_) => return v,
                v @ StepResult::SpawnRunnable(_) => return v,
                v @ StepResult::Error(_) => return v,
                StepResult::ReplaceRunnable(runnable) => self.constrain_runnable = Some(runnable),

                // 约束执行完成并返回了一个值。
                // 约束的返回值约定为一个 Pair：(布尔值表示是否panic, 布尔值表示约束是否通过)
                StepResult::Return(ref v) => {
                    // 解析约束的返回值
                    unwrap_step_result!(v.weak().with_data(|v_obj| {
                        // 约束的返回值必须是一个 Pair 对象
                        let OnionObject::Pair(constrain_result_pair) = v_obj else {
                            // 如果不是 Pair，则认为约束失败（或者约束实现有误）
                            return Err(RuntimeError::DetailedError(
                                "Constrain runnable did not return a Pair object"
                                    .to_string()
                                    .into(),
                            ));
                        };

                        // 第一个元素 (key) 表示约束执行是否 panic
                        match constrain_result_pair.get_key().to_boolean() {
                            Ok(true) => {
                                // true 表示约束执行没有 panic
                                // 第二个元素 (value) 表示约束是否通过
                                if constrain_result_pair.get_value().to_boolean()? {
                                    // 约束通过，清除 `constrain_runnable`，准备处理下一个参数或阶段。
                                    self.constrain_runnable = None;
                                    // 注意：这里原代码是 `return Ok(())`，这不符合 `step` 的签名。
                                    // 应该返回 `Ok(StepResult::Continue)` 以便继续处理参数。
                                    // 假设这是期望的行为，即约束成功后，继续当前 `step` 的后续逻辑。
                                    // 如果这里直接返回，则当前 `step` 的参数处理逻辑会被跳过。
                                    // 为了与原逻辑最接近（即清除约束后继续本轮 step 的后续参数处理），
                                    // 我们不在这里 `return`，而是让代码流继续到下面的 `match self.phase`。
                                    // 如果期望的是约束成功后立即开始下一轮 `step`，則應 `return Ok(StepResult::Continue)`。
                                    // 鉴于后续代码会继续处理参数，这里不返回是合理的。
                                    Ok(()) // 标记约束已处理，但不立即返回，让后续的 phase match 执行
                                } else {
                                    // 约束未通过 (返回 false)
                                    return Err(RuntimeError::DetailedError(
                                        "Argument constraint failed".to_string().into(),
                                    ));
                                }
                            }
                            Ok(false) => {
                                // false 表示约束执行过程中发生了 panic
                                return Err(RuntimeError::CustomValue(Box::new(
                                    constrain_result_pair.get_value().stabilize(),
                                )));
                            }
                            Err(err) => return Err(err), // 转换布尔值失败
                        }
                    }))

                    // 如果约束成功并通过 (上面返回 Ok(()) 但没有实际 return)，则会继续到下面的 phase 处理。
                    // 如果约束失败或 panic (上面返回 Err)，则整个 step 会在这里结束。
                }
                StepResult::SetSelfObject(_) => {
                    // 这个启动器不支持设置 self 对象，因此返回错误。
                    return StepResult::Error(RuntimeError::DetailedError(
                        "OnionLambdaRunnableLauncher does not support setting self object"
                            .to_string()
                            .into(),
                    ));
                }
            }
        } // 结束 `if let Some(constrain_runnable)`

        // 阶段二：根据当前处理阶段 (phase) 处理参数
        match self.phase {
            // 阶段 2.1: 处理命名参数
            ArgumentProcessingPhase::NamedArguments => {
                // 直接使用 self.argument_tuple 获取参数元素
                let argument_elements = &self.argument_tuple.get_elements();
                let argument_count = argument_elements.len();

                // 如果当前参数索引超出了提供的参数列表的范围，
                // 说明所有提供的参数都已在命名参数阶段被初步检查过。
                // 切换到位置参数处理阶段。
                if self.current_argument_index >= argument_count {
                    self.phase = ArgumentProcessingPhase::PositionalArguments;
                    self.current_argument_index = 0; // 重置索引以供位置参数阶段使用
                    return StepResult::Continue; // 请求调度器再次调用 step
                }

                // 获取当前正在处理的由调用者提供的参数
                let current_arg_index = self.current_argument_index;
                self.current_argument_index += 1; // 移动到下一个提供的参数

                let arg_obj_view = &argument_elements[current_arg_index];
                // 检查当前提供的参数是否是命名参数 (`OnionObject::Named`)
                if let OnionObject::Named(named_arg) = arg_obj_view {
                    let key_to_match = &named_arg.get_key(); // 获取命名参数的名称

                    // 遍历 Lambda 定义中的参数，直接使用 self.parameter_tuple
                    let parameter_elements = &self.parameter_tuple.get_elements();
                    let parameter_count = parameter_elements.len();

                    for param_idx in 0..parameter_count {
                        // 直接访问参数定义
                        let param_element = &parameter_elements[param_idx];
                        let matched = match param_element {
                            // 情况 A: Lambda 定义的参数也是一个命名参数 (`name: Type`)
                            OnionObject::Named(param_named_def) => {
                                if unwrap_step_result!(param_named_def
                                    .get_key()
                                    .equals(key_to_match))
                                {
                                    // 名称匹配成功！
                                    self.collected_arguments[param_idx] = arg_obj_view.stabilize();
                                    self.assigned[param_idx] = true;
                                    true
                                } else {
                                    false
                                }
                            }
                            // 情况 B: Lambda 定义的参数是一个 LazySet (`name: {constraint}`)
                            OnionObject::LazySet(lazy_set) => {
                                if let OnionObject::Named(container_named) =
                                    lazy_set.get_container()
                                {
                                    if unwrap_step_result!(container_named
                                        .get_key()
                                        .equals(key_to_match))
                                    {
                                        // 名称匹配成功！
                                        self.collected_arguments[param_idx] =
                                            arg_obj_view.stabilize();
                                        self.assigned[param_idx] = true;

                                        // 设置约束
                                        let argument_for_filter = OnionObject::Tuple(
                                            OnionTuple::new(vec![named_arg.get_value().clone()])
                                                .into(),
                                        )
                                        .consume_and_stabilize();

                                        let runnable = Box::new(unwrap_step_result!(
                                            OnionLambdaRunnableLauncher::new_static(
                                                &lazy_set.get_filter().stabilize(),
                                                &argument_for_filter,
                                                &|r| Ok(r),
                                            )
                                        ));
                                        self.constrain_runnable =
                                            Some(Box::new(Scheduler::new(vec![runnable])));
                                        true
                                    } else {
                                        false
                                    }
                                } else {
                                    false
                                }
                            }
                            _ => false,
                        };

                        if matched {
                            return StepResult::Continue;
                        }
                    }

                    // 如果遍历完所有 Lambda 定义的参数后，没有找到匹配的名称，
                    // 说明这是一个额外的命名参数。
                    self.collected_arguments.push(arg_obj_view.stabilize());
                    self.assigned.push(true);
                };

                // 如果当前提供的参数不是 OnionObject::Named，则在命名参数阶段被忽略。
                StepResult::Continue
            }
            // 阶段 2.2: 处理位置参数
            ArgumentProcessingPhase::PositionalArguments => {
                // 直接使用 self.argument_tuple 获取参数数量
                let argument_count = self.argument_tuple.get_elements().len();

                // 如果当前参数索引超出了提供的参数列表的范围，
                // 说明所有提供的参数都已在位置参数阶段被处理。
                // 切换到完成阶段。
                if self.current_argument_index >= argument_count {
                    self.phase = ArgumentProcessingPhase::Done;
                    return StepResult::Continue; // 请求调度器再次调用 step
                }

                // 获取当前正在处理的由调用者提供的参数
                let current_processing_arg_idx = self.current_argument_index;
                self.current_argument_index += 1; // 移动到下一个提供的参数

                // 直接从 self.argument_tuple 获取参数元素
                let argument_elements = &self.argument_tuple.get_elements();

                if current_processing_arg_idx >= argument_elements.len() {
                    return StepResult::Error(RuntimeError::DetailedError(
                        "Argument index out of bounds during positional processing"
                            .to_string()
                            .into(),
                    ));
                }

                let current_provided_arg_view = &argument_elements[current_processing_arg_idx];

                // 如果当前提供的参数是命名参数，则在位置参数阶段跳过。
                if let OnionObject::Named(_) = current_provided_arg_view {
                    // Skip named arguments in this phase.
                } else {
                    // This is a positional argument.
                    let current_provided_arg_static = current_provided_arg_view.stabilize();

                    // 尝试找到第一个尚未被赋值的 Lambda 定义参数槽。
                    if let Some(param_idx) = self.assigned.iter().position(|&assigned| !assigned) {
                        // 找到了一个未分配的参数槽。直接从 self.parameter_tuple 访问参数定义
                        let parameter_elements = &self.parameter_tuple.get_elements();

                        if param_idx >= parameter_elements.len() {
                            return StepResult::Error(RuntimeError::DetailedError(
                                "Parameter index out of bounds for assignment"
                                    .to_string()
                                    .into(),
                            ));
                        }

                        let param_def = &parameter_elements[param_idx];
                        match param_def {
                            // 情况 A: Lambda 定义的参数是 `name: Type` (Named)
                            OnionObject::Named(original_named_def) => {
                                // 将位置参数包装成一个新的 Named 对象，使用原始定义的名称。
                                let new_value_for_slot = OnionObject::Named(
                                    OnionNamed::new(
                                        original_named_def.get_key().clone(),
                                        current_provided_arg_view.clone(),
                                    )
                                    .into(),
                                )
                                .consume_and_stabilize();
                                self.collected_arguments[param_idx] = new_value_for_slot;
                                self.assigned[param_idx] = true;
                            }
                            // 情况 B: Lambda 定义的参数是 `name: {constraint}` (LazySet)
                            OnionObject::LazySet(lazy_set_def) => {
                                if let OnionObject::Named(container_named_def) =
                                    lazy_set_def.get_container()
                                {
                                    // 将位置参数包装成 Named 对象
                                    let new_value_for_slot = OnionObject::Named(
                                        OnionNamed::new(
                                            container_named_def.get_key().clone(),
                                            current_provided_arg_view.clone(),
                                        )
                                        .into(),
                                    )
                                    .consume_and_stabilize();
                                    self.collected_arguments[param_idx] = new_value_for_slot;
                                    self.assigned[param_idx] = true;

                                    // 设置约束
                                    // The argument to the filter lambda is the provided argument itself.
                                    let argument_for_filter = OnionObject::Tuple(
                                        OnionTuple::new(vec![current_provided_arg_view.clone()])
                                            .into(),
                                    )
                                    .consume_and_stabilize();

                                    let runnable = Box::new(unwrap_step_result!(
                                        OnionLambdaRunnableLauncher::new_static(
                                            &lazy_set_def.get_filter().stabilize(),
                                            &argument_for_filter,
                                            &|r| Ok(r),
                                        )
                                    ));
                                    self.constrain_runnable =
                                        Some(Box::new(Scheduler::new(vec![runnable])));
                                } else {
                                    return StepResult::Error(RuntimeError::DetailedError(
                                        "LazySet's container must be a Named object for positional assignment".to_string().into(),
                                    ));
                                }
                            }
                            // 情况 C: Lambda 定义的参数是普通类型
                            _ => {
                                self.collected_arguments[param_idx] = current_provided_arg_static;
                                self.assigned[param_idx] = true;
                            }
                        }
                    } else {
                        // 所有 Lambda 定义的参数槽都已被填充。 This is an extra positional argument.
                        self.collected_arguments.push(current_provided_arg_static);
                        self.assigned.push(true);
                    }
                }

                StepResult::Continue
            }

            // 阶段 2.3: 完成参数处理，准备启动 Lambda
            ArgumentProcessingPhase::Done => {
                // 检查是否仍有未完成的约束。如果 `constrain_runnable` 仍然是 `Some`，
                // 这意味着上一个参数的约束还没有执行完毕或返回结果。
                // 此时应该等待约束完成，而不是直接创建 Lambda。
                if self.constrain_runnable.is_some() {
                    // 理论上，如果约束存在，应该在 `step` 的开头被处理。
                    // 如果执行到 `Done` 阶段约束仍在，说明之前的约束处理逻辑可能需要返回 `Continue`
                    // 直到约束被清除。或者，这是一个不期望的状态。
                    // 为安全起见，如果还有约束，则继续等待。
                    return StepResult::Continue;
                }

                // 所有参数都已收集完毕，并且所有约束（如果有的话）都已满足。
                // 使用 `collected_arguments` 创建最终的参数元组。
                let final_args_static = OnionTuple::new_static_no_ref(&self.collected_arguments);

                // 获取原始 Lambda 定义对象。
                unwrap_step_result!(self.lambda.weak().with_data(|obj| {
                    if let OnionObject::Lambda((lambda_def, self_object)) = obj {
                        // 使用最终的参数元组和 Lambda 定义来创建实际的 Lambda 可运行实例。
                        let runnable = lambda_def
                            .create_runnable(final_args_static, &self.lambda, self_object, gc)
                            .map_err(|e| {
                                RuntimeError::InvalidType(
                                    format!("Failed to create runnable from lambda: {}", e).into(),
                                )
                            })?;
                        // 应用 mapper。
                        // 成功映射，返回 ReplaceRunnable
                        (self.runnable_mapper)(runnable)
                            .map(|result_runnable| StepResult::ReplaceRunnable(result_runnable))
                    } else {
                        // 这是一个内部错误，启动器持有的 lambda 对象不是 Lambda 类型。
                        Err(RuntimeError::DetailedError(
                            "Launcher's lambda object is not OnionObject::Lambda"
                                .to_string()
                                .into(),
                        ))
                    }
                }))
            }
        }
    }

    fn format_context(&self) -> String {
        let mut parts = Vec::new();

        // 1. 核心信息：为哪个 Lambda 工作
        parts.push(format!(
            "-> Launching Lambda: {}",
            format_object_summary(self.lambda.weak())
        ));

        // 2. 当前状态机阶段
        parts.push(format!(
            "  - Phase: {:?} (at argument index {})",
            // 使用 Debug trait 来打印枚举变体名称
            self.phase,
            self.current_argument_index
        ));

        // 3. 是否在等待约束
        if let Some(constrain_runnable) = &self.constrain_runnable {
            parts.push("  - Status: Waiting for parameter constraint to resolve".to_string());
            // 递归地获取约束 runnable 的上下文，并缩进
            let constrain_context = constrain_runnable.format_context();
            for line in constrain_context.lines() {
                parts.push(format!("    {}", line));
            }
        } else {
            parts.push("  - Status: Processing arguments".to_string());
        }

        // 4. 传入的参数概览
        parts.push(format!(
            "  - Provided Arguments ({} total):",
            self.argument_tuple.get_elements().len()
        ));
        for (i, arg) in self.argument_tuple.get_elements().iter().enumerate() {
            let indicator = if i == self.current_argument_index - 1 {
                "<- current"
            } else {
                ""
            };
            parts.push(format!(
                "    - [{}]: {} {}",
                i,
                format_object_summary(arg),
                indicator
            ));
        }

        // 5. Lambda 的参数定义概览
        parts.push(format!(
            "  - Lambda Parameters ({} total):",
            self.parameter_tuple.get_elements().len()
        ));
        for (i, param) in self.parameter_tuple.get_elements().iter().enumerate() {
            parts.push(format!("    - [{}]: {}", i, format_object_summary(param)));
        }

        // 6. 已收集和分配的参数状态
        parts.push(format!(
            "  - Collected/Assigned Arguments ({} slots):",
            self.collected_arguments.len()
        ));
        for i in 0..self.collected_arguments.len() {
            let assigned_status = if i < self.assigned.len() && self.assigned[i] {
                "Assigned"
            } else if i < self.assigned.len() {
                "Not Assigned (default)"
            } else {
                "Appended"
            };
            parts.push(format!(
                "    - Slot [{}]: {} ({})",
                i,
                format_object_summary(self.collected_arguments[i].weak()),
                assigned_status
            ));
        }

        parts.join("\n")
    }
}
