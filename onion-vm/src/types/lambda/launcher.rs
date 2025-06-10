use arc_gc::gc::GC;

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
};

enum ArgumentProcessingPhase {
    NamedArguments,
    PositionalArguments,
    Done,
}

pub struct OnionLambdaRunnableLauncher {
    lambda: OnionStaticObject, // The OnionObject::Lambda itself
    parameter_elements: Vec<OnionStaticObject>, // Elements from lambda.parameter tuple (definitions)
    argument_elements: Vec<OnionStaticObject>,  // Elements from the provided argument tuple

    pub collected_arguments: Vec<OnionStaticObject>, // Starts as clone of parameter_elements, gets updated
    pub assigned: Vec<bool>, // Tracks assignment for parameter_elements slots, then for appended args

    phase: ArgumentProcessingPhase,
    current_argument_index: usize, // Index into argument_elements for current phase

    runnable_mapper:
        Option<Box<dyn FnOnce(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError>>>,

    constrain_runnable: Option<Box<dyn Runnable>>,
}

impl OnionLambdaRunnableLauncher {
    pub fn new_static<F: 'static>(
        lambda_obj: &OnionStaticObject,
        argument_tuple_obj: &OnionStaticObject,
        runnable_mapper: F,
    ) -> Result<OnionLambdaRunnableLauncher, RuntimeError>
    where
        F: FnOnce(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError>,
    {
        let lambda_definition = lambda_obj.weak().try_borrow()?.with_data(|obj_ref| {
            if let OnionObject::Lambda(definition) = obj_ref {
                Ok(definition.clone())
            } else {
                Err(RuntimeError::DetailedError(
                    "Expected a Lambda definition object".to_string(),
                ))
            }
        })?;

        let parameter_elements: Vec<OnionStaticObject> = lambda_definition
            .parameter
            .try_borrow()?
            .with_data(|p_obj| {
                if let OnionObject::Tuple(tuple) = p_obj {
                    Ok(tuple.elements.clone())
                } else {
                    Err(RuntimeError::DetailedError(
                        "Lambda parameters must be a Tuple".to_string(),
                    ))
                }
            })?
            .into_iter()
            .map(|o| o.stabilize())
            .collect();

        let argument_elements = argument_tuple_obj
            .weak()
            .try_borrow()?
            .with_data(|arg_obj| {
                if let OnionObject::Tuple(tuple) = arg_obj {
                    Ok(tuple.elements.clone())
                } else {
                    Err(RuntimeError::DetailedError(
                        "Lambda arguments must be a Tuple".to_string(),
                    ))
                }
            })?
            .into_iter()
            .map(|o| o.stabilize())
            .collect();

        let mut collected_arguments = Vec::new();
        for param in &parameter_elements {
            param.weak().try_borrow()?.with_data(|param_obj| {
                match param_obj {
                    OnionObject::LazySet(lazy_set) => {
                        // If the parameter is a LazySet, we collect its container.
                        collected_arguments.push(lazy_set.get_container().clone().stabilize());
                    }
                    _ => {
                        // For other types, we just clone the parameter as is.
                        collected_arguments.push(param.clone());
                    }
                }
                Ok(())
            })?;
        }
        let assigned = vec![false; parameter_elements.len()];

        Ok(OnionLambdaRunnableLauncher {
            lambda: lambda_obj.clone(),
            parameter_elements,
            argument_elements,
            collected_arguments,
            assigned,
            phase: ArgumentProcessingPhase::NamedArguments,
            current_argument_index: 0,
            runnable_mapper: Some(Box::new(runnable_mapper)),
            constrain_runnable: None,
        })
    }
}

impl Runnable for OnionLambdaRunnableLauncher {
    fn receive(
        &mut self,
        step_result: StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        match step_result {
            StepResult::Continue => Ok(()),
            StepResult::NewRunnable(_) => {
                // This should not happen, as this launcher is not designed to yield new runnables.
                Err(RuntimeError::DetailedError(
                    "OnionLambdaRunnableLauncher cannot yield new runnables".to_string(),
                ))
            }
            StepResult::Return(_) => {
                // This should not happen, as this launcher is not designed to return values.
                Err(RuntimeError::DetailedError(
                    "OnionLambdaRunnableLauncher cannot return values".to_string(),
                ))
            }
            StepResult::ReplaceRunnable(_) => {
                // This should not happen, as this launcher is not designed to replace runnables.
                Err(RuntimeError::DetailedError(
                    "OnionLambdaRunnableLauncher cannot replace runnables".to_string(),
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
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> Result<StepResult, RuntimeError> {
        // 阶段一：执行参数约束 (如果存在)
        // 如果 `constrain_runnable` (通常是一个由 LazySet 的 filter 创建的 Lambda 启动器) 存在，
        // 意味着当前正在处理的参数有一个关联的约束需要被满足。
        if let Some(constrain_runnable) = self.constrain_runnable.as_mut() {
            // 执行约束可运行对象的 `step` 方法
            match constrain_runnable.step(gc)? {
                // 约束尚未完成，需要继续执行。启动器也返回 Continue。
                StepResult::Continue => {
                    return Ok(StepResult::Continue);
                }
                // 约束执行过程中不应该产生新的可运行对象或替换自身。
                StepResult::NewRunnable(_) => {
                    return Err(RuntimeError::DetailedError(
                        "OnionLambdaRunnableLauncher's constrain_runnable cannot yield new runnables".to_string(),
                    ));
                }
                StepResult::ReplaceRunnable(_) => {
                    return Err(RuntimeError::DetailedError(
                        "OnionLambdaRunnableLauncher's constrain_runnable cannot replace runnables"
                            .to_string(),
                    ));
                }
                // 约束执行完成并返回了一个值。
                // 约束的返回值约定为一个 Pair：(布尔值表示是否panic, 布尔值表示约束是否通过)
                StepResult::Return(v) => {
                    // 解析约束的返回值
                    v.weak().try_borrow()?.with_data(|v_obj| {
                        // 约束的返回值必须是一个 Pair 对象
                        let OnionObject::Pair(constrain_result_pair) = v_obj else {
                            // 如果不是 Pair，则认为约束失败（或者约束实现有误）
                            return Err(RuntimeError::DetailedError(
                                "Constrain runnable did not return a Pair object".to_string(),
                            ));
                        };

                        // 第一个元素 (key) 表示约束执行是否 panic
                        match constrain_result_pair.get_key().try_borrow()?.to_boolean() {
                            Ok(true) => {
                                // true 表示约束执行没有 panic
                                // 第二个元素 (value) 表示约束是否通过
                                if constrain_result_pair
                                    .get_value()
                                    .try_borrow()?
                                    .to_boolean()?
                                {
                                    // 约束通过，清除 `constrain_runnable`，准备处理下一个参数或阶段。
                                    self.constrain_runnable = None;
                                    // 注意：这里原代码是 `return Ok(())`，这不符合 `step` 的签名。
                                    // 应该返回 `Ok(StepResult::Continue)` 以便继续处理参数。
                                    // 假设这是期望的行为，即约束成功后，继续当前 `step` 的后续逻辑。
                                    // 如果这里直接返回，则当前 `step` 的参数处理逻辑会被跳过。
                                    // 为了与原逻辑最接近（即清除约束后继续本轮 step 的后续参数处理），
                                    // 我们不在这里 `return`，而是让代码流继续到下面的 `match self.phase`。
                                    // 如果期望的是约束成功后立即开始下一轮 `step`，则应 `return Ok(StepResult::Continue)`。
                                    // 鉴于后续代码会继续处理参数，这里不返回是合理的。
                                    Ok(()) // 标记约束已处理，但不立即返回，让后续的 phase match 执行
                                } else {
                                    // 约束未通过 (返回 false)
                                    return Err(RuntimeError::DetailedError(
                                        "Argument constraint failed".to_string(),
                                    ));
                                }
                            }
                            Ok(false) => {
                                // false 表示约束执行过程中发生了 panic
                                return Err(RuntimeError::CustomValue(
                                    constrain_result_pair
                                        .get_value()
                                        .try_borrow()?
                                        .clone()
                                        .stabilize(),
                                ));
                            }
                            Err(err) => return Err(err), // 转换布尔值失败
                        }
                    })?;
                    // 如果约束成功并通过 (上面返回 Ok(()) 但没有实际 return)，则会继续到下面的 phase 处理。
                    // 如果约束失败或 panic (上面返回 Err)，则整个 step 会在这里结束。
                }
            }
        } // 结束 `if let Some(constrain_runnable)`

        // 阶段二：根据当前处理阶段 (phase) 处理参数
        match self.phase {
            // 阶段 2.1: 处理命名参数
            ArgumentProcessingPhase::NamedArguments => {
                // 如果当前参数索引超出了提供的参数列表的范围，
                // 说明所有提供的参数都已在命名参数阶段被初步检查过。
                // 切换到位置参数处理阶段。
                if self.current_argument_index >= self.argument_elements.len() {
                    self.phase = ArgumentProcessingPhase::PositionalArguments;
                    self.current_argument_index = 0; // 重置索引以供位置参数阶段使用
                    return Ok(StepResult::Continue); // 请求调度器再次调用 step
                }

                // 获取当前正在处理的由调用者提供的参数
                let current_provided_arg_static =
                    &self.argument_elements[self.current_argument_index];
                self.current_argument_index += 1; // 移动到下一个提供的参数

                // 尝试借用当前提供的参数对象
                let arg_obj_view = current_provided_arg_static.weak().try_borrow()?;
                // 检查当前提供的参数是否是命名参数 (`OnionObject::Named`)
                if let OnionObject::Named(named_arg) = &*arg_obj_view {
                    let key_to_match = &named_arg.key; // 获取命名参数的名称

                    // 遍历 Lambda 定义中的参数 (`parameter_elements`)，尝试匹配名称
                    for param_idx in 0..self.parameter_elements.len() {
                        // 如果此参数槽已经被赋值，则跳过 (避免一个定义参数被多个命名参数赋值)
                        // 注意：原代码没有这个检查，一个定义参数可以被后来的同名参数覆盖。
                        // if self.assigned[param_idx] { continue; }

                        let param_element_static = &self.parameter_elements[param_idx];
                        let param_element_view = param_element_static.weak().try_borrow()?;

                        match &*param_element_view {
                            // 情况 A: Lambda 定义的参数也是一个命名参数 (`name: Type`)
                            OnionObject::Named(param_named_def) => {
                                if param_named_def.key.equals(key_to_match)? {
                                    // 名称匹配成功！
                                    // 将提供的命名参数存入 `collected_arguments` 的对应位置。
                                    self.collected_arguments[param_idx] =
                                        current_provided_arg_static.clone();
                                    self.assigned[param_idx] = true; // 标记此参数槽已分配
                                    return Ok(StepResult::Continue); // 处理了一个参数，继续下一步
                                }
                            }
                            // 情况 B: Lambda 定义的参数是一个 LazySet (`name: {constraint}`)
                            // LazySet 的 container 存储了参数名和类型，filter 存储了约束。
                            OnionObject::LazySet(lazy_set) => {
                                // 检查 LazySet 的 container 是否为 Named 对象，并匹配名称
                                // `with_data` 用于安全访问 OnionObject 内部数据
                                let matched_in_lazyset =
                                    lazy_set.container.with_data(|container| {
                                        if let OnionObject::Named(container_named) = container {
                                            if container_named.key.equals(key_to_match)? {
                                                // 名称匹配成功！
                                                // 将提供的命名参数（其值部分）存入 `collected_arguments`。
                                                // 注意：这里存的是整个 `current_provided_arg_static` (即 `name: value` 对)
                                                self.collected_arguments[param_idx] =
                                                    current_provided_arg_static.clone();
                                                self.assigned[param_idx] = true; // 标记已分配

                                                // 设置约束：将 LazySet 的 filter (一个 Lambda) 设置为 `constrain_runnable`。
                                                // 约束 Lambda 的参数是当前提供的命名参数的值。
                                                let argument_for_filter =
                                                    OnionObject::Tuple(OnionTuple::new(vec![
                                                        named_arg.get_value().clone(), // 提取命名参数的值
                                                    ]))
                                                    .stabilize(); // 创建包含单个参数的元组

                                                // 为约束 Lambda 创建一个新的启动器
                                                let runnable = Box::new(
                                                    OnionLambdaRunnableLauncher::new_static(
                                                        &lazy_set.filter.clone().stabilize(), // 约束 Lambda
                                                        &argument_for_filter, // 约束 Lambda 的参数
                                                        |r| Ok(r), // 简单的 runnable_mapper
                                                    )?,
                                                );
                                                // 将约束启动器包装在 Scheduler 中并设置为 `constrain_runnable`
                                                self.constrain_runnable =
                                                    Some(Box::new(Scheduler::new(vec![runnable])));
                                                return Ok(true); // 表示在 LazySet 中找到了匹配
                                            }
                                        }
                                        Ok(false) // 在 LazySet 的 container 中未找到匹配的 Named 对象或名称不匹配
                                    })?;
                                if matched_in_lazyset {
                                    return Ok(StepResult::Continue); // 约束已设置，等待下一轮 step 执行约束
                                }
                            }
                            _ => {} // Lambda 定义的参数不是 Named 或 LazySet，在命名参数阶段不直接匹配
                        }
                    } // 结束参数定义遍历

                    // 如果遍历完所有 Lambda 定义的参数后，没有找到匹配的名称，
                    // 说明这是一个额外的命名参数 (例如，用于可变参数或动态属性)。
                    // 将其追加到 `collected_arguments` 的末尾。
                    self.collected_arguments
                        .push(current_provided_arg_static.clone());
                    self.assigned.push(true); // 标记这个追加的参数也已分配
                }
                // 如果当前提供的参数不是 OnionObject::Named，则在命名参数阶段被忽略。
                // （例如，一个直接的位置参数混在命名参数列表中间，它会被留到位置参数阶段处理）
                Ok(StepResult::Continue) // 无论是否处理了命名参数，都继续下一步
            }

            // 阶段 2.2: 处理位置参数
            ArgumentProcessingPhase::PositionalArguments => {
                // 如果当前参数索引超出了提供的参数列表的范围，
                // 说明所有提供的参数都已在位置参数阶段被处理。
                // 切换到完成阶段。
                if self.current_argument_index >= self.argument_elements.len() {
                    self.phase = ArgumentProcessingPhase::Done;
                    return Ok(StepResult::Continue); // 请求调度器再次调用 step
                }

                // 获取当前正在处理的由调用者提供的参数
                let current_provided_arg_static =
                    &self.argument_elements[self.current_argument_index];
                self.current_argument_index += 1; // 移动到下一个提供的参数

                let arg_obj_view = current_provided_arg_static.weak().try_borrow()?;
                // 如果当前提供的参数是命名参数，则在位置参数阶段跳过。
                // (命名参数应该已经在 NamedArguments 阶段处理完毕，或者作为额外参数追加了)
                if let OnionObject::Named(_) = &*arg_obj_view {
                    // Skip named arguments in this phase.
                } else {
                    // 当前提供的是一个位置参数。
                    // 尝试找到第一个尚未被赋值的 Lambda 定义参数槽。
                    if let Some(param_idx) = self.assigned.iter().position(|&assigned| !assigned) {
                        // 找到了一个未分配的参数槽。
                        let original_param_def_static = &self.parameter_elements[param_idx];
                        let original_param_obj_view =
                            original_param_def_static.weak().try_borrow()?;

                        match &*original_param_obj_view {
                            // 情况 A: Lambda 定义的参数是 `name: Type` (Named)
                            // 此时，位置参数将被用于填充这个具名参数的值。
                            OnionObject::Named(original_named_def) => {
                                // 将位置参数包装成一个新的 Named 对象，使用原始定义的名称。
                                let new_value_for_slot = OnionObject::Named(OnionNamed::new(
                                    original_named_def.key.as_ref().clone(), // 使用原始参数定义的名称
                                    current_provided_arg_static.weak().clone(), // 值是当前的位置参数
                                ))
                                .stabilize();
                                self.collected_arguments[param_idx] = new_value_for_slot;
                                self.assigned[param_idx] = true;
                            }
                            // 情况 B: Lambda 定义的参数是 `name: {constraint}` (LazySet)
                            OnionObject::LazySet(lazy_set_def) => {
                                // 检查 LazySet 的 container 是否为 Named 对象，以获取参数名。
                                lazy_set_def.container.with_data(|container_obj| {
                                    if let OnionObject::Named(container_named_def) = container_obj {
                                        // 将位置参数包装成 Named 对象，使用 LazySet container 中定义的名称。
                                        let new_value_for_slot =
                                            OnionObject::Named(OnionNamed::new(
                                                container_named_def.key.as_ref().clone(),
                                                current_provided_arg_static.weak().clone(),
                                            ))
                                            .stabilize();
                                        self.collected_arguments[param_idx] = new_value_for_slot;
                                        self.assigned[param_idx] = true;

                                        // 设置约束：将 LazySet 的 filter 设置为 `constrain_runnable`。
                                        // 约束 Lambda 的参数是当前提供的位置参数本身。
                                        let argument_for_filter =
                                            OnionObject::Tuple(OnionTuple::new(vec![
                                                current_provided_arg_static.weak().clone(),
                                            ]))
                                            .stabilize();
                                        let runnable =
                                            Box::new(OnionLambdaRunnableLauncher::new_static(
                                                &lazy_set_def.filter.clone().stabilize(),
                                                &argument_for_filter,
                                                |r| Ok(r),
                                            )?);
                                        self.constrain_runnable =
                                            Some(Box::new(Scheduler::new(vec![runnable])));
                                        return Ok(()); // 成功设置约束
                                    }
                                    // 如果 LazySet 的 container 不是 Named，则无法确定参数名，这是一个错误。
                                    Err(RuntimeError::DetailedError(
                                        // 错误信息 "Constant's container must be a Named object" 似乎有误，应为 "LazySet's container..."
                                        "LazySet's container must be a Named object for positional assignment".to_string(),
                                    ))
                                })?;
                            }
                            // 情况 C: Lambda 定义的参数是普通类型 (不是 Named 或 LazySet)
                            // 直接将位置参数赋值给它。
                            _ => {
                                self.collected_arguments[param_idx] =
                                    current_provided_arg_static.clone();
                                self.assigned[param_idx] = true;
                            }
                        }
                    } else {
                        // 所有 Lambda 定义的参数槽都已被填充，或者 Lambda 本身没有参数定义。
                        // 将此位置参数作为额外参数追加到 `collected_arguments`。
                        // (用于支持可变参数列表等情况)
                        self.collected_arguments
                            .push(current_provided_arg_static.clone());
                        self.assigned.push(true); // 标记这个追加的参数也已分配
                    }
                }
                Ok(StepResult::Continue) // 处理了一个参数或跳过了一个参数，继续下一步
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
                    return Ok(StepResult::Continue);
                }

                // 所有参数都已收集完毕，并且所有约束（如果有的话）都已满足。
                // 使用 `collected_arguments` 创建最终的参数元组。
                let final_args_static =
                    OnionTuple::new_static_no_ref(self.collected_arguments.clone());

                // 获取原始 Lambda 定义对象。
                let lambda_arc = self.lambda.weak().try_borrow()?.with_data(|obj| {
                    if let OnionObject::Lambda(lambda_def) = obj {
                        Ok(lambda_def.clone())
                    } else {
                        // 这是一个内部错误，启动器持有的 lambda 对象不是 Lambda 类型。
                        Err(RuntimeError::DetailedError(
                            "Launcher's lambda object is not OnionObject::Lambda".to_string(),
                        ))
                    }
                })?;

                // 使用最终的参数元组和 Lambda 定义来创建实际的 Lambda 可运行实例。
                let runnable = lambda_arc
                    .create_runnable(final_args_static, &self.lambda, gc)
                    .map_err(|e| {
                        RuntimeError::InvalidType(format!(
                            "Failed to create runnable from lambda: {}",
                            e
                        ))
                    })?;

                // 获取 `runnable_mapper`。这是一个一次性的闭包，
                // 用于在启动器完成其工作后，对新创建的 Lambda 可运行对象进行可能的转换或包装。
                let mapper = self.runnable_mapper.take().ok_or_else(|| {
                    RuntimeError::DetailedError(
                        "Runnable mapper has already been consumed or was never set".to_string(),
                    )
                })?;

                // 应用 mapper。
                mapper(runnable)
                    .map(|result_runnable| StepResult::ReplaceRunnable(result_runnable)) // 成功映射，返回 ReplaceRunnable
                    .map_err(|e| {
                        // mapper 执行失败
                        RuntimeError::DetailedError(format!("Failed to map runnable: {}", e))
                    })
            }
        }
    }

    fn copy(&self, _gc: &mut GC<OnionObjectCell>) -> Box<dyn Runnable> {
        // 由于 `runnable_mapper` 是 `FnOnce`，它只能被调用一次，因此启动器不能被安全地复制。
        panic!(
            "OnionLambdaRunnableLauncher cannot be copied due to FnOnce closure in runnable_mapper"
        )
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        // 此启动器不提供上下文格式化功能。
        Err(RuntimeError::DetailedError(
            "OnionLambdaRunnableLauncher does not support context formatting".to_string(),
        ))
    }
}
