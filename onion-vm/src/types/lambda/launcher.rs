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
    lambda: OnionStaticObject,         // The OnionObject::Lambda itself
    argument_tuple: OnionStaticObject, // The provided argument tuple object

    collected_arguments: Vec<OnionStaticObject>, // Arguments collected during processing
    assigned: Vec<bool>, // Tracks assignment for parameter slots, then for appended args

    phase: ArgumentProcessingPhase,
    current_argument_index: usize, // Index into argument_elements for current phase

    runnable_mapper: &'static dyn Fn(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError>,

    constrain_runnable: Option<Box<dyn Runnable>>,
}

impl OnionLambdaRunnableLauncher {
    pub fn new_static<F: 'static>(
        lambda_obj: &OnionStaticObject,
        argument_tuple_obj: &OnionStaticObject,
        runnable_mapper: &'static F,
    ) -> Result<OnionLambdaRunnableLauncher, RuntimeError>
    where
        F: Fn(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError>,
    {
        // Initialize collected_arguments based on parameter count
        let mut collected_arguments = Vec::new();
        let mut assigned = Vec::new();

        lambda_obj.weak().try_borrow()?.with_data(|obj_ref| {
            if let OnionObject::Lambda(definition) = obj_ref {
                definition.parameter.try_borrow()?.with_data(|p_obj| {
                    if let OnionObject::Tuple(tuple) = p_obj {
                        // Initialize collected_arguments and assigned based on parameter count
                        for param in &tuple.elements {
                            param.with_data(|param_obj| {
                                match param_obj {
                                    OnionObject::LazySet(lazy_set) => {
                                        // If the parameter is a LazySet, we collect its container.
                                        collected_arguments
                                            .push(lazy_set.get_container().clone().stabilize());
                                    }
                                    _ => {
                                        // For other types, we just clone the parameter as is.
                                        collected_arguments.push(param.clone().stabilize());
                                    }
                                }
                                Ok(())
                            })?;
                        }
                        assigned = vec![false; tuple.elements.len()];
                        Ok(())
                    } else {
                        Err(RuntimeError::DetailedError(
                            "Lambda parameters must be a Tuple".to_string(),
                        ))
                    }
                })
            } else {
                Err(RuntimeError::DetailedError(
                    "Expected a Lambda definition object".to_string(),
                ))
            }
        })?;

        Ok(OnionLambdaRunnableLauncher {
            lambda: lambda_obj.clone(),
            argument_tuple: argument_tuple_obj.clone(),
            collected_arguments,
            assigned,
            phase: ArgumentProcessingPhase::NamedArguments,
            current_argument_index: 0,
            runnable_mapper: runnable_mapper,
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
                                    // 如果期望的是约束成功后立即开始下一轮 `step`，則應 `return Ok(StepResult::Continue)`。
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
                // Get argument count using with_data to avoid storing Vec
                let argument_count =
                    self.argument_tuple
                        .weak()
                        .try_borrow()?
                        .with_data(|arg_obj| {
                            if let OnionObject::Tuple(tuple) = arg_obj {
                                Ok(tuple.elements.len())
                            } else {
                                Err(RuntimeError::DetailedError(
                                    "Lambda arguments must be a Tuple".to_string(),
                                ))
                            }
                        })?;

                // 如果当前参数索引超出了提供的参数列表的范围，
                // 说明所有提供的参数都已在命名参数阶段被初步检查过。
                // 切换到位置参数处理阶段。
                if self.current_argument_index >= argument_count {
                    self.phase = ArgumentProcessingPhase::PositionalArguments;
                    self.current_argument_index = 0; // 重置索引以供位置参数阶段使用
                    return Ok(StepResult::Continue); // 请求调度器再次调用 step
                }

                // 获取当前正在处理的由调用者提供的参数
                let current_arg_index = self.current_argument_index;
                self.current_argument_index += 1; // 移动到下一个提供的参数

                self
                    .argument_tuple
                    .weak()
                    .try_borrow()?
                    .with_data(|arg_obj| {
                        if let OnionObject::Tuple(tuple) = arg_obj {
                            if current_arg_index < tuple.elements.len() {
                                let arg_obj_view = tuple.elements[current_arg_index].try_borrow()?;
                // 检查当前提供的参数是否是命名参数 (`OnionObject::Named`)
                if let OnionObject::Named(named_arg) = &*arg_obj_view {
                    let key_to_match = &named_arg.key; // 获取命名参数的名称

                    // 遍历 Lambda 定义中的参数，使用 with_data 访问
                    let parameter_count =
                        self.lambda.weak().try_borrow()?.with_data(|obj_ref| {
                            if let OnionObject::Lambda(definition) = obj_ref {
                                definition.parameter.try_borrow()?.with_data(|p_obj| {
                                    if let OnionObject::Tuple(tuple) = p_obj {
                                        Ok(tuple.elements.len())
                                    } else {
                                        Err(RuntimeError::DetailedError(
                                            "Lambda parameters must be a Tuple".to_string(),
                                        ))
                                    }
                                })
                            } else {
                                Err(RuntimeError::DetailedError(
                                    "Expected a Lambda definition object".to_string(),
                                ))
                            }
                        })?;

                    for param_idx in 0..parameter_count {
                        // 使用 with_data 访问参数定义
                        let matched = self.lambda.weak().try_borrow()?.with_data(|obj_ref| {
                            if let OnionObject::Lambda(definition) = obj_ref {
                                definition.parameter.try_borrow()?.with_data(|p_obj| {
                                    if let OnionObject::Tuple(tuple) = p_obj {
                                        if param_idx < tuple.elements.len() {
                                            tuple.elements[param_idx].with_data(|param_obj| {
                                                match param_obj {
                                                    // 情况 A: Lambda 定义的参数也是一个命名参数 (`name: Type`)
                                                    OnionObject::Named(param_named_def) => {
                                                        if param_named_def.key.equals(key_to_match)? {
                                                            // 名称匹配成功！
                                                            self.collected_arguments[param_idx] = arg_obj_view.clone().stabilize();
                                                            self.assigned[param_idx] = true;
                                                            return Ok(true);
                                                        }
                                                        Ok(false)
                                                    }
                                                    // 情况 B: Lambda 定义的参数是一个 LazySet (`name: {constraint}`)
                                                    OnionObject::LazySet(lazy_set) => {
                                                        let matched_in_lazyset = lazy_set.container.with_data(|container| {
                                                            if let OnionObject::Named(container_named) = container {
                                                                if container_named.key.equals(key_to_match)? {
                                                                    // 名称匹配成功！
                                                                    self.collected_arguments[param_idx] = arg_obj_view.clone().stabilize();
                                                                    self.assigned[param_idx] = true;

                                                                    // 设置约束
                                                                    let argument_for_filter = OnionObject::Tuple(OnionTuple::new(vec![
                                                                        named_arg.get_value().clone(),
                                                                    ])).stabilize();

                                                                    let runnable = Box::new(
                                                                        OnionLambdaRunnableLauncher::new_static(
                                                                            &lazy_set.filter.clone().stabilize(),
                                                                            &argument_for_filter,
                                                                            &|r| Ok(r),
                                                                        )?,
                                                                    );
                                                                    self.constrain_runnable = Some(Box::new(Scheduler::new(vec![runnable])));
                                                                    return Ok(true);
                                                                }
                                                            }
                                                            Ok(false)
                                                        })?;
                                                        Ok(matched_in_lazyset)
                                                    }
                                                    _ => Ok(false),
                                                }
                                            })
                                        } else {
                                            Ok(false)
                                        }
                                    } else {
                                        Err(RuntimeError::DetailedError(
                                            "Lambda parameters must be a Tuple".to_string(),
                                        ))
                                    }
                                })
                            } else {
                                Err(RuntimeError::DetailedError(
                                    "Expected a Lambda definition object".to_string(),
                                ))
                            }
                        })?;

                        if matched {
                            return Ok(());
                        }
                    }

                    // 如果遍历完所有 Lambda 定义的参数后，没有找到匹配的名称，
                    // 说明这是一个额外的命名参数。
                    self.collected_arguments
                        .push(arg_obj_view.clone().stabilize());
                    self.assigned.push(true);
                };
                                Ok(())

                            } else {
                                Err(RuntimeError::DetailedError(
                                    "Argument index out of bounds".to_string(),
                                ))
                            }
                        } else {
                            Err(RuntimeError::DetailedError(
                                "Lambda arguments must be a Tuple".to_string(),
                            ))
                        }
                    })?;

                // 如果当前提供的参数不是 OnionObject::Named，则在命名参数阶段被忽略。
                Ok(StepResult::Continue)
            } // 阶段 2.2: 处理位置参数
            ArgumentProcessingPhase::PositionalArguments => {
                // Get argument count using with_data to avoid storing Vec
                let argument_count =
                    self.argument_tuple
                        .weak()
                        .try_borrow()?
                        .with_data(|arg_obj| {
                            if let OnionObject::Tuple(tuple) = arg_obj {
                                Ok(tuple.elements.len())
                            } else {
                                Err(RuntimeError::DetailedError(
                                    "Lambda arguments must be a Tuple".to_string(),
                                ))
                            }
                        })?;

                // 如果当前参数索引超出了提供的参数列表的范围，
                // 说明所有提供的参数都已在位置参数阶段被处理。
                // 切换到完成阶段。
                if self.current_argument_index >= argument_count {
                    self.phase = ArgumentProcessingPhase::Done;
                    return Ok(StepResult::Continue); // 请求调度器再次调用 step
                }

                // 获取当前正在处理的由调用者提供的参数
                let current_processing_arg_idx = self.current_argument_index; // MODIFIED: was current_arg_index
                self.current_argument_index += 1; // 移动到下一个提供的参数

                // Access the argument tuple and the specific argument within this closure
                self.argument_tuple.weak().try_borrow()?.with_data(|arg_tuple_obj| {
                    let tuple_elements = if let OnionObject::Tuple(t) = arg_tuple_obj {
                        &t.elements
                    } else {
                        return Err(RuntimeError::DetailedError("Lambda arguments must be a Tuple".to_string()));
                    };

                    if current_processing_arg_idx >= tuple_elements.len() {
                        // This should ideally be caught by the argument_count check earlier,
                        // but as a safeguard within the closure.
                        return Err(RuntimeError::DetailedError("Argument index out of bounds during positional processing".to_string()));
                    }

                    // Get a borrowed view of the current argument from the argument tuple
                    let current_provided_arg_weak = &tuple_elements[current_processing_arg_idx];
                    let current_provided_arg_view = current_provided_arg_weak.try_borrow()?; // This is GcRef<OnionObjectCell>

                    // 如果当前提供的参数是命名参数，则在位置参数阶段跳过。
                    if let OnionObject::Named(_) = &*current_provided_arg_view {
                        // Skip named arguments in this phase.
                    } else {
                        // This is a positional argument.
                        // We'll need its OnionStaticObject form for assignments or creating new objects.
                        // We clone the weak reference and stabilize it.
                        let current_provided_arg_static = current_provided_arg_weak.clone().stabilize();

                        // 尝试找到第一个尚未被赋值的 Lambda 定义参数槽。
                        if let Some(param_idx) = self.assigned.iter().position(|&assigned| !assigned) {
                            // 找到了一个未分配的参数槽。 Access the lambda's parameter definition.
                            self.lambda.weak().try_borrow()?.with_data(|lambda_obj_ref| {
                                let lambda_def = if let OnionObject::Lambda(def) = lambda_obj_ref { def }
                                else { return Err(RuntimeError::DetailedError("Expected a Lambda definition object".to_string())); };

                                lambda_def.parameter.try_borrow()?.with_data(|param_tuple_obj_ref| {
                                    let param_tuple_elements = if let OnionObject::Tuple(pt) = param_tuple_obj_ref { &pt.elements }
                                    else { return Err(RuntimeError::DetailedError("Lambda parameters must be a Tuple".to_string())); };

                                    if param_idx >= param_tuple_elements.len() {
                                        return Err(RuntimeError::DetailedError("Parameter index out of bounds for assignment".to_string()));
                                    }

                                    param_tuple_elements[param_idx].with_data(|param_def_obj_ref| {
                                        match param_def_obj_ref {
                                            // 情况 A: Lambda 定义的参数是 `name: Type` (Named)
                                            OnionObject::Named(original_named_def) => {
                                                // 将位置参数包装成一个新的 Named 对象，使用原始定义的名称。
                                                // For the value of the new Named object, we use the weak reference of the provided argument.
                                                let new_value_for_slot = OnionObject::Named(OnionNamed::new(
                                                    original_named_def.key.as_ref().clone(),
                                                    current_provided_arg_weak.clone(), // Use weak ref here
                                                )).stabilize();
                                                self.collected_arguments[param_idx] = new_value_for_slot;
                                                self.assigned[param_idx] = true;
                                                Ok(())
                                            }
                                            // 情况 B: Lambda 定义的参数是 `name: {constraint}` (LazySet)
                                            OnionObject::LazySet(lazy_set_def) => {
                                                lazy_set_def.container.with_data(|container_obj_ref| {
                                                    if let OnionObject::Named(container_named_def) = container_obj_ref {
                                                        // 将位置参数包装成 Named 对象
                                                        let new_value_for_slot = OnionObject::Named(OnionNamed::new(
                                                            container_named_def.key.as_ref().clone(),
                                                            current_provided_arg_weak.clone(), // Use weak ref here
                                                        )).stabilize();
                                                        self.collected_arguments[param_idx] = new_value_for_slot;
                                                        self.assigned[param_idx] = true;

                                                        // 设置约束
                                                        // The argument to the filter lambda is the provided argument itself.
                                                        let argument_for_filter = OnionObject::Tuple(OnionTuple::new(vec![
                                                            current_provided_arg_weak.clone(), // Use weak ref here
                                                        ])).stabilize();
                                                        let runnable = Box::new(OnionLambdaRunnableLauncher::new_static(
                                                            &lazy_set_def.filter.clone().stabilize(),
                                                            &argument_for_filter,
                                                            &|r| Ok(r),
                                                        )?);
                                                        self.constrain_runnable = Some(Box::new(Scheduler::new(vec![runnable])));
                                                        Ok(())
                                                    } else {
                                                        Err(RuntimeError::DetailedError(
                                                            "LazySet\'s container must be a Named object for positional assignment".to_string(),
                                                        ))
                                                    }
                                                }) // End of lazy_set_def.container.with_data
                                            }
                                            // 情况 C: Lambda 定义的参数是普通类型
                                            _ => { 
                                                self.collected_arguments[param_idx] = current_provided_arg_static; // Assign the stabilized positional argument
                                                self.assigned[param_idx] = true;
                                                Ok(())
                                            }
                                        }
                                    }) // End of param_tuple_elements[param_idx].with_data
                                }) // End of lambda_def.parameter.try_borrow()?.with_data
                            })?; // End of self.lambda.weak().try_borrow()?.with_data
                        } else {
                            // 所有 Lambda 定义的参数槽都已被填充。 This is an extra positional argument.
                            self.collected_arguments.push(current_provided_arg_static); // Add the stabilized argument
                            self.assigned.push(true);
                        }
                    }
                    Ok(()) // Return for the main with_data on argument_tuple
                })?; // Propagate error from with_data

                Ok(StepResult::Continue)
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
                self.lambda.weak().try_borrow()?.with_data(|obj| {
                    if let OnionObject::Lambda(lambda_def) = obj {
                        // 使用最终的参数元组和 Lambda 定义来创建实际的 Lambda 可运行实例。
                        let runnable = lambda_def
                            .create_runnable(final_args_static, &self.lambda, gc)
                            .map_err(|e| {
                                RuntimeError::InvalidType(format!(
                                    "Failed to create runnable from lambda: {}",
                                    e
                                ))
                            })?;
                        // 应用 mapper。
                        // 成功映射，返回 ReplaceRunnable
                        (self.runnable_mapper)(runnable)
                            .map(|result_runnable| StepResult::ReplaceRunnable(result_runnable))
                    } else {
                        // 这是一个内部错误，启动器持有的 lambda 对象不是 Lambda 类型。
                        Err(RuntimeError::DetailedError(
                            "Launcher's lambda object is not OnionObject::Lambda".to_string(),
                        ))
                    }
                })
            }
        }
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        // 此启动器不提供上下文格式化功能。
        Err(RuntimeError::DetailedError(
            "OnionLambdaRunnableLauncher does not support context formatting".to_string(),
        ))
    }
}
