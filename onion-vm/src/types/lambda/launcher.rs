use std::sync::Arc;

use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::definition::OnionLambdaDefinition,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    unwrap_object, unwrap_step_result,
    utils::{
        fastmap::{OnionFastMap, OnionKeyPool},
        format_object_summary,
    },
};

#[allow(unused)]
pub struct OnionLambdaRunnableLauncher {
    lambda: OnionStaticObject, // The OnionObject::Lambda itself
    lambda_ref: Arc<OnionLambdaDefinition>,
    lambda_self_object: OnionStaticObject,
    argument: OnionStaticObject,
    string_pool: OnionKeyPool<String>,

    collected_arguments: OnionFastMap<String, OnionStaticObject>, // Arguments collected during processing
    current_argument_index: usize, // Index into argument_elements for current phase
    expect_argument_size: usize,
    pair_to_insert: Option<(usize, OnionStaticObject)>, // If we are processing a pair, this is the key-value pair to insert

    runnable_mapper:
        Arc<dyn Fn(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError> + Sync + Send>,
}

impl OnionLambdaRunnableLauncher {
    // string_pool 是被调用的 Lambda 的所需要的字符串池，这意味着被调用者无法使用除了 string_pool 中的字符串
    // runnable_mapper 是一个函数，用于将生成的 Runnable 进行映射处理
    pub fn new_static<F: Sync + Send + 'static>(
        lambda: &OnionObject,
        argument: OnionStaticObject,
        runnable_mapper: F,
    ) -> Result<OnionLambdaRunnableLauncher, RuntimeError>
    where
        F: Fn(Box<dyn Runnable>) -> Result<Box<dyn Runnable>, RuntimeError> + Sync + Send + 'static,
    {
        let OnionObject::Lambda((lambda_ref, self_object)) = lambda else {
            return Err(RuntimeError::InvalidType(
                "Cannot launch non-lambda object".to_string().into(),
            ));
        };
        let expect_argument_size =
            lambda_ref.with_parameter(|param: &OnionObject| match param {
                OnionObject::Tuple(tuple) => Ok(tuple.get_elements().len()),
                OnionObject::Pair(_) => Ok(1),
                OnionObject::String(_) => Ok(1),
                _ => Err(RuntimeError::InvalidType(
                    "Expect tuple or pair or string for lambda's parameter"
                        .to_string()
                        .into(),
                )),
            })?;
        let key_pool = lambda_ref.create_key_pool();
        Ok(Self {
            lambda: lambda.stabilize(),
            lambda_ref: lambda_ref.clone(),
            lambda_self_object: self_object.stabilize(),
            argument: argument,
            string_pool: key_pool.clone(),
            collected_arguments: OnionFastMap::new(key_pool),
            current_argument_index: 0,
            expect_argument_size: expect_argument_size,
            pair_to_insert: None,
            runnable_mapper: Arc::new(runnable_mapper),
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
            StepResult::Return(constraint_result) => {
                // 我们在这里接收约束求解结果
                if constraint_result.weak().to_boolean()? {
                    if let Some((key, value)) = self.pair_to_insert.take() {
                        self.collected_arguments.push_with_index(key, value);
                    }
                    Ok(())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Constraint check failed".to_string().into(),
                    ))
                }
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

    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        if self.current_argument_index == self.expect_argument_size {
            let runnable = unwrap_step_result!(self.lambda_ref.create_runnable(
                &self.collected_arguments,
                &self.lambda,
                self.lambda_self_object.weak(),
                gc,
            ));

            let mapped = unwrap_step_result!((self.runnable_mapper)(runnable));
            return StepResult::ReplaceRunnable(mapped);
        }

        if self.current_argument_index > self.expect_argument_size {
            return StepResult::Error(RuntimeError::InvalidOperation(
                "Too many arguments provided for lambda".to_string().into(),
            ));
        }

        let result: Result<
            (
                Option<Box<dyn Runnable>>,
                Option<(usize, OnionStaticObject)>,
            ),
            RuntimeError,
        > = self.lambda_ref.with_parameter(|param| {

            // 这里的 keyval 是一个可选的元组，包含了参数的索引和对应的值
            // 键索引是指向被调用者的字符串池中的索引！这意味着如果VM要调用rust函数，rust函数的参数也必须在这个字符串池中
            let mut keyval: Option<(usize, OnionStaticObject)> = None;

            let runnable_result = match param {
                OnionObject::Tuple(params_tuple) => {
                    self.argument.weak().with_data(|argument| {
                        let OnionObject::Tuple(argument_tuple) = argument else {
                            return Err(RuntimeError::InvalidType(
                                "Lambda arguments must be a Tuple when the parameter is a Tuple".to_string().into(),
                            ));
                        };
                        if params_tuple.get_elements().len() != argument_tuple.get_elements().len()
                        {
                            return Err(RuntimeError::InvalidOperation(
                                "Arity Mismatch".to_string().into(),
                            ));
                        }
                        let param_def = params_tuple.get_elements().get(self.current_argument_index)
                            .ok_or_else(|| {
                                RuntimeError::InvalidOperation(
                                    "Internal Error: Parameter index out of bounds"
                                        .to_string()
                                        .into(),
                                )
                            })?;
                        let argument_value = argument_tuple
                            .get_elements()
                            .get(self.current_argument_index)
                            .cloned()
                            .ok_or_else(|| {
                                RuntimeError::InvalidOperation(
                                    "Internal Error: Argument index out of bounds"
                                        .to_string()
                                        .into(),
                                )
                            })?;
                        param_def.with_data(|param_obj| {
                            match param_obj {
                                OnionObject::Pair(param_pair) => {
                                    let param_name = param_pair.get_key().with_data(|o| unwrap_object!(o, OnionObject::String).map(|s| s.as_ref().clone()))?;
                                    let constraint_obj = param_pair.get_value();
                                    let key_index = self.collected_arguments.to_index(&param_name).ok_or_else(|| {
                                        RuntimeError::InvalidOperation(
                                            format!("Cannot find parameter '{}' in string pool", param_name).into(),
                                        )
                                    })?;
                                    keyval = Some((key_index, argument_value.stabilize()));
                                    constraint_obj.with_data(|constraint| match constraint {
                                        OnionObject::Boolean(true) => Ok(None),
                                        OnionObject::Boolean(false) => Err(RuntimeError::InvalidOperation(format!("Constraint check failed for parameter '{}'", param_name).into())),
                                        lambda @ OnionObject::Lambda(_) => {
                                            let launcher = OnionLambdaRunnableLauncher::new_static(
                                                &lambda,
                                                argument_value.stabilize(),
                                                |r| Ok(r)
                                            )?;
                                            Ok(Some(Box::new(launcher) as Box<dyn Runnable>))
                                        }
                                        _ => Err(RuntimeError::InvalidType(format!("Invalid constraint type for parameter '{}'. Expected Boolean or Lambda.", param_name).into())),
                                    })
                                }
                                OnionObject::String(param_name) => {
                                    let key_index = self.collected_arguments.to_index(param_name.as_ref()).ok_or_else(|| {
                                        RuntimeError::InvalidOperation(
                                            format!("Cannot find parameter '{}' in string pool", param_name).into(),
                                        )
                                    })?;
                                    keyval = Some((key_index, argument_value.stabilize()));
                                    // Implicit `true` constraint means no runnable is needed.
                                    Ok(None)
                                }
                                _ => Err(RuntimeError::InvalidType("Lambda parameter definition inside a tuple must be a String or a Pair".to_string().into())),
                            }
                        })
                    })
                }
                OnionObject::Pair(single_param) => {
                    let key = single_param.get_key().with_data(|o| {
                        unwrap_object!(o, OnionObject::String).map(|s| self.collected_arguments.to_index(s.as_ref()).ok_or_else(|| {
                            RuntimeError::InvalidOperation(
                                format!("Cannot find parameter '{}' in string pool", s.as_ref()).into(),
                            )
                        }))
                    })??;
                    let v = single_param.get_value();

                    // Assign to the local keyval.
                    keyval = Some((key, self.argument.clone()));

                    v.with_data(|constraint| match constraint {
                        OnionObject::Boolean(true) => Ok(None),
                        OnionObject::Boolean(false) => Err(RuntimeError::InvalidOperation(
                            "Constraint check failed".to_string().into(),
                        )),
                        lambda @ OnionObject::Lambda(_) => {
                            let launcher = OnionLambdaRunnableLauncher::new_static(
                                &lambda,
                                self.argument.clone(),
                                |r| Ok(r),
                            )?;
                            Ok(Some(Box::new(launcher) as Box<dyn Runnable>))
                        }
                        _ => Err(RuntimeError::InvalidType(
                            "Invalid constraint type".to_string().into(),
                        )),
                    })
                }
                OnionObject::String(single_param) => {
                    let key = self.collected_arguments.to_index(single_param.as_ref()).ok_or_else(|| {
                        RuntimeError::InvalidOperation(
                            format!("Cannot find parameter '{}' in string pool", single_param.as_ref()).into(),
                        )
                    })?;
                    // Assign to the local keyval.
                    keyval = Some((key, self.argument.clone()));
                    Ok(None)
                }
                _ => Err(RuntimeError::InvalidType(
                    "Expect tuple or pair for lambda's parameter"
                        .to_string()
                        .into(),
                )),
            };

            // Return the tuple containing both the runnable and the keyval.
            runnable_result.map(|runnable| (runnable, keyval))
        });

        // Unpack the result tuple here.
        let (constraint_runnable, returned_keyval) = unwrap_step_result!(result);
        self.current_argument_index += 1;

        match constraint_runnable {
            Some(runnable) => {
                // If there's a runnable, we need to defer insertion.
                self.pair_to_insert = returned_keyval;
                StepResult::NewRunnable(runnable)
            }
            None => {
                // No runnable, so we can process the argument immediately.
                if let Some((key, value)) = returned_keyval {
                    self.collected_arguments.push_with_index(key, value);
                }
                StepResult::Continue
            }
        }
    }

    fn format_context(&self) -> String {
        "-> At lambda runnable launcher".to_string()
            + &format!(
                " (current index: {}, expected: {})",
                self.current_argument_index, self.expect_argument_size
            )
            + &format!(", lambda: {}", format_object_summary(self.lambda.weak()))
    }
}
