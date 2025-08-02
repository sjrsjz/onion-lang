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

#[allow(unused)]
pub struct OnionLambdaRunnableLauncher {
    lambda: OnionStaticObject, // The OnionObject::Lambda itself
    lambda_ref: Arc<OnionLambdaDefinition>,
    lambda_self_object: OnionStaticObject,

    argument: OnionStaticObject, // hold the refs for flatten_argument
    flatten_argument: Vec<OnionObject>,

    string_pool: OnionKeyPool<String>,
    current_argument_index: usize, // Index into argument_elements for current phase

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
        let key_pool = lambda_ref.create_key_pool();

        let flatten_argument = lambda_ref
            .get_parameter()
            .unpack_arguments(argument.weak())?;

        Ok(Self {
            lambda: lambda.stabilize(),
            lambda_ref: lambda_ref.clone(),
            lambda_self_object: self_object.stabilize(),
            argument,
            flatten_argument,
            string_pool: key_pool.clone(),
            current_argument_index: 0,
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
        if self.current_argument_index == self.lambda_ref.get_flatten_param_keys().len() {
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

            let mapped = unwrap_step_result!((self.runnable_mapper)(runnable));
            return StepResult::ReplaceRunnable(mapped);
        }

        let mut index = self.current_argument_index;
        while index < self.lambda_ref.get_flatten_param_keys().len() {
            match &self.lambda_ref.get_flatten_param_constraints()[index] {
                OnionObject::Boolean(v) => {
                    if !*v {
                        self.current_argument_index = index + 1;
                        return StepResult::Error(RuntimeError::InvalidOperation(
                            "Constraint check failed".to_string().into(),
                        ));
                    }
                }
                lambda @ OnionObject::Lambda(_) => {
                    self.current_argument_index = index + 1;
                    return StepResult::NewRunnable(Box::new(unwrap_step_result!(
                        OnionLambdaRunnableLauncher::new_static(
                            lambda,
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
