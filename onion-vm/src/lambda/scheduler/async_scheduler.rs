use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        object::{OnionObject, OnionObjectCell},
        pair::OnionPair,
    },
};

pub struct AsyncScheduler {
    pub(crate) runnables: Vec<Box<dyn Runnable>>,
}

impl AsyncScheduler {
    pub fn new(runnables: Vec<Box<dyn Runnable>>) -> Self {
        AsyncScheduler { runnables }
    }
}

impl Runnable for AsyncScheduler {
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> Result<StepResult, RuntimeError> {
        if self.runnables.is_empty() {
            return Ok(StepResult::Return(Box::new(OnionPair::new_static(
                &OnionObject::Boolean(true).stabilize(),
                &OnionObject::Undefined(Some("All runnables completed".to_string())).stabilize(),
            ))));
        }

        let mut i = 0;
        while i < self.runnables.len() {
            match self.runnables[i].step(gc) {
                Ok(step_result) => {
                    match step_result {
                        StepResult::Continue => {
                            i += 1; // 继续处理下一个runnable
                        }
                        StepResult::NewRunnable(new_runnable) => {
                            self.runnables.push(new_runnable);
                            self.runnables[i].receive(
                                StepResult::Return(Box::new(
                                    OnionObject::Undefined(Some("Task Launched".to_string()))
                                        .stabilize(),
                                )),
                                gc,
                            )?;
                            i += 1; // 继续处理下一个runnable
                        }
                        StepResult::ReplaceRunnable(new_runnable) => {
                            // self.runnables.push(new_runnable);
                            // self.runnables[i].receive(
                            //     StepResult::Return(
                            //         OnionObject::Undefined(Some("Task Launched".to_string())).stabilize(),
                            //     ),
                            //     gc,
                            // )?;
                            // i += 1; // 继续处理下一个runnable
                            self.runnables[i] = new_runnable;
                            i += 1; // 继续处理下一个runnable
                        }
                        StepResult::Return(_) => {
                            // 移除已完成的runnable
                            self.runnables.remove(i);
                        }
                    }
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        // 如果所有runnable都需要继续，返回Continue
        Ok(StepResult::Continue)
    }
    fn receive(
        &mut self,
        _step_result: StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "AsyncScheduler does not support receive".to_string(),
        ))
    }

    fn copy(&self) -> Box<dyn Runnable> {
        Box::new(AsyncScheduler {
            runnables: self.runnables.iter().map(|r| r.copy()).collect(),
        })
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        let mut tasks_json_array = serde_json::Value::Array(vec![]);
        for runnable in &self.runnables {
            let frame_json = runnable.format_context()?;
            tasks_json_array.as_array_mut().unwrap().push(frame_json);
        }
        // {type: "AsyncScheduler", tasks: tasks_json_array}
        Ok(serde_json::json!({
            "type": "AsyncScheduler",
            "tasks": tasks_json_array
        }))
    }
}
