use std::sync::Arc;

use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        object::{OnionObject, OnionObjectCell},
        pair::OnionPair,
    },
};

pub struct Scheduler {
    pub(crate) runnable_stack: Vec<Box<dyn Runnable>>,
}

impl Scheduler {
    pub fn new(runnable_stack: Vec<Box<dyn Runnable>>) -> Self {
        Scheduler { runnable_stack }
    }
}

impl Runnable for Scheduler {
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        if let Some(runnable) = self.runnable_stack.last_mut() {
            match runnable.step(gc) {
                StepResult::Continue => StepResult::Continue,
                v @ StepResult::SpawnRunnable(_) => return v,
                StepResult::NewRunnable(new_runnable) => {
                    self.runnable_stack.push(new_runnable);
                    StepResult::Continue
                }
                StepResult::ReplaceRunnable(new_runnable) => {
                    self.runnable_stack.last_mut().map(|r| *r = new_runnable);
                    StepResult::Continue
                }
                StepResult::Return(ref result) => {
                    self.runnable_stack.pop();
                    if let Some(top_runnable) = self.runnable_stack.last_mut() {
                        match top_runnable.receive(&StepResult::Return(result.clone()), gc) {
                            Ok(_) => {}
                            Err(RuntimeError::CustomValue(ref e)) => {
                                return StepResult::Return(
                                    OnionPair::new_static(
                                        &OnionObject::Boolean(false).stabilize(),
                                        &e,
                                    )
                                    .into(),
                                )
                            }
                            Err(e) => {
                                return StepResult::Return(
                                    OnionPair::new_static(
                                        &OnionObject::Boolean(false).stabilize(),
                                        &OnionObject::String(Arc::new(e.to_string())).stabilize(),
                                    )
                                    .into(),
                                )
                            }
                        };
                        StepResult::Continue
                    } else {
                        //self.result = *result;
                        StepResult::Return(
                            OnionPair::new_static(
                                &OnionObject::Boolean(true).stabilize(),
                                result.as_ref(),
                            )
                            .into(),
                        )
                    }
                }
                StepResult::SetSelfObject(ref self_object) => {
                    if let Some(top_runnable) = self.runnable_stack.last_mut() {
                        match top_runnable
                            .receive(&StepResult::SetSelfObject(self_object.clone()), gc)
                        {
                            Ok(_) => {}
                            Err(RuntimeError::CustomValue(ref e)) => {
                                return StepResult::Return(
                                    OnionPair::new_static(
                                        &OnionObject::Boolean(false).stabilize(),
                                        &e,
                                    )
                                    .into(),
                                )
                            }
                            Err(e) => {
                                return StepResult::Return(
                                    OnionPair::new_static(
                                        &OnionObject::Boolean(false).stabilize(),
                                        &OnionObject::String(Arc::new(e.to_string())).stabilize(),
                                    )
                                    .into(),
                                )
                            }
                        }
                    }
                    StepResult::Continue
                }
                StepResult::Error(ref error) => {
                    if let RuntimeError::Pending = error {
                        // 如果是 Pending 状态，继续等待
                        return StepResult::Error(RuntimeError::Pending);
                    }
                    return StepResult::Return(
                        OnionPair::new_static(
                            &OnionObject::Boolean(false).stabilize(),
                            &match error {
                                RuntimeError::CustomValue(ref v) => v.as_ref().clone(),
                                _ => OnionObject::Undefined(Some(error.to_string().into()))
                                    .stabilize(),
                            },
                        )
                        .into(),
                    );
                }
            }
        } else {
            StepResult::Error(RuntimeError::DetailedError(
                "No runnable in stack".to_string().into(),
            ))
        }
    }
    fn receive(
        &mut self,
        step_result: &StepResult,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        if let Some(runnable) = self.runnable_stack.last_mut() {
            runnable.receive(&step_result, gc)
        } else {
            Err(RuntimeError::DetailedError(
                "No runnable in stack".to_string().into(),
            ))
        }
    }

    fn format_context(&self) -> String {
        if self.runnable_stack.is_empty() {
            return "Scheduler: No active runnables.".to_string();
        }

        // 我们将从栈顶（最近的调用）开始，一直到栈底
        // 所以我们倒序遍历 `runnable_stack`
        let contexts: Vec<String> = self
            .runnable_stack
            .iter()
            .rev() // .rev() is crucial for correct stack trace order
            .enumerate() // Use enumerate to add frame numbers
            .map(|(index, runnable)| {
                let header = format!("--- Frame #{} ---", index);
                let inner_context = runnable.format_context();
                format!("{}\n{}", header, inner_context)
            })
            .collect();

        // 将所有帧的上下文用换行符连接起来
        contexts.join("\n\n") // Use double newline to separate frames
    }
}
