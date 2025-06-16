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
                    )
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

    fn copy(&self) -> Box<dyn Runnable> {
        Box::new(Scheduler {
            runnable_stack: self.runnable_stack.iter().map(|r| r.copy()).collect(),
        })
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        let mut stack_json_array = serde_json::Value::Array(vec![]);
        for runnable in &self.runnable_stack {
            let frame_json = runnable.format_context()?;
            stack_json_array.as_array_mut().unwrap().push(frame_json);
        }
        // {type: "Scheduler", frames: frame_json_array}
        Ok(serde_json::json!({
            "type": "Scheduler",
            "frames": stack_json_array
        }))
    }
}
