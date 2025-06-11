use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        pair::OnionPair,
    },
};

pub struct Scheduler {
    pub(crate) runnable_stack: Vec<Box<dyn Runnable>>,
    pub(crate) result: OnionStaticObject,
}

impl Scheduler {
    pub fn new(runnable_stack: Vec<Box<dyn Runnable>>) -> Self {
        Scheduler {
            runnable_stack,
            result: OnionStaticObject::default(),
        }
    }
}

impl Runnable for Scheduler {
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> Result<StepResult, RuntimeError> {
        if let Some(runnable) = self.runnable_stack.last_mut() {
            match match runnable.step(gc) {
                Ok(step_result) => step_result,
                Err(error) => {
                    return Ok(StepResult::Return(OnionPair::new_static(
                        &OnionObject::Boolean(false).stabilize(),
                        &match error {
                            RuntimeError::CustomValue(v) => v,
                            _ => OnionObject::Undefined(Some(error.to_string())).stabilize(),
                        },
                    )))
                }
            } {
                StepResult::Continue => Ok(StepResult::Continue),
                StepResult::NewRunnable(new_runnable) => {
                    self.runnable_stack.push(new_runnable);
                    Ok(StepResult::Continue)
                }
                StepResult::ReplaceRunnable(new_runnable) => {
                    self.runnable_stack.pop();
                    self.runnable_stack.push(new_runnable);
                    Ok(StepResult::Continue)
                }
                StepResult::Return(result) => {
                    self.runnable_stack.pop();
                    if let Some(top_runnable) = self.runnable_stack.last_mut() {
                        match top_runnable.receive(StepResult::Return(result.clone()), gc) {
                            Ok(_) => {}
                            Err(e) => {
                                return Ok(StepResult::Return(OnionPair::new_static(
                                    &OnionObject::Boolean(false).stabilize(),
                                    &OnionObject::String(e.to_string()).stabilize(),
                                )))
                            }
                        };
                        Ok(StepResult::Continue)
                    } else {
                        self.result = result;
                        Ok(StepResult::Return(OnionPair::new_static(
                            &OnionObject::Boolean(true).stabilize(),
                            &self.result.clone(),
                        )))
                    }
                }
            }
        } else {
            Ok(StepResult::Return(OnionPair::new_static(
                &OnionObject::Boolean(true).stabilize(),
                &self.result.clone(),
            )))
        }
    }
    fn receive(
        &mut self,
        step_result: StepResult,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        if let Some(runnable) = self.runnable_stack.last_mut() {
            runnable.receive(step_result, gc)
        } else {
            Err(RuntimeError::DetailedError(
                "No runnable in stack".to_string(),
            ))
        }
    }

    fn copy(&self) -> Box<dyn Runnable> {
        Box::new(Scheduler {
            runnable_stack: self.runnable_stack.iter().map(|r| r.copy()).collect(),
            result: self.result.clone(),
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
