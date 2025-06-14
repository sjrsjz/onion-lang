use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::launcher::OnionLambdaRunnableLauncher,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
};

#[derive(Clone)]
pub struct Mapping {
    pub(crate) container: OnionStaticObject,
    pub(crate) mapper: OnionStaticObject,
    pub(crate) collected: Vec<OnionStaticObject>,
    pub(crate) current_index: usize,
}

impl Runnable for Mapping {
    fn copy(&self) -> Box<dyn Runnable> {
        Box::new(Mapping {
            container: self.container.clone(),
            mapper: self.mapper.clone(),
            collected: self.collected.clone(),
            current_index: self.current_index,
        })
    }

    fn receive(
        &mut self,
        step_result: &StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        match step_result {
            StepResult::Return(result) => {
                self.collected.push(result.as_ref().clone());
                self.current_index += 1; // 移动到下一个元素
                Ok(())
            }
            _ => Err(RuntimeError::DetailedError(
                "Unexpected step result in mapping".to_string().into(),
            )),
        }
    }

    fn step(&mut self, _gc: &mut GC<OnionObjectCell>) -> StepResult {
        self.container
            .weak()
            .with_data(|container| match container {
                OnionObject::Tuple(tuple) => {
                    // 使用索引获取当前元素
                    if let Some(element) = tuple.get_elements().get(self.current_index) {
                        let element_clone = element.clone();
                        self.mapper.weak().with_data(|mapper_obj| match mapper_obj {
                            OnionObject::Lambda(_) => {
                                // let OnionObject::Tuple(params) = &*lambda.parameter.try_borrow()?
                                // else {
                                //     return Err(RuntimeError::InvalidType(format!(
                                //         "Map's parameter must be a tuple, got {:?}",
                                //         lambda.parameter
                                //     )));
                                // };
                                // let argument =
                                //     params.clone_and_named_assignment(&OnionTuple::new(vec![
                                //         element_clone,
                                //     ]))?;
                                // let runnable = lambda.create_runnable(argument, &self.mapper, gc)?;
                                let argument =
                                    OnionObject::Tuple(OnionTuple::new(vec![element_clone]).into())
                                        .stabilize();
                                let runnable = Box::new(OnionLambdaRunnableLauncher::new_static(
                                    &self.mapper,
                                    &argument,
                                    &|r| Ok(r),
                                )?);
                                Ok(StepResult::NewRunnable(runnable))
                            }
                            OnionObject::Boolean(false) => Ok(StepResult::Continue),
                            _ => {
                                self.collected.push(element_clone.stabilize());
                                Ok(StepResult::Continue)
                            }
                        })
                    } else {
                        // 所有元素都处理完了
                        Ok(StepResult::Return(
                            OnionTuple::new_static_no_ref(self.collected.clone()).into(),
                        ))
                    }
                }
                _ => Err(RuntimeError::InvalidType(
                    "Container must be a tuple".to_string().into(),
                )),
            })
            .unwrap_or_else(|e| StepResult::Error(e))
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        return Ok(serde_json::json!({
            "type": "Mapping",
            "container": self.container.to_string(),
            "mapper": self.mapper.to_string(),
            "collected": self.collected.iter().map(|o| o.to_string()).collect::<Vec<_>>(),
            "current_index": self.current_index,
        }));
    }
}
