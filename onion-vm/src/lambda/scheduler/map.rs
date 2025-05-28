use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        object::{ObjectError, OnionObject, OnionStaticObject},
        tuple::OnionTuple,
    },
};

#[derive(Clone)]
pub struct Mapping {
    pub(crate) container: OnionStaticObject,
    pub(crate) map: OnionStaticObject,
    pub(crate) collected: Vec<OnionStaticObject>,
    pub(crate) current_index: usize,
}

impl Runnable for Mapping {
    fn set_argument(
        &mut self,
        _argument: OnionStaticObject,
        _gc: &mut GC<OnionObject>,
    ) -> Result<(), ObjectError> {
        Ok(()) // This collector does not use an argument, so we can ignore it.
    }
    fn copy(&self, _gc: &mut GC<OnionObject>) -> Box<dyn Runnable> {
        Box::new(Mapping {
            container: self.container.clone(),
            map: self.map.clone(),
            collected: self.collected.clone(),
            current_index: self.current_index,
        })
    }

    fn receive(
        &mut self,
        step_result: StepResult,
        _gc: &mut GC<OnionObject>,
    ) -> Result<(), RuntimeError> {
        match step_result {
            StepResult::Return(result) => {
                self.collected.push(result);
                self.current_index += 1; // 移动到下一个元素
                Ok(())
            }
            StepResult::Error(err) => Err(err),
            _ => Err(RuntimeError::DetailedError(
                "Unexpected step result in mapping".to_string(),
            )),
        }
    }

    fn step(&mut self, gc: &mut GC<OnionObject>) -> Result<StepResult, RuntimeError> {
        self.container
            .weak()
            .with_data(|container| match container {
                OnionObject::Tuple(tuple) => {
                    // 使用索引获取当前元素
                    if let Some(item) = tuple.elements.get(self.current_index) {
                        let item_clone = item.clone();
                        self.map.weak().with_data(|filter| match filter {
                            OnionObject::Lambda(func) => {
                                let OnionObject::Tuple(params) = func.parameter.as_ref() else {
                                    return Err(ObjectError::InvalidType(format!(
                                        "Map's parameter must be a tuple, got {:?}",
                                        func.parameter
                                    )));
                                };
                                let argument =
                                    params.clone_and_named_assignment(&OnionTuple::new(vec![
                                        item_clone,
                                    ]))?;
                                let runnable = func.create_runnable(argument, gc)?;
                                Ok(StepResult::NewRunnable(runnable))
                            }
                            OnionObject::Boolean(false) => Ok(StepResult::Continue),
                            _ => {
                                self.collected.push(item_clone.stabilize());
                                Ok(StepResult::Continue)
                            }
                        })
                    } else {
                        // 所有元素都处理完了
                        Ok(StepResult::Return(OnionTuple::new_static_no_ref(
                            self.collected.clone(),
                        )))
                    }
                }
                _ => Err(ObjectError::InvalidType(
                    "Container must be a tuple".to_string(),
                )),
            })
            .map_err(RuntimeError::ObjectError)
    }
}
