use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    onion_tuple,
    types::lambda::launcher::OnionLambdaRunnableLauncher,
    unwrap_step_result,
};

use super::{
    lambda::definition::{LambdaBody, OnionLambdaDefinition},
    object::{OnionObject, OnionObjectCell, OnionStaticObject},
    tuple::OnionTuple,
};

#[derive(Clone)]
pub struct OnionLazySet {
    container: OnionObject,
    filter: OnionObject,
}

impl GCTraceable<OnionObjectCell> for OnionLazySet {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        self.container.collect(queue);
        self.filter.collect(queue);
    }
}

impl Debug for OnionLazySet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LazySet({:?}, {:?})", self.container, self.filter)
    }
}

impl OnionLazySet {
    pub fn new(container: OnionObject, filter: OnionObject) -> Self {
        OnionLazySet {
            container: container.into(),
            filter: filter.into(),
        }
    }

    pub fn new_static(
        container: &OnionStaticObject,
        filter: &OnionStaticObject,
    ) -> OnionStaticObject {
        OnionObject::LazySet(
            OnionLazySet {
                container: container.weak().clone(),
                filter: filter.weak().clone(),
            }
            .into(),
        )
        .stabilize()
    }

    #[inline(always)]
    pub fn get_container(&self) -> &OnionObject {
        &self.container
    }

    #[inline(always)]
    pub fn get_filter(&self) -> &OnionObject {
        &self.filter
    }

    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.container.upgrade(collected);
        self.filter.upgrade(collected)
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        match key {
            OnionObject::String(s) if s.as_str() == "container" => f(&self.container),
            OnionObject::String(s) if s.as_str() == "filter" => f(&self.filter),
            OnionObject::String(s) if s.as_str() == "collect" => {
                let collector = OnionLazySetCollector {
                    container: self.container.stabilize(),
                    filter: self.filter.stabilize(),
                    collected: Vec::new(),
                    current_index: 0,
                };
                let collector = OnionLambdaDefinition::new_static(
                    &onion_tuple!(),
                    LambdaBody::NativeFunction(Box::new(collector)),
                    None,
                    None,
                    "collector".to_string(),
                );
                // Keep the collector alive until after we use its weak reference
                let result = {
                    let collector_weak = collector.weak();
                    f(collector_weak)
                };
                result
            }
            _ => Err(RuntimeError::InvalidOperation(
                format!("Attribute '{:?}' not found in lazy set", key).into(),
            )),
        }
    }
}

#[derive(Clone)]
pub struct OnionLazySetCollector {
    pub(crate) container: OnionStaticObject,
    pub(crate) filter: OnionStaticObject,
    pub(crate) collected: Vec<OnionStaticObject>,
    pub(crate) current_index: usize,
}

impl Runnable for OnionLazySetCollector {
    fn copy(&self) -> Box<dyn Runnable> {
        Box::new(OnionLazySetCollector {
            container: self.container.clone(),
            filter: self.filter.clone(),
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
                match result.weak() {
                    OnionObject::Boolean(true) => {
                        match self.container.weak() {
                            OnionObject::Tuple(tuple) => {
                                // 如果是布尔值 true，表示需要收集当前元素
                                if let Some(item) = tuple.get_elements().get(self.current_index - 1)
                                {
                                    self.collected.push(item.stabilize());
                                    Ok(())
                                } else {
                                    // 所有元素都处理完了
                                    Ok(())
                                }
                            }
                            _ => Err(RuntimeError::DetailedError(
                                "Container must be a tuple".to_string().into(),
                            )),
                        }
                    }
                    _ => {
                        // 如果返回的不是布尔值，直接忽略
                        Ok(())
                    }
                }
            }
            _ => Err(RuntimeError::DetailedError(
                "Unexpected step result in lazy set collector"
                    .to_string()
                    .into(),
            )),
        }
    }

    fn step(&mut self, _gc: &mut GC<OnionObjectCell>) -> StepResult {
        unwrap_step_result!(self
            .container
            .weak()
            .with_data(|container| match container {
                OnionObject::Tuple(tuple) => {
                    // 使用索引获取当前元素
                    if let Some(item) = tuple.get_elements().get(self.current_index) {
                        let item_clone = item.clone();
                        self.current_index += 1; // 移动到下一个元素

                        self.filter
                            .weak()
                            .with_data(|filter: &OnionObject| match filter {
                                OnionObject::Lambda(_) => {
                                    // let OnionObject::Tuple(params) = func.parameter.try_borrow()?
                                    // else {
                                    //     return Err(RuntimeError::InvalidType(format!(
                                    //         "Filter's parameter must be a tuple, got {:?}",
                                    //         func.parameter
                                    //     )));
                                    // };
                                    // let argument =
                                    //     params.clone_and_named_assignment(&OnionTuple::new(vec![
                                    //         item_clone,
                                    //     ]))?;
                                    // let runnable = func.create_runnable(argument, &self.filter, gc)?;
                                    let argument = OnionObject::Tuple(
                                        OnionTuple::new(vec![item_clone]).into(),
                                    )
                                    .consume_and_stabilize();
                                    let runnable =
                                        Box::new(OnionLambdaRunnableLauncher::new_static(
                                            &self.filter,
                                            &argument,
                                            &|r| Ok(r),
                                        )?);
                                    Ok(StepResult::NewRunnable(runnable))
                                }
                                OnionObject::Boolean(false) => Ok(StepResult::Continue),
                                _ => {
                                    self.collected.push(item_clone.consume_and_stabilize());
                                    Ok(StepResult::Continue)
                                }
                            })
                    } else {
                        // 所有元素都处理完了
                        Ok(StepResult::Return(
                            OnionTuple::new_static_no_ref(&self.collected).into(),
                        ))
                    }
                }
                _ => Err(RuntimeError::InvalidType(
                    "Container must be a tuple".to_string().into(),
                )),
            }))
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        return Ok(serde_json::json!({
            "type": "LazySetCollector",
            "container": self.container.to_string(),
            "filter": self.filter.to_string(),
            "collected": self.collected.iter().map(|o| o.to_string()).collect::<Vec<_>>(),
            "current_index": self.current_index,
        }));
    }
}

impl OnionLazySet {
    pub fn reconstruct_container(&self) -> Result<OnionObject, RuntimeError> {
        Ok(OnionObject::LazySet(
            OnionLazySet {
                container: self.container.clone(),
                filter: self.filter.clone(),
            }
            .into(),
        ))
    }
}
