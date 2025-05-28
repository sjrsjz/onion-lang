use std::{cell::RefCell, fmt::Debug, sync::Arc};

use arc_gc::{arc::GCArc, gc::GC, traceable::GCTraceable};

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    onion_tuple,
};

use super::{
    lambda::definition::{LambdaBody, OnionLambdaDefinition},
    object::{ObjectError, OnionObject, OnionStaticObject},
    tuple::OnionTuple,
};

#[derive(Clone)]
pub struct OnionLazySet {
    pub container: Box<OnionObject>,
    pub filter: Box<OnionObject>,
}

impl GCTraceable for OnionLazySet {
    fn visit(&self) {
        self.container.visit();
        self.filter.visit();
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
            container: Box::new(container),
            filter: Box::new(filter),
        }
    }

    pub fn new_static(
        container: &OnionStaticObject,
        filter: &OnionStaticObject,
    ) -> OnionStaticObject {
        OnionObject::LazySet(OnionLazySet {
            container: Box::new(container.weak().clone()),
            filter: Box::new(filter.weak().clone()),
        })
        .stabilize()
    }

    pub fn get_container(&self) -> &OnionObject {
        &self.container
    }

    pub fn get_filter(&self) -> &OnionObject {
        &self.filter
    }

    pub fn upgrade(&self) -> Option<Vec<GCArc<OnionObject>>> {
        match (self.container.upgrade(), self.filter.upgrade()) {
            (Some(mut container_arcs), Some(filter_arcs)) => {
                container_arcs.extend(filter_arcs);
                Some(container_arcs)
            }
            _ => None,
        }
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, ObjectError>
    where
        F: Fn(&OnionObject) -> Result<R, ObjectError>,
    {
        match key {
            OnionObject::String(s) if s.as_str() == "container" => f(&self.container),
            OnionObject::String(s) if s.as_str() == "filter" => f(&self.filter),
            OnionObject::String(s) if s.as_str() == "collect" => {
                let collector = OnionLazySetCollector {
                    container: self.container.clone().stabilize(),
                    filter: self.filter.clone().stabilize(),
                    collected: Vec::new(),
                    current_index: 0,
                };
                let collector = OnionLambdaDefinition::new_static(
                    &onion_tuple!(),
                    LambdaBody::NativeFunction(Arc::new(RefCell::new(collector))),
                    None,
                    None,
                    "collector".to_string(),
                );
                f(&collector.weak())
            }
            _ => Err(ObjectError::InvalidOperation(format!(
                "Attribute '{:?}' not found in lazy set",
                key
            ))),
        }
    }

    pub fn with_attribute_mut<F, R>(&mut self, key: &OnionObject, f: &F) -> Result<R, ObjectError>
    where
        F: Fn(&mut OnionObject) -> Result<R, ObjectError>,
    {
        match key {
            OnionObject::String(s) if s.as_str() == "container" => f(&mut self.container),
            OnionObject::String(s) if s.as_str() == "filter" => f(&mut self.filter),
            _ => Err(ObjectError::InvalidOperation(format!(
                "Attribute '{:?}' not found in lazy set",
                key
            ))),
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
    fn set_argument(
        &mut self,
        _argument: OnionStaticObject,
        _gc: &mut GC<OnionObject>,
    ) -> Result<(), ObjectError> {
        Ok(()) // This collector does not use an argument, so we can ignore it.
    }
    fn copy(&self, _gc: &mut GC<OnionObject>) -> Box<dyn Runnable> {
        Box::new(OnionLazySetCollector {
            container: self.container.clone(),
            filter: self.filter.clone(),
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
                match result.weak() {
                    OnionObject::Boolean(true) => {
                        match self.container.weak() {
                            OnionObject::Tuple(tuple) => {
                                // 如果是布尔值 true，表示需要收集当前元素
                                if let Some(item) = tuple.elements.get(self.current_index - 1) {
                                    self.collected.push(item.clone().stabilize());
                                    Ok(())
                                } else {
                                    // 所有元素都处理完了
                                    Ok(())
                                }
                            }
                            _ => Err(RuntimeError::DetailedError(
                                "Container must be a tuple".to_string(),
                            )),
                        }
                    }
                    _ => {
                        // 如果返回的不是布尔值，直接忽略
                        Ok(())
                    }
                }
            }
            StepResult::Error(err) => Err(err),
            _ => Err(RuntimeError::DetailedError(
                "Unexpected step result in lazy set collector".to_string(),
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
                        self.current_index += 1; // 移动到下一个元素

                        self.filter.weak().with_data(|filter| match filter {
                            OnionObject::Lambda(func) => {
                                let OnionObject::Tuple(params) = func.parameter.as_ref() else {
                                    return Err(ObjectError::InvalidType(format!(
                                        "Filter's parameter must be a tuple, got {:?}",
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
