use std::{collections::VecDeque, fmt::Debug, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    onion_tuple,
    types::lambda::{definition::LambdaType, launcher::OnionLambdaRunnableLauncher},
    unwrap_step_result,
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

use super::{
    lambda::definition::{LambdaBody, OnionLambdaDefinition},
    object::{OnionObject, OnionObjectCell, OnionStaticObject},
    tuple::OnionTuple,
};

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
                let empty_pool = OnionKeyPool::create(vec![]);
                let collector = OnionLazySetCollector {
                    container: self.container.stabilize(),
                    filter: self.filter.stabilize(),
                    collected: Vec::new(),
                    current_index: 0,
                };
                let collector = OnionLambdaDefinition::new_static(
                    &onion_tuple!(),
                    LambdaBody::NativeFunction((
                        Arc::new({
                            let collector = collector.clone();
                            move || Box::new(collector.clone())
                        }),
                        empty_pool.clone(),
                    )),
                    &OnionFastMap::new(empty_pool),
                    "collector".to_string(),
                    LambdaType::Normal,
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

    fn bind_self_object(
        &mut self,
        _self_object: &OnionObject,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Ok(())
    }

    fn capture(
        &mut self,
        _argument: &OnionFastMap<String, OnionStaticObject>,
        _captured_vars: &OnionFastMap<String, OnionObject>,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Ok(())
    }

    fn step(&mut self, _gc: &mut GC<OnionObjectCell>) -> StepResult {
        unwrap_step_result!(
            self.container
                .weak()
                .with_data(|container| match container {
                    OnionObject::Tuple(tuple) => {
                        // 使用索引获取当前元素
                        if let Some(item) = tuple.get_elements().get(self.current_index) {
                            self.current_index += 1; // 移动到下一个元素
                            self.filter
                                .weak()
                                .with_data(|filter: &OnionObject| match filter {
                                    OnionObject::Lambda(_) => {
                                        let runnable =
                                            Box::new(OnionLambdaRunnableLauncher::new_static(
                                                filter,
                                                item.stabilize(),
                                                &|r| Ok(r),
                                            )?);
                                        Ok(StepResult::NewRunnable(runnable))
                                    }
                                    v => {
                                        if v.to_boolean()? {
                                            self.collected.push(item.stabilize());
                                        }
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
                })
        )
    }

    fn format_context(&self) -> String {
        // 尝试获取容器的总长度，用于进度报告
        let container_len = self
            .container
            .weak()
            .with_data(|c| {
                Ok(if let OnionObject::Tuple(t) = c {
                    t.get_elements().len()
                } else {
                    0 // 如果容器不是元组或弱引用失效，返回0
                })
            })
            .unwrap_or(0);

        // 使用 format! 宏构建一个清晰、多行的字符串
        format!(
            "-> Collecting from LazySet:\n   - Filter Function: {:?}\n   - From Container: {:?}\n   - Progress: Checking element {} / {}\n   - Items Collected: {}",
            // 1. 过滤器信息
            // 使用 Debug 格式打印 filter 对象，以识别是哪个 lambda
            self.filter,
            // 2. 容器信息
            // 使用 Debug 格式打印 container 对象
            self.container,
            // 3. 进度信息
            // current_index 告诉我们下一个要检查的元素索引
            self.current_index,
            container_len,
            // 4. 已收集结果的数量
            self.collected.len()
        )
    }
}
