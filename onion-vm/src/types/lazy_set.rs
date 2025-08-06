//! Onion 惰性集合（LazySet）模块。
//!
//! 提供 Onion 语言运行时的惰性集合实现，支持基于过滤器的延迟求值、
//! 集合元素的惰性遍历与收集。适用于大数据集的高效筛选与惰性计算场景。
//!
//! # 主要功能
//! - 惰性集合的容器与过滤器表达
//! - 惰性收集与遍历接口
//! - 支持 GC 跟踪与升级
//! - 集合收集器的可运行实现

use std::{collections::VecDeque, fmt::Debug, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::lambda::{
        definition::LambdaType, launcher::OnionLambdaRunnableLauncher, parameter::LambdaParameter,
    },
    unwrap_step_result,
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

use super::{
    lambda::definition::{LambdaBody, OnionLambdaDefinition},
    object::{OnionObject, OnionObjectCell, OnionStaticObject},
    tuple::OnionTuple,
};

/// Onion 惰性集合。
///
/// 封装一个容器对象和一个过滤器对象，支持基于过滤器的惰性筛选。
/// 适用于大规模数据的延迟求值与高效遍历。
///
/// # 字段
/// - `container`: 集合的底层容器对象（如元组、数组等）
/// - `filter`: 用于筛选元素的过滤器对象（如 Lambda 表达式）
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
    /// 创建新的惰性集合。
    ///
    /// # 参数
    /// - `container`: 底层容器对象
    /// - `filter`: 过滤器对象
    ///
    /// # 返回
    /// 新的惰性集合实例
    pub fn new(container: OnionObject, filter: OnionObject) -> Self {
        OnionLazySet {
            container: container.into(),
            filter: filter.into(),
        }
    }

    /// 创建静态惰性集合对象。
    ///
    /// # 参数
    /// - `container`: 静态容器对象
    /// - `filter`: 静态过滤器对象
    ///
    /// # 返回
    /// 稳定化后的静态惰性集合对象
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

    /// 获取底层容器对象的引用。
    #[inline(always)]
    pub fn get_container(&self) -> &OnionObject {
        &self.container
    }

    /// 获取过滤器对象的引用。
    #[inline(always)]
    pub fn get_filter(&self) -> &OnionObject {
        &self.filter
    }

    /// 升级集合中的所有对象引用。
    ///
    /// 用于 GC 跟踪，防止集合中的对象被提前回收。
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.container.upgrade(collected);
        self.filter.upgrade(collected)
    }

    /// 按属性名访问集合的内部成员或操作。
    ///
    /// 支持访问 "container"、"filter"、"collect" 三个属性：
    /// - `container`: 返回底层容器对象
    /// - `filter`: 返回过滤器对象
    /// - `collect`: 返回惰性收集器 Lambda
    ///
    /// # 参数
    /// - `key`: 属性名对象
    /// - `f`: 处理函数，对应属性对象作为参数
    ///
    /// # 返回
    /// - `Ok(R)`: 访问成功，返回处理函数结果
    /// - `Err(RuntimeError)`: 属性不存在或访问失败
    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        match key {
            OnionObject::String(s) if s.as_ref() == "container" => f(&self.container),
            OnionObject::String(s) if s.as_ref() == "filter" => f(&self.filter),
            OnionObject::String(s) if s.as_ref() == "collect" => {
                let empty_pool = OnionKeyPool::create(vec![]);
                let collector = OnionLazySetCollector {
                    container: self.container.stabilize(),
                    filter: self.filter.stabilize(),
                    collected: Vec::new(),
                    current_index: 0,
                };
                let collector = OnionLambdaDefinition::new_static(
                    LambdaParameter::Multiple(Box::new([])),
                    LambdaBody::NativeFunction((
                        Arc::new({
                            let collector = collector.clone();
                            move |_, _, _, _| Box::new(collector.clone())
                        }),
                        empty_pool.clone(),
                    )),
                    OnionFastMap::new(empty_pool),
                    "collector".into(),
                    LambdaType::Atomic,
                );
                // 保证 collector 生命周期直到 weak 被用完
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

/// Onion 惰性集合收集器。
///
/// 作为可运行对象实现，支持惰性集合的逐步遍历与收集。
/// 每次 step 调用处理一个元素，最终返回收集结果。
///
/// # 字段
/// - `container`: 静态容器对象
/// - `filter`: 静态过滤器对象
/// - `collected`: 已收集的元素列表
/// - `current_index`: 当前处理的元素索引
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
                                "Container must be a tuple".into(),
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
                                        let runnable = Box::new(OnionLambdaRunnableLauncher::new(
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
                        "Container must be a tuple".into(),
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
