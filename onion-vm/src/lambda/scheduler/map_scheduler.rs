//! Map 调度器实现：用于对容器（如元组）中的每个元素应用映射器函数，并收集结果。
//!
//! 该模块实现了 Mapping 结构体及其 Runnable trait，用于在 Onion 虚拟机中高效地进行 map 操作。
//! 支持惰性调度、逐步执行和结果收集，适用于异步/协作式执行环境。
use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::launcher::OnionLambdaRunnableLauncher,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    utils::format_object_summary,
};

/// 表示一次 map 操作的调度状态。
///
/// - `container`：待映射的容器对象（通常为元组）
/// - `mapper`：映射器函数对象（lambda 或其他）
/// - `collected`：已收集的映射结果
/// - `current_index`：当前处理到的元素索引
#[derive(Clone)]
pub struct Mapping {
    /// 待映射的容器对象（如元组）
    container: OnionStaticObject,
    /// 映射器函数对象
    mapper: OnionStaticObject,
    /// 已收集的结果
    collected: Vec<OnionStaticObject>,
    /// 当前处理的元素索引
    current_index: usize,
}

impl Mapping {
    /// 创建一个新的 Mapping 实例。
    ///
    /// # 参数
    /// * `container` - 待映射的容器对象（通常为元组）
    /// * `mapper` - 映射器函数对象（lambda 或其他）
    ///
    /// # 返回值
    /// 返回新创建的 Mapping 实例，初始状态下未处理任何元素。
    pub fn new(container: &OnionStaticObject, mapper: &OnionStaticObject) -> Self {
        Mapping {
            container: container.clone(),
            mapper: mapper.clone(),
            collected: vec![],
            current_index: 0,
        }
    }
}

impl Runnable for Mapping {
    /// 接收子任务的执行结果，并收集到结果列表。
    ///
    /// # 参数
    /// * `step_result` - 子任务的执行结果，期望为 StepResult::Return
    /// * `_gc` - 垃圾收集器引用（未使用）
    ///
    /// # 返回值
    /// * Ok(()) - 成功收集结果并推进索引
    /// * Err(RuntimeError) - 收到非预期结果类型
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
                "Unexpected step result in mapping".into(),
            )),
        }
    }

    /// 执行 map 操作的一个调度步骤。
    ///
    /// - 若当前元素存在，尝试用 mapper 处理之：
    ///   - 若 mapper 是 lambda，则生成新 Runnable 进行调用
    ///   - 若 mapper 为 false，跳过当前元素
    ///   - 其他情况直接收集元素
    /// - 若所有元素处理完毕，返回收集到的新元组
    ///
    /// # 返回值
    /// * StepResult::NewRunnable - 需要调度新任务
    /// * StepResult::Continue - 继续下一个元素
    /// * StepResult::Return - 所有元素处理完毕，返回结果
    /// * StepResult::Error - 类型错误或其他异常
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
                                let runnable = Box::new(OnionLambdaRunnableLauncher::new(
                                    mapper_obj,
                                    element.stabilize(),
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
                            OnionTuple::new_static_no_ref(&self.collected).into(),
                        ))
                    }
                }
                _ => Err(RuntimeError::InvalidType(
                    "Container must be a tuple".into(),
                )),
            })
            .unwrap_or_else(|e| StepResult::Error(e))
    }

    /// 格式化 map 调度器的当前上下文信息。
    ///
    /// 输出当前映射器、容器、进度和已收集结果数量，便于调试。
    ///
    /// # 返回值
    /// 返回格式化的多行字符串，展示 map 操作的状态。
    fn format_context(&self) -> String {
        // 使用 format! 宏来构建一个多行的字符串
        format!(
            "-> In 'map' operation:\n   - Mapper: {}\n   - Container: {}\n   - Progress: Processing element {} / {}\n   - Collected Items: {}",
            // 1. 映射器信息
            // 使用对象的简略表示（比如 debug 格式）
            format_object_summary(self.mapper.weak()),
            // 2. 容器信息
            format_object_summary(self.container.weak()),
            // 3. 进度信息
            self.current_index,
            self.container
                .weak()
                .with_data(|c| {
                    Ok(if let OnionObject::Tuple(t) = c {
                        t.get_elements().len()
                    } else {
                        0 // Or some other placeholder like "N/A"
                    })
                })
                .unwrap_or(0), // Provide a default if weak link is dead
            // 4. 已收集结果的数量
            self.collected.len()
        )
    }
}
