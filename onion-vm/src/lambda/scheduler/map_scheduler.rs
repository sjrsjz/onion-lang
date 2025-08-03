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

#[derive(Clone)]
pub struct Mapping {
    pub(crate) container: OnionStaticObject,
    pub(crate) mapper: OnionStaticObject,
    pub(crate) collected: Vec<OnionStaticObject>,
    pub(crate) current_index: usize,
}

impl Runnable for Mapping {
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
                                let runnable = Box::new(OnionLambdaRunnableLauncher::new_static(
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
