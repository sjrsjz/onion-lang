
//! 通用调度器（Scheduler）实现：用于管理和调度一组可运行对象（Runnable）。
//! 
//! 该模块实现了基于栈的调度机制，支持嵌套任务、协作式执行和错误处理。
//! Scheduler 作为 Onion 虚拟机的基础调度单元，负责推进任务栈、处理返回值、错误和新任务的生成。
use std::sync::Arc;

use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        object::{OnionObject, OnionObjectCell},
        pair::OnionPair,
    },
};


/// 表示一个基于栈的调度器。
/// 
/// - `runnable_stack`：任务栈，栈顶为当前活跃任务。
pub struct Scheduler {
    /// 任务栈，存储所有待调度的 Runnable 对象，栈顶为当前活跃任务
    runnable_stack: Vec<Box<dyn Runnable>>,
}


impl Scheduler {
    /// 创建一个新的调度器实例。
    /// 
    /// # 参数
    /// * `runnable_stack` - 初始任务栈
    /// 
    /// # 返回值
    /// 返回新创建的 Scheduler 实例
    pub fn new(runnable_stack: Vec<Box<dyn Runnable>>) -> Self {
        Scheduler { runnable_stack }
    }
}

impl Runnable for Scheduler {
    /// 推进调度器的执行，处理栈顶任务并根据结果调整任务栈。
    /// 
    /// # 调度逻辑
    /// - 若栈顶任务返回 Continue，则继续等待
    /// - 若返回 NewRunnable，则将新任务压栈
    /// - 若返回 ReplaceRunnable，则替换栈顶任务
    /// - 若返回 Return，则弹栈并将结果传递给新的栈顶任务
    /// - 若返回 Error，则根据错误类型返回错误或包装为 Pair
    /// 
    /// # 返回值
    /// * StepResult::Continue - 还有任务需要继续执行
    /// * StepResult::Return - 所有任务完成，返回最终结果
    /// * StepResult::Error - 栈为空或遇到致命错误
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        if let Some(runnable) = self.runnable_stack.last_mut() {
            match runnable.step(gc) {
                StepResult::Continue => StepResult::Continue,
                v @ StepResult::SpawnRunnable(_) => return v,
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
                                );
                            }
                            Err(e) => {
                                return StepResult::Return(
                                    OnionPair::new_static(
                                        &OnionObject::Boolean(false).stabilize(),
                                        &OnionObject::String(Arc::from(e.to_string())).stabilize(),
                                    )
                                    .into(),
                                );
                            }
                        };
                        StepResult::Continue
                    } else {
                        // 所有任务都已完成，返回最终结果
                        StepResult::Return(
                            OnionPair::new_static(
                                &OnionObject::Boolean(true).stabilize(),
                                result.as_ref(),
                            )
                            .into(),
                        )
                    }
                }
                StepResult::Error(ref error) => {
                    if let RuntimeError::Pending = error {
                        // 如果是 Pending 状态，继续等待
                        return StepResult::Error(RuntimeError::Pending);
                    }
                    return StepResult::Return(
                        OnionPair::new_static(
                            &OnionObject::Boolean(false).stabilize(),
                            &match error {
                                RuntimeError::CustomValue(v) => v.as_ref().clone(),
                                _ => OnionObject::Undefined(Some(error.to_string().into()))
                                    .stabilize(),
                            },
                        )
                        .into(),
                    );
                }
            }
        } else {
            StepResult::Error(RuntimeError::DetailedError(
                "No runnable in stack".into(),
            ))
        }
    }

    /// 向栈顶任务传递结果。
    /// 
    /// # 参数
    /// * `step_result` - 要传递的结果
    /// * `gc` - 垃圾收集器引用
    /// 
    /// # 返回值
    /// * Ok(()) - 成功传递
    /// * Err(RuntimeError) - 栈为空或传递失败
    fn receive(
        &mut self,
        step_result: &StepResult,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        if let Some(runnable) = self.runnable_stack.last_mut() {
            runnable.receive(&step_result, gc)
        } else {
            Err(RuntimeError::DetailedError(
                "No runnable in stack".into(),
            ))
        }
    }

    /// 格式化调度器的当前上下文信息。
    /// 
    /// 以栈帧形式输出所有活跃任务的上下文，便于调试。
    /// 
    /// # 返回值
    /// 返回格式化的多行字符串，展示任务栈的状态。
    fn format_context(&self) -> String {
        if self.runnable_stack.is_empty() {
            return "Scheduler: No active runnables.".to_string();
        }

        // 从栈顶（最近的调用）到栈底，倒序遍历 runnable_stack
        let contexts: Vec<String> = self
            .runnable_stack
            .iter()
            .rev() // .rev() 保证栈帧顺序正确
            .enumerate() // 添加帧编号
            .map(|(index, runnable)| {
                let header = format!("--- Frame #{} ---", index);
                let inner_context = runnable.format_context();
                format!("{}\n{}", header, inner_context)
            })
            .collect();

        // 用双换行分隔所有帧的上下文
        contexts.join("\n\n")
    }
}
