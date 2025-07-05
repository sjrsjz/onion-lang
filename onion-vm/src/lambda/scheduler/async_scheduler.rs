use arc_gc::gc::GC;
use std::{collections::VecDeque, sync::Arc};

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        async_handle::OnionAsyncHandle,
        object::{GCArcStorage, OnionObjectCell, OnionObjectExt},
    },
    unwrap_step_result,
};

const NUM_PRIORITY_LEVELS: usize = 3; // 优先级级别数量 - 1

/// 2^n-1 序列
#[inline(always)]
fn generate_sched_step(n: usize) -> u64 {
    (1u64 << (n + 1)) - 1
}

pub struct Task {
    runnable: Box<dyn Runnable>,
    task_handler: (Arc<OnionAsyncHandle>, GCArcStorage),
    priority: usize, // 优先级，决定调度间隔
}

impl Task {
    pub fn new(
        runnable: Box<dyn Runnable>,
        task_handler: (Arc<OnionAsyncHandle>, GCArcStorage),
        priority: usize,
    ) -> Self {
        Self {
            runnable,
            task_handler,
            priority,
        }
    }

    pub fn copy(&self) -> Task {
        Task {
            runnable: self.runnable.copy(),
            task_handler: self.task_handler.clone(),
            priority: self.priority,
        }
    }

    pub fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        let mut context = self.runnable.format_context()?;
        context["priority"] = serde_json::json!(self.priority);
        Ok(context)
    }
}

pub struct AsyncScheduler {
    queue: VecDeque<Task>,
    main_task_handler: (Arc<OnionAsyncHandle>, GCArcStorage), // 主任务处理器
    step: u64,                                                // 当前调度步数
}

impl AsyncScheduler {
    pub fn new(main_task: Task) -> Self {
        let mut queue = VecDeque::new();
        let main_task_handler = main_task.task_handler.clone();
        queue.push_back(main_task);
        AsyncScheduler {
            queue,
            main_task_handler,
            step: 0,
        }
    }
}

impl Runnable for AsyncScheduler {
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        // 单队列调度：遍历队列，按步数调度
        let len = self.queue.len();
        if len == 0 {
            // 所有任务都已完成，此时对 main_task_handler 执行valueof
            return StepResult::Return(
                unwrap_step_result!(self.main_task_handler.0.value_of()).into(),
            );
        }
        let mut i = 0;
        self.step += 1;
        while i < len {
            if let Some(mut task) = self.queue.pop_front() {
                // 只有 step % generate_sched_step(priority) == 0 时才调度
                if self.step % generate_sched_step(task.priority) == 0 {
                    let step_result = task.runnable.step(gc);
                    match step_result {
                        StepResult::Continue => {
                            task.priority = 0; // 重置优先级
                            self.queue.push_back(task);
                        }
                        StepResult::Return(ref result) => {
                            unwrap_step_result!(task.task_handler.0.set_result(result.weak()));
                        }
                        StepResult::Error(RuntimeError::Pending) => {
                            // 一旦pending立即降级
                            task.priority = std::cmp::min(task.priority + 1, NUM_PRIORITY_LEVELS);
                            self.queue.push_back(task);
                        }
                        e @ StepResult::Error(_) => return e,
                        StepResult::NewRunnable(_) => {
                            // AsyncScheduler 不支持 NewRunnable 因为它没有意义，出现 NewRunnable 就意味着逻辑有问题
                            return StepResult::Error(RuntimeError::DetailedError(
                                "AsyncScheduler does not support NewRunnable"
                                    .to_string()
                                    .into(),
                            ));
                        }
                        StepResult::ReplaceRunnable(_) => {
                            // 同上
                            return StepResult::Error(RuntimeError::DetailedError(
                                "AsyncScheduler does not support ReplaceRunnable"
                                    .to_string()
                                    .into(),
                            ));
                        }
                        StepResult::SpawnRunnable(new_task) => {
                            self.queue.push_back(*new_task);
                            self.queue.push_back(task);
                        }
                        StepResult::SetSelfObject(_) => {
                            self.queue.push_back(task);
                        }
                    }
                } else {
                    // 未到调度步，放回队尾
                    self.queue.push_back(task);
                }
            }
            i += 1;
        }
        StepResult::Continue
    }

    fn receive(
        &mut self,
        _step_result: &StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "AsyncScheduler does not support receive".to_string().into(),
        ))
    }

    fn copy(&self) -> Box<dyn Runnable> {
        let new_queue = self.queue.iter().map(|r| r.copy()).collect();
        Box::new(AsyncScheduler {
            queue: new_queue,
            main_task_handler: self.main_task_handler.clone(),
            step: self.step,
        })
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        let mut tasks_json_array = serde_json::Value::Array(vec![]);
        for task in &self.queue {
            let frame_json = task.format_context()?;
            tasks_json_array.as_array_mut().unwrap().push(frame_json);
        }
        Ok(serde_json::json!({
            "type": "AsyncScheduler",
            "tasks": tasks_json_array,
            "step": self.step
        }))
    }
}
