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
    
    fn format_context(&self) -> String {
        let mut output = Vec::new();

        // 1. 调度器自身的状态
        output.push(format!(
            "-> AsyncScheduler Status:\n   - Current Step: {}\n   - Total Tasks in Queue: {}",
            self.step,
            self.queue.len()
        ));

        // 2. 遍历队列中的所有任务
        if self.queue.is_empty() {
            output.push("   - Queue is empty.".to_string());
        } else {
            output.push("--- Task Queue Details ---".to_string());
            for (index, task) in self.queue.iter().enumerate() {
                // 下一次轮到该任务执行的步数
                let next_run_step = {
                    let sched_interval = generate_sched_step(task.priority);
                    // 计算下一个能被 sched_interval 整除的 step
                    if self.step % sched_interval == 0 {
                        self.step // 就是当前步
                    } else {
                        self.step - (self.step % sched_interval) + sched_interval
                    }
                };

                // 获取 Runnable 的类型名
                let runnable_type = std::any::type_name_of_val(&*task.runnable);

                // 3. 为每个任务创建一个摘要条目
                let task_summary = format!(
                    "  [Task #{}] Priority: {} (Next run at step {}), Type: {}",
                    index,
                    task.priority,
                    next_run_step,
                    runnable_type.split("::").last().unwrap_or(runnable_type) // 简化类型名显示
                );
                output.push(task_summary);

                // 4. 获取并缩进该任务内部的上下文
                let inner_context = task.runnable.format_context();
                for line in inner_context.lines() {
                    // 为内部上下文的每一行添加缩进，以保持层次结构清晰
                    output.push(format!("    {}", line));
                }
            }
        }

        output.join("\n")
    }
}
