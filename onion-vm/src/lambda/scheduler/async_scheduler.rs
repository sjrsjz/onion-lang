//! 异步任务调度器（AsyncScheduler）实现：用于管理和调度一组可运行对象（Runnable）。
//! 
//! 该模块实现了基于优先级的协作式多任务调度，支持任务的动态优先级调整和新任务的生成。
//! AsyncScheduler 作为 Onion 虚拟机的顶层调度单元，负责按优先级推进任务队列、处理返回值、错误和新任务的生成。
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

/// 优先级级别数量减一，用于限制最大优先级
const NUM_PRIORITY_LEVELS: usize = 3;

/// 生成调度步长，基于 2^(n+1)-1 序列
///
/// 这个函数为给定的优先级生成调度间隔。优先级越高（数值越大），
/// 调度间隔越长，任务被执行的频率越低。
///
/// # 参数
/// * `n` - 优先级级别（0表示最高优先级）
///
/// # 返回值
/// 返回该优先级对应的调度间隔步数
///
/// # 示例
/// - 优先级0: 2^1-1 = 1 (每步都执行)
/// - 优先级1: 2^2-1 = 3 (每3步执行一次)
/// - 优先级2: 2^3-1 = 7 (每7步执行一次)
#[inline(always)]
fn generate_sched_step(n: usize) -> u64 {
    (1u64 << (n + 1)) - 1
}

/// 表示调度器中的一个任务
///
/// 每个任务包含可执行的代码、任务处理器和优先级信息。
/// 优先级决定了任务被调度的频率，数值越大表示优先级越低。
pub struct Task {
    /// 可执行的任务代码
    runnable: Box<dyn Runnable>,
    /// 异步任务处理器，用于设置任务结果和管理任务状态
    task_handler: (Arc<OnionAsyncHandle>, GCArcStorage),
    /// 任务优先级，0表示最高优先级，数值越大优先级越低
    priority: usize,
}

impl Task {
    /// 创建一个新的任务
    ///
    /// # 参数
    /// * `runnable` - 实现了 Runnable trait 的可执行对象
    /// * `task_handler` - 异步任务处理器，包含 AsyncHandle 和 GC 存储
    /// * `priority` - 任务的初始优先级，0表示最高优先级
    ///
    /// # 返回值
    /// 返回新创建的 Task 实例
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

/// 异步任务调度器
///
/// 实现了基于优先级的协作式多任务调度。任务按照优先级和调度间隔被执行，
/// 优先级越高的任务执行频率越高。调度器使用轮询方式处理任务队列，
/// 支持任务的动态优先级调整和新任务的生成。
///
/// # 调度策略
/// - 使用单队列存储所有任务
/// - 基于 2^(n+1)-1 序列确定调度间隔
/// - 当任务pending时自动降低优先级
/// - 支持任务完成时的结果设置
///
/// # 优先级系统
/// - 0: 最高优先级，每步都执行
/// - 1: 中等优先级，每3步执行一次
/// - 2: 低优先级，每7步执行一次
/// - 3: 最低优先级，每15步执行一次
pub struct AsyncScheduler {
    /// 任务队列，使用双端队列实现高效的入队出队操作
    queue: VecDeque<Task>,
    /// 主任务处理器，用于处理调度器完成时的结果
    main_task_handler: (Arc<OnionAsyncHandle>, GCArcStorage),
    /// 当前调度步数，用于计算任务的执行时机
    step: u64,
}

impl AsyncScheduler {
    /// 创建一个新的异步调度器
    ///
    /// # 参数
    /// * `main_task` - 主要任务，调度器会首先执行此任务
    ///
    /// # 返回值
    /// 返回新创建的 AsyncScheduler 实例，主任务已添加到队列中
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
    /// 执行一个调度步骤
    ///
    /// 这是调度器的核心方法，负责遍历任务队列并按优先级调度执行任务。
    /// 每次调用会增加步数计数器，并检查每个任务是否应该在当前步执行。
    ///
    /// # 参数
    /// * `gc` - 垃圾收集器的可变引用，用于内存管理
    ///
    /// # 返回值
    /// * `StepResult::Continue` - 还有任务需要继续执行
    /// * `StepResult::Return(_)` - 所有任务完成，返回主任务的结果
    /// * `StepResult::Error(_)` - 执行过程中出现错误
    ///
    /// # 调度逻辑
    /// 1. 检查队列是否为空，为空则返回主任务结果
    /// 2. 增加步数计数器
    /// 3. 遍历队列中的每个任务
    /// 4. 根据任务优先级和当前步数决定是否执行
    /// 5. 处理任务执行结果：完成、继续、挂起或错误
    /// 6. 动态调整任务优先级和队列位置
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

    /// 接收来自其他 Runnable 的结果
    ///
    /// AsyncScheduler 不支持接收外部结果，因为它是顶层调度器。
    /// 任何尝试向调度器发送结果的操作都会返回错误。
    ///
    /// # 参数
    /// * `_step_result` - 被忽略的步骤结果
    /// * `_gc` - 被忽略的垃圾收集器引用
    ///
    /// # 返回值
    /// 总是返回 `RuntimeError::DetailedError`，表示不支持此操作
    fn receive(
        &mut self,
        _step_result: &StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "AsyncScheduler does not support receive".into(),
        ))
    }

    /// 格式化调度器的当前上下文信息
    ///
    /// 生成包含调度器状态和所有任务详细信息的格式化字符串，
    /// 用于调试和监控调度器的运行状态。
    ///
    /// # 返回值
    /// 返回包含以下信息的格式化字符串：
    /// - 调度器当前状态（步数、队列长度）
    /// - 每个任务的详细信息（优先级、下次执行步数、类型）
    /// - 每个任务内部的上下文信息（缩进显示）
    ///
    /// # 输出格式
    /// ```
    /// -> AsyncScheduler Status:
    ///    - Current Step: 42
    ///    - Total Tasks in Queue: 3
    /// --- Task Queue Details ---
    ///   [Task #0] Priority: 1 (Next run at step 45), Type: SomeRunnable
    ///     ... (task internal context)
    ///   [Task #1] Priority: 0 (Next run at step 43), Type: AnotherRunnable
    ///     ... (task internal context)
    /// ```
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
