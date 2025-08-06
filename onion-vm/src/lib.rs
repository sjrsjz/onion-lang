//! OnionVM 虚拟机核心库。
//!
//! 本 crate 提供 Onion 语言运行时的核心类型、虚拟机执行引擎、
//! Lambda 表达式、垃圾回收、类型系统与高性能工具集。
//!
//! # 主要模块
//! - `types`：Onion 运行时核心类型系统（对象、元组、lambda、集合等）
//! - `lambda`：抽象可调度匿名函数对象及调度器实现
//! - `utils`：高性能工具与辅助结构（如 FastMap）
//!
//! # 主要导出
//! - `GC`：基于 arc_gc 的垃圾回收器
//! - `GCTraceable`：GC 跟踪 trait
//! - `GCArc`/`GCArcWeak`：GC 智能指针
//!
//! # 使用指南
//!
//! ## 基本VM执行流程
//!
//! OnionVM 执行字节码的标准流程如下：
//!
//! ```ignore
//! // 1. 创建垃圾回收器，设置内存阈值
//! let mut gc = GC::new_with_memory_threshold(1024 * 1024); // 1 MB threshold
//!
//! // 2. 验证VM指令包（假设已有vm_instructions_package）
//! VMInstructionPackage::validate(&vm_instructions_package)?;
//!
//! // 3. 创建标准库和捕获变量
//! let stdlib = stdlib::build_module();
//! let mut capture = OnionFastMap::new(vm_instructions_package.create_key_pool());
//! capture.push("stdlib", stdlib.weak().clone());
//!
//! // 4. 创建Lambda定义
//! let lambda = OnionLambdaDefinition::new_static(
//!     LambdaParameter::Multiple([].into()),
//!     LambdaBody::Instruction(Arc::new(vm_instructions_package.clone())),
//!     capture,
//!     "__main__".into(),
//!     LambdaType::Atomic,
//! );
//!
//! // 5. 创建调度器和执行循环
//! let args = OnionTuple::new_static(vec![]);
//! let mut scheduler: Box<dyn Runnable> = Box::new(Scheduler::new(vec![
//!     Box::new(OnionLambdaRunnableLauncher::new(lambda.weak(), args, Ok)?)
//! ]));
//!
//! // 6. 主执行循环
//! loop {
//!     match scheduler.step(&mut gc) {
//!         StepResult::Continue => continue,
//!         StepResult::Return(result) => {
//!             // 处理返回结果
//!             break;
//!         }
//!         StepResult::Error(error) => {
//!             // 处理运行时错误
//!             return Err(error);
//!         }
//!         _ => {
//!             // 处理其他情况
//!         }
//!     }
//! }
//! ```
//!
//! ## 核心组件说明
//!
//! ### 垃圾回收器 (GC)
//! - 基于 arc_gc 实现的增量垃圾回收器
//! - 可设置内存阈值触发自动回收
//! - 在调试模式下建议每步执行后手动回收
//!
//! ### Lambda系统
//! - `OnionLambdaDefinition`：Lambda函数定义
//! - `OnionLambdaRunnableLauncher`：Lambda启动器
//! - `Scheduler`：多任务调度器，管理可运行对象队列
//!
//! ### 执行状态
//! - `StepResult::Continue`：继续执行下一步
//! - `StepResult::Return(value)`：正常返回结果
//! - `StepResult::Error(error)`：运行时错误
//! - `StepResult::SpawnRunnable(runnable)`：生成新的异步任务
//! - `StepResult::Pending`：等待状态，需要继续轮询
//!
//! ### 错误处理
//! - 运行时错误会包含详细的上下文信息
//! - 可通过 `scheduler.format_context()` 获取完整执行上下文
//! - 程序逻辑错误通过返回值的success字段标识
//!
//! ## 类型系统
//! - 所有对象都基于 `OnionObject` 枚举
//! - 支持弱引用和强引用转换
//! - 内置类型包括：字符串、数字、布尔值、元组、Lambda等
//!
//! ## 性能优化
//! - 使用 `OnionFastMap` 替代标准 HashMap 获得更好性能
//! - 字符串池 (`OnionKeyPool`) 减少内存分配
//! - 增量垃圾回收避免长时间停顿

pub mod types;
pub mod lambda;
pub use arc_gc::gc::GC as GC;
pub use arc_gc::traceable::GCTraceable as GCTraceable;
pub use arc_gc::arc::GCArc as GCArc;
pub use arc_gc::arc::GCArcWeak as GCArcWeak;
pub mod utils;