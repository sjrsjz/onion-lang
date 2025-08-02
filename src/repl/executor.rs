use colored::*;
use onion_vm::{
    GC,
    lambda::{
        runnable::{Runnable, RuntimeError, StepResult},
        scheduler::scheduler::Scheduler,
    },
    types::{
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinition},
            launcher::OnionLambdaRunnableLauncher,
            parameter::LambdaParameter,
            vm_instructions::{
                instruction_set::VMInstructionPackage, ir::IRPackage, ir_translator::IRTranslator,
            },
        },
        object::{OnionObject, OnionStaticObject},
        tuple::OnionTuple,
    },
    unwrap_object,
    utils::fastmap::OnionFastMap,
};

use onion_frontend::{compile::build_code, utils::cycle_detector};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

/// REPL专用执行器
#[derive(Clone)]
pub struct ReplExecutor {
    /// 存储历史执行结果的元组
    out_tuple: OnionStaticObject,
    /// 中断信号标志
    interrupted: Arc<AtomicBool>,
}

impl ReplExecutor {
    pub fn new(interrupted: Arc<AtomicBool>) -> Self {
        // 创建空的Out元组用于存储历史结果
        let empty_tuple = OnionTuple::new_static(vec![]);
        Self {
            out_tuple: empty_tuple,
            interrupted,
        }
    }

    /// 获取历史执行结果数量
    pub fn history_count(&self) -> usize {
        if let OnionObject::Tuple(tuple) = self.out_tuple.weak() {
            tuple.get_elements().len()
        } else {
            0
        }
    }
    /// 清空历史记录
    pub fn clear_history(&mut self) {
        let empty_tuple = OnionTuple::new_static(vec![]);
        self.out_tuple = empty_tuple;
    }

    /// 执行代码并将结果存储到Out参数中
    pub fn execute_code(
        &mut self,
        code: &str,
        cycle_detector: &mut cycle_detector::CycleDetector<String>,
        dir_stack: &mut onion_frontend::dir_stack::DirectoryStack,
    ) -> Result<(), String> {
        let ir_package = build_code(code, cycle_detector, dir_stack)
            .map_err(|e| format!("Compilation failed\n{e}"))?;

        self.execute_ir_package(&ir_package)
    }

    fn execute_ir_package(&mut self, ir_package: &IRPackage) -> Result<(), String> {
        let mut translator = IRTranslator::new(ir_package);
        translator
            .translate()
            .map_err(|e| format!("IR translation failed: {e:?}"))?;

        let vm_instructions_package = translator.get_result();
        self.execute_bytecode_package(&vm_instructions_package)
    }

    fn execute_bytecode_package(
        &mut self,
        vm_instructions_package: &VMInstructionPackage,
    ) -> Result<(), String> {
        // 创建标准库对象
        let stdlib = crate::stdlib::build_module();
        let mut capture = OnionFastMap::new(vm_instructions_package.create_key_pool());
        capture.push(&"stdlib".to_string(), stdlib.weak().clone());
        capture.push(&"Out".to_string(), self.out_tuple.weak().clone());

        // 创建Lambda定义，包含stdlib和Out两个参数
        let lambda = OnionLambdaDefinition::new_static(
            LambdaParameter::Multiple(vec![]),
            LambdaBody::Instruction(Arc::new(vm_instructions_package.clone())),
            capture,
            "__main__".to_string(),
            LambdaType::Normal,
        );

        let args = OnionTuple::new_static(vec![]);
        let mut scheduler: Box<dyn Runnable> = Box::new(Scheduler::new(vec![Box::new(
            OnionLambdaRunnableLauncher::new_static(lambda.weak(), args, Ok)
                .map_err(|e| format!("Failed to create runnable Lambda: {e:?}"))?,
        )]));

        let mut gc = GC::new_with_memory_threshold(1024 * 1024);

        // 执行代码
        loop {
            #[cfg(debug_assertions)]
            gc.collect();
            // 检查中断信号
            if self.interrupted.load(Ordering::SeqCst) {
                // 重置中断标志，以便下次执行可以正常进行
                // self.interrupted.store(false, Ordering::SeqCst); // 由 REPL 循环在每个命令开始时重置
                return Err("Execution interrupted by Ctrl+C".to_string());
            }

            match scheduler.step(&mut gc) {
                StepResult::Continue => {
                    // 继续下一步
                }
                StepResult::SpawnRunnable(_) => {
                    return Err("Cannot spawn async task in sync context".to_string());
                }
                StepResult::Error(ref error) => {
                    // 处理错误
                    if self.interrupted.load(Ordering::SeqCst) {
                        // 重置中断标志
                        // self.interrupted.store(false, Ordering::SeqCst); // 由 REPL 循环在每个命令开始时重置
                        return Err("Execution interrupted by Ctrl+C".to_string());
                    }
                    if let RuntimeError::Pending = error {
                        // 如果是 Pending 状态，继续等待
                        continue;
                    }
                    eprintln!("\n{}", "--- Runtime Error Occurred ---".red().bold());
                    eprintln!("An unrecoverable error was caught at the top level.");

                    eprintln!("\n{}", "Error Details:".yellow().underline());
                    eprintln!("{error}");

                    eprintln!(
                        "\n{}",
                        "Full Execution Context at Time of Crash:"
                            .yellow()
                            .underline()
                    );
                    eprintln!("{}", scheduler.format_context());

                    return Err("Execution failed. See details above.".to_string());
                }
                StepResult::NewRunnable(_) => {
                    unreachable!()
                }
                StepResult::ReplaceRunnable(_) => {
                    unreachable!()
                }
                StepResult::Return(ref result) => {
                    let result_borrowed = result.weak();
                    let result = unwrap_object!(result_borrowed, OnionObject::Pair)
                        .map_err(|e| format!("Failed to unwrap result: {e:?}"))?;
                    let key = result.get_key();
                    let success = *unwrap_object!(key, OnionObject::Boolean)
                        .map_err(|e| format!("Failed to get success key: {e:?}"))?;

                    if !success {
                        let error_msg = result
                            .get_value()
                            .to_string(&vec![])
                            .map_err(|e| format!("Failed to get error message: {e:?}"))?;

                        eprintln!("{} {}", "Error:".red().bold(), error_msg);

                        eprintln!(
                            "\n{}",
                            "Context at Time of Failure Return:".yellow().underline()
                        );
                        eprintln!("{}", scheduler.format_context());

                        return Err("Execution returned a failure value.".to_string());
                    }

                    // 获取执行结果并存储到Out元组中
                    let result_value = result.get_value();

                    // 将结果添加到Out元组中
                    self.add_result_to_out(result_value.clone());
                    let is_undefined = result_value
                        .with_data(|data| Ok(unwrap_object!(data, OnionObject::Undefined).is_ok()))
                        .map_err(|e| format!("Failed to check if result is Undefined: {e:?}"))?;
                    if is_undefined {
                        break; // 如果结果是Undefined，则不需要打印
                    }
                    // 打印结果
                    let result_str = result_value
                        .to_string(&vec![])
                        .map_err(|e| format!("Failed to get result value: {e:?}"))?;
                    println!("{} {}", "Result:".cyan(), result_str);
                    break;
                }
            }
        }

        Ok(())
    }

    /// 将结果添加到Out元组中
    fn add_result_to_out(&mut self, result: OnionObject) {
        let new_elements = {
            if let OnionObject::Tuple(tuple) = self.out_tuple.weak() {
                let mut elements = tuple.get_elements().clone();
                elements.push(result);
                elements
            } else {
                vec![result]
            }
        };

        self.out_tuple =
            OnionObject::Tuple(OnionTuple::new(new_elements).into()).consume_and_stabilize();
    }
}

impl Default for ReplExecutor {
    fn default() -> Self {
        Self::new(Arc::new(AtomicBool::new(false)))
    }
}
