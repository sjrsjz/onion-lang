use colored::*;
use onion_vm::{
    GC,
    lambda::{
        runnable::{Runnable, RuntimeError, StepResult},
        scheduler::scheduler::Scheduler,
    },
    types::{
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinitionInner},
            launcher::OnionLambdaRunnableLauncher,
            parameter::LambdaParameter,
            vm_instructions::{
                instruction_set::VMInstructionPackage, ir::IRPackage, ir_translator::IRTranslator,
            },
        },
        object::{OnionObject, OnionStaticObject},
        tuple::OnionTuple,
        undefined::OnionUndefined,
    },
    unwrap_object,
    utils::fastmap::OnionFastMap,
};

use onion_frontend::diagnostics::collector::DiagnosticCollector;
use onion_frontend::{compile::build_code, parser::Source};
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
        source: &Source,
        collector: &mut DiagnosticCollector,
    ) -> Result<(), String> {
        let ir_package = match build_code(collector, source) {
            Ok(package) => {
                if collector.has_errors() {
                    return Err("Compilation failed".to_string());
                }
                package
            }
            Err(_) => return Err("Compilation failed".to_string()),
        };

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
        capture.push("stdlib", stdlib.weak().clone());
        capture.push("Out", self.out_tuple.weak().clone());

        // 创建Lambda定义，包含stdlib和Out两个参数
        let lambda = OnionLambdaDefinitionInner::new_static(
            LambdaParameter::Multiple(Box::new([])),
            LambdaBody::Instruction(Arc::new(vm_instructions_package.clone())),
            capture,
            "__main__".into(),
            LambdaType::Atomic,
        );

        let args = OnionTuple::new_static(vec![]);
        let mut scheduler: Box<dyn Runnable> = Box::new(Scheduler::new(vec![Box::new(
            OnionLambdaRunnableLauncher::new(
                lambda.weak(),
                OnionUndefined::new_static(None),
                args,
                Ok,
            )
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
                    // 在 with_data 闭包内提取所有需要的数据
                    let result_data = result
                        .weak()
                        .with_data(|obj| {
                            // 解包 Pair
                            let pair = unwrap_object!(obj, OnionObject::Pair)?;

                            // 提取 success 标志
                            let success = pair.get_key().with_data(|key_obj| {
                                let bool_val = unwrap_object!(key_obj, OnionObject::BooleanValue)?;
                                Ok(bool_val.value())
                            })?;

                            // 提取错误消息（如果失败的话）
                            let error_message = if !success {
                                Some(pair.get_value().display(&vec![])?)
                            } else {
                                None
                            };

                            // 提取结果值
                            let result_value = pair.get_value().clone();

                            // 检查是否是 Undefined
                            let is_undefined = pair.get_value().with_data(|value_obj| {
                                Ok(unwrap_object!(value_obj, OnionObject::Undefined).is_ok())
                            })?;

                            // 获取要打印的内容（如果需要打印的话）
                            let print_content = if success && !is_undefined {
                                Some(pair.get_value().display(&vec![])?)
                            } else {
                                None
                            };

                            Ok((success, error_message, result_value, print_content))
                        })
                        .map_err(|e| format!("Failed to process return value: {e:?}"))?;

                    let (success, error_message, result_value, print_content) = result_data;

                    if !success {
                        // 处理失败情况
                        if let Some(error_msg) = error_message {
                            eprintln!("{} {}", "Error:".red().bold(), error_msg);
                        }
                        eprintln!(
                            "\n{}",
                            "Context at Time of Failure Return:".yellow().underline()
                        );
                        eprintln!("{}", scheduler.format_context());
                        return Err("Execution returned a failure value.".to_string());
                    }

                    // 将结果添加到Out元组中
                    self.add_result_to_out(result_value);

                    // 打印结果（如果需要的话）
                    if let Some(result_str) = print_content {
                        println!("{} {}", "Result:".cyan(), result_str);
                    }
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
                let mut elements = tuple.get_elements().to_vec();
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
