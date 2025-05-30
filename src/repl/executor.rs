use colored::*;
use onion_vm::{
    lambda::{
        runnable::{Runnable, StepResult},
        scheduler::scheduler::Scheduler,
    },
    types::{
        lambda::{
            definition::{LambdaBody, OnionLambdaDefinition},
            vm_instructions::{
                instruction_set::VMInstructionPackage, ir::IRPackage, ir_translator::IRTranslator,
            },
        },
        named::OnionNamed,
        object::{ObjectError, OnionObject, OnionStaticObject},
        tuple::OnionTuple,
    },
    unwrap_object, GC,
};

use onion_frontend::compile::build_code;

/// REPL专用执行器
pub struct ReplExecutor {
    /// 存储历史执行结果的元组
    out_tuple: OnionStaticObject,
}

impl ReplExecutor {
    pub fn new() -> Self {
        // 创建空的Out元组用于存储历史结果
        let empty_tuple = OnionTuple::new_static(vec![]);
        Self {
            out_tuple: empty_tuple,
        }
    }

    /// 获取历史执行结果数量
    pub fn history_count(&self) -> usize {
        if let OnionObject::Tuple(tuple) = self.out_tuple.weak() {
            tuple.elements.len()
        } else {
            0
        }
    }

    /// 获取最后一次执行的结果
    pub fn get_last_result(&self) -> Option<OnionStaticObject> {
        if let OnionObject::Tuple(tuple) = self.out_tuple.weak() {
            tuple
                .elements
                .last()
                .map(|obj| OnionStaticObject::new(obj.clone()))
        } else {
            None
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
        dir_stack: &mut onion_frontend::dir_stack::DirStack,
    ) -> Result<(), String> {
        let ir_package =
            build_code(code, dir_stack).map_err(|e| format!("Compilation failed: {}", e))?;

        self.execute_ir_package(&ir_package)
    }

    fn execute_ir_package(&mut self, ir_package: &IRPackage) -> Result<(), String> {
        let mut translator = IRTranslator::new(ir_package);
        translator
            .translate()
            .map_err(|e| format!("IR translation failed: {:?}", e))?;

        let vm_instructions_package = translator.get_result();
        self.execute_bytecode_package(&vm_instructions_package)
    }

    fn execute_bytecode_package(
        &mut self,
        vm_instructions_package: &VMInstructionPackage,
    ) -> Result<(), String> {
        // 创建标准库对象
        let stdlib_pair = OnionNamed::new_static(
            &OnionObject::String("stdlib".to_string()).stabilize(),
            &crate::stdlib::build_module(),
        );

        // 创建Out参数对象 - 直接使用当前的out_tuple
        let out_pair = OnionNamed::new_static(
            &OnionObject::String("Out".to_string()).stabilize(),
            &self.out_tuple.clone(),
        );

        // 创建Lambda定义，包含stdlib和Out两个参数
        let lambda = OnionLambdaDefinition::new_static(
            &OnionTuple::new_static(vec![&stdlib_pair, &out_pair]),
            LambdaBody::Instruction(Box::new(OnionObject::InstructionPackage(
                vm_instructions_package.clone(),
            ))),
            None,
            None,
            "__main__".to_string(),
        );

        let OnionObject::Lambda(lambda_ref) = lambda.weak() else {
            return Err("Failed to create Lambda definition".to_string());
        };

        let args = OnionTuple::new_static(vec![]);

        // 绑定参数
        let assigned_argument = lambda_ref
            .with_parameter(|param| {
                unwrap_object!(param, OnionObject::Tuple)?
                    .clone_and_named_assignment(unwrap_object!(args.weak(), OnionObject::Tuple)?)
            })
            .map_err(|e| format!("Failed to assign arguments to Lambda: {:?}", e))?;

        let lambda = lambda_ref
            .create_runnable(assigned_argument, &lambda, &mut GC::new())
            .map_err(|e| format!("Failed to create runnable Lambda: {:?}", e))?;

        // 初始化调度器和GC
        let mut scheduler = Scheduler::new(vec![lambda]);
        let mut gc = GC::new();

        // 执行代码
        loop {
            match scheduler.step(&mut gc) {
                Ok(step_result) => match step_result {
                    StepResult::Continue => {
                        // 继续下一步
                    }
                    StepResult::NewRunnable(_) => {
                        // 添加新的可执行对象到调度器
                        unreachable!()
                    }
                    StepResult::Return(result) => {
                        let result = unwrap_object!(result.weak(), OnionObject::Pair)
                            .map_err(|e| format!("Failed to unwrap result: {:?}", e))?;
                        let success = unwrap_object!(result.get_key(), OnionObject::Boolean)
                            .map_err(|e| format!("Failed to get success key: {:?}", e))?;

                        if !success {
                            let error_msg = unwrap_object!(result.get_value(), OnionObject::String)
                                .map_err(|e| format!("Failed to get error message: {:?}", e))?;
                            println!("{} {}", "Error:".red().bold(), error_msg);
                            return Err("Execution failed".to_string());
                        }

                        // 获取执行结果并存储到Out元组中
                        let result_value = result.get_value();

                        // 将结果添加到Out元组中
                        self.add_result_to_out(result_value.clone());
                        let is_undefined = result_value
                            .with_data(|data| {
                                Ok(unwrap_object!(data, OnionObject::Undefined).is_ok())
                            })
                            .map_err(|e| {
                                format!("Failed to check if result is Undefined: {:?}", e)
                            })?;
                        if is_undefined {
                            break; // 如果结果是Undefined，则不需要打印
                        }
                        // 打印结果
                        let result_str = result_value
                            .to_string()
                            .map_err(|e| format!("Failed to get result value: {:?}", e))?;
                        println!("{} {}", "Result:".cyan(), result_str);
                        break;
                    }
                    StepResult::Error(err) => {
                        return Err(format!("Runtime error: {}", err));
                    }
                },
                Err(e) => {
                    return Err(format!("Execution error: {}", e));
                }
            }
        }

        Ok(())
    }

    /// 将结果添加到Out元组中
    fn add_result_to_out(&mut self, result: OnionObject) {
        if let OnionObject::Tuple(tuple) = self.out_tuple.weak() {
            let mut new_elements = tuple.elements.clone();
            new_elements.push(result);
            self.out_tuple = OnionTuple::new_static_no_ref(
                new_elements
                    .into_iter()
                    .map(|obj| OnionStaticObject::new(obj))
                    .collect(),
            );
        }
    }
}

impl Default for ReplExecutor {
    fn default() -> Self {
        Self::new()
    }
}
