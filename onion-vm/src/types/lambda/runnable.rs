use std::sync::Arc;

use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::vm_instructions::opcode::get_processed_opcode,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
};

use super::{
    context::{Context, Frame},
    vm_instructions::{
        self,
        instruction_set::{VMInstruction, VMInstructionPackage},
        opcode::ProcessedOpcode,
    },
};

type InstructionHandler =
    fn(&mut OnionLambdaRunnable, &ProcessedOpcode, &mut GC<OnionObjectCell>) -> StepResult;

// 静态指令表，在程序启动时初始化一次
static INSTRUCTION_TABLE: std::sync::LazyLock<Vec<InstructionHandler>> =
    std::sync::LazyLock::new(|| {
        let mut instruction_table: Vec<InstructionHandler> = vec![
        |_, opcode, _| StepResult::Error(RuntimeError::DetailedError(format!("Invalid instruction: {:?}", opcode).into())); // 默认处理函数 - 返回无效指令错误
        256 // 数组大小，确保能容纳所有可能的操作码
    ];

        // 使用枚举值作为索引，填充对应的处理函数
        // 栈操作
        instruction_table[VMInstruction::LoadNull as usize] = vm_instructions::load_null;
        instruction_table[VMInstruction::LoadInt32 as usize] = vm_instructions::load_int;
        instruction_table[VMInstruction::LoadInt64 as usize] = vm_instructions::load_int;
        instruction_table[VMInstruction::LoadFloat32 as usize] = vm_instructions::load_float;
        instruction_table[VMInstruction::LoadFloat64 as usize] = vm_instructions::load_float;
        instruction_table[VMInstruction::LoadString as usize] = vm_instructions::load_string;
        instruction_table[VMInstruction::LoadBytes as usize] = vm_instructions::load_bytes;
        instruction_table[VMInstruction::LoadBool as usize] = vm_instructions::load_bool;
        instruction_table[VMInstruction::LoadLambda as usize] = vm_instructions::load_lambda;
        instruction_table[VMInstruction::LoadUndefined as usize] = vm_instructions::load_undefined;

        // 数据结构构建
        instruction_table[VMInstruction::BuildTuple as usize] = vm_instructions::build_tuple;
        instruction_table[VMInstruction::BuildKeyValue as usize] = vm_instructions::build_keyval;
        instruction_table[VMInstruction::BuildNamed as usize] = vm_instructions::build_named;
        instruction_table[VMInstruction::BuildRange as usize] = vm_instructions::build_range;
        instruction_table[VMInstruction::BuildSet as usize] = vm_instructions::build_set;
        // 二元操作符
        instruction_table[VMInstruction::BinaryIn as usize] = vm_instructions::is_in;
        instruction_table[VMInstruction::BinaryIs as usize] = vm_instructions::check_is_same_object;

        instruction_table[VMInstruction::BinaryAdd as usize] = vm_instructions::binary_add;
        instruction_table[VMInstruction::BinarySub as usize] = vm_instructions::binary_subtract;
        instruction_table[VMInstruction::BinaryMul as usize] = vm_instructions::binary_multiply;
        instruction_table[VMInstruction::BinaryDiv as usize] = vm_instructions::binary_divide;
        instruction_table[VMInstruction::BinaryMod as usize] = vm_instructions::binary_modulus;
        instruction_table[VMInstruction::BinaryPow as usize] = vm_instructions::binary_power;
        instruction_table[VMInstruction::BinaryBitAnd as usize] =
            vm_instructions::binary_bitwise_and;
        instruction_table[VMInstruction::BinaryBitOr as usize] = vm_instructions::binary_bitwise_or;
        instruction_table[VMInstruction::BinaryBitXor as usize] =
            vm_instructions::binary_bitwise_xor;
        instruction_table[VMInstruction::BinaryShl as usize] = vm_instructions::binary_shift_left;
        instruction_table[VMInstruction::BinaryShr as usize] = vm_instructions::binary_shift_right;
        instruction_table[VMInstruction::BinaryEq as usize] = vm_instructions::binary_equal;
        instruction_table[VMInstruction::BinaryNe as usize] = vm_instructions::binary_not_equal;
        instruction_table[VMInstruction::BinaryGt as usize] = vm_instructions::binary_greater;
        instruction_table[VMInstruction::BinaryLt as usize] = vm_instructions::binary_less;
        instruction_table[VMInstruction::BinaryGe as usize] = vm_instructions::binary_greater_equal;
        instruction_table[VMInstruction::BinaryLe as usize] = vm_instructions::binary_less_equal;
        instruction_table[VMInstruction::MapTo as usize] = vm_instructions::map_to;

        // 一元操作
        instruction_table[VMInstruction::UnaryBitNot as usize] = vm_instructions::unary_bitwise_not;
        instruction_table[VMInstruction::UnaryAbs as usize] = vm_instructions::unary_plus;
        instruction_table[VMInstruction::UnaryNeg as usize] = vm_instructions::unary_minus;

        // 变量与引用
        instruction_table[VMInstruction::StoreVar as usize] = vm_instructions::let_var;
        instruction_table[VMInstruction::LoadVar as usize] = vm_instructions::get_var;
        instruction_table[VMInstruction::SetValue as usize] = vm_instructions::set_var;
        instruction_table[VMInstruction::GetAttr as usize] = vm_instructions::get_attr;
        instruction_table[VMInstruction::IndexOf as usize] = vm_instructions::index_of;
        instruction_table[VMInstruction::KeyOf as usize] = vm_instructions::key_of;
        instruction_table[VMInstruction::ValueOf as usize] = vm_instructions::value_of;
        instruction_table[VMInstruction::TypeOf as usize] = vm_instructions::type_of;
        instruction_table[VMInstruction::ShallowCopy as usize] = vm_instructions::copy;
        instruction_table[VMInstruction::Swap as usize] = vm_instructions::swap;
        instruction_table[VMInstruction::LengthOf as usize] = vm_instructions::get_length;
        instruction_table[VMInstruction::Mut as usize] = vm_instructions::mutablize;
        instruction_table[VMInstruction::Const as usize] = vm_instructions::immutablize;
        instruction_table[VMInstruction::ForkInstruction as usize] =
            vm_instructions::fork_instruction;
        instruction_table[VMInstruction::Launch as usize] = vm_instructions::launch_thread;
        instruction_table[VMInstruction::Spawn as usize] = vm_instructions::spawn_task;

        // 控制流
        instruction_table[VMInstruction::Call as usize] = vm_instructions::call_lambda;
        instruction_table[VMInstruction::AsyncCall as usize] = vm_instructions::async_call;
        instruction_table[VMInstruction::SyncCall as usize] = vm_instructions::sync_call;
        instruction_table[VMInstruction::Return as usize] = vm_instructions::return_value;
        instruction_table[VMInstruction::Raise as usize] = vm_instructions::raise;
        instruction_table[VMInstruction::Jump as usize] = vm_instructions::jump;
        instruction_table[VMInstruction::JumpIfFalse as usize] = vm_instructions::jump_if_false;

        // 帧操作
        instruction_table[VMInstruction::NewFrame as usize] = vm_instructions::new_frame;
        instruction_table[VMInstruction::PopFrame as usize] = vm_instructions::pop_frame;
        instruction_table[VMInstruction::ResetStack as usize] = vm_instructions::clear_stack;
        instruction_table[VMInstruction::Pop as usize] = vm_instructions::discard_top;

        // 模块操作
        instruction_table[VMInstruction::Import as usize] = vm_instructions::import;

        instruction_table[VMInstruction::Assert as usize] = vm_instructions::assert;

        instruction_table
    });

pub struct OnionLambdaRunnable {
    pub(crate) argument: OnionStaticObject,
    pub(crate) result: OnionStaticObject,
    pub(crate) this_lambda: OnionStaticObject,
    pub(crate) context: Context,
    pub(crate) ip: isize, // Instruction pointer
    pub(crate) instruction: Arc<VMInstructionPackage>,
}

impl OnionLambdaRunnable {
    pub fn new(
        argument: OnionStaticObject,
        self_object: &OnionObject,
        this_lambda: &OnionStaticObject,
        instruction: Arc<VMInstructionPackage>,
        ip: isize,
    ) -> Result<Self, RuntimeError> {
        let mut new_context = Context::new();
        Context::push_frame(
            &mut new_context,
            Frame {
                variables: rustc_hash::FxHashMap::default(),
                stack: Vec::new(),
            },
        );

        let (index_this, index_self, index_arguments) = {
            let string_pool = instruction.get_string_pool();

            let index_this = string_pool
                .iter()
                .position(|s| s == "this")
                .ok_or_else(|| {
                    RuntimeError::InvalidOperation(
                        "Missing required variable 'this' in string pool"
                            .to_string()
                            .into(),
                    )
                })?;

            let index_self = string_pool
                .iter()
                .position(|s| s == "self")
                .ok_or_else(|| {
                    RuntimeError::InvalidOperation(
                        "Missing required variable 'self' in string pool"
                            .to_string()
                            .into(),
                    )
                })?;

            let index_arguments = string_pool
                .iter()
                .position(|s| s == "arguments")
                .ok_or_else(|| {
                    RuntimeError::InvalidOperation(
                        "Missing required variable 'arguments' in string pool"
                            .to_string()
                            .into(),
                    )
                })?;

            (index_this, index_self, index_arguments)
        };

        // 设置内置变量
        new_context
            .let_variable(index_this, this_lambda.clone())
            .map_err(|e| {
                RuntimeError::InvalidOperation(
                    format!("Failed to initialize 'this' variable: {}", e).into(),
                )
            })?;

        new_context
            .let_variable(index_self, self_object.stabilize())
            .map_err(|e| {
                RuntimeError::InvalidOperation(
                    format!("Failed to initialize 'self' variable: {}", e).into(),
                )
            })?;

        new_context
            .let_variable(index_arguments, argument.clone())
            .map_err(|e| {
                RuntimeError::InvalidOperation(
                    format!("Failed to initialize 'arguments' variable: {}", e).into(),
                )
            })?;

        let pool = instruction.get_string_pool();

        argument.weak().with_data(|data| {
            if let OnionObject::Tuple(tuple) = data {
                for item in tuple.get_elements().iter() {
                    match item {
                        OnionObject::Named(named) => {
                            named.get_key().with_data(|key| match key {
                                OnionObject::String(key_str) => {
                                    match pool.iter().position(|s| s.eq(key_str.as_ref())) {
                                        Some(index) => new_context
                                            .let_variable(index, named.get_value().stabilize()),
                                        None => {
                                            // do nothing because the runnable does not need this variable
                                            Ok(())
                                        }
                                    }
                                }
                                _ => Ok(()),
                            })?;
                        }
                        _ => {}
                    }
                }
                Ok(())
            } else {
                Err(RuntimeError::InvalidOperation(
                    "Argument must be a tuple".to_string().into(),
                ))
            }
        })?;

        Ok(OnionLambdaRunnable {
            argument,
            this_lambda: this_lambda.clone(),
            result: OnionStaticObject::default(),
            context: new_context,
            ip,
            instruction,
        })
    }
}

impl Runnable for OnionLambdaRunnable {
    fn receive(
        &mut self,
        step_result: &StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        if let StepResult::Return(result) = step_result {
            self.context.push_object(result.as_ref().clone())?;
            Ok(())
        } else {
            Err(RuntimeError::DetailedError(
                "receive not implemented for cases except `Return`"
                    .to_string()
                    .into(),
            ))
        }
    }
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        const MAX_INLINE_STEPS: usize = 1024;

        let mut steps = 0;

        // 获取代码的原始指针和长度，避免借用冲突
        let (code_ptr, code_len) = {
            let code = self.instruction.get_code();
            (code.as_ptr(), code.len())
        };

        loop {
            if steps >= MAX_INLINE_STEPS {
                break;
            }
            steps += 1;

            let mut ip = self.ip as usize;
            if ip >= code_len {
                return StepResult::Error(RuntimeError::DetailedError(
                    "Instruction pointer out of bounds".to_string().into(),
                ));
            }

            // 使用 unsafe 从原始指针创建切片
            let code = unsafe { std::slice::from_raw_parts(code_ptr, code_len) };
            let pending_ip = ip;
            let opcode = get_processed_opcode(code, &mut ip);

            let handler = unsafe { *INSTRUCTION_TABLE.get_unchecked(opcode.instruction as usize) };
            self.ip = ip as isize;

            match handler(self, &opcode, gc) {
                StepResult::Continue => continue,
                StepResult::Error(RuntimeError::Pending) => {
                    // 如果是 Pending 状态，继续等待
                    self.ip = pending_ip as isize; // 恢复 IP
                    return StepResult::Error(RuntimeError::Pending);
                }
                v => return v,
            }
        }

        StepResult::Continue
    }
    fn copy(&self) -> Box<dyn Runnable> {
        Box::new(OnionLambdaRunnable {
            argument: self.argument.clone(),
            this_lambda: self.this_lambda.clone(),
            result: self.result.clone(),
            context: self.context.clone(),
            ip: self.ip,
            instruction: self.instruction.clone(),
        })
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        let mut stack_json_array = serde_json::Value::Array(vec![]);
        for frame in &self.context.frames {
            let frame_json = frame.format_context();
            stack_json_array.as_array_mut().unwrap().push(frame_json);
        }
        // {type: "OnionLambdaRunnable", frames: frame_json_array}
        Ok(serde_json::json!({
            "type": "lambda_runnable",
            "frames": stack_json_array,
            "ip": self.ip,
            "argument": self.argument.to_string(),
            "this_lambda": self.this_lambda.to_string(),
            "result": self.result.to_string(),
        }))
    }
}
#[cfg(test)]
mod size_tests {
    use super::*;

    #[test]
    fn print_sizes() {
        println!("StepResult size: {}", std::mem::size_of::<StepResult>());
        println!("RuntimeError size: {}", std::mem::size_of::<RuntimeError>());
        println!("StepResult size: {}", std::mem::size_of::<StepResult>());
    }
}
