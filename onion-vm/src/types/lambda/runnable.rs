use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
};

use super::{
    context::{Context, Frame},
    vm_instructions::{
        self,
        instruction_set::{VMInstruction, VMInstructionPackage},
        opcode::{Instruction32, ProcessedOpcode},
    },
};

type InstructionHandler = fn(
    &mut OnionLambdaRunnable,
    &ProcessedOpcode,
    &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError>;

pub struct OnionLambdaRunnable {
    pub(crate) argument: OnionStaticObject,
    pub(crate) result: OnionStaticObject,
    pub(crate) capture: OnionStaticObject,
    pub(crate) self_object: OnionStaticObject,
    pub(crate) this_lambda: OnionStaticObject,
    pub(crate) context: Context,
    pub(crate) error: Option<RuntimeError>,
    pub(crate) ip: isize, // Instruction pointer
    pub(crate) instruction: Box<VMInstructionPackage>,

    pub(crate) instruction_table: Vec<InstructionHandler>,
}

impl OnionLambdaRunnable {
    pub fn new(
        argument: OnionStaticObject,
        capture: OnionStaticObject,
        self_object: OnionStaticObject,
        this_lambda: OnionStaticObject,
        instruction: Box<VMInstructionPackage>,
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

        let OnionObject::Tuple(tuple) = &*argument.weak().try_borrow()? else {
            return Err(RuntimeError::InvalidOperation(
                "Argument must be a tuple".to_string(),
            ));
        };

        let (index_this, index_self, index_arguments) = {
            let string_pool = instruction.get_string_pool();

            let index_this = string_pool
                .iter()
                .position(|s| s == "this")
                .ok_or_else(|| {
                    RuntimeError::InvalidOperation(
                        "Missing required variable 'this' in string pool".to_string(),
                    )
                })?;

            let index_self = string_pool
                .iter()
                .position(|s| s == "self")
                .ok_or_else(|| {
                    RuntimeError::InvalidOperation(
                        "Missing required variable 'self' in string pool".to_string(),
                    )
                })?;

            let index_arguments = string_pool
                .iter()
                .position(|s| s == "arguments")
                .ok_or_else(|| {
                    RuntimeError::InvalidOperation(
                        "Missing required variable 'arguments' in string pool".to_string(),
                    )
                })?;

            (index_this, index_self, index_arguments)
        };

        // 设置内置变量
        new_context
            .let_variable(index_this, this_lambda.clone())
            .map_err(|e| {
                RuntimeError::InvalidOperation(format!(
                    "Failed to initialize 'this' variable: {}",
                    e
                ))
            })?;

        new_context
            .let_variable(index_self, self_object.clone())
            .map_err(|e| {
                RuntimeError::InvalidOperation(format!(
                    "Failed to initialize 'self' variable: {}",
                    e
                ))
            })?;

        new_context
            .let_variable(index_arguments, argument.clone())
            .map_err(|e| {
                RuntimeError::InvalidOperation(format!(
                    "Failed to initialize 'arguments' variable: {}",
                    e
                ))
            })?;

        for item in tuple.elements.iter() {
            match &*item.try_borrow()? {
                OnionObject::Named(named) => {
                    named.get_key().with_data(|key| match key {
                        OnionObject::String(key_str) => {
                            match instruction
                                .get_string_pool()
                                .iter()
                                .position(|s| *s == *key_str)
                            {
                                Some(index) => new_context
                                    .let_variable(index, named.get_value().clone().stabilize()),
                                None => {
                                    // do nothing because the runnable does not need this variable
                                    Ok(())
                                }
                            }
                            // new_context.let_variable(
                            //     key_str.clone(),
                            //     named.get_value().clone().stabilize(),
                            // )
                        }
                        _ => Ok(()),
                    })?;
                }
                _ => {}
            }
        }

        let mut instruction_table: Vec<InstructionHandler> = vec![
        |_, opcode, _| Err(RuntimeError::DetailedError(format!("Invalid instruction: {:?}", opcode))); // 默认处理函数 - 返回无效指令错误
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

        // 控制流
        instruction_table[VMInstruction::Call as usize] = vm_instructions::call_lambda;
        instruction_table[VMInstruction::AsyncCall as usize] = vm_instructions::async_call;
        instruction_table[VMInstruction::SyncCall as usize] = vm_instructions::sync_call;
        instruction_table[VMInstruction::Return as usize] = vm_instructions::return_value;
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

        Ok(OnionLambdaRunnable {
            argument: argument.clone(),
            capture,
            self_object,
            this_lambda,
            result: OnionStaticObject::default(),
            context: new_context,
            error: None,
            ip,
            instruction,
            instruction_table,
        })
    }
}

impl Runnable for OnionLambdaRunnable {
    fn receive(
        &mut self,
        step_result: StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        if let StepResult::Return(result) = step_result {
            self.context.push_object(result)?;
            Ok(())
        } else {
            Err(RuntimeError::DetailedError(
                "receive not implemented for cases except `Return`".to_string(),
            ))
        }
    }
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> Result<StepResult, RuntimeError> {
        if let Some(error) = &self.error {
            return Ok(StepResult::Error(error.clone()));
        }

        let (code, raw, new_ip) = {
            let code = self.instruction.get_code();
            if self.ip < 0 || self.ip as usize >= code.len() {
                return Err(RuntimeError::DetailedError(
                    "Instruction pointer out of bounds".to_string(),
                ));
            }
            let opcode = code[self.ip as usize];
            let mut ip = self.ip as usize;
            let mut instruction32 = Instruction32::new(code, &mut ip);
            let code = instruction32.get_processed_opcode();
            (code, opcode, ip)
        };
        match code {
            Some(opcode) => {
                let handler = self
                    .instruction_table
                    .get(opcode.instruction as usize)
                    .ok_or_else(|| {
                        RuntimeError::DetailedError(format!(
                            "No handler for opcode: {}",
                            opcode.instruction
                        ))
                    })?;
                self.ip = new_ip as isize;
                let result = handler(self, &opcode, gc)?;
                return Ok(result);
            }
            None => {
                self.error = Some(RuntimeError::DetailedError(format!(
                    "Invalid instruction format at IP {}: {}",
                    self.ip, raw
                )));
                return Err(self.error.clone().unwrap());
            }
        }
    }

    fn copy(&self, _gc: &mut GC<OnionObjectCell>) -> Box<dyn Runnable> {
        Box::new(OnionLambdaRunnable {
            argument: self.argument.clone(),
            capture: self.capture.clone(),
            self_object: self.self_object.clone(),
            this_lambda: self.this_lambda.clone(),
            result: self.result.clone(),
            context: self.context.clone(),
            error: self.error.clone(),
            ip: self.ip,
            instruction: self.instruction.clone(),
            instruction_table: self.instruction_table.clone(),
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
            "capture": self.capture.to_string(),
            "self_object": self.self_object.to_string(),
            "this_lambda": self.this_lambda.to_string(),
            "result": self.result.to_string(),
        }))
    }
}
