use std::{
    cell::{Ref, RefCell},
    sync::Arc,
};

use arc_gc::gc::GC;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::object::{ObjectError, OnionObject, OnionStaticObject},
};

use super::{
    context::{Context, Frame},
    vm_instructions::{
        self,
        instruction_set::{VMInstruction, VMInstructionPackage},
        opcode::{Instruction32, ProcessedOpcode},
    },
};

type InstructionHandler =
    fn(&mut OnionLambdaRunnable, &ProcessedOpcode, &mut GC) -> Result<StepResult, RuntimeError>;

pub struct OnionLambdaRunnable {
    pub(crate) argument: OnionStaticObject,
    pub(crate) context: Context,
    pub(crate) result: OnionStaticObject,
    pub(crate) error: Option<RuntimeError>,
    pub(crate) ip: isize, // Instruction pointer
    pub(crate) instruction: Arc<RefCell<VMInstructionPackage>>,

    pub(crate) instruction_table: Vec<InstructionHandler>,
}

impl OnionLambdaRunnable {
    pub fn new(
        argument: OnionStaticObject,
        instruction: Arc<RefCell<VMInstructionPackage>>,
        ip: isize,
    ) -> Result<Self, ObjectError> {
        let mut new_context = Context::new();
        Context::push_frame(
            &mut new_context,
            Frame::Normal(rustc_hash::FxHashMap::default(), Vec::new()),
        );

        let OnionObject::Tuple(tuple) = argument.weak() else {
            return Err(ObjectError::InvalidOperation(
                "Argument must be a tuple".to_string(),
            ));
        };

        for (_, item) in tuple.elements.iter().enumerate() {
            match item {
                OnionObject::Named(named) => {
                    named.get_key().with_data(|key| match key {
                        OnionObject::String(key_str) => {
                            new_context.let_variable(key_str, named.get_value().clone().stabilize())
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
        instruction_table[VMInstruction::Pop as usize] = vm_instructions::discard_top;

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
        instruction_table[VMInstruction::Mut as usize] = vm_instructions::heap_to_gc;
        instruction_table[VMInstruction::Const as usize] = vm_instructions::immutablize;
        instruction_table[VMInstruction::Fork as usize] = vm_instructions::fork_instruction;

        // 控制流
        instruction_table[VMInstruction::Call as usize] = vm_instructions::call_lambda;
        instruction_table[VMInstruction::Return as usize] = vm_instructions::return_value;
        instruction_table[VMInstruction::Jump as usize] = vm_instructions::jump;
        instruction_table[VMInstruction::JumpIfFalse as usize] = vm_instructions::jump_if_false;

        // 帧操作
        instruction_table[VMInstruction::NewFrame as usize] = vm_instructions::new_frame;
        instruction_table[VMInstruction::PopFrame as usize] = vm_instructions::pop_frame;
        instruction_table[VMInstruction::ResetStack as usize] = vm_instructions::clear_stack;

        // 模块操作
        //instruction_table[VMInstruction::Import as usize] = vm_instructions::import;

        // 特殊操作
        //instruction_table[VMInstruction::BindSelf as usize] = vm_instructions::bind_self;
        instruction_table[VMInstruction::Assert as usize] = vm_instructions::assert;

        Ok(OnionLambdaRunnable {
            argument,
            context: new_context,
            result: OnionStaticObject::default(),
            error: None,
            ip,
            instruction,
            instruction_table,
        })
    }
    pub fn borrow_instruction(&self) -> Result<Ref<VMInstructionPackage>, RuntimeError> {
        self.instruction
            .try_borrow()
            .map_err(|_| RuntimeError::DetailedError("Failed to borrow instruction".to_string()))
    }
}

impl Runnable for OnionLambdaRunnable {
    fn receive(&mut self, step_result: StepResult, gc: &mut GC) -> Result<(), RuntimeError> {
        if let StepResult::Return(result) = step_result {
            self.context.push_object(result)?;
            Ok(())
        } else {
            Err(RuntimeError::DetailedError(
                "receive not implemented for cases except `Return`".to_string(),
            ))
        }
    }
    fn step(&mut self, gc: &mut GC) -> Result<StepResult, RuntimeError> {
        if let Some(error) = &self.error {
            return Ok(StepResult::Error(error.clone()));
        }

        if self.ip < 0 || self.ip as usize >= self.borrow_instruction()?.get_code().len() {
            return Err(RuntimeError::DetailedError(
                "Instruction pointer out of bounds".to_string(),
            ));
        }

        let (code, raw, new_ip) = {
            let instruction = self.borrow_instruction()?;
            let opcode = instruction.get_code()[self.ip as usize];
            let mut ip = self.ip as usize;
            let mut instruction32 = Instruction32::new(instruction.get_code(), &mut ip);
            let code: Option<ProcessedOpcode> = instruction32.get_processed_opcode();
            (code, opcode, ip)
        };
        match code {
            Some(opcode) => {
                // println!(
                //     "Executing opcode: {} at IP: {}",
                //     opcode._to_string(),
                //     self.ip
                // );
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

    fn copy(&self, gc: &mut GC) -> Box<dyn Runnable> {
        Box::new(OnionLambdaRunnable {
            argument: self.argument.clone(),
            context: self.context.clone(),
            result: self.result.clone(),
            error: self.error.clone(),
            ip: self.ip,
            instruction: Arc::clone(&self.instruction),
            instruction_table: self.instruction_table.clone(),
        })
    }
}
