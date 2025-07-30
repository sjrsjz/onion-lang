use std::sync::Arc;

use arc_gc::gc::GC;
use rustc_hash::FxHashMap;
use unicode_width::UnicodeWidthStr;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::{
            build_dict_from_hashmap,
            vm_instructions::opcode::{
                self, OpcodeArgument, build_operand_argument, decode_opcode, get_operand_arg,
                get_processed_opcode,
            },
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    utils::find_line_and_col_from_source,
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
        instruction_table[VMInstruction::KeyOf as usize] = vm_instructions::key_of;
        instruction_table[VMInstruction::ValueOf as usize] = vm_instructions::value_of;
        instruction_table[VMInstruction::TypeOf as usize] = vm_instructions::type_of;
        instruction_table[VMInstruction::Swap as usize] = vm_instructions::swap;
        instruction_table[VMInstruction::LengthOf as usize] = vm_instructions::get_length;
        instruction_table[VMInstruction::Mut as usize] = vm_instructions::mutablize;
        instruction_table[VMInstruction::Const as usize] = vm_instructions::immutablize;
        instruction_table[VMInstruction::ForkInstruction as usize] =
            vm_instructions::fork_instruction;
        instruction_table[VMInstruction::Launch as usize] = vm_instructions::launch_thread;
        instruction_table[VMInstruction::Spawn as usize] = vm_instructions::spawn_task;
        instruction_table[VMInstruction::MakeAtomic as usize] = vm_instructions::make_atomic;
        instruction_table[VMInstruction::MakeAsync as usize] = vm_instructions::make_async;
        instruction_table[VMInstruction::MakeSync as usize] = vm_instructions::make_sync;

        // 控制流
        instruction_table[VMInstruction::Apply as usize] = vm_instructions::apply;
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
    pub(crate) context: Context,
    pub(crate) ip: isize,             // Instruction pointer
    pub(crate) ip_before_step: isize, // previous instruction pointer
    pub(crate) instruction: Arc<VMInstructionPackage>,
}

impl OnionLambdaRunnable {
    pub fn new(
        argument: &FxHashMap<String, OnionStaticObject>,
        capture: &FxHashMap<String, OnionObject>,
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
            .let_variable(index_arguments, build_dict_from_hashmap(argument))
            .map_err(|e| {
                RuntimeError::InvalidOperation(
                    format!("Failed to initialize 'arguments' variable: {}", e).into(),
                )
            })?;

        let pool = instruction.get_string_pool();

        for (argument_name, value) in argument.iter() {
            if let Some(index) = pool.iter().position(|s| s == argument_name) {
                new_context
                    .let_variable(index, value.clone())
                    .map_err(|e| {
                        RuntimeError::InvalidOperation(
                            format!("Failed to initialize argument '{}': {}", argument_name, e)
                                .into(),
                        )
                    })?;
            }
        }

        // 设置捕获的变量
        for (capture_name, value) in capture.iter() {
            if let Some(index) = pool.iter().position(|s| s == capture_name) {
                new_context
                    .let_variable(index, value.stabilize())
                    .map_err(|e| {
                        RuntimeError::InvalidOperation(
                            format!(
                                "Failed to initialize captured variable '{}': {}",
                                capture_name, e
                            )
                            .into(),
                        )
                    })?;
            }
        }

        Ok(OnionLambdaRunnable {
            context: new_context,
            ip,
            ip_before_step: ip,
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
            self.ip_before_step = pending_ip as isize; // 保存上一步的 IP

            match handler(self, &opcode, gc) {
                StepResult::Continue => continue,
                StepResult::Error(e) => {
                    self.ip = pending_ip as isize; // 恢复 IP
                    return StepResult::Error(e);
                }
                v => return v,
            }
        }

        StepResult::Continue
    }

    fn format_context(&self) -> String {
        let ip = self.ip_before_step as usize;
        let mut parts = Vec::new();

        // 尝试获取详细的源码位置信息
        if let Some(location) = get_source_location_for_ip(&self.instruction, ip) {
            // --- Part 1: 高级、人类可读的上下文 ---
            parts.push(format!("-> at {}:{}", location.line, location.column));
            parts.push(location.code_snippet);
        } else {
            // --- Part 1 (回退): 原始的、无源码的上下文 ---
            let disassembly = disassemble_instruction(&self.instruction, ip); // 假设这个函数存在
            parts.push(format!("-> Executing VM Code at ip: {}", ip));
            parts.push(format!("  - Current Instruction: {}", disassembly));
        }

        // --- Part 2: VM 调用栈和操作数栈状态 ---
        let context_state = self.context.format_context(self.instruction.as_ref());
        parts.push("\n--- Lambda Execution State ---".to_string());
        parts.push(context_state);

        parts.join("\n")
    }
}

/// 反汇编函数：将给定 IP 位置的指令转换为人类可读的字符串。
///
/// 此版本适配了变长指令集，其中第一个 u32 word 是元数据，
/// 后续的 words 是操作数的实际值。
pub fn disassemble_instruction(package: &VMInstructionPackage, ip: usize) -> String {
    let code = package.get_code();
    if ip >= code.len() {
        return format!("<IP:{} out of bounds>", ip);
    }

    // --- 1. 解码元数据 ---
    // 我们需要一个可变的指针来模拟执行过程，但不能影响原始 ip
    let mut temp_ip = ip;
    let opcode_word = opcode::take_u32(code, &mut temp_ip);
    let decoded_opcode = decode_opcode(opcode_word);

    // --- 2. 获取操作数的原始值 ---
    // 注意：这里我们只读取值，但不解释它们，因为格式化依赖于标志
    let mut next_ip = temp_ip; // 保存操作数开始的位置
    let raw_operand1 = get_operand_arg(code, &mut next_ip, decoded_opcode.operand1());
    let raw_operand2 = get_operand_arg(code, &mut next_ip, decoded_opcode.operand2());
    let raw_operand3 = get_operand_arg(code, &mut next_ip, decoded_opcode.operand3());

    // --- 3. 格式化每个操作数 ---
    // 这是一个辅助闭包，用于将单个操作数格式化为字符串
    let format_operand = |flag: u8, raw_value: u64| -> Option<String> {
        // 使用你的 `build_operand_argument` 来获取结构化的操作数类型
        let arg = build_operand_argument(flag, raw_value);

        match arg {
            OpcodeArgument::None => None,
            OpcodeArgument::Int32(v) => Some(format!("{}", v)),
            OpcodeArgument::Int64(v) => Some(format!("{}L", v)), // 'L' for long
            OpcodeArgument::Float32(v) => Some(format!("{}f", v)), // 'f' for float
            OpcodeArgument::Float64(v) => Some(format!("{}", v)),
            OpcodeArgument::String(idx) => {
                let s = package
                    .get_string_pool()
                    .get(idx as usize)
                    .map(|s| format!("Str({}) -> \"{}\"", idx, s))
                    .unwrap_or_else(|| format!("Str({}) -> <Invalid>", idx));
                Some(s)
            }
            OpcodeArgument::ByteArray(idx) => {
                let b = package
                    .get_bytes_pool()
                    .get(idx as usize)
                    .map(|b| format!("Bytes({}) -> {:X?}", idx, b))
                    .unwrap_or_else(|| format!("Bytes({}) -> <Invalid>", idx));
                Some(b)
            }
        }
    };

    let op1_str = format_operand(decoded_opcode.operand1(), raw_operand1);
    let op2_str = format_operand(decoded_opcode.operand2(), raw_operand2);
    let op3_str = format_operand(decoded_opcode.operand3(), raw_operand3);

    // --- 4. 组合最终的字符串 ---
    let instruction_name = VMInstruction::from_opcode(decoded_opcode.instruction())
        .map(|instr| format!("{:?}", instr))
        .unwrap_or_else(|| format!("Invalid({})", decoded_opcode.instruction()));

    let parts = vec![op1_str, op2_str, op3_str];
    // 过滤掉 None 的操作数
    let operands_str = parts
        .into_iter()
        .filter_map(|p| p)
        .collect::<Vec<String>>()
        .join(", ");

    if operands_str.is_empty() {
        instruction_name
    } else {
        format!("{} {}", instruction_name, operands_str)
    }
}
#[derive(Debug)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    // 包含高亮的代码片段，例如：
    //   "  12 | let x = 10 / 0;\n"
    //   "     |         ^^^^^^"
    pub code_snippet: String,
}

pub fn get_source_location_for_ip(
    package: &VMInstructionPackage,
    ip: usize,
) -> Option<SourceLocation> {
    let source = package.get_source().as_ref()?;
    let debug_info = package.get_debug_info().get(&ip)?;

    let (span_start, span_end) = debug_info.token_span();

    if span_start >= span_end {
        return None;
    }

    let (line_idx, col_char_idx) = find_line_and_col_from_source(span_start, source);
    let line_content = source.lines().nth(line_idx).unwrap_or("");

    let error_token_text: String = source
        .chars()
        .skip(span_start)
        .take(span_end - span_start)
        .collect();

    let display_offset = line_content
        .chars()
        .take(col_char_idx)
        .collect::<String>()
        .width();

    let underline_width = error_token_text.width();

    let line_num_width = 5;
    let line_num = line_idx + 1;

    let code_snippet = format!(
        " {:>width$} | {}\n {empty:>width$} | {padding}{underline}",
        line_num,
        line_content,
        empty = "",
        padding = " ".repeat(display_offset),
        underline = "^".repeat(underline_width),
        width = line_num_width
    );

    Some(SourceLocation {
        line: line_num,
        column: col_char_idx + 1,
        code_snippet,
    })
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
