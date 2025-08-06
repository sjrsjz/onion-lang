//! Onion Lambda 可执行模块。
//!
//! 提供 Onion 语言中 Lambda 函数的虚拟机执行环境，包括指令执行、调用栈管理、
//! 错误处理和调试信息输出等核心功能。支持高性能的字节码执行和完整的运行时环境。

use std::{ptr::addr_eq, sync::Arc};

use arc_gc::gc::GC;
use unicode_width::UnicodeWidthStr;

use crate::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::vm_instructions::opcode::{
            self, OpcodeArgument, build_operand_argument, decode_opcode, get_operand_arg,
            get_processed_opcode,
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    utils::{fastmap::OnionFastMap, find_line_and_col_from_source},
};

use super::{
    context::{Context, Frame},
    vm_instructions::{
        self,
        instruction_set::{VMInstruction, VMInstructionPackage},
        opcode::ProcessedOpcode,
    },
};

/// 指令处理函数类型定义。
///
/// 每个虚拟机指令对应一个处理函数，用于执行具体的操作逻辑。
///
/// # 参数
/// - `&mut OnionLambdaRunnable`: 可执行对象的可变引用，用于访问执行上下文和状态
/// - `&ProcessedOpcode`: 已解码的操作码，包含指令类型和操作数
/// - `&mut GC<OnionObjectCell>`: 垃圾收集器的可变引用，用于内存管理
///
/// # 返回
/// 执行步骤的结果，包括继续执行、返回值、错误等状态
type InstructionHandler =
    fn(&mut OnionLambdaRunnable, &ProcessedOpcode, &mut GC<OnionObjectCell>) -> StepResult;

/// 静态指令表，在程序启动时初始化一次。
///
/// 使用指令枚举值作为索引，直接映射到对应的处理函数，
/// 实现 O(1) 时间复杂度的指令分发，提升虚拟机执行性能。
///
/// 表大小固定为 256，覆盖所有可能的 8 位操作码值。
/// 未定义的操作码将映射到默认错误处理函数。
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
        instruction_table[VMInstruction::BuildKeyValue as usize] = vm_instructions::build_pair;
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

/// Onion Lambda 可执行对象。
///
/// 表示一个可执行的 Lambda 函数实例，包含完整的执行上下文、
/// 指令指针和字节码指令包。每个 Lambda 调用都会创建一个新的
/// 可执行对象实例，提供独立的执行环境。
///
/// # 执行模型
/// - 基于栈的虚拟机架构
/// - 支持增量执行和暂停/恢复
/// - 提供完整的调试信息和错误处理
/// - 支持垃圾回收和内存管理
///
/// # 示例
/// ```ignore
/// let runnable = OnionLambdaRunnable::new(
///     &arguments,     // 函数参数
///     &captures,      // 闭包捕获的变量
///     &self_object,   // self 引用
///     &this_lambda,   // this 引用
///     instruction,    // 字节码指令包
///     0,              // 起始指令指针
/// )?;
/// ```
pub struct OnionLambdaRunnable {
    /// 执行上下文，包含变量作用域、操作数栈等运行时状态
    pub(super) context: Context,
    /// 当前指令指针，指向下一条要执行的指令
    pub(super) ip: isize,
    /// 执行步骤前的指令指针，用于错误报告和调试
    pub(super) ip_before_step: isize,
    /// 字节码指令包，包含指令序列、常量池等数据
    pub(super) instruction: Arc<VMInstructionPackage>,
}

impl OnionLambdaRunnable {
    /// 创建新的 Lambda 可执行对象。
    ///
    /// 初始化完整的执行环境，包括变量绑定、作用域设置和内置变量配置。
    /// 会验证参数池和捕获池与指令包的字符串池一致性，确保执行安全。
    ///
    /// # 参数
    /// - `argument`: 函数参数映射，键为参数名索引，值为参数值
    /// - `capture`: 闭包捕获的变量映射，键为变量名索引，值为变量值
    /// - `self_object`: self 引用对象，用于方法调用
    /// - `this_lambda`: this 引用对象，指向当前 Lambda 函数
    /// - `instruction`: 字节码指令包，包含可执行代码和元数据
    /// - `ip`: 起始指令指针位置
    ///
    /// # 返回
    /// - `Ok(OnionLambdaRunnable)`: 成功创建的可执行对象
    /// - `Err(RuntimeError)`: 初始化失败，如缺少必要变量或池不匹配
    ///
    /// # 错误
    /// - `InvalidOperation`: 字符串池不匹配或缺少必要的内置变量
    ///
    /// # 内置变量
    /// 自动设置以下内置变量：
    /// - `this`: 当前 Lambda 函数引用
    /// - `self`: 方法调用的对象引用
    /// - 函数参数：按名称绑定到对应值
    /// - 捕获变量：闭包捕获的外部作用域变量
    pub fn new(
        argument: &OnionFastMap<Box<str>, OnionStaticObject>,
        capture: &OnionFastMap<Box<str>, OnionObject>,
        self_object: &OnionObject,
        this_lambda: &OnionStaticObject,
        instruction: Arc<VMInstructionPackage>,
        ip: isize,
    ) -> Result<Self, RuntimeError> {
        if !addr_eq(argument.pool().keys(), instruction.get_string_pool()) {
            panic!(
                "Argument pool does not match instruction string pool: {:?} != {:?}",
                argument.pool().keys(),
                instruction.get_string_pool()
            );
        }
        if !addr_eq(capture.pool().keys(), instruction.get_string_pool()) {
            panic!(
                "Capture pool does not match instruction string pool: {:?} != {:?}",
                capture.pool().keys(),
                instruction.get_string_pool()
            );
        }

        let mut new_context = Context::new();
        Context::push_frame(&mut new_context, Frame::new());

        let index_this = instruction.get_string_index("this").ok_or_else(|| {
            RuntimeError::InvalidOperation(
                "Missing required variable 'this' in string pool"
                    .to_string()
                    .into(),
            )
        })?;
        let index_self = instruction.get_string_index("self").ok_or_else(|| {
            RuntimeError::InvalidOperation(
                "Missing required variable 'self' in string pool"
                    .to_string()
                    .into(),
            )
        })?;
        /*
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
        */

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

        // new_context
        //     .let_variable(index_arguments, build_dict_from_hashmap(argument))
        //     .map_err(|e| {
        //         RuntimeError::InvalidOperation(
        //             format!("Failed to initialize 'arguments' variable: {}", e).into(),
        //         )
        //     })?;

        for (argument_index, value) in argument.pairs() {
            new_context
                .let_variable(*argument_index, value.clone())
                .map_err(|e| {
                    RuntimeError::InvalidOperation(
                        format!("Failed to initialize argument '{}': {}", argument_index, e).into(),
                    )
                })?;
        }

        // 设置捕获的变量
        for (capture_index, value) in capture.pairs() {
            new_context
                .let_variable(*capture_index, value.stabilize())
                .map_err(|e| {
                    RuntimeError::InvalidOperation(
                        format!(
                            "Failed to initialize captured variable '{}': {}",
                            capture_index, e
                        )
                        .into(),
                    )
                })?;
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
    /// 接收其他可执行对象的执行结果。
    ///
    /// 当前仅支持接收返回值结果，将其推入操作数栈中。
    /// 这主要用于处理子函数调用的返回值或异步操作的结果。
    ///
    /// # 参数
    /// - `step_result`: 要接收的执行步骤结果
    /// - `_gc`: 垃圾收集器引用（当前未使用）
    ///
    /// # 返回
    /// - `Ok(())`: 成功接收结果
    /// - `Err(RuntimeError)`: 接收失败或不支持的结果类型
    ///
    /// # 错误
    /// - `DetailedError`: 当接收非返回值类型的结果时
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
                "receive not implemented for cases except 'Return'"
                    .to_string()
                    .into(),
            ))
        }
    }

    /// 执行一个或多个指令步骤。
    ///
    /// 实现高性能的指令执行循环，支持批量执行以减少函数调用开销。
    /// 通过静态指令表实现 O(1) 指令分发，提供优异的执行性能。
    ///
    /// # 执行策略
    /// - 批量执行最多 1024 条指令以优化性能
    /// - 使用 unsafe 代码避免重复边界检查
    /// - 通过静态函数表实现快速指令分发
    /// - 保存错误时的指令指针用于调试
    ///
    /// # 参数
    /// - `gc`: 垃圾收集器的可变引用，用于内存管理
    ///
    /// # 返回
    /// - `StepResult::Continue`: 需要继续执行更多指令
    /// - `StepResult::Return(value)`: 函数执行完成，返回结果值
    /// - `StepResult::Error(error)`: 执行过程中发生错误
    /// - `StepResult::Call(runnable)`: 需要调用其他可执行对象
    ///
    /// # 安全性
    /// 使用 unsafe 代码进行性能优化，但通过边界检查确保内存安全。
    /// 指令指针越界会立即返回错误而不是导致未定义行为。
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
                    "Instruction pointer out of bounds".into(),
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

    /// 格式化当前执行上下文为可读字符串。
    ///
    /// 生成详细的调试信息，包括源码位置、当前指令和执行状态。
    /// 优先显示源码级别的调试信息，如果不可用则回退到字节码级别。
    ///
    /// # 输出格式
    /// 1. **源码位置**（如果可用）：
    ///    - 行号和列号
    ///    - 带有错误高亮的源码片段
    /// 2. **字节码信息**（回退选项）：
    ///    - 当前指令指针位置
    ///    - 反汇编的指令内容
    /// 3. **执行状态**：
    ///    - 调用栈信息
    ///    - 操作数栈状态
    ///    - 变量作用域内容
    ///
    /// # 返回
    /// 格式化的多行字符串，包含完整的调试上下文信息
    ///
    /// # 示例输出
    /// ```text
    /// -> at 15:8
    ///     15 | let x = 10 / 0;
    ///        |         ^^^^^^
    ///
    /// --- Lambda Execution State ---
    /// Frame 0:
    ///   Variables: x=10, y="hello"
    ///   Stack: [Integer(10), Integer(0)]
    /// ```
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
/// 解析变长指令格式，提取操作码和操作数，并格式化为易读的汇编代码形式。
/// 支持所有虚拟机指令类型，包括立即数、字符串引用、字节数组引用等。
///
/// # 指令格式
/// - 第一个 u32 word：元数据，包含操作码和操作数类型标志
/// - 后续 words：操作数的实际值，根据类型标志解释
///
/// # 参数
/// - `package`: 字节码指令包，包含指令序列和常量池
/// - `ip`: 要反汇编的指令指针位置
///
/// # 返回
/// 人类可读的指令字符串，包含指令名称和格式化的操作数
///
/// # 示例输出
/// ```text
/// LoadInt32 42
/// LoadString Str(5) -> "hello"
/// BinaryAdd
/// Jump 15
/// ```
///
/// # 错误处理
/// 如果指令指针越界，返回包含错误信息的字符串而不是 panic
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
        // 使用你的 'build_operand_argument' 来获取结构化的操作数类型
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

/// 源码位置信息。
///
/// 包含与特定指令指针位置对应的源码行列信息和代码片段，
/// 用于生成高质量的错误消息和调试输出。
///
/// # 字段
/// - `line`: 源码行号（从 1 开始）
/// - `column`: 源码列号（从 1 开始）
/// - `code_snippet`: 格式化的代码片段，包含行号和错误高亮
///
/// # 代码片段格式
/// ```text
///    15 | let x = 10 / 0;
///       |         ^^^^^^
/// ```
/// 其中 `^^^^^^` 标记了错误发生的具体位置。
#[derive(Debug)]
struct SourceLocation {
    /// 源码行号（从 1 开始计数）
    pub line: usize,
    /// 源码列号（从 1 开始计数）
    pub column: usize,
    /// 包含高亮的代码片段，格式化为多行字符串
    ///
    /// 示例格式：
    /// ```text
    ///   "  12 | let x = 10 / 0;\n"
    ///   "     |         ^^^^^^"
    /// ```
    pub code_snippet: String,
}

/// 根据指令指针获取对应的源码位置信息。
///
/// 通过调试信息将虚拟机指令指针映射回原始源码位置，
/// 生成包含行列号和高亮代码片段的详细位置信息。
///
/// # 参数
/// - `package`: 字节码指令包，包含调试信息和源码
/// - `ip`: 指令指针位置
///
/// # 返回
/// - `Some(SourceLocation)`: 成功映射到源码位置
/// - `None`: 无法映射（缺少源码或调试信息）
///
/// # 功能特性
/// - 支持 Unicode 字符的正确宽度计算
/// - 生成美观的错误高亮显示
/// - 处理多字节字符的对齐问题
/// - 提供上下文行信息
///
/// # 错误处理
/// 当源码或调试信息不可用时返回 None，调用方应提供回退方案。
fn get_source_location_for_ip(package: &VMInstructionPackage, ip: usize) -> Option<SourceLocation> {
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

    /// 打印关键数据结构的内存大小。
    ///
    /// 输出 `StepResult` 和 `RuntimeError` 等核心类型的字节大小，
    /// 用于性能分析和内存优化决策。
    ///
    /// # 注意
    /// 这些大小可能因编译器版本、目标平台和编译选项而有所不同。
    #[test]
    fn print_sizes() {
        println!("StepResult size: {}", std::mem::size_of::<StepResult>());
        println!("RuntimeError size: {}", std::mem::size_of::<RuntimeError>());
        println!("StepResult size: {}", std::mem::size_of::<StepResult>());
    }
}
