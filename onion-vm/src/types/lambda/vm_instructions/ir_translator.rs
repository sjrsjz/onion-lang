//! Onion 虚拟机 IR 到字节码翻译器。
//!
//! - `IRTranslator`：将中间表示（IR）翻译为 VMInstructionPackage 字节码包。
//! - 支持常量池去重、跳转重定向、调试信息收集等。
//! - 提供字符串、索引、字节常量池分配与唯一性保证。

use std::collections::HashMap;
use std::fmt::Display;
use std::result::Result;

use super::instruction_set::*;
use super::ir::DebugInfo;
use super::ir::IR;
use super::ir::IROperation;
use super::ir::IRPackage;
use super::opcode::*;

/// IR 翻译错误类型。
///
/// 用于描述 IR 到字节码翻译过程中遇到的错误。
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum IRTranslatorError {
    /// 遇到无法识别或非法的 IR 指令
    InvalidInstruction(IR),
}

impl Display for IRTranslatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRTranslatorError::InvalidInstruction(ir) => {
                write!(
                    f,
                    "Invalid IR instruction encountered during translation to bytecode: {:?}",
                    ir
                )
            }
        }
    }
}

/// Onion 虚拟机 IR 到字节码的翻译器。
///
/// 负责将 IRPackage 中的中间表示指令翻译为 VMInstructionPackage 字节码包，
/// 并自动管理常量池、跳转重定向、调试信息等。
#[derive(Debug)]
pub struct IRTranslator {
    /// IR 包（中间表示）
    ir_package: IRPackage,
    /// 函数签名到入口点的映射
    function_ips: HashMap<String, usize>,
    /// IR 指令到字节码偏移的映射
    ir_to_ip: Vec<usize>,
    /// 生成的字节码流
    code: Vec<u32>,
    /// 字符串常量池
    string_pool: Vec<String>,
    /// 字符串到索引的映射
    string_to_index: HashMap<String, usize>,
    /// 索引常量池
    index_pool: Vec<Vec<usize>>,
    /// 索引数组到索引的映射
    index_to_index: HashMap<Vec<usize>, usize>,
    /// 字节常量池
    bytes_pool: Vec<Vec<u8>>,
    /// 字节数组到索引的映射
    bytes_to_index: HashMap<Vec<u8>, usize>,
    /// 调试信息表
    debug_infos: HashMap<usize, DebugInfo>,
}

pub const PRE_ALLOCATED_VARIABLE_STRINGS: [&str; 2] = ["this", /*"arguments",*/ "self"];

impl IRTranslator {
    /// 构造新的 IRTranslator。
    ///
    /// # 参数
    /// * `ir_package` - 要翻译的 IR 包
    ///
    /// # 返回值
    /// 返回新构造的 IRTranslator
    pub fn new(ir_package: &IRPackage) -> Self {
        let mut translator = IRTranslator {
            ir_package: ir_package.clone(),
            function_ips: HashMap::default(),
            ir_to_ip: vec![],
            code: vec![],
            string_pool: vec![],
            string_to_index: HashMap::default(),
            index_pool: vec![],
            index_to_index: HashMap::default(),
            bytes_pool: vec![],
            bytes_to_index: HashMap::default(),
            debug_infos: HashMap::default(),
        };
        // 预分配常用变量名
        for &s in &PRE_ALLOCATED_VARIABLE_STRINGS {
            translator.alloc_string(s.to_string());
        }
        translator
    }

    /// 分配字符串常量，保证唯一性。
    ///
    /// # 参数
    /// * `value` - 要分配的字符串
    ///
    /// # 返回值
    /// 返回字符串在常量池中的索引
    pub fn alloc_string(&mut self, value: String) -> usize {
        if let Some(&existing_index) = self.string_to_index.get(&value) {
            return existing_index;
        }
        let index = self.string_pool.len();
        self.string_to_index.insert(value.clone(), index);
        self.string_pool.push(value);
        index
    }

    /// 分配索引常量，保证唯一性。
    ///
    /// # 参数
    /// * `value` - 要分配的索引数组
    ///
    /// # 返回值
    /// 返回索引数组在常量池中的索引
    pub fn alloc_index(&mut self, value: Vec<usize>) -> usize {
        if let Some(&existing_index) = self.index_to_index.get(&value) {
            return existing_index;
        }
        let index = self.index_pool.len();
        self.index_to_index.insert(value.clone(), index);
        self.index_pool.push(value);
        index
    }

    /// 分配字节常量，保证唯一性。
    ///
    /// # 参数
    /// * `value` - 要分配的字节数组
    ///
    /// # 返回值
    /// 返回字节数组在常量池中的索引
    pub fn alloc_bytes(&mut self, value: Vec<u8>) -> usize {
        if let Some(&existing_index) = self.bytes_to_index.get(&value) {
            return existing_index;
        }
        let index = self.bytes_pool.len();
        self.bytes_to_index.insert(value.clone(), index);
        self.bytes_pool.push(value);
        index
    }
}

impl IRTranslator {
    /// 将 IR 包翻译为字节码流。
    ///
    /// 处理所有 IR 指令，生成对应的字节码、常量池和调试信息。
    /// 支持跳转重定向、常量池分配、错误处理等。
    ///
    /// # 返回值
    /// * Ok(()) - 翻译成功
    /// * Err(IRTranslatorError) - 遇到非法 IR 指令
    pub fn translate(&mut self) -> Result<(), IRTranslatorError> {
        let mut redirect_table = Vec::<(
            usize, /*偏移计算位置*/
            usize, /*填充位置*/
            usize, /*跳转的ir*/
            bool,
        )>::new(); // bool 表示是 i64 填充
        let cloned = self.ir_package.instructions.clone();
        for idx in 0..cloned.len() {
            let (debug_info, ir) = cloned[idx].clone();
            self.ir_to_ip.push(self.code.len());
            self.debug_infos.insert(self.code.len(), debug_info);
            match ir {
                IR::LoadInt(value) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::LoadInt64 as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    self.code.push(Opcode32::lower32(value as u64));
                    self.code.push(Opcode32::upper32(value as u64));
                }
                IR::LoadFloat(value) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::LoadFloat64 as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64 | OperandFlag::ShiftType,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    self.code.push(Opcode32::f64lower32(value));
                    self.code.push(Opcode32::f64upper32(value));
                }
                IR::LoadNull => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::LoadNull as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::LoadUndefined => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::LoadUndefined as u8, 0, 0, 0)
                            .get_opcode(),
                    );
                }
                IR::LoadString(value) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::LoadString as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64 | OperandFlag::UseConstPool,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    let index = self.alloc_string(value);
                    self.code.push(Opcode32::lower32(index as u64));
                    self.code.push(Opcode32::upper32(index as u64));
                }
                IR::LoadBytes(value) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::LoadBytes as u8,
                            OperandFlag::Valid
                                | OperandFlag::ArgSize64
                                | OperandFlag::UseConstPool
                                | OperandFlag::ShiftType,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    let index = self.alloc_bytes(value);
                    self.code.push(Opcode32::lower32(index as u64));
                    self.code.push(Opcode32::upper32(index as u64));
                }
                IR::LoadBool(value) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::LoadBool as u8,
                            OperandFlag::Valid as u8,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    self.code.push(value as u32);
                }
                IR::LoadLambda(signature, capture_vars) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::LoadLambda as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64 | OperandFlag::UseConstPool,
                            OperandFlag::Valid | OperandFlag::ArgSize64,
                            0,
                        )
                        .get_opcode(),
                    );
                    let index = self.alloc_string(signature);
                    self.code.push(Opcode32::lower32(index as u64));
                    self.code.push(Opcode32::upper32(index as u64));
                    let index = capture_vars
                        .iter()
                        .map(|s| self.alloc_string(s.clone()))
                        .collect::<Vec<_>>();
                    let index = self.alloc_index(index);
                    self.code.push(Opcode32::lower32(index as u64));
                    self.code.push(Opcode32::upper32(index as u64));
                }
                IR::ForkInstruction => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::ForkInstruction as u8, 0, 0, 0)
                            .get_opcode(),
                    );
                }
                IR::BuildTuple(size) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::BuildTuple as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    self.code.push(Opcode32::lower32(size as u64));
                    self.code.push(Opcode32::upper32(size as u64));
                }
                IR::BuildKeyValue => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::BuildKeyValue as u8, 0, 0, 0)
                            .get_opcode(),
                    );
                }
                IR::BuildNamed => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::BuildNamed as u8, 0, 0, 0)
                            .get_opcode(),
                    );
                }
                IR::BuildRange => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::BuildRange as u8, 0, 0, 0)
                            .get_opcode(),
                    );
                }
                IR::BinaryOp(op) => {
                    let opcode = match op {
                        IROperation::Add => VMInstruction::BinaryAdd,
                        IROperation::Subtract => VMInstruction::BinarySub,
                        IROperation::Multiply => VMInstruction::BinaryMul,
                        IROperation::Divide => VMInstruction::BinaryDiv,
                        IROperation::Modulus => VMInstruction::BinaryMod,
                        IROperation::Power => VMInstruction::BinaryPow,
                        IROperation::And => VMInstruction::BinaryBitAnd,
                        IROperation::Or => VMInstruction::BinaryBitOr,
                        IROperation::Xor => VMInstruction::BinaryBitXor,
                        IROperation::ShiftLeft => VMInstruction::BinaryShl,
                        IROperation::ShiftRight => VMInstruction::BinaryShr,
                        IROperation::Equal => VMInstruction::BinaryEq,
                        IROperation::NotEqual => VMInstruction::BinaryNe,
                        IROperation::Greater => VMInstruction::BinaryGt,
                        IROperation::Less => VMInstruction::BinaryLt,
                        IROperation::GreaterEqual => VMInstruction::BinaryGe,
                        IROperation::LessEqual => VMInstruction::BinaryLe,
                        _ => {
                            return Err(IRTranslatorError::InvalidInstruction(IR::BinaryOp(
                                op.clone(),
                            )));
                        }
                    };
                    self.code
                        .push(Opcode32::build_opcode(opcode as u8, 0, 0, 0).get_opcode());
                }
                IR::UnaryOp(op) => {
                    let opcode = match op {
                        IROperation::Not => VMInstruction::UnaryBitNot,
                        IROperation::Abs => VMInstruction::UnaryAbs,
                        IROperation::Minus => VMInstruction::UnaryNeg,
                        _ => {
                            return Err(IRTranslatorError::InvalidInstruction(IR::UnaryOp(
                                op.clone(),
                            )));
                        }
                    };
                    self.code
                        .push(Opcode32::build_opcode(opcode as u8, 0, 0, 0).get_opcode());
                }
                IR::Let(name) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::StoreVar as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64 | OperandFlag::UseConstPool,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    let index = self.alloc_string(name);
                    self.code.push(Opcode32::lower32(index as u64));
                    self.code.push(Opcode32::upper32(index as u64));
                }
                IR::Get(name) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::LoadVar as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64 | OperandFlag::UseConstPool,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    let index = self.alloc_string(name);
                    self.code.push(Opcode32::lower32(index as u64));
                    self.code.push(Opcode32::upper32(index as u64));
                }
                IR::Set => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::SetValue as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::GetAttr => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::GetAttr as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::KeyOf => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::KeyOf as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::ValueOf => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::ValueOf as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::TypeOf => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::TypeOf as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Apply => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Apply as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Return => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Return as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::NewFrame => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::NewFrame as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::PopFrame => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::PopFrame as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Pop => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Pop as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::JumpOffset(offset) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::Jump as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    self.code.push(0u32);
                    self.code.push(0u32);
                    redirect_table.push((
                        self.code.len(),
                        self.code.len() - 2,
                        (idx as isize + offset + 1) as usize,
                        true,
                    ));
                }
                IR::JumpIfFalseOffset(offset) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::JumpIfFalse as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64,
                            0,
                            0,
                        )
                        .get_opcode(),
                    );
                    self.code.push(0u32);
                    self.code.push(0u32);
                    redirect_table.push((
                        self.code.len(),
                        self.code.len() - 2,
                        (idx as isize + offset + 1) as usize,
                        true,
                    ));
                }
                IR::ResetStack => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::ResetStack as u8, 0, 0, 0)
                            .get_opcode(),
                    );
                }
                IR::Mut => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Mut as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Const => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Const as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Launch => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Launch as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Spawn => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Spawn as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Assert => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Assert as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Import => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Import as u8, 0, 0, 0).get_opcode(),
                    );
                }
                // IR::Namespace(name) => {
                //     self.code.push(
                //         Opcode32::build_opcode(
                //             VMInstruction::Namespace as u8,
                //             OperandFlag::Valid | OperandFlag::ArgSize64,
                //             0,
                //             0,
                //         )
                //         .get_opcode(),
                //     );
                //     let index = self.alloc_string(name);
                //     self.code.push(Opcode32::lower32(index as u64));
                //     self.code.push(Opcode32::upper32(index as u64));
                // }
                IR::In => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::BinaryIn as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::IsSameObject => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::BinaryIs as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Raise => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::Raise as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::BuildLazySet => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::BuildSet as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::Swap(a, b) => {
                    self.code.push(
                        Opcode32::build_opcode(
                            VMInstruction::Swap as u8,
                            OperandFlag::Valid | OperandFlag::ArgSize64,
                            OperandFlag::Valid | OperandFlag::ArgSize64,
                            0,
                        )
                        .get_opcode(),
                    );
                    self.code.push(Opcode32::lower32(a as u64));
                    self.code.push(Opcode32::upper32(a as u64));
                    self.code.push(Opcode32::lower32(b as u64));
                    self.code.push(Opcode32::upper32(b as u64));
                }
                IR::LengthOf => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::LengthOf as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::MapTo => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::MapTo as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::MakeAsync => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::MakeAsync as u8, 0, 0, 0)
                            .get_opcode(),
                    );
                }
                IR::MakeSync => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::MakeSync as u8, 0, 0, 0).get_opcode(),
                    );
                }
                IR::MakeAtomic => {
                    self.code.push(
                        Opcode32::build_opcode(VMInstruction::MakeAtomic as u8, 0, 0, 0)
                            .get_opcode(),
                    );
                }
                _ => {
                    return Err(IRTranslatorError::InvalidInstruction(ir));
                }
            }
        }

        // 处理跳转指令
        // 偏移是相对于操作数而言的，而非相对于指令起始地址
        for (ip, write_ip, ir_ip, is_i64) in redirect_table {
            let computed_jump_offset =
                *self
                    .ir_to_ip
                    .get(ir_ip)
                    .ok_or(IRTranslatorError::InvalidInstruction(IR::JumpOffset(
                        ir_ip as isize,
                    )))? as isize
                    - ip as isize;
            if is_i64 {
                self.code[write_ip] = Opcode32::lower32(computed_jump_offset as u64);
                self.code[write_ip + 1] = Opcode32::upper32(computed_jump_offset as u64);
            } else {
                self.code[write_ip] = computed_jump_offset as u32;
            }
        }

        // 填充签名跳转表
        for (name, ip) in self.ir_package.function_ips.iter() {
            self.function_ips.insert(name.clone(), self.ir_to_ip[*ip]);
        }

        Ok(())
    }

    /// 获取翻译结果，生成最终的 VMInstructionPackage。
    pub fn get_result(&self) -> VMInstructionPackage {
        VMInstructionPackage::new(
            self.function_ips.clone(),
            self.code.clone(),
            self.string_pool.clone(),
            self.index_pool.clone(),
            self.bytes_pool.clone(),
            self.debug_infos.clone(),
            self.ir_package.source.clone(),
        )
    }
}
