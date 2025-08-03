use std::collections::HashMap;
use std::fmt::Display;
use std::result::Result;

use super::instruction_set::*;
use super::ir::DebugInfo;
use super::ir::IR;
use super::ir::IROperation;
use super::ir::IRPackage;
use super::opcode::*;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum IRTranslatorError {
    InvalidInstruction(IR),
}

impl Display for IRTranslatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRTranslatorError::InvalidInstruction(ir) => {
                write!(f, "Invalid IR instruction encountered during translation to bytecode: {:?}", ir)
            }
        }
    }
}

#[derive(Debug)]
pub struct IRTranslator {
    ir_package: IRPackage,
    function_ips: HashMap<String, usize>,
    ir_to_ip: Vec<usize>,
    code: Vec<u32>,
    string_pool: Vec<String>,
    string_to_index: HashMap<String, usize>, // 新增：字符串到索引的映射
    index_pool: Vec<Vec<usize>>,             // 新增：索引池
    index_to_index: HashMap<Vec<usize>, usize>, // 新增：索引数组到索引的映射
    bytes_pool: Vec<Vec<u8>>,
    bytes_to_index: HashMap<Vec<u8>, usize>, // 新增：字节数组去重
    debug_infos: HashMap<usize, DebugInfo>,
}

pub const PRE_ALLOCATED_VARIABLE_STRINGS: [&str; 2] = ["this", /*"arguments",*/ "self"];

impl IRTranslator {
    pub fn new(ir_package: &IRPackage) -> Self {
        let mut translator = IRTranslator {
            ir_package: ir_package.clone(),
            function_ips: HashMap::default(),
            ir_to_ip: vec![],
            code: vec![],
            string_pool: vec![],
            string_to_index: HashMap::default(), // 初始化映射表
            index_pool: vec![],                  // 初始化索引池
            index_to_index: HashMap::default(),  // 初始化索引数组映射表
            bytes_pool: vec![],
            bytes_to_index: HashMap::default(), // 初始化字节数组映射表
            debug_infos: HashMap::default(),
        };
        for &s in &PRE_ALLOCATED_VARIABLE_STRINGS {
            translator.alloc_string(s.to_string()); // 预分配
        }
        return translator;
    }

    // 优化后的字符串分配方法 - 确保相同字符串索引唯一
    pub fn alloc_string(&mut self, value: String) -> usize {
        // 先检查是否已经存在
        if let Some(&existing_index) = self.string_to_index.get(&value) {
            return existing_index;
        }

        // 不存在则创建新的
        let index = self.string_pool.len();
        self.string_to_index.insert(value.clone(), index);
        self.string_pool.push(value);
        index
    }

    pub fn alloc_index(&mut self, value: Vec<usize>) -> usize {
        // 先检查是否已经存在
        if let Some(&existing_index) = self.index_to_index.get(&value) {
            return existing_index;
        }

        // 不存在则创建新的
        let index = self.index_pool.len();
        self.index_to_index.insert(value.clone(), index);
        self.index_pool.push(value);
        index
    }

    // 同样优化字节数组分配
    pub fn alloc_bytes(&mut self, value: Vec<u8>) -> usize {
        // 先检查是否已经存在
        if let Some(&existing_index) = self.bytes_to_index.get(&value) {
            return existing_index;
        }

        // 不存在则创建新的
        let index = self.bytes_pool.len();
        self.bytes_to_index.insert(value.clone(), index);
        self.bytes_pool.push(value);
        index
    }
}

impl IRTranslator {
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
                        IROperation::Add => VMInstruction::UnaryAbs,
                        IROperation::Subtract => VMInstruction::UnaryNeg,
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
                IR::BuildSet => {
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
            let calced_offset =
                *self
                    .ir_to_ip
                    .get(ir_ip)
                    .ok_or(IRTranslatorError::InvalidInstruction(IR::JumpOffset(
                        ir_ip as isize,
                    )))? as isize
                    - ip as isize;
            if is_i64 {
                self.code[write_ip] = Opcode32::lower32(calced_offset as u64);
                self.code[write_ip + 1] = Opcode32::upper32(calced_offset as u64);
            } else {
                self.code[write_ip] = calced_offset as u32;
            }
        }

        // 填充签名跳转表
        for (name, ip) in self.ir_package.function_ips.iter() {
            self.function_ips.insert(name.clone(), self.ir_to_ip[*ip]);
        }

        Ok(())
    }

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
