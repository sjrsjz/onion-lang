use std::ops::BitOr;

use super::instruction_set::VMInstruction;

#[derive(Debug)]
pub struct Opcode32 {
    opcode: u32,
}
#[allow(dead_code)]
impl Opcode32 {
    pub fn build_opcode(
        instruction: u8,
        operand1: u8,
        operand2: u8,
        operand3: u8,
    ) -> Opcode32 {
        let opcode = ((instruction as u32) << 24)
            | ((operand1 as u32) << 16)
            | ((operand2 as u32) << 8)
            | (operand3 as u32);
        Opcode32 { opcode }
    }
    pub fn get_opcode(&self) -> u32 {
        self.opcode
    }
    pub fn lower32(uint: u64) -> u32 {
        (uint & 0xFFFFFFFF) as u32
    }
    pub fn upper32(uint: u64) -> u32 {
        ((uint >> 32) & 0xFFFFFFFF) as u32
    }
    pub fn f64lower32(uint: f64) -> u32 {
        let bits = uint.to_bits();
        (bits & 0xFFFFFFFF) as u32
    }
    pub fn f64upper32(uint: f64) -> u32 {
        let bits = uint.to_bits();
        ((bits >> 32) & 0xFFFFFFFF) as u32
    }
    pub fn f32lower32(uint: f32) -> u32 {
        
        uint.to_bits()
    }
}

pub enum OperandFlag {
    Valid = 0b00000001,
    UseConstPool = 0b00000010,
    ArgSize64 = 0b000000100,
    ShiftType = 0b00001000, // string/byte array or int/float, determine by UseConstPool
}

impl BitOr for OperandFlag {
    type Output = u8;

    fn bitor(self, rhs: Self) -> Self::Output {
        (self as u8) | (rhs as u8)
    }   
}

impl BitOr<u8> for OperandFlag {
    type Output = u8;

    fn bitor(self, rhs: u8) -> Self::Output {
        (self as u8) | rhs
    }
}

impl BitOr<OperandFlag> for u8 {
    type Output = u8;

    fn bitor(self, rhs: OperandFlag) -> Self::Output {
        self | (rhs as u8)
    }
}

pub struct DecodedOpcode {
    instruction: u8,
    operand1: u8, // const pool idx/constant (1bit)
    // argsize (1bit, 32/64)
    // valid (1bit)
    operand2: u8,
    operand3: u8,
}

impl DecodedOpcode{
    pub fn to_string(&self) -> String {
        format!(
            "Instruction: {:?}, Operand1: {}, Operand2: {}, Operand3: {}",
            VMInstruction::from_opcode(self.instruction), self.operand1, self.operand2, self.operand3
        )
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum OpcodeArgument {
    None,
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
    String(u64),
    ByteArray(u64),
}
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ProcessedOpcode {
    pub instruction: u8,
    pub operand1: OpcodeArgument,
    pub operand2: OpcodeArgument,
    pub operand3: OpcodeArgument,
}

impl ProcessedOpcode {
    pub fn _to_string(&self) -> String {
        let mut result = format!("Instruction: {:?}, ", VMInstruction::from_opcode(self.instruction));
        result += &format!("Operand1: {:?}, ", self.operand1);
        result += &format!("Operand2: {:?}, ", self.operand2);
        result += &format!("Operand3: {:?}", self.operand3);
        result
    }    
}

#[derive(Debug)]
pub struct Instruction32<'t> {
    bytes: &'t Vec<u32>, // 4 bytes
    pointer: &'t mut usize,
}

impl<'t> Instruction32<'t> {
    pub fn new(bytes: &'t Vec<u32>, pointer: &'t mut usize) -> Self {
        Instruction32 { bytes, pointer }
    }

    #[inline]
    pub fn take_u32(&mut self) -> Option<u32> {
        if *self.pointer < self.bytes.len() {
            let byte = self.bytes[*self.pointer];
            *self.pointer += 1;
            Some(byte)
        } else {
            None
        }
    }

    #[inline]
    pub fn take_u64(&mut self) -> Option<u64> {
        if *self.pointer + 1 < self.bytes.len() {
            let byte = ((self.bytes[*self.pointer + 1] as u64) << 32)
                | (self.bytes[*self.pointer] as u64);
            *self.pointer += 2;
            Some(byte)
        } else {
            None
        }
    }

    #[inline]
    pub fn get_processed_opcode(&mut self) -> Option<ProcessedOpcode> {
        let opcode = self.take_u32()?;
        let decoded_opcode = Self::decode_opcode(opcode);
        let operand1_arg = self.get_operand_arg(decoded_opcode.operand1)?;
        let operand2_arg = self.get_operand_arg(decoded_opcode.operand2)?;
        let operand3_arg = self.get_operand_arg(decoded_opcode.operand3)?;

        // 构建操作数
        let operand1 = self.build_operand_argument(decoded_opcode.operand1, operand1_arg);
        let operand2 = self.build_operand_argument(decoded_opcode.operand2, operand2_arg);
        let operand3 = self.build_operand_argument(decoded_opcode.operand3, operand3_arg);

        Some(ProcessedOpcode {
            instruction: decoded_opcode.instruction,
            operand1,
            operand2,
            operand3,
        })
    }

    // 根据操作数标志和原始值构建具体的操作数类型
    #[inline]
    fn build_operand_argument(&self, operand_flags: u8, arg_value: u64) -> OpcodeArgument {
        // 如果操作数无效，返回None
        if (operand_flags & OperandFlag::Valid as u8) == 0 {
            return OpcodeArgument::None;
        }

        // 处理常量池引用
        if (operand_flags & OperandFlag::UseConstPool as u8) != 0 {
            // 根据常量类型决定返回字符串引用或字节数组引用
            return if (operand_flags & OperandFlag::ShiftType as u8) == 0 {
                OpcodeArgument::String(arg_value)
            } else {
                OpcodeArgument::ByteArray(arg_value)
            };
        }

        // 处理直接值
        if (operand_flags & OperandFlag::ShiftType as u8) != 0 {
            // 浮点数值
            return if (operand_flags & OperandFlag::ArgSize64 as u8) != 0 {
                // 64位浮点数
                OpcodeArgument::Float64(f64::from_bits(arg_value))
            } else {
                // 32位浮点数
                OpcodeArgument::Float32(f32::from_bits(arg_value as u32))
            };
        }

        // 整数值
        if (operand_flags & OperandFlag::ArgSize64 as u8) != 0 {
            // 64位整数
            OpcodeArgument::Int64(arg_value as i64)
        } else {
            // 32位整数
            OpcodeArgument::Int32(arg_value as i32)
        }
    }

    #[inline]
    pub fn decode_opcode(opcode: u32) -> DecodedOpcode {
        let instruction = (opcode >> 24) as u8;
        let operand1 = ((opcode >> 16) & 0xFF) as u8;
        let operand2 = ((opcode >> 8) & 0xFF) as u8;
        let operand3 = (opcode & 0xFF) as u8;

        DecodedOpcode {
            instruction,
            operand1,
            operand2,
            operand3,
        }
    }

    #[inline]
    fn get_operand_arg(&mut self, operand: u8) -> Option<u64> {
        if (operand & OperandFlag::Valid as u8) == 0 {
            return Some(0);
        }
        if (operand & OperandFlag::ArgSize64 as u8) != 0 {
            // 64 bit
            let arg = self.take_u64()?;
            Some(arg)
        } else {
            // 32 bit
            let arg = self.take_u32()?;
            Some(arg as u64)
        }
    }
}
