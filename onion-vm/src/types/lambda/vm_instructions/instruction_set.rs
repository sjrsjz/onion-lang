use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

use crate::utils::fastmap::{OnionFastMap, OnionKeyPool};

use super::ir::DebugInfo;

/// 虚拟机指令集
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum VMInstruction {
    // ===== 栈操作 (0-9) =====
    LoadNull = 0,
    LoadInt32 = 1,
    LoadInt64 = 2,
    LoadFloat32 = 3,
    LoadFloat64 = 4,
    LoadString = 5,
    LoadBytes = 6,
    LoadBool = 7,
    LoadLambda = 8,
    LoadUndefined = 9, // 未定义值

    // ===== 数据结构构建 (10-19) =====
    BuildTuple = 10,
    BuildKeyValue = 11,
    BuildNamed = 12,
    BuildRange = 13,
    BuildSet = 14,

    // ===== 二元操作符 (20-39) =====
    BinaryAdd = 20,    // +
    BinarySub = 21,    // -
    BinaryMul = 22,    // *
    BinaryDiv = 23,    // /
    BinaryMod = 24,    // %
    BinaryPow = 25,    // **
    BinaryBitAnd = 26, // &
    BinaryBitOr = 27,  // |
    BinaryBitXor = 28, // ^
    BinaryShl = 29,    // <<
    BinaryShr = 30,    // >>
    BinaryEq = 31,     // ==
    BinaryNe = 32,     // !=
    BinaryGt = 33,     // >
    BinaryLt = 34,     // <
    BinaryGe = 35,     // >=
    BinaryLe = 36,     // <=
    BinaryIn = 37,     // in
    BinaryIs = 38,     // is
    MapTo = 39,        // map to

    // ===== 一元操作 (40-49) =====
    UnaryBitNot = 40, // ~
    UnaryAbs = 41,    // abs
    UnaryNeg = 42,    // -

    // ===== 变量与引用 (50-69) =====
    StoreVar = 50,   // 存储变量
    LoadVar = 51,    // 加载变量
    SetValue = 52,   // 设置值
    GetAttr = 53,    // 获取属性
    KeyOf = 55,      // 获取键
    ValueOf = 56,    // 获取值
    TypeOf = 57,     // 获取类型
    Mut = 58,        // 创建引用
    Const = 59,      // 解引用
    Swap = 60,       // 交换栈两个值
    LengthOf = 61,   // 获取对象长度
    Launch = 62,     // 启动线程
    Spawn = 63,      // 启动任务
    MakeAsync = 64,  // 异步池化调度
    MakeSync = 65,   // 同步池化调度
    MakeAtomic = 66, // 将Lambda转换为标准调度

    // ===== 控制流 (70-79) =====
    Apply = 70,       // 函数应用/索引应用
    Return = 71,      // 返回
    Jump = 72,        // 跳转
    JumpIfFalse = 73, // 条件跳转
    Raise = 74,       // 抛出自定义值

    // ===== 帧操作 (80-89) =====
    NewFrame = 80,   // 新建帧
    PopFrame = 81,   // 弹出帧
    ResetStack = 82, // 重置栈
    Pop = 83,        // 弹出栈顶

    // ===== 模块操作 (90-99) =====
    Import = 90, // 导入模块

    // ===== 特殊操作 (100-109) =====
    ForkInstruction = 100, // 复制当前函数指令集
    Assert = 101,          // 断言

    // ===== 其他 =====
    Nop = 255, // 空操作
}

impl VMInstruction {
    /// 根据操作码获取指令
    pub fn from_opcode(opcode: u8) -> Option<Self> {
        match opcode {
            // ===== 栈操作 (0-9) =====
            0 => Some(Self::LoadNull),
            1 => Some(Self::LoadInt32),
            2 => Some(Self::LoadInt64),
            3 => Some(Self::LoadFloat32),
            4 => Some(Self::LoadFloat64),
            5 => Some(Self::LoadString),
            6 => Some(Self::LoadBytes),
            7 => Some(Self::LoadBool),
            8 => Some(Self::LoadLambda),
            9 => Some(Self::LoadUndefined),

            // ===== 数据结构构建 (10-19) =====
            10 => Some(Self::BuildTuple),
            11 => Some(Self::BuildKeyValue),
            12 => Some(Self::BuildNamed),
            13 => Some(Self::BuildRange),
            14 => Some(Self::BuildSet),

            // ===== 二元操作符 (20-39) =====
            20 => Some(Self::BinaryAdd),
            21 => Some(Self::BinarySub),
            22 => Some(Self::BinaryMul),
            23 => Some(Self::BinaryDiv),
            24 => Some(Self::BinaryMod),
            25 => Some(Self::BinaryPow),
            26 => Some(Self::BinaryBitAnd),
            27 => Some(Self::BinaryBitOr),
            28 => Some(Self::BinaryBitXor),
            29 => Some(Self::BinaryShl),
            30 => Some(Self::BinaryShr),
            31 => Some(Self::BinaryEq),
            32 => Some(Self::BinaryNe),
            33 => Some(Self::BinaryGt),
            34 => Some(Self::BinaryLt),
            35 => Some(Self::BinaryGe),
            36 => Some(Self::BinaryLe),
            37 => Some(Self::BinaryIn),
            38 => Some(Self::BinaryIs),
            39 => Some(Self::MapTo),

            // ===== 一元操作 (40-49) =====
            40 => Some(Self::UnaryBitNot),
            41 => Some(Self::UnaryAbs),
            42 => Some(Self::UnaryNeg),

            // ===== 变量与引用 (50-69) =====
            50 => Some(Self::StoreVar),
            51 => Some(Self::LoadVar),
            52 => Some(Self::SetValue),
            53 => Some(Self::GetAttr),
            55 => Some(Self::KeyOf),
            56 => Some(Self::ValueOf),
            57 => Some(Self::TypeOf),
            58 => Some(Self::Mut),
            59 => Some(Self::Const),
            60 => Some(Self::Swap),
            61 => Some(Self::LengthOf),
            62 => Some(Self::Launch),
            63 => Some(Self::Spawn),
            64 => Some(Self::MakeAsync),
            65 => Some(Self::MakeSync),
            66 => Some(Self::MakeAtomic),

            // ===== 控制流 (70-79) =====
            70 => Some(Self::Apply),
            71 => Some(Self::Return),
            72 => Some(Self::Jump),
            73 => Some(Self::JumpIfFalse),
            74 => Some(Self::Raise),

            // ===== 帧操作 (80-89) =====
            80 => Some(Self::NewFrame),
            81 => Some(Self::PopFrame),
            82 => Some(Self::ResetStack),
            83 => Some(Self::Pop),

            // ===== 模块操作 (90-99) =====
            90 => Some(Self::Import),

            // ===== 特殊操作 (100-109) =====
            100 => Some(Self::ForkInstruction),
            101 => Some(Self::Assert),

            // ===== 其他 =====
            255 => Some(Self::Nop),

            _ => None,
        }
    }
}

use std::{collections::HashMap, fs, sync::Arc};
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMInstructionPackage {
    function_ips: FxHashMap<String, usize>, // 签名定位表
    code: Vec<u32>,
    string_pool: Arc<[Box<str>]>,
    string_to_index: Arc<FxHashMap<Box<str>, usize>>,
    index_pool: Vec<Vec<usize>>,
    bytes_pool: Vec<Vec<u8>>,
    debug_infos: HashMap<usize, DebugInfo>,
    source: Option<String>,
}

impl VMInstructionPackage {
    pub fn new(
        function_ips: HashMap<String, usize>,
        code: Vec<u32>,
        string_pool: Vec<String>,
        index_pool: Vec<Vec<usize>>,
        bytes_pool: Vec<Vec<u8>>,
        debug_infos: HashMap<usize, DebugInfo>,
        source: Option<String>,
    ) -> Self {
        let string_to_index = string_pool
            .iter()
            .enumerate()
            .map(|(index, string)| (string.clone().into_boxed_str(), index))
            .collect();
        VMInstructionPackage {
            function_ips: function_ips.into_iter().collect(),
            code,
            string_pool: Arc::from(
                string_pool
                    .into_iter()
                    .map(|s| s.into_boxed_str())
                    .collect::<Vec<_>>(),
            ),
            string_to_index: Arc::new(string_to_index),
            index_pool,
            bytes_pool,
            debug_infos,
            source,
        }
    }
    #[inline(always)]
    pub fn get_table(&self) -> &FxHashMap<String, usize> {
        &self.function_ips
    }
    #[inline(always)]
    pub fn get_code(&self) -> &Vec<u32> {
        &self.code
    }
    #[inline(always)]
    pub fn get_string_pool(&self) -> &[Box<str>] {
        &self.string_pool
    }
    #[inline(always)]
    pub fn get_string_index(&self, string: &str) -> Option<usize> {
        self.string_to_index.get(string).copied()
    }
    #[inline(always)]
    pub fn get_index_pool(&self) -> &Vec<Vec<usize>> {
        &self.index_pool
    }
    #[inline(always)]
    pub fn get_bytes_pool(&self) -> &Vec<Vec<u8>> {
        &self.bytes_pool
    }
    #[inline(always)]
    pub fn get_source(&self) -> &Option<String> {
        &self.source
    }
    #[inline(always)]
    pub fn get_debug_info(&self) -> &HashMap<usize, DebugInfo> {
        &self.debug_infos
    }
    #[inline(always)]
    pub fn create_fast_map<V>(&self) -> OnionFastMap<Box<str>, V> {
        OnionFastMap::new(OnionKeyPool::new(
            self.string_pool.clone(),
            self.string_to_index.clone(),
        ))
    }

    #[inline(always)]
    pub fn create_key_pool(&self) -> OnionKeyPool<Box<str>> {
        OnionKeyPool::new(self.string_pool.clone(), self.string_to_index.clone())
    }

    pub fn write_to_file(&self, path: &str) -> Result<(), std::io::Error> {
        let serialized = bincode::serialize(self)
            .map_err(|e| std::io::Error::other(format!("Serialization error: {}", e)))?;

        fs::write(path, serialized)
    }

    pub fn read_from_file(path: &str) -> Result<Self, std::io::Error> {
        let bytes = fs::read(path)?;
        bincode::deserialize(&bytes)
            .map_err(|e| std::io::Error::other(format!("Deserialization error: {}", e)))
    }

    pub fn validate(&self) -> Result<(), String> {
        // 验证字符串池是否不重复
        let mut seen_strings = std::collections::HashSet::new();
        for (index, string) in self.string_pool.iter().enumerate() {
            if !seen_strings.insert(string) {
                return Err(format!(
                    "Duplicate string found at index {}: {}",
                    index, string
                ));
            }
        }
        // 验证字节池是否不重复
        let mut seen_bytes = std::collections::HashSet::new();
        for (index, bytes) in self.bytes_pool.iter().enumerate() {
            if !seen_bytes.insert(bytes.clone()) {
                return Err(format!(
                    "Duplicate bytes found at index {}: {:?}",
                    index, bytes
                ));
            }
        }
        Ok(())
    }
}
