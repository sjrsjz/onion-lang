//! Onion VM 对象模型核心实现。
//!
//! 本模块定义了 Onion 语言运行时的完整对象系统，包括对象枚举、对象单元格、
//! 静态对象封装、扩展trait、以及丰富的类型操作与转换接口。
//!
//! # 核心设计原则
//!
//! ## 不可变性原则
//! Onion VM 采用严格的不可变对象模型：
//! - 所有基础类型都是不可变的
//! - "可变"操作实际上是创建新对象并更新引用
//! - `Mut` 类型只是指向 `OnionObjectCell` 的弱引用容器
//!
//! ## 垃圾回收集成
//! - 所有对象都支持 GC 跟踪与升级
//! - 使用 `arc_gc` 提供的智能指针系统
//! - 支持弱引用避免循环引用
//!
//! ## 类型安全与扩展性
//! - 通过 `OnionObjectExt` trait 支持自定义类型
//! - 统一的错误处理与类型转换接口
//! - 丰富的运算符重载支持
//!
//! # 主要组件
//!
//! ## `OnionObject`
//! 核心对象枚举，包含所有内置类型：
//! - 基础类型：Integer, Float, String, Boolean 等
//! - 容器类型：Tuple, Pair, LazySet
//! - 函数类型：Lambda
//! - 扩展类型：Custom
//! - 可变引用：Mut
//!
//! ## `OnionObjectCell`
//! 线程安全的对象容器，提供：
//! - 读写锁保护的对象访问
//! - 幂等性检查与验证
//! - GC 跟踪支持
//!
//! ## `OnionStaticObject`
//! 稳定化的对象封装，用于：
//! - 跨函数调用的对象传递
//! - 常量与字面量的表示
//! - 序列化与反序列化
//!
//! # 典型用法
//! ```ignore
//! // 创建基础对象
//! let obj = OnionObject::Integer(42);
//! let static_obj = obj.stabilize();
//!
//! // 类型转换
//! let as_float = obj.to_float()?;
//! let as_string = obj.to_string(&vec![])?;
//!
//! // 运算操作
//! let sum = obj1.binary_add(&obj2)?;
//!
//! // 属性访问
//! obj.with_attribute(&key, |value| { ... })?;
//! ```

use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
    ptr::addr_eq,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use crate::{
    lambda::runnable::RuntimeError,
    types::lambda::{
        native::{
            native_bool_converter, native_bytes_converter, native_elements_method,
            native_float_converter, native_int_converter, native_length_method,
            native_string_converter, wrap_native_function,
        },
        parameter::LambdaParameter,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};
use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};
use base64::{Engine as _, engine::general_purpose};

use super::{
    lambda::{
        definition::OnionLambdaDefinition, vm_instructions::instruction_set::VMInstructionPackage,
    },
    lazy_set::OnionLazySet,
    pair::OnionPair,
    tuple::OnionTuple,
};

/// Onion 对象单元格。
///
/// 线程安全的对象容器，使用读写锁保护内部对象，确保并发访问的安全性。
/// 是 GC 系统中对象存储的基本单位。
///
/// # 设计原则
/// - 严格的幂等性检查：Cell 中不应包含 `Mut` 对象
/// - 线程安全的读写操作
/// - GC 跟踪与弱引用支持
///
/// # 关键方法
/// - `with_data()`: 安全地访问内部对象
/// - `with_data_mut()`: 安全地可变访问内部对象
/// - `with_attribute()`: 属性访问代理
pub struct OnionObjectCell(pub RwLock<OnionObject>);

impl OnionObjectCell {
    /// 安全地访问内部对象数据。
    ///
    /// 获取对象的只读访问权限，并执行提供的闭包。
    /// 包含严格的幂等性检查，确保 Cell 中不包含 `Mut` 对象。
    ///
    /// # 参数
    /// - `f`: 处理对象的闭包
    ///
    /// # 返回
    /// 闭包的执行结果
    ///
    /// # 错误
    /// - `BorrowError`: 无法获取读锁
    /// - Panic: 如果 Cell 中包含 `Mut` 对象（表示 VM 逻辑错误）
    #[inline(always)]
    // 严格遵循幂等律的情况下，Cell里不可能出现Mut对象。
    // 如果出现了Mut对象，说明VM对象分配或GC逻辑有bug
    pub fn with_data<T, F>(&self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&OnionObject) -> Result<T, RuntimeError>,
    {
        match self.0.read() {
            Ok(guard) => match &*guard {
                OnionObject::Mut(_) => {
                    panic!(
                        "CRITICAL: OnionObjectCell contains Mut object. This indicates a bug in VM object allocation or GC logic. Check mutablize() and object creation paths."
                    )
                }
                obj => f(obj),
            },
            Err(_) => Err(RuntimeError::BorrowError(
                "Failed to borrow OnionObjectCell at 'with_data'"
                    .to_string()
                    .into(),
            )),
        }
    }

    /// 安全地可变访问内部对象数据。
    ///
    /// 获取对象的可写访问权限，并执行提供的闭包。
    /// 同样包含严格的幂等性检查。
    ///
    /// # 参数
    /// - `f`: 处理对象的可变闭包
    ///
    /// # 返回
    /// 闭包的执行结果
    ///
    /// # 错误
    /// - `BorrowError`: 无法获取写锁
    /// - Panic: 如果 Cell 中包含 `Mut` 对象
    #[inline(always)]
    // 严格遵循幂等律的情况下，Cell里不可能出现Mut对象。
    // 如果出现了Mut对象，说明VM对象分配或GC逻辑有bug
    pub fn with_data_mut<T, F>(&self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&mut OnionObject) -> Result<T, RuntimeError>,
    {
        match self.0.write() {
            Ok(mut guard) => match &mut *guard {
                OnionObject::Mut(_) => {
                    panic!(
                        "CRITICAL: OnionObjectCell contains Mut object. This indicates a bug in VM object allocation or GC logic. Check mutablize() and object creation paths."
                    )
                }
                obj => f(obj),
            },
            Err(_) => Err(RuntimeError::BorrowError(
                "Failed to borrow OnionObjectCell at 'with_data_mut'"
                    .to_string()
                    .into(),
            )),
        }
    }

    /// 属性访问代理。
    ///
    /// 通过内部对象的 `with_attribute` 方法访问属性。
    ///
    /// # 参数
    /// - `key`: 属性键对象
    /// - `f`: 处理属性值的闭包
    #[inline(always)]
    pub fn with_attribute<T, F>(&self, key: &OnionObject, f: &F) -> Result<T, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<T, RuntimeError>,
    {
        self.0
            .read()
            .map_err(|_| {
                RuntimeError::BorrowError(
                    "Failed to borrow OnionObjectCell at 'with_attribute'"
                        .to_string()
                        .into(),
                )
            })?
            .with_attribute(key, f)
    }

    /// 升级对象的弱引用为强引用。
    ///
    /// 用于 GC 跟踪，防止对象被提前回收。
    #[inline(always)]
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        match self.0.read() {
            Ok(obj) => obj.upgrade(collected),
            Err(_) => {
                // 如果无法借用，可能是因为对象已经被回收或正在被其他线程使用
                // 这里选择忽略
            }
        }
    }

    /// 稳定化对象为静态对象。
    ///
    /// 将 Cell 中的对象转换为 `OnionStaticObject`，
    /// 用于跨函数调用和长期存储。
    #[inline(always)]
    pub fn stabilize(self) -> OnionStaticObject {
        OnionStaticObject::new(self.try_borrow().unwrap().clone())
    }

    #[inline(always)]
    pub fn equals(&self, other: &Self) -> Result<bool, RuntimeError> {
        self.with_data(|obj| other.with_data(|other_obj| obj.equals(other_obj)))
    }

    #[inline(always)]
    pub fn repr(&self, ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        self.with_data(|obj| obj.repr(ptrs))
    }

    #[inline(always)]
    pub fn try_borrow(&self) -> Result<RwLockReadGuard<OnionObject>, RuntimeError> {
        self.0.read().map_err(|_| {
            RuntimeError::BorrowError(
                "Failed to borrow OnionObjectCell at 'try_borrow'"
                    .to_string()
                    .into(),
            )
        })
    }
    #[inline(always)]
    pub fn try_borrow_mut(&self) -> Result<RwLockWriteGuard<OnionObject>, RuntimeError> {
        self.0.write().map_err(|_| {
            RuntimeError::BorrowError(
                "Failed to borrow OnionObjectCell at 'try_borrow_mut'"
                    .to_string()
                    .into(),
            )
        })
    }
}

impl std::ops::Deref for OnionObjectCell {
    type Target = RwLock<OnionObject>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for OnionObjectCell {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<RwLock<OnionObject>> for OnionObjectCell {
    fn from(cell: RwLock<OnionObject>) -> Self {
        OnionObjectCell(cell)
    }
}

impl From<OnionObject> for OnionObjectCell {
    fn from(obj: OnionObject) -> Self {
        OnionObjectCell(RwLock::new(obj))
    }
}

impl GCTraceable<OnionObjectCell> for OnionObjectCell {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        if let Ok(obj) = self.0.read() {
            obj.collect(queue);
        }
    }
}

impl Debug for OnionObjectCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0.read())
    }
}

impl Display for OnionObjectCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0.read())
    }
}

#[derive(Clone)]
/// Onion VM 核心对象枚举。
///
/// 定义了 Onion 语言运行时的所有对象类型。遵循严格的不可变性原则：
/// 所有类型都是不可变的，"可变"操作通过创建新对象并更新引用实现。
///
/// # 设计原则
/// - **不可变性**: 对象一旦创建就不能修改
/// - **引用透明性**: 相同输入总是产生相同输出
/// - **GC 友好**: 所有类型都支持垃圾回收跟踪
/// - **类型安全**: 严格的类型检查与转换
///
/// # 类型分类
///
/// ## 基础不可变类型
/// - `Integer(i64)`: 64位有符号整数
/// - `Float(f64)`: 64位浮点数
/// - `String(Arc<str>)`: 不可变字符串
/// - `Bytes(Arc<[u8]>)`: 不可变字节数组
/// - `Boolean(bool)`: 布尔值
/// - `Range(i64, i64)`: 整数范围
/// - `Null`: 空值
/// - `Undefined(Option<Arc<str>>)`: 未定义值（可选错误信息）
///
/// ## 容器类型
/// - `Tuple(Arc<OnionTuple>)`: 有序对象集合
/// - `Pair(Arc<OnionPair>)`: 键值对
/// - `LazySet(Arc<OnionLazySet>)`: 惰性集合
///
/// ## 函数与扩展类型
/// - `Lambda((Arc<OnionLambdaDefinition>, Arc<OnionObject>))`: Lambda 函数
/// - `Custom(Arc<dyn OnionObjectExt>)`: 自定义扩展类型
/// - `InstructionPackage(Arc<VMInstructionPackage>)`: 字节码指令包
///
/// ## 可变引用类型
/// - `Mut(GCArcWeak<OnionObjectCell>)`: 可变对象的弱引用容器
///
/// # 重要说明
/// `Mut` 类型不应直接使用，应通过 `mutablize()` 方法创建。
/// `Mut` 只是一个指向 `OnionObjectCell` 的弱引用容器，不违反不可变性原则。
pub enum OnionObject {
    // OnionObject is the main type for all objects in the Onion VM.
    // The VM's types are all immutable(Mut is a immutable pointer to a VM object)
    // If we need to 'mutate' an object, it is IMPOSSIBLE to mutate the object itself, we can only let the Mut object point to a new object.
    // So all types defined in OnionObject do not implement 'Clone', because deep cloning an object is volition of the immutability principle.

    // immutable basic types
    Integer(i64),
    Float(f64),
    String(Arc<str>),
    Bytes(Arc<[u8]>),
    Boolean(bool),
    Range(i64, i64),
    Null,
    Undefined(Option<Arc<str>>),
    InstructionPackage(Arc<VMInstructionPackage>),

    // immutable container types
    Tuple(Arc<OnionTuple>),
    Pair(Arc<OnionPair>),
    LazySet(Arc<OnionLazySet>),
    Lambda((Arc<OnionLambdaDefinition>, Arc<OnionObject>)), // (definition, self_object)
    Custom(Arc<dyn OnionObjectExt>),

    // mutable? types, DO NOT USE THIS TYPE DIRECTLY, use 'mutablize' instead
    // 'Mut' is just a container for a weak reference to an OnionObjectCell,
    Mut(GCArcWeak<OnionObjectCell>),
}

/// Onion 对象扩展 trait。
///
/// 定义了所有自定义对象类型必须实现的接口，提供完整的对象行为规范。
/// 支持类型内省、GC 管理、类型转换、运算操作等功能。
///
/// # 核心功能分类
///
/// ## 类型内省与 GC 管理
/// - `as_any()`: 类型向下转换支持
/// - `upgrade()`: GC 弱引用升级
///
/// ## 基础类型转换
/// - `to_integer()`, `to_float()`, `to_string()` 等
/// - `to_boolean()`: 布尔值转换
/// - `repr()`: 调试表示
/// - `type_of()`: 类型名称
///
/// ## 容器操作
/// - `len()`: 长度获取
/// - `contains()`: 包含关系检查
/// - `apply()`: 函数应用
///
/// ## 键值操作
/// - `key_of()`, `value_of()`: 键值提取
/// - `with_attribute()`: 属性访问
///
/// ## 比较操作
/// - `equals()`: 值相等性比较（必须实现）
/// - `is_same()`: 引用相等性比较（必须实现）
/// - `binary_eq()`, `binary_lt()`, `binary_gt()`: 二元比较
///
/// ## 算术运算
/// - `binary_add()`, `binary_sub()`, `binary_mul()`, `binary_div()`: 四则运算
/// - `binary_mod()`, `binary_pow()`: 取模与幂运算
///
/// ## 逻辑运算
/// - `binary_and()`, `binary_or()`, `binary_xor()`: 位运算
/// - `binary_shl()`, `binary_shr()`: 位移运算
///
/// ## 一元运算
/// - `unary_neg()`: 负号运算
/// - `unary_not()`: 逻辑非运算
///
/// # 实现注意事项
/// - 所有方法都有默认的错误实现
/// - 只需要重写适用于特定类型的方法
/// - 必须实现 `equals()` 和 `is_same()` 方法
/// - 所有实现都应该是线程安全的
///
/// # 错误处理
/// 大部分方法默认返回 `InvalidType` 或 `InvalidOperation` 错误，
/// 具体类型应该重写相关方法提供正确的实现。
pub trait OnionObjectExt: GCTraceable<OnionObjectCell> + Debug + Send + Sync + 'static {
    // Type introspection for downcasting
    fn as_any(&self) -> &dyn std::any::Any;

    // GC and memory management
    fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>);

    // Basic type conversions
    fn to_integer(&self) -> Result<i64, RuntimeError> {
        Err(RuntimeError::InvalidType(
            format!("Cannot convert {:?} to Integer", self).into(),
        ))
    }
    fn to_float(&self) -> Result<f64, RuntimeError> {
        Err(RuntimeError::InvalidType(
            format!("Cannot convert {:?} to Float", self).into(),
        ))
    }
    #[allow(unused_variables)]
    fn to_string(&self, ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Err(RuntimeError::InvalidType(
            format!("Cannot convert {:?} to String", self).into(),
        ))
    }
    fn to_bytes(&self) -> Result<Box<[u8]>, RuntimeError> {
        Err(RuntimeError::InvalidType(
            format!("Cannot convert {:?} to Bytes", self).into(),
        ))
    }
    fn to_boolean(&self) -> Result<bool, RuntimeError> {
        Err(RuntimeError::InvalidType(
            format!("Cannot convert {:?} to Boolean", self).into(),
        ))
    }
    #[allow(unused_variables)]
    fn repr(&self, ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("{:?}", self))
    }
    fn type_of(&self) -> Result<String, RuntimeError> {
        Err(RuntimeError::InvalidType(
            format!("Cannot get type of {:?}", self).into(),
        ))
    }

    // Container operations
    fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("len() not supported for {:?}", self).into(),
        ))
    }
    fn contains(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("contains() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn apply(&self, value: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!(
                "apply() not supported for {:?} with value {:?}",
                self, value
            )
            .into(),
        ))
    }

    // Key-value operations
    fn key_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("key_of() not supported for {:?}", self).into(),
        ))
    }
    fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("value_of() not supported for {:?}", self).into(),
        ))
    }
    #[allow(unused_variables)]
    fn with_attribute(
        &self,
        key: &OnionObject,
        f: &mut dyn FnMut(&OnionObject) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!(
                "with_attribute() not supported for {:?} with key {:?}",
                self, key
            )
            .into(),
        ))
    }

    // Comparison operations
    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError>;
    fn is_same(&self, other: &OnionObject) -> Result<bool, RuntimeError>;
    fn binary_eq(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_eq() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_lt(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_lt() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_gt(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_gt() not supported for {:?} and {:?}", self, other).into(),
        ))
    }

    // Binary arithmetic operations
    fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_add() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_sub(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_sub() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_mul(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_mul() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_div(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_div() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_mod(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_mod() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_pow(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_pow() not supported for {:?} and {:?}", self, other).into(),
        ))
    }

    // Binary logical operations
    fn binary_and(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_and() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_or(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_or() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_xor(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_xor() not supported for {:?} and {:?}", self, other).into(),
        ))
    }

    // Binary shift operations
    fn binary_shl(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_shl() not supported for {:?} and {:?}", self, other).into(),
        ))
    }
    fn binary_shr(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("binary_shr() not supported for {:?} and {:?}", self, other).into(),
        ))
    }

    // Unary operations
    fn unary_neg(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("unary_neg() not supported for {:?}", self).into(),
        ))
    }
    fn unary_plus(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("unary_plus() not supported for {:?}", self).into(),
        ))
    }
    fn unary_not(&self) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("unary_not() not supported for {:?}", self).into(),
        ))
    }
}

impl Debug for OnionObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // 使用 repr
        write!(
            f,
            "{}",
            self.repr(&vec![])
                .unwrap_or_else(|_| "BrokenReference".to_string())
        )
    }
}

impl GCTraceable<OnionObjectCell> for OnionObject {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        match self {
            OnionObject::Mut(weak) => {
                queue.push_back(weak.clone());
            }
            OnionObject::Tuple(tuple) => tuple.collect(queue),
            OnionObject::Pair(pair) => pair.collect(queue),
            OnionObject::LazySet(lazy_set) => lazy_set.collect(queue),
            OnionObject::Lambda(lambda) => {
                lambda.0.collect(queue);
                lambda.1.collect(queue);
            }
            OnionObject::Custom(custom) => custom.collect(queue),

            _ => {}
        }
    }
}
impl OnionObject {
    /// 升级对象中的弱引用为强引用。
    ///
    /// 遍历对象结构，将所有弱引用升级为强引用，用于 GC 跟踪。
    ///
    /// # 参数
    /// - `collected`: 用于收集强引用的向量
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    collected.push(strong);
                }
            }
            OnionObject::Tuple(tuple) => tuple.upgrade(collected),
            OnionObject::Pair(pair) => pair.upgrade(collected),
            OnionObject::LazySet(lazy_set) => lazy_set.upgrade(collected),
            OnionObject::Lambda(lambda) => {
                lambda.0.upgrade(collected);
                lambda.1.upgrade(collected);
            }
            OnionObject::Custom(custom) => custom.upgrade(collected),
            _ => {}
        }
    }

    /// 将对象转换为对象单元格。
    ///
    /// 创建一个包含当前对象的 `OnionObjectCell`，用于 GC 管理。
    #[inline(always)]
    pub fn to_cell(self) -> OnionObjectCell {
        OnionObjectCell(RwLock::new(self))
    }

    /// 稳定化对象（引用方式）。
    ///
    /// 创建对象的 `OnionStaticObject` 包装，通过克隆实现。
    #[inline(always)]
    pub fn stabilize(&self) -> OnionStaticObject {
        OnionStaticObject::new(self.clone())
    }

    /// 稳定化对象（消费方式）。
    ///
    /// 通过消费当前对象创建 `OnionStaticObject`，避免克隆。
    #[inline(always)]
    pub fn consume_and_stabilize(self) -> OnionStaticObject {
        OnionStaticObject::new(self)
    }

    /// 获取对象的长度。
    ///
    /// 支持多种容器类型的长度计算：
    /// - Tuple: 元素数量
    /// - String: 字符串长度
    /// - Bytes: 字节数组长度
    /// - Range: 范围大小
    ///
    /// # 返回
    /// 包含长度值的静态整数对象
    pub fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Tuple(tuple) => tuple.len(),
            OnionObject::String(s) => {
                Ok(OnionStaticObject::new(OnionObject::Integer(s.len() as i64)))
            }
            OnionObject::Bytes(b) => {
                Ok(OnionStaticObject::new(OnionObject::Integer(b.len() as i64)))
            }
            OnionObject::Range(start, end) => Ok(OnionStaticObject::new(OnionObject::Integer(
                (end - start) as i64,
            ))),
            OnionObject::Custom(custom) => custom.len(),
            _ => Err(RuntimeError::InvalidOperation(
                format!("len() not supported for {:?}", self).into(),
            )),
        })
    }

    pub fn contains(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Tuple(tuple), _) => tuple.contains(other_obj),
                (OnionObject::String(s), OnionObject::String(other_s)) => {
                    Ok(s.contains(other_s.as_ref()))
                }
                (OnionObject::Bytes(b), OnionObject::Bytes(other_b)) => Ok(b
                    .windows(other_b.len())
                    .any(|window| window.eq(other_b.as_ref()))),
                (OnionObject::Range(l, r), OnionObject::Integer(i)) => Ok(*i >= *l && *i < *r),
                (OnionObject::Range(start, end), OnionObject::Float(f)) => {
                    Ok(*f >= *start as f64 && *f < *end as f64)
                }
                (OnionObject::Range(start, end), OnionObject::Range(other_start, other_end)) => {
                    Ok(*other_start >= *start && *other_end <= *end)
                }
                (OnionObject::Custom(custom), _) => custom.contains(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!("contains() not supported for {:?}", obj).into(),
                )),
            })
        })
    }

    #[inline(always)]
    pub fn with_data<T, F>(&self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&OnionObject) -> Result<T, RuntimeError>,
    {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    strong.as_ref().with_data(f)
                } else {
                    Err(RuntimeError::BrokenReference)
                }
            }
            _ => f(self),
        }
    }

    /// 对象的可变数据访问。
    ///
    /// 如果是 `Mut` 对象，则访问其指向的对象；否则直接访问当前对象。
    ///
    /// # 参数
    /// - `f`: 处理对象数据的可变闭包
    #[inline(always)]
    pub fn with_data_mut<T, F>(&mut self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&mut OnionObject) -> Result<T, RuntimeError>,
    {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    strong.as_ref().with_data_mut(f)
                } else {
                    Err(RuntimeError::BrokenReference)
                }
            }
            _ => f(self),
        }
    }

    /// 对可变对象进行赋值操作。
    ///
    /// 只有 `Mut` 类型的对象可以被赋值，这保证了不可变性原则。
    /// 赋值实际上是更新 `Mut` 对象指向的内容。
    ///
    /// # 参数
    /// - `other`: 要赋予的新值
    ///
    /// # 错误
    /// - `InvalidOperation`: 尝试对不可变对象赋值
    /// - `BrokenReference`: `Mut` 对象的引用已失效
    ///
    /// # 安全性
    /// 由于不可变对象无法保证赋值后的稳定性，只允许对 `Mut` 对象赋值。
    #[inline(always)]
    /// Assign a new value to a mutable object.
    pub fn assign(&self, other: &OnionObject) -> Result<(), RuntimeError> {
        // 由于我们无法保证赋值后GCArcWeak指向对象的稳定性，对不可变对象进行赋值操作会导致潜在的内存安全问题。
        // Mut类型由于是被GC管理的，因此可以安全地进行赋值操作（前提是GCArcWeak指向的对象仍然存在）。
        let OnionObject::Mut(weak) = self else {
            return Err(RuntimeError::InvalidOperation(
                format!("Cannot assign to immutable object: {:?}", self).into(),
            ));
        };
        match weak.upgrade() {
            Some(strong) => {
                // 先克隆要赋值的内容，避免借用冲突
                let new_value = other.with_data(|other| Ok(other.clone()))?;

                // 然后进行赋值
                strong.as_ref().with_data_mut(|obj| {
                    *obj = new_value;
                    Ok(())
                })
            }
            None => Err(RuntimeError::BrokenReference),
        }
    }

    /// 将对象转换为整数。
    ///
    /// 支持多种类型的整数转换：
    /// - Integer: 直接返回
    /// - Float: 截断为整数
    /// - String: 尝试解析为整数
    /// - Boolean: true=1, false=0
    /// - Custom: 调用自定义转换逻辑
    ///
    /// # 返回
    /// 转换后的整数值
    ///
    /// # 错误
    /// - `InvalidType`: 无法转换的类型或格式错误
    pub fn to_integer(&self) -> Result<i64, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(*i),
            OnionObject::Float(f) => Ok(*f as i64),
            OnionObject::String(s) => s
                .parse::<i64>()
                .map_err(|e| RuntimeError::InvalidType(e.to_string().into())),
            OnionObject::Boolean(b) => Ok(if *b { 1 } else { 0 }),
            OnionObject::Custom(custom) => custom.to_integer(),
            _ => Err(RuntimeError::InvalidType(
                format!("Cannot convert {:?} to Integer", obj).into(),
            )),
        })
    }
    pub fn to_float(&self) -> Result<f64, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(*i as f64),
            OnionObject::Float(f) => Ok(*f),
            OnionObject::String(s) => s
                .parse::<f64>()
                .map_err(|e| RuntimeError::InvalidType(e.to_string().into())),
            OnionObject::Boolean(b) => Ok(if *b { 1.0 } else { 0.0 }),
            OnionObject::Custom(custom) => custom.to_float(),
            _ => Err(RuntimeError::InvalidType(
                format!("Cannot convert {:?} to Float", obj).into(),
            )),
        })
    }

    pub fn to_string(&self, ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        self.with_data(|obj| {
            for ptr in ptrs {
                if addr_eq(obj, *ptr) {
                    return Ok("...".to_string());
                }
            }
            let mut new_ptrs = ptrs.clone();
            new_ptrs.push(obj);
            match obj {
                OnionObject::Integer(i) => Ok(i.to_string()),
                OnionObject::Float(f) => Ok(f.to_string()),
                OnionObject::String(s) => Ok(s.to_string()),
                OnionObject::Bytes(b) => Ok(format!(
                    "$\"{}\"",
                    general_purpose::STANDARD.encode(b.as_ref())
                )),
                OnionObject::Boolean(b) => Ok(if *b {
                    "true".to_string()
                } else {
                    "false".to_string()
                }),
                OnionObject::Null => Ok("null".to_string()),
                OnionObject::Undefined(s) => Ok(match s {
                    Some(s) => format!("undefined({:?})", s),
                    None => "undefined".to_string(),
                }),
                OnionObject::Range(start, end) => Ok(format!("{}..{}", start, end)),
                OnionObject::Tuple(tuple) => match tuple.get_elements().len() {
                    0 => Ok("()".to_string()),
                    1 => {
                        let first = tuple.get_elements().first().unwrap();
                        Ok(format!("({},)", first.repr(&new_ptrs)?))
                    }
                    _ => {
                        let elements: Result<Vec<String>, RuntimeError> = tuple
                            .get_elements()
                            .iter()
                            .map(|e| e.repr(&new_ptrs))
                            .collect();
                        Ok(format!("({})", elements?.join(", ")))
                    }
                },
                OnionObject::Pair(pair) => {
                    let left = pair.get_key().repr(&new_ptrs)?;
                    let right = pair.get_value().repr(&new_ptrs)?;
                    Ok(format!("{} : {}", left, right))
                }
                OnionObject::LazySet(lazy_set) => {
                    let container = lazy_set.get_container().repr(&new_ptrs)?;
                    let filter = lazy_set.get_filter().repr(&new_ptrs)?;
                    Ok(format!("[{} | {}]", container, filter))
                }
                OnionObject::InstructionPackage(_) => Ok("InstructionPackage(...)".to_string()),
                OnionObject::Lambda(lambda) => {
                    let body = lambda.0.get_body().to_string();
                    Ok(format!(
                        "{}::{} -> {}",
                        lambda.0.get_signature(),
                        lambda.0.get_parameter(),
                        body
                    ))
                }
                OnionObject::Custom(custom) => custom.to_string(&new_ptrs),
                _ => {
                    // 使用 Debug trait来处理其他类型的转换
                    Ok(format!("{:?}", obj))
                }
            }
        })
    }

    pub fn repr(&self, ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        self.with_data(|obj| {
            for ptr in ptrs {
                if addr_eq(obj, *ptr) {
                    return Ok("...".to_string());
                }
            }
            let mut new_ptrs = ptrs.clone();
            new_ptrs.push(obj);
            match obj {
                OnionObject::Integer(i) => Ok(format!("{}", i)),
                OnionObject::Float(f) => Ok(format!("{}", f)),
                OnionObject::String(s) => Ok(format!("{:?}", s)),
                OnionObject::Bytes(b) => Ok(format!(
                    "$\"{}\"",
                    general_purpose::STANDARD.encode(b.as_ref())
                )),
                OnionObject::Boolean(b) => Ok(format!("{}", b)),
                OnionObject::Null => Ok("null".to_string()),
                OnionObject::Undefined(s) => Ok(match s {
                    Some(s) => format!("undefined({:?})", s),
                    None => "undefined".to_string(),
                }),
                OnionObject::Range(start, end) => Ok(format!("{}..{}", start, end)),
                OnionObject::Tuple(tuple) => match tuple.get_elements().len() {
                    0 => Ok("()".to_string()),
                    1 => {
                        let first = tuple.get_elements().first().unwrap();
                        Ok(format!("({},)", first.repr(&new_ptrs)?))
                    }
                    _ => {
                        let elements: Result<Vec<String>, RuntimeError> = tuple
                            .get_elements()
                            .iter()
                            .map(|e| e.repr(&new_ptrs))
                            .collect();
                        Ok(format!("({})", elements?.join(", ")))
                    }
                },
                OnionObject::Pair(pair) => {
                    let left = pair.get_key().repr(&new_ptrs)?;
                    let right = pair.get_value().repr(&new_ptrs)?;
                    Ok(format!("{} : {}", left, right))
                }
                OnionObject::LazySet(lazy_set) => {
                    let container = lazy_set.get_container().repr(&new_ptrs)?;
                    let filter = lazy_set.get_filter().repr(&new_ptrs)?;
                    Ok(format!("[{} | {}]", container, filter))
                }
                OnionObject::InstructionPackage(_) => Ok("InstructionPackage(...)".to_string()),
                OnionObject::Lambda(lambda) => Ok(format!(
                    "{}::{} -> {}",
                    lambda.0.get_signature(),
                    lambda.0.get_parameter(),
                    lambda.0.get_body()
                )),
                OnionObject::Mut(weak) => {
                    if let Some(strong) = weak.upgrade() {
                        let inner_repr = strong
                            .as_ref()
                            .try_borrow()
                            .map_err(|_| {
                                RuntimeError::BorrowError(
                                    "Failed to borrow Mut object at 'repr'".into(),
                                )
                            })?
                            .repr(&new_ptrs)?;
                        Ok(format!("mut ({})", inner_repr))
                    } else {
                        Ok("Mut(BrokenReference)".to_string())
                    }
                }
                OnionObject::Custom(custom) => {
                    let custom_repr = custom.repr(&new_ptrs)?;
                    Ok(format!("Custom({})", custom_repr))
                }
            }
        })
    }
    pub fn to_bytes(&self) -> Result<Box<[u8]>, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(i.to_string().into_bytes().into_boxed_slice()),
            OnionObject::Float(f) => Ok(f.to_string().into_bytes().into_boxed_slice()),
            OnionObject::String(s) => Ok(s.as_bytes().to_vec().into_boxed_slice()),
            OnionObject::Bytes(b) => Ok(b.as_ref().to_vec().into_boxed_slice()),
            OnionObject::Boolean(b) => Ok(if *b {
                b"true".to_vec().into_boxed_slice()
            } else {
                b"false".to_vec().into_boxed_slice()
            }),
            OnionObject::Custom(custom) => custom.to_bytes(),
            _ => Err(RuntimeError::InvalidType(
                format!("Cannot convert {:?} to Bytes", obj).into(),
            )),
        })
    }

    pub fn to_boolean(&self) -> Result<bool, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(*i != 0),
            OnionObject::Float(f) => Ok(*f != 0.0),
            OnionObject::String(s) => Ok(!s.is_empty()),
            OnionObject::Bytes(b) => Ok(!b.is_empty()),
            OnionObject::Boolean(b) => Ok(*b),
            OnionObject::Null => Ok(false),
            OnionObject::Undefined(_) => Ok(false),
            OnionObject::Custom(custom) => custom.to_boolean(),
            _ => Err(RuntimeError::InvalidType(
                format!("Cannot convert {:?} to Boolean", obj).into(),
            )),
        })
    }

    /// 将对象转换为可变对象。
    ///
    /// 在 GC 中创建一个新的 `OnionObjectCell`，并返回指向它的 `Mut` 对象。
    /// 这是创建可变引用的正确方式，遵循幂等性原则。
    ///
    /// # 参数
    /// - `gc`: GC 管理器，用于分配新的对象单元格
    ///
    /// # 返回
    /// 包含 `Mut` 对象的静态对象
    ///
    /// # 幂等性
    /// 多次调用 `mutablize` 会创建指向相同值的不同可变对象，
    /// 这符合 Onion VM 的不可变性设计。
    #[inline(always)]
    fn mutablize(self, gc: &mut GC<OnionObjectCell>) -> OnionStaticObject {
        let arc = gc.create(OnionObjectCell::from(self));
        OnionStaticObject {
            obj: OnionObject::Mut(arc.as_weak()),
            _arcs: GCArcStorage::Single(arc),
        }
    }
}

/// OnionObject 的核心方法实现。
///
/// 包含对象的相等性比较、类型转换、运算操作等核心功能。
/// 所有方法都遵循 Onion VM 的不可变性和幂等性原则。
impl OnionObject {
    /// 对象相等性比较。
    ///
    /// 实现了类型兼容的相等性检查，支持：
    /// - 基础类型的直接比较
    /// - 数值类型的隐式转换比较（Integer ↔ Float）
    /// - 容器类型的递归比较
    /// - 自定义类型的扩展比较
    ///
    /// # 参数
    /// - `other`: 要比较的另一个对象
    ///
    /// # 返回
    /// - `Ok(true)`: 对象相等
    /// - `Ok(false)`: 对象不相等
    /// - `Err(RuntimeError)`: 比较过程中发生错误
    pub fn equals(&self, other: &Self) -> Result<bool, RuntimeError> {
        self.with_data(|left| {
            other.with_data(|right| {
                match (left, right) {
                    (OnionObject::Integer(i1), OnionObject::Integer(i2)) => Ok(i1 == i2),
                    (OnionObject::Float(f1), OnionObject::Float(f2)) => Ok(f1 == f2),
                    (OnionObject::Integer(i1), OnionObject::Float(f2)) => Ok(*i1 as f64 == *f2),
                    (OnionObject::Float(f1), OnionObject::Integer(i2)) => Ok(*f1 == *i2 as f64),
                    (OnionObject::String(s1), OnionObject::String(s2)) => Ok(s1 == s2),
                    (OnionObject::Bytes(b1), OnionObject::Bytes(b2)) => Ok(b1 == b2),
                    (OnionObject::Boolean(b1), OnionObject::Boolean(b2)) => Ok(b1 == b2),
                    (OnionObject::Range(start1, end1), OnionObject::Range(start2, end2)) => {
                        Ok(start1 == start2 && end1 == end2)
                    }
                    (OnionObject::Null, OnionObject::Null) => Ok(true),
                    (OnionObject::Undefined(_), OnionObject::Undefined(_)) => Ok(true),
                    (OnionObject::Tuple(t1), _) => t1.equals(other),
                    (OnionObject::Pair(p1), _) => p1.equals(other),
                    (OnionObject::Custom(c1), _) => c1.equals(other),

                    // 理论上Mut类型不应该出现在这里
                    _ => Ok(false),
                }
            })
        })
    }
    pub fn is_same(&self, other: &Self) -> Result<bool, RuntimeError> {
        match (self, other) {
            (OnionObject::Mut(weak1), OnionObject::Mut(weak2)) => {
                if let (Some(strong1), Some(strong2)) = (weak1.upgrade(), weak2.upgrade()) {
                    Ok(addr_eq(strong1.as_ref(), strong2.as_ref()))
                } else {
                    Ok(false)
                }
            }
            (OnionObject::Custom(c1), _) => c1.is_same(other),
            _ => self.equals(other),
        }
    }

    pub fn binary_add(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 + i2)))
                }
                (OnionObject::Float(f1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 + f2)))
                }
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(*i1 as f64 + f2)))
                }
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 + *i2 as f64)))
                }
                (OnionObject::String(s1), OnionObject::String(s2)) => Ok(OnionStaticObject::new(
                    OnionObject::String(Arc::from(format!("{}{}", s1, s2))),
                )),
                (OnionObject::Bytes(b1), OnionObject::Bytes(b2)) => {
                    let mut new_bytes = Vec::with_capacity(b1.len() + b2.len());
                    new_bytes.extend_from_slice(b1.as_ref());
                    new_bytes.extend_from_slice(b2.as_ref());
                    Ok(OnionStaticObject::new(OnionObject::Bytes(Arc::from(
                        new_bytes,
                    ))))
                }
                (OnionObject::Range(start1, end1), OnionObject::Range(start2, end2)) => Ok(
                    OnionStaticObject::new(OnionObject::Range(start1 + start2, end1 + end2)),
                ),
                (OnionObject::Tuple(t1), _) => t1.binary_add(other_obj),
                (OnionObject::Custom(c1), _) => c1.binary_add(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary add operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_sub(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 - i2)))
                }
                (OnionObject::Float(f1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 - f2)))
                }
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(*i1 as f64 - f2)))
                }
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 - *i2 as f64)))
                }
                (OnionObject::Custom(c1), _) => c1.binary_sub(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary sub operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_mul(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 * i2)))
                }
                (OnionObject::Float(f1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 * f2)))
                }
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(*i1 as f64 * f2)))
                }
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 * *i2 as f64)))
                }
                (OnionObject::Custom(c1), _) => c1.binary_mul(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary mul operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_div(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    if *i2 == 0 {
                        return Err(RuntimeError::InvalidOperation("Division by zero".into()));
                    }
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 / i2)))
                }
                (OnionObject::Float(f1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 / f2)))
                }
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(*i1 as f64 / f2)))
                }
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 / *i2 as f64)))
                }
                (OnionObject::Custom(c1), _) => c1.binary_div(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary div operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_mod(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    if *i2 == 0 {
                        return Err(RuntimeError::InvalidOperation("Division by zero".into()));
                    }
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 % i2)))
                }
                (OnionObject::Float(f1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 % f2)))
                }
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(*i1 as f64 % f2)))
                }
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1 % *i2 as f64)))
                }
                (OnionObject::Custom(c1), _) => c1.binary_mod(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary mod operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_pow(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => Ok(OnionStaticObject::new(
                    OnionObject::Integer(i1.pow(*i2 as u32)),
                )),
                (OnionObject::Float(f1), OnionObject::Float(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Float(f1.powf(*f2))))
                }
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => Ok(OnionStaticObject::new(
                    OnionObject::Float((*i1 as f64).powf(*f2)),
                )),
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => Ok(OnionStaticObject::new(
                    OnionObject::Float(f1.powi(*i2 as i32)),
                )),
                (OnionObject::Custom(c1), _) => c1.binary_pow(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary pow operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_and(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 & i2)))
                }
                (OnionObject::Boolean(f1), OnionObject::Boolean(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Boolean(*f1 && *f2)))
                }
                (OnionObject::Custom(c1), _) => c1.binary_and(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary and operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_or(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 | i2)))
                }
                (OnionObject::Boolean(f1), OnionObject::Boolean(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Boolean(*f1 || *f2)))
                }
                (OnionObject::Custom(c1), _) => c1.binary_or(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary or operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_xor(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 ^ i2)))
                }
                (OnionObject::Custom(c1), _) => c1.binary_xor(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary xor operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_shl(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 << i2)))
                }
                (OnionObject::Custom(c1), _) => c1.binary_shl(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary shl operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_shr(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 >> i2)))
                }
                (OnionObject::Custom(c1), _) => c1.binary_shr(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary shr operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_eq(&self, other: &Self) -> Result<bool, RuntimeError> {
        self.equals(other)
    }

    pub fn binary_lt(&self, other: &Self) -> Result<bool, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => Ok(i1 < i2),
                (OnionObject::Float(f1), OnionObject::Float(f2)) => Ok(f1 < f2),
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => Ok((*i1 as f64) < *f2),
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => Ok(*f1 < *i2 as f64),
                (OnionObject::Custom(c1), _) => c1.binary_lt(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary lt operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn binary_gt(&self, other: &Self) -> Result<bool, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => Ok(i1 > i2),
                (OnionObject::Float(f1), OnionObject::Float(f2)) => Ok(f1 > f2),
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => Ok((*i1 as f64) > *f2),
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => Ok(*f1 > *i2 as f64),
                (OnionObject::Custom(c1), _) => c1.binary_gt(other_obj),
                _ => Err(RuntimeError::InvalidOperation(
                    format!(
                        "Invalid binary gt operation for {:?} and {:?}",
                        obj, other_obj
                    )
                    .into(),
                )),
            })
        })
    }

    pub fn unary_neg(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(OnionStaticObject::new(OnionObject::Integer(-i))),
            OnionObject::Float(f) => Ok(OnionStaticObject::new(OnionObject::Float(-f))),
            OnionObject::Custom(custom) => custom.unary_neg(),
            _ => Err(RuntimeError::InvalidOperation(
                format!("Invalid unary neg operation for {:?}", obj).into(),
            )),
        })
    }

    pub fn unary_plus(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(OnionStaticObject::new(OnionObject::Integer(i.abs()))),
            OnionObject::Float(f) => Ok(OnionStaticObject::new(OnionObject::Float(f.abs()))),
            OnionObject::Custom(custom) => custom.unary_plus(),
            _ => Err(RuntimeError::InvalidOperation(
                format!("Invalid unary plus operation for {:?}", obj).into(),
            )),
        })
    }

    pub fn unary_not(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Boolean(b) => Ok(OnionStaticObject::new(OnionObject::Boolean(!b))),
            OnionObject::Integer(i) => Ok(OnionStaticObject::new(OnionObject::Integer(!i))),
            OnionObject::Custom(custom) => custom.unary_not(),
            _ => Err(RuntimeError::InvalidOperation(
                format!("Invalid unary not operation for {:?}", obj).into(),
            )),
        })
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        self.with_data(|obj| match obj {
            OnionObject::Integer(_) => {
                if let OnionObject::String(key_str) = key {
                    match key_str.as_ref() {
                        "int" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::int",
                                OnionKeyPool::create(vec![]),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::float",
                                OnionKeyPool::create(vec![]),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::string",
                                OnionKeyPool::create(vec![]),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bool",
                                OnionKeyPool::create(vec![]),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bytes",
                                OnionKeyPool::create(vec![]),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        _ => {}
                    }
                }
                Err(RuntimeError::InvalidOperation(
                    format!(
                        "Attribute '{}' not found for Integer",
                        match key {
                            OnionObject::String(s) => s.as_ref(),
                            _ => "<non-string>",
                        }
                    )
                    .into(),
                ))
            }
            OnionObject::Float(_) => {
                if let OnionObject::String(key_str) = key {
                    match key_str.as_ref() {
                        "int" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::int",
                                OnionKeyPool::create(vec![]),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::float",
                                OnionKeyPool::create(vec![]),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::string",
                                OnionKeyPool::create(vec![]),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bool",
                                OnionKeyPool::create(vec![]),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bytes",
                                OnionKeyPool::create(vec![]),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        _ => {}
                    }
                }
                Err(RuntimeError::InvalidOperation(
                    format!(
                        "Attribute '{}' not found for Float",
                        match key {
                            OnionObject::String(s) => s.as_ref(),
                            _ => "<non-string>",
                        }
                    )
                    .into(),
                ))
            }
            OnionObject::Boolean(_) => {
                if let OnionObject::String(key_str) = key {
                    match key_str.as_ref() {
                        "int" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::int",
                                OnionKeyPool::create(vec![]),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::float",
                                OnionKeyPool::create(vec![]),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::string",
                                OnionKeyPool::create(vec![]),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bool",
                                OnionKeyPool::create(vec![]),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bytes",
                                OnionKeyPool::create(vec![]),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        _ => {}
                    }
                }
                Err(RuntimeError::InvalidOperation(
                    format!(
                        "Attribute '{}' not found for Boolean",
                        match key {
                            OnionObject::String(s) => s.as_ref(),
                            _ => "<non-string>",
                        }
                    )
                    .into(),
                ))
            }
            OnionObject::Tuple(tuple) => {
                // 先检查原型链/自定义属性
                if let Ok(result) = tuple.with_attribute(key, f) {
                    return Ok(result);
                }

                // 再检查native方法
                if let OnionObject::String(key_str) = key {
                    match key_str.as_ref() {
                        "int" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::int",
                                OnionKeyPool::create(vec![]),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::float",
                                OnionKeyPool::create(vec![]),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::string",
                                OnionKeyPool::create(vec![]),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bool",
                                OnionKeyPool::create(vec![]),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bytes",
                                OnionKeyPool::create(vec![]),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        "length" => {
                            let length_method = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "builtin::length",
                                OnionKeyPool::create(vec![]),
                                &native_length_method,
                            );
                            return f(length_method.weak());
                        }
                        "elements" => {
                            let elements_method = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "builtin::elements",
                                OnionKeyPool::create(vec![]),
                                &native_elements_method,
                            );
                            return f(elements_method.weak());
                        }
                        _ => {}
                    }
                }
                Err(RuntimeError::InvalidOperation(
                    format!(
                        "Attribute '{}' not found for Tuple",
                        match key {
                            OnionObject::String(s) => s.as_ref(),
                            _ => "<non-string>",
                        }
                    )
                    .into(),
                ))
            }
            OnionObject::Pair(pair) => pair.with_attribute(key, f),
            OnionObject::Lambda(lambda) => lambda.0.with_attribute(key, f),
            OnionObject::LazySet(lazy_set) => lazy_set.with_attribute(key, f),
            OnionObject::String(_) => {
                if let OnionObject::String(key_str) = key {
                    match key_str.as_ref() {
                        "int" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::int",
                                OnionKeyPool::create(vec![]),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::float",
                                OnionKeyPool::create(vec![]),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::string",
                                OnionKeyPool::create(vec![]),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bool",
                                OnionKeyPool::create(vec![]),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bytes",
                                OnionKeyPool::create(vec![]),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        "length" => {
                            let length_method = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "builtin::length",
                                OnionKeyPool::create(vec![]),
                                &native_length_method,
                            );
                            return f(length_method.weak());
                        }
                        "elements" => {
                            let elements_method = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "builtin::elements",
                                OnionKeyPool::create(vec![]),
                                &native_elements_method,
                            );
                            return f(elements_method.weak());
                        }
                        _ => {}
                    }
                }
                Err(RuntimeError::InvalidOperation(
                    format!(
                        "Attribute '{}' not found for String",
                        match key {
                            OnionObject::String(s) => s.as_ref(),
                            _ => "<non-string>",
                        }
                    )
                    .into(),
                ))
            }
            OnionObject::Bytes(_) => {
                if let OnionObject::String(key_str) = key {
                    match key_str.as_ref() {
                        "int" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::int",
                                OnionKeyPool::create(vec![]),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::float",
                                OnionKeyPool::create(vec![]),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::string",
                                OnionKeyPool::create(vec![]),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bool",
                                OnionKeyPool::create(vec![]),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bytes",
                                OnionKeyPool::create(vec![]),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        "length" => {
                            let length_method = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "builtin::length",
                                OnionKeyPool::create(vec![]),
                                &native_length_method,
                            );
                            return f(length_method.weak());
                        }
                        "elements" => {
                            let elements_method = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "builtin::elements",
                                OnionKeyPool::create(vec![]),
                                &native_elements_method,
                            );
                            return f(elements_method.weak());
                        }
                        _ => {}
                    }
                }
                Err(RuntimeError::InvalidOperation(
                    format!(
                        "Attribute '{}' not found for Bytes",
                        match key {
                            OnionObject::String(s) => s.as_ref(),
                            _ => "<non-string>",
                        }
                    )
                    .into(),
                ))
            }
            OnionObject::Range(_, _) => {
                if let OnionObject::String(key_str) = key {
                    match key_str.as_ref() {
                        "int" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::int",
                                OnionKeyPool::create(vec![]),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::float",
                                OnionKeyPool::create(vec![]),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::string",
                                OnionKeyPool::create(vec![]),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bool",
                                OnionKeyPool::create(vec![]),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "converter::bytes",
                                OnionKeyPool::create(vec![]),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        "length" => {
                            let length_method = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "builtin::length",
                                OnionKeyPool::create(vec![]),
                                &native_length_method,
                            );
                            return f(length_method.weak());
                        }
                        "elements" => {
                            let elements_method = wrap_native_function(
                                LambdaParameter::Multiple(Box::new([])),
                                OnionFastMap::new(OnionKeyPool::create(vec![])),
                                obj,
                                "builtin::elements",
                                OnionKeyPool::create(vec![]),
                                &native_elements_method,
                            );
                            return f(elements_method.weak());
                        }
                        _ => {}
                    }
                }
                Err(RuntimeError::InvalidOperation(
                    format!(
                        "Attribute '{}' not found for Range",
                        match key {
                            OnionObject::String(s) => s.as_ref(),
                            _ => "<non-string>",
                        }
                    )
                    .into(),
                ))
            }
            OnionObject::Custom(custom) => {
                let mut result: Result<R, RuntimeError> = Err(RuntimeError::InvalidOperation(
                    "Custom with_attribute not called".into(),
                ));
                let mut closure = |obj: &OnionObject| -> Result<(), RuntimeError> {
                    result = f(obj);
                    Ok(())
                };
                custom.with_attribute(key, &mut closure)?;
                result
            }
            _ => Err(RuntimeError::InvalidOperation(
                format!("with_attribute() not supported for {:?}", self).into(),
            )),
        })
    }
    pub fn apply(&self, value: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            value.with_data(|value| match obj {
                OnionObject::Tuple(tuple) => {
                    let index = match value {
                        OnionObject::Integer(i) => *i,
                        _ => {
                            return Err(RuntimeError::InvalidType(
                                "Argument 'index' must be an integer".into(),
                            ));
                        }
                    };

                    let elements = tuple.get_elements();
                    if (index as usize) < elements.len() {
                        Ok(OnionStaticObject::new(elements[index as usize].clone()))
                    } else {
                        Err(RuntimeError::InvalidOperation(
                            "Index out of bounds for tuple".into(),
                        ))
                    }
                }
                OnionObject::String(s) => {
                    let index = match value {
                        OnionObject::Integer(i) => *i,
                        _ => {
                            return Err(RuntimeError::InvalidType(
                                "Argument 'index' must be an integer".into(),
                            ));
                        }
                    };
                    if index < 0 || index >= s.len() as i64 {
                        return Err(RuntimeError::InvalidOperation(
                            format!("Index out of bounds for String: {}", s).into(),
                        ));
                    }
                    Ok(OnionStaticObject::new(OnionObject::String(Arc::from(
                        s.chars().nth(index as usize).unwrap().to_string(),
                    ))))
                }
                OnionObject::Bytes(b) => {
                    let index = match value {
                        OnionObject::Integer(i) => *i,
                        _ => {
                            return Err(RuntimeError::InvalidType(
                                "Argument 'index' must be an integer".into(),
                            ));
                        }
                    };
                    if index < 0 || index >= b.len() as i64 {
                        return Err(RuntimeError::InvalidOperation(
                            format!("Index out of bounds for Bytes: {:?}", b).into(),
                        ));
                    }
                    Ok(OnionStaticObject::new(OnionObject::Bytes(Arc::from(vec![
                        b[index as usize],
                    ]))))
                }
                OnionObject::Custom(custom) => custom.apply(value),
                _ => Err(RuntimeError::InvalidOperation(
                    format!("index_of() not supported for {:?}", self).into(),
                )),
            })
        })
    }

    pub fn key_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Pair(pair) => Ok(pair.get_key().stabilize()),
            OnionObject::Lambda(lambda) => Ok(lambda.0.get_parameter().to_onion()),
            OnionObject::LazySet(set) => Ok(set.get_container().stabilize()),
            OnionObject::Custom(custom) => custom.key_of(),
            _ => Err(RuntimeError::InvalidOperation(
                format!("key_of() not supported for {:?}", obj).into(),
            )),
        })
    }

    pub fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Pair(pair) => Ok(pair.get_value().stabilize()),
            OnionObject::LazySet(set) => Ok(set.get_filter().stabilize()),
            OnionObject::Undefined(s) => Ok(OnionStaticObject::new(OnionObject::String(
                Arc::from(s.as_ref().map(|o| o.as_ref()).unwrap_or_else(|| "")),
            ))),
            OnionObject::Custom(custom) => custom.value_of(),
            _ => Err(RuntimeError::InvalidOperation(
                format!("value_of() not supported for {:?}", obj).into(),
            )),
        })
    }

    pub fn type_of(&self) -> Result<String, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(_) => Ok("Integer".to_string()),
            OnionObject::Float(_) => Ok("Float".to_string()),
            OnionObject::String(_) => Ok("String".to_string()),
            OnionObject::Bytes(_) => Ok("Bytes".to_string()),
            OnionObject::Boolean(_) => Ok("Boolean".to_string()),
            OnionObject::Null => Ok("Null".to_string()),
            OnionObject::Undefined(_) => Ok("Undefined".to_string()),
            OnionObject::Tuple(_) => Ok("Tuple".to_string()),
            OnionObject::Pair(_) => Ok("Pair".to_string()),
            OnionObject::LazySet(_) => Ok("LazySet".to_string()),
            OnionObject::InstructionPackage(_) => Ok("InstructionPackage".to_string()),
            OnionObject::Lambda(_) => Ok("Lambda".to_string()),
            OnionObject::Custom(custom) => custom.type_of(),
            _ => Err(RuntimeError::InvalidOperation(
                format!("type_of() not supported for {:?}", obj).into(),
            )),
        })
    }

    #[inline(always)]
    pub fn copy(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| Ok(obj.stabilize()))
    }
}

#[derive(Clone)]
/// GC 强引用存储策略枚举。
///
/// 根据对象的复杂性选择不同的引用存储策略，优化内存使用和性能。
///
/// # 变体说明
/// - `None`: 无需额外引用（基础类型如 Integer, String）
/// - `Single`: 单个强引用（如 Mut 对象）
/// - `Multiple`: 多个强引用（复杂对象如 Tuple, Lambda）
///
/// # 设计考虑
/// 这种分层设计避免了为简单对象分配不必要的 Vec，
/// 同时为复杂对象提供了灵活的引用管理。
pub enum GCArcStorage {
    None,
    Single(GCArc<OnionObjectCell>),
    Multiple(Arc<Vec<GCArc<OnionObjectCell>>>),
}

// impl GCArcStorage {
//     #[inline(always)]
//     pub fn from_vec(v: Vec<GCArc<OnionObjectCell>>) -> Self {
//         match v.len() {
//             0 => Self::None,
//             1 => Self::Single(v[0].clone()),
//             _ => Self::Multiple(v),
//         }
//     }
// }

#[derive(Clone)]
/// Onion 静态对象封装。
///
/// 稳定化的对象封装，用于跨函数调用、长期存储和序列化。
/// 通过持有必要的强引用来确保对象在使用期间不被 GC 回收。
///
/// # 设计目的
/// - **跨边界传递**: 在函数调用、模块间传递对象
/// - **长期存储**: 作为常量、全局变量等存储
/// - **GC 稳定性**: 确保对象在使用期间不被回收
/// - **序列化支持**: 提供对象的稳定表示
///
/// # 内部结构
/// - `obj`: 实际的对象数据
/// - `_arcs`: GC 强引用存储，防止对象被回收
///
/// # GC 策略
/// 根据对象类型采用不同的 GC 策略：
/// - **基础类型**: 无需额外引用（如 Integer, String 等）
/// - **Mut 类型**: 持有单个强引用
/// - **复杂类型**: 持有多个强引用（如 Tuple, Lambda 等）
///
/// # 关键方法
/// - `new()`: 创建静态对象并自动设置 GC 引用
/// - `weak()`: 获取内部对象的弱引用
/// - `mutablize()`: 转换为可变对象
/// - `immutablize()`: 从可变对象中提取值
pub struct OnionStaticObject {
    _arcs: GCArcStorage,
    obj: OnionObject,
}

impl Default for OnionStaticObject {
    fn default() -> Self {
        OnionStaticObject {
            obj: OnionObject::Undefined(None),
            _arcs: GCArcStorage::None,
        }
    }
}

impl Debug for OnionStaticObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "OnionStaticObject({:?})", self.obj)
    }
}

impl Display for OnionStaticObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "OnionStaticObject({:?})", self.obj)
    }
}

impl OnionStaticObject {
    #[inline(always)]
    pub fn new(obj: OnionObject) -> Self {
        let arcs = match &obj {
            OnionObject::Mut(obj) => match obj.upgrade() {
                None => GCArcStorage::None,
                Some(arc) => GCArcStorage::Single(arc),
            },
            OnionObject::Boolean(_)
            | OnionObject::Integer(_)
            | OnionObject::Float(_)
            | OnionObject::String(_)
            | OnionObject::Bytes(_)
            | OnionObject::Null
            | OnionObject::Undefined(_)
            | OnionObject::Range(_, _)
            | OnionObject::InstructionPackage(_) => GCArcStorage::None,
            _ => {
                let mut arcs = vec![];
                obj.upgrade(&mut arcs);
                GCArcStorage::Multiple(Arc::new(arcs))
            }
        };
        OnionStaticObject {
            obj: obj,
            _arcs: arcs,
        }
    }

    #[inline(always)]
    pub fn weak(&self) -> &OnionObject {
        &self.obj
    }

    #[inline(always)]
    /// 将值装箱成可变容器，严格遵循幂等律的情况下，Cell里不可能出现Mut对象。
    /// 也就是说，mut mut x和mut x是等价的。
    pub fn mutablize(self, gc: &mut GC<OnionObjectCell>) -> OnionStaticObject {
        match self.weak() {
            OnionObject::Mut(_) => self,
            v => v.clone().mutablize(gc),
        }
    }

    #[inline(always)]
    /// 将值从可变容器中卸载，严格遵循幂等律
    /// 也就是说，const const x和const x是等价的。
    pub fn immutablize(self) -> Result<OnionStaticObject, RuntimeError> {
        match self.weak() {
            OnionObject::Mut(v) => match v.upgrade() {
                None => Err(RuntimeError::BrokenReference),
                Some(arc) => match arc.as_ref().0.read() {
                    Ok(data) => Ok(OnionStaticObject::new(data.clone())),
                    Err(_) => Err(RuntimeError::BrokenReference),
                },
            },
            _ => Ok(self),
        }
    }
}

#[macro_export]
macro_rules! unwrap_object {
    ($obj:expr, $variant:path) => {
        match $obj {
            $variant(o) => Ok(o),
            _ => Err(RuntimeError::InvalidType(
                format!("Expected {}, found {:?}", stringify!($variant), $obj).into(),
            )),
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn test_detailed_memory_sizes() {
        println!("详细内存分析:");
        println!(
            "OnionObjectCell: {} bytes",
            std::mem::size_of::<OnionObjectCell>()
        );
        println!("OnionObject: {} bytes", std::mem::size_of::<OnionObject>());
        println!(
            "OnionStaticObject: {} bytes",
            std::mem::size_of::<OnionStaticObject>()
        );
        println!(
            "GCArcStorage: {} bytes",
            std::mem::size_of::<GCArcStorage>()
        );
        println!(
            "GCArc<OnionObjectCell>: {} bytes",
            std::mem::size_of::<GCArc<OnionObjectCell>>()
        );
        println!("Arc<String>: {} bytes", std::mem::size_of::<Arc<String>>());
        println!(
            "Arc<Vec<u8>>: {} bytes",
            std::mem::size_of::<Arc<Vec<u8>>>()
        );
        println!(
            "GCArcWeak<OnionObjectCell>: {} bytes",
            std::mem::size_of::<GCArcWeak<OnionObjectCell>>()
        );
        println!("OnionTuple: {} bytes", std::mem::size_of::<OnionTuple>());
        println!("OnionPair: {} bytes", std::mem::size_of::<OnionPair>());
        println!(
            "OnionLazySet: {} bytes",
            std::mem::size_of::<OnionLazySet>()
        );
    }

    #[test]
    fn benchmark_realistic_vm_operations() {
        println!("真实VM操作性能测试 (使用OnionStaticObject + clone):");

        // 模拟VM中的整数运算
        let start = Instant::now();
        let mut result_sum = 0i64;

        for i in 0..5_000_000 {
            // 创建OnionStaticObject（模拟从栈或常量池加载）
            let obj1 = OnionObject::Integer(i).stabilize();
            let obj2 = OnionObject::Integer(i + 1).stabilize();

            // 通过with_data访问（模拟VM的实际访问模式）
            let result = obj1.weak().with_data(|data1| {
                obj2.weak().with_data(|data2| {
                    // 模拟binary_add操作
                    match (data1, data2) {
                        (OnionObject::Integer(a), OnionObject::Integer(b)) => {
                            Ok(OnionObject::Integer(a + b).stabilize())
                        }
                        _ => Err(RuntimeError::InvalidOperation("Type error".into())),
                    }
                })
            });

            if let Ok(sum) = result {
                // 提取结果值（模拟VM获取计算结果）
                if let Ok(val) = sum.weak().with_data(|data| match data {
                    OnionObject::Integer(v) => Ok(*v),
                    _ => Err(RuntimeError::InvalidType("Not integer".into())),
                }) {
                    result_sum += val;
                }
            }
        }

        let duration = start.elapsed();
        println!("500万次VM风格整数运算: {:.2}s", duration.as_secs_f64());
        println!("每秒操作数: {:.0}", 5_000_000.0 / duration.as_secs_f64());
        println!("结果校验: {}", result_sum);
    }

    #[test]
    fn benchmark_vm_style_arithmetic() {
        println!("VM风格算术运算性能测试:");

        let start = Instant::now();
        let mut final_result = 0i64;

        for i in 0..2_000_000 {
            // 创建操作数
            let left = OnionObject::Integer(i).stabilize();
            let right = OnionObject::Integer(i + 1).stabilize();

            // 使用实际的binary_add方法
            if let Ok(result) = left
                .weak()
                .with_data(|l_data| right.weak().with_data(|r_data| l_data.binary_add(r_data)))
            {
                // 继续进行乘法运算
                let multiplier = OnionObject::Integer(2).stabilize();
                if let Ok(mul_result) = result.weak().with_data(|add_data| {
                    multiplier
                        .weak()
                        .with_data(|mul_data| add_data.binary_mul(mul_data))
                }) {
                    // 提取最终结果
                    if let Ok(val) = mul_result.weak().with_data(|data| data.to_integer()) {
                        final_result += val;
                    }
                }
            }
        }

        let duration = start.elapsed();
        println!("200万次复合运算: {:.2}s", duration.as_secs_f64());
        println!("每秒操作数: {:.0}", 2_000_000.0 / duration.as_secs_f64());
        println!("最终结果: {}", final_result);
    }

    #[test]
    fn benchmark_object_creation_overhead() {
        println!("对象创建开销测试:");

        // 测试OnionStaticObject创建性能
        let start = Instant::now();
        let mut objects = Vec::with_capacity(1_000_000);

        for i in 0..1_000_000 {
            let obj = OnionObject::Integer(i).stabilize();
            objects.push(obj);
        }

        let creation_time = start.elapsed();
        println!(
            "100万个OnionStaticObject创建: {:.2}s",
            creation_time.as_secs_f64()
        );

        // 测试访问性能
        let start = Instant::now();
        let mut sum = 0i64;

        for obj in &objects {
            if let Ok(val) = obj.weak().with_data(|data| data.to_integer()) {
                sum += val;
            }
        }

        let access_time = start.elapsed();
        println!("100万次对象访问: {:.2}s", access_time.as_secs_f64());
        println!("访问校验和: {}", sum);

        // 测试克隆性能
        let start = Instant::now();
        let mut cloned_objects = Vec::with_capacity(objects.len());

        for obj in &objects[..100_000] {
            // 只测试10万个避免内存不足
            cloned_objects.push(obj.clone());
        }

        let clone_time = start.elapsed();
        println!("10万个对象克隆: {:.2}s", clone_time.as_secs_f64());
    }

    #[test]
    fn benchmark_string_operations_realistic() {
        println!("真实字符串操作性能测试:");

        let start = Instant::now();
        let mut total_length = 0usize;

        for i in 0..500_000 {
            // 创建字符串对象
            let str_obj = OnionObject::String(Arc::from(format!("string_{}", i))).stabilize();

            // 获取字符串长度（模拟len()操作）
            if let Ok(len_obj) = str_obj.weak().with_data(|data| data.len()) {
                if let Ok(length) = len_obj.weak().with_data(|data| data.to_integer()) {
                    total_length += length as usize;
                }
            }

            // 字符串拼接操作
            let suffix = OnionObject::String(Arc::from("_suffix".to_string())).stabilize();
            if let Ok(concat_result) = str_obj.weak().with_data(|str_data| {
                suffix
                    .weak()
                    .with_data(|suffix_data| str_data.binary_add(suffix_data))
            }) {
                // 模拟使用拼接结果
                if let Ok(concat_str) = concat_result
                    .weak()
                    .with_data(|data| data.to_string(&mut vec![]))
                {
                    total_length += concat_str.len();
                }
            }
        }

        let duration = start.elapsed();
        println!("50万次字符串操作: {:.2}s", duration.as_secs_f64());
        println!("每秒操作数: {:.0}", 500_000.0 / duration.as_secs_f64());
        println!("总字符串长度: {}", total_length);
    }

    #[test]
    fn benchmark_refcell_overhead() {
        println!("RefCell开销分析:");

        // 测试直接访问vs RefCell访问的性能差异
        let direct_integers: Vec<i64> = (0..1_000_000).collect();
        let wrapped_integers: Vec<OnionStaticObject> = (0..1_000_000)
            .map(|i| OnionObject::Integer(i).stabilize())
            .collect();

        // 直接访问基准
        let start = Instant::now();
        let mut sum1 = 0i64;
        for &val in &direct_integers {
            sum1 += val * 2;
        }
        let direct_time = start.elapsed();

        // RefCell访问
        let start = Instant::now();
        let mut sum2 = 0i64;
        for obj in &wrapped_integers {
            if let Ok(val) = obj.weak().with_data(|data| match data {
                OnionObject::Integer(i) => Ok(*i),
                _ => Err(RuntimeError::InvalidType("Not integer".into())),
            }) {
                sum2 += val * 2;
            }
        }
        let refcell_time = start.elapsed();

        println!("直接访问100万个i64: {:.2}s", direct_time.as_secs_f64());
        println!(
            "RefCell访问100万个OnionObject: {:.2}s",
            refcell_time.as_secs_f64()
        );
        println!(
            "RefCell开销倍数: {:.1}x",
            refcell_time.as_secs_f64() / direct_time.as_secs_f64()
        );
        println!("校验: {} vs {}", sum1, sum2);
    }
}
