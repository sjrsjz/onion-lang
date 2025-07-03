use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
    ptr::addr_eq,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};
use base64::{engine::general_purpose, Engine as _};

use crate::{
    lambda::runnable::RuntimeError,
    onion_tuple,
    types::lambda::native::{
        native_bool_converter, native_bytes_converter, native_elements_method,
        native_float_converter, native_int_converter, native_length_method,
        native_string_converter, wrap_native_function,
    },
};

use super::{
    lambda::{
        definition::OnionLambdaDefinition, vm_instructions::instruction_set::VMInstructionPackage,
    },
    lazy_set::OnionLazySet,
    named::OnionNamed,
    pair::OnionPair,
    tuple::OnionTuple,
};

// Newtype wrapper to allow implementing GCTraceable for RefCell<OnionObject>
pub struct OnionObjectCell(pub RwLock<OnionObject>);

impl OnionObjectCell {
    #[inline(always)]
    /// 严格遵循幂等律的情况下，Cell里不可能出现Mut对象。
    /// 如果出现了Mut对象，说明VM对象分配或GC逻辑有bug
    pub fn with_data<T, F>(&self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&OnionObject) -> Result<T, RuntimeError>,
    {
        match self.0.read() {
            Ok(guard) => match &*guard {
                OnionObject::Mut(_) => {
                    panic!("CRITICAL: OnionObjectCell contains Mut object. This indicates a bug in VM object allocation or GC logic. Check mutablize() and object creation paths.")
                }
                obj => f(obj),
            },
            Err(_) => Err(RuntimeError::BorrowError(
                "Failed to borrow OnionObjectCell at `with_data`"
                    .to_string()
                    .into(),
            )),
        }
    }
    #[inline(always)]
    /// 严格遵循幂等律的情况下，Cell里不可能出现Mut对象。
    /// 如果出现了Mut对象，说明VM对象分配或GC逻辑有bug
    pub fn with_data_mut<T, F>(&self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&mut OnionObject) -> Result<T, RuntimeError>,
    {
        match self.0.write() {
            Ok(mut guard) => match &mut *guard {
                OnionObject::Mut(_) => {
                    panic!("CRITICAL: OnionObjectCell contains Mut object. This indicates a bug in VM object allocation or GC logic. Check mutablize() and object creation paths.")
                }
                obj => f(obj),
            },
            Err(_) => Err(RuntimeError::BorrowError(
                "Failed to borrow OnionObjectCell at `with_data_mut`"
                    .to_string()
                    .into(),
            )),
        }
    }

    #[inline(always)]
    pub fn with_attribute<T, F>(&self, key: &OnionObject, f: &F) -> Result<T, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<T, RuntimeError>,
    {
        self.0
            .read()
            .map_err(|_| {
                RuntimeError::BorrowError(
                    "Failed to borrow OnionObjectCell at `with_attribute`"
                        .to_string()
                        .into(),
                )
            })?
            .with_attribute(key, f)
    }

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
                "Failed to borrow OnionObjectCell at `try_borrow`"
                    .to_string()
                    .into(),
            )
        })
    }
    #[inline(always)]
    pub fn try_borrow_mut(&self) -> Result<RwLockWriteGuard<OnionObject>, RuntimeError> {
        self.0.write().map_err(|_| {
            RuntimeError::BorrowError(
                "Failed to borrow OnionObjectCell at `try_borrow_mut`"
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
/// OnionObject is the main type for all objects in the Onion VM.
/// The VM's types are all immutable(Mut is a immutable pointer to a VM object)
/// If we need to `mutate` an object, it is IMPOSSIBLE to mutate the object itself, we can only let the Mut object point to a `reconstructed` object.
/// `reconstruct_container` is used to reconstruct the container object, which is used to regenerate all `Arc` references to the object.
pub enum OnionObject {
    // immutable basic types
    Integer(i64),
    Float(f64),
    String(Arc<String>),
    Bytes(Arc<Vec<u8>>),
    Boolean(bool),
    Range(i64, i64),
    Null,
    Undefined(Option<Arc<String>>),
    InstructionPackage(Arc<VMInstructionPackage>),

    // immutable container types, need `reconstruct_container` for clone to solve circular references
    Tuple(Arc<OnionTuple>),
    Pair(Arc<OnionPair>),
    Named(Arc<OnionNamed>),
    LazySet(Arc<OnionLazySet>),
    Lambda(Arc<OnionLambdaDefinition>),

    Custom(Arc<dyn OnionObjectExt>),
    // mutable? types, DO NOT USE THIS TYPE DIRECTLY, use `mutablize` instead
    Mut(GCArcWeak<OnionObjectCell>),
}

pub trait OnionObjectExt: GCTraceable<OnionObjectCell> + Debug + Send + Sync + 'static {
    // Type introspection for downcasting
    fn as_any(&self) -> &dyn std::any::Any;

    // GC and memory management
    fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>);

    fn reconstruct_container(&self) -> Result<OnionObject, RuntimeError>;

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
    fn to_bytes(&self) -> Result<Vec<u8>, RuntimeError> {
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
    fn at(&self, index: i64) -> Result<OnionStaticObject, RuntimeError> {
        Err(RuntimeError::InvalidOperation(
            format!("at() not supported for {:?} with index {}", self, index).into(),
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
            OnionObject::Named(named) => named.collect(queue),
            OnionObject::LazySet(lazy_set) => lazy_set.collect(queue),
            OnionObject::Lambda(lambda) => lambda.collect(queue),
            OnionObject::Custom(custom) => custom.collect(queue),

            _ => {}
        }
    }
}
impl OnionObject {
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    collected.push(strong);
                }
            }
            OnionObject::Tuple(tuple) => tuple.upgrade(collected),
            OnionObject::Pair(pair) => pair.upgrade(collected),
            OnionObject::Named(named) => named.upgrade(collected),
            OnionObject::LazySet(lazy_set) => lazy_set.upgrade(collected),
            OnionObject::Lambda(lambda) => lambda.upgrade(collected),
            OnionObject::Custom(custom) => custom.upgrade(collected),
            _ => {}
        }
    }

    #[inline(always)]
    pub fn to_cell(self) -> OnionObjectCell {
        OnionObjectCell(RwLock::new(self))
    }

    #[inline(always)]
    pub fn stabilize(&self) -> OnionStaticObject {
        OnionStaticObject::new(self.clone())
    }

    #[inline(always)]
    pub fn consume_and_stabilize(self) -> OnionStaticObject {
        OnionStaticObject::new(self)
    }
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
                    .any(|window| window == other_b.as_slice())),
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

    #[inline(always)]
    /// Assign a new value to a mutable object.
    pub fn assign(&self, other: &OnionObject) -> Result<(), RuntimeError> {
        // 由于我们无法保证赋值后GCArcWeak指向对象的稳定性，对不可变对象进行赋值操作会导致潜在的内存安全问题。
        // Mut类型由于是被GC管理的，因此可以安全地进行赋值操作（前提是GCArcWeak指向的对象仍然存在）。
        let OnionObject::Mut(weak) = self else {
            return Err(RuntimeError::InvalidOperation(
                format!("Cannot assign to non-mutable object: {:?}", self).into(),
            ));
        };
        match weak.upgrade() {
            Some(strong) => {
                // 先克隆要赋值的内容，避免借用冲突
                let new_value = other.with_data(|other| other.reconstruct_container())?;

                // 然后进行赋值
                strong.as_ref().with_data_mut(|obj| {
                    *obj = new_value;
                    Ok(())
                })
            }
            None => Err(RuntimeError::BrokenReference),
        }
    }

    /// 重建容器对象，主要用于 mutable_obj1 = obj2 这种赋值
    pub fn reconstruct_container(&self) -> Result<OnionObject, RuntimeError> {
        match self {
            OnionObject::Tuple(tuple) => tuple.reconstruct_container(),
            OnionObject::Pair(pair) => pair.reconstruct_container(),
            OnionObject::Named(named) => named.reconstruct_container(),
            OnionObject::LazySet(lazy_set) => lazy_set.reconstruct_container(),
            OnionObject::Lambda(lambda) => lambda.reconstruct_container(),
            OnionObject::Custom(custom) => custom.reconstruct_container(),
            _ => Ok(self.clone()), // 对于其他类型，直接克隆（由于Mut自身就是不可变地址，因此直接
                                   // 克隆即可，并且只有容器才可能造成循环）
        }
    }
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
                OnionObject::String(s) => Ok(s.as_ref().clone()),
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
                OnionObject::Named(named) => {
                    let name = named.get_key().repr(&new_ptrs)?;
                    let value = named.get_value().repr(&new_ptrs)?;
                    Ok(format!("{} => {}", name, value))
                }
                OnionObject::LazySet(lazy_set) => {
                    let container = lazy_set.get_container().repr(&new_ptrs)?;
                    let filter = lazy_set.get_filter().repr(&new_ptrs)?;
                    Ok(format!("[{} | {}]", container, filter))
                }
                OnionObject::InstructionPackage(_) => Ok("InstructionPackage(...)".to_string()),
                OnionObject::Lambda(lambda) => {
                    let params = lambda.get_parameter().repr(&new_ptrs)?;
                    let body = lambda.get_body().to_string();
                    Ok(format!(
                        "{}::{} -> {}",
                        lambda.get_signature(),
                        params,
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
                OnionObject::Named(named) => {
                    let name = named.get_key().repr(&new_ptrs)?;
                    let value = named.get_value().repr(&new_ptrs)?;
                    Ok(format!("{} => {}", name, value))
                }
                OnionObject::LazySet(lazy_set) => {
                    let container = lazy_set.get_container().repr(&new_ptrs)?;
                    let filter = lazy_set.get_filter().repr(&new_ptrs)?;
                    Ok(format!("[{} | {}]", container, filter))
                }
                OnionObject::InstructionPackage(_) => Ok("InstructionPackage(...)".to_string()),
                OnionObject::Lambda(lambda) => {
                    let params = lambda.get_parameter().repr(&new_ptrs)?;
                    Ok(format!(
                        "{}::{} -> {}",
                        lambda.get_signature(),
                        params,
                        lambda.get_body()
                    ))
                }
                OnionObject::Mut(weak) => {
                    if let Some(strong) = weak.upgrade() {
                        let inner_repr = strong
                            .as_ref()
                            .try_borrow()
                            .map_err(|_| {
                                RuntimeError::BorrowError(
                                    "Failed to borrow Mut object at `repr`".to_string().into(),
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
    pub fn to_bytes(&self) -> Result<Vec<u8>, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(i.to_string().into_bytes()),
            OnionObject::Float(f) => Ok(f.to_string().into_bytes()),
            OnionObject::String(s) => Ok(s.as_bytes().to_vec()),
            OnionObject::Bytes(b) => Ok(b.as_ref().clone()),
            OnionObject::Boolean(b) => Ok(if *b {
                b"true".to_vec()
            } else {
                b"false".to_vec()
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

    #[inline(always)]
    fn mutablize(self, gc: &mut GC<OnionObjectCell>) -> OnionStaticObject {
        let arc = gc.create(OnionObjectCell::from(self));
        OnionStaticObject {
            obj: OnionObject::Mut(arc.as_weak()),
            _arcs: GCArcStorage::Single(arc),
        }
    }
}

impl OnionObject {
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
                    (OnionObject::Named(n1), _) => n1.equals(other),
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
                    OnionObject::String(Arc::new(format!("{}{}", s1, s2))),
                )),
                (OnionObject::Bytes(b1), OnionObject::Bytes(b2)) => {
                    let mut new_bytes = b1.as_ref().clone();
                    new_bytes.extend_from_slice(b2);
                    Ok(OnionStaticObject::new(OnionObject::Bytes(Arc::new(
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
                        return Err(RuntimeError::InvalidOperation(
                            "Division by zero".to_string().into(),
                        ));
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
                        return Err(RuntimeError::InvalidOperation(
                            "Division by zero".to_string().into(),
                        ));
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
                    match key_str.as_str() {
                        "int" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::int".to_string(),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::float".to_string(),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::string".to_string(),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bool".to_string(),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bytes".to_string(),
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
                    match key_str.as_str() {
                        "int" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::int".to_string(),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::float".to_string(),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::string".to_string(),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bool".to_string(),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bytes".to_string(),
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
                    match key_str.as_str() {
                        "int" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::int".to_string(),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::float".to_string(),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::string".to_string(),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bool".to_string(),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bytes".to_string(),
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
                    match key_str.as_str() {
                        "int" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::int".to_string(),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::float".to_string(),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::string".to_string(),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bool".to_string(),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bytes".to_string(),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        "length" => {
                            let length_method = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "builtin::length".to_string(),
                                &native_length_method,
                            );
                            return f(length_method.weak());
                        }
                        "elements" => {
                            let elements_method = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "builtin::elements".to_string(),
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
            OnionObject::Named(named) => named.with_attribute(key, f),
            OnionObject::Pair(pair) => pair.with_attribute(key, f),
            OnionObject::Lambda(lambda) => lambda.with_attribute(key, f),
            OnionObject::LazySet(lazy_set) => lazy_set.with_attribute(key, f),
            OnionObject::String(_) => {
                if let OnionObject::String(key_str) = key {
                    match key_str.as_str() {
                        "int" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::int".to_string(),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::float".to_string(),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::string".to_string(),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bool".to_string(),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bytes".to_string(),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        "length" => {
                            let length_method = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "builtin::length".to_string(),
                                &native_length_method,
                            );
                            return f(length_method.weak());
                        }
                        "elements" => {
                            let elements_method = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "builtin::elements".to_string(),
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
                    match key_str.as_str() {
                        "int" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::int".to_string(),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::float".to_string(),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::string".to_string(),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bool".to_string(),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bytes".to_string(),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        "length" => {
                            let length_method = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "builtin::length".to_string(),
                                &native_length_method,
                            );
                            return f(length_method.weak());
                        }
                        "elements" => {
                            let elements_method = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "builtin::elements".to_string(),
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
                    match key_str.as_str() {
                        "int" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::int".to_string(),
                                &native_int_converter,
                            );
                            return f(converter.weak());
                        }
                        "float" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::float".to_string(),
                                &native_float_converter,
                            );
                            return f(converter.weak());
                        }
                        "string" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::string".to_string(),
                                &native_string_converter,
                            );
                            return f(converter.weak());
                        }
                        "bool" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bool".to_string(),
                                &native_bool_converter,
                            );
                            return f(converter.weak());
                        }
                        "bytes" => {
                            let converter = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "converter::bytes".to_string(),
                                &native_bytes_converter,
                            );
                            return f(converter.weak());
                        }
                        "length" => {
                            let length_method = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "builtin::length".to_string(),
                                &native_length_method,
                            );
                            return f(length_method.weak());
                        }
                        "elements" => {
                            let elements_method = wrap_native_function(
                                &onion_tuple!(),
                                None,
                                Some(&self.stabilize()),
                                "builtin::elements".to_string(),
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
                    "Custom with_attribute not called".to_string().into(),
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
    pub fn at(&self, index: i64) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Tuple(tuple) => tuple.at(index),
            OnionObject::String(s) => {
                if index < 0 || index >= s.len() as i64 {
                    return Err(RuntimeError::InvalidOperation(
                        format!("Index out of bounds for String: {}", s).into(),
                    ));
                }
                Ok(OnionStaticObject::new(OnionObject::String(Arc::new(
                    s.chars().nth(index as usize).unwrap().to_string(),
                ))))
            }
            OnionObject::Bytes(b) => {
                if index < 0 || index >= b.len() as i64 {
                    return Err(RuntimeError::InvalidOperation(
                        format!("Index out of bounds for Bytes: {:?}", b).into(),
                    ));
                }
                Ok(OnionStaticObject::new(OnionObject::Bytes(Arc::new(vec![
                    b[index as usize],
                ]))))
            }
            OnionObject::Custom(custom) => custom.at(index),
            _ => Err(RuntimeError::InvalidOperation(
                format!("index_of() not supported for {:?}", self).into(),
            )),
        })
    }

    pub fn key_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Named(named) => Ok(named.get_key().stabilize()),
            OnionObject::Pair(pair) => Ok(pair.get_key().stabilize()),
            OnionObject::Custom(custom) => custom.key_of(),
            _ => Err(RuntimeError::InvalidOperation(
                format!("key_of() not supported for {:?}", obj).into(),
            )),
        })
    }

    pub fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Named(named) => Ok(named.get_value().stabilize()),
            OnionObject::Pair(pair) => Ok(pair.get_value().stabilize()),
            OnionObject::Undefined(s) => Ok(OnionStaticObject::new(OnionObject::String(Arc::new(
                s.as_ref()
                    .map(|o| o.as_ref().clone())
                    .unwrap_or_else(|| "".to_string()),
            )))),
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
            OnionObject::Named(_) => Ok("Named".to_string()),
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
        println!("OnionNamed: {} bytes", std::mem::size_of::<OnionNamed>());
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
                        _ => Err(RuntimeError::InvalidOperation(
                            "Type error".to_string().into(),
                        )),
                    }
                })
            });

            if let Ok(sum) = result {
                // 提取结果值（模拟VM获取计算结果）
                if let Ok(val) = sum.weak().with_data(|data| match data {
                    OnionObject::Integer(v) => Ok(*v),
                    _ => Err(RuntimeError::InvalidType("Not integer".to_string().into())),
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
            let str_obj = OnionObject::String(Arc::new(format!("string_{}", i))).stabilize();

            // 获取字符串长度（模拟len()操作）
            if let Ok(len_obj) = str_obj.weak().with_data(|data| data.len()) {
                if let Ok(length) = len_obj.weak().with_data(|data| data.to_integer()) {
                    total_length += length as usize;
                }
            }

            // 字符串拼接操作
            let suffix = OnionObject::String(Arc::new("_suffix".to_string())).stabilize();
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
                _ => Err(RuntimeError::InvalidType("Not integer".to_string().into())),
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
