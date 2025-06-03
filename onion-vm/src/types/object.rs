use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    ptr::addr_eq,
};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::lambda::runnable::RuntimeError;

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
#[derive(Clone)]
pub struct OnionObjectCell(pub RefCell<OnionObject>);

impl OnionObjectCell {
    #[inline(always)]
    pub fn with_data<T, F>(&self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&OnionObject) -> Result<T, RuntimeError>,
    {
        match self.0.try_borrow() {
            Ok(obj) => obj.with_data(f),
            Err(_) => Err(RuntimeError::BrokenReference),
        }
    }
    #[inline(always)]
    pub fn with_data_mut<T, F>(&self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&mut OnionObject) -> Result<T, RuntimeError>,
    {
        match self.0.try_borrow_mut() {
            Ok(mut obj) => obj.with_data_mut(f),
            Err(_) => Err(RuntimeError::BrokenReference),
        }
    }

    #[inline(always)]
    pub fn with_attribute<T, F>(&self, key: &OnionObject, f: &F) -> Result<T, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<T, RuntimeError>,
    {
        match self.0.try_borrow() {
            Ok(obj) => obj.with_attribute(key, f),
            Err(_) => Err(RuntimeError::BrokenReference),
        }
    }

    #[inline(always)]
    pub fn with_attribute_mut<T, F>(&self, key: &OnionObject, f: &F) -> Result<T, RuntimeError>
    where
        F: Fn(&mut OnionObject) -> Result<T, RuntimeError>,
    {
        match self.0.try_borrow_mut() {
            Ok(mut obj) => obj.with_attribute_mut(key, f),
            Err(_) => Err(RuntimeError::BrokenReference),
        }
    }

    #[inline(always)]
    pub fn upgrade(&self) -> Option<Vec<GCArc<OnionObjectCell>>> {
        match self.0.try_borrow() {
            Ok(obj) => obj.upgrade(),
            Err(_) => None,
        }
    }

    #[inline(always)]
    pub fn stabilize(self) -> OnionStaticObject {
        OnionStaticObject::new(self.borrow().clone())
    }

    #[inline(always)]
    pub fn equals(&self, other: &Self) -> Result<bool, RuntimeError> {
        self.with_data(|obj| other.with_data(|other_obj| obj.equals(other_obj)))
    }

    #[inline(always)]
    pub fn try_borrow(&self) -> Result<std::cell::Ref<OnionObject>, RuntimeError> {
        self.0
            .try_borrow()
            .map_err(|_| RuntimeError::BrokenReference)
    }
    #[inline(always)]
    pub fn try_borrow_mut(&self) -> Result<std::cell::RefMut<OnionObject>, RuntimeError> {
        self.0
            .try_borrow_mut()
            .map_err(|_| RuntimeError::BrokenReference)
    }
}

impl std::ops::Deref for OnionObjectCell {
    type Target = RefCell<OnionObject>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for OnionObjectCell {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<RefCell<OnionObject>> for OnionObjectCell {
    fn from(cell: RefCell<OnionObject>) -> Self {
        OnionObjectCell(cell)
    }
}

impl From<OnionObject> for OnionObjectCell {
    fn from(obj: OnionObject) -> Self {
        OnionObjectCell(RefCell::new(obj))
    }
}

impl GCTraceable for OnionObjectCell {
    fn visit(&self) {
        if let Ok(obj) = self.0.try_borrow() {
            obj.visit();
        }
    }
}

impl Debug for OnionObjectCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0.borrow())
    }
}

impl Display for OnionObjectCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0.borrow())
    }
}

#[derive(Clone)]
pub enum OnionObject {
    // immutable basic types
    Integer(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    Boolean(bool),
    Range(i64, i64),
    Null,
    Undefined(Option<String>),
    Tuple(OnionTuple),
    Pair(OnionPair),
    Named(OnionNamed),
    LazySet(OnionLazySet),
    InstructionPackage(VMInstructionPackage),
    Lambda(OnionLambdaDefinition),

    // mutable types, DO NOT USE THIS TYPE DIRECTLY, use `mutablize` instead
    Mut(GCArcWeak<OnionObjectCell>),
}

impl Debug for OnionObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OnionObject::Integer(i) => write!(f, "Integer({})", i),
            OnionObject::Float(fl) => write!(f, "Float({})", fl),
            OnionObject::String(s) => write!(f, "String({})", s),
            OnionObject::Bytes(b) => write!(f, "Bytes({:?})", b),
            OnionObject::Boolean(b) => write!(f, "Boolean({})", b),
            OnionObject::Range(start, end) => write!(f, "Range({}, {})", start, end),
            OnionObject::Null => write!(f, "Null"),
            OnionObject::Undefined(s) => write!(f, "Undefined({})", s.as_deref().unwrap_or("")),
            OnionObject::Tuple(tuple) => write!(f, "Tuple"),
            OnionObject::Pair(pair) => write!(f, "{:?}", pair),
            OnionObject::Named(named) => write!(f, "{:?}", named),
            OnionObject::LazySet(lazy_set) => write!(f, "{:?}", lazy_set),
            OnionObject::InstructionPackage(_) => write!(f, "InstructionPackage(...)"),
            OnionObject::Lambda(lambda) => write!(f, "{:?}", lambda),
            OnionObject::Mut(r) => write!(
                f,
                "Mut({})",
                match r.upgrade() {
                    Some(strong) => {
                        format!("{:?}", strong.as_ref())
                    }
                    None => "Broken Reference".to_string(),
                }
            ),
        }
    }
}

// impl Drop for OnionObject {
//     fn drop(&mut self) {
//         match self {
//             OnionObject::Integer(_) => {
//                 println!("Dropping Integer");
//             }
//             OnionObject::Float(_) => {
//                 println!("Dropping Float");
//             }
//             OnionObject::String(_) => {
//                 println!("Dropping String");
//             }
//             OnionObject::Bytes(_) => {
//                 println!("Dropping Bytes");
//             }
//             OnionObject::Boolean(_) => {
//                 println!("Dropping Boolean");
//             }
//             OnionObject::Range(_, _) => {
//                 println!("Dropping Range");
//             }
//             OnionObject::Null => {
//                 println!("Dropping Null");
//             }
//             OnionObject::Undefined(_) => {
//                 println!("Dropping Undefined");
//             }
//             OnionObject::Tuple(tuple) => {
//                 println!("Dropping Tuple with {} elements", tuple.elements.len());
//             }
//             OnionObject::Pair(_) => {
//                 println!("Dropping Pair");
//             }
//             OnionObject::Named(_) => {
//                 println!("Dropping Named");
//             }
//             OnionObject::LazySet(_) => {
//                 println!("Dropping LazySet");
//             }
//             OnionObject::InstructionPackage(_) => {
//                 println!("Dropping InstructionPackage");
//             }
//             OnionObject::Lambda(lambda) => {
//                 println!("Dropping Lambda with signature: {:?}", lambda.signature);
//             }
//             OnionObject::Mut(weak) => {
//                 if weak.is_valid() {
//                     println!("Dropping Mut with strong reference");
//                 } else {
//                     println!("Dropping Mut with broken reference");
//                 }
//             }            
//         }
//     }
// }

impl GCTraceable for OnionObject {
    fn visit(&self) {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    strong.mark_and_visit();
                }
            }
            OnionObject::Tuple(tuple) => tuple.visit(),
            OnionObject::Pair(pair) => pair.visit(),
            OnionObject::Named(named) => named.visit(),
            OnionObject::LazySet(lazy_set) => lazy_set.visit(),
            OnionObject::Lambda(lambda) => lambda.visit(),

            _ => {}
        }
    }
}
impl OnionObject {
    pub fn upgrade(&self) -> Option<Vec<GCArc<OnionObjectCell>>> {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    Some(vec![strong])
                } else {
                    None
                }
            }
            OnionObject::Tuple(tuple) => tuple.upgrade(),
            OnionObject::Pair(pair) => pair.upgrade(),
            OnionObject::Named(named) => named.upgrade(),
            OnionObject::LazySet(lazy_set) => lazy_set.upgrade(),
            OnionObject::Lambda(lambda) => lambda.upgrade(),
            _ => None,
        }
    }

    pub fn to_cell(self) -> OnionObjectCell {
        OnionObjectCell(RefCell::new(self))
    }

    #[inline(always)]
    pub fn stabilize(self) -> OnionStaticObject {
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
            _ => Err(RuntimeError::InvalidOperation(format!(
                "len() not supported for {:?}",
                self
            ))),
        })
    }

    pub fn contains(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Tuple(tuple), _) => tuple.contains(other_obj),
                (OnionObject::String(s), OnionObject::String(other_s)) => Ok(s.contains(other_s)),
                (OnionObject::Bytes(b), OnionObject::Bytes(other_b)) => {
                    Ok(b.windows(other_b.len()).any(|window| window == other_b))
                }
                (OnionObject::Range(l, r), OnionObject::Integer(i)) => Ok(*i >= *l && *i < *r),
                (OnionObject::Range(start, end), OnionObject::Float(f)) => {
                    Ok(*f >= *start as f64 && *f < *end as f64)
                }
                (OnionObject::Range(start, end), OnionObject::Range(other_start, other_end)) => {
                    Ok(*other_start >= *start && *other_end <= *end)
                }
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "contains() not supported for {:?}",
                    obj
                ))),
            })
        })
    }

    /// Warning: This method can cause stack overflow if there are nested Mut references.
    ///
    /// TODO: Add recursion depth limit after VM core is complete.
    pub fn clone_value(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| Ok(obj.clone().stabilize()))
    }

    /// Warning: This method can cause stack overflow if there are nested Mut references.
    ///
    /// TODO: Add recursion depth limit after VM core is complete.
    pub fn with_data<T, F>(&self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&OnionObject) -> Result<T, RuntimeError>,
    {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    // FIXME: Potential infinite recursion if Mut contains another Mut
                    strong.as_ref().with_data(f)
                } else {
                    Err(RuntimeError::BrokenReference)
                }
            }
            _ => f(self),
        }
    }

    /// Warning: This method can cause stack overflow if there are nested Mut references.
    ///
    /// TODO: Add recursion depth limit after VM core is complete.
    pub fn with_data_mut<T, F>(&mut self, f: F) -> Result<T, RuntimeError>
    where
        F: FnOnce(&mut OnionObject) -> Result<T, RuntimeError>,
    {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    // FIXME: Potential infinite recursion if Mut contains another Mut
                    strong.as_ref().with_data_mut(f)
                } else {
                    Err(RuntimeError::BrokenReference)
                }
            }
            _ => f(self),
        }
    }
    /// Assign a new value to a mutable object.
    pub fn assign(&self, other: &OnionObject) -> Result<(), RuntimeError> {
        // 由于我们无法保证赋值后GCArcWeak指向对象的稳定性，对不可变对象进行赋值操作会导致潜在的内存安全问题。
        // Mut类型由于是被GC管理的，因此可以安全地进行赋值操作（前提是GCArcWeak指向的对象仍然存在）。
        let OnionObject::Mut(weak) = self else {
            return Err(RuntimeError::InvalidOperation(format!(
                "Cannot assign to non-mutable object: {:?}",
                self
            )));
        };
        match weak.upgrade() {
            Some(strong) => strong.as_ref().with_data_mut(|obj| {
                other.with_data(|other| {
                    *obj = other.clone();
                    Ok(())
                })
            }),
            None => Err(RuntimeError::BrokenReference),
        }
    }

    /// Replace the value of a mutable object with another mutable object.
    pub fn replace_mut(&mut self, other: &OnionObject) -> Result<(), RuntimeError> {
        match (self, other) {
            (OnionObject::Mut(to), OnionObject::Mut(from)) => {
                *to = from.clone();
                Ok(())
            }
            _ => Err(RuntimeError::InvalidOperation(format!(
                "Invalid replace_mut() operation"
            ))),
        }
    }

    pub fn to_integer(&self) -> Result<i64, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(*i),
            OnionObject::Float(f) => Ok(*f as i64),
            OnionObject::String(s) => s
                .parse::<i64>()
                .map_err(|e| RuntimeError::InvalidType(e.to_string())),
            OnionObject::Boolean(b) => Ok(if *b { 1 } else { 0 }),
            _ => Err(RuntimeError::InvalidType(format!(
                "Cannot convert {:?} to Integer",
                obj
            ))),
        })
    }

    pub fn to_float(&self) -> Result<f64, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(*i as f64),
            OnionObject::Float(f) => Ok(*f),
            OnionObject::String(s) => s
                .parse::<f64>()
                .map_err(|e| RuntimeError::InvalidType(e.to_string())),
            OnionObject::Boolean(b) => Ok(if *b { 1.0 } else { 0.0 }),
            _ => Err(RuntimeError::InvalidType(format!(
                "Cannot convert {:?} to Float",
                obj
            ))),
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
                OnionObject::String(s) => Ok(s.clone()),
                OnionObject::Bytes(b) => Ok(String::from_utf8_lossy(b).to_string()),
                OnionObject::Boolean(b) => Ok(if *b {
                    "true".to_string()
                } else {
                    "false".to_string()
                }),
                OnionObject::Null => Ok("null".to_string()),
                OnionObject::Undefined(s) => Ok(match s {
                    Some(s) => format!("undefined({})", s),
                    None => "undefined".to_string(),
                }),
                OnionObject::Range(start, end) => Ok(format!("{}..{}", start, end)),
                OnionObject::Tuple(tuple) => match tuple.elements.len() {
                    0 => Ok("()".to_string()),
                    1 => {
                        let first = tuple.elements.first().unwrap();
                        Ok(format!(
                            "({},)",
                            first
                                .try_borrow()
                                .map_err(|_| RuntimeError::BrokenReference)?
                                .to_string(&new_ptrs)?
                        ))
                    }
                    _ => {
                        let elements: Result<Vec<String>, RuntimeError> = tuple
                            .elements
                            .iter()
                            .map(|e| {
                                e.try_borrow()
                                    .map_err(|_| RuntimeError::BrokenReference)?
                                    .to_string(&new_ptrs)
                            })
                            .collect();
                        Ok(format!("({})", elements?.join(", ")))
                    }
                },
                OnionObject::Pair(pair) => {
                    let left = pair.key.borrow().to_string(&new_ptrs)?;
                    let right = pair.value.borrow().to_string(&new_ptrs)?;
                    Ok(format!("{} : {}", left, right))
                }
                OnionObject::Named(named) => {
                    let name = named.key.try_borrow()?.to_string(&new_ptrs)?;
                    let value = named.value.try_borrow()?.to_string(&new_ptrs)?;
                    Ok(format!("{} => {}", name, value))
                }
                OnionObject::LazySet(lazy_set) => {
                    let container = lazy_set.container.try_borrow()?.to_string(&new_ptrs)?;
                    let filter = lazy_set.filter.try_borrow()?.to_string(&new_ptrs)?;
                    Ok(format!("[{} | {}]", container, filter))
                }
                OnionObject::InstructionPackage(_) => Ok("InstructionPackage(...)".to_string()),
                OnionObject::Lambda(lambda) => {
                    let params = lambda.parameter.try_borrow()?.to_string(&new_ptrs)?;
                    let body = lambda.body.to_string();
                    Ok(format!("{}::{} -> {}", lambda.signature, params, body))
                }
                _ => {
                    // 使用 Debug trait来处理其他类型的转换
                    Ok(format!("{:?}", obj))
                }
            }
        })
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(i.to_string().into_bytes()),
            OnionObject::Float(f) => Ok(f.to_string().into_bytes()),
            OnionObject::String(s) => Ok(s.as_bytes().to_vec()),
            OnionObject::Bytes(b) => Ok(b.clone()),
            OnionObject::Boolean(b) => Ok(if *b {
                b"true".to_vec()
            } else {
                b"false".to_vec()
            }),
            _ => Err(RuntimeError::InvalidType(format!(
                "Cannot convert {:?} to Bytes",
                obj
            ))),
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
            _ => Err(RuntimeError::InvalidType(format!(
                "Cannot convert {:?} to Boolean",
                obj
            ))),
        })
    }
    pub fn mutablize(self, gc: &mut GC<OnionObjectCell>) -> OnionStaticObject {
        let arc = gc.create(OnionObjectCell::from(self));
        OnionStaticObject {
            obj: OnionObject::Mut(arc.as_weak()).to_cell(),
            arcs: GCArcStorage::from_option_vec(Some(vec![arc])),
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
                    OnionObject::String(format!("{}{}", s1, s2)),
                )),
                (OnionObject::Bytes(b1), OnionObject::Bytes(b2)) => {
                    let mut new_bytes = b1.clone();
                    new_bytes.extend_from_slice(b2);
                    Ok(OnionStaticObject::new(OnionObject::Bytes(new_bytes)))
                }
                (OnionObject::Range(start1, end1), OnionObject::Range(start2, end2)) => Ok(
                    OnionStaticObject::new(OnionObject::Range(start1 + start2, end1 + end2)),
                ),
                (OnionObject::Tuple(t1), _) => t1.binary_add(other_obj),
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary add operation for {:?} and {:?}",
                    obj, other_obj
                ))),
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
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary sub operation for {:?} and {:?}",
                    obj, other_obj
                ))),
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
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary mul operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_div(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    if *i2 == 0 {
                        return Err(RuntimeError::InvalidOperation(
                            "Division by zero".to_string(),
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
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary div operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_mod(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    if *i2 == 0 {
                        return Err(RuntimeError::InvalidOperation(
                            "Division by zero".to_string(),
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
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary mod operation for {:?} and {:?}",
                    obj, other_obj
                ))),
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
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary pow operation for {:?} and {:?}",
                    obj, other_obj
                ))),
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
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary and operation for {:?} and {:?}",
                    obj, other_obj
                ))),
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
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary or operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_xor(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 ^ i2)))
                }
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary xor operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_shl(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 << i2)))
                }
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary shl operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_shr(&self, other: &Self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 >> i2)))
                }
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary shr operation for {:?} and {:?}",
                    obj, other_obj
                ))),
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
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary lt operation for {:?} and {:?}",
                    obj, other_obj
                ))),
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
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Invalid binary gt operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn unary_neg(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(OnionStaticObject::new(OnionObject::Integer(-i))),
            OnionObject::Float(f) => Ok(OnionStaticObject::new(OnionObject::Float(-f))),
            _ => Err(RuntimeError::InvalidOperation(format!(
                "Invalid unary neg operation for {:?}",
                obj
            ))),
        })
    }

    pub fn unary_plus(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(OnionStaticObject::new(OnionObject::Integer(i.abs()))),
            OnionObject::Float(f) => Ok(OnionStaticObject::new(OnionObject::Float(f.abs()))),
            _ => Err(RuntimeError::InvalidOperation(format!(
                "Invalid unary plus operation for {:?}",
                obj
            ))),
        })
    }

    pub fn unary_not(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Boolean(b) => Ok(OnionStaticObject::new(OnionObject::Boolean(!b))),
            OnionObject::Integer(i) => Ok(OnionStaticObject::new(OnionObject::Integer(!i))),
            _ => Err(RuntimeError::InvalidOperation(format!(
                "Invalid unary not operation for {:?}",
                obj
            ))),
        })
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        self.with_data(|obj| match obj {
            OnionObject::Tuple(tuple) => tuple.with_attribute(key, f),
            OnionObject::Named(named) => named.with_attribute(key, f),
            OnionObject::Pair(pair) => pair.with_attribute(key, f),
            OnionObject::Lambda(lambda) => lambda.with_attribute(key, f),
            OnionObject::LazySet(lazy_set) => lazy_set.with_attribute(key, f),
            _ => Err(RuntimeError::InvalidOperation(format!(
                "with_attribute() not supported for {:?}",
                self
            ))),
        })
    }

    pub fn with_attribute_mut<F, R>(&mut self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&mut OnionObject) -> Result<R, RuntimeError>,
    {
        self.with_data_mut(|obj| match obj {
            OnionObject::Tuple(tuple) => tuple.with_attribute_mut(key, f),
            OnionObject::Named(named) => named.with_attribute_mut(key, f),
            OnionObject::Pair(pair) => pair.with_attribute_mut(key, f),
            OnionObject::Lambda(lambda) => lambda.with_attribute_mut(key, f),
            OnionObject::LazySet(lazy_set) => lazy_set.with_attribute_mut(key, f),
            _ => Err(RuntimeError::InvalidOperation(format!(
                "with_attribute_mut() not supported for {:?}",
                obj
            ))),
        })
    }

    pub fn at(&self, index: i64) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Tuple(tuple) => tuple.at(index),
            OnionObject::String(s) => {
                if index < 0 || index >= s.len() as i64 {
                    return Err(RuntimeError::InvalidOperation(format!(
                        "Index out of bounds for String: {}",
                        s
                    )));
                }
                Ok(OnionStaticObject::new(OnionObject::String(
                    s.chars().nth(index as usize).unwrap().to_string(),
                )))
            }
            OnionObject::Bytes(b) => {
                if index < 0 || index >= b.len() as i64 {
                    return Err(RuntimeError::InvalidOperation(format!(
                        "Index out of bounds for Bytes: {:?}",
                        b
                    )));
                }
                Ok(OnionStaticObject::new(OnionObject::Bytes(vec![
                    b[index as usize],
                ])))
            }
            _ => Err(RuntimeError::InvalidOperation(format!(
                "index_of() not supported for {:?}",
                self
            ))),
        })
    }

    pub fn key_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Named(named) => Ok(named.get_key().clone().stabilize()),
            OnionObject::Pair(pair) => Ok(pair.get_key().clone().stabilize()),
            _ => Err(RuntimeError::InvalidOperation(format!(
                "key_of() not supported for {:?}",
                obj
            ))),
        })
    }

    pub fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| match obj {
            OnionObject::Named(named) => Ok(named.get_value().clone().stabilize()),
            OnionObject::Pair(pair) => Ok(pair.get_value().clone().stabilize()),
            OnionObject::Undefined(s) => {
                Ok(OnionStaticObject::new(OnionObject::Undefined(s.clone())))
            }
            _ => Err(RuntimeError::InvalidOperation(format!(
                "value_of() not supported for {:?}",
                obj
            ))),
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
            _ => Err(RuntimeError::InvalidOperation(format!(
                "type_of() not supported for {:?}",
                obj
            ))),
        })
    }

    #[inline(always)]
    pub fn copy(&self) -> Result<OnionStaticObject, RuntimeError> {
        self.with_data(|obj| Ok(obj.clone().stabilize()))
    }
}

#[derive(Clone)]
pub enum GCArcStorage {
    None,
    Single(GCArc<OnionObjectCell>),
    Multiple(Vec<GCArc<OnionObjectCell>>),
}

impl Debug for GCArcStorage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "GCArcStorage::None"),
            Self::Single(_) => write!(f, "GCArcStorage::Single(<arc>)"),
            Self::Multiple(v) => write!(f, "GCArcStorage::Multiple({} arcs)", v.len()),
        }
    }
}

impl GCArcStorage {
    pub fn from_option_vec(vec: Option<Vec<GCArc<OnionObjectCell>>>) -> Self {
        match vec {
            None => Self::None,
            Some(v) => match v.len() {
                0 => Self::None,
                1 => Self::Single(v.into_iter().next().unwrap()),
                _ => Self::Multiple(v),
            },
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn len(&self) -> usize {
        match self {
            Self::None => 0,
            Self::Single(_) => 1,
            Self::Multiple(v) => v.len(),
        }
    }
}

#[derive(Clone)]
pub struct OnionStaticObject {
    obj: OnionObjectCell,
    #[allow(dead_code)]
    arcs: GCArcStorage,
}

impl Default for OnionStaticObject {
    fn default() -> Self {
        OnionStaticObject {
            obj: OnionObject::Undefined(None).to_cell(),
            arcs: GCArcStorage::None,
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
            | OnionObject::Range(_, _) => GCArcStorage::None,
            _ => GCArcStorage::from_option_vec(obj.upgrade()),
        };
        OnionStaticObject {
            obj: obj.to_cell(),
            arcs,
        }
    }

    #[inline(always)]
    pub fn weak(&self) -> &OnionObjectCell {
        &self.obj
    }

    #[inline(always)]
    pub fn mutablize(
        &self,
        gc: &mut GC<OnionObjectCell>,
    ) -> Result<OnionStaticObject, RuntimeError> {
        self.obj.with_data(|obj| Ok(obj.clone().mutablize(gc)))
    }
}

#[macro_export]
macro_rules! unwrap_object {
    ($obj:expr, $variant:path) => {
        match $obj {
            $variant(o) => Ok(o),
            _ => Err(RuntimeError::InvalidType(format!(
                "Expected {}, found {:?}",
                stringify!($variant),
                $obj
            ))),
        }
    };
}
