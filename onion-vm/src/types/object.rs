use std::{fmt::Debug, ptr::addr_eq};

use arc_gc::{
    arc::{GCArc, GCArcWeak}, gc::GC, traceable::GCTraceable
};

use super::{named::OnionNamed, pair::OnionPair, tuple::OnionTuple};

#[allow(dead_code)]
#[derive(Debug)]
pub enum ObjectError {
    InvalidType,
    InvalidOperation(String),
    BrokenReference,
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
    Undefined(String),
    Tuple(OnionTuple),
    Pair(OnionPair),
    Named(OnionNamed),

    // mutable types, DO NOT USE THIS TYPE DIRECTLY, use `mutablize` instead
    Mut(GCArcWeak),
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
            OnionObject::Undefined(s) => write!(f, "Undefined({})", s),
            OnionObject::Tuple(tuple) => write!(f, "Tuple({:?})", tuple),
            OnionObject::Pair(pair) => write!(f, "Pair({:?})", pair),
            OnionObject::Named(named) => write!(f, "Named({:?})", named),
            OnionObject::Mut(_) => write!(f, "Mut()"),
        }
    }
}

impl GCTraceable for OnionObject {
    fn visit(&self) {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    strong.mark_and_visit();
                }
            }
            _ => {}
        }
    }
}
impl OnionObject {
    pub fn upgrade(&self) -> Option<Vec<GCArc>> {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    Some(vec![strong])
                } else {
                    None
                }
            }
            OnionObject::Tuple(tuple) => tuple.upgrade(),
            _ => None,
        }
    }

    pub fn len(&self) -> Result<OnionStaticObject, ObjectError> {
        match self {
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
            _ => Err(ObjectError::InvalidOperation(format!(
                "len() not supported for {:?}",
                self
            ))),
        }
    }

    /// Warning: This method can cause stack overflow if there are nested Mut references.
    ///
    /// TODO: Add recursion depth limit after VM core is complete.
    pub fn value(&self) -> Result<OnionStaticObject, ObjectError> {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    // FIXME: Potential infinite recursion if Mut contains another Mut
                    strong.downcast::<OnionObject>().value()
                } else {
                    Err(ObjectError::BrokenReference)
                }
            }
            _ => Ok(OnionStaticObject::new(self.clone())),
        }
    }

    /// Warning: This method can cause stack overflow if there are nested Mut references.
    ///
    /// TODO: Add recursion depth limit after VM core is complete.
    pub fn with_data<T, F>(&self, f: F) -> Result<T, ObjectError>
    where
        F: FnOnce(&OnionObject) -> Result<T, ObjectError>,
    {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(strong) = weak.upgrade() {
                    // FIXME: Potential infinite recursion if Mut contains another Mut
                    strong.downcast::<OnionObject>().with_data(f)
                } else {
                    Err(ObjectError::BrokenReference)
                }
            }
            _ => f(self),
        }
    }

    /// Warning: This method can cause stack overflow if there are nested Mut references.
    ///
    /// TODO: Add recursion depth limit after VM core is complete.
    pub fn with_data_mut<T, F>(&mut self, f: F) -> Result<T, ObjectError>
    where
        F: FnOnce(&mut OnionObject) -> Result<T, ObjectError>,
    {
        match self {
            OnionObject::Mut(weak) => {
                if let Some(mut strong) = weak.upgrade() {
                    // FIXME: Potential infinite recursion if Mut contains another Mut
                    strong.downcast_mut::<OnionObject>().with_data_mut(f)
                } else {
                    Err(ObjectError::BrokenReference)
                }
            }
            _ => f(self),
        }
    }
    /// Assign a new value
    pub fn assign(&mut self, other: OnionObject) -> Result<(), ObjectError> {
        self.with_data_mut(|obj| {
            *obj = other;
            Ok(())
        })
    }

    /// Replace the value of a mutable object with another mutable object.
    pub fn replace_mut(&mut self, other: &OnionObject) -> Result<(), ObjectError> {
        match (self, other) {
            (OnionObject::Mut(to), OnionObject::Mut(from)) => {
                *to = from.clone();
                Ok(())
            }
            _ => Err(ObjectError::InvalidOperation(format!(
                "Invalid replace_mut() operation"
            ))),
        }
    }


    pub fn to_integer(&self) -> Result<i64, ObjectError> {
        match self {
            OnionObject::Integer(i) => Ok(*i),
            OnionObject::Float(f) => Ok(*f as i64),
            OnionObject::String(s) => s.parse::<i64>().map_err(|_| ObjectError::InvalidType),
            OnionObject::Boolean(b) => Ok(if *b { 1 } else { 0 }),
            _ => Err(ObjectError::InvalidType),
        }
    }

    pub fn to_float(&self) -> Result<f64, ObjectError> {
        match self {
            OnionObject::Integer(i) => Ok(*i as f64),
            OnionObject::Float(f) => Ok(*f),
            OnionObject::String(s) => s.parse::<f64>().map_err(|_| ObjectError::InvalidType),
            OnionObject::Boolean(b) => Ok(if *b { 1.0 } else { 0.0 }),
            _ => Err(ObjectError::InvalidType),
        }
    }

    pub fn to_string(&self) -> Result<String, ObjectError> {
        match self {
            OnionObject::Integer(i) => Ok(i.to_string()),
            OnionObject::Float(f) => Ok(f.to_string()),
            OnionObject::String(s) => Ok(s.clone()),
            OnionObject::Bytes(b) => Ok(String::from_utf8_lossy(b).to_string()),
            OnionObject::Boolean(b) => Ok(if *b { "true".to_string() } else { "false".to_string() }),
            _ => Err(ObjectError::InvalidType),
        }
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, ObjectError> {
        match self {
            OnionObject::Integer(i) => Ok(i.to_string().into_bytes()),
            OnionObject::Float(f) => Ok(f.to_string().into_bytes()),
            OnionObject::String(s) => Ok(s.clone().into_bytes()),
            OnionObject::Bytes(b) => Ok(b.clone()),
            OnionObject::Boolean(b) => Ok(if *b { vec![1] } else { vec![0] }),
            _ => Err(ObjectError::InvalidType),
        }
    }

    pub fn to_boolean(&self) -> Result<bool, ObjectError> {
        match self {
            OnionObject::Integer(i) => Ok(*i != 0),
            OnionObject::Float(f) => Ok(*f != 0.0),
            OnionObject::String(s) => Ok(!s.is_empty()),
            OnionObject::Bytes(b) => Ok(!b.is_empty()),
            OnionObject::Boolean(b) => Ok(*b),
            _ => Err(ObjectError::InvalidType),
        }
    }

    pub fn heap(self, gc: &mut GC) -> GCArc {
        gc.create(self)
    }

    pub fn mutablize(self, gc: &mut GC) -> OnionStaticObject {
        let arc = self.heap(gc);
        OnionStaticObject {
            obj: OnionObject::Mut(arc.as_weak()),
            arcs: Some(vec![arc]),
        }
    }

}

impl OnionObject {
    pub fn equals(&self, other: &Self) -> Result<bool, ObjectError> {
        match (self.value()?.weak(), other.value()?.weak()) {
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
    }

    pub fn is_same(&self, other: &Self) -> Result<bool, ObjectError> {
        match (self, other) {
            (OnionObject::Mut(weak1), OnionObject::Mut(weak2)) => {
                if let (Some(strong1), Some(strong2)) = (weak1.upgrade(), weak2.upgrade()) {
                    Ok(addr_eq(strong1.raw_ptr(), strong2.raw_ptr()))
                } else {
                    Ok(false)
                }
            }
            _ => self.equals(other),
        }
    }

    pub fn binary_add(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
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
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary add operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_sub(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
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
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary sub operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_mul(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
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
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary mul operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_div(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    if *i2 == 0 {
                        return Err(ObjectError::InvalidOperation(
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
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary div operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_mod(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    if *i2 == 0 {
                        return Err(ObjectError::InvalidOperation(
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
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary mod operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_pow(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
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
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary pow operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_and(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 & i2)))
                }
                (OnionObject::Boolean(f1), OnionObject::Boolean(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Boolean(*f1 && *f2)))
                }
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary and operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_or(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 | i2)))
                }
                (OnionObject::Boolean(f1), OnionObject::Boolean(f2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Boolean(*f1 || *f2)))
                }
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary or operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_xor(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 ^ i2)))
                }
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary xor operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_shl(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 << i2)))
                }
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary shl operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_shr(&self, other: &Self) -> Result<OnionStaticObject, ObjectError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => {
                    Ok(OnionStaticObject::new(OnionObject::Integer(i1 >> i2)))
                }
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary shr operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_eq(&self, other: &Self) -> Result<bool, ObjectError> {
        self.equals(other)
    }

    pub fn binary_lt(&self, other: &Self) -> Result<bool, ObjectError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => Ok(i1 < i2),
                (OnionObject::Float(f1), OnionObject::Float(f2)) => Ok(f1 < f2),
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => Ok((*i1 as f64) < *f2),
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => Ok(*f1 < *i2 as f64),
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary lt operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn binary_gt(&self, other: &Self) -> Result<bool, ObjectError> {
        self.with_data(|obj| {
            other.with_data(|other_obj| match (obj, other_obj) {
                (OnionObject::Integer(i1), OnionObject::Integer(i2)) => Ok(i1 > i2),
                (OnionObject::Float(f1), OnionObject::Float(f2)) => Ok(f1 > f2),
                (OnionObject::Integer(i1), OnionObject::Float(f2)) => Ok((*i1 as f64) > *f2),
                (OnionObject::Float(f1), OnionObject::Integer(i2)) => Ok(*f1 > *i2 as f64),
                _ => Err(ObjectError::InvalidOperation(format!(
                    "Invalid binary gt operation for {:?} and {:?}",
                    obj, other_obj
                ))),
            })
        })
    }

    pub fn unary_neg(&self) -> Result<OnionStaticObject, ObjectError> {
        self.with_data(|obj| match obj {
            OnionObject::Integer(i) => Ok(OnionStaticObject::new(OnionObject::Integer(-i))),
            OnionObject::Float(f) => Ok(OnionStaticObject::new(OnionObject::Float(-f))),
            _ => Err(ObjectError::InvalidOperation(format!(
                "Invalid unary neg operation for {:?}",
                obj
            ))),
        })
    }

    pub fn unary_not(&self) -> Result<OnionStaticObject, ObjectError> {
        self.with_data(|obj| match obj {
            OnionObject::Boolean(b) => Ok(OnionStaticObject::new(OnionObject::Boolean(!b))),
            OnionObject::Integer(i) => Ok(OnionStaticObject::new(OnionObject::Integer(!i))),
            _ => Err(ObjectError::InvalidOperation(format!(
                "Invalid unary not operation for {:?}",
                obj
            ))),
        })
    }

    pub fn get_attribute<'t>(
        &'t self,
        key: &OnionObject,
    ) -> Result<Option<&'t OnionObject>, ObjectError> {
        match self {
            OnionObject::Tuple(tuple) => tuple.get_attribute(key),
            OnionObject::Named(named) => named.get_attribute(key),
            OnionObject::Pair(pair) => pair.get_attribute(key),
            _ => Err(ObjectError::InvalidOperation(format!(
                "get_attribute() not supported for {:?}",
                self
            ))),
        }
    }

    pub fn get_attr(&self, key: &OnionObject) -> Result<OnionStaticObject, ObjectError> {
        match self.get_attribute(key)? {
            Some(value) => Ok(OnionStaticObject::new(value.clone())),
            None => Ok(OnionStaticObject::new(OnionObject::Undefined(format!(
                "Attribute not found: {:?}",
                key
            )))),
        }
    }

    pub fn index_of(&self, index: i64) -> Result<OnionStaticObject, ObjectError> {
        match self {
            OnionObject::Tuple(tuple) => tuple.at(index),
            OnionObject::String(s) => {
                if index < 0 || index >= s.len() as i64 {
                    return Err(ObjectError::InvalidOperation(format!(
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
                    return Err(ObjectError::InvalidOperation(format!(
                        "Index out of bounds for Bytes: {:?}",
                        b
                    )));
                }
                Ok(OnionStaticObject::new(OnionObject::Bytes(vec![
                    b[index as usize],
                ])))
            }
            _ => Err(ObjectError::InvalidOperation(format!(
                "index_of() not supported for {:?}",
                self
            ))),
        }
    }
}

#[derive(Clone)]
pub struct OnionStaticObject {
    pub obj: OnionObject,
    pub arcs: Option<Vec<GCArc>>,
}

impl OnionStaticObject {
    pub fn new(obj: OnionObject) -> Self {
        let arcs = obj.upgrade();
        OnionStaticObject { obj, arcs }
    }

    pub fn weak(&self) -> &OnionObject {
        &self.obj
    }

    pub fn weak_mut(&mut self) -> &mut OnionObject {
        &mut self.obj
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use arc_gc::gc::GC;

    #[test]
    fn test_basic_types_creation() {
        let int_obj = OnionObject::Integer(42);
        let float_obj = OnionObject::Float(3.14);
        let string_obj = OnionObject::String("hello".to_string());
        let bool_obj = OnionObject::Boolean(true);
        let null_obj = OnionObject::Null;
        
        assert!(matches!(int_obj, OnionObject::Integer(42)));
        assert!(matches!(float_obj, OnionObject::Float(f) if (f - 3.14).abs() < f64::EPSILON));
        assert!(matches!(string_obj, OnionObject::String(s) if s == "hello"));
        assert!(matches!(bool_obj, OnionObject::Boolean(true)));
        assert!(matches!(null_obj, OnionObject::Null));
    }

    #[test]
    fn test_type_conversions() {
        let int_obj = OnionObject::Integer(42);
        let float_obj = OnionObject::Float(3.14);
        let string_obj = OnionObject::String("123".to_string());
        let bool_obj = OnionObject::Boolean(true);

        // Test to_integer
        assert_eq!(int_obj.to_integer().unwrap(), 42);
        assert_eq!(float_obj.to_integer().unwrap(), 3);
        assert_eq!(string_obj.to_integer().unwrap(), 123);
        assert_eq!(bool_obj.to_integer().unwrap(), 1);

        // Test to_float
        assert_eq!(int_obj.to_float().unwrap(), 42.0);
        assert_eq!(float_obj.to_float().unwrap(), 3.14);
        assert_eq!(bool_obj.to_float().unwrap(), 1.0);

        // Test to_string
        assert_eq!(int_obj.to_string().unwrap(), "42");
        assert_eq!(float_obj.to_string().unwrap(), "3.14");
        assert_eq!(bool_obj.to_string().unwrap(), "true");

        // Test to_boolean
        assert_eq!(int_obj.to_boolean().unwrap(), true);
        assert_eq!(OnionObject::Integer(0).to_boolean().unwrap(), false);
        assert_eq!(float_obj.to_boolean().unwrap(), true);
        assert_eq!(OnionObject::Float(0.0).to_boolean().unwrap(), false);
        assert_eq!(bool_obj.to_boolean().unwrap(), true);
    }

    #[test]
    fn test_binary_arithmetic_operations() {
        let int1 = OnionObject::Integer(10);
        let int2 = OnionObject::Integer(5);
        let float1 = OnionObject::Float(10.5);
        let float2 = OnionObject::Float(2.5);

        // Addition
        let result = int1.binary_add(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(15)));

        let result = float1.binary_add(&float2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Float(f) if (*f - 13.0).abs() < f64::EPSILON));

        let result = int1.binary_add(&float2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Float(f) if (*f - 12.5).abs() < f64::EPSILON));

        // Subtraction
        let result = int1.binary_sub(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(5)));

        // Multiplication
        let result = int1.binary_mul(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(50)));

        // Division
        let result = int1.binary_div(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(2)));

        // Division by zero
        let zero = OnionObject::Integer(0);
        assert!(int1.binary_div(&zero).is_err());

        // Modulo
        let result = int1.binary_mod(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(0)));

        // Power
        let result = int2.binary_pow(&OnionObject::Integer(2)).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(25)));
    }

    #[test]
    fn test_string_operations() {
        let str1 = OnionObject::String("Hello".to_string());
        let str2 = OnionObject::String(" World".to_string());

        // String concatenation
        let result = str1.binary_add(&str2).unwrap();
        assert!(matches!(result.weak(), OnionObject::String(s) if s == "Hello World"));

        // String length
        let result = str1.len().unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(5)));

        // String indexing
        let result = str1.index_of(1).unwrap();
        assert!(matches!(result.weak(), OnionObject::String(s) if s == "e"));

        // Out of bounds
        assert!(str1.index_of(10).is_err());
    }

    #[test]
    fn test_bytes_operations() {
        let bytes1 = OnionObject::Bytes(vec![1, 2, 3]);
        let bytes2 = OnionObject::Bytes(vec![4, 5]);

        // Bytes concatenation
        let result = bytes1.binary_add(&bytes2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Bytes(b) if *b == vec![1, 2, 3, 4, 5]));

        // Bytes length
        let result = bytes1.len().unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(3)));

        // Bytes indexing
        let result = bytes1.index_of(0).unwrap();
        assert!(matches!(result.weak(), OnionObject::Bytes(b) if *b == vec![1]));
    }

    #[test]
    fn test_range_operations() {
        let range1 = OnionObject::Range(1, 10);
        let range2 = OnionObject::Range(5, 15);

        // Range length
        let result = range1.len().unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(9)));

        // Range addition
        let result = range1.binary_add(&range2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Range(6, 25)));
    }

    #[test]
    fn test_bitwise_operations() {
        let int1 = OnionObject::Integer(12); // 1100 in binary
        let int2 = OnionObject::Integer(10); // 1010 in binary

        // Bitwise AND
        let result = int1.binary_and(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(8))); // 1000

        // Bitwise OR
        let result = int1.binary_or(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(14))); // 1110

        // Bitwise XOR
        let result = int1.binary_xor(&int2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(6))); // 0110

        // Left shift
        let result = int1.binary_shl(&OnionObject::Integer(1)).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(24))); // 11000

        // Right shift
        let result = int1.binary_shr(&OnionObject::Integer(1)).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(6))); // 0110
    }

    #[test]
    fn test_boolean_operations() {
        let bool1 = OnionObject::Boolean(true);
        let bool2 = OnionObject::Boolean(false);

        // Boolean AND
        let result = bool1.binary_and(&bool2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Boolean(false)));

        // Boolean OR
        let result = bool1.binary_or(&bool2).unwrap();
        assert!(matches!(result.weak(), OnionObject::Boolean(true)));
    }

    #[test]
    fn test_comparison_operations() {
        let int1 = OnionObject::Integer(10);
        let int2 = OnionObject::Integer(5);
        let float1 = OnionObject::Float(10.0);

        // Equality
        assert!(int1.binary_eq(&float1).unwrap());
        assert!(!int1.binary_eq(&int2).unwrap());

        // Less than
        assert!(!int1.binary_lt(&int2).unwrap());
        assert!(int2.binary_lt(&int1).unwrap());

        // Greater than
        assert!(int1.binary_gt(&int2).unwrap());
        assert!(!int2.binary_gt(&int1).unwrap());
    }

    #[test]
    fn test_unary_operations() {
        let int_obj = OnionObject::Integer(42);
        let float_obj = OnionObject::Float(-3.14);
        let bool_obj = OnionObject::Boolean(true);

        // Unary negation
        let result = int_obj.unary_neg().unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(-42)));

        let result = float_obj.unary_neg().unwrap();
        assert!(matches!(result.weak(), OnionObject::Float(f) if (*f - 3.14).abs() < f64::EPSILON));

        // Unary not
        let result = bool_obj.unary_not().unwrap();
        assert!(matches!(result.weak(), OnionObject::Boolean(false)));

        let result = int_obj.unary_not().unwrap();
        let expected = !42i64;
        assert!(matches!(result.weak(), OnionObject::Integer(i) if *i == expected));
    }

    #[test]
    fn test_equals_and_is_same() {
        let int1 = OnionObject::Integer(42);
        let int2 = OnionObject::Integer(42);
        let int3 = OnionObject::Integer(24);
        let float1 = OnionObject::Float(42.0);

        // Test equals
        assert!(int1.equals(&int2).unwrap());
        assert!(!int1.equals(&int3).unwrap());
        assert!(int1.equals(&float1).unwrap()); // Cross-type equality

        // Test is_same (should be same as equals for non-Mut types)
        assert!(int1.is_same(&int2).unwrap());
        assert!(!int1.is_same(&int3).unwrap());
    }

    #[test]
    fn test_mutable_objects() {
        let mut gc = GC::new();
        
        let int_obj = OnionObject::Integer(42);
        let mut mut_obj = int_obj.mutablize(&mut gc);

        // Test value extraction
        let value = mut_obj.weak().value().unwrap();
        assert!(matches!(value.weak(), OnionObject::Integer(42)));

        // Test assignment through with_data_mut
        let new_value = OnionObject::Integer(100);
        let result = mut_obj.weak_mut().assign(new_value);
        assert!(result.is_ok());
    }

    #[test]
    fn test_invalid_operations() {
        let string_obj = OnionObject::String("hello".to_string());
        let int_obj = OnionObject::Integer(42);

        // Invalid arithmetic operations
        assert!(string_obj.binary_mul(&int_obj).is_err());
        assert!(string_obj.binary_div(&int_obj).is_err());
        assert!(string_obj.unary_neg().is_err());

        // Invalid type conversions
        let null_obj = OnionObject::Null;
        assert!(null_obj.to_integer().is_err());
        assert!(null_obj.to_float().is_err());

        // Invalid attribute access
        assert!(int_obj.get_attribute(&string_obj).is_err());
    }

    #[test]
    fn test_static_object_wrapper() {
        let int_obj = OnionObject::Integer(42);
        let static_obj = OnionStaticObject::new(int_obj);

        assert!(matches!(static_obj.weak(), OnionObject::Integer(42)));
        assert!(static_obj.arcs.is_none()); // No GC arcs for basic types
    }

    #[test]
    fn test_debug_formatting() {
        let int_obj = OnionObject::Integer(42);
        let string_obj = OnionObject::String("test".to_string());
        let bool_obj = OnionObject::Boolean(true);
        let null_obj = OnionObject::Null;

        let debug_str = format!("{:?}", int_obj);
        assert!(debug_str.contains("Integer(42)"));

        let debug_str = format!("{:?}", string_obj);
        assert!(debug_str.contains("String(test)"));

        let debug_str = format!("{:?}", bool_obj);
        assert!(debug_str.contains("Boolean(true)"));

        let debug_str = format!("{:?}", null_obj);
        assert!(debug_str.contains("Null"));
    }
}
