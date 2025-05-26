use std::fmt::Debug;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use super::object::{ObjectError, OnionObject, OnionStaticObject};

#[derive(Clone)]
pub struct OnionTuple {
    pub elements: Vec<OnionObject>,
}

impl GCTraceable for OnionTuple {
    fn visit(&self) {
        for element in &self.elements {
            element.visit();
        }
    }
}

impl Debug for OnionTuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.elements.len() {
            0 => write!(f, "()"),
            1 => write!(f, "({:?},)", self.elements[0]),
            _ => {
                let elements: Vec<String> = self.elements.iter().map(|e| format!("{:?}", e)).collect();
                write!(f, "({})", elements.join(", "))
            }
            
        }
    }
}

#[macro_export]
macro_rules! onion_tuple {
    ($($x:expr),*) => {
        OnionTuple::new_static(vec![$($x),*])
    };
    () => {

    };
}

impl OnionTuple {
    pub fn new(elements: Vec<OnionObject>) -> Self {
        OnionTuple { elements }
    }

    pub fn new_static(elements: Vec<&OnionStaticObject>) -> OnionStaticObject {
        OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
            elements: elements.into_iter().map(|e| e.weak().clone()).collect(),
        }))
    }

    pub fn upgrade(&self) -> Option<Vec<GCArc>> {
        if self.elements.is_empty() {
            return None;
        }
        let mut arcs = Vec::new();
        for element in &self.elements {
            match element.upgrade() {
                Some(arc) => arcs.extend(arc),
                None => {
                    return None;
                }
            }
        }
        Some(arcs)
    }

    pub fn len(&self) -> Result<OnionStaticObject, ObjectError> {
        Ok(OnionStaticObject::new(OnionObject::Integer(
            self.elements.len() as i64,
        )))
    }

    pub fn at(&self, index: i64) -> Result<OnionStaticObject, ObjectError> {
        if index < 0 || index >= self.elements.len() as i64 {
            return Ok(OnionStaticObject::new(OnionObject::Undefined(
                "Index out of bounds".to_string(),
            )));
        }
        Ok(OnionStaticObject::new(
            self.elements[index as usize].clone(),
        ))
    }

    pub fn with_index<F, R>(&self, index: i64, f: &F) -> Result<R, ObjectError>
    where
        F: Fn(&OnionObject) -> Result<R, ObjectError>,
    {
        if index < 0 || index >= self.elements.len() as i64 {
            return Err(ObjectError::InvalidOperation(format!(
                "Index out of bounds: {}",
                index
            )));
        }
        f(&self.elements[index as usize])
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, ObjectError>
    where
        F: Fn(&OnionObject) -> Result<R, ObjectError>,
    {
        for element in &self.elements {
            match element {
                OnionObject::Named(named) => {
                    if named.key.as_ref().equals(key)? {
                        return f(named.value.as_ref());
                    }
                }
                OnionObject::Pair(pair) => {
                    if pair.key.as_ref().equals(key)? {
                        return f(pair.value.as_ref());
                    }
                }
                _ => {}
            }
        }
        Err(ObjectError::InvalidOperation(format!(
            "Attribute {:?} not found in tuple",
            key
        )))
    }

    pub fn with_attribute_mut<F, R>(&mut self, key: &OnionObject, f: F) -> Result<R, ObjectError>
    where
        F: Fn(&mut OnionObject) -> Result<R, ObjectError>,
    {
        for element in &mut self.elements {
            match element {
                OnionObject::Named(named) => {
                    if named.key.as_ref().equals(key)? {
                        return f(named.value.as_mut());
                    }
                }
                OnionObject::Pair(pair) => {
                    if pair.key.as_ref().equals(key)? {
                        return f(pair.value.as_mut());
                    }
                }
                _ => {}
            }
        }
        Err(ObjectError::InvalidOperation(format!(
            "Attribute {:?} not found in tuple",
            key
        )))
    }

    pub fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, ObjectError> {
        match other {
            OnionObject::Tuple(other_tuple) => {
                let mut new_elements = self.elements.clone();
                new_elements.extend(other_tuple.elements.clone());
                Ok(OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
                    elements: new_elements,
                })))
            }
            _ => Ok(OnionStaticObject::new(OnionObject::Undefined(
                "Invalid operation".to_string(),
            ))),
        }
    }
}

impl OnionTuple {
    pub fn equals(&self, other: &OnionObject) -> Result<bool, ObjectError> {
        match other {
            OnionObject::Tuple(other_tuple) => {
                if self.elements.len() != other_tuple.elements.len() {
                    return Ok(false);
                }
                for (a, b) in self.elements.iter().zip(&other_tuple.elements) {
                    if a.equals(b)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }
}
