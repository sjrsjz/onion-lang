use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};

use crate::lambda::runnable::RuntimeError;

use super::object::{OnionObject, OnionObjectCell, OnionStaticObject};

pub struct OnionTuple {
    elements: Box<[OnionObject]>,
}

impl GCTraceable<OnionObjectCell> for OnionTuple {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        for element in &self.elements {
            element.collect(queue);
        }
    }
}

impl Debug for OnionTuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.elements.len() {
            0 => write!(f, "()"),
            1 => write!(f, "({:?},)", self.elements[0]),
            _ => {
                let elements: Vec<String> =
                    self.elements.iter().map(|e| format!("{:?}", e)).collect();
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
        OnionTuple {
            elements: elements.into(),
        }
    }

    pub fn new_static(elements: Vec<&OnionStaticObject>) -> OnionStaticObject {
        OnionStaticObject::new(OnionObject::Tuple(
            OnionTuple {
                elements: elements
                    .into_iter()
                    .map(|e| e.weak().clone())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            }
            .into(),
        ))
    }

    pub fn new_static_no_ref(elements: &Vec<OnionStaticObject>) -> OnionStaticObject {
        OnionObject::Tuple(
            OnionTuple {
                elements: elements
                    .into_iter()
                    .map(|e| e.weak().clone())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            }
            .into(),
        )
        .consume_and_stabilize()
    }

    #[inline(always)]
    pub fn get_elements(&self) -> &Box<[OnionObject]> {
        &self.elements
    }

    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.elements.iter().for_each(|e| e.upgrade(collected));
    }

    pub fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(OnionStaticObject::new(OnionObject::Integer(
            self.elements.len() as i64,
        )))
    }

    pub fn at(&self, index: i64) -> Result<OnionStaticObject, RuntimeError> {
        if index < 0 || index >= self.elements.len() as i64 {
            return Err(RuntimeError::InvalidOperation(
                format!("Index out of bounds: {}", index).into(),
            ));
        }
        Ok(OnionStaticObject::new(
            self.elements[index as usize].clone(),
        ))
    }

    pub fn with_index<F, R>(&self, index: i64, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        if index < 0 || index >= self.elements.len() as i64 {
            return Err(RuntimeError::InvalidOperation(
                format!("Index out of bounds: {}", index).into(),
            ));
        }
        let borrowed = &self.elements[index as usize];
        f(borrowed)
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        for element in &self.elements {
            match element {
                OnionObject::Pair(pair) => {
                    if pair.get_key().equals(key)? {
                        return f(&pair.get_value());
                    }
                }
                _ => {}
            }
        }
        Err(RuntimeError::InvalidOperation(
            format!("Attribute {:?} not found in tuple", key).into(),
        ))
    }

    pub fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::Tuple(other_tuple) => {
                let new_elements: Box<[OnionObject]> = self
                    .elements
                    .iter()
                    .chain(other_tuple.elements.iter())
                    .cloned()
                    .collect();
                Ok(OnionStaticObject::new(OnionObject::Tuple(
                    OnionTuple {
                        elements: new_elements,
                    }
                    .into(),
                )))
            }
            _ => Ok(OnionStaticObject::new(OnionObject::Undefined(Some(
                format!("Cannot add tuple with {:?}", other).into(),
            )))),
        }
    }

    pub fn contains(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        for element in &self.elements {
            if element.equals(other)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl OnionTuple {
    pub fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
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
