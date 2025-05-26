use std::fmt::Debug;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use super::object::{ObjectError, OnionObject, OnionStaticObject};

#[derive(Clone)]
pub struct OnionPair {
    pub key: Box<OnionObject>,   // 使用 Box 避免递归
    pub value: Box<OnionObject>, // 使用 Box 避免递归
}

impl GCTraceable for OnionPair {
    fn visit(&self) {
        self.key.visit();
        self.value.visit();
    }
}

impl Debug for OnionPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} : {:?}", self.key, self.value)
    }
}

impl OnionPair {
    pub fn new(key: OnionObject, value: OnionObject) -> Self {
        OnionPair {
            key: Box::new(key),
            value: Box::new(value),
        }
    }

    pub fn new_static(key: &OnionStaticObject, value: &OnionStaticObject) -> OnionStaticObject {
        OnionObject::Pair(OnionPair {
            key: Box::new(key.weak().clone()),
            value: Box::new(value.weak().clone()),
        }).stabilize()
    }

    pub fn get_key(&self) -> &OnionObject {
        &self.key
    }

    pub fn get_key_mut(&mut self) -> &mut OnionObject {
        &mut self.key
    }

    pub fn get_value(&self) -> &OnionObject {
        &self.value
    }

    pub fn get_value_mut(&mut self) -> &mut OnionObject {
        &mut self.value
    }

    pub fn upgrade(&self) -> Option<Vec<GCArc>> {
        match (self.key.upgrade(), self.value.upgrade()) {
            (Some(mut key_arcs), Some(value_arcs)) => {
                key_arcs.extend(value_arcs);
                Some(key_arcs)
            }
            (Some(key_arcs), None) => Some(key_arcs),
            (None, Some(value_arcs)) => Some(value_arcs),
            (None, None) => None,
        }
    }
}

impl OnionPair {
    pub fn equals(&self, other: &OnionObject) -> Result<bool, ObjectError> {
        match other {
            OnionObject::Pair(pair) => {
                if self.key.equals(pair.key.as_ref())? && self.value.equals(pair.value.as_ref())? {
                    return Ok(true);
                }
            }
            _ => {}
        }
        Ok(false)
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, ObjectError>
    where
        F: Fn(&OnionObject) -> Result<R, ObjectError>,
    {
        self.value
            .with_attribute(key, f)
            .or_else(|_| self.key.with_attribute(key, f))
    }

    pub fn with_attribute_mut<F, R>(&mut self, key: &OnionObject, f: &F) -> Result<R, ObjectError>
    where
        F: Fn(&mut OnionObject) -> Result<R, ObjectError>,
    {
        self.value
            .with_attribute_mut(key, f)
            .or_else(|_| self.key.with_attribute_mut(key, f))
    }
}
