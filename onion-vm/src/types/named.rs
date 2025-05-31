use std::fmt::Debug;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use super::object::{ObjectError, OnionObject, OnionObjectCell, OnionStaticObject};

#[derive(Clone)]
pub struct OnionNamed {
    pub key: Box<OnionObjectCell>,   // 使用 Box 避免递归
    pub value: Box<OnionObjectCell>, // 使用 Box 避免递归
}

impl GCTraceable for OnionNamed {
    fn visit(&self) {
        self.key.visit();
        self.value.visit();
    }
}

impl Debug for OnionNamed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} => {:?}", self.key, self.value)
    }
}

impl OnionNamed {
    pub fn new(key: OnionObjectCell, value: OnionObjectCell) -> Self {
        OnionNamed {
            key: Box::new(key),
            value: Box::new(value),
        }
    }

    pub fn new_static(key: &OnionStaticObject, value: &OnionStaticObject) -> OnionStaticObject {
        OnionObject::Named(OnionNamed {
            key: Box::new(key.weak().clone()),
            value: Box::new(value.weak().clone()),
        })
        .stabilize()
    }

    pub fn get_key(&self) -> &OnionObjectCell {
        &self.key
    }

    pub fn get_key_mut(&mut self) -> &mut OnionObjectCell {
        &mut self.key
    }

    pub fn get_value(&self) -> &OnionObjectCell {
        &self.value
    }

    pub fn get_value_mut(&mut self) -> &mut OnionObjectCell {
        &mut self.value
    }

    pub fn upgrade(&self) -> Option<Vec<GCArc<OnionObjectCell>>> {
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

impl OnionNamed {
    pub fn equals(&self, other: &OnionObject) -> Result<bool, ObjectError> {
        match other {
            OnionObject::Named(pair) => {
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
