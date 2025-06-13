use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};

use crate::lambda::runnable::RuntimeError;

use super::object::{OnionObject, OnionObjectCell, OnionStaticObject};

#[derive(Clone)]
pub struct OnionPair {
    pub key: Box<OnionObject>,   // 使用 Box 避免递归
    pub value: Box<OnionObject>, // 使用 Box 避免递归
}

impl GCTraceable<OnionObjectCell> for OnionPair {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        self.key.collect(queue);
        self.value.collect(queue);
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
        })
        .stabilize()
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

    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.key.upgrade(collected);
        self.value.upgrade(collected)
    }
}

impl OnionPair {
    pub fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
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

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        self.value
            .with_attribute(key, f)
            .or_else(|_| self.key.with_attribute(key, f))
    }
}
