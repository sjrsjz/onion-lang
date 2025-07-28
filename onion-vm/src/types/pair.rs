use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};

use crate::lambda::runnable::RuntimeError;

use super::object::{OnionObject, OnionObjectCell, OnionStaticObject};

pub struct OnionPair {
    key: OnionObject,   // 使用 Box 避免递归
    value: OnionObject, // 使用 Box 避免递归
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
            key: key.into(),
            value: value.into(),
        }
    }

    pub fn new_static(key: &OnionStaticObject, value: &OnionStaticObject) -> OnionStaticObject {
        OnionObject::Pair(
            OnionPair {
                key: key.weak().clone(),
                value: value.weak().clone(),
            }
            .into(),
        )
        .consume_and_stabilize()
    }

    #[inline(always)]
    pub fn get_key(&self) -> &OnionObject {
        &self.key
    }

    #[inline(always)]
    pub fn get_value(&self) -> &OnionObject {
        &self.value
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
                if self.key.equals(&pair.key)? && self.value.equals(&pair.value)? {
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