use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};

use crate::lambda::runnable::RuntimeError;

use super::object::{OnionObject, OnionObjectCell, OnionStaticObject};

#[derive(Clone)]
pub struct OnionNamed {
    key: OnionObject,
    value: OnionObject,
}

impl GCTraceable<OnionObjectCell> for OnionNamed {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        self.key.collect(queue);
        self.value.collect(queue);
    }
}

impl Debug for OnionNamed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} => {:?}", self.key, self.value)
    }
}

impl OnionNamed {
    pub fn new(key: OnionObject, value: OnionObject) -> Self {
        OnionNamed {
            key: key.into(),
            value: value.into(),
        }
    }

    pub fn new_static(key: &OnionStaticObject, value: &OnionStaticObject) -> OnionStaticObject {
        OnionObject::Named(
            OnionNamed {
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

impl OnionNamed {
    pub fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::Named(pair) => {
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
impl OnionNamed {
    pub fn reconstruct_container(&self) -> Result<OnionObject, RuntimeError> {
        Ok(OnionObject::Named(
            OnionNamed {
                key: self.key.clone(),
                value: self.value.clone(),
            }
            .into(),
        ))
    }
}
