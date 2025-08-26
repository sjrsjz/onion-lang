use std::{collections::VecDeque, fmt::Debug, sync::Arc};

use crate::{
    lambda::runnable::RuntimeError,
    types::{
        lambda::vm_instructions::instruction_set::VMInstructionPackage,
        object::{
            OnionObject, OnionObjectCell, OnionObjectProtocol, OnionObjectProtocolStatic,
            OnionStaticObject,
        },
    },
};
use arc_gc::{arc::GCArcWeak, traceable::GCTraceable};

#[derive(Clone)]
pub struct OnionInstructionPackage {
    value: Arc<VMInstructionPackage>,
}

impl GCTraceable<OnionObjectCell> for OnionInstructionPackage {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {}
}

impl Debug for OnionInstructionPackage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

impl OnionInstructionPackage {
    #[inline(always)]
    pub fn new(value: VMInstructionPackage) -> Self {
        OnionInstructionPackage {
            value: Arc::new(value),
        }
    }

    #[inline(always)]
    pub fn new_static(value: VMInstructionPackage) -> OnionStaticObject {
        OnionObject::InstructionPackage(Self::new(value)).consume_and_stabilize()
    }

    #[inline(always)]
    pub fn value(&self) -> &VMInstructionPackage {
        &self.value
    }
}

impl OnionObjectProtocol for OnionInstructionPackage {
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok("InstructionPackage(...)".into())
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("InstructionPackage".into())
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, _collected: &mut Vec<arc_gc::arc::GCArc<OnionObjectCell>>) {}

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        // 采用简单的指针比较
        match other {
            OnionObject::InstructionPackage(other) => Ok(Arc::ptr_eq(&self.value, &other.value)),
            _ => Ok(false),
        }
    }
}

impl OnionObjectProtocolStatic for OnionInstructionPackage {}
