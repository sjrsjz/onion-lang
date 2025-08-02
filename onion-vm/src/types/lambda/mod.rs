use rustc_hash::FxHashMap;

use crate::types::{
    object::{OnionObject, OnionStaticObject},
    pair::OnionPair,
    tuple::OnionTuple,
};

pub mod context;
pub mod definition;
pub mod launcher;
pub(crate) mod native;
pub mod runnable;
pub mod vm_instructions;
pub mod parameter;

pub fn build_dict_from_hashmap(
    hashmap: &FxHashMap<String, OnionStaticObject>,
) -> OnionStaticObject {
    let mut pairs = vec![];
    for (key, value) in hashmap {
        pairs.push(OnionPair::new_static(
            &OnionObject::String(key.clone().into()).stabilize(),
            &value,
        ));
    }
    OnionTuple::new_static_no_ref(&pairs)
}

pub fn build_dict_from_hashmap_weak(hashmap: &FxHashMap<String, OnionObject>) -> OnionStaticObject {
    let mut pairs = vec![];
    for (key, value) in hashmap {
        pairs.push(
            OnionObject::Pair(
                OnionPair::new(OnionObject::String(key.clone().into()), value.clone()).into(),
            )
            .consume_and_stabilize(),
        );
    }
    OnionTuple::new_static_no_ref(&pairs)
}
