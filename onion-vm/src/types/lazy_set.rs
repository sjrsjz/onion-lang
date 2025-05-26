use std::fmt::Debug;

use arc_gc::traceable::GCTraceable;

use super::object::{OnionObject, OnionStaticObject};

#[derive(Clone)]
pub struct OnionLazySet {
    pub container: Box<OnionObject>,
    pub filter: Box<OnionObject>,
}

impl GCTraceable for OnionLazySet {
    fn visit(&self) {
        self.container.visit();
        self.filter.visit();
    }
}

impl Debug for OnionLazySet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LazySet({:?}, {:?})", self.container, self.filter)
    }
}

impl OnionLazySet {
    pub fn new(container: OnionObject, filter: OnionObject) -> Self {
        OnionLazySet {
            container: Box::new(container),
            filter: Box::new(filter),
        }
    }

    pub fn new_static(container: &OnionStaticObject, filter: &OnionStaticObject) -> OnionStaticObject {
        OnionObject::LazySet(OnionLazySet {
            container: Box::new(container.clone()),
            filter: Box::new(filter.clone()),
        }).stabilize()
    }

    pub fn get_container(&self) -> &OnionObject {
        &self.container
    }

    pub fn get_filter(&self) -> &OnionObject {
        &self.filter
    }
}