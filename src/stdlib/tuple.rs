use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

use super::{build_dict, wrap_native_function};

/// Pushes a value to a tuple, returning a new tuple.
fn push(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(container) = argument.get(&"container".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "push requires a 'container' argument".into(),
        ));
    };
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "push requires a 'value' argument".into(),
        ));
    };

    container.weak().with_data(|data| match data {
        OnionObject::Tuple(tuple) => {
            let mut new_elements = tuple.get_elements().clone();
            new_elements.push(value.weak().clone());
            Ok(OnionObject::Tuple(OnionTuple::new(new_elements).into()).stabilize())
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'container' must be a tuple for push"
                .to_string()
                .into(),
        )),
    })
}

/// Pops a value from a tuple, returning a new tuple.
fn pop(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(container) = argument.get(&"container".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "pop requires a 'container' argument".into(),
        ));
    };

    container.weak().with_data(|data| match data {
        OnionObject::Tuple(tuple) => {
            let mut new_elements = tuple.get_elements().clone();
            if new_elements.pop().is_some() {
                Ok(OnionObject::Tuple(OnionTuple::new(new_elements).into()).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    "Cannot pop from an empty tuple".into(),
                ))
            }
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'container' must be a tuple for pop"
                .to_string()
                .into(),
        )),
    })
}

/// Inserts a value into a tuple at a specific index, returning a new tuple.
fn insert(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(container) = argument.get(&"container".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "insert requires a 'container' argument".into(),
        ));
    };
    let Some(index_obj) = argument.get(&"index".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "insert requires an 'index' argument".into(),
        ));
    };
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "insert requires a 'value' argument".into(),
        ));
    };

    container.weak().with_data(|data| match data {
        OnionObject::Tuple(tuple) => {
            let index = match index_obj.weak() {
                OnionObject::Integer(i) => *i,
                _ => {
                    return Err(RuntimeError::InvalidType(
                        "Argument 'index' must be an integer".into(),
                    ));
                }
            };

            let mut new_elements = tuple.get_elements().clone();
            if (index as usize) <= new_elements.len() {
                new_elements.insert(index as usize, value.weak().clone());
                Ok(OnionObject::Tuple(OnionTuple::new(new_elements).into()).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    "Index out of bounds for insert".into(),
                ))
            }
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'container' must be a tuple for insert"
                .to_string()
                .into(),
        )),
    })
}

/// Removes a value from a tuple at a specific index, returning a new tuple.
fn remove(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(container) = argument.get(&"container".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "remove requires a 'container' argument".into(),
        ));
    };
    let Some(index_obj) = argument.get(&"index".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "remove requires an 'index' argument".into(),
        ));
    };

    container.weak().with_data(|data| match data {
        OnionObject::Tuple(tuple) => {
            let index = match index_obj.weak() {
                OnionObject::Integer(i) => *i,
                _ => {
                    return Err(RuntimeError::InvalidType(
                        "Argument 'index' must be an integer".into(),
                    ));
                }
            };

            let mut new_elements = tuple.get_elements().clone();
            if (index as usize) < new_elements.len() {
                new_elements.remove(index as usize);
                Ok(OnionObject::Tuple(OnionTuple::new(new_elements).into()).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    "Index out of bounds for remove".into(),
                ))
            }
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'container' must be a tuple for remove"
                .to_string()
                .into(),
        )),
    })
}

/// Build the tuple manipulation module.
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // tuple.push(container, value)
    module.insert(
        "push".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(vec![
                LambdaParameter::top("container"),
                LambdaParameter::top("value"),
            ]),
            OnionFastMap::default(),
            "tuple::push".to_string(),
            OnionKeyPool::create(vec!["container".to_string(), "value".to_string()]),
            &push,
        ),
    );

    // tuple.pop(container)
    module.insert(
        "pop".to_string(),
        wrap_native_function(
            LambdaParameter::top("container"),
            OnionFastMap::default(),
            "tuple::pop".to_string(),
            OnionKeyPool::create(vec!["container".to_string()]),
            &pop,
        ),
    );

    // tuple.insert(container, index, value)
    module.insert(
        "insert".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(vec![
                LambdaParameter::top("container"),
                LambdaParameter::top("index"),
                LambdaParameter::top("value"),
            ]),
            OnionFastMap::default(),
            "tuple::insert".to_string(),
            OnionKeyPool::create(vec![
                "container".to_string(),
                "index".to_string(),
                "value".to_string(),
            ]),
            &insert,
        ),
    );

    // tuple.remove(container, index)
    module.insert(
        "remove".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(vec![
                LambdaParameter::top("container"),
                LambdaParameter::top("index"),
            ]),
            OnionFastMap::default(),
            "tuple::remove".to_string(),
            OnionKeyPool::create(vec!["container".to_string(), "index".to_string()]),
            &remove,
        ),
    );

    build_dict(module)
}
