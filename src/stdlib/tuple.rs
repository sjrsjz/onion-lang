use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::{
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    GC,
};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

fn push(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let tuple = get_attr_direct(data, "container".to_string())?;
        let value = get_attr_direct(data, "value".to_string())?;
        tuple.weak().with_data(|tuple| match tuple {
            OnionObject::Tuple(tuple) => {
                let mut new_tuple = tuple.get_elements().clone();
                new_tuple.push(value.weak().clone());
                Ok(OnionObject::Tuple(OnionTuple::new(new_tuple).into()).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "Expected a tuple for 'container'".to_string().into(),
            )),
        })
    })
}

fn pop(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let tuple = get_attr_direct(data, "container".to_string())?;
        tuple.weak().with_data(|tuple| match tuple {
            OnionObject::Tuple(tuple) => {
                let mut new_tuple = tuple.get_elements().clone();
                match new_tuple.pop() {
                    Some(_) => {
                        Ok(OnionObject::Tuple(OnionTuple::new(new_tuple).into()).stabilize())
                    }
                    None => Err(RuntimeError::InvalidOperation(
                        "Cannot pop from an empty tuple".to_string().into(),
                    )),
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "Expected a tuple for 'container'".to_string().into(),
            )),
        })
    })
}

fn insert(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let tuple = get_attr_direct(data, "container".to_string())?;
        let index = get_attr_direct(data, "index".to_string())?;
        let value = get_attr_direct(data, "value".to_string())?;
        tuple.weak().with_data(|tuple| match tuple {
            OnionObject::Tuple(tuple) => {
                if let OnionObject::Integer(index) = index.weak() {
                    let mut new_tuple = tuple.get_elements().clone();
                    if (*index as usize) <= new_tuple.len() {
                        new_tuple.insert(*index as usize, value.weak().clone());
                        Ok(OnionObject::Tuple(OnionTuple::new(new_tuple).into()).stabilize())
                    } else {
                        Err(RuntimeError::InvalidOperation(
                            "Index out of bounds".to_string().into(),
                        ))
                    }
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Index must be an integer".to_string().into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "Expected a tuple for 'container'".to_string().into(),
            )),
        })
    })
}

fn remove(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let tuple = get_attr_direct(data, "container".to_string())?;
        let index = get_attr_direct(data, "index".to_string())?;
        tuple.weak().with_data(|tuple| match tuple {
            OnionObject::Tuple(tuple) => {
                if let OnionObject::Integer(index) = index.weak() {
                    let mut new_tuple = tuple.get_elements().clone();
                    if (*index as usize) < new_tuple.len() {
                        new_tuple.remove(*index as usize);
                        Ok(OnionObject::Tuple(OnionTuple::new(new_tuple).into()).stabilize())
                    } else {
                        Err(RuntimeError::InvalidOperation(
                            "Index out of bounds".to_string().into(),
                        ))
                    }
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Index must be an integer".to_string().into(),
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "Expected a tuple for 'container'".to_string().into(),
            )),
        })
    })
}

/// Build the type conversion module
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // Tuple module
    let mut push_params = IndexMap::new();
    push_params.insert(
        "container".to_string(),
        OnionObject::Undefined(Some("Container tuple".to_string().into())).stabilize(),
    );
    push_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to push".to_string().into())).stabilize(),
    );
    module.insert(
        "push".to_string(),
        wrap_native_function(
            &build_named_dict(push_params),
            None,
            None,
            "tuple::push".to_string(),
            &push,
        ),
    );

    let mut pop_params = IndexMap::new();
    pop_params.insert(
        "container".to_string(),
        OnionObject::Undefined(Some("Container tuple".to_string().into())).stabilize(),
    );
    module.insert(
        "pop".to_string(),
        wrap_native_function(
            &build_named_dict(pop_params),
            None,
            None,
            "tuple::pop".to_string(),
            &pop,
        ),
    );

    let mut insert_params = IndexMap::new();
    insert_params.insert(
        "container".to_string(),
        OnionObject::Undefined(Some("Container tuple".to_string().into())).stabilize(),
    );
    insert_params.insert(
        "index".to_string(),
        OnionObject::Undefined(Some("Index to insert at".to_string().into())).stabilize(),
    );
    insert_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to insert".to_string().into())).stabilize(),
    );
    module.insert(
        "insert".to_string(),
        wrap_native_function(
            &build_named_dict(insert_params),
            None,
            None,
            "tuple::insert".to_string(),
            &insert,
        ),
    );

    let mut remove_params = IndexMap::new();
    remove_params.insert(
        "container".to_string(),
        OnionObject::Undefined(Some("Container tuple".to_string().into())).stabilize(),
    );
    remove_params.insert(
        "index".to_string(),
        OnionObject::Undefined(Some("Index to remove".to_string().into())).stabilize(),
    );
    module.insert(
        "remove".to_string(),
        wrap_native_function(
            &build_named_dict(remove_params),
            None,
            None,
            "tuple::remove".to_string(),
            &remove,
        ),
    );

    build_named_dict(module)
}
