use std::vec;

use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

use crate::stdlib::tuple;

use super::{build_dict, wrap_native_function};

/// Convert object to string
fn to_string(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "to_string requires a 'value' argument".into(),
        ));
    };
    let string_representation = value.weak().to_string(&vec![])?;
    Ok(OnionObject::String(string_representation.into()).stabilize())
}

/// Convert object to integer
fn to_int(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "to_int requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| match data {
        OnionObject::String(s) => match s.trim().parse::<i64>() {
            Ok(i) => Ok(OnionObject::Integer(i).stabilize()),
            Err(e) => Err(RuntimeError::InvalidOperation(
                format!("Cannot convert string '{s}' to integer: {e}").into(),
            )),
        },
        OnionObject::Float(f) => Ok(OnionObject::Integer(*f as i64).stabilize()),
        OnionObject::Integer(i) => Ok(OnionObject::Integer(*i).stabilize()),
        OnionObject::Boolean(b) => Ok(OnionObject::Integer(if *b { 1 } else { 0 }).stabilize()),
        _ => Err(RuntimeError::InvalidOperation(
            format!("Cannot convert {data:?} to integer").into(),
        )),
    })
}

/// Convert object to float
fn to_float(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "to_float requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| match data {
        OnionObject::String(s) => match s.trim().parse::<f64>() {
            Ok(f) => Ok(OnionObject::Float(f).stabilize()),
            Err(e) => Err(RuntimeError::InvalidOperation(
                format!("Cannot convert string '{s}' to float: {e}").into(),
            )),
        },
        OnionObject::Integer(i) => Ok(OnionObject::Float(*i as f64).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Float(*f).stabilize()),
        OnionObject::Boolean(b) => Ok(OnionObject::Float(if *b { 1.0 } else { 0.0 }).stabilize()),
        _ => Err(RuntimeError::InvalidOperation(
            format!("Cannot convert {data:?} to float").into(),
        )),
    })
}

/// Convert object to boolean
fn to_bool(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "to_bool requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| match data {
        OnionObject::String(s) => {
            let s = s.trim().to_lowercase();
            if s == "true" || s == "1" || s == "yes" || s == "y" {
                Ok(OnionObject::Boolean(true).stabilize())
            } else if s == "false" || s == "0" || s == "no" || s == "n" || s.is_empty() {
                Ok(OnionObject::Boolean(false).stabilize())
            } else {
                Err(RuntimeError::InvalidOperation(
                    format!("Cannot convert string '{s}' to boolean").into(),
                ))
            }
        }
        OnionObject::Integer(i) => Ok(OnionObject::Boolean(*i != 0).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Boolean(*f != 0.0).stabilize()),
        OnionObject::Boolean(b) => Ok(OnionObject::Boolean(*b).stabilize()),
        OnionObject::Undefined(_) => Ok(OnionObject::Boolean(false).stabilize()),
        OnionObject::Null => Ok(OnionObject::Boolean(false).stabilize()),
        _ => Ok(OnionObject::Boolean(true).stabilize()), // Other object types default to true
    })
}

/// Get object type name
fn type_of(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "type_of requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| {
        let type_name = data.type_of()?;
        Ok(OnionObject::String(type_name.into()).stabilize())
    })
}

/// Check if object is an integer
fn is_int(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "is_int requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| match data {
        OnionObject::Integer(_) => Ok(OnionObject::Boolean(true).stabilize()),
        _ => Ok(OnionObject::Boolean(false).stabilize()),
    })
}

/// Check if object is a float
fn is_float(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "is_float requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| match data {
        OnionObject::Float(_) => Ok(OnionObject::Boolean(true).stabilize()),
        _ => Ok(OnionObject::Boolean(false).stabilize()),
    })
}

/// Check if object is a string
fn is_string(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "is_string requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| match data {
        OnionObject::String(_) => Ok(OnionObject::Boolean(true).stabilize()),
        _ => Ok(OnionObject::Boolean(false).stabilize()),
    })
}

/// Check if object is a boolean
fn is_bool(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "is_bool requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| match data {
        OnionObject::Boolean(_) => Ok(OnionObject::Boolean(true).stabilize()),
        _ => Ok(OnionObject::Boolean(false).stabilize()),
    })
}

/// Check if object is bytes
fn is_bytes(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "is_bytes requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| match data {
        OnionObject::Bytes(_) => Ok(OnionObject::Boolean(true).stabilize()),
        _ => Ok(OnionObject::Boolean(false).stabilize()),
    })
}

/// Convert object to bytes
fn to_bytes(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "to_bytes requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|data| match data {
        OnionObject::String(s) => Ok(OnionObject::Bytes(s.as_bytes().to_vec().into()).stabilize()),
        OnionObject::Bytes(b) => Ok(OnionObject::Bytes(b.clone()).stabilize()),
        OnionObject::Integer(i) => {
            Ok(OnionObject::Bytes(i.to_string().into_bytes().into()).stabilize())
        }
        OnionObject::Float(f) => {
            Ok(OnionObject::Bytes(f.to_string().into_bytes().into()).stabilize())
        }
        OnionObject::Boolean(b) => Ok(OnionObject::Bytes(if *b {
            vec![1u8].into()
        } else {
            vec![0u8].into()
        })
        .stabilize()),
        _ => Err(RuntimeError::InvalidOperation(
            format!("Cannot convert {data:?} to bytes").into(),
        )),
    })
}

// get attr or undefined
fn find(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(obj) = argument.get("obj") else {
        return Err(RuntimeError::DetailedError(
            "find requires an 'obj' argument".into(),
        ));
    };
    let Some(key) = argument.get("key") else {
        return Err(RuntimeError::DetailedError(
            "find requires a 'key' argument".into(),
        ));
    };

    let key_borrowed = key.weak();
    match obj
        .weak()
        .with_attribute(key_borrowed, &|obj| Ok(obj.stabilize()))
    {
        Ok(value) => Ok(value),
        Err(RuntimeError::InvalidOperation(ref err)) => {
            // If the attribute is not found, return undefined
            Ok(OnionObject::Undefined(Some(err.clone().into())).stabilize())
        }
        Err(e) => {
            // If any other error occurs, propagate it
            Err(e)
        }
    }
}

/// Build the type conversion module
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // Type conversion functions
    module.insert(
        "to_string".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::to_string",
            OnionKeyPool::create(vec!["value".into()]),
            &to_string,
        ),
    );

    module.insert(
        "to_int".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::to_int",
            OnionKeyPool::create(vec!["value".into()]),
            &to_int,
        ),
    );

    module.insert(
        "to_float".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::to_float",
            OnionKeyPool::create(vec!["value".into()]),
            &to_float,
        ),
    );

    module.insert(
        "to_bool".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::to_bool",
            OnionKeyPool::create(vec!["value".into()]),
            &to_bool,
        ),
    );

    module.insert(
        "to_bytes".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::to_bytes",
            OnionKeyPool::create(vec!["value".into()]),
            &to_bytes,
        ),
    );

    // Type checking functions
    module.insert(
        "type_of".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::type_of",
            OnionKeyPool::create(vec!["value".into()]),
            &type_of,
        ),
    );

    module.insert(
        "is_int".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::is_int",
            OnionKeyPool::create(vec!["value".into()]),
            &is_int,
        ),
    );

    module.insert(
        "is_float".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::is_float",
            OnionKeyPool::create(vec!["value".into()]),
            &is_float,
        ),
    );

    module.insert(
        "is_string".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::is_string",
            OnionKeyPool::create(vec!["value".into()]),
            &is_string,
        ),
    );

    module.insert(
        "is_bool".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::is_bool",
            OnionKeyPool::create(vec!["value".into()]),
            &is_bool,
        ),
    );

    module.insert(
        "is_bytes".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::is_bytes",
            OnionKeyPool::create(vec!["value".into()]),
            &is_bytes,
        ),
    );

    // Find attribute function (has two parameters)
    module.insert(
        "find".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("obj"), LambdaParameter::top("key")].into(),
            ),
            OnionFastMap::default(), // No default arguments for these
            "types::find",
            OnionKeyPool::create(vec!["obj".into(), "key".into()]),
            &find,
        ),
    );

    // Assuming tuple::build_module() is already updated and compatible
    module.insert("tuple".to_string(), tuple::build_module());

    build_dict(module)
}
