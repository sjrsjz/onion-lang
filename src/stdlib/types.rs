use std::vec;

use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
    GC,
};

use super::{build_named_dict, get_attr_direct, tuple, wrap_native_function};

/// Convert object to string
fn to_string(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        let string_representation = value.weak().to_string(&vec![])?;
        Ok(OnionObject::String(string_representation.into()).stabilize())
    })
}

/// Convert object to integer
fn to_int(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| match data {
            OnionObject::String(s) => match s.trim().parse::<i64>() {
                Ok(i) => Ok(OnionObject::Integer(i).stabilize()),
                Err(e) => Err(RuntimeError::InvalidOperation(
                    format!("Cannot convert string '{}' to integer: {}", s, e).into(),
                )),
            },
            OnionObject::Float(f) => Ok(OnionObject::Integer(*f as i64).stabilize()),
            OnionObject::Integer(i) => Ok(OnionObject::Integer(*i).stabilize()),
            OnionObject::Boolean(b) => Ok(OnionObject::Integer(if *b { 1 } else { 0 }).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                format!("Cannot convert {:?} to integer", data).into(),
            )),
        })
    })
}

/// Convert object to float
fn to_float(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| match data {
            OnionObject::String(s) => match s.trim().parse::<f64>() {
                Ok(f) => Ok(OnionObject::Float(f).stabilize()),
                Err(e) => Err(RuntimeError::InvalidOperation(
                    format!("Cannot convert string '{}' to float: {}", s, e).into(),
                )),
            },
            OnionObject::Integer(i) => Ok(OnionObject::Float(*i as f64).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Float(*f).stabilize()),
            OnionObject::Boolean(b) => {
                Ok(OnionObject::Float(if *b { 1.0 } else { 0.0 }).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                format!("Cannot convert {:?} to float", data).into(),
            )),
        })
    })
}

/// Convert object to boolean
fn to_bool(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| match data {
            OnionObject::String(s) => {
                let s = s.trim().to_lowercase();
                if s == "true" || s == "1" || s == "yes" || s == "y" {
                    Ok(OnionObject::Boolean(true).stabilize())
                } else if s == "false" || s == "0" || s == "no" || s == "n" || s.is_empty() {
                    Ok(OnionObject::Boolean(false).stabilize())
                } else {
                    Err(RuntimeError::InvalidOperation(
                        format!("Cannot convert string '{}' to boolean", s).into(),
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
    })
}

/// Get object type name
fn type_of(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| {
            let type_name = data.type_of()?;
            Ok(OnionObject::String(type_name.into()).stabilize())
        })
    })
}

/// Check if object is an integer
fn is_int(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| match data {
            OnionObject::Integer(_) => Ok(OnionObject::Boolean(true).stabilize()),
            _ => Ok(OnionObject::Boolean(false).stabilize()),
        })
    })
}

/// Check if object is a float
fn is_float(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| match data {
            OnionObject::Float(_) => Ok(OnionObject::Boolean(true).stabilize()),
            _ => Ok(OnionObject::Boolean(false).stabilize()),
        })
    })
}

/// Check if object is a string
fn is_string(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| match data {
            OnionObject::String(_) => Ok(OnionObject::Boolean(true).stabilize()),
            _ => Ok(OnionObject::Boolean(false).stabilize()),
        })
    })
}

/// Check if object is a boolean
fn is_bool(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| match data {
            OnionObject::Boolean(_) => Ok(OnionObject::Boolean(true).stabilize()),
            _ => Ok(OnionObject::Boolean(false).stabilize()),
        })
    })
}

/// Check if object is bytes
fn is_bytes(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| match data {
            OnionObject::Bytes(_) => Ok(OnionObject::Boolean(true).stabilize()),
            _ => Ok(OnionObject::Boolean(false).stabilize()),
        })
    })
}

/// Convert object to bytes
fn to_bytes(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;

        value.weak().with_data(|data| match data {
            OnionObject::String(s) => {
                Ok(OnionObject::Bytes(s.as_bytes().to_vec().into()).stabilize())
            }
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
                format!("Cannot convert {:?} to bytes", data).into(),
            )),
        })
    })
}

// get attr or undefined
fn find(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let obj = get_attr_direct(data, "obj".to_string())?;
        let key = get_attr_direct(data, "key".to_string())?;
        let key_borrowed = key.weak();
        match obj
            .weak()
            .with_attribute(key_borrowed, &|obj| Ok(obj.stabilize()))
        {
            Ok(value) => Ok(value),
            Err(RuntimeError::InvalidOperation(ref err)) => {
                // If the attribute is not found, return undefined
                Ok(OnionObject::Undefined(Some(err.as_ref().clone().into())).stabilize())
            }
            Err(e) => {
                // If any other error occurs, propagate it
                Err(e)
            }
        }
    })
}

/// Build the type conversion module
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // Type conversion functions
    let mut to_string_params = IndexMap::new();
    to_string_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to convert to string".to_string().into())).stabilize(),
    );
    module.insert(
        "to_string".to_string(),
        wrap_native_function(
            &build_named_dict(to_string_params),
            None,
            None,
            "types::to_string".to_string(),
            &to_string,
        ),
    );

    let mut to_int_params = IndexMap::new();
    to_int_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to convert to integer".to_string().into())).stabilize(),
    );
    module.insert(
        "to_int".to_string(),
        wrap_native_function(
            &build_named_dict(to_int_params),
            None,
            None,
            "types::to_int".to_string(),
            &to_int,
        ),
    );

    let mut to_float_params = IndexMap::new();
    to_float_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to convert to float".to_string().into())).stabilize(),
    );
    module.insert(
        "to_float".to_string(),
        wrap_native_function(
            &build_named_dict(to_float_params),
            None,
            None,
            "types::to_float".to_string(),
            &to_float,
        ),
    );

    let mut to_bool_params = IndexMap::new();
    to_bool_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to convert to boolean".to_string().into())).stabilize(),
    );
    module.insert(
        "to_bool".to_string(),
        wrap_native_function(
            &build_named_dict(to_bool_params),
            None,
            None,
            "types::to_bool".to_string(),
            &to_bool,
        ),
    );

    // to_bytes 函数 - 转换为字节
    let mut to_bytes_params = IndexMap::new();
    to_bytes_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to convert to bytes".to_string().into())).stabilize(),
    );
    module.insert(
        "to_bytes".to_string(),
        wrap_native_function(
            &build_named_dict(to_bytes_params),
            None,
            None,
            "types::to_bytes".to_string(),
            &to_bytes,
        ),
    );

    // Type checking functions
    let mut type_of_params = IndexMap::new();
    type_of_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to get type of".to_string().into())).stabilize(),
    );
    module.insert(
        "type_of".to_string(),
        wrap_native_function(
            &build_named_dict(type_of_params),
            None,
            None,
            "types::type_of".to_string(),
            &type_of,
        ),
    );

    let mut is_int_params = IndexMap::new();
    is_int_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to check if is integer".to_string().into())).stabilize(),
    );
    module.insert(
        "is_int".to_string(),
        wrap_native_function(
            &build_named_dict(is_int_params),
            None,
            None,
            "types::is_int".to_string(),
            &is_int,
        ),
    );

    let mut is_float_params = IndexMap::new();
    is_float_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to check if is float".to_string().into())).stabilize(),
    );
    module.insert(
        "is_float".to_string(),
        wrap_native_function(
            &build_named_dict(is_float_params),
            None,
            None,
            "types::is_float".to_string(),
            &is_float,
        ),
    );

    let mut is_string_params = IndexMap::new();
    is_string_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to check if is string".to_string().into())).stabilize(),
    );
    module.insert(
        "is_string".to_string(),
        wrap_native_function(
            &build_named_dict(is_string_params),
            None,
            None,
            "types::is_string".to_string(),
            &is_string,
        ),
    );

    let mut is_bool_params = IndexMap::new();
    is_bool_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to check if is boolean".to_string().into())).stabilize(),
    );
    module.insert(
        "is_bool".to_string(),
        wrap_native_function(
            &build_named_dict(is_bool_params),
            None,
            None,
            "types::is_bool".to_string(),
            &is_bool,
        ),
    );

    // is_bytes 函数 - 检查是否是字节
    let mut is_bytes_params = IndexMap::new();
    is_bytes_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value to check if is bytes".to_string().into())).stabilize(),
    );
    module.insert(
        "is_bytes".to_string(),
        wrap_native_function(
            &build_named_dict(is_bytes_params),
            None,
            None,
            "types::is_bytes".to_string(),
            &is_bytes,
        ),
    );

    // Find attribute function
    let mut find_params = IndexMap::new();
    find_params.insert(
        "obj".to_string(),
        OnionObject::Undefined(Some("Object to find attribute in".to_string().into())).stabilize(),
    );
    find_params.insert(
        "key".to_string(),
        OnionObject::Undefined(Some("Key to find in object".to_string().into())).stabilize(),
    );
    module.insert(
        "find".to_string(),
        wrap_native_function(
            &build_named_dict(find_params),
            None,
            None,
            "types::find".to_string(),
            &find,
        ),
    );

    module.insert("tuple".to_string(), tuple::build_module());

    build_named_dict(module)
}
