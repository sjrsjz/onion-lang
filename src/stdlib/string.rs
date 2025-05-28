use std::collections::HashMap;

use onion_vm::{
    lambda::runnable::RuntimeError,
    types::{
        object::{ObjectError, OnionObject, OnionStaticObject},
    },
    GC,
};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

fn length(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| {
            let string = get_attr_direct(data, "string".to_string())?;
            string.weak().with_data(|string_data| {
                match string_data {
                    OnionObject::String(s) => Ok(OnionObject::Integer(s.len() as i64).stabilize()),
                    _ => Err(ObjectError::InvalidOperation(
                        "length requires string".to_string(),
                    )),
                }
            })
        })
        .map_err(RuntimeError::ObjectError)
}

fn trim(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| {
            let string = get_attr_direct(data, "string".to_string())?;
            string.weak().with_data(|string_data| {
                match string_data {
                    OnionObject::String(s) => Ok(OnionObject::String(s.trim().to_string()).stabilize()),
                    _ => Err(ObjectError::InvalidOperation(
                        "trim requires string".to_string(),
                    )),
                }
            })
        })
        .map_err(RuntimeError::ObjectError)
}

fn uppercase(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| {
            let string = get_attr_direct(data, "string".to_string())?;
            string.weak().with_data(|string_data| {
                match string_data {
                    OnionObject::String(s) => Ok(OnionObject::String(s.to_uppercase()).stabilize()),
                    _ => Err(ObjectError::InvalidOperation(
                        "uppercase requires string".to_string(),
                    )),
                }
            })
        })
        .map_err(RuntimeError::ObjectError)
}

fn lowercase(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| {
            let string = get_attr_direct(data, "string".to_string())?;
            string.weak().with_data(|string_data| {
                match string_data {
                    OnionObject::String(s) => Ok(OnionObject::String(s.to_lowercase()).stabilize()),
                    _ => Err(ObjectError::InvalidOperation(
                        "lowercase requires string".to_string(),
                    )),
                }
            })
        })
        .map_err(RuntimeError::ObjectError)
}

fn contains(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| {
            let string = get_attr_direct(data, "string".to_string())?;
            let substring = get_attr_direct(data, "substring".to_string())?;
            
            string.weak().with_data(|string_data| {
                substring.weak().with_data(|substring_data| {
                    match (string_data, substring_data) {
                        (OnionObject::String(s), OnionObject::String(sub)) => {
                            Ok(OnionObject::Boolean(s.contains(sub)).stabilize())
                        }
                        _ => Err(ObjectError::InvalidOperation(
                            "contains requires string arguments".to_string(),
                        )),
                    }
                })
            })
        })
        .map_err(RuntimeError::ObjectError)
}

fn concat(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| {
            let a = get_attr_direct(data, "a".to_string())?;
            let b = get_attr_direct(data, "b".to_string())?;
            
            a.weak().with_data(|a_data| {
                b.weak().with_data(|b_data| {
                    match (a_data, b_data) {
                        (OnionObject::String(s1), OnionObject::String(s2)) => {
                            let mut result = s1.clone();
                            result.push_str(s2);
                            Ok(OnionObject::String(result).stabilize())
                        }
                        _ => Err(ObjectError::InvalidOperation(
                            "concat requires string arguments".to_string(),
                        )),
                    }
                })
            })
        })
        .map_err(RuntimeError::ObjectError)
}

pub fn build_module() -> OnionStaticObject {
    let mut module = HashMap::new();

    // length 函数
    let mut length_params = HashMap::new();
    length_params.insert(
        "string".to_string(),
        OnionObject::Undefined("String to get length".to_string()).stabilize(),
    );
    module.insert(
        "length".to_string(),
        wrap_native_function(
            &build_named_dict(length_params),
            None,
            None,
            "string::length".to_string(),
            length,
        ),
    );

    // trim 函数
    let mut trim_params = HashMap::new();
    trim_params.insert(
        "string".to_string(),
        OnionObject::Undefined("String to trim".to_string()).stabilize(),
    );
    module.insert(
        "trim".to_string(),
        wrap_native_function(
            &build_named_dict(trim_params),
            None,
            None,
            "string::trim".to_string(),
            trim,
        ),
    );

    // uppercase 函数
    let mut uppercase_params = HashMap::new();
    uppercase_params.insert(
        "string".to_string(),
        OnionObject::Undefined("String to convert to uppercase".to_string()).stabilize(),
    );
    module.insert(
        "uppercase".to_string(),
        wrap_native_function(
            &build_named_dict(uppercase_params),
            None,
            None,
            "string::uppercase".to_string(),
            uppercase,
        ),
    );

    // lowercase 函数
    let mut lowercase_params = HashMap::new();
    lowercase_params.insert(
        "string".to_string(),
        OnionObject::Undefined("String to convert to lowercase".to_string()).stabilize(),
    );
    module.insert(
        "lowercase".to_string(),
        wrap_native_function(
            &build_named_dict(lowercase_params),
            None,
            None,
            "string::lowercase".to_string(),
            lowercase,
        ),
    );

    // contains 函数
    let mut contains_params = HashMap::new();
    contains_params.insert(
        "string".to_string(),
        OnionObject::Undefined("String to search within".to_string()).stabilize(),
    );
    contains_params.insert(
        "substring".to_string(),
        OnionObject::Undefined("Substring to search for".to_string()).stabilize(),
    );
    module.insert(
        "contains".to_string(),
        wrap_native_function(
            &build_named_dict(contains_params),
            None,
            None,
            "string::contains".to_string(),
            contains,
        ),
    );

    // concat 函数
    let mut concat_params = HashMap::new();
    concat_params.insert(
        "a".to_string(),
        OnionObject::Undefined("First string to concatenate".to_string()).stabilize(),
    );
    concat_params.insert(
        "b".to_string(),
        OnionObject::Undefined("Second string to concatenate".to_string()).stabilize(),
    );
    module.insert(
        "concat".to_string(),
        wrap_native_function(
            &build_named_dict(concat_params),
            None,
            None,
            "string::concat".to_string(),
            concat,
        ),
    );

    build_named_dict(module)
}
