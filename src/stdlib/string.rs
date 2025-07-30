use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
};
use rustc_hash::FxHashMap;

use super::{build_dict, get_attr_direct, wrap_native_function};

fn length(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        string.weak().with_data(|string_data| match string_data {
            OnionObject::String(s) => Ok(OnionObject::Integer(s.len() as i64).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "length requires string".to_string().into(),
            )),
        })
    })
}

fn trim(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        string.weak().with_data(|string_data| match string_data {
            OnionObject::String(s) => {
                Ok(OnionObject::String(s.trim().to_string().into()).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "trim requires string".to_string().into(),
            )),
        })
    })
}

fn uppercase(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        string.weak().with_data(|string_data| match string_data {
            OnionObject::String(s) => Ok(OnionObject::String(s.to_uppercase().into()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "uppercase requires string".to_string().into(),
            )),
        })
    })
}

fn lowercase(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        string.weak().with_data(|string_data| match string_data {
            OnionObject::String(s) => Ok(OnionObject::String(s.to_lowercase().into()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "lowercase requires string".to_string().into(),
            )),
        })
    })
}

fn contains(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let substring = get_attr_direct(data, "substring".to_string())?;

        string.weak().with_data(|string_data| {
            substring
                .weak()
                .with_data(|substring_data| match (string_data, substring_data) {
                    (OnionObject::String(s), OnionObject::String(sub)) => {
                        Ok(OnionObject::Boolean(s.contains(sub.as_ref())).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "contains requires string arguments".to_string().into(),
                    )),
                })
        })
    })
}

fn concat(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let a = get_attr_direct(data, "a".to_string())?;
        let b = get_attr_direct(data, "b".to_string())?;

        a.weak().with_data(|a_data| {
            b.weak().with_data(|b_data| match (a_data, b_data) {
                (OnionObject::String(s1), OnionObject::String(s2)) => {
                    let mut result = s1.as_ref().clone();
                    result.push_str(s2);
                    Ok(OnionObject::String(result.into()).stabilize())
                }
                _ => Err(RuntimeError::InvalidOperation(
                    "concat requires string arguments".to_string().into(),
                )),
            })
        })
    })
}

/// Split string by delimiter
fn split(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    use onion_vm::types::tuple::OnionTuple;

    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let delimiter = get_attr_direct(data, "delimiter".to_string())?;

        string.weak().with_data(|string_data| {
            delimiter
                .weak()
                .with_data(|delimiter_data| match (string_data, delimiter_data) {
                    (OnionObject::String(s), OnionObject::String(delim)) => {
                        let parts: Vec<_> = s
                            .split(delim.as_ref())
                            .map(|part| OnionObject::String(part.to_string().into()).stabilize())
                            .collect();
                        Ok(OnionTuple::new_static_no_ref(&parts))
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "split requires string arguments".to_string().into(),
                    )),
                })
        })
    })
}

/// Replace all occurrences of a substring
fn replace(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let from = get_attr_direct(data, "from".to_string())?;
        let to = get_attr_direct(data, "to".to_string())?;

        string.weak().with_data(|string_data| {
            from.weak().with_data(|from_data| {
                to.weak()
                    .with_data(|to_data| match (string_data, from_data, to_data) {
                        (
                            OnionObject::String(s),
                            OnionObject::String(f),
                            OnionObject::String(t),
                        ) => {
                            let result = s.replace(f.as_ref(), t);
                            Ok(OnionObject::String(result.into()).stabilize())
                        }
                        _ => Err(RuntimeError::InvalidOperation(
                            "replace requires string arguments".to_string().into(),
                        )),
                    })
            })
        })
    })
}

/// Get substring from start to end index
fn substr(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let start = get_attr_direct(data, "start".to_string())?;
        let length = get_attr_direct(data, "length".to_string())?;

        string.weak().with_data(|string_data| {
            start.weak().with_data(|start_data| {
                length.weak().with_data(|length_data| {
                    match (string_data, start_data, length_data) {
                        (
                            OnionObject::String(s),
                            OnionObject::Integer(start_idx),
                            OnionObject::Integer(len),
                        ) => {
                            let start_idx = *start_idx as usize;
                            let len = *len as usize;

                            if start_idx >= s.len() {
                                Ok(OnionObject::String("".to_string().into()).stabilize())
                            } else {
                                let end_idx = std::cmp::min(start_idx + len, s.len());
                                let result = s
                                    .chars()
                                    .skip(start_idx)
                                    .take(end_idx - start_idx)
                                    .collect::<String>();
                                Ok(OnionObject::String(result.into()).stabilize())
                            }
                        }
                        _ => Err(RuntimeError::InvalidOperation(
                            "substr requires string and integer arguments"
                                .to_string()
                                .into(),
                        )),
                    }
                })
            })
        })
    })
}

/// Find the index of a substring
fn index_of(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let substring = get_attr_direct(data, "substring".to_string())?;

        string.weak().with_data(|string_data| {
            substring
                .weak()
                .with_data(|substring_data| match (string_data, substring_data) {
                    (OnionObject::String(s), OnionObject::String(sub)) => {
                        match s.find(sub.as_ref()) {
                            Some(index) => Ok(OnionObject::Integer(index as i64).stabilize()),
                            None => Ok(OnionObject::Integer(-1).stabilize()),
                        }
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "index_of requires string arguments".to_string().into(),
                    )),
                })
        })
    })
}

/// Check if string starts with a prefix
fn starts_with(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let prefix = get_attr_direct(data, "prefix".to_string())?;

        string.weak().with_data(|string_data| {
            prefix
                .weak()
                .with_data(|prefix_data| match (string_data, prefix_data) {
                    (OnionObject::String(s), OnionObject::String(p)) => {
                        Ok(OnionObject::Boolean(s.starts_with(p.as_ref())).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "starts_with requires string arguments".to_string().into(),
                    )),
                })
        })
    })
}

/// Check if string ends with a suffix
fn ends_with(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let suffix = get_attr_direct(data, "suffix".to_string())?;

        string.weak().with_data(|string_data| {
            suffix
                .weak()
                .with_data(|suffix_data| match (string_data, suffix_data) {
                    (OnionObject::String(s), OnionObject::String(suf)) => {
                        Ok(OnionObject::Boolean(s.ends_with(suf.as_ref())).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "ends_with requires string arguments".to_string().into(),
                    )),
                })
        })
    })
}

/// Repeat string n times
fn repeat(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let count = get_attr_direct(data, "count".to_string())?;

        string.weak().with_data(|string_data| {
            count
                .weak()
                .with_data(|count_data| match (string_data, count_data) {
                    (OnionObject::String(s), OnionObject::Integer(n)) => {
                        if *n < 0 {
                            return Err(RuntimeError::InvalidOperation(
                                "repeat count cannot be negative".to_string().into(),
                            ));
                        }
                        let result = s.repeat(*n as usize);
                        Ok(OnionObject::String(result.into()).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "repeat requires string and integer arguments"
                            .to_string()
                            .into(),
                    )),
                })
        })
    })
}

/// Pad string on the left with specified character
fn pad_left(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let length = get_attr_direct(data, "length".to_string())?;
        let pad_char = get_attr_direct(data, "pad_char".to_string())?;

        string.weak().with_data(|string_data| {
            length.weak().with_data(|length_data| {
                pad_char.weak().with_data(|pad_char_data| {
                    match (string_data, length_data, pad_char_data) {
                        (
                            OnionObject::String(s),
                            OnionObject::Integer(len),
                            OnionObject::String(pad),
                        ) => {
                            let target_len = *len as usize;
                            if s.len() >= target_len {
                                Ok(OnionObject::String(s.clone()).stabilize())
                            } else {
                                let pad_count = target_len - s.len();
                                let pad_char = pad.chars().next().unwrap_or(' ');
                                let padded =
                                    format!("{}{}", pad_char.to_string().repeat(pad_count), s);
                                Ok(OnionObject::String(padded.into()).stabilize())
                            }
                        }
                        _ => Err(RuntimeError::InvalidOperation(
                            "pad_left requires string, integer, and string arguments"
                                .to_string()
                                .into(),
                        )),
                    }
                })
            })
        })
    })
}

/// Pad string on the right with specified character
fn pad_right(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        let length = get_attr_direct(data, "length".to_string())?;
        let pad_char = get_attr_direct(data, "pad_char".to_string())?;

        string.weak().with_data(|string_data| {
            length.weak().with_data(|length_data| {
                pad_char.weak().with_data(|pad_char_data| {
                    match (string_data, length_data, pad_char_data) {
                        (
                            OnionObject::String(s),
                            OnionObject::Integer(len),
                            OnionObject::String(pad),
                        ) => {
                            let target_len = *len as usize;
                            if s.len() >= target_len {
                                Ok(OnionObject::String(s.clone()).stabilize())
                            } else {
                                let pad_count = target_len - s.len();
                                let pad_char = pad.chars().next().unwrap_or(' ');
                                let padded =
                                    format!("{}{}", s, pad_char.to_string().repeat(pad_count));
                                Ok(OnionObject::String(padded.into()).stabilize())
                            }
                        }
                        _ => Err(RuntimeError::InvalidOperation(
                            "pad_right requires string, integer, and string arguments"
                                .to_string()
                                .into(),
                        )),
                    }
                })
            })
        })
    })
}

/// Check if string is empty
fn is_empty(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        string.weak().with_data(|string_data| match string_data {
            OnionObject::String(s) => Ok(OnionObject::Boolean(s.is_empty()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "is_empty requires string".to_string().into(),
            )),
        })
    })
}

/// Reverse a string
fn reverse(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        string.weak().with_data(|string_data| match string_data {
            OnionObject::String(s) => {
                let reversed: String = s.chars().rev().collect();
                Ok(OnionObject::String(reversed.into()).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "reverse requires string".to_string().into(),
            )),
        })
    })
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // 统一参数定义
    let string_arg = &OnionObject::String("string".to_string().into()).stabilize();

    // 单参数函数
    module.insert(
        "length".to_string(),
        wrap_native_function(
            string_arg,
            &FxHashMap::default(),
            "string::length".to_string(),
            &length,
        ),
    );
    module.insert(
        "trim".to_string(),
        wrap_native_function(
            string_arg,
            &FxHashMap::default(),
            "string::trim".to_string(),
            &trim,
        ),
    );
    module.insert(
        "uppercase".to_string(),
        wrap_native_function(
            string_arg,
            &FxHashMap::default(),
            "string::uppercase".to_string(),
            &uppercase,
        ),
    );
    module.insert(
        "lowercase".to_string(),
        wrap_native_function(
            string_arg,
            &FxHashMap::default(),
            "string::lowercase".to_string(),
            &lowercase,
        ),
    );
    module.insert(
        "is_empty".to_string(),
        wrap_native_function(
            string_arg,
            &FxHashMap::default(),
            "string::is_empty".to_string(),
            &is_empty,
        ),
    );
    module.insert(
        "reverse".to_string(),
        wrap_native_function(
            string_arg,
            &FxHashMap::default(),
            "string::reverse".to_string(),
            &reverse,
        ),
    );

    // 多参数函数
    module.insert(
        "contains".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "substring"]),
            &FxHashMap::default(),
            "string::contains".to_string(),
            &contains,
        ),
    );
    module.insert(
        "concat".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["a", "b"]),
            &FxHashMap::default(),
            "string::concat".to_string(),
            &concat,
        ),
    );
    module.insert(
        "split".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "delimiter"]),
            &FxHashMap::default(),
            "string::split".to_string(),
            &split,
        ),
    );
    module.insert(
        "replace".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "from", "to"]),
            &FxHashMap::default(),
            "string::replace".to_string(),
            &replace,
        ),
    );
    module.insert(
        "substr".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "start", "length"]),
            &FxHashMap::default(),
            "string::substr".to_string(),
            &substr,
        ),
    );
    module.insert(
        "index_of".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "substring"]),
            &FxHashMap::default(),
            "string::index_of".to_string(),
            &index_of,
        ),
    );
    module.insert(
        "starts_with".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "prefix"]),
            &FxHashMap::default(),
            "string::starts_with".to_string(),
            &starts_with,
        ),
    );
    module.insert(
        "ends_with".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "suffix"]),
            &FxHashMap::default(),
            "string::ends_with".to_string(),
            &ends_with,
        ),
    );
    module.insert(
        "repeat".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "count"]),
            &FxHashMap::default(),
            "string::repeat".to_string(),
            &repeat,
        ),
    );
    module.insert(
        "pad_left".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "length", "pad_char"]),
            &FxHashMap::default(),
            "string::pad_left".to_string(),
            &pad_left,
        ),
    );
    module.insert(
        "pad_right".to_string(),
        wrap_native_function(
            &super::build_string_tuple(&["string", "length", "pad_char"]),
            &FxHashMap::default(),
            "string::pad_right".to_string(),
            &pad_right,
        ),
    );

    build_dict(module)
}
