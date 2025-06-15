use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
    GC,
};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

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

    // length 函数
    let mut length_params = IndexMap::new();
    length_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to get length".to_string().into())).stabilize(),
    );
    module.insert(
        "length".to_string(),
        wrap_native_function(
            &build_named_dict(length_params),
            None,
            None,
            "string::length".to_string(),
            &length,
        ),
    );

    // trim 函数
    let mut trim_params = IndexMap::new();
    trim_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to trim".to_string().into())).stabilize(),
    );
    module.insert(
        "trim".to_string(),
        wrap_native_function(
            &build_named_dict(trim_params),
            None,
            None,
            "string::trim".to_string(),
            &trim,
        ),
    );

    // uppercase 函数
    let mut uppercase_params = IndexMap::new();
    uppercase_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to convert to uppercase".to_string().into()))
            .stabilize(),
    );
    module.insert(
        "uppercase".to_string(),
        wrap_native_function(
            &build_named_dict(uppercase_params),
            None,
            None,
            "string::uppercase".to_string(),
            &uppercase,
        ),
    );

    // lowercase 函数
    let mut lowercase_params = IndexMap::new();
    lowercase_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to convert to lowercase".to_string().into()))
            .stabilize(),
    );
    module.insert(
        "lowercase".to_string(),
        wrap_native_function(
            &build_named_dict(lowercase_params),
            None,
            None,
            "string::lowercase".to_string(),
            &lowercase,
        ),
    );

    // contains 函数
    let mut contains_params = IndexMap::new();
    contains_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to search within".to_string().into())).stabilize(),
    );
    contains_params.insert(
        "substring".to_string(),
        OnionObject::Undefined(Some("Substring to search for".to_string().into())).stabilize(),
    );
    module.insert(
        "contains".to_string(),
        wrap_native_function(
            &build_named_dict(contains_params),
            None,
            None,
            "string::contains".to_string(),
            &contains,
        ),
    );

    // concat 函数
    let mut concat_params = IndexMap::new();
    concat_params.insert(
        "a".to_string(),
        OnionObject::Undefined(Some("First string to concatenate".to_string().into())).stabilize(),
    );
    concat_params.insert(
        "b".to_string(),
        OnionObject::Undefined(Some("Second string to concatenate".to_string().into())).stabilize(),
    );
    module.insert(
        "concat".to_string(),
        wrap_native_function(
            &build_named_dict(concat_params),
            None,
            None,
            "string::concat".to_string(),
            &concat,
        ),
    );

    // split 函数
    let mut split_params = IndexMap::new();
    split_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to split".to_string().into())).stabilize(),
    );
    split_params.insert(
        "delimiter".to_string(),
        OnionObject::Undefined(Some("Delimiter to split by".to_string().into())).stabilize(),
    );
    module.insert(
        "split".to_string(),
        wrap_native_function(
            &build_named_dict(split_params),
            None,
            None,
            "string::split".to_string(),
            &split,
        ),
    );

    // replace 函数
    let mut replace_params = IndexMap::new();
    replace_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to perform replacement on".to_string().into()))
            .stabilize(),
    );
    replace_params.insert(
        "from".to_string(),
        OnionObject::Undefined(Some("Substring to replace".to_string().into())).stabilize(),
    );
    replace_params.insert(
        "to".to_string(),
        OnionObject::Undefined(Some("Replacement string".to_string().into())).stabilize(),
    );
    module.insert(
        "replace".to_string(),
        wrap_native_function(
            &build_named_dict(replace_params),
            None,
            None,
            "string::replace".to_string(),
            &replace,
        ),
    );

    // substr 函数
    let mut substr_params = IndexMap::new();
    substr_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to extract substring from".to_string().into()))
            .stabilize(),
    );
    substr_params.insert(
        "start".to_string(),
        OnionObject::Undefined(Some("Start index".to_string().into())).stabilize(),
    );
    substr_params.insert(
        "length".to_string(),
        OnionObject::Undefined(Some("Length of substring".to_string().into())).stabilize(),
    );
    module.insert(
        "substr".to_string(),
        wrap_native_function(
            &build_named_dict(substr_params),
            None,
            None,
            "string::substr".to_string(),
            &substr,
        ),
    );

    // index_of 函数
    let mut index_of_params = IndexMap::new();
    index_of_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to search in".to_string().into())).stabilize(),
    );
    index_of_params.insert(
        "substring".to_string(),
        OnionObject::Undefined(Some("Substring to find".to_string().into())).stabilize(),
    );
    module.insert(
        "index_of".to_string(),
        wrap_native_function(
            &build_named_dict(index_of_params),
            None,
            None,
            "string::index_of".to_string(),
            &index_of,
        ),
    );

    // starts_with 函数
    let mut starts_with_params = IndexMap::new();
    starts_with_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to check".to_string().into())).stabilize(),
    );
    starts_with_params.insert(
        "prefix".to_string(),
        OnionObject::Undefined(Some("Prefix to check for".to_string().into())).stabilize(),
    );
    module.insert(
        "starts_with".to_string(),
        wrap_native_function(
            &build_named_dict(starts_with_params),
            None,
            None,
            "string::starts_with".to_string(),
            &starts_with,
        ),
    );

    // ends_with 函数
    let mut ends_with_params = IndexMap::new();
    ends_with_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to check".to_string().into())).stabilize(),
    );
    ends_with_params.insert(
        "suffix".to_string(),
        OnionObject::Undefined(Some("Suffix to check for".to_string().into())).stabilize(),
    );
    module.insert(
        "ends_with".to_string(),
        wrap_native_function(
            &build_named_dict(ends_with_params),
            None,
            None,
            "string::ends_with".to_string(),
            &ends_with,
        ),
    );

    // repeat 函数
    let mut repeat_params = IndexMap::new();
    repeat_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to repeat".to_string().into())).stabilize(),
    );
    repeat_params.insert(
        "count".to_string(),
        OnionObject::Undefined(Some("Number of times to repeat".to_string().into())).stabilize(),
    );
    module.insert(
        "repeat".to_string(),
        wrap_native_function(
            &build_named_dict(repeat_params),
            None,
            None,
            "string::repeat".to_string(),
            &repeat,
        ),
    );

    // pad_left 函数
    let mut pad_left_params = IndexMap::new();
    pad_left_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to pad".to_string().into())).stabilize(),
    );
    pad_left_params.insert(
        "length".to_string(),
        OnionObject::Undefined(Some("Target length".to_string().into())).stabilize(),
    );
    pad_left_params.insert(
        "pad_char".to_string(),
        OnionObject::Undefined(Some("Character to pad with".to_string().into())).stabilize(),
    );
    module.insert(
        "pad_left".to_string(),
        wrap_native_function(
            &build_named_dict(pad_left_params),
            None,
            None,
            "string::pad_left".to_string(),
            &pad_left,
        ),
    );

    // pad_right 函数
    let mut pad_right_params = IndexMap::new();
    pad_right_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to pad".to_string().into())).stabilize(),
    );
    pad_right_params.insert(
        "length".to_string(),
        OnionObject::Undefined(Some("Target length".to_string().into())).stabilize(),
    );
    pad_right_params.insert(
        "pad_char".to_string(),
        OnionObject::Undefined(Some("Character to pad with".to_string().into())).stabilize(),
    );
    module.insert(
        "pad_right".to_string(),
        wrap_native_function(
            &build_named_dict(pad_right_params),
            None,
            None,
            "string::pad_right".to_string(),
            &pad_right,
        ),
    );

    // is_empty 函数
    let mut is_empty_params = IndexMap::new();
    is_empty_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to check if empty".to_string().into())).stabilize(),
    );
    module.insert(
        "is_empty".to_string(),
        wrap_native_function(
            &build_named_dict(is_empty_params),
            None,
            None,
            "string::is_empty".to_string(),
            &is_empty,
        ),
    );

    // reverse 函数
    let mut reverse_params = IndexMap::new();
    reverse_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to reverse".to_string().into())).stabilize(),
    );
    module.insert(
        "reverse".to_string(),
        wrap_native_function(
            &build_named_dict(reverse_params),
            None,
            None,
            "string::reverse".to_string(),
            &reverse,
        ),
    );

    build_named_dict(module)
}
