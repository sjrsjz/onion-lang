use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
    GC,
};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

/// Get the length of bytes
fn length(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        bytes.weak().with_data(|bytes_data| match bytes_data {
            OnionObject::Bytes(b) => Ok(OnionObject::Integer(b.len() as i64).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "length requires bytes".to_string().into(),
            )),
        })
    })
}

/// Concatenate two byte arrays
fn concat(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let a = get_attr_direct(data, "a".to_string())?;
        let b = get_attr_direct(data, "b".to_string())?;

        a.weak().with_data(|a_data| {
            b.weak().with_data(|b_data| match (a_data, b_data) {
                (OnionObject::Bytes(b1), OnionObject::Bytes(b2)) => {
                    let mut result = b1.as_ref().clone();
                    result.extend_from_slice(b2);
                    Ok(OnionObject::Bytes(result.into()).stabilize())
                }
                _ => Err(RuntimeError::InvalidOperation(
                    "concat requires bytes arguments".to_string().into(),
                )),
            })
        })
    })
}

/// Get a slice of bytes from start to start+length
fn slice(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let start = get_attr_direct(data, "start".to_string())?;
        let length = get_attr_direct(data, "length".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            start.weak().with_data(|start_data| {
                length.weak().with_data(|length_data| {
                    match (bytes_data, start_data, length_data) {
                        (
                            OnionObject::Bytes(b),
                            OnionObject::Integer(start_idx),
                            OnionObject::Integer(len),
                        ) => {
                            let start_idx = *start_idx as usize;
                            let len = *len as usize;

                            if start_idx >= b.len() {
                                Ok(OnionObject::Bytes(Vec::new().into()).stabilize())
                            } else {
                                let end_idx = std::cmp::min(start_idx + len, b.len());
                                let result = b[start_idx..end_idx].to_vec();
                                Ok(OnionObject::Bytes(result.into()).stabilize())
                            }
                        }
                        _ => Err(RuntimeError::InvalidOperation(
                            "slice requires bytes and integer arguments"
                                .to_string()
                                .into(),
                        )),
                    }
                })
            })
        })
    })
}

/// Get byte at specific index
fn get_at(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let index = get_attr_direct(data, "index".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            index.weak().with_data(|index_data| match (bytes_data, index_data) {
                (OnionObject::Bytes(b), OnionObject::Integer(idx)) => {
                    let idx = *idx as usize;
                    if idx >= b.len() {
                        Err(RuntimeError::InvalidOperation(
                            "index out of bounds".to_string().into(),
                        ))
                    } else {
                        Ok(OnionObject::Integer(b[idx] as i64).stabilize())
                    }
                }
                _ => Err(RuntimeError::InvalidOperation(
                    "get_at requires bytes and integer arguments"
                        .to_string()
                        .into(),
                )),
            })
        })
    })
}

/// Set byte at specific index (returns new bytes with modified value)
fn set_at(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let index = get_attr_direct(data, "index".to_string())?;
        let value = get_attr_direct(data, "value".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            index.weak().with_data(|index_data| {
                value.weak().with_data(|value_data| {
                    match (bytes_data, index_data, value_data) {
                        (
                            OnionObject::Bytes(b),
                            OnionObject::Integer(idx),
                            OnionObject::Integer(val),
                        ) => {
                            let idx = *idx as usize;
                            let val = *val as u8;
                            if idx >= b.len() {
                                Err(RuntimeError::InvalidOperation(
                                    "index out of bounds".to_string().into(),
                                ))
                            } else {
                                // 创建新的副本而不是修改原始数据，避免UB行为
                                let mut result = b.as_ref().clone();
                                result[idx] = val;
                                Ok(OnionObject::Bytes(result.into()).stabilize())
                            }
                        }
                        _ => Err(RuntimeError::InvalidOperation(
                            "set_at requires bytes and integer arguments"
                                .to_string()
                                .into(),
                        )),
                    }
                })
            })
        })
    })
}

/// Find the index of a byte sequence
fn index_of(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let pattern = get_attr_direct(data, "pattern".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            pattern
                .weak()
                .with_data(|pattern_data| match (bytes_data, pattern_data) {
                    (OnionObject::Bytes(b), OnionObject::Bytes(pat)) => {
                        if pat.is_empty() {
                            return Ok(OnionObject::Integer(0).stabilize());
                        }
                        
                        for i in 0..=b.len().saturating_sub(pat.len()) {
                            if &b[i..i + pat.len()] == pat.as_ref() {
                                return Ok(OnionObject::Integer(i as i64).stabilize());
                            }
                        }
                        Ok(OnionObject::Integer(-1).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "index_of requires bytes arguments".to_string().into(),
                    )),
                })
        })
    })
}

/// Check if bytes contains a pattern
fn contains(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let pattern = get_attr_direct(data, "pattern".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            pattern
                .weak()
                .with_data(|pattern_data| match (bytes_data, pattern_data) {
                    (OnionObject::Bytes(b), OnionObject::Bytes(pat)) => {
                        if pat.is_empty() {
                            return Ok(OnionObject::Boolean(true).stabilize());
                        }
                        
                        for i in 0..=b.len().saturating_sub(pat.len()) {
                            if &b[i..i + pat.len()] == pat.as_ref() {
                                return Ok(OnionObject::Boolean(true).stabilize());
                            }
                        }
                        Ok(OnionObject::Boolean(false).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "contains requires bytes arguments".to_string().into(),
                    )),
                })
        })
    })
}

/// Check if bytes starts with a pattern
fn starts_with(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let pattern = get_attr_direct(data, "pattern".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            pattern
                .weak()
                .with_data(|pattern_data| match (bytes_data, pattern_data) {
                    (OnionObject::Bytes(b), OnionObject::Bytes(pat)) => {
                        Ok(OnionObject::Boolean(b.starts_with(pat)).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "starts_with requires bytes arguments".to_string().into(),
                    )),
                })
        })
    })
}

/// Check if bytes ends with a pattern
fn ends_with(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let pattern = get_attr_direct(data, "pattern".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            pattern
                .weak()
                .with_data(|pattern_data| match (bytes_data, pattern_data) {
                    (OnionObject::Bytes(b), OnionObject::Bytes(pat)) => {
                        Ok(OnionObject::Boolean(b.ends_with(pat)).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "ends_with requires bytes arguments".to_string().into(),
                    )),
                })
        })
    })
}

/// Repeat bytes n times
fn repeat(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let count = get_attr_direct(data, "count".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            count
                .weak()
                .with_data(|count_data| match (bytes_data, count_data) {
                    (OnionObject::Bytes(b), OnionObject::Integer(n)) => {
                        if *n < 0 {
                            return Err(RuntimeError::InvalidOperation(
                                "repeat count cannot be negative".to_string().into(),
                            ));
                        }
                        let mut result = Vec::new();
                        for _ in 0..*n {
                            result.extend_from_slice(b);
                        }
                        Ok(OnionObject::Bytes(result.into()).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "repeat requires bytes and integer arguments"
                            .to_string()
                            .into(),
                    )),
                })
        })
    })
}

/// Check if bytes is empty
fn is_empty(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        bytes.weak().with_data(|bytes_data| match bytes_data {
            OnionObject::Bytes(b) => Ok(OnionObject::Boolean(b.is_empty()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "is_empty requires bytes".to_string().into(),
            )),
        })
    })
}

/// Reverse bytes
fn reverse(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        bytes.weak().with_data(|bytes_data| match bytes_data {
            OnionObject::Bytes(b) => {
                let mut result = b.as_ref().clone();
                result.reverse();
                Ok(OnionObject::Bytes(result.into()).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "reverse requires bytes".to_string().into(),
            )),
        })
    })
}

/// Convert bytes to string using UTF-8 encoding
fn to_string(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        bytes.weak().with_data(|bytes_data| match bytes_data {
            OnionObject::Bytes(b) => {
                match String::from_utf8(b.as_ref().clone()) {
                    Ok(s) => Ok(OnionObject::String(s.into()).stabilize()),
                    Err(_) => Err(RuntimeError::InvalidOperation(
                        "bytes is not valid UTF-8".to_string().into(),
                    )),
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "to_string requires bytes".to_string().into(),
            )),
        })
    })
}

/// Convert string to bytes using UTF-8 encoding
fn from_string(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let string = get_attr_direct(data, "string".to_string())?;
        string.weak().with_data(|string_data| match string_data {
            OnionObject::String(s) => {
                let bytes = s.as_bytes().to_vec();
                Ok(OnionObject::Bytes(bytes.into()).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "from_string requires string".to_string().into(),
            )),
        })
    })
}

/// Pad bytes on the left with specified byte value
fn pad_left(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let length = get_attr_direct(data, "length".to_string())?;
        let pad_byte = get_attr_direct(data, "pad_byte".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            length.weak().with_data(|length_data| {
                pad_byte.weak().with_data(|pad_byte_data| {
                    match (bytes_data, length_data, pad_byte_data) {
                        (
                            OnionObject::Bytes(b),
                            OnionObject::Integer(len),
                            OnionObject::Integer(pad),
                        ) => {
                            let target_len = *len as usize;
                            let pad_byte = *pad as u8;
                            if b.len() >= target_len {
                                Ok(OnionObject::Bytes(b.clone()).stabilize())
                            } else {
                                let pad_count = target_len - b.len();
                                let mut result = vec![pad_byte; pad_count];
                                result.extend_from_slice(b);
                                Ok(OnionObject::Bytes(result.into()).stabilize())
                            }
                        }
                        _ => Err(RuntimeError::InvalidOperation(
                            "pad_left requires bytes and integer arguments"
                                .to_string()
                                .into(),
                        )),
                    }
                })
            })
        })
    })
}

/// Pad bytes on the right with specified byte value
fn pad_right(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        let length = get_attr_direct(data, "length".to_string())?;
        let pad_byte = get_attr_direct(data, "pad_byte".to_string())?;

        bytes.weak().with_data(|bytes_data| {
            length.weak().with_data(|length_data| {
                pad_byte.weak().with_data(|pad_byte_data| {
                    match (bytes_data, length_data, pad_byte_data) {
                        (
                            OnionObject::Bytes(b),
                            OnionObject::Integer(len),
                            OnionObject::Integer(pad),
                        ) => {
                            let target_len = *len as usize;
                            let pad_byte = *pad as u8;
                            if b.len() >= target_len {
                                Ok(OnionObject::Bytes(b.clone()).stabilize())
                            } else {
                                let pad_count = target_len - b.len();
                                let mut result = b.as_ref().clone();
                                result.extend(vec![pad_byte; pad_count]);
                                Ok(OnionObject::Bytes(result.into()).stabilize())
                            }
                        }
                        _ => Err(RuntimeError::InvalidOperation(
                            "pad_right requires bytes and integer arguments"
                                .to_string()
                                .into(),
                        )),
                    }
                })
            })
        })
    })
}

/// Create bytes from a list of integers
fn from_integers(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let list = get_attr_direct(data, "list".to_string())?;
        list.weak().with_data(|list_data| match list_data {
            OnionObject::Tuple(t) => {
                let mut result = Vec::new();
                for item in t.get_elements() {
                    item.with_data(|item_data| match item_data {
                        OnionObject::Integer(i) => {
                            if *i < 0 || *i > 255 {
                                Err(RuntimeError::InvalidOperation(
                                    "byte value must be between 0 and 255".to_string().into(),
                                ))
                            } else {
                                result.push(*i as u8);
                                Ok(())
                            }
                        }
                        _ => Err(RuntimeError::InvalidOperation(
                            "list must contain only integers".to_string().into(),
                        )),
                    })?;
                }
                Ok(OnionObject::Bytes(result.into()).stabilize())
            }
            _ => Err(RuntimeError::InvalidOperation(
                "from_integers requires tuple argument".to_string().into(),
            )),
        })
    })
}

/// Convert bytes to a list of integers
fn to_integers(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    use onion_vm::types::tuple::OnionTuple;
    
    argument.weak().with_data(|data| {
        let bytes = get_attr_direct(data, "bytes".to_string())?;
        bytes.weak().with_data(|bytes_data| match bytes_data {
            OnionObject::Bytes(b) => {
                let integers: Vec<_> = b
                    .iter()
                    .map(|&byte| OnionObject::Integer(byte as i64).stabilize())
                    .collect();
                Ok(OnionTuple::new_static_no_ref(&integers))
            }
            _ => Err(RuntimeError::InvalidOperation(
                "to_integers requires bytes".to_string().into(),
            )),
        })
    })
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // length 函数
    let mut length_params = IndexMap::new();
    length_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to get length".to_string().into())).stabilize(),
    );
    module.insert(
        "length".to_string(),
        wrap_native_function(
            &build_named_dict(length_params),
            None,
            None,
            "bytes::length".to_string(),
            &length,
        ),
    );

    // concat 函数
    let mut concat_params = IndexMap::new();
    concat_params.insert(
        "a".to_string(),
        OnionObject::Undefined(Some("First bytes to concatenate".to_string().into())).stabilize(),
    );
    concat_params.insert(
        "b".to_string(),
        OnionObject::Undefined(Some("Second bytes to concatenate".to_string().into())).stabilize(),
    );
    module.insert(
        "concat".to_string(),
        wrap_native_function(
            &build_named_dict(concat_params),
            None,
            None,
            "bytes::concat".to_string(),
            &concat,
        ),
    );

    // slice 函数
    let mut slice_params = IndexMap::new();
    slice_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to slice".to_string().into())).stabilize(),
    );
    slice_params.insert(
        "start".to_string(),
        OnionObject::Undefined(Some("Start index".to_string().into())).stabilize(),
    );
    slice_params.insert(
        "length".to_string(),
        OnionObject::Undefined(Some("Length of slice".to_string().into())).stabilize(),
    );
    module.insert(
        "slice".to_string(),
        wrap_native_function(
            &build_named_dict(slice_params),
            None,
            None,
            "bytes::slice".to_string(),
            &slice,
        ),
    );

    // get_at 函数
    let mut get_at_params = IndexMap::new();
    get_at_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to get from".to_string().into())).stabilize(),
    );
    get_at_params.insert(
        "index".to_string(),
        OnionObject::Undefined(Some("Index to get byte from".to_string().into())).stabilize(),
    );
    module.insert(
        "get_at".to_string(),
        wrap_native_function(
            &build_named_dict(get_at_params),
            None,
            None,
            "bytes::get_at".to_string(),
            &get_at,
        ),
    );    // set_at 函数 - 返回新的字节数组
    let mut set_at_params = IndexMap::new();
    set_at_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to modify".to_string().into())).stabilize(),
    );
    set_at_params.insert(
        "index".to_string(),
        OnionObject::Undefined(Some("Index to set byte at".to_string().into())).stabilize(),
    );
    set_at_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Byte value to set (0-255)".to_string().into())).stabilize(),
    );
    module.insert(
        "set_at".to_string(),
        wrap_native_function(
            &build_named_dict(set_at_params),
            None,
            None,
            "bytes::set_at".to_string(),
            &set_at,
        ),
    );

    // index_of 函数
    let mut index_of_params = IndexMap::new();
    index_of_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to search in".to_string().into())).stabilize(),
    );
    index_of_params.insert(
        "pattern".to_string(),
        OnionObject::Undefined(Some("Byte pattern to find".to_string().into())).stabilize(),
    );
    module.insert(
        "index_of".to_string(),
        wrap_native_function(
            &build_named_dict(index_of_params),
            None,
            None,
            "bytes::index_of".to_string(),
            &index_of,
        ),
    );

    // contains 函数
    let mut contains_params = IndexMap::new();
    contains_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to search within".to_string().into())).stabilize(),
    );
    contains_params.insert(
        "pattern".to_string(),
        OnionObject::Undefined(Some("Byte pattern to search for".to_string().into())).stabilize(),
    );
    module.insert(
        "contains".to_string(),
        wrap_native_function(
            &build_named_dict(contains_params),
            None,
            None,
            "bytes::contains".to_string(),
            &contains,
        ),
    );

    // starts_with 函数
    let mut starts_with_params = IndexMap::new();
    starts_with_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to check".to_string().into())).stabilize(),
    );
    starts_with_params.insert(
        "pattern".to_string(),
        OnionObject::Undefined(Some("Pattern to check for".to_string().into())).stabilize(),
    );
    module.insert(
        "starts_with".to_string(),
        wrap_native_function(
            &build_named_dict(starts_with_params),
            None,
            None,
            "bytes::starts_with".to_string(),
            &starts_with,
        ),
    );

    // ends_with 函数
    let mut ends_with_params = IndexMap::new();
    ends_with_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to check".to_string().into())).stabilize(),
    );
    ends_with_params.insert(
        "pattern".to_string(),
        OnionObject::Undefined(Some("Pattern to check for".to_string().into())).stabilize(),
    );
    module.insert(
        "ends_with".to_string(),
        wrap_native_function(
            &build_named_dict(ends_with_params),
            None,
            None,
            "bytes::ends_with".to_string(),
            &ends_with,
        ),
    );

    // repeat 函数
    let mut repeat_params = IndexMap::new();
    repeat_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to repeat".to_string().into())).stabilize(),
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
            "bytes::repeat".to_string(),
            &repeat,
        ),
    );

    // is_empty 函数
    let mut is_empty_params = IndexMap::new();
    is_empty_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to check if empty".to_string().into())).stabilize(),
    );
    module.insert(
        "is_empty".to_string(),
        wrap_native_function(
            &build_named_dict(is_empty_params),
            None,
            None,
            "bytes::is_empty".to_string(),
            &is_empty,
        ),
    );

    // reverse 函数
    let mut reverse_params = IndexMap::new();
    reverse_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to reverse".to_string().into())).stabilize(),
    );
    module.insert(
        "reverse".to_string(),
        wrap_native_function(
            &build_named_dict(reverse_params),
            None,
            None,
            "bytes::reverse".to_string(),
            &reverse,
        ),
    );

    // to_string 函数
    let mut to_string_params = IndexMap::new();
    to_string_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to convert to string".to_string().into())).stabilize(),
    );
    module.insert(
        "to_string".to_string(),
        wrap_native_function(
            &build_named_dict(to_string_params),
            None,
            None,
            "bytes::to_string".to_string(),
            &to_string,
        ),
    );

    // from_string 函数
    let mut from_string_params = IndexMap::new();
    from_string_params.insert(
        "string".to_string(),
        OnionObject::Undefined(Some("String to convert to bytes".to_string().into())).stabilize(),
    );
    module.insert(
        "from_string".to_string(),
        wrap_native_function(
            &build_named_dict(from_string_params),
            None,
            None,
            "bytes::from_string".to_string(),
            &from_string,
        ),
    );

    // pad_left 函数
    let mut pad_left_params = IndexMap::new();
    pad_left_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to pad".to_string().into())).stabilize(),
    );
    pad_left_params.insert(
        "length".to_string(),
        OnionObject::Undefined(Some("Target length".to_string().into())).stabilize(),
    );
    pad_left_params.insert(
        "pad_byte".to_string(),
        OnionObject::Undefined(Some("Byte value to pad with (0-255)".to_string().into())).stabilize(),
    );
    module.insert(
        "pad_left".to_string(),
        wrap_native_function(
            &build_named_dict(pad_left_params),
            None,
            None,
            "bytes::pad_left".to_string(),
            &pad_left,
        ),
    );

    // pad_right 函数
    let mut pad_right_params = IndexMap::new();
    pad_right_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to pad".to_string().into())).stabilize(),
    );
    pad_right_params.insert(
        "length".to_string(),
        OnionObject::Undefined(Some("Target length".to_string().into())).stabilize(),
    );
    pad_right_params.insert(
        "pad_byte".to_string(),
        OnionObject::Undefined(Some("Byte value to pad with (0-255)".to_string().into())).stabilize(),
    );
    module.insert(
        "pad_right".to_string(),
        wrap_native_function(
            &build_named_dict(pad_right_params),
            None,
            None,
            "bytes::pad_right".to_string(),
            &pad_right,
        ),
    );

    // from_integers 函数
    let mut from_integers_params = IndexMap::new();
    from_integers_params.insert(
        "list".to_string(),
        OnionObject::Undefined(Some("Tuple of integers (0-255) to convert to bytes".to_string().into())).stabilize(),
    );
    module.insert(
        "from_integers".to_string(),
        wrap_native_function(
            &build_named_dict(from_integers_params),
            None,
            None,
            "bytes::from_integers".to_string(),
            &from_integers,
        ),
    );

    // to_integers 函数
    let mut to_integers_params = IndexMap::new();
    to_integers_params.insert(
        "bytes".to_string(),
        OnionObject::Undefined(Some("Bytes to convert to integers".to_string().into())).stabilize(),
    );
    module.insert(
        "to_integers".to_string(),
        wrap_native_function(
            &build_named_dict(to_integers_params),
            None,
            None,
            "bytes::to_integers".to_string(),
            &to_integers,
        ),
    );

    build_named_dict(module)
}
