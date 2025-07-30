use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
};
use rustc_hash::FxHashMap;

use crate::stdlib::build_string_tuple;

use super::{build_dict, get_attr_direct, wrap_native_function};

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
                length
                    .weak()
                    .with_data(|length_data| match (bytes_data, start_data, length_data) {
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
            index
                .weak()
                .with_data(|index_data| match (bytes_data, index_data) {
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
            OnionObject::Bytes(b) => match String::from_utf8(b.as_ref().clone()) {
                Ok(s) => Ok(OnionObject::String(s.into()).stabilize()),
                Err(_) => Err(RuntimeError::InvalidOperation(
                    "bytes is not valid UTF-8".to_string().into(),
                )),
            },
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

    module.insert(
        "length".to_string(),
        wrap_native_function(
            &OnionObject::String("bytes".to_string().into()).stabilize(),
            &FxHashMap::default(),
            "bytes::length".to_string(),
            &length,
        ),
    );

    module.insert(
        "concat".to_string(),
        wrap_native_function(
            &build_string_tuple(&["a", "b"]),
            &FxHashMap::default(),
            "bytes::concat".to_string(),
            &concat,
        ),
    );

    module.insert(
        "slice".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "start", "length"]),
            &FxHashMap::default(),
            "bytes::slice".to_string(),
            &slice,
        ),
    );

    module.insert(
        "get_at".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "index"]),
            &FxHashMap::default(),
            "bytes::get_at".to_string(),
            &get_at,
        ),
    ); 

    module.insert(
        "set_at".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "index", "value"]),
            &FxHashMap::default(),
            "bytes::set_at".to_string(),
            &set_at,
        ),
    );


    module.insert(
        "index_of".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "pattern"]),
            &FxHashMap::default(),
            "bytes::index_of".to_string(),
            &index_of,
        ),
    );

    module.insert(
        "contains".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "pattern"]),
            &FxHashMap::default(),
            "bytes::contains".to_string(),
            &contains,
        ),
    );

    module.insert(
        "starts_with".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "pattern"]),
            &FxHashMap::default(),
            "bytes::starts_with".to_string(),
            &starts_with,
        ),
    );

    module.insert(
        "ends_with".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "pattern"]),
            &FxHashMap::default(),
            "bytes::ends_with".to_string(),
            &ends_with,
        ),
    );

    module.insert(
        "repeat".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "count"]),
            &FxHashMap::default(),
            "bytes::repeat".to_string(),
            &repeat,
        ),
    );

    module.insert(
        "is_empty".to_string(),
        wrap_native_function(
            &OnionObject::String("bytes".to_string().into()).stabilize(),
            &FxHashMap::default(),
            "bytes::is_empty".to_string(),
            &is_empty,
        ),
    );

    module.insert(
        "reverse".to_string(),
        wrap_native_function(
            &OnionObject::String("bytes".to_string().into()).stabilize(),
            &FxHashMap::default(),
            "bytes::reverse".to_string(),
            &reverse,
        ),
    );

    module.insert(
        "to_string".to_string(),
        wrap_native_function(
            &OnionObject::String("bytes".to_string().into()).stabilize(),
            &FxHashMap::default(),
            "bytes::to_string".to_string(),
            &to_string,
        ),
    );

    module.insert(
        "from_string".to_string(),
        wrap_native_function(
            &OnionObject::String("string".to_string().into()).stabilize(),
            &FxHashMap::default(),
            "bytes::from_string".to_string(),
            &from_string,
        ),
    );

    module.insert(
        "pad_left".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "length", "pad_byte"]),
            &FxHashMap::default(),
            "bytes::pad_left".to_string(),
            &pad_left,
        ),
    );

    module.insert(
        "pad_right".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "length", "pad_byte"]),
            &FxHashMap::default(),
            "bytes::pad_right".to_string(),
            &pad_right,
        ),
    );


    module.insert(
        "from_integers".to_string(),
        wrap_native_function(
            &OnionObject::String("list".to_string().into()).stabilize(), // 这里不使用build_string_tuple是因为我们期望其能直接接收一个元组而非通过单元素元组传入
            &FxHashMap::default(),
            "bytes::from_integers".to_string(),
            &from_integers,
        ),
    );

    module.insert(
        "to_integers".to_string(),
        wrap_native_function(
            &OnionObject::String("bytes".to_string().into()).stabilize(),
            &FxHashMap::default(),
            "bytes::to_integers".to_string(),
            &to_integers,
        ),
    );

    build_dict(module)
}
