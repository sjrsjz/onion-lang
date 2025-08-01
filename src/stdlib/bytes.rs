use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

// 引入所需的辅助函数
use crate::stdlib::{build_dict, build_string_tuple, wrap_native_function};

// --- Helper functions for robust argument parsing ---
fn get_bytes_arg<'a>(
    argument: &'a OnionFastMap<String, OnionStaticObject>,
    name: &str,
) -> Result<&'a [u8], RuntimeError> {
    let obj = argument.get(&name.to_string()).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires a '{name}' argument")
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::Bytes(b) => Ok(b.as_ref()),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be bytes")
                .to_string()
                .into(),
        )),
    }
}

fn get_integer_arg(
    argument: &OnionFastMap<String, OnionStaticObject>,
    name: &str,
) -> Result<i64, RuntimeError> {
    let obj = argument.get(&name.to_string()).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires an '{name}' argument")
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::Integer(i) => Ok(*i),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be an integer")
                .to_string()
                .into(),
        )),
    }
}

fn get_integer_tuple_arg(
    argument: &OnionFastMap<String, OnionStaticObject>,
    name: &str,
) -> Result<Vec<i64>, RuntimeError> {
    let obj = argument.get(&name.to_string()).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires a '{name}' argument")
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::Tuple(tuple) => tuple
            .get_elements()
            .iter()
            .map(|item| match item {
                OnionObject::Integer(i) => Ok(*i),
                _ => Err(RuntimeError::InvalidType(
                    "All elements in the list must be integers"
                        .to_string()
                        .into(),
                )),
            })
            .collect(),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be a tuple of integers")
                .to_string()
                .into(),
        )),
    }
}

// --- Refactored Native Functions ---

fn length(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    Ok(OnionObject::Integer(bytes.len() as i64).stabilize())
}

fn concat(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let a = get_bytes_arg(argument, "a")?;
    let b = get_bytes_arg(argument, "b")?;
    let result = [a, b].concat();
    Ok(OnionObject::Bytes(result.into()).stabilize())
}

fn slice(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let start = get_integer_arg(argument, "start")?;
    let length = get_integer_arg(argument, "length")?;

    if start < 0 || length < 0 {
        return Err(RuntimeError::InvalidOperation(
            "start and length for slice cannot be negative"
                .to_string()
                .into(),
        ));
    }
    let start_idx = start as usize;
    let len = length as usize;

    let end_idx = std::cmp::min(start_idx.saturating_add(len), bytes.len());
    let start_idx_clamped = std::cmp::min(start_idx, bytes.len());

    let result = bytes[start_idx_clamped..end_idx].to_vec();
    Ok(OnionObject::Bytes(result.into()).stabilize())
}

fn get_at(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let index = get_integer_arg(argument, "index")?;
    if index < 0 {
        return Err(RuntimeError::InvalidOperation(
            "Index cannot be negative".to_string().into(),
        ));
    }
    bytes
        .get(index as usize)
        .map(|&byte| OnionObject::Integer(byte as i64).stabilize())
        .ok_or_else(|| RuntimeError::InvalidOperation("Index out of bounds".to_string().into()))
}

fn set_at(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let index = get_integer_arg(argument, "index")?;
    let value = get_integer_arg(argument, "value")?;

    if index < 0 {
        return Err(RuntimeError::InvalidOperation(
            "Index cannot be negative".to_string().into(),
        ));
    }
    if !(0..=255).contains(&value) {
        return Err(RuntimeError::InvalidOperation(
            "Value must be a valid byte (0-255)".to_string().into(),
        ));
    }

    let idx = index as usize;
    if idx >= bytes.len() {
        return Err(RuntimeError::InvalidOperation(
            "Index out of bounds".to_string().into(),
        ));
    }

    let mut result = bytes.to_vec();
    result[idx] = value as u8;
    Ok(OnionObject::Bytes(result.into()).stabilize())
}

fn index_of(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let pattern = get_bytes_arg(argument, "pattern")?;

    let index = bytes
        .windows(pattern.len())
        .position(|window| window == pattern)
        .map(|i| i as i64)
        .unwrap_or(-1);

    Ok(OnionObject::Integer(index).stabilize())
}

fn contains(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let pattern = get_bytes_arg(argument, "pattern")?;
    Ok(
        OnionObject::Boolean(bytes.windows(pattern.len()).any(|window| window == pattern))
            .stabilize(),
    )
}

fn starts_with(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let pattern = get_bytes_arg(argument, "pattern")?;
    Ok(OnionObject::Boolean(bytes.starts_with(pattern)).stabilize())
}

fn ends_with(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let pattern = get_bytes_arg(argument, "pattern")?;
    Ok(OnionObject::Boolean(bytes.ends_with(pattern)).stabilize())
}

fn repeat(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let count = get_integer_arg(argument, "count")?;

    if count < 0 {
        return Err(RuntimeError::InvalidOperation(
            "Repeat count cannot be negative".to_string().into(),
        ));
    }

    Ok(OnionObject::Bytes(bytes.repeat(count as usize).into()).stabilize())
}

fn is_empty(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    Ok(OnionObject::Boolean(bytes.is_empty()).stabilize())
}

fn reverse(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let mut result = bytes.to_vec();
    result.reverse();
    Ok(OnionObject::Bytes(result.into()).stabilize())
}

fn to_string(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    String::from_utf8(bytes.to_vec())
        .map(|s| OnionObject::String(s.into()).stabilize())
        .map_err(|_| RuntimeError::InvalidOperation("Bytes are not valid UTF-8".to_string().into()))
}

fn from_string(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let obj = argument.get(&"string".to_string()).ok_or_else(|| {
        RuntimeError::DetailedError("Function requires a 'string' argument".to_string().into())
    })?;
    match obj.weak() {
        OnionObject::String(s) => Ok(OnionObject::Bytes(s.as_bytes().to_vec().into()).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'string' must be a string".to_string().into(),
        )),
    }
}

fn pad_left(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let length = get_integer_arg(argument, "length")?;
    let pad_byte_val = get_integer_arg(argument, "pad_byte")?;

    if length < 0 {
        return Err(RuntimeError::InvalidOperation(
            "Padding length cannot be negative".to_string().into(),
        ));
    }
    if !(0..=255).contains(&pad_byte_val) {
        return Err(RuntimeError::InvalidOperation(
            "pad_byte must be a valid byte (0-255)".to_string().into(),
        ));
    }

    let target_len = length as usize;
    if bytes.len() >= target_len {
        return Ok(OnionObject::Bytes(bytes.to_vec().into()).stabilize());
    }

    let pad_count = target_len - bytes.len();
    let mut result = vec![pad_byte_val as u8; pad_count];
    result.extend_from_slice(bytes);
    Ok(OnionObject::Bytes(result.into()).stabilize())
}

fn pad_right(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let length = get_integer_arg(argument, "length")?;
    let pad_byte_val = get_integer_arg(argument, "pad_byte")?;

    if length < 0 {
        return Err(RuntimeError::InvalidOperation(
            "Padding length cannot be negative".to_string().into(),
        ));
    }
    if !(0..=255).contains(&pad_byte_val) {
        return Err(RuntimeError::InvalidOperation(
            "pad_byte must be a valid byte (0-255)".to_string().into(),
        ));
    }

    let target_len = length as usize;
    if bytes.len() >= target_len {
        return Ok(OnionObject::Bytes(bytes.to_vec().into()).stabilize());
    }

    let pad_count = target_len - bytes.len();
    let mut result = bytes.to_vec();
    result.extend(vec![pad_byte_val as u8; pad_count]);
    Ok(OnionObject::Bytes(result.into()).stabilize())
}

fn from_integers(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let list = get_integer_tuple_arg(argument, "list")?;
    let mut result = Vec::with_capacity(list.len());
    for i in list {
        if !(0..=255).contains(&i) {
            return Err(RuntimeError::InvalidOperation(
                "All integers in the list must be valid bytes (0-255)"
                    .to_string()
                    .into(),
            ));
        }
        result.push(i as u8);
    }
    Ok(OnionObject::Bytes(result.into()).stabilize())
}

fn to_integers(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let bytes = get_bytes_arg(argument, "bytes")?;
    let integers: Vec<_> = bytes
        .iter()
        .map(|&byte| OnionObject::Integer(byte as i64))
        .collect();
    Ok(OnionObject::Tuple(OnionTuple::new(integers).into()).stabilize())
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();
    let bytes_arg = OnionObject::String("bytes".to_string().into()).stabilize();

    module.insert(
        "length".to_string(),
        wrap_native_function(
            &bytes_arg,
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::length".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string()]),
            &length,
        ),
    );
    module.insert(
        "is_empty".to_string(),
        wrap_native_function(
            &bytes_arg,
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::is_empty".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string()]),
            &is_empty,
        ),
    );
    module.insert(
        "reverse".to_string(),
        wrap_native_function(
            &bytes_arg,
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::reverse".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string()]),
            &reverse,
        ),
    );
    module.insert(
        "to_string".to_string(),
        wrap_native_function(
            &bytes_arg,
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::to_string".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string()]),
            &to_string,
        ),
    );
    module.insert(
        "to_integers".to_string(),
        wrap_native_function(
            &bytes_arg,
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::to_integers".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string()]),
            &to_integers,
        ),
    );

    module.insert(
        "from_string".to_string(),
        wrap_native_function(
            &OnionObject::String("string".to_string().into()).stabilize(),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::from_string".to_string(),
            OnionKeyPool::create(vec!["string".to_string()]),
            &from_string,
        ),
    );
    module.insert(
        "from_integers".to_string(),
        wrap_native_function(
            &OnionObject::String("list".to_string().into()).stabilize(),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::from_integers".to_string(),
            OnionKeyPool::create(vec!["list".to_string()]),
            &from_integers,
        ),
    );

    module.insert(
        "concat".to_string(),
        wrap_native_function(
            &build_string_tuple(&["a", "b"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::concat".to_string(),
            OnionKeyPool::create(vec!["a".to_string(), "b".to_string()]),
            &concat,
        ),
    );
    module.insert(
        "slice".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "start", "length"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::slice".to_string(),
            OnionKeyPool::create(vec![
                "bytes".to_string(),
                "start".to_string(),
                "length".to_string(),
            ]),
            &slice,
        ),
    );
    module.insert(
        "get_at".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "index"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::get_at".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string(), "index".to_string()]),
            &get_at,
        ),
    );
    module.insert(
        "set_at".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "index", "value"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::set_at".to_string(),
            OnionKeyPool::create(vec![
                "bytes".to_string(),
                "index".to_string(),
                "value".to_string(),
            ]),
            &set_at,
        ),
    );
    module.insert(
        "index_of".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "pattern"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::index_of".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string(), "pattern".to_string()]),
            &index_of,
        ),
    );
    module.insert(
        "contains".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "pattern"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::contains".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string(), "pattern".to_string()]),
            &contains,
        ),
    );
    module.insert(
        "starts_with".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "pattern"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::starts_with".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string(), "pattern".to_string()]),
            &starts_with,
        ),
    );
    module.insert(
        "ends_with".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "pattern"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::ends_with".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string(), "pattern".to_string()]),
            &ends_with,
        ),
    );
    module.insert(
        "repeat".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "count"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::repeat".to_string(),
            OnionKeyPool::create(vec!["bytes".to_string(), "count".to_string()]),
            &repeat,
        ),
    );
    module.insert(
        "pad_left".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "length", "pad_byte"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::pad_left".to_string(),
            OnionKeyPool::create(vec![
                "bytes".to_string(),
                "length".to_string(),
                "pad_byte".to_string(),
            ]),
            &pad_left,
        ),
    );
    module.insert(
        "pad_right".to_string(),
        wrap_native_function(
            &build_string_tuple(&["bytes", "length", "pad_byte"]),
            &OnionFastMap::new(OnionKeyPool::create(vec![])),
            "bytes::pad_right".to_string(),
            OnionKeyPool::create(vec![
                "bytes".to_string(),
                "length".to_string(),
                "pad_byte".to_string(),
            ]),
            &pad_right,
        ),
    );

    build_dict(module)
}
