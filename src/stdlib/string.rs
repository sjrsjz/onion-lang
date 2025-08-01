use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError, types::{
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    }, utils::fastmap::{OnionFastMap, OnionKeyPool}, GC
};

// 引入所需的辅助函数
use super::{build_dict, build_string_tuple, wrap_native_function};

fn get_string_arg<'a>(
    argument: &'a OnionFastMap<String, OnionStaticObject>,
    name: &str,
) -> Result<&'a str, RuntimeError> {
    let obj = argument.get(&name.to_string()).ok_or_else(|| {
        RuntimeError::DetailedError(
            format!("Function requires a '{name}' argument")
                .to_string()
                .into(),
        )
    })?;
    match obj.weak() {
        OnionObject::String(s) => Ok(s.as_ref()),
        _ => Err(RuntimeError::InvalidType(
            format!("Argument '{name}' must be a string")
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

fn length(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::Integer(s.chars().count() as i64).stabilize())
}

fn trim(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::String(s.trim().to_string().into()).stabilize())
}

fn uppercase(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::String(s.to_uppercase().into()).stabilize())
}

fn lowercase(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::String(s.to_lowercase().into()).stabilize())
}

fn contains(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let substring = get_string_arg(argument, "substring")?;
    Ok(OnionObject::Boolean(string.contains(substring)).stabilize())
}

fn concat(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s1 = get_string_arg(argument, "a")?;
    let s2 = get_string_arg(argument, "b")?;
    let result = [s1, s2].concat();
    Ok(OnionObject::String(result.into()).stabilize())
}

fn split(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let delimiter = get_string_arg(argument, "delimiter")?;
    let parts: Vec<_> = string
        .split(delimiter)
        .map(|part| OnionObject::String(part.to_string().into()))
        .collect();
    Ok(OnionObject::Tuple(OnionTuple::new(parts).into()).stabilize())
}

fn replace(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let from = get_string_arg(argument, "from")?;
    let to = get_string_arg(argument, "to")?;
    let result = string.replace(from, to);
    Ok(OnionObject::String(result.into()).stabilize())
}

fn substr(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let start = get_integer_arg(argument, "start")?;
    let length = get_integer_arg(argument, "length")?;

    if start < 0 || length < 0 {
        return Err(RuntimeError::InvalidOperation(
            "start and length for substr cannot be negative"
                .to_string()
                .into(),
        ));
    }
    let start_idx = start as usize;
    let len = length as usize;

    let result: String = string.chars().skip(start_idx).take(len).collect();
    Ok(OnionObject::String(result.into()).stabilize())
}

fn index_of(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let substring = get_string_arg(argument, "substring")?;
    let index = string.find(substring).map(|i| i as i64).unwrap_or(-1);
    Ok(OnionObject::Integer(index).stabilize())
}

fn starts_with(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let prefix = get_string_arg(argument, "prefix")?;
    Ok(OnionObject::Boolean(string.starts_with(prefix)).stabilize())
}

fn ends_with(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let suffix = get_string_arg(argument, "suffix")?;
    Ok(OnionObject::Boolean(string.ends_with(suffix)).stabilize())
}

fn repeat(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let count = get_integer_arg(argument, "count")?;

    if count < 0 {
        return Err(RuntimeError::InvalidOperation(
            "repeat count cannot be negative".to_string().into(),
        ));
    }
    let result = string.repeat(count as usize);
    Ok(OnionObject::String(result.into()).stabilize())
}

fn pad_left(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let length = get_integer_arg(argument, "length")?;
    let pad_char_str = get_string_arg(argument, "pad_char")?;

    if length < 0 {
        return Err(RuntimeError::InvalidOperation(
            "padding length cannot be negative".to_string().into(),
        ));
    }
    let target_len = length as usize;
    let s_char_len = string.chars().count();

    if s_char_len >= target_len {
        return Ok(OnionObject::String(string.to_string().into()).stabilize());
    }

    let pad_char = pad_char_str.chars().next().unwrap_or(' ');
    let pad_count = target_len - s_char_len;
    let padded = format!("{}{}", pad_char.to_string().repeat(pad_count), string);
    Ok(OnionObject::String(padded.into()).stabilize())
}

fn pad_right(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let length = get_integer_arg(argument, "length")?;
    let pad_char_str = get_string_arg(argument, "pad_char")?;

    if length < 0 {
        return Err(RuntimeError::InvalidOperation(
            "padding length cannot be negative".to_string().into(),
        ));
    }
    let target_len = length as usize;
    let s_char_len = string.chars().count();

    if s_char_len >= target_len {
        return Ok(OnionObject::String(string.to_string().into()).stabilize());
    }

    let pad_char = pad_char_str.chars().next().unwrap_or(' ');
    let pad_count = target_len - s_char_len;
    let padded = format!("{}{}", string, pad_char.to_string().repeat(pad_count));
    Ok(OnionObject::String(padded.into()).stabilize())
}

fn is_empty(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::Boolean(s.is_empty()).stabilize())
}

fn reverse(
    argument: &OnionFastMap<String, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    let reversed: String = s.chars().rev().collect();
    Ok(OnionObject::String(reversed.into()).stabilize())
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    let string_arg = OnionObject::String("string".to_string().into()).stabilize();

    module.insert(
        "length".to_string(),
        wrap_native_function(
            &string_arg,
            &OnionFastMap::default(),
            "string::length".to_string(),
            OnionKeyPool::create(vec!["string".to_string()]),
            &length,
        ),
    );
    module.insert(
        "trim".to_string(),
        wrap_native_function(
            &string_arg,
            &OnionFastMap::default(),
            "string::trim".to_string(),
            OnionKeyPool::create(vec!["string".to_string()]),
            &trim,
        ),
    );
    module.insert(
        "uppercase".to_string(),
        wrap_native_function(
            &string_arg,
            &OnionFastMap::default(),
            "string::uppercase".to_string(),
            OnionKeyPool::create(vec!["string".to_string()]),
            &uppercase,
        ),
    );
    module.insert(
        "lowercase".to_string(),
        wrap_native_function(
            &string_arg,
            &OnionFastMap::default(),
            "string::lowercase".to_string(),
            OnionKeyPool::create(vec!["string".to_string()]),
            &lowercase,
        ),
    );
    module.insert(
        "is_empty".to_string(),
        wrap_native_function(
            &string_arg,
            &OnionFastMap::default(),
            "string::is_empty".to_string(),
            OnionKeyPool::create(vec!["string".to_string()]),
            &is_empty,
        ),
    );
    module.insert(
        "reverse".to_string(),
        wrap_native_function(
            &string_arg,
            &OnionFastMap::default(),
            "string::reverse".to_string(),
            OnionKeyPool::create(vec!["string".to_string()]),
            &reverse,
        ),
    );

    module.insert(
        "contains".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "substring"]),
            &OnionFastMap::default(),
            "string::contains".to_string(),
            OnionKeyPool::create(vec!["string".to_string(), "substring".to_string()]),
            &contains,
        ),
    );
    module.insert(
        "concat".to_string(),
        wrap_native_function(
            &build_string_tuple(&["a", "b"]),
            &OnionFastMap::default(),
            "string::concat".to_string(),
            OnionKeyPool::create(vec!["a".to_string(), "b".to_string()]),
            &concat,
        ),
    );
    module.insert(
        "split".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "delimiter"]),
            &OnionFastMap::default(),
            "string::split".to_string(),
            OnionKeyPool::create(vec!["string".to_string(), "delimiter".to_string()]),
            &split,
        ),
    );
    module.insert(
        "replace".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "from", "to"]),
            &OnionFastMap::default(),
            "string::replace".to_string(),
            OnionKeyPool::create(vec![
                "string".to_string(),
                "from".to_string(),
                "to".to_string(),
            ]),
            &replace,
        ),
    );
    module.insert(
        "substr".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "start", "length"]),
            &OnionFastMap::default(),
            "string::substr".to_string(),
            OnionKeyPool::create(vec![
                "string".to_string(),
                "start".to_string(),
                "length".to_string(),
            ]),
            &substr,
        ),
    );
    module.insert(
        "index_of".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "substring"]),
            &OnionFastMap::default(),
            "string::index_of".to_string(),
            OnionKeyPool::create(vec!["string".to_string(), "substring".to_string()]),
            &index_of,
        ),
    );
    module.insert(
        "starts_with".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "prefix"]),
            &OnionFastMap::default(),
            "string::starts_with".to_string(),
            OnionKeyPool::create(vec!["string".to_string(), "prefix".to_string()]),
            &starts_with,
        ),
    );
    module.insert(
        "ends_with".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "suffix"]),
            &OnionFastMap::default(),
            "string::ends_with".to_string(),
            OnionKeyPool::create(vec!["string".to_string(), "suffix".to_string()]),
            &ends_with,
        ),
    );
    module.insert(
        "repeat".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "count"]),
            &OnionFastMap::default(),
            "string::repeat".to_string(),
            OnionKeyPool::create(vec!["string".to_string(), "count".to_string()]),
            &repeat,
        ),
    );
    module.insert(
        "pad_left".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "length", "pad_char"]),
            &OnionFastMap::default(),
            "string::pad_left".to_string(),
            OnionKeyPool::create(vec![
                "string".to_string(),
                "length".to_string(),
                "pad_char".to_string(),
            ]),
            &pad_left,
        ),
    );
    module.insert(
        "pad_right".to_string(),
        wrap_native_function(
            &build_string_tuple(&["string", "length", "pad_char"]),
            &OnionFastMap::default(),
            "string::pad_right".to_string(),
            OnionKeyPool::create(vec![
                "string".to_string(),
                "length".to_string(),
                "pad_char".to_string(),
            ]),
            &pad_right,
        ),
    );

    build_dict(module)
}
