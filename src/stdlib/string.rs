use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

// 引入所需的辅助函数
use super::{build_dict, wrap_native_function};

fn get_string_arg<'a>(
    argument: &'a OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<&'a str, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
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
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    name: &str,
) -> Result<i64, RuntimeError> {
    let obj = argument.get(name).ok_or_else(|| {
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
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::Integer(s.chars().count() as i64).stabilize())
}

fn trim(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::String(s.trim().into()).stabilize())
}

fn uppercase(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::String(s.to_uppercase().into()).stabilize())
}

fn lowercase(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::String(s.to_lowercase().into()).stabilize())
}

fn contains(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let substring = get_string_arg(argument, "substring")?;
    Ok(OnionObject::Boolean(string.contains(substring)).stabilize())
}

fn concat(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s1 = get_string_arg(argument, "a")?;
    let s2 = get_string_arg(argument, "b")?;
    let result = [s1, s2].concat();
    Ok(OnionObject::String(result.into()).stabilize())
}

fn split(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let delimiter = get_string_arg(argument, "delimiter")?;
    let parts: Vec<_> = string
        .split(delimiter)
        .map(|part| OnionObject::String(part.into()))
        .collect();
    Ok(OnionObject::Tuple(OnionTuple::new(parts).into()).stabilize())
}

fn replace(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let from = get_string_arg(argument, "from")?;
    let to = get_string_arg(argument, "to")?;
    let result = string.replace(from, to);
    Ok(OnionObject::String(result.into()).stabilize())
}

fn substr(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
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
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let substring = get_string_arg(argument, "substring")?;
    let index = string.find(substring).map(|i| i as i64).unwrap_or(-1);
    Ok(OnionObject::Integer(index).stabilize())
}

fn starts_with(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let prefix = get_string_arg(argument, "prefix")?;
    Ok(OnionObject::Boolean(string.starts_with(prefix)).stabilize())
}

fn ends_with(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let suffix = get_string_arg(argument, "suffix")?;
    Ok(OnionObject::Boolean(string.ends_with(suffix)).stabilize())
}

fn repeat(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let count = get_integer_arg(argument, "count")?;

    if count < 0 {
        return Err(RuntimeError::InvalidOperation(
            "repeat count cannot be negative".into(),
        ));
    }
    let result = string.repeat(count as usize);
    Ok(OnionObject::String(result.into()).stabilize())
}

fn pad_left(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let length = get_integer_arg(argument, "length")?;
    let pad_char_str = get_string_arg(argument, "pad_char")?;

    if length < 0 {
        return Err(RuntimeError::InvalidOperation(
            "padding length cannot be negative".into(),
        ));
    }
    let target_len = length as usize;
    let s_char_len = string.chars().count();

    if s_char_len >= target_len {
        return Ok(OnionObject::String(string.into()).stabilize());
    }

    let pad_char = pad_char_str.chars().next().unwrap_or(' ');
    let pad_count = target_len - s_char_len;
    let padded = format!("{}{}", pad_char.to_string().repeat(pad_count), string);
    Ok(OnionObject::String(padded.into()).stabilize())
}

fn pad_right(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let string = get_string_arg(argument, "string")?;
    let length = get_integer_arg(argument, "length")?;
    let pad_char_str = get_string_arg(argument, "pad_char")?;

    if length < 0 {
        return Err(RuntimeError::InvalidOperation(
            "padding length cannot be negative".into(),
        ));
    }
    let target_len = length as usize;
    let s_char_len = string.chars().count();

    if s_char_len >= target_len {
        return Ok(OnionObject::String(string.into()).stabilize());
    }

    let pad_char = pad_char_str.chars().next().unwrap_or(' ');
    let pad_count = target_len - s_char_len;
    let padded = format!("{}{}", string, pad_char.to_string().repeat(pad_count));
    Ok(OnionObject::String(padded.into()).stabilize())
}

fn is_empty(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    Ok(OnionObject::Boolean(s.is_empty()).stabilize())
}

fn reverse(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let s = get_string_arg(argument, "string")?;
    let reversed: String = s.chars().rev().collect();
    Ok(OnionObject::String(reversed.into()).stabilize())
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // --- Single-argument functions ---
    module.insert(
        "length".to_string(),
        wrap_native_function(
            LambdaParameter::top("string"),
            OnionFastMap::default(),
            "string::length",
            OnionKeyPool::create(vec!["string".into()]),
            &length,
        ),
    );
    module.insert(
        "trim".to_string(),
        wrap_native_function(
            LambdaParameter::top("string"),
            OnionFastMap::default(),
            "string::trim",
            OnionKeyPool::create(vec!["string".into()]),
            &trim,
        ),
    );
    module.insert(
        "uppercase".to_string(),
        wrap_native_function(
            LambdaParameter::top("string"),
            OnionFastMap::default(),
            "string::uppercase",
            OnionKeyPool::create(vec!["string".into()]),
            &uppercase,
        ),
    );
    module.insert(
        "lowercase".to_string(),
        wrap_native_function(
            LambdaParameter::top("string"),
            OnionFastMap::default(),
            "string::lowercase",
            OnionKeyPool::create(vec!["string".into()]),
            &lowercase,
        ),
    );
    module.insert(
        "is_empty".to_string(),
        wrap_native_function(
            LambdaParameter::top("string"),
            OnionFastMap::default(),
            "string::is_empty",
            OnionKeyPool::create(vec!["string".into()]),
            &is_empty,
        ),
    );
    module.insert(
        "reverse".to_string(),
        wrap_native_function(
            LambdaParameter::top("string"),
            OnionFastMap::default(),
            "string::reverse",
            OnionKeyPool::create(vec!["string".into()]),
            &reverse,
        ),
    );

    // --- Two-argument functions ---
    module.insert(
        "contains".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("substring"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::contains",
            OnionKeyPool::create(vec!["string".into(), "substring".into()]),
            &contains,
        ),
    );
    module.insert(
        "concat".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("a"), LambdaParameter::top("b")].into(),
            ),
            OnionFastMap::default(),
            "string::concat",
            OnionKeyPool::create(vec!["a".into(), "b".into()]),
            &concat,
        ),
    );
    module.insert(
        "split".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("delimiter"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::split",
            OnionKeyPool::create(vec!["string".into(), "delimiter".into()]),
            &split,
        ),
    );
    module.insert(
        "index_of".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("substring"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::index_of",
            OnionKeyPool::create(vec!["string".into(), "substring".into()]),
            &index_of,
        ),
    );
    module.insert(
        "starts_with".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("prefix"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::starts_with",
            OnionKeyPool::create(vec!["string".into(), "prefix".into()]),
            &starts_with,
        ),
    );
    module.insert(
        "ends_with".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("suffix"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::ends_with",
            OnionKeyPool::create(vec!["string".into(), "suffix".into()]),
            &ends_with,
        ),
    );
    module.insert(
        "repeat".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("count"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::repeat",
            OnionKeyPool::create(vec!["string".into(), "count".into()]),
            &repeat,
        ),
    );

    // --- Three-argument functions ---
    module.insert(
        "replace".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("from"),
                    LambdaParameter::top("to"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::replace",
            OnionKeyPool::create(vec!["string".into(), "from".into(), "to".into()]),
            &replace,
        ),
    );
    module.insert(
        "substr".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("start"),
                    LambdaParameter::top("length"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::substr",
            OnionKeyPool::create(vec!["string".into(), "start".into(), "length".into()]),
            &substr,
        ),
    );
    module.insert(
        "pad_left".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("length"),
                    LambdaParameter::top("pad_char"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::pad_left",
            OnionKeyPool::create(vec!["string".into(), "length".into(), "pad_char".into()]),
            &pad_left,
        ),
    );
    module.insert(
        "pad_right".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("string"),
                    LambdaParameter::top("length"),
                    LambdaParameter::top("pad_char"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "string::pad_right",
            OnionKeyPool::create(vec!["string".into(), "length".into(), "pad_char".into()]),
            &pad_right,
        ),
    );

    build_dict(module)
}
