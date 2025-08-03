use super::{build_dict, wrap_native_function};
use indexmap::IndexMap;
use onion_vm::types::lambda::parameter::LambdaParameter;
use onion_vm::types::{pair::OnionPair, tuple::OnionTuple};
use onion_vm::utils::fastmap::{OnionFastMap, OnionKeyPool};
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
};
use serde_json::Value;

/// 将 OnionObject 递归转换为 serde_json::Value。
fn to_json(obj: OnionObject) -> Result<Value, RuntimeError> {
    obj.with_data(|data| {
        match data {
            OnionObject::String(s) => Ok(Value::String(s.as_ref().clone())),
            OnionObject::Integer(n) => Ok(Value::Number(serde_json::Number::from(*n))),
            OnionObject::Float(f) => Ok(Value::Number(serde_json::Number::from_f64(*f).unwrap())),
            OnionObject::Boolean(b) => Ok(Value::Bool(*b)),
            OnionObject::Null => Ok(Value::Null),
            OnionObject::Pair(p) => {
                // 将 Pair 转换为只有一个键值对的 JSON 对象
                let key = p.get_key().to_string(&vec![])?;
                let value = to_json(p.get_value().clone())?;
                let mut map = serde_json::Map::new();
                map.insert(key, value);
                Ok(Value::Object(map))
            }
            OnionObject::Tuple(t) => {
                let elements = t.get_elements();
                // 检查元组是否可以被视为一个字典（所有元素都是键值对）
                if !elements.is_empty()
                    && elements.iter().all(|e| matches!(e, OnionObject::Pair(_)))
                {
                    let mut map = serde_json::Map::new();
                    for element in elements {
                        if let OnionObject::Pair(pair) = element {
                            let key = pair.get_key().to_string(&vec![])?;
                            let value = to_json(pair.get_value().clone())?;
                            map.insert(key, value);
                        }
                    }
                    Ok(Value::Object(map))
                } else {
                    // 否则，转换为 JSON 数组
                    let vec: Result<Vec<_>, _> =
                        elements.iter().map(|e| to_json(e.clone())).collect();
                    Ok(Value::Array(vec?))
                }
            }
            _ => Err(RuntimeError::InvalidType(
                "Cannot convert type to JSON".into(),
            )),
        }
    })
}

/// 将 serde_json::Value 递归转换为 OnionObject。
fn from_json(value: Value) -> Result<OnionStaticObject, RuntimeError> {
    match value {
        Value::Null => Ok(OnionObject::Null.stabilize()),
        Value::Bool(b) => Ok(OnionObject::Boolean(b).stabilize()),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(OnionObject::Integer(i).stabilize())
            } else if let Some(f) = n.as_f64() {
                Ok(OnionObject::Float(f).stabilize())
            } else {
                Err(RuntimeError::InvalidType(
                    "Invalid JSON number format".into(),
                ))
            }
        }
        Value::String(s) => Ok(OnionObject::String(s.into()).stabilize()),
        Value::Array(arr) => {
            let elements: Result<Vec<_>, _> = arr
                .into_iter()
                .map(|v| from_json(v).map(|obj| obj.weak().clone()))
                .collect();
            Ok(OnionObject::Tuple(OnionTuple::new(elements?).into()).stabilize())
        }
        Value::Object(obj) => {
            let pairs: Result<Vec<_>, _> = obj
                .into_iter()
                .map(|(k, v)| {
                    let key_obj = OnionObject::String(k.into());
                    let value_obj = from_json(v)?.weak().clone();
                    Ok(OnionObject::Pair(OnionPair::new(key_obj, value_obj).into()))
                })
                .collect();
            Ok(OnionObject::Tuple(OnionTuple::new(pairs?).into()).stabilize())
        }
    }
}

/// 解析 JSON 字符串为 OnionObject
fn parse_json(json_str: &str) -> Result<OnionStaticObject, RuntimeError> {
    let value: Value = serde_json::from_str(json_str)
        .map_err(|e| RuntimeError::DetailedError(format!("JSON parse error: {e}").into()))?;
    from_json(value)
}

/// 将 OnionObject 序列化为 JSON 字符串
fn stringify_json(obj: OnionObject) -> Result<String, RuntimeError> {
    let json_value = to_json(obj)?;
    serde_json::to_string(&json_value)
        .map_err(|e| RuntimeError::DetailedError(format!("JSON stringify error: {e}").into()))
}

/// 将 OnionObject 序列化为格式化的 JSON 字符串
fn stringify_json_pretty(obj: OnionObject) -> Result<String, RuntimeError> {
    let json_value = to_json(obj)?;
    serde_json::to_string_pretty(&json_value)
        .map_err(|e| RuntimeError::DetailedError(format!("JSON stringify error: {e}").into()))
}

// --- 新的原生函数封装 (遵循 io 模块的模式) ---

fn json_parse(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(json_string_obj) = argument.get(&"json_string".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "json.parse requires a 'json_string' argument"
                .to_string()
                .into(),
        ));
    };

    json_string_obj.weak().with_data(|data| match data {
        OnionObject::String(s) => parse_json(s),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'json_string' must be a string".into(),
        )),
    })
}

fn json_stringify(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(object_to_stringify) = argument.get(&"object".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "json.stringify requires an 'object' argument"
                .to_string()
                .into(),
        ));
    };

    let json_str = stringify_json(object_to_stringify.weak().clone())?;
    Ok(OnionObject::String(json_str.into()).stabilize())
}

fn json_stringify_pretty(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(object_to_stringify) = argument.get(&"object".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "json.stringify_pretty requires an 'object' argument"
                .to_string()
                .into(),
        ));
    };

    let json_str = stringify_json_pretty(object_to_stringify.weak().clone())?;
    Ok(OnionObject::String(json_str.into()).stabilize())
}
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // json.parse(json_string)
    module.insert(
        "parse".to_string(),
        wrap_native_function(
            LambdaParameter::top("json_string"), // <-- Changed here
            OnionFastMap::default(),
            "json::parse".to_string(),
            OnionKeyPool::create(vec!["json_string".to_string()]),
            &json_parse,
        ),
    );

    // json.stringify(object)
    module.insert(
        "stringify".to_string(),
        wrap_native_function(
            LambdaParameter::top("object"), // <-- Changed here
            OnionFastMap::default(),
            "json::stringify".to_string(),
            OnionKeyPool::create(vec!["object".to_string()]),
            &json_stringify,
        ),
    );

    // json.stringify_pretty(object)
    module.insert(
        "stringify_pretty".to_string(),
        wrap_native_function(
            LambdaParameter::top("object"), // <-- Changed here
            OnionFastMap::default(),
            "json::stringify_pretty".to_string(),
            OnionKeyPool::create(vec!["object".to_string()]),
            &json_stringify_pretty,
        ),
    );

    build_dict(module)
}
