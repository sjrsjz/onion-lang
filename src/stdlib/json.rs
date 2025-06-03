use super::{build_named_dict, wrap_native_function};
use indexmap::IndexMap;
use onion_vm::types::{pair::OnionPair, tuple::OnionTuple};
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionStaticObject},
};
use serde_json::Value;

fn to_json(obj: OnionObject) -> Result<Value, RuntimeError> {
    obj.with_data(|data| {
        match data {
            OnionObject::String(s) => Ok(Value::String(s.clone())),
            OnionObject::Integer(n) => Ok(Value::Number(serde_json::Number::from(*n))),
            OnionObject::Float(f) => Ok(Value::Number(serde_json::Number::from_f64(*f).unwrap())),
            OnionObject::Boolean(b) => Ok(Value::Bool(*b)),
            OnionObject::Null => Ok(Value::Null),
            OnionObject::Pair(p) => {
                // 将 Pair 转换为只有一个键值对的 JSON 对象
                let key = p
                    .get_key()
                    .try_borrow()?
                    .to_string(&vec![])
                    .map_err(|e| RuntimeError::InvalidOperation(e.to_string()))?;
                let value = to_json(p.get_value().try_borrow()?.clone())
                    .map_err(|e| RuntimeError::InvalidOperation(e.to_string()))?;
                let mut map = serde_json::Map::new();
                map.insert(key, value);
                Ok(Value::Object(map))
            }
            OnionObject::Tuple(t) => {
                // 检查是否所有元素都是 Pair，如果是则转换为 JSON 对象
                let all_pairs = t
                    .elements
                    .iter()
                    .all(|e| matches!(&*e.borrow(), OnionObject::Pair(_)));

                if all_pairs && !t.elements.is_empty() {
                    // 转换为 JSON 对象 (字典)
                    let mut map = serde_json::Map::new();
                    for element in &t.elements {
                        if let OnionObject::Pair(pair) = &*element.try_borrow()? {
                            let key = pair
                                .get_key()
                                .try_borrow()?
                                .to_string(&vec![])
                                .map_err(|e| RuntimeError::InvalidOperation(e.to_string()))?;
                            let value = to_json(pair.get_value().try_borrow()?.clone())
                                .map_err(|e| RuntimeError::InvalidOperation(e.to_string()))?;
                            map.insert(key, value);
                        }
                    }
                    Ok(Value::Object(map))
                } else {
                    // 转换为 JSON 数组
                    let vec: Vec<_> = t
                        .elements
                        .iter()
                        .map(|e| to_json(e.borrow().clone()))
                        .collect();
                    Ok(Value::Array(
                        vec.into_iter()
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|e| RuntimeError::InvalidOperation(e.to_string()))?,
                    ))
                }
            }
            _ => Err(RuntimeError::InvalidType(format!(
                "Cannot convert {:?} to JSON",
                data
            ))),
        }
    })
}

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
                    "Invalid number format".to_string(),
                ))
            }
        }
        Value::String(s) => Ok(OnionObject::String(s).stabilize()),
        Value::Array(arr) => {
            let elements: Result<Vec<_>, _> = arr
                .into_iter()
                .map(|v| from_json(v).map(|obj| obj.weak().clone()))
                .collect();
            match elements {
                Ok(elems) => Ok(OnionObject::Tuple(OnionTuple::new(elems)).stabilize()),
                Err(e) => Err(e),
            }
        }
        Value::Object(obj) => {
            let pairs: Result<Vec<_>, _> = obj
                .into_iter()
                .map(|(k, v)| {
                    let key_obj = OnionObject::String(k).to_cell();
                    let value_obj = from_json(v)?.weak().clone();
                    Ok(OnionObject::Pair(OnionPair::new(key_obj, value_obj)).to_cell())
                })
                .collect();
            match pairs {
                Ok(pair_elems) => Ok(OnionObject::Tuple(OnionTuple::new(pair_elems)).stabilize()),
                Err(e) => Err(e),
            }
        }
    }
}

/// 解析 JSON 字符串为 OnionObject
pub fn parse_json(json_str: &str) -> Result<OnionStaticObject, RuntimeError> {
    let value: Value = serde_json::from_str(json_str)
        .map_err(|e| RuntimeError::InvalidOperation(format!("JSON parse error: {}", e)))?;
    from_json(value)
}

/// 将 OnionObject 序列化为 JSON 字符串
pub fn stringify_json(obj: OnionObject) -> Result<String, RuntimeError> {
    let json_value = to_json(obj)?;
    serde_json::to_string(&json_value)
        .map_err(|e| RuntimeError::InvalidOperation(format!("JSON stringify error: {}", e)))
}

/// 将 OnionObject 序列化为格式化的 JSON 字符串
pub fn stringify_json_pretty(obj: OnionObject) -> Result<String, RuntimeError> {
    let json_value = to_json(obj)?;
    serde_json::to_string_pretty(&json_value)
        .map_err(|e| RuntimeError::InvalidOperation(format!("JSON stringify error: {}", e)))
}

/// 构建 JSON 模块
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // JSON.parse 函数参数定义
    let mut parse_params = IndexMap::new();
    parse_params.insert(
        "json_string".to_string(),
        OnionObject::Undefined(Some("JSON string to parse".to_string())).stabilize(),
    );

    // JSON.parse 函数
    module.insert(
        "parse".to_string(),
        wrap_native_function(
            &build_named_dict(parse_params),
            None,
            None,
            "json_parse".to_string(),
            |args, _gc| {
                args.weak().with_data(|data| {
                    let json_string = super::get_attr_direct(data, "json_string".to_string())?;
                    json_string
                        .weak()
                        .with_data(|string_data| match string_data {
                            OnionObject::String(s) => parse_json(s)
                                .map_err(|e| RuntimeError::InvalidOperation(e.to_string())),
                            _ => Err(RuntimeError::InvalidOperation(
                                "parse requires string".to_string(),
                            )),
                        })
                })
            },
        ),
    );

    // JSON.stringify 函数参数定义
    let mut stringify_params = IndexMap::new();
    stringify_params.insert(
        "object".to_string(),
        OnionObject::Undefined(Some("Object to stringify".to_string())).stabilize(),
    );

    // JSON.stringify 函数
    module.insert(
        "stringify".to_string(),
        wrap_native_function(
            &build_named_dict(stringify_params),
            None,
            None,
            "json_stringify".to_string(),
            |args, _gc| {
                args.weak().with_data(|data| {
                    let obj = super::get_attr_direct(data, "object".to_string())?;
                    let json_str = stringify_json(obj.weak().try_borrow()?.clone())
                        .map_err(|e| RuntimeError::InvalidOperation(e.to_string()))?;
                    Ok(OnionObject::String(json_str).stabilize())
                })
            },
        ),
    );

    // JSON.stringify_pretty 函数参数定义
    let mut stringify_pretty_params = IndexMap::new();
    stringify_pretty_params.insert(
        "object".to_string(),
        OnionObject::Undefined(Some(
            "Object to stringify with pretty formatting".to_string(),
        ))
        .stabilize(),
    );

    // JSON.stringify_pretty 函数
    module.insert(
        "stringify_pretty".to_string(),
        wrap_native_function(
            &build_named_dict(stringify_pretty_params),
            None,
            None,
            "json_stringify_pretty".to_string(),
            |args, _gc| {
                args.weak().with_data(|data| {
                    let obj = super::get_attr_direct(data, "object".to_string())?;
                    let json_str = stringify_json_pretty(obj.weak().try_borrow()?.clone())
                        .map_err(|e| RuntimeError::InvalidOperation(e.to_string()))?;
                    Ok(OnionObject::String(json_str).stabilize())
                })
            },
        ),
    );

    build_named_dict(module)
}
