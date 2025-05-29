use onion_vm::{lambda::runnable::RuntimeError, types::object::{ObjectError, OnionObject, OnionStaticObject}};
use onion_vm::types::{tuple::OnionTuple, pair::OnionPair};
use serde_json::Value;
use std::collections::HashMap;
use super::{build_named_dict, wrap_native_function};

fn to_json(
    obj: OnionObject,
) -> Result<Value, RuntimeError> {
    obj
        .with_data(|data| {
            match data {
                OnionObject::String(s) => Ok(Value::String(s.clone())),
                OnionObject::Integer(n) => Ok(Value::Number(serde_json::Number::from(*n))),
                OnionObject::Float(f) => Ok(Value::Number(serde_json::Number::from_f64(*f).unwrap())),
                OnionObject::Boolean(b) => Ok(Value::Bool(*b)),
                OnionObject::Null => Ok(Value::Null),
                OnionObject::Pair(p) => {
                    // 将 Pair 转换为只有一个键值对的 JSON 对象
                    let key = p.get_key().to_string().map_err(|e| ObjectError::InvalidOperation(e.to_string()))?;
                    let value = to_json(p.get_value().clone()).map_err(|e| ObjectError::InvalidOperation(e.to_string()))?;
                    let mut map = serde_json::Map::new();
                    map.insert(key, value);
                    Ok(Value::Object(map))
                }
                OnionObject::Tuple(t) => {
                    // 检查是否所有元素都是 Pair，如果是则转换为 JSON 对象
                    let all_pairs = t.elements.iter().all(|e| matches!(e, OnionObject::Pair(_)));
                    
                    if all_pairs && !t.elements.is_empty() {
                        // 转换为 JSON 对象 (字典)
                        let mut map = serde_json::Map::new();
                        for element in &t.elements {
                            if let OnionObject::Pair(pair) = element {
                                let key = pair.get_key().to_string().map_err(|e| ObjectError::InvalidOperation(e.to_string()))?;
                                let value = to_json(pair.get_value().clone()).map_err(|e| ObjectError::InvalidOperation(e.to_string()))?;
                                map.insert(key, value);
                            }
                        }
                        Ok(Value::Object(map))
                    } else {
                        // 转换为 JSON 数组
                        let vec: Vec<_> = t.elements.iter()
                            .map(|e| to_json(e.clone()))
                            .collect();
                        Ok(Value::Array(vec.into_iter().collect::<Result<Vec<_>, _>>().map_err(|e| ObjectError::InvalidOperation(e.to_string()))?))
                    }
                }
                _ => Err(ObjectError::InvalidType(format!("Cannot convert {:?} to JSON", data))),
            }
        })
        .map_err(RuntimeError::ObjectError)
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
                Err(RuntimeError::ObjectError(ObjectError::InvalidType(
                    "Invalid number format".to_string()
                )))
            }
        }
        Value::String(s) => Ok(OnionObject::String(s).stabilize()),
        Value::Array(arr) => {
            let elements: Result<Vec<_>, _> = arr.into_iter()
                .map(|v| from_json(v).map(|obj| obj.weak().clone()))
                .collect();
            match elements {
                Ok(elems) => Ok(OnionObject::Tuple(OnionTuple::new(elems)).stabilize()),
                Err(e) => Err(e),
            }
        }
        Value::Object(obj) => {
            let pairs: Result<Vec<_>, _> = obj.into_iter()
                .map(|(k, v)| {
                    let key_obj = OnionObject::String(k);
                    let value_obj = from_json(v)?.weak().clone();
                    Ok(OnionObject::Pair(OnionPair::new(key_obj, value_obj)))
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
        .map_err(|e| RuntimeError::ObjectError(ObjectError::InvalidOperation(
            format!("JSON parse error: {}", e)
        )))?;
    from_json(value)
}

/// 将 OnionObject 序列化为 JSON 字符串
pub fn stringify_json(obj: OnionObject) -> Result<String, RuntimeError> {
    let json_value = to_json(obj)?;
    serde_json::to_string(&json_value)
        .map_err(|e| RuntimeError::ObjectError(ObjectError::InvalidOperation(
            format!("JSON stringify error: {}", e)
        )))
}

/// 将 OnionObject 序列化为格式化的 JSON 字符串
pub fn stringify_json_pretty(obj: OnionObject) -> Result<String, RuntimeError> {
    let json_value = to_json(obj)?;
    serde_json::to_string_pretty(&json_value)
        .map_err(|e| RuntimeError::ObjectError(ObjectError::InvalidOperation(
            format!("JSON stringify error: {}", e)
        )))
}

/// 构建 JSON 模块
pub fn build_module() -> OnionStaticObject {
    let mut module = HashMap::new();
    
    // JSON.parse 函数参数定义
    let mut parse_params = HashMap::new();
    parse_params.insert(
        "json_string".to_string(),
        OnionObject::Undefined("JSON string to parse".to_string()).stabilize(),
    );
    
    // JSON.parse 函数
    module.insert("parse".to_string(), wrap_native_function(
        &build_named_dict(parse_params),
        None,
        None,
        "json_parse".to_string(),
        |args, _gc| {
            args.weak().with_data(|data| {
                let json_string = super::get_attr_direct(data, "json_string".to_string())?;
                json_string.weak().with_data(|string_data| {
                    match string_data {
                        OnionObject::String(s) => parse_json(s).map_err(|e| ObjectError::InvalidOperation(e.to_string())),
                        _ => Err(ObjectError::InvalidOperation("parse requires string".to_string())),
                    }
                })
            }).map_err(RuntimeError::ObjectError)
        }
    ));
    
    // JSON.stringify 函数参数定义
    let mut stringify_params = HashMap::new();
    stringify_params.insert(
        "object".to_string(),
        OnionObject::Undefined("Object to stringify".to_string()).stabilize(),
    );
    
    // JSON.stringify 函数
    module.insert("stringify".to_string(), wrap_native_function(
        &build_named_dict(stringify_params),
        None,
        None,
        "json_stringify".to_string(),
        |args, _gc| {
            args.weak().with_data(|data| {
                let obj = super::get_attr_direct(data, "object".to_string())?;
                let json_str = stringify_json(obj.weak().clone()).map_err(|e| ObjectError::InvalidOperation(e.to_string()))?;
                Ok(OnionObject::String(json_str).stabilize())
            }).map_err(RuntimeError::ObjectError)
        }
    ));
    
    // JSON.stringify_pretty 函数参数定义
    let mut stringify_pretty_params = HashMap::new();
    stringify_pretty_params.insert(
        "object".to_string(),
        OnionObject::Undefined("Object to stringify with pretty formatting".to_string()).stabilize(),
    );
    
    // JSON.stringify_pretty 函数
    module.insert("stringify_pretty".to_string(), wrap_native_function(
        &build_named_dict(stringify_pretty_params),
        None,
        None,
        "json_stringify_pretty".to_string(),
        |args, _gc| {
            args.weak().with_data(|data| {
                let obj = super::get_attr_direct(data, "object".to_string())?;
                let json_str = stringify_json_pretty(obj.weak().clone()).map_err(|e| ObjectError::InvalidOperation(e.to_string()))?;
                Ok(OnionObject::String(json_str).stabilize())
            }).map_err(RuntimeError::ObjectError)
        }
    ));
    
    build_named_dict(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use onion_vm::types::object::OnionObject;
    
    #[test]
    fn test_basic_types_to_json() {
        // 测试基本类型转换
        let int_obj = OnionObject::Integer(42);
        let result = to_json(int_obj).unwrap();
        assert_eq!(result, Value::Number(serde_json::Number::from(42i64)));
        
        let str_obj = OnionObject::String("hello".to_string());
        let result = to_json(str_obj).unwrap();
        assert_eq!(result, Value::String("hello".to_string()));
        
        let bool_obj = OnionObject::Boolean(true);
        let result = to_json(bool_obj).unwrap();
        assert_eq!(result, Value::Bool(true));
        
        let null_obj = OnionObject::Null;
        let result = to_json(null_obj).unwrap();
        assert_eq!(result, Value::Null);
    }
    
    #[test]
    fn test_tuple_as_array_to_json() {
        // 测试 Tuple 转换为 JSON 数组
        let tuple = OnionTuple::new(vec![
            OnionObject::Integer(1),
            OnionObject::String("test".to_string()),
            OnionObject::Boolean(false),
        ]);
        let result = to_json(OnionObject::Tuple(tuple)).unwrap();
        
        let expected = Value::Array(vec![
            Value::Number(serde_json::Number::from(1i64)),
            Value::String("test".to_string()),
            Value::Bool(false),
        ]);
        assert_eq!(result, expected);
    }
    
    #[test]
    fn test_tuple_with_pairs_as_object_to_json() {
        // 测试包含 Pair 的 Tuple 转换为 JSON 对象
        let pair1 = OnionPair::new(
            OnionObject::String("name".to_string()),
            OnionObject::String("Alice".to_string()),
        );
        let pair2 = OnionPair::new(
            OnionObject::String("age".to_string()),
            OnionObject::Integer(30),
        );
        
        let tuple = OnionTuple::new(vec![
            OnionObject::Pair(pair1),
            OnionObject::Pair(pair2),
        ]);
        
        let result = to_json(OnionObject::Tuple(tuple)).unwrap();
        
        if let Value::Object(map) = result {
            assert_eq!(map.get("name"), Some(&Value::String("Alice".to_string())));
            assert_eq!(map.get("age"), Some(&Value::Number(serde_json::Number::from(30i64))));
        } else {
            panic!("Expected JSON object");
        }
    }
    
    #[test]
    fn test_from_json_basic_types() {
        // 测试从 JSON 转换为基本类型
        let result = from_json(Value::Number(serde_json::Number::from(42))).unwrap();
        assert!(matches!(result.weak(), OnionObject::Integer(42)));
        
        let result = from_json(Value::String("hello".to_string())).unwrap();
        assert!(matches!(result.weak(), OnionObject::String(s) if s == "hello"));
        
        let result = from_json(Value::Bool(true)).unwrap();
        assert!(matches!(result.weak(), OnionObject::Boolean(true)));
        
        let result = from_json(Value::Null).unwrap();
        assert!(matches!(result.weak(), OnionObject::Null));
    }
    
    #[test]
    fn test_parse_stringify_roundtrip() {
        let json_str = r#"{"name": "Alice", "age": 30, "active": true}"#;
        let parsed = parse_json(json_str).unwrap();
        let stringified = stringify_json(parsed.weak().clone()).unwrap();
        
        // 解析回来验证结构
        let reparsed: Value = serde_json::from_str(&stringified).unwrap();
        let original: Value = serde_json::from_str(json_str).unwrap();
        assert_eq!(reparsed, original);
    }
}