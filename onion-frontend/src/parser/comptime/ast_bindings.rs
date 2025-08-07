//! AST 构造与绑定模块。
//!
//! 本模块为 Onion 语言的编译期/运行期 AST 构造与序列化提供原生绑定，
//! 允许通过 VM 层以对象方式动态构建、序列化、反序列化、操作 AST。
//! 提供所有 AST 节点类型的构造函数、常量节点、以及 AST <-> 字节流的互转能力。
//!
//! # 主要内容
//! - AST 各节点类型的构造函数（如 string/number/boolean/variable/let/operation/lambda_def 等）
//! - AST 常量节点（如 null/undefined/assign/tuple/if/while 等）
//! - AST 的序列化与反序列化
//! - 构建完整 AST 模块导出给 VM
use std::{collections::HashSet, sync::Arc};

use base64::Engine;
use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        pair::OnionPair,
        tuple::OnionTuple,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

// Assuming these are in the current scope from your provided code
use crate::parser::{
    ast::{ASTNode, ASTNodeModifier, ASTNodeOperation, ASTNodeType},
    comptime::{OnionASTObject, native::wrap_native_function},
};

/// 将 Rust 层的字典转为 Onion VM 静态对象（Tuple of Pair）。
fn build_dict(dict: IndexMap<String, OnionStaticObject>) -> OnionStaticObject {
    let mut pairs = vec![];
    for (key, value) in dict {
        pairs.push(OnionPair::new_static(
            &OnionObject::String(key.into()).stabilize(),
            &value,
        ));
    }
    OnionTuple::new_static_no_ref(&pairs)
}
// Helper to wrap a new ASTNode into the required Onion object format.
/// 将 ASTNode 包装为 Onion VM 可识别的自定义对象。
fn ast_wrapper(ast: ASTNode) -> OnionStaticObject {
    OnionObject::Custom(Arc::new(OnionASTObject { ast })).stabilize()
}

// --- 各类带数据 AST 节点的构造函数 ---

/// 构造 String 类型 AST 节点。
fn string(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value = argument.get("value").ok_or_else(|| {
        RuntimeError::InvalidOperation("string() requires a 'value' argument".into())
    })?;
    value.weak().with_data(|data| match data {
        OnionObject::String(s) => Ok(ast_wrapper(ASTNode {
            node_type: ASTNodeType::String(s.to_string()),
            children: vec![],
            source_location: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' for string() must be a string".into(),
        )),
    })
}

/// 构造 Base64/Bytes 类型 AST 节点。
fn bytes(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value = argument.get("value").ok_or_else(|| {
        RuntimeError::InvalidOperation("bytes() requires a 'value' argument".into())
    })?;
    value.weak().with_data(|data| match data {
        OnionObject::Bytes(s) => Ok(ast_wrapper(ASTNode {
            node_type: ASTNodeType::Base64(base64::engine::general_purpose::STANDARD.encode(s)),
            children: vec![],
            source_location: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' for bytes() must be a string".into(),
        )),
    })
}

/// 反序列化 AST 节点（从字节流转 ASTNode）。
fn deserialize_ast(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value = argument.get("value").ok_or_else(|| {
        RuntimeError::InvalidOperation("deserialize() requires a 'value' argument".into())
    })?;
    value.weak().with_data(|data| match data {
        OnionObject::Bytes(data) => {
            let deserialized: Result<ASTNode, _> =
                bincode::serde::decode_from_slice(data.as_ref(), bincode::config::standard())
                    .map(|(result, _)| result);
            match deserialized {
                Ok(node) => Ok(ast_wrapper(node)),
                Err(err) => Err(RuntimeError::InvalidType(
                    format!("Failed to deserialize AST: {}", err).into(),
                )),
            }
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' for deserialize() must be a bytes".into(),
        )),
    })
}

/// 序列化 AST 节点（ASTNode -> 字节流）。
fn serialize_ast(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value = argument.get("value").ok_or_else(|| {
        RuntimeError::InvalidOperation("serialize() requires a 'value' argument".into())
    })?;
    value.weak().with_data(|data| match data {
        OnionObject::Custom(ast_obj) => {
            let ast_obj: &OnionASTObject = ast_obj
                .as_any()
                .downcast_ref()
                .ok_or_else(|| RuntimeError::InvalidType("Expected OnionASTObject".into()))?;
            let serialized = bincode::serde::encode_to_vec(
                &ast_obj.ast,
                bincode::config::standard(),
            )
            .map_err(|err| {
                RuntimeError::InvalidType(format!("Failed to serialize AST: {}", err).into())
            })?;
            Ok(OnionObject::Bytes(serialized.into()).stabilize())
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' for serialize() must be an ASTNode".into(),
        )),
    })
}

/// 构造 Boolean 类型 AST 节点。
fn boolean(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value = argument.get("value").ok_or_else(|| {
        RuntimeError::InvalidOperation("boolean() requires a 'value' argument".into())
    })?;
    value.weak().with_data(|data| match data {
        OnionObject::Boolean(b) => Ok(ast_wrapper(ASTNode {
            node_type: ASTNodeType::Boolean(*b),
            children: vec![],
            source_location: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' for boolean() must be a boolean".into(),
        )),
    })
}

/// 构造 Number 类型 AST 节点。
fn number(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value = argument.get("value").ok_or_else(|| {
        RuntimeError::InvalidOperation("number() requires a 'value' argument".into())
    })?;
    value.weak().with_data(|data| match data {
        OnionObject::Integer(n) => Ok(ast_wrapper(ASTNode {
            node_type: ASTNodeType::Number(n.to_string()),
            children: vec![],
            source_location: None,
        })),
        OnionObject::Float(f) => Ok(ast_wrapper(ASTNode {
            node_type: ASTNodeType::Number(f.to_string()),
            children: vec![],
            source_location: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' for number() must be a number".into(),
        )),
    })
}

/// 构造 Variable 类型 AST 节点。
fn variable(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let name = argument.get("name").ok_or_else(|| {
        RuntimeError::InvalidOperation("variable() requires a 'name' argument".into())
    })?;
    name.weak().with_data(|data| match data {
        OnionObject::String(s) => Ok(ast_wrapper(ASTNode {
            node_type: ASTNodeType::Variable(s.to_string()),
            children: vec![],
            source_location: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'name' for variable() must be a string".into(),
        )),
    })
}

/// 构造 Let 类型 AST 节点。
fn let_var(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let name = argument.get("name").ok_or_else(|| {
        RuntimeError::InvalidOperation("let_var() requires a 'name' argument".into())
    })?;
    name.weak().with_data(|data| match data {
        OnionObject::String(s) => Ok(ast_wrapper(ASTNode {
            node_type: ASTNodeType::Let(s.to_string()),
            children: vec![],
            source_location: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'name' for let_var() must be a string".into(),
        )),
    })
}

/// 构造 Operation 类型 AST 节点。
fn operation(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let op = argument.get("op").ok_or_else(|| {
        RuntimeError::InvalidOperation("operation() requires an 'op' argument".into())
    })?;
    op.weak().with_data(|data| match data {
        OnionObject::String(s) => {
            let op_type = match s.as_ref() {
                "+" => ASTNodeOperation::Add,
                "abs" => ASTNodeOperation::Abs,
                "-" => ASTNodeOperation::Subtract,
                "minus" => ASTNodeOperation::Minus,
                "*" => ASTNodeOperation::Multiply,
                "/" => ASTNodeOperation::Divide,
                "%" => ASTNodeOperation::Modulus,
                "**" => ASTNodeOperation::Power,
                "and" => ASTNodeOperation::And,
                "xor" => ASTNodeOperation::Xor,
                "or" => ASTNodeOperation::Or,
                "not" => ASTNodeOperation::Not,
                "==" => ASTNodeOperation::Equal,
                "!=" => ASTNodeOperation::NotEqual,
                ">" => ASTNodeOperation::Greater,
                "<" => ASTNodeOperation::Less,
                ">=" => ASTNodeOperation::GreaterEqual,
                "<=" => ASTNodeOperation::LessEqual,
                "<<" => ASTNodeOperation::LeftShift,
                ">>" => ASTNodeOperation::RightShift,
                _ => {
                    return Err(RuntimeError::InvalidOperation(
                        format!("Unsupported operation: {}", s).into(),
                    ));
                }
            };
            Ok(ast_wrapper(ASTNode {
                node_type: ASTNodeType::Operation(op_type),
                children: vec![],
                source_location: None,
            }))
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'op' for operation() must be a string".into(),
        )),
    })
}

/// 构造 Modifier 类型 AST 节点。
fn modifier(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let name = argument.get("name").ok_or_else(|| {
        RuntimeError::InvalidOperation("modifier() requires a 'name' argument".into())
    })?;
    name.weak().with_data(|data| match data {
        OnionObject::String(s) => {
            let mod_type = match s.as_ref() {
                "mut" => ASTNodeModifier::Mut,
                "const" => ASTNodeModifier::Const,
                "keyof" => ASTNodeModifier::KeyOf,
                "valueof" => ASTNodeModifier::ValueOf,
                "assert" => ASTNodeModifier::Assert,
                "import" => ASTNodeModifier::Import,
                "typeof" => ASTNodeModifier::TypeOf,
                "lengthof" => ASTNodeModifier::LengthOf,
                "launch" => ASTNodeModifier::Launch,
                "spawn" => ASTNodeModifier::Spawn,
                "async" => ASTNodeModifier::Async,
                "sync" => ASTNodeModifier::Sync,
                "atomic" => ASTNodeModifier::Atomic,
                _ => {
                    return Err(RuntimeError::InvalidOperation(
                        format!("Unsupported modifier: {}", s).into(),
                    ));
                }
            };
            Ok(ast_wrapper(ASTNode {
                node_type: ASTNodeType::Modifier(mod_type),
                children: vec![],
                source_location: None,
            }))
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'name' for modifier() must be a string".into(),
        )),
    })
}

/// 构造 LambdaDef 类型 AST 节点。
fn lambda_def(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let is_dyn = argument.get("dyn").ok_or_else(|| {
        RuntimeError::InvalidOperation("lambda_def() requires a 'dyn' argument".into())
    })?;
    let captures = argument.get("captures").ok_or_else(|| {
        RuntimeError::InvalidOperation("lambda_def() requires a 'captures' argument".into())
    })?;
    is_dyn.weak().with_data(|data| match data {
        OnionObject::Boolean(is_dyn) => {
            captures
                .weak()
                .with_data(|captures_data| match captures_data {
                    OnionObject::Tuple(captured) => {
                        let mut captures = HashSet::new();
                        for v in captured.get_elements() {
                            let key = v.with_data(|data| match data {
                                OnionObject::String(s) => Ok(s.to_string()),
                                _ => Err(RuntimeError::InvalidType(
                                    "Capture variable must be a string".into(),
                                )),
                            })?;
                            captures.insert(key);
                        }
                        Ok(ast_wrapper(ASTNode {
                            node_type: ASTNodeType::LambdaDef(*is_dyn, captures),
                            children: vec![],
                            source_location: None,
                        }))
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Argument 'captures' for lambda_def() must be a tuple".into(),
                    )),
                })
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'name' for lambda_def() must be a string".into(),
        )),
    })
}

fn from_onion(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let value = argument.get("value").ok_or_else(|| {
        RuntimeError::InvalidOperation("from() requires a 'value' argument".into())
    })?;

    value
        .weak()
        .with_data(|data| OnionASTObject::from_onion(data).map(ast_wrapper))
}

/// 构建 AST 构造模块，导出所有节点构造函数和常量。
///
/// 返回值为 VM 可用的静态对象（Tuple of Pair）。
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // Inject CONSTANTS for data-less ASTNodeTypes
    let data_less_nodes = [
        ("null", ASTNodeType::Null),
        ("undefined", ASTNodeType::Undefined),
        ("frame", ASTNodeType::Frame),
        ("assign", ASTNodeType::Assign),
        ("expressions", ASTNodeType::Expressions),
        ("apply", ASTNodeType::Apply),
        ("tuple", ASTNodeType::Tuple),
        ("assume_tuple", ASTNodeType::AssumeTuple),
        ("pair", ASTNodeType::Pair),
        ("getattr", ASTNodeType::GetAttr),
        ("return", ASTNodeType::Return),
        ("if", ASTNodeType::If),
        ("while", ASTNodeType::While),
        ("break", ASTNodeType::Break),
        ("continue", ASTNodeType::Continue),
        ("range", ASTNodeType::Range),
        ("in", ASTNodeType::In),
        ("set", ASTNodeType::LazySet),
        ("map", ASTNodeType::Map),
        ("is", ASTNodeType::Is),
        ("raise", ASTNodeType::Raise),
        ("dynamic", ASTNodeType::Dynamic),
        ("static", ASTNodeType::Static),
        ("comptime", ASTNodeType::Comptime),
    ];

    for (name, node_type) in data_less_nodes {
        module.insert(
            name.to_string(),
            ast_wrapper(ASTNode {
                node_type,
                children: vec![],
                source_location: None,
            }),
        );
    }

    // Inject FUNCTIONS for data-carrying ASTNodeTypes
    let builders: Vec<(
        &str,
        fn(
            &OnionFastMap<Box<str>, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>,
        Vec<&str>,
    )> = vec![
        ("string", string, vec!["value"]),
        ("boolean", boolean, vec!["value"]),
        ("number", number, vec!["value"]),
        ("bytes", bytes, vec!["value"]),
        ("variable", variable, vec!["name"]),
        ("let", let_var, vec!["name"]),
        ("operation", operation, vec!["op"]),
        ("modifier", modifier, vec!["name"]),
        ("lambda_def", lambda_def, vec!["dyn", "captures"]),
        ("serialize", serialize_ast, vec!["value"]),
        ("deserialize", deserialize_ast, vec!["value"]),
        ("from", from_onion, vec!["value"]),
    ];

    for (name, func, params) in builders {
        // CORRECTED: Build the LambdaParameter structure correctly.
        let param_def = if params.len() == 1 {
            LambdaParameter::top(params[0])
        } else {
            LambdaParameter::Multiple(
                params
                    .iter()
                    .map(|&p| LambdaParameter::top(p))
                    .collect::<Vec<_>>()
                    .into(),
            )
        };

        let key_pool = OnionKeyPool::create(params.into_iter().map(|s| s.into()).collect());

        module.insert(
            name.to_string(),
            wrap_native_function(
                param_def, // Use the correctly constructed parameter definition
                OnionFastMap::default(),
                &format!("ast::{}", name),
                key_pool,
                Arc::new(func),
            ),
        );
    }

    build_dict(module)
}
