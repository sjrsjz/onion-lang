use std::{collections::HashSet, sync::Arc};

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
fn ast_wrapper(ast: ASTNode) -> OnionStaticObject {
    OnionObject::Custom(Arc::new(OnionASTObject { ast })).stabilize()
}

// --- Builder functions for ASTNodeTypes that CARRY DATA ---

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
            start_token: None,
            end_token: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' for string() must be a string".into(),
        )),
    })
}

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
            start_token: None,
            end_token: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' for boolean() must be a boolean".into(),
        )),
    })
}

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
            start_token: None,
            end_token: None,
        })),
        OnionObject::Float(f) => Ok(ast_wrapper(ASTNode {
            node_type: ASTNodeType::Number(f.to_string()),
            children: vec![],
            start_token: None,
            end_token: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'value' for number() must be a number".into(),
        )),
    })
}

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
            start_token: None,
            end_token: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'name' for variable() must be a string".into(),
        )),
    })
}

fn let_(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let name = argument.get("name").ok_or_else(|| {
        RuntimeError::InvalidOperation("let_() requires a 'name' argument".into())
    })?;
    name.weak().with_data(|data| match data {
        OnionObject::String(s) => Ok(ast_wrapper(ASTNode {
            node_type: ASTNodeType::Let(s.to_string()),
            children: vec![],
            start_token: None,
            end_token: None,
        })),
        _ => Err(RuntimeError::InvalidType(
            "Argument 'name' for let_() must be a string".into(),
        )),
    })
}

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
                start_token: None,
                end_token: None,
            }))
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'op' for operation() must be a string".into(),
        )),
    })
}

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
                "await" => ASTNodeModifier::Await,
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
                start_token: None,
                end_token: None,
            }))
        }
        _ => Err(RuntimeError::InvalidType(
            "Argument 'name' for modifier() must be a string".into(),
        )),
    })
}

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
                            start_token: None,
                            end_token: None,
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

/// Build the main AST construction module, providing functions and constants.
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
        ("set", ASTNodeType::Set),
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
                start_token: None,
                end_token: None,
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
        ("variable", variable, vec!["name"]),
        ("let", let_, vec!["name"]),
        ("operation", operation, vec!["op"]),
        ("modifier", modifier, vec!["name"]),
        ("lambda_def", lambda_def, vec!["dyn", "captures"]),
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
