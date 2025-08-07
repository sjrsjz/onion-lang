//! 编译时模块：提供 AST 与 Onion VM 对象间的双向转换和集成。
//!
//! 本模块实现了编译时 AST 节点的 VM 对象包装、绑定、以及相互转换机制。
//! 主要类型 `OnionASTObject` 实现了 `OnionObjectExt` trait，使 AST 能在 VM 中以对象形式操作。
//! 支持 AST <-> VM 对象的双向转换、序列化、运算符重载等。
//!
//! # 主要内容
//! - `OnionASTObject`：AST 的 VM 对象包装器
//! - AST 与 VM 基础类型（String/Boolean/Number/Tuple/Pair 等）的互转
//! - 运算符重载（如 `<<` 用于替换子节点）
//! - 子模块：`ast_bindings`（AST 构造绑定）、`native`（原生函数）、`solver`（求解器）
//!
//! # 用法示例
//! ```ignore
//! let ast_obj = OnionASTObject::new(ast_node);
//! let vm_obj = OnionObject::Custom(Arc::new(ast_obj));
//! let converted_back = OnionASTObject::from_onion(&vm_obj)?;
//! ```

pub mod ast_bindings;
pub mod native;
pub mod solver;
use std::sync::Arc;

use onion_vm::{
    GCTraceable,
    lambda::runnable::RuntimeError,
    types::{
        object::{OnionObject, OnionObjectCell, OnionObjectExt, OnionStaticObject},
        tuple::OnionTuple,
    },
};

use crate::parser::ast::{ASTNode, ASTNodeType};
use base64::engine::Engine;

/// AST 节点的 VM 对象包装器。
///
/// 将 AST 节点包装为 Onion VM 可操作的自定义对象，实现 VM 对象接口，
/// 支持相等性比较、序列化、运算符重载、类型查询等。
#[derive(Debug, Clone)]
pub struct OnionASTObject {
    ast: ASTNode,
}

impl GCTraceable<OnionObjectCell> for OnionASTObject {
    fn collect(&self, _: &mut std::collections::VecDeque<onion_vm::GCArcWeak<OnionObjectCell>>) {}
}

impl OnionObjectExt for OnionASTObject {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, _: &mut Vec<onion_vm::GCArc<OnionObjectCell>>) {}

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        other.with_data(|data| match data {
            OnionObject::Custom(data) => {
                if let Some(ast_object) = data.as_any().downcast_ref::<OnionASTObject>() {
                    Ok(self.ast == ast_object.ast)
                } else {
                    Ok(false)
                }
            }
            _ => Ok(false),
        })
    }

    fn is_same(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        self.equals(other)
    }

    fn key_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        // 获取一个移除了子节点的 AST 节点
        let key_ast = ASTNode {
            node_type: self.ast.node_type.clone(),
            children: vec![],
            source_location: self.ast.source_location.clone(),
        };
        Ok(OnionObject::Custom(Arc::new(OnionASTObject { ast: key_ast })).stabilize())
    }

    fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        // 获取 AST 的子节点列表
        let children = self
            .ast
            .children
            .iter()
            .map(|child| OnionObject::Custom(Arc::new(OnionASTObject { ast: child.clone() })))
            .collect::<Vec<_>>();
        Ok(OnionObject::Tuple(OnionTuple::new(children).into()).stabilize())
    }

    fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(OnionObject::Integer(self.ast.children.len() as i64).stabilize())
    }

    fn apply(&self, value: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        value.with_data(|data| match data {
            OnionObject::Integer(i) => {
                // 通过索引访问子节点
                let index = if *i < 0 {
                    return Err(RuntimeError::InvalidOperation(
                        "Negative index is not allowed".into(),
                    ));
                } else {
                    *i as usize
                };

                if index >= self.ast.children.len() {
                    return Err(RuntimeError::InvalidOperation(
                        format!(
                            "Index {} out of bounds for AST children of length {}",
                            index,
                            self.ast.children.len()
                        )
                        .into(),
                    ));
                }

                let child_ast = self.ast.children[index].clone();
                Ok(OnionObject::Custom(Arc::new(OnionASTObject { ast: child_ast })).stabilize())
            }
            OnionObject::Pair(pair) => {
                // 通过 Pair 替换指定索引的子节点
                let index = pair.get_key().with_data(|key_data| match key_data {
                    OnionObject::Integer(i) => {
                        if *i < 0 {
                            Err(RuntimeError::InvalidOperation(
                                "Negative index is not allowed".into(),
                            ))
                        } else {
                            Ok(*i as usize)
                        }
                    }
                    _ => Err(RuntimeError::InvalidType(
                        "Pair key must be an integer index".into()
                    )),
                })?;

                if index >= self.ast.children.len() {
                    return Err(RuntimeError::InvalidOperation(
                        format!(
                            "Index {} out of bounds for AST children of length {}",
                            index,
                            self.ast.children.len()
                        )
                        .into(),
                    ));
                }

                // 将 Pair 的值转换为 AST 节点
                let new_child_ast = OnionASTObject::from_onion(pair.get_value())?;

                // 创建新的子节点列表，替换指定索引的节点
                let mut new_children = self.ast.children.clone();
                new_children[index] = new_child_ast;

                // 创建新的 AST 对象
                let new_ast = ASTNode {
                    node_type: self.ast.node_type.clone(),
                    source_location: self.ast.source_location.clone(),
                    children: new_children,
                };

                Ok(OnionObject::Custom(Arc::new(OnionASTObject { ast: new_ast })).stabilize())
            }
            _ => Err(RuntimeError::InvalidType(
                "Apply argument must be an integer (for access) or a pair (for replacement)".into()
            )),
        })
    }

    fn with_attribute(
            &self,
            key: &OnionObject,
            f: &mut dyn FnMut(&OnionObject) -> Result<(), RuntimeError>,
        ) -> Result<(), RuntimeError> {
        key.with_data(|key_data| match key_data {
            OnionObject::String(attr_name) => {
                match attr_name.as_ref() {
                    "node_type" => {
                        // 返回节点类型的字符串表示
                        let type_name = match &self.ast.node_type {
                            ASTNodeType::Null => "Null",
                            ASTNodeType::Undefined => "Undefined",
                            ASTNodeType::String(_) => "String",
                            ASTNodeType::Boolean(_) => "Boolean",
                            ASTNodeType::Number(_) => "Number",
                            ASTNodeType::Base64(_) => "Base64",
                            ASTNodeType::Variable(_) => "Variable",
                            ASTNodeType::Required(_) => "Required",
                            ASTNodeType::Let(_) => "Let",
                            ASTNodeType::Frame => "Frame",
                            ASTNodeType::Assign => "Assign",
                            ASTNodeType::LambdaDef(_, _) => "LambdaDef",
                            ASTNodeType::Expressions => "Expressions",
                            ASTNodeType::Apply => "Apply",
                            ASTNodeType::Operation(_) => "Operation",
                            ASTNodeType::Tuple => "Tuple",
                            ASTNodeType::AssumeTuple => "AssumeTuple",
                            ASTNodeType::Pair => "Pair",
                            ASTNodeType::GetAttr => "GetAttr",
                            ASTNodeType::Return => "Return",
                            ASTNodeType::If => "If",
                            ASTNodeType::While => "While",
                            ASTNodeType::Modifier(_) => "Modifier",
                            ASTNodeType::Break => "Break",
                            ASTNodeType::Continue => "Continue",
                            ASTNodeType::Range => "Range",
                            ASTNodeType::In => "In",
                            ASTNodeType::Namespace(_) => "Namespace",
                            ASTNodeType::LazySet => "Set",
                            ASTNodeType::Map => "Map",
                            ASTNodeType::Is => "Is",
                            ASTNodeType::Raise => "Raise",
                            ASTNodeType::Dynamic => "Dynamic",
                            ASTNodeType::Static => "Static",
                            ASTNodeType::Comptime => "Comptime",
                        };
                        let type_obj = OnionObject::String(type_name.into());
                        f(&type_obj)
                    },
                    "has_data" => {
                        // 返回是否携带数据
                        let has_data = matches!(
                            &self.ast.node_type,
                            ASTNodeType::String(_) | ASTNodeType::Boolean(_) | ASTNodeType::Number(_) |
                            ASTNodeType::Base64(_) | ASTNodeType::Variable(_) | ASTNodeType::Required(_) |
                            ASTNodeType::Let(_) | ASTNodeType::LambdaDef(_, _) | ASTNodeType::Operation(_) |
                            ASTNodeType::Modifier(_) | ASTNodeType::Namespace(_)
                        );
                        let has_data_obj = OnionObject::Boolean(has_data);
                        f(&has_data_obj)
                    },
                    "data" => {
                        // 返回节点类型携带的原始数据
                        match &self.ast.node_type {
                            ASTNodeType::String(s) => {
                                let data_obj = OnionObject::String(s.clone().into());
                                f(&data_obj)
                            },
                            ASTNodeType::Boolean(b) => {
                                let data_obj = OnionObject::Boolean(*b);
                                f(&data_obj)
                            },
                            ASTNodeType::Number(n) => {
                                let data_obj = OnionObject::String(n.clone().into());
                                f(&data_obj)
                            },
                            ASTNodeType::Base64(b64) => {
                                let data_obj = OnionObject::String(b64.clone().into());
                                f(&data_obj)
                            },
                            ASTNodeType::Variable(name) | ASTNodeType::Required(name) | 
                            ASTNodeType::Let(name) | ASTNodeType::Namespace(name) => {
                                let data_obj = OnionObject::String(name.clone().into());
                                f(&data_obj)
                            },
                            ASTNodeType::LambdaDef(is_dyn, captures) => {
                                // 返回一个包含 is_dyn 和 captures 的元组
                                let captures_vec: Vec<OnionObject> = captures.iter()
                                    .map(|s| OnionObject::String(s.clone().into()))
                                    .collect();
                                let captures_tuple = OnionObject::Tuple(OnionTuple::new(captures_vec).into());
                                let data_tuple = OnionObject::Tuple(OnionTuple::new(vec![
                                    OnionObject::Boolean(*is_dyn),
                                    captures_tuple
                                ]).into());
                                f(&data_tuple)
                            },
                            ASTNodeType::Operation(op) => {
                                let op_str = match op {
                                    crate::parser::ast::ASTNodeOperation::Add => "+",
                                    crate::parser::ast::ASTNodeOperation::Abs => "abs",
                                    crate::parser::ast::ASTNodeOperation::Subtract => "-",
                                    crate::parser::ast::ASTNodeOperation::Minus => "minus",
                                    crate::parser::ast::ASTNodeOperation::Multiply => "*",
                                    crate::parser::ast::ASTNodeOperation::Divide => "/",
                                    crate::parser::ast::ASTNodeOperation::Modulus => "%",
                                    crate::parser::ast::ASTNodeOperation::Power => "**",
                                    crate::parser::ast::ASTNodeOperation::And => "and",
                                    crate::parser::ast::ASTNodeOperation::Xor => "xor",
                                    crate::parser::ast::ASTNodeOperation::Or => "or",
                                    crate::parser::ast::ASTNodeOperation::Not => "not",
                                    crate::parser::ast::ASTNodeOperation::Equal => "==",
                                    crate::parser::ast::ASTNodeOperation::NotEqual => "!=",
                                    crate::parser::ast::ASTNodeOperation::Greater => ">",
                                    crate::parser::ast::ASTNodeOperation::Less => "<",
                                    crate::parser::ast::ASTNodeOperation::GreaterEqual => ">=",
                                    crate::parser::ast::ASTNodeOperation::LessEqual => "<=",
                                    crate::parser::ast::ASTNodeOperation::LeftShift => "<<",
                                    crate::parser::ast::ASTNodeOperation::RightShift => ">>",
                                };
                                let data_obj = OnionObject::String(op_str.into());
                                f(&data_obj)
                            },
                            ASTNodeType::Modifier(mod_type) => {
                                let mod_str = match mod_type {
                                    crate::parser::ast::ASTNodeModifier::Mut => "mut",
                                    crate::parser::ast::ASTNodeModifier::Const => "const",
                                    crate::parser::ast::ASTNodeModifier::KeyOf => "keyof",
                                    crate::parser::ast::ASTNodeModifier::ValueOf => "valueof",
                                    crate::parser::ast::ASTNodeModifier::Assert => "assert",
                                    crate::parser::ast::ASTNodeModifier::Import => "import",
                                    crate::parser::ast::ASTNodeModifier::TypeOf => "typeof",
                                    crate::parser::ast::ASTNodeModifier::LengthOf => "lengthof",
                                    crate::parser::ast::ASTNodeModifier::Launch => "launch",
                                    crate::parser::ast::ASTNodeModifier::Spawn => "spawn",
                                    crate::parser::ast::ASTNodeModifier::Async => "async",
                                    crate::parser::ast::ASTNodeModifier::Sync => "sync",
                                    crate::parser::ast::ASTNodeModifier::Atomic => "atomic",
                                };
                                let data_obj = OnionObject::String(mod_str.into());
                                f(&data_obj)
                            },
                            _ => {
                                // 对于没有数据的节点类型，返回 null
                                let null_obj = OnionObject::Null;
                                f(&null_obj)
                            }
                        }
                    },
                    // 直接通过数据字段名访问
                    "value" => {
                        match &self.ast.node_type {
                            ASTNodeType::String(s) | ASTNodeType::Number(s) | ASTNodeType::Base64(s) => {
                                let value_obj = OnionObject::String(s.clone().into());
                                f(&value_obj)
                            },
                            ASTNodeType::Boolean(b) => {
                                let value_obj = OnionObject::Boolean(*b);
                                f(&value_obj)
                            },
                            _ => Err(RuntimeError::InvalidOperation(
                                "Attribute 'value' is only supported for String, Number, Base64, and Boolean node types".into()
                            ))
                        }
                    },
                    "name" => {
                        match &self.ast.node_type {
                            ASTNodeType::Variable(name) | ASTNodeType::Required(name) | 
                            ASTNodeType::Let(name) | ASTNodeType::Namespace(name) => {
                                let name_obj = OnionObject::String(name.clone().into());
                                f(&name_obj)
                            },
                            _ => Err(RuntimeError::InvalidOperation(
                                "Attribute 'name' is only supported for Variable, Required, Let, and Namespace node types".into()
                            ))
                        }
                    },
                    "op" => {
                        match &self.ast.node_type {
                            ASTNodeType::Operation(op) => {
                                let op_str = match op {
                                    crate::parser::ast::ASTNodeOperation::Add => "+",
                                    crate::parser::ast::ASTNodeOperation::Abs => "abs",
                                    crate::parser::ast::ASTNodeOperation::Subtract => "-",
                                    crate::parser::ast::ASTNodeOperation::Minus => "minus",
                                    crate::parser::ast::ASTNodeOperation::Multiply => "*",
                                    crate::parser::ast::ASTNodeOperation::Divide => "/",
                                    crate::parser::ast::ASTNodeOperation::Modulus => "%",
                                    crate::parser::ast::ASTNodeOperation::Power => "**",
                                    crate::parser::ast::ASTNodeOperation::And => "and",
                                    crate::parser::ast::ASTNodeOperation::Xor => "xor",
                                    crate::parser::ast::ASTNodeOperation::Or => "or",
                                    crate::parser::ast::ASTNodeOperation::Not => "not",
                                    crate::parser::ast::ASTNodeOperation::Equal => "==",
                                    crate::parser::ast::ASTNodeOperation::NotEqual => "!=",
                                    crate::parser::ast::ASTNodeOperation::Greater => ">",
                                    crate::parser::ast::ASTNodeOperation::Less => "<",
                                    crate::parser::ast::ASTNodeOperation::GreaterEqual => ">=",
                                    crate::parser::ast::ASTNodeOperation::LessEqual => "<=",
                                    crate::parser::ast::ASTNodeOperation::LeftShift => "<<",
                                    crate::parser::ast::ASTNodeOperation::RightShift => ">>",
                                };
                                let op_obj = OnionObject::String(op_str.into());
                                f(&op_obj)
                            },
                            _ => Err(RuntimeError::InvalidOperation(
                                "Attribute 'op' is only supported for Operation node type".into()
                            ))
                        }
                    },
                    "modifier" => {
                        match &self.ast.node_type {
                            ASTNodeType::Modifier(mod_type) => {
                                let mod_str = match mod_type {
                                    crate::parser::ast::ASTNodeModifier::Mut => "mut",
                                    crate::parser::ast::ASTNodeModifier::Const => "const",
                                    crate::parser::ast::ASTNodeModifier::KeyOf => "keyof",
                                    crate::parser::ast::ASTNodeModifier::ValueOf => "valueof",
                                    crate::parser::ast::ASTNodeModifier::Assert => "assert",
                                    crate::parser::ast::ASTNodeModifier::Import => "import",
                                    crate::parser::ast::ASTNodeModifier::TypeOf => "typeof",
                                    crate::parser::ast::ASTNodeModifier::LengthOf => "lengthof",
                                    crate::parser::ast::ASTNodeModifier::Launch => "launch",
                                    crate::parser::ast::ASTNodeModifier::Spawn => "spawn",
                                    crate::parser::ast::ASTNodeModifier::Async => "async",
                                    crate::parser::ast::ASTNodeModifier::Sync => "sync",
                                    crate::parser::ast::ASTNodeModifier::Atomic => "atomic",
                                };
                                let mod_obj = OnionObject::String(mod_str.into());
                                f(&mod_obj)
                            },
                            _ => Err(RuntimeError::InvalidOperation(
                                "Attribute 'modifier' is only supported for Modifier node type".into()
                            ))
                        }
                    },
                    "is_dyn" | "dyn" => {
                        match &self.ast.node_type {
                            ASTNodeType::LambdaDef(is_dyn, _) => {
                                let dyn_obj = OnionObject::Boolean(*is_dyn);
                                f(&dyn_obj)
                            },
                            _ => Err(RuntimeError::InvalidOperation(
                                "Attribute 'is_dyn'/'dyn' is only supported for LambdaDef node type".into()
                            ))
                        }
                    },
                    "captures" => {
                        match &self.ast.node_type {
                            ASTNodeType::LambdaDef(_, captures) => {
                                let captures_vec: Vec<OnionObject> = captures.iter()
                                    .map(|s| OnionObject::String(s.clone().into()))
                                    .collect();
                                let captures_obj = OnionObject::Tuple(OnionTuple::new(captures_vec).into());
                                f(&captures_obj)
                            },
                            _ => Err(RuntimeError::InvalidOperation(
                                "Attribute 'captures' is only supported for LambdaDef node type".into()
                            ))
                        }
                    },
                    _ => Err(RuntimeError::InvalidOperation(
                        format!("Unknown attribute '{}'. Available attributes: node_type, has_data, data, value (for String/Number/Base64/Boolean), name (for Variable/Required/Let/Namespace), op (for Operation), modifier (for Modifier), is_dyn/dyn (for LambdaDef), captures (for LambdaDef)", attr_name).into()
                    ))
                }
            },
            _ => Err(RuntimeError::InvalidType(
                "Attribute key must be a string".into()
            ))
        })
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("OnionASTObject".to_string())
    }

    fn to_string(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("OnionASTObject: {:?}", self.ast))
    }
    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("OnionASTObject: {:?}", self.ast))
    }

    fn binary_shl(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        // self << tuple 用来将自身的元组替换成另一个
        other.with_data(|data| match data {
            OnionObject::Tuple(tuple) => {
                let mut children = vec![];
                for elem in tuple.get_elements() {
                    children.push(OnionASTObject::from_onion(elem)?);
                }
                let new_ast = ASTNode {
                    node_type: self.ast.node_type.clone(),
                    source_location: self.ast.source_location.clone(),
                    children,
                };
                Ok(OnionObject::Custom(Arc::new(OnionASTObject { ast: new_ast })).stabilize())
            }
            _ => Err(RuntimeError::InvalidType(
                "Expected a tuple for binary shift left".into(),
            )),
        })
    }
}

impl OnionASTObject {
    /// 创建新的 AST 对象包装器。
    pub fn new(ast: ASTNode) -> Self {
        Self { ast }
    }

    /// 将 Onion VM 对象转换为 AST 节点。
    ///
    /// # 参数
    /// - `object`：VM 对象引用。
    ///
    /// # 返回
    /// 转换后的 AST 节点，或运行时错误。
    ///
    /// # 支持的类型
    /// - Custom(OnionASTObject)：直接提取 AST
    /// - Boolean/String/Bytes/Float/Integer/Null/Undefined：转为对应字面量节点
    /// - Tuple：转为 Tuple 节点（递归转换子元素）
    /// - Range：转为 Range 节点
    /// - Pair：转为 Pair 节点
    pub fn from_onion(object: &OnionObject) -> Result<ASTNode, RuntimeError> {
        match object {
            OnionObject::Custom(ast_object) => {
                if let Some(ast_object) = ast_object.as_any().downcast_ref::<OnionASTObject>() {
                    Ok(ast_object.ast.clone())
                } else {
                    Err(RuntimeError::InvalidType(
                        format!(
                            "Unsupported OnionObject type for AST conversion: {:?}",
                            object
                        )
                        .into(),
                    ))
                }
            }
            OnionObject::Mut(_) => Err(RuntimeError::InvalidOperation(
                ("Mutable objects may introduce cyclic references, ".to_owned()
                    + "which cannot be safely or deterministically converted to AST objects")
                    .into(),
            )),
            OnionObject::Boolean(v) => Ok(ASTNode {
                node_type: ASTNodeType::Boolean(*v),
                source_location: None,
                children: vec![],
            }),
            OnionObject::String(s) => Ok(ASTNode {
                node_type: ASTNodeType::String(s.as_ref().into()),
                source_location: None,
                children: vec![],
            }),
            OnionObject::Bytes(b) => {
                let b64 = base64::engine::general_purpose::STANDARD.encode(b);
                Ok(ASTNode {
                    node_type: ASTNodeType::Base64(b64),
                    source_location: None,
                    children: vec![],
                })
            }
            OnionObject::Float(f) => Ok(ASTNode {
                node_type: ASTNodeType::Number(f.to_string()),
                source_location: None,
                children: vec![],
            }),
            OnionObject::Integer(i) => Ok(ASTNode {
                node_type: ASTNodeType::Number(i.to_string()),
                source_location: None,
                children: vec![],
            }),
            OnionObject::Null => Ok(ASTNode {
                node_type: ASTNodeType::Null,
                source_location: None,
                children: vec![],
            }),
            OnionObject::Undefined(_) => Ok(ASTNode {
                node_type: ASTNodeType::Undefined,
                source_location: None,
                children: vec![],
            }),
            OnionObject::Tuple(tuple) => {
                let children = tuple
                    .get_elements()
                    .iter()
                    .map(|elem| OnionASTObject::from_onion(elem))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ASTNode {
                    node_type: ASTNodeType::Tuple,
                    source_location: None,
                    children,
                })
            }
            OnionObject::Range(start, end) => Ok(ASTNode {
                node_type: ASTNodeType::Range,
                source_location: None,
                children: vec![
                    ASTNode {
                        node_type: ASTNodeType::Number(start.to_string()),
                        source_location: None,
                        children: vec![],
                    },
                    ASTNode {
                        node_type: ASTNodeType::Number(end.to_string()),
                        source_location: None,
                        children: vec![],
                    },
                ],
            }),
            OnionObject::Pair(pair) => {
                let left = OnionASTObject::from_onion(pair.get_key())?;
                let right = OnionASTObject::from_onion(pair.get_value())?;
                Ok(ASTNode {
                    node_type: ASTNodeType::Pair,
                    source_location: None,
                    children: vec![left, right],
                })
            }
            v => Err(RuntimeError::InvalidType(
                format!("Unsupported OnionObject type for AST conversion: {:?}", v).into(),
            )),
        }
    }
}
