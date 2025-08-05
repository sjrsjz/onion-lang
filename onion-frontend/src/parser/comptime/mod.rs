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
    pub fn new(ast: ASTNode) -> Self {
        Self { ast }
    }
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
