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
