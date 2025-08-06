//! Lambda 参数定义与处理模块。
//!
//! 支持单参数和多参数的递归定义，提供参数约束、解包、扁平化等功能。
//! 参数可以是简单的键值对，也可以是嵌套的参数结构。

use std::fmt::Display;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    lambda::runnable::RuntimeError,
    types::{
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        pair::OnionPair,
        tuple::OnionTuple,
    },
};

/// Lambda 参数定义。
///
/// 支持单参数和多参数的递归结构：
/// - `Single`: 单个参数，包含参数名和约束对象
/// - `Multiple`: 多个参数，支持嵌套结构
///
/// # 示例
/// ```ignore
/// // 单参数: x : Int
/// LambdaParameter::Single(("x".into(), OnionObject::Integer(0)))
///
/// // 多参数: (a : String, (b : Int, c : Bool))
/// LambdaParameter::Multiple(box [
///     LambdaParameter::Single(("a".into(), OnionObject::String(...))),
///     LambdaParameter::Multiple(box [...])
/// ])
/// ```
#[derive(Debug, Clone)]
pub enum LambdaParameter {
    /// 单个参数：(参数名, 约束对象)
    Single((Box<str>, OnionObject)),
    /// 多个参数：递归参数列表
    Multiple(Box<[LambdaParameter]>),
}

impl GCTraceable<OnionObjectCell> for LambdaParameter {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<OnionObjectCell>>,
    ) {
        match self {
            LambdaParameter::Single((_, obj)) => {
                obj.collect(queue);
            }
            LambdaParameter::Multiple(params) => {
                for param in params {
                    param.collect(queue);
                }
            }
        }
    }
}

impl Display for LambdaParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LambdaParameter::Single((key, obj)) => {
                write!(f, "{} : {:?}", key, obj)
            }
            LambdaParameter::Multiple(params) => {
                write!(
                    f,
                    "({})",
                    params
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

impl LambdaParameter {
    /// 创建 top 参数（约束为 true，顶类型）。
    pub fn top(key: &str) -> Self {
        Self::Single((key.into(), OnionObject::Boolean(true)))
    }

    /// 创建 bottom 参数（约束为 false，底类型）。
    pub fn bottom(key: &str) -> Self {
        Self::Single((key.into(), OnionObject::Boolean(false)))
    }
}

impl LambdaParameter {
    /// 获取参数的总数量（扁平化后）。
    ///
    /// 递归计算所有嵌套参数的数量总和。
    pub fn len(&self) -> usize {
        match self {
            LambdaParameter::Single(_) => 1,
            LambdaParameter::Multiple(params) => params.iter().map(LambdaParameter::len).sum(),
        }
    }

    /// 按扁平化顺序获取指定索引的参数约束定义。
    ///
    /// # 参数
    /// - `index`: 扁平化后的参数索引
    ///
    /// # 返回
    /// - `Some(&OnionObject)`: 对应位置的约束对象
    /// - `None`: 索引超出范围
    pub fn constraint_at(&self, index: usize) -> Option<&OnionObject> {
        match self {
            LambdaParameter::Single((_, obj)) => {
                if index == 0 {
                    Some(obj)
                } else {
                    None
                }
            }
            LambdaParameter::Multiple(params) => {
                let mut curr = index;
                for param in params {
                    let len = param.len();
                    if curr < len {
                        return param.constraint_at(curr);
                    } else {
                        curr -= len;
                    }
                }
                None
            }
        }
    }

    /// 按扁平化顺序获取指定索引的参数名。
    #[allow(dead_code)]
    pub fn key_at(&self, index: usize) -> Option<&str> {
        match self {
            LambdaParameter::Single((key, _)) => {
                if index == 0 {
                    Some(key)
                } else {
                    None
                }
            }
            LambdaParameter::Multiple(params) => {
                let mut curr = index;
                for param in params {
                    let len = param.len();
                    if curr < len {
                        return param.key_at(curr);
                    } else {
                        curr -= len;
                    }
                }
                None
            }
        }
    }

    /// 按扁平化顺序获取指定索引的参数名和约束。
    #[allow(dead_code)]
    pub fn at(&self, index: usize) -> Option<(&str, &OnionObject)> {
        match self {
            LambdaParameter::Single((key, obj)) => {
                if index == 0 {
                    Some((key, obj))
                } else {
                    None
                }
            }
            LambdaParameter::Multiple(params) => {
                let mut curr = index;
                for param in params {
                    let len = param.len();
                    if curr < len {
                        return param.at(curr);
                    } else {
                        curr -= len;
                    }
                }
                None
            }
        }
    }

    /// 将参数结构打包为 Onion 对象。
    ///
    /// - 单参数打包为 Pair 对象
    /// - 多参数打包为 Tuple 对象，递归处理嵌套结构
    ///
    /// # 返回
    /// 稳定化的 Onion 对象，可用于序列化或传输
    pub fn to_onion(&self) -> OnionStaticObject {
        fn inner(param: &LambdaParameter) -> OnionObject {
            match param {
                LambdaParameter::Single((key, obj)) => OnionObject::Pair(
                    OnionPair::new(OnionObject::String(key.clone().into()), obj.clone()).into(),
                ),
                LambdaParameter::Multiple(params) => {
                    let mut pairs = vec![];
                    for param in params {
                        pairs.push(inner(param));
                    }
                    OnionObject::Tuple(OnionTuple::new(pairs).into())
                }
            }
        }
        inner(self).consume_and_stabilize()
    }

    /// 从 Onion 对象解析参数结构。
    ///
    /// 支持从 Pair、Tuple、String 等对象解析参数：
    /// - Pair: 解析为单参数
    /// - Tuple: 解析为多参数，递归处理
    /// - String: 解析为简单参数（约束为 true）
    ///
    /// # 错误
    /// 当对象格式不符合预期时返回 `RuntimeError::InvalidType`
    pub fn from_onion(obj: &OnionObject) -> Result<LambdaParameter, RuntimeError> {
        fn inner(obj: &OnionObject) -> Result<LambdaParameter, RuntimeError> {
            match obj {
                OnionObject::Pair(pair) => {
                    let key = match pair.get_key() {
                        OnionObject::String(s) => s.as_ref(),
                        _ => {
                            return Err(RuntimeError::InvalidType(
                                format!("Expected string key, found: {:?}", obj).into(),
                            ));
                        }
                    };
                    let value = pair.get_value();
                    Ok(LambdaParameter::Single((key.into(), value.clone())))
                }
                OnionObject::Tuple(tuple) => {
                    let mut params = Vec::with_capacity(tuple.get_elements().len());
                    for item in tuple.get_elements().iter() {
                        params.push(inner(item)?);
                    }
                    Ok(LambdaParameter::Multiple(params.into_boxed_slice()))
                }
                OnionObject::String(s) => {
                    // 处理单个字符串参数
                    Ok(LambdaParameter::Single((
                        Box::from(s.as_ref()),
                        OnionObject::Boolean(true),
                    )))
                }
                _ => Err(RuntimeError::InvalidType(
                    format!("Expected one of pair, tuple, or string, found: {:?}", obj).into(),
                )),
            }
        }
        inner(obj)
    }
}

impl LambdaParameter {
    /// 升级参数中的弱引用为强引用。
    ///
    /// 遍历参数结构，将所有对象的弱引用升级为强引用，
    /// 防止在垃圾回收过程中被意外回收。
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        match self {
            Self::Single((_, v)) => v.upgrade(collected),
            Self::Multiple(v) => v.iter().for_each(|e| e.upgrade(collected)),
        }
    }
}

impl LambdaParameter {
    /// 根据参数结构解包实际参数。
    ///
    /// 将传入的参数对象按照当前参数结构进行解包：
    /// - 单参数：直接接受任意对象
    /// - 多参数：要求传入 Tuple，且元素数量匹配
    ///
    /// # 参数
    /// - `argument`: 待解包的参数对象
    ///
    /// # 返回
    /// 解包后的参数列表，按扁平化顺序排列
    ///
    /// # 错误
    /// - `Arity Mismatch`: 参数数量不匹配
    /// - `InvalidType`: 期望 Tuple 但传入其他类型
    pub fn unpack_arguments(
        &self,
        argument: &OnionObject,
    ) -> Result<Vec<OnionObject>, RuntimeError> {
        let mut collected = Vec::with_capacity(self.len());

        fn inner(
            object: &OnionObject,
            layer: &LambdaParameter,
            collected: &mut Vec<OnionObject>,
        ) -> Result<(), RuntimeError> {
            match layer {
                LambdaParameter::Single(_) => {
                    // If expecting one param, the argument can be a single object.
                    collected.push(object.clone());
                    Ok(())
                }
                LambdaParameter::Multiple(v) => {
                    // If expecting multiple, the argument must be a tuple.
                    object.with_data(|data| match data {
                        OnionObject::Tuple(tuple) => {
                            if tuple.get_elements().len() != v.len() {
                                return Err(RuntimeError::InvalidOperation(
                                    "Arity Mismatch".into(),
                                ));
                            }
                            for (i, object) in tuple.get_elements().iter().enumerate() {
                                inner(object, &v[i], collected)?;
                            }
                            Ok(())
                        }
                        _ => Err(RuntimeError::InvalidType(
                            format!("Expected tuple, found: {:?}", data).into(),
                        )),
                    })
                }
            }
        }

        inner(argument, &self, &mut collected)?;
        Ok(collected)
    }
}

impl LambdaParameter {
    /// 递归扁平化参数结构，生成参数名列表。
    ///
    /// 将嵌套的参数结构展开为一维的参数名列表。
    /// 例如：`(a, (b, c))` 会返回 `["a", "b", "c"]`。
    ///
    /// # 返回
    /// 按深度优先顺序排列的参数名列表
    pub fn flatten_keys(&self) -> Box<[Box<str>]> {
        let mut keys = Vec::with_capacity(self.len());
        fn inner(param: &LambdaParameter, collected_keys: &mut Vec<Box<str>>) {
            match param {
                LambdaParameter::Single((name, _)) => {
                    collected_keys.push(name.clone());
                }
                LambdaParameter::Multiple(params) => {
                    for sub_param in params {
                        inner(sub_param, collected_keys);
                    }
                }
            }
        }
        inner(self, &mut keys);
        keys.into_boxed_slice()
    }

    /// 递归扁平化参数结构，生成约束对象列表。
    ///
    /// 将嵌套的参数结构展开为一维的约束对象列表。
    /// 例如：`(a: Int, (b: String, c: Bool))` 会返回 `[Int, String, Bool]`。
    ///
    /// # 返回
    /// 按深度优先顺序排列的约束对象列表
    pub fn flatten_constraints(&self) -> Box<[OnionObject]> {
        let mut constraints = Vec::with_capacity(self.len());

        fn inner(param: &LambdaParameter, collected_constraints: &mut Vec<OnionObject>) {
            match param {
                LambdaParameter::Single((_, constraint_obj)) => {
                    collected_constraints.push(constraint_obj.clone());
                }
                LambdaParameter::Multiple(params) => {
                    for sub_param in params {
                        inner(sub_param, collected_constraints);
                    }
                }
            }
        }
        inner(self, &mut constraints);
        constraints.into_boxed_slice()
    }
}
