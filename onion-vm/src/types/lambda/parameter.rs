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

#[derive(Debug, Clone)]
pub enum LambdaParameter {
    Single((Box<str>, OnionObject)),
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
    pub fn top(key: &str) -> Self {
        Self::Single((key.into(), OnionObject::Boolean(true)))
    }

    pub fn bottom(key: &str) -> Self {
        Self::Single((key.into(), OnionObject::Boolean(false)))
    }
}

impl LambdaParameter {
    pub fn len(&self) -> usize {
        match self {
            LambdaParameter::Single(_) => 1,
            LambdaParameter::Multiple(params) => params.iter().map(LambdaParameter::len).sum(),
        }
    }

    /// 按照扁平化的方式依照顺序获取参数的约束定义
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

    /// 打包成一个对象
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
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        match self {
            Self::Single((_, v)) => v.upgrade(collected),
            Self::Multiple(v) => v.iter().for_each(|e| e.upgrade(collected)),
        }
    }
}

impl LambdaParameter {
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
    /// Recursively flattens the parameter structure to produce a flat list of parameter names.
    ///
    /// For a parameter structure like `(a, (b, c))`, this will return `["a", "b", "c"]`.
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

    /// Recursively flattens the parameter structure to produce a flat list of constraint objects.
    ///
    /// For a parameter structure like `(a: Int, (b: String, c: Bool))`,
    /// this will return a Vec containing the OnionObject for `Int`, `String`, and `Bool`.
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
