//! Onion 元组（Tuple）类型模块。
//!
//! 提供 Onion 语言运行时的元组类型实现，支持任意对象的有序集合表达，
//! 并支持 GC 跟踪、静态构造、索引访问、属性查找等功能。
//!
//! # 主要功能
//! - 元组的构造与静态化
//! - 元素访问、索引与属性查找
//! - 元组拼接与包含关系判断
//! - GC 跟踪与升级

use std::{collections::VecDeque, fmt::Debug, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};

use crate::lambda::runnable::RuntimeError;

use super::object::{OnionObject, OnionObjectCell, OnionStaticObject};

/// Onion 元组类型。
///
/// 封装有序对象集合，支持高效的索引访问与属性查找。
///
/// # 字段
/// - `elements`: 元组元素的有序对象数组
#[derive(Clone)]
pub struct OnionTuple {
    elements: Arc<[OnionObject]>,
}

impl GCTraceable<OnionObjectCell> for OnionTuple {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        for element in self.elements.as_ref() {
            element.collect(queue);
        }
    }
}

impl Debug for OnionTuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.elements.len() {
            0 => write!(f, "()"),
            1 => write!(f, "({:?},)", self.elements[0]),
            _ => {
                let elements: Vec<String> =
                    self.elements.iter().map(|e| format!("{:?}", e)).collect();
                write!(f, "({})", elements.join(", "))
            }
        }
    }
}

#[macro_export]
macro_rules! onion_tuple {
    ($($x:expr),*) => {
        OnionTuple::new_static(vec![$($x),*])
    };
    () => {

    };
}

impl OnionTuple {
    /// 创建新的元组。
    ///
    /// # 参数
    /// - `elements`: 元组元素对象列表
    ///
    /// # 返回
    /// 新的元组实例
    pub fn new(elements: Vec<OnionObject>) -> Self {
        OnionTuple {
            elements: elements.into(),
        }
    }

    /// 创建静态元组对象（元素为静态对象）。
    ///
    /// # 参数
    /// - `elements`: 静态对象引用列表
    ///
    /// # 返回
    /// 稳定化后的静态元组对象
    pub fn new_static(elements: Vec<&OnionStaticObject>) -> OnionStaticObject {
        OnionStaticObject::new(OnionObject::Tuple(
            OnionTuple {
                elements: elements
                    .into_iter()
                    .map(|e| e.weak().clone())
                    .collect::<Vec<_>>()
                    .into(),
            }
            .into(),
        ))
    }

    /// 创建静态元组对象（元素为静态对象，参数为 Vec 所有权）。
    ///
    /// # 参数
    /// - `elements`: 静态对象列表
    ///
    /// # 返回
    /// 稳定化后的静态元组对象
    pub fn new_static_no_ref(elements: &Vec<OnionStaticObject>) -> OnionStaticObject {
        OnionObject::Tuple(
            OnionTuple {
                elements: elements
                    .into_iter()
                    .map(|e| e.weak().clone())
                    .collect::<Vec<_>>()
                    .into(),
            }
            .into(),
        )
        .consume_and_stabilize()
    }

    /// 创建静态元组对象（元素为静态对象，参数为切片）。
    ///
    /// # 参数
    /// - `elements`: 静态对象切片
    ///
    /// # 返回
    /// 稳定化后的静态元组对象
    pub fn new_from_slice(elements: &[OnionStaticObject]) -> OnionStaticObject {
        OnionObject::Tuple(
            OnionTuple {
                elements: elements
                    .iter()
                    .map(|e| e.weak().clone())
                    .collect::<Vec<_>>()
                    .into(),
            }
            .into(),
        )
        .consume_and_stabilize()
    }

    /// 获取元组元素的引用。
    #[inline(always)]
    pub fn get_elements(&self) -> &[OnionObject] {
        self.elements.as_ref()
    }

    /// 升级元组中所有元素的对象引用。
    ///
    /// 用于 GC 跟踪，防止元素被提前回收。
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.elements.iter().for_each(|e| e.upgrade(collected));
    }

    /// 获取元组长度。
    ///
    /// # 返回
    /// 元组长度的静态整数对象
    pub fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(OnionStaticObject::new(OnionObject::Integer(
            self.elements.len() as i64,
        )))
    }

    /// 按索引访问元组元素。
    ///
    /// # 参数
    /// - `index`: 元组索引（从 0 开始）
    ///
    /// # 返回
    /// 对应索引的静态对象
    ///
    /// # 错误
    /// - `InvalidOperation`: 索引越界
    pub fn at(&self, index: i64) -> Result<OnionStaticObject, RuntimeError> {
        if index < 0 || index >= self.elements.len() as i64 {
            return Err(RuntimeError::InvalidOperation(
                format!("Index out of bounds: {}", index).into(),
            ));
        }
        Ok(OnionStaticObject::new(
            self.elements[index as usize].clone(),
        ))
    }

    /// 按索引借用元组元素并应用函数。
    ///
    /// # 参数
    /// - `index`: 元组索引
    /// - `f`: 处理函数
    ///
    /// # 返回
    /// 处理函数的返回值
    ///
    /// # 错误
    /// - `InvalidOperation`: 索引越界
    pub fn with_index<F, R>(&self, index: i64, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        if index < 0 || index >= self.elements.len() as i64 {
            return Err(RuntimeError::InvalidOperation(
                format!("Index out of bounds: {}", index).into(),
            ));
        }
        let borrowed = &self.elements[index as usize];
        f(borrowed)
    }

    /// 按属性名查找元组中的键值对元素。
    ///
    /// # 参数
    /// - `key`: 属性名对象
    /// - `f`: 处理函数，对应值作为参数
    ///
    /// # 返回
    /// - `Ok(R)`: 查找成功，返回处理函数结果
    /// - `Err(RuntimeError)`: 未找到属性
    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        for element in self.elements.as_ref() {
            match element {
                OnionObject::Pair(pair) => {
                    if pair.get_key().equals(key)? {
                        return f(&pair.get_value());
                    }
                }
                _ => {}
            }
        }
        Err(RuntimeError::InvalidOperation(
            format!("Attribute {:?} not found in tuple", key).into(),
        ))
    }

    /// 元组拼接（二元加法）。
    ///
    /// # 参数
    /// - `other`: 另一个元组对象
    ///
    /// # 返回
    /// 拼接后的新元组对象
    pub fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::Tuple(other_tuple) => {
                let new_elements: Arc<[OnionObject]> = self
                    .elements
                    .iter()
                    .chain(other_tuple.elements.iter())
                    .cloned()
                    .collect();
                Ok(OnionStaticObject::new(OnionObject::Tuple(
                    OnionTuple {
                        elements: new_elements,
                    }
                    .into(),
                )))
            }
            _ => Ok(OnionStaticObject::new(OnionObject::Undefined(Some(
                format!("Cannot add tuple with {:?}", other).into(),
            )))),
        }
    }

    /// 判断元组是否包含指定对象。
    ///
    /// # 参数
    /// - `other`: 要查找的对象
    ///
    /// # 返回
    /// - `true`: 包含
    /// - `false`: 不包含
    pub fn contains(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        for element in self.elements.as_ref() {
            if element.equals(other)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl OnionTuple {
    pub fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::Tuple(other_tuple) => {
                if self.elements.len() != other_tuple.elements.len() {
                    return Ok(false);
                }
                for (a, b) in self.elements.iter().zip(other_tuple.elements.as_ref()) {
                    if a.equals(b)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }
}
