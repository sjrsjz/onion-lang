//! Onion 键值对（Pair）类型模块。
//!
//! 提供 Onion 语言运行时的键值对类型实现，支持通用对象作为键和值，
//! 并支持 GC 跟踪、静态构造、属性访问等功能。
//!
//! # 主要功能
//! - 键值对的构造与静态化
//! - 键、值的访问与升级
//! - 键值对的等价性判断
//! - 属性访问代理

use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};

use crate::lambda::runnable::RuntimeError;

use super::object::{OnionObject, OnionObjectCell, OnionStaticObject};

/// Onion 键值对类型。
///
/// 封装通用对象作为键和值，支持 GC 跟踪与静态化。
///
/// # 字段
/// - `key`: 键对象
/// - `value`: 值对象
pub struct OnionPair {
    key: OnionObject,
    value: OnionObject,
}

impl GCTraceable<OnionObjectCell> for OnionPair {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        self.key.collect(queue);
        self.value.collect(queue);
    }
}

impl Debug for OnionPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} : {:?}", self.key, self.value)
    }
}

impl OnionPair {
    /// 创建新的键值对。
    ///
    /// # 参数
    /// - `key`: 键对象
    /// - `value`: 值对象
    ///
    /// # 返回
    /// 新的键值对实例
    pub fn new(key: OnionObject, value: OnionObject) -> Self {
        OnionPair {
            key: key.into(),
            value: value.into(),
        }
    }

    /// 创建静态键值对对象。
    ///
    /// # 参数
    /// - `key`: 静态键对象
    /// - `value`: 静态值对象
    ///
    /// # 返回
    /// 稳定化后的静态键值对对象
    pub fn new_static(key: &OnionStaticObject, value: &OnionStaticObject) -> OnionStaticObject {
        OnionObject::Pair(
            OnionPair {
                key: key.weak().clone(),
                value: value.weak().clone(),
            }
            .into(),
        )
        .consume_and_stabilize()
    }

    /// 获取键对象的引用。
    #[inline(always)]
    pub fn get_key(&self) -> &OnionObject {
        &self.key
    }

    /// 获取值对象的引用。
    #[inline(always)]
    pub fn get_value(&self) -> &OnionObject {
        &self.value
    }

    /// 升级键和值的对象引用。
    ///
    /// 用于 GC 跟踪，防止键和值被提前回收。
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.key.upgrade(collected);
        self.value.upgrade(collected)
    }
}

impl OnionPair {
    pub fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::Pair(pair) => {
                if self.key.equals(&pair.key)? && self.value.equals(&pair.value)? {
                    return Ok(true);
                }
            }
            _ => {}
        }
        Ok(false)
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        self.value
            .with_attribute(key, f)
            .or_else(|_| self.key.with_attribute(key, f))
    }
}
