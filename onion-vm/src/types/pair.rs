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

use std::{collections::VecDeque, fmt::Debug, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};

use crate::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObjectProtocol, OnionObjectProtocolStatic},
};

use super::object::{OnionObject, OnionObjectCell, OnionStaticObject};

/// Onion 键值对类型。
///
/// 封装通用对象作为键和值，支持 GC 跟踪与静态化。
///
/// # 字段
/// - `value.0 (key)`: 键对象
/// - `value.1 (value)`: 值对象
#[derive(Clone)]
pub struct OnionPair {
    value: Arc<(OnionObject, OnionObject)>,
}

impl GCTraceable<OnionObjectCell> for OnionPair {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        self.value.0.collect(queue);
        self.value.1.collect(queue);
    }
}

impl Debug for OnionPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} : {:?}", self.value.0, self.value.1)
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
            value: Arc::new((key, value)),
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
        OnionObject::Pair(Self::new(key.weak().clone(), value.weak().clone()))
            .consume_and_stabilize()
    }

    /// 获取键对象的引用。
    #[inline(always)]
    pub fn get_key(&self) -> &OnionObject {
        &self.value.0
    }

    /// 获取值对象的引用。
    #[inline(always)]
    pub fn get_value(&self) -> &OnionObject {
        &self.value.1
    }

    /// 升级键和值的对象引用。
    ///
    /// 用于 GC 跟踪，防止键和值被提前回收。
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.value.0.upgrade(collected);
        self.value.1.upgrade(collected)
    }
}

impl OnionObjectProtocol for OnionPair {
    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        match other {
            OnionObject::Pair(pair) => {
                if self.value.0.equals(&pair.value.0)? && self.value.1.equals(&pair.value.1)? {
                    return Ok(true);
                }
            }
            _ => {}
        }
        Ok(false)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {}

    fn repr(&self, ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!(
            "{} : {}",
            self.value.0.repr(ptrs)?,
            self.value.1.repr(ptrs)?
        ))
    }

    fn display(&self, ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!(
            "{} : {}",
            self.value.0.repr(ptrs)?,
            self.value.1.repr(ptrs)?
        ))
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("Pair".into())
    }

    fn apply(
        &self,
        _this_object: &OnionObject,
        self_object: Option<&OnionObject>,
        value: &OnionObject,
        gc: &mut arc_gc::gc::GC<OnionObjectCell>,
    ) -> Result<Result<crate::lambda::runnable::StepResult, OnionStaticObject>, RuntimeError> {
        self.get_value()
            .apply(Some(self_object.unwrap_or(self.get_key())), value, gc)
    }
}

impl OnionObjectProtocolStatic for OnionPair {
    fn with_attribute<F, R>(
        &self,
        self_object: &OnionObject,
        key: &OnionObject,
        f: &F,
    ) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        self.value
            .1
            .with_attribute(self_object, key, &f)
            .or_else(|_| self.value.0.with_attribute(self_object, key, &f))
    }
}
