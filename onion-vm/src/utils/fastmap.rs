
//! Onion 高性能键池与映射（FastMap）模块。
//!
//! 提供高效的键池（OnionKeyPool）与稀疏映射（OnionFastMap）实现，
//! 支持常量池优化、索引访问、键到索引的高效映射，适用于虚拟机参数、捕获、环境等场景。
//!
//! # 主要功能
//! - 键池的唯一化与索引映射
//! - 稀疏键值对存储与高效查找
//! - 支持索引与键的双向访问
//! - 线性插入、查找与覆盖

use std::{hash::Hash, sync::Arc};

use rustc_hash::FxHashMap;


/// Onion 键池。
///
/// 提供唯一化的键集合与键到索引的高效映射，适用于常量池、参数池等场景。
///
/// # 字段
/// - `keys`: 键的有序集合
/// - `key_to_index`: 键到索引的哈希映射
#[derive(Debug, Clone)]
pub struct OnionKeyPool<K: PartialEq + Eq + Hash + Clone> {
    keys: Arc<[K]>,
    key_to_index: Arc<FxHashMap<K, usize>>,
}

impl<K: PartialEq + Eq + Hash + Clone> OnionKeyPool<K> {
    pub fn create(keys: Vec<K>) -> Self {
        let mut key_to_index = FxHashMap::default();
        for (i, k) in keys.iter().enumerate() {
            key_to_index.insert(k.clone(), i);
        }
        Self {
            keys: keys.into(),
            key_to_index: Arc::new(key_to_index),
        }
    }

    pub fn new(k: Arc<[K]>, i: Arc<FxHashMap<K, usize>>) -> Self {
        Self {
            keys: k,
            key_to_index: i,
        }
    }
    pub fn keys(&self) -> &[K] {
        &self.keys
    }

    pub fn indices(&self) -> &FxHashMap<K, usize> {
        &self.key_to_index
    }
}


/// Onion 高性能稀疏映射。
///
/// 基于键池索引实现的稀疏键值对存储，支持高效查找与插入。
/// 适用于参数绑定、捕获环境等虚拟机场景。
/// 不保证键的唯一性，后插入的值可能会覆盖先前的值。
///
/// # 字段
/// - `pairs`: (索引, 值) 对列表
/// - `pool`: 关联的键池
#[derive(Debug, Clone)]
pub struct OnionFastMap<K: PartialEq + Eq + Hash + Clone, V> {
    pairs: Vec<(usize, V)>,
    pool: OnionKeyPool<K>,
}

impl<K: PartialEq + Eq + Hash + Clone, V> OnionFastMap<K, V> {
    pub fn new(pool: OnionKeyPool<K>) -> Self {
        Self {
            pairs: Vec::new(),
            pool,
        }
    }

    pub fn pool(&self) -> &OnionKeyPool<K> {
        &self.pool
    }

    pub fn new_with_pairs(pairs: Vec<(usize, V)>, pool: OnionKeyPool<K>) -> Self {
        Self { pairs, pool }
    }

    #[inline(always)]
    pub fn push<Q: ?Sized>(&mut self, key: &Q, value: V) -> Option<()>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        if let Some(index) = self.pool.indices().get(key).copied() {
            self.pairs.push((index, value));
            Some(())
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn push_with_index(&mut self, index: usize, value: V) -> Option<()> {
        if index < self.pool.keys().len() {
            self.pairs.push((index, value));
            Some(())
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn pairs(&self) -> &[(usize, V)] {
        &self.pairs
    }

    #[inline(always)]
    pub fn set_pairs(&mut self, pairs: Vec<(usize, V)>) {
        self.pairs = pairs;
    }

    #[inline(always)]
    pub fn clear(&mut self) {
        self.pairs.clear();
    }

    #[inline(always)]
    pub fn to_index<Q: ?Sized>(&self, key: &Q) -> Option<usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.pool.indices().get(key).copied()
    }

    #[inline(always)]
    pub fn from_index(&self, index: usize) -> Option<&K> {
        self.pool.keys().get(index)
    }

    #[inline(always)]
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        let target_id = self.to_index(key)?;
        self.pairs
            .iter()
            .rfind(|(id, _)| *id == target_id)
            .map(|(_, v)| v)
    }
    #[inline(always)]
    pub fn get_by_index(&self, target_index: usize) -> Option<&V> {
        // 同样，需要线性扫描
        self.pairs
            .iter()
            .rfind(|(id, _)| *id == target_index)
            .map(|(_, v)| v)
    }
}

impl<K: PartialEq + Eq + Hash + Clone, V> Default for OnionFastMap<K, V> {
    fn default() -> Self {
        Self::new(OnionKeyPool::create(Vec::new()))
    }
}
