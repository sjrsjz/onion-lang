//! 通用循环检测器工具。
//!
//! 该模块实现了一个通用的循环检测器（CycleDetector），用于在递归遍历、图遍历、依赖分析等场景中检测环路。
//! 通过不可变结构和链式 enter 操作，可以安全地在多分支递归中追踪访问路径，避免重复节点导致的循环。
//!
//! # 典型用法
//! ```ignore
//! let detector = CycleDetector::new();
//! let detector = detector.enter(node1)?;
//! let detector = detector.enter(node2)?;
//! // ...递归...
//! ```
use std::{collections::HashSet, fmt::Debug, hash::Hash};

/// 通用循环检测器。
///
/// 用于递归遍历、依赖分析等场景下的环路检测。
/// 通过不可变结构和链式 enter 操作，安全追踪访问路径。
///
/// # 类型参数
/// - `T`：节点类型，需实现 `Clone + Eq + Hash + Debug`
#[derive(Debug, Clone)]
pub struct CycleDetector<T: Clone + Eq + Hash + Debug> {
    /// 路径中已访问的节点集合。
    nodes_in_path: HashSet<T>,
    /// 当前递归路径的节点栈（顺序记录）。
    stack: Vec<T>,
}

impl<T: Clone + Eq + Hash + Debug> CycleDetector<T> {
    /// 创建一个新的空循环检测器。
    ///
    /// # 返回
    /// 空路径的 CycleDetector 实例。
    pub fn new() -> Self {
        CycleDetector {
            nodes_in_path: HashSet::new(),
            stack: Vec::new(),
        }
    }

    /// 尝试进入一个新节点，返回新的 CycleDetector 或检测到环路。
    ///
    /// # 参数
    /// - `node`：要进入的新节点
    ///
    /// # 返回
    /// - `Ok(new_detector)`：成功进入，返回包含新节点的新检测器
    /// - `Err(node)`：检测到环路，返回导致环路的节点
    pub fn enter(&self, node: T) -> Result<Self, T> {
        if self.nodes_in_path.contains(&node) {
            return Err(node); // 检测到环路
        }
        let mut new_nodes = self.nodes_in_path.clone();
        new_nodes.insert(node.clone());
        let mut new_stack = self.stack.clone();
        new_stack.push(node);
        Ok(CycleDetector {
            nodes_in_path: new_nodes,
            stack: new_stack,
        })
    }

    /// 获取当前路径上的最后一个节点。
    ///
    /// # 返回
    /// - `Some(&T)`：如果路径非空，返回最后一个节点的引用
    /// - `None`：路径为空
    pub fn last(&self) -> Option<&T> {
        self.stack.last()
    }
}
