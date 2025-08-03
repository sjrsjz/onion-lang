use std::{collections::HashSet, fmt::Debug, hash::Hash};

// 新的设计，不再需要生命周期参数！
// 它现在是一个独立的、拥有自己数据的对象。
#[derive(Debug, Clone)]
pub struct CycleDetector<T: Clone + Eq + Hash + Debug> {
    // 我们不再需要 path_stack，因为路径信息隐含在了调用栈中
    // 我们只需要知道当前路径上存在哪些节点
    nodes_in_path: HashSet<T>,
    stack: Vec<T>
}

impl<T: Clone + Eq + Hash + Debug> CycleDetector<T> {
    pub fn new() -> Self {
        CycleDetector {
            nodes_in_path: HashSet::new(),
            stack: Vec::new()
        }
    }

    /// 尝试进入一个新节点，并返回一个新的 CycleDetector。
    pub fn enter(&self, node: T) -> Result<Self, T> {
        if self.nodes_in_path.contains(&node) {
            return Err(node); // 循环！
        }

        // 创建一个新的 HashSet，克隆所有旧的节点
        let mut new_nodes = self.nodes_in_path.clone();
        // 插入新节点
        new_nodes.insert(node.clone());
        let mut new_stack = self.stack.clone();
        new_stack.push(node);

        // 返回一个全新的 CycleDetector 实例
        Ok(CycleDetector {
            nodes_in_path: new_nodes,
            stack: new_stack
        })
    }

    pub fn last(&self) -> Option<&T> {
        self.stack.last()
    }
}
