use std::{collections::HashSet, hash::Hash};

/// Detects cycles in a path during traversal.
///
/// This utility helps in detecting if adding a node to the current
/// path would create a cycle. It uses a stack to keep track of the
/// current path and a set for quick lookups of nodes already in the path.
///
/// Usage involves calling `visit` when entering a node. If successful,
/// it returns a `NodeGuard`. When the `NodeGuard` goes out of scope,
/// the node is automatically removed from the path.
pub struct CycleDetector<T: Clone + Eq + Hash> {
    path_stack: Vec<T>,
    nodes_in_path: HashSet<T>,
}

/// A guard that ensures a node is removed from the `CycleDetector`'s
/// path when it goes out of scope.
#[must_use = "if unused, the node will immediately be considered exited"]
pub struct NodeGuard<'a, T: Clone + Eq + Hash> {
    detector: &'a mut CycleDetector<T>,
}

impl<'a, T: Clone + Eq + Hash> NodeGuard<'a, T> {
    pub fn get_detector(&self) -> &CycleDetector<T> {
        self.detector
    }
    pub fn get_detector_mut(&mut self) -> &mut CycleDetector<T> {
        self.detector
    }
}

impl<T: Clone + Eq + Hash> CycleDetector<T> {
    /// Creates a new, empty `CycleDetector`.
    pub fn new() -> Self {
        CycleDetector {
            path_stack: Vec::new(),
            nodes_in_path: HashSet::new(),
        }
    }

    /// Attempts to mark a node as visited in the current path.
    ///
    /// If the node is already in the current path, a cycle is detected,
    /// and an `Err` containing the node that caused the cycle is returned.
    ///
    /// If the node can be successfully added to the path (no cycle detected),
    /// an `Ok` containing a `NodeGuard` is returned. The `NodeGuard` ensures
    /// that the node is removed from the path when the guard goes out of scope.
    ///
    /// # Arguments
    ///
    /// * `node`: The node to visit.
    ///
    /// # Returns
    ///
    /// * `Ok(NodeGuard)` if the node was successfully added to the path.
    /// * `Err(T)` if adding the node would create a cycle, where `T` is the `node` itself.
    pub fn visit(&mut self, node: T) -> Result<NodeGuard<'_, T>, T> {
        if self.nodes_in_path.contains(&node) {
            return Err(node); // Cycle detected, node is returned
        }
        // No cycle, add to path
        // node is consumed by insert, so clone for path_stack
        self.path_stack.push(node.clone());
        self.nodes_in_path.insert(node); // node ownership moved here
        
        Ok(NodeGuard { detector: self })
    }

    /// Internal method to remove the latest node from the path.
    /// Called when a `NodeGuard` is dropped.
    fn leave_node(&mut self) {
        if let Some(node) = self.path_stack.pop() {
            self.nodes_in_path.remove(&node);
        } else {
            // This case should ideally not be reached if NodeGuard is used correctly,
            // as a guard is only created after a successful addition to the path_stack.
            // Depending on strictness, one might panic here.
            // For robustness, we'll allow it, though it indicates a potential logic error elsewhere.
        }
    }

    /// Returns the current path being tracked.
    pub fn current_path(&self) -> &Vec<T> {
        &self.path_stack
    }

    /// Checks if a specific node is currently in the active path.
    pub fn is_node_in_path(&self, node: &T) -> bool {
        self.nodes_in_path.contains(node)
    }
}

impl<'a, T: Clone + Eq + Hash> Drop for NodeGuard<'a, T> {
    fn drop(&mut self) {
        self.detector.leave_node();
    }
}

// Default implementation for new()
impl<T: Clone + Eq + Hash> Default for CycleDetector<T> {
    fn default() -> Self {
        Self::new()
    }
}