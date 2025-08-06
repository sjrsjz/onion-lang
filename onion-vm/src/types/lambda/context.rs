//! Onion 虚拟机执行上下文与帧管理。
//!
//! - `Frame`：单个执行帧，包含变量表和操作数栈。
//! - `Context`：多帧调用栈，支持变量作用域、栈操作、帧切换等。
//! - 提供丰富的栈操作、变量管理与调试辅助方法。

use rustc_hash::FxHashMap as HashMap;

use crate::{
    lambda::runnable::RuntimeError,
    types::{
        lambda::vm_instructions::instruction_set::VMInstructionPackage, object::OnionStaticObject,
    },
    utils::format_object_summary,
};


/// 虚拟机执行帧。
/// 
/// 包含变量表和操作数栈，代表一次函数调用或执行环境。
#[derive(Clone, Debug)]
pub struct Frame {
    /// 变量表，key 为常量池索引
    variables: HashMap<usize, OnionStaticObject>,
    /// 操作数栈
    stack: Vec<OnionStaticObject>,
}

impl Frame {
    pub fn new() -> Self {
        Frame {
            variables: HashMap::default(),
            stack: Vec::new(),
        }
    }
    #[inline(always)]
    pub fn get_stack(&self) -> &Vec<OnionStaticObject> {
        &self.stack
    }
    #[inline(always)]
    pub fn get_stack_mut(&mut self) -> &mut Vec<OnionStaticObject> {
        &mut self.stack
    }

    pub fn format_context(&self, package: &VMInstructionPackage) -> String {
        let mut parts = Vec::new();
        let string_pool = package.get_string_pool();

        // --- Part 1: Format Variables ---
        if self.variables.is_empty() {
            parts.push("  - Variables: (none)".to_string());
        } else {
            parts.push("  - Variables:".to_string());
            for (id, value) in &self.variables {
                // [关键修改] 使用 id 从 string_pool 中查找变量名
                let var_name = string_pool
                    .get(*id)
                    .map(|s| s.as_ref())
                    .unwrap_or("<Unknown Var>");

                let value_summary = format_object_summary(value.weak());
                parts.push(format!("    - {}: {}", var_name, value_summary));
            }
        }

        // --- Part 2: Format Operand Stack ---
        if self.stack.is_empty() {
            parts.push("  - Operand Stack: (empty)".to_string());
        } else {
            parts.push(format!("  - Operand Stack ({} items):", self.stack.len()));
            for (i, value) in self.stack.iter().rev().enumerate() {
                // [修改点] 在这里也使用辅助函数
                let value_summary = format_object_summary(value.weak());
                parts.push(format!("    - [Top - {}]: {}", i, value_summary));
            }
        }

        parts.join("\n")
    }
}


/// Onion 虚拟机执行上下文。
/// 
/// 管理多帧调用栈，支持作用域、变量、栈和帧的各种操作。
#[derive(Clone)]
pub struct Context {
    /// 调用帧栈，栈顶为当前活跃帧
    frames: Vec<Frame>,
}

impl Context {
    pub fn new() -> Self {
        Context { frames: Vec::new() }
    }

    pub fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame);
    }

    pub fn pop_frame(&mut self) -> Result<Frame, RuntimeError> {
        match self.frames.pop() {
            Some(frame) => Ok(frame),
            None => Err(RuntimeError::DetailedError(
                "Cannot pop frame from empty context".into(),
            )),
        }
    }
    pub fn concat_last_frame(&mut self) -> Result<(), RuntimeError> {
        if self.frames.len() < 2 {
            return Ok(());
        }

        let last_frame = self.frames.pop().unwrap();
        let second_last_frame = self.frames.last_mut().unwrap();
        second_last_frame.stack.extend(last_frame.stack);
        Ok(())
    }
    pub fn clear_stack(&mut self) {
        if self.frames.len() > 0 {
            self.frames.last_mut().unwrap().stack.clear();
        }
    }

    pub fn push_object(&mut self, object: OnionStaticObject) -> Result<(), RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot push object to empty context".into(),
            ));
        }
        self.frames.last_mut().unwrap().stack.push(object);
        Ok(())
    }

    pub fn pop(&mut self) -> Result<OnionStaticObject, RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot pop object from empty context".into(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        if last_frame.get_stack().len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot pop object from empty stack".into(),
            ));
        }
        let stack = last_frame.get_stack_mut();
        Ok(stack.pop().unwrap())
    }

    pub fn discard_objects(&mut self, count: usize) -> Result<(), RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot discard objects from empty context"
                    .to_string()
                    .into(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        let stack = last_frame.get_stack_mut();
        if stack.len() < count {
            return Err(RuntimeError::DetailedError(
                "Cannot discard more objects than available in stack"
                    .to_string()
                    .into(),
            ));
        }
        stack.truncate(stack.len() - count);
        Ok(())
    }

    pub fn discard_objects_offset(
        &mut self,
        offset: usize,
        count: usize,
    ) -> Result<(), RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot discard objects from empty context"
                    .to_string()
                    .into(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        let stack = last_frame.get_stack_mut();
        if stack.len() < offset + count {
            return Err(RuntimeError::DetailedError(
                "Cannot discard more objects than available in stack"
                    .to_string()
                    .into(),
            ));
        }

        // 使用 drain 一次性删除范围内的元素
        let remove_start = stack.len() - offset - count;
        let remove_end = stack.len() - offset;
        stack.drain(remove_start..remove_end);
        Ok(())
    }

    pub fn get_object_rev(&self, idx: usize) -> Result<&OnionStaticObject, RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot get object from empty context".into(),
            ));
        }
        let last_frame = self.frames.last().unwrap();
        if last_frame.get_stack().len() <= idx {
            return Err(RuntimeError::DetailedError(
                "Cannot get object from empty stack".into(),
            ));
        }
        let stack = last_frame.get_stack();
        match stack.get(stack.len() - 1 - idx) {
            None => Err(RuntimeError::DetailedError(
                "Index out of bounds".into(),
            )),
            Some(o) => Ok(o),
        }
    }

    pub fn get_object_rev_mut(
        &mut self,
        idx: usize,
    ) -> Result<&mut OnionStaticObject, RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot get object from empty context".into(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        if last_frame.get_stack().len() <= idx {
            return Err(RuntimeError::DetailedError(
                "Cannot get object from empty stack".into(),
            ));
        }
        let stack = last_frame.get_stack_mut();
        let idx = stack.len() - 1 - idx;
        match stack.get_mut(idx) {
            None => Err(RuntimeError::DetailedError(
                "Index out of bounds".into(),
            )),
            Some(o) => Ok(o),
        }
    }

    #[inline(always)]
    pub fn let_variable(
        &mut self,
        name: usize,
        value: OnionStaticObject,
    ) -> Result<(), RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::InvalidOperation(
                "Cannot let variable in empty context".into(),
            ));
        }

        let last_frame = self.frames.last_mut().unwrap();
        last_frame.variables.insert(name, value);
        Ok(())
    }

    #[inline(always)]
    pub fn get_variable(&self, name: usize) -> Option<&OnionStaticObject> {
        if self.frames.len() == 0 {
            return None;
        } // 反向遍历所有帧，从最新的帧开始查找
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.variables.get(&name) {
                return Some(value);
            }
        }
        None
    }

    fn _debug_print(&self) {
        println!("Context Debug Print:");
        for (i, frame) in self.frames.iter().enumerate() {
            println!("Frame {}: {:?}", i, frame);
        }
    }

    pub fn get_variable_mut(&mut self, name: usize) -> Option<&mut OnionStaticObject> {
        if self.frames.len() == 0 {
            return None;
        } // 反向遍历所有帧，从最新的帧开始查找
        for frame in self.frames.iter_mut().rev() {
            if let Some(value) = frame.variables.get_mut(&name) {
                return Some(value);
            }
        }
        None
    }

    pub fn swap(&mut self, idx1: usize, idx2: usize) -> Result<(), RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot swap objects in empty context".into(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        let stack = last_frame.get_stack_mut();
        if stack.len() <= idx1 || stack.len() <= idx2 {
            return Err(RuntimeError::DetailedError(
                "Cannot swap objects in empty stack".into(),
            ));
        }
        let len = stack.len();
        stack.swap(len - 1 - idx1, len - 1 - idx2);
        Ok(())
    }

    pub fn get_current_stack_mut(&mut self) -> Result<&mut Vec<OnionStaticObject>, RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot get stack from empty context".into(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        Ok(last_frame.get_stack_mut())
    }

    #[inline(always)]
    pub fn push_to_stack(stack: &mut Vec<OnionStaticObject>, object: OnionStaticObject) {
        stack.push(object);
    }

    pub fn pop_from_stack(
        stack: &mut Vec<OnionStaticObject>,
    ) -> Result<OnionStaticObject, RuntimeError> {
        if stack.is_empty() {
            return Err(RuntimeError::DetailedError(
                "Cannot pop from empty stack".into(),
            ));
        }
        Ok(stack.pop().unwrap())
    }

    #[inline(always)]
    pub fn discard_from_stack(
        stack: &mut Vec<OnionStaticObject>,
        count: usize,
    ) -> Result<(), RuntimeError> {
        if stack.len() < count {
            return Err(RuntimeError::DetailedError(
                "Cannot discard more objects than available in stack"
                    .to_string()
                    .into(),
            ));
        }
        stack.truncate(stack.len() - count);
        Ok(())
    }

    #[inline(always)]
    pub fn get_object_from_stack(
        stack: &Vec<OnionStaticObject>,
        idx: usize,
    ) -> Result<&OnionStaticObject, RuntimeError> {
        if stack.len() <= idx {
            return Err(RuntimeError::DetailedError(
                "Index out of bounds".into(),
            ));
        }
        Ok(&stack[stack.len() - 1 - idx])
    }

    pub fn replace_last_object(stack: &mut Vec<OnionStaticObject>, object: OnionStaticObject) {
        let last_index = stack.len() - 1;
        stack[last_index] = object;
    }

    pub fn format_context(&self, package: &VMInstructionPackage) -> String {
        if self.frames.is_empty() {
            return "Context: (No active frames)".to_string();
        }

        let mut parts = Vec::new();
        parts.push(format!("Call Stack ({} frames):", self.frames.len()));

        // 从栈顶（最近的调用帧）开始打印
        for (i, frame) in self.frames.iter().rev().enumerate() {
            // 你需要一种方法来命名你的帧。这通常与函数名相关联。
            // 暂时我们用索引代替。
            parts.push(format!("--- Frame #{} (most recent) ---", i));

            // 调用我们刚刚为 Frame 实现的 format_context
            let frame_context = frame.format_context(package);
            parts.push(frame_context);
        }

        parts.join("\n")
    }
}
