use rustc_hash::FxHashMap as HashMap;
use serde_json::{Map, Value};

use crate::{lambda::runnable::RuntimeError, types::object::OnionStaticObject};

#[derive(Clone, Debug)]
pub struct Frame {
    pub variables: HashMap<usize, OnionStaticObject>,
    pub stack: Vec<OnionStaticObject>,
}

impl Frame {
    pub fn get_stack(&self) -> &Vec<OnionStaticObject> {
        &self.stack
    }
    pub fn get_stack_mut(&mut self) -> &mut Vec<OnionStaticObject> {
        &mut self.stack
    }

    pub fn format_context(&self) -> Value {
        let mut frame_obj = Map::new();

        // Format variables
        let mut variables = Map::new();
        for (var_name, var_value) in &self.variables {
            let value_str = var_value
                .weak()
                .try_borrow()
                .map(|obj| {
                    obj.to_string(&vec![])
                        .unwrap_or_else(|_| format!("{:?}", obj))
                })
                .unwrap_or_else(|_| "<borrow_error>".to_string());
            variables.insert(var_name.to_string(), Value::String(value_str));
        }
        frame_obj.insert("variables".to_string(), Value::Object(variables));

        // Format stack
        let stack_values: Vec<Value> = self
            .stack
            .iter()
            .map(|obj| {
                let obj_str = obj
                    .weak()
                    .try_borrow()
                    .map(|o| o.to_string(&vec![]).unwrap_or_else(|_| format!("{:?}", o)))
                    .unwrap_or_else(|_| "<borrow_error>".to_string());
                Value::String(obj_str)
            })
            .collect();
        frame_obj.insert("stack".to_string(), Value::Array(stack_values));

        Value::Object(frame_obj)
    }
}

#[derive(Clone)]
pub struct Context {
    pub(crate) frames: Vec<Frame>,
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
                "Cannot pop frame from empty context".to_string(),
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
            self.frames.last_mut().unwrap().get_stack_mut().clear();
        }
    }

    pub fn push_object(&mut self, object: OnionStaticObject) -> Result<(), RuntimeError> {
        if self.frames.len() > 0 {
            self.frames.last_mut().unwrap().get_stack_mut().push(object);
            Ok(())
        } else {
            Err(RuntimeError::DetailedError(
                "Cannot push object to empty context".to_string(),
            ))
        }
    }

    pub fn pop(&mut self) -> Result<OnionStaticObject, RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot pop object from empty context".to_string(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        if last_frame.get_stack().len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot pop object from empty stack".to_string(),
            ));
        }
        let stack = last_frame.get_stack_mut();
        Ok(stack.pop().unwrap())
    }

    pub fn discard_objects(&mut self, count: usize) -> Result<(), RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot discard objects from empty context".to_string(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        let stack = last_frame.get_stack_mut();
        if stack.len() < count {
            return Err(RuntimeError::DetailedError(
                "Cannot discard more objects than available in stack".to_string(),
            ));
        }
        // for _ in 0..count {
        //     stack.pop();
        // }
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
                "Cannot discard objects from empty context".to_string(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        let stack = last_frame.get_stack_mut();
        if stack.len() < offset + count {
            return Err(RuntimeError::DetailedError(
                "Cannot discard more objects than available in stack".to_string(),
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
                "Cannot get object from empty context".to_string(),
            ));
        }
        let last_frame = self.frames.last().unwrap();
        if last_frame.get_stack().len() <= idx {
            return Err(RuntimeError::DetailedError(
                "Cannot get object from empty stack".to_string(),
            ));
        }
        let stack = last_frame.get_stack();
        match stack.get(stack.len() - 1 - idx) {
            None => Err(RuntimeError::DetailedError(
                "Index out of bounds".to_string(),
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
                "Cannot get object from empty context".to_string(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        if last_frame.get_stack().len() <= idx {
            return Err(RuntimeError::DetailedError(
                "Cannot get object from empty stack".to_string(),
            ));
        }
        let stack = last_frame.get_stack_mut();
        let idx = stack.len() - 1 - idx;
        match stack.get_mut(idx) {
            None => Err(RuntimeError::DetailedError(
                "Index out of bounds".to_string(),
            )),
            Some(o) => Ok(o),
        }
    }

    // pub fn let_variable(
    //     &mut self,
    //     name: String,
    //     value: OnionStaticObject,
    // ) -> Result<(), RuntimeError> {
    //     if self.frames.len() == 0 {
    //         return Err(RuntimeError::InvalidOperation(
    //             "Cannot let variable in empty context".to_string(),
    //         ));
    //     }

    //     let last_frame = self.frames.last_mut().unwrap();

    //     match last_frame {
    //         Frame::Normal(vars, _) => {
    //             vars.insert(name, value);
    //         }
    //     }    //     Ok(())
    // }

    pub fn let_variable(
        &mut self,
        name: usize,
        value: OnionStaticObject,
    ) -> Result<(), RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::InvalidOperation(
                "Cannot let variable in empty context".to_string(),
            ));
        }

        let last_frame = self.frames.last_mut().unwrap();
        last_frame.variables.insert(name, value);
        Ok(())
    }

    // pub fn get_variable(&self, name: &String) -> Result<&OnionStaticObject, RuntimeError> {
    //     if self.frames.len() == 0 {
    //         return Err(RuntimeError::DetailedError(
    //             "Cannot get variable from empty context".to_string(),
    //         ));
    //     }

    //     // 反向遍历所有帧，从最新的帧开始查找
    //     for frame in self.frames.iter().rev() {
    //         match frame {
    //             Frame::Normal(vars, _) => {
    //                 if let Some(value) = vars.get(name) {
    //                     return Ok(value);
    //                 }
    //             }
    //         }
    //     }

    //     Err(RuntimeError::DetailedError(format!(
    //         "Variable `{}` not found",
    //         name
    //     )))
    // }

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

    // pub fn get_variable_mut(
    //     &mut self,
    //     name: &String,
    // ) -> Result<&mut OnionStaticObject, RuntimeError> {
    //     if self.frames.len() == 0 {
    //         return Err(RuntimeError::DetailedError(
    //             "Cannot get variable from empty context".to_string(),
    //         ));
    //     }

    //     // 反向遍历所有帧，从最新的帧开始查找
    //     for frame in self.frames.iter_mut().rev() {
    //         match frame {
    //             Frame::Normal(vars, _) => {
    //                 if let Some(value) = vars.get_mut(name) {
    //                     return Ok(value);
    //                 }
    //             }
    //         }
    //     }

    //     Err(RuntimeError::DetailedError(format!(
    //         "Variable `{}` not found",
    //         name
    //     )))
    // }

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
                "Cannot swap objects in empty context".to_string(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        let stack = last_frame.get_stack_mut();
        if stack.len() <= idx1 || stack.len() <= idx2 {
            return Err(RuntimeError::DetailedError(
                "Cannot swap objects in empty stack".to_string(),
            ));
        }
        let len = stack.len();
        stack.swap(len - 1 - idx1, len - 1 - idx2);
        Ok(())
    }

    pub fn get_current_stack_mut(&mut self) -> Result<&mut Vec<OnionStaticObject>, RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot get stack from empty context".to_string(),
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
                "Cannot pop from empty stack".to_string(),
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
                "Cannot discard more objects than available in stack".to_string(),
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
                "Index out of bounds".to_string(),
            ));
        }
        Ok(&stack[stack.len() - 1 - idx])
    }

    pub fn replace_last_object(stack: &mut Vec<OnionStaticObject>, object: OnionStaticObject) {
        let last_index = stack.len() - 1;
        stack[last_index] = object;
    }
}

impl Context {
    pub fn format_to_json(&self) -> Value {
        let mut frames = Map::new();

        for (i, frame) in self.frames.iter().enumerate() {
            let mut frame_obj = Map::new();

            // Format variables
            let mut variables = Map::new();
            for (var_name, var_value) in &frame.variables {
                let value_str = var_value
                    .weak()
                    .try_borrow()
                    .map(|obj| {
                        obj.to_string(&vec![])
                            .unwrap_or_else(|_| format!("{:?}", obj))
                    })
                    .unwrap_or_else(|_| "<borrow_error>".to_string());

                variables.insert(var_name.to_string(), Value::String(value_str));
            }
            frame_obj.insert("variables".to_string(), Value::Object(variables));

            // Format stack
            let stack_values: Vec<Value> = frame
                .stack
                .iter()
                .map(|obj| {
                    let obj_str = obj
                        .weak()
                        .try_borrow()
                        .map(|o| o.to_string(&vec![]).unwrap_or_else(|_| format!("{:?}", o)))
                        .unwrap_or_else(|_| "<borrow_error>".to_string());
                    Value::String(obj_str)
                })
                .collect();
            frame_obj.insert("stack".to_string(), Value::Array(stack_values));

            frames.insert(format!("frame_{}", i), Value::Object(frame_obj));
        }

        Value::Object(frames)
    }
}
