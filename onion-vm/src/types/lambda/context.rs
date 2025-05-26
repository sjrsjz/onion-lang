use std::collections::HashMap;

use crate::{lambda::runnable::RuntimeError, types::object::OnionStaticObject};

#[derive(Clone)]
pub enum StackObject {
    Object(OnionStaticObject), // Object with optional reference to a vector of references
    ReturnPoint(isize),
}

pub enum Frame {
    Normal(HashMap<String, OnionStaticObject>, Vec<StackObject>), // Normal frame
}

impl Frame {
    pub fn get_stack(&self) -> &Vec<StackObject> {
        match self {
            Frame::Normal(_, stack) => stack,
        }
    }
    pub fn get_stack_mut(&mut self) -> &mut Vec<StackObject> {
        match self {
            Frame::Normal(_, stack) => stack,
        }
    }
}

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

    pub fn local_return(&mut self) -> Result<Option<Vec<StackObject>>, RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot pop frame from empty context".to_string(),
            ));
        }

        let last_frame = self.frames.pop().unwrap();
        let stack = last_frame.get_stack();
        if self.frames.len() == 0 {
            return Ok(Some(stack.clone()));
        }
        self.frames
            .last_mut()
            .unwrap()
            .get_stack_mut()
            .extend(stack.clone());
        Ok(None)
    }

    pub fn clear_stack(&mut self) {
        if self.frames.len() > 0 {
            self.frames.last_mut().unwrap().get_stack_mut().clear();
        }
    }

    pub fn push_object(&mut self, object: OnionStaticObject) -> Result<(), RuntimeError> {
        if self.frames.len() > 0 {
            self.frames
                .last_mut()
                .unwrap()
                .get_stack_mut()
                .push(StackObject::Object(object));
            Ok(())
        } else {
            Err(RuntimeError::DetailedError(
                "Cannot push object to empty context".to_string(),
            ))
        }
    }
    pub fn push_return_point(&mut self, return_point: isize) {
        if self.frames.len() > 0 {
            self.frames
                .last_mut()
                .unwrap()
                .get_stack_mut()
                .push(StackObject::ReturnPoint(return_point));
        }
    }
    pub fn pop(&mut self) -> Result<StackObject, RuntimeError> {
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
        for _ in 0..count {
            stack.pop();
        }
        Ok(())
    }

    pub fn discard_objects_offset(&mut self, offset: usize, count: usize) -> Result<(), RuntimeError> {
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
        for _ in 0..count {
            stack.remove(stack.len() - 1 - offset);
        }
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
        match &stack[stack.len() - 1 - idx] {
            StackObject::Object(object) => Ok(object),
            StackObject::ReturnPoint(_) => Err(RuntimeError::DetailedError(
                "Expected object, found return point".to_string(),
            )),
        }
    }

    pub fn get_object_rev_mut(&mut self, idx: usize) -> Result<&mut OnionStaticObject, RuntimeError> {
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
        match &mut stack[idx] {
            StackObject::Object(object) => Ok(object),
            StackObject::ReturnPoint(_) => Err(RuntimeError::DetailedError(
                "Expected object, found return point".to_string(),
            )),
        }
    }

    pub fn let_variable(&mut self, name: &String, value: OnionStaticObject) -> Result<(), RuntimeError> {

        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot let variable in empty context".to_string(),
            ));
        }

        let last_frame = self.frames.last_mut().unwrap();
        
        match last_frame {
            Frame::Normal(vars, _) => {
                vars.insert(name.clone(), value);
            }
        }

        Ok(())
    }

    pub fn get_variable(&self, name: &String) -> Result<&OnionStaticObject, RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot get variable from empty context".to_string(),
            ));
        }
        let last_frame = self.frames.last().unwrap();
        match last_frame {
            Frame::Normal(vars, _) => {
                if let Some(value) = vars.get(name) {
                    return Ok(value);
                }
            }
        }
        Err(RuntimeError::DetailedError(format!(
            "Variable {} not found",
            name
        )))
    }

    pub fn get_variable_mut(&mut self, name: &String) -> Result<&mut OnionStaticObject, RuntimeError> {
        if self.frames.len() == 0 {
            return Err(RuntimeError::DetailedError(
                "Cannot get variable from empty context".to_string(),
            ));
        }
        let last_frame = self.frames.last_mut().unwrap();
        match last_frame {
            Frame::Normal(vars, _) => {
                if let Some(value) = vars.get_mut(name) {
                    return Ok(value);
                }
            }
        }
        Err(RuntimeError::DetailedError(format!(
            "Variable {} not found",
            name
        )))
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
        let temp = stack[idx1].clone();
        stack[idx1] = stack[idx2].clone();
        stack[idx2] = temp;
        Ok(())
    }

}
