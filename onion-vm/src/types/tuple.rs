use std::{collections::VecDeque, fmt::Debug};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    traceable::GCTraceable,
};

use crate::lambda::runnable::RuntimeError;

use super::object::{OnionObject, OnionObjectCell, OnionStaticObject};

#[derive(Clone)]
pub struct OnionTuple {
    pub elements: Box<Vec<OnionObject>>,
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
    pub fn new(elements: Vec<OnionObject>) -> Self {
        OnionTuple {
            elements: elements.into(),
        }
    }

    pub fn new_static(elements: Vec<&OnionStaticObject>) -> OnionStaticObject {
        OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
            elements: elements
                .into_iter()
                .map(|e| e.weak().clone())
                .collect::<Vec<_>>()
                .into(),
        }))
    }

    pub fn new_static_no_ref(elements: Vec<OnionStaticObject>) -> OnionStaticObject {
        OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
            elements: elements
                .into_iter()
                .map(|e| e.weak().clone())
                .collect::<Vec<_>>()
                .into(),
        }))
    }
    pub fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        self.elements.iter().for_each(|e| e.upgrade(collected));
    }

    pub fn len(&self) -> Result<OnionStaticObject, RuntimeError> {
        Ok(OnionStaticObject::new(OnionObject::Integer(
            self.elements.len() as i64,
        )))
    }

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

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, RuntimeError>
    where
        F: Fn(&OnionObject) -> Result<R, RuntimeError>,
    {
        for element in self.elements.as_ref() {
            match element {
                OnionObject::Named(named) => {
                    if named.key.equals(key)? {
                        return f(&named.value);
                    }
                }
                OnionObject::Pair(pair) => {
                    if pair.key.equals(key)? {
                        return f(&pair.value);
                    }
                }
                _ => {}
            }
        }
        Err(RuntimeError::InvalidOperation(
            format!("Attribute {:?} not found in tuple", key).into(),
        ))
    }

    pub fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, RuntimeError> {
        match other {
            OnionObject::Tuple(other_tuple) => {
                let mut new_elements = self.elements.clone();
                new_elements.extend(other_tuple.elements.as_ref().clone());
                Ok(OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
                    elements: new_elements,
                })))
            }
            _ => Ok(OnionStaticObject::new(OnionObject::Undefined(Some(
                format!("Cannot add tuple with {:?}", other),
            )))),
        }
    }

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

// impl OnionTuple {
//     pub fn clone_and_named_assignment(
//         &self,
//         other: &OnionTuple,
//     ) -> Result<OnionStaticObject, RuntimeError> {
//         let mut new_elements = self.elements.clone();
//         let mut assigned = vec![false; new_elements.len()];

//         for other_element in &other.elements {
//             match &*other_element {
//                 OnionObject::Named(named) => {
//                     let key = named.key.as_ref();
//                     let mut found = false;

//                     // 查找是否有相同的key
//                     for (i, element) in new_elements.iter_mut().enumerate() {
//                         let should_replace = {
//                             match &*element {
//                                 OnionObject::Named(existing_named) => {
//                                     existing_named.key.equals(key)?
//                                 }
//                                 _ => false,
//                             }
//                         };

//                         if should_replace {
//                             // Now we can replace it since the borrow is dropped
//                             *element = other_element.clone();
//                             assigned[i] = true;
//                             found = true;
//                             break;
//                         }
//                     }

//                     if !found {
//                         // 如果没有相同的key，则添加到新的元素中
//                         new_elements.push(other_element.clone());
//                         assigned.push(true);
//                     }
//                 }
//                 _ => {}
//             }
//         }

//         // 处理未赋值的元素
//         for other_element in &other.elements {
//             match &*other_element {
//                 OnionObject::Named(_) => {}
//                 _ => {
//                     // 找到第一个未赋值的元素，并赋值
//                     let mut found = false;
//                     for (i, assigned_flag) in assigned.iter_mut().enumerate() {
//                         if !*assigned_flag {
//                             let should_replace_with_clone = {
//                                 match &mut *new_elements[i].try_borrow_mut()? {
//                                     OnionObject::Named(v) => {
//                                         *v.get_value_mut() = other_element.clone();
//                                         false
//                                     }
//                                     _ => {
//                                         // 如果是其他类型的元素，需要直接赋值
//                                         true
//                                     }
//                                 }
//                             };

//                             if should_replace_with_clone {
//                                 new_elements[i] = other_element.clone();
//                             }
//                             *assigned_flag = true;
//                             found = true;
//                             break;
//                         }
//                     }
//                     if !found {
//                         // 如果没有未赋值的元素，则添加到新的元素中
//                         new_elements.push(other_element.clone());
//                         assigned.push(true);
//                     }
//                 }
//             }
//         }

//         Ok(OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
//             elements: new_elements,
//         })))
//     }
// }

impl OnionTuple {
    pub fn push(&mut self, element: OnionObject) {
        self.elements.push(element);
    }
    pub fn pop(&mut self) -> Option<OnionStaticObject> {
        if self.elements.is_empty() {
            None
        } else {
            let last_element = self.elements.last().cloned().map(|e| e.stabilize());
            self.elements.pop();
            last_element
        }
    }
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }
    pub fn clear(&mut self) {
        self.elements.clear();
    }
    pub fn insert(&mut self, index: usize, element: OnionObject) -> Result<(), RuntimeError> {
        if index > self.elements.len() {
            return Err(RuntimeError::InvalidOperation(
                format!("Index out of bounds: {}", index).into(),
            ));
        }
        self.elements.insert(index, element);
        Ok(())
    }
    pub fn remove(&mut self, index: usize) -> Result<OnionStaticObject, RuntimeError> {
        let element_to_remove = self
            .elements
            .get(index)
            .ok_or_else(|| {
                RuntimeError::InvalidOperation(format!("Index out of bounds: {}", index).into())
            })?
            .clone()
            .stabilize();
        self.elements.remove(index);
        Ok(element_to_remove)
    }
}
