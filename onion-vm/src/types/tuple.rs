use std::fmt::Debug;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use super::object::{ObjectError, OnionObject, OnionObjectCell, OnionStaticObject};

#[derive(Clone)]
pub struct OnionTuple {
    pub elements: Vec<OnionObjectCell>,
}

impl GCTraceable for OnionTuple {
    fn visit(&self) {
        for element in &self.elements {
            element.visit();
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
    pub fn new(elements: Vec<OnionObjectCell>) -> Self {
        OnionTuple { elements }
    }

    pub fn new_static(elements: Vec<&OnionStaticObject>) -> OnionStaticObject {
        OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
            elements: elements.into_iter().map(|e| e.weak().clone()).collect(),
        }))
    }

    pub fn new_static_no_ref(elements: Vec<OnionStaticObject>) -> OnionStaticObject {
        OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
            elements: elements.into_iter().map(|e| e.weak().clone()).collect(),
        }))
    }
    pub fn upgrade(&self) -> Option<Vec<GCArc<OnionObjectCell>>> {
        if self.elements.is_empty() {
            return None;
        }
        let mut arcs = Vec::new();
        for element in &self.elements {
            match element.upgrade() {
                Some(arc) => arcs.extend(arc),
                None => {}
            }
        }
        Some(arcs)
    }

    pub fn len(&self) -> Result<OnionStaticObject, ObjectError> {
        Ok(OnionStaticObject::new(OnionObject::Integer(
            self.elements.len() as i64,
        )))
    }

    pub fn at(&self, index: i64) -> Result<OnionStaticObject, ObjectError> {
        if index < 0 || index >= self.elements.len() as i64 {
            return Ok(OnionStaticObject::new(OnionObject::Undefined(Some(
                "Index out of bounds".to_string(),
            ))));
        }
        Ok(OnionStaticObject::new(
            self.elements[index as usize].try_borrow()?.clone(),
        ))
    }

    pub fn with_index<F, R>(&self, index: i64, f: &F) -> Result<R, ObjectError>
    where
        F: Fn(&OnionObject) -> Result<R, ObjectError>,
    {
        if index < 0 || index >= self.elements.len() as i64 {
            return Err(ObjectError::InvalidOperation(format!(
                "Index out of bounds: {}",
                index
            )));
        }
        let borrowed = self.elements[index as usize].try_borrow()?;
        f(&*borrowed)
    }

    pub fn with_attribute<F, R>(&self, key: &OnionObject, f: &F) -> Result<R, ObjectError>
    where
        F: Fn(&OnionObject) -> Result<R, ObjectError>,
    {
        for element in &self.elements {
            match &*element.try_borrow()? {
                OnionObject::Named(named) => {
                    if named.key.try_borrow()?.equals(key)? {
                        return f(&*named.value.as_ref().try_borrow()?);
                    }
                }
                OnionObject::Pair(pair) => {
                    if pair.key.try_borrow()?.equals(key)? {
                        return f(&*pair.value.as_ref().try_borrow()?);
                    }
                }
                _ => {}
            }
        }
        Err(ObjectError::InvalidOperation(format!(
            "Attribute {:?} not found in tuple",
            key
        )))
    }

    pub fn with_attribute_mut<F, R>(&mut self, key: &OnionObject, f: F) -> Result<R, ObjectError>
    where
        F: Fn(&mut OnionObject) -> Result<R, ObjectError>,
    {
        for element in &mut self.elements {
            match &*element.try_borrow()? {
                OnionObject::Named(named) => {
                    if named.key.try_borrow()?.equals(key)? {
                        return f(&mut *named.value.try_borrow_mut().map_err(|_| {
                            ObjectError::InvalidOperation(format!(
                                "Failed to borrow value for key {:?}",
                                key
                            ))
                        })?);
                    }
                }
                OnionObject::Pair(pair) => {
                    if pair.key.try_borrow()?.equals(key)? {
                        return f(&mut *pair.value.try_borrow_mut().map_err(|_| {
                            ObjectError::InvalidOperation(format!(
                                "Failed to borrow value for key {:?}",
                                key
                            ))
                        })?);
                    }
                }
                _ => {}
            }
        }
        Err(ObjectError::InvalidOperation(format!(
            "Attribute {:?} not found in tuple",
            key
        )))
    }

    pub fn binary_add(&self, other: &OnionObject) -> Result<OnionStaticObject, ObjectError> {
        match other {
            OnionObject::Tuple(other_tuple) => {
                let mut new_elements = self.elements.clone();
                new_elements.extend(other_tuple.elements.clone());
                Ok(OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
                    elements: new_elements,
                })))
            }
            _ => Ok(OnionStaticObject::new(OnionObject::Undefined(Some(
                format!("Cannot add tuple with {:?}", other),
            )))),
        }
    }

    pub fn contains(&self, other: &OnionObject) -> Result<bool, ObjectError> {
        for element in &self.elements {
            if element.try_borrow()?.equals(other)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl OnionTuple {
    pub fn equals(&self, other: &OnionObject) -> Result<bool, ObjectError> {
        match other {
            OnionObject::Tuple(other_tuple) => {
                if self.elements.len() != other_tuple.elements.len() {
                    return Ok(false);
                }
                for (a, b) in self.elements.iter().zip(&other_tuple.elements) {
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

impl OnionTuple {
    pub fn clone_and_named_assignment(
        &self,
        other: &OnionTuple,
    ) -> Result<OnionStaticObject, ObjectError> {
        let mut new_elements = self.elements.clone();
        let mut assigned = vec![false; new_elements.len()];

        for other_element in &other.elements {
            match &*other_element.try_borrow()? {
                OnionObject::Named(named) => {
                    let key = named.key.as_ref();
                    let mut found = false;

                    // 查找是否有相同的key
                    for (i, element) in new_elements.iter_mut().enumerate() {
                        let should_replace = {
                            match &*element.try_borrow()? {
                                OnionObject::Named(existing_named) => {
                                    existing_named.key.as_ref().equals(key)?
                                }
                                _ => false,
                            }
                        };

                        if should_replace {
                            // Now we can replace it since the borrow is dropped
                            *element = other_element.clone();
                            assigned[i] = true;
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        // 如果没有相同的key，则添加到新的元素中
                        new_elements.push(other_element.clone());
                        assigned.push(true);
                    }
                }
                _ => {}
            }
        }

        // 处理未赋值的元素
        for other_element in &other.elements {
            match &*other_element.try_borrow()? {
                OnionObject::Named(_) => {}
                _ => {
                    // 找到第一个未赋值的元素，并赋值
                    let mut found = false;
                    for (i, assigned_flag) in assigned.iter_mut().enumerate() {
                        if !*assigned_flag {
                            let should_replace_with_clone = {
                                match &mut *new_elements[i].try_borrow_mut()? {
                                    OnionObject::Named(v) => {
                                        *v.get_value_mut() = other_element.clone();
                                        false
                                    }
                                    _ => {
                                        // 如果是其他类型的元素，需要直接赋值
                                        true
                                    }
                                }
                            };

                            if should_replace_with_clone {
                                new_elements[i] = other_element.clone();
                            }
                            *assigned_flag = true;
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // 如果没有未赋值的元素，则添加到新的元素中
                        new_elements.push(other_element.clone());
                        assigned.push(true);
                    }
                }
            }
        }

        Ok(OnionStaticObject::new(OnionObject::Tuple(OnionTuple {
            elements: new_elements,
        })))
    }
}
