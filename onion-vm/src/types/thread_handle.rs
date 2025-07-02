use std::{
    any::Any,
    collections::VecDeque,
    fmt::{Debug, Formatter},
    sync::{Arc, Mutex},
    thread::JoinHandle,
};

use arc_gc::{arc::GCArc, arc::GCArcWeak, traceable::GCTraceable};

use crate::{lambda::runnable::RuntimeError, types::object::OnionStaticObject};

use super::object::{OnionObject, OnionObjectCell, OnionObjectExt};

/// A wrapper around JoinHandle to make it work with OnionObject
pub struct OnionThreadHandle {
    handle: Mutex<Option<JoinHandle<Result<Box<OnionStaticObject>, RuntimeError>>>>,
    is_finished: Mutex<bool>,
    cached_result: Mutex<Option<Result<OnionStaticObject, RuntimeError>>>,
}

impl OnionThreadHandle {
    pub fn new(handle: JoinHandle<Result<Box<OnionStaticObject>, RuntimeError>>) -> Self {
        Self {
            handle: Mutex::new(Some(handle)),
            is_finished: Mutex::new(false),
            cached_result: Mutex::new(None),
        }
    }
    /// Check if the thread has finished without blocking
    pub fn is_finished(&self) -> bool {
        *self.is_finished.lock().unwrap() || self.handle.lock().unwrap().is_none()
    }
}

impl Clone for OnionThreadHandle {
    fn clone(&self) -> Self {
        // JoinHandle cannot be cloned, so we create a new handle that's already finished
        Self {
            handle: Mutex::new(None),
            is_finished: Mutex::new(*self.is_finished.lock().unwrap()),
            cached_result: Mutex::new(self.cached_result.lock().unwrap().clone()),
        }
    }
}

impl Debug for OnionThreadHandle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "OnionThreadHandle(finished: {}, has_handle: {})",
            self.is_finished(),
            self.handle.lock().unwrap().is_some()
        )
    }
}

impl GCTraceable<OnionObjectCell> for OnionThreadHandle {
    fn collect(&self, _queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        // Thread handles don't contain OnionObjects, so nothing to collect
    }
}

impl OnionObjectExt for OnionThreadHandle {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("ThreadHandle(finished: {})", self.is_finished()))
    }

    fn equals(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        // Thread handles are only equal if they're the exact same handle
        if let OnionObject::Custom(other_custom) = other {
            if let Some(_other_handle) = other_custom.as_any().downcast_ref::<OnionThreadHandle>() {
                // Since we can't compare JoinHandles directly, we consider them not equal
                // unless they're the exact same Arc (which would be checked by is_same)
                Ok(false)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    fn upgrade(&self, _collected: &mut Vec<GCArc<OnionObjectCell>>) {
        // Thread handles don't contain OnionObjects, so nothing to upgrade
    }

    fn reconstruct_container(&self) -> Result<OnionObject, RuntimeError> {
        Ok(OnionObject::Custom(Arc::new(self.clone())))
    }

    fn is_same(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        if let OnionObject::Custom(other_custom) = other {
            if let Some(other_handle) = other_custom.as_any().downcast_ref::<OnionThreadHandle>() {
                // Use pointer equality to check if it's the same Arc
                Ok(std::ptr::eq(self, other_handle))
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    fn to_boolean(&self) -> Result<bool, RuntimeError> {
        // A thread handle is "truthy" if it hasn't finished yet
        Ok(!self.is_finished())
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("ThreadHandle".to_string())
    }

    fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        // 检查是否已有缓存结果
        if let Some(ref cached) = *self.cached_result.lock().unwrap() {
            return cached.clone();
        }

        // 阻塞并获取线程结果
        let mut handle_guard = self.handle.lock().unwrap();
        if let Some(handle) = handle_guard.take() {
            *self.is_finished.lock().unwrap() = true;
            drop(handle_guard); // 释放锁

            let result = match handle.join() {
                Ok(result) => match result {
                    Ok(object) => Ok(object.as_ref().clone()),
                    Err(err) => Err(err),
                },
                Err(_) => Err(RuntimeError::DetailedError(
                    "Thread panicked during execution".to_string().into(),
                )),
            };

            // 缓存结果
            *self.cached_result.lock().unwrap() = Some(result.clone());
            result
        } else {
            Err(RuntimeError::DetailedError(
                "Thread has already finished".to_string().into(),
            ))
        }
    }

    fn with_attribute(
        &self,
        key: &OnionObject,
        f: &mut dyn FnMut(&OnionObject) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        match key {
            OnionObject::String(s) => match s.as_str() {
                "is_finished" => f(&OnionObject::Boolean(self.is_finished())),
                "has_handle" => {
                    let has_handle = self.handle.lock().unwrap().is_some();
                    f(&OnionObject::Boolean(has_handle))
                }
                "has_result" => {
                    let has_result = self.cached_result.lock().unwrap().is_some();
                    f(&OnionObject::Boolean(has_result))
                }
                "is_success" => {
                    if let Some(ref cached) = *self.cached_result.lock().unwrap() {
                        f(&OnionObject::Boolean(cached.is_ok()))
                    } else {
                        f(&OnionObject::Boolean(false)) // 还没有结果
                    }
                }
                "is_error" => {
                    if let Some(ref cached) = *self.cached_result.lock().unwrap() {
                        f(&OnionObject::Boolean(cached.is_err()))
                    } else {
                        f(&OnionObject::Boolean(false)) // 还没有结果
                    }
                }
                "error" => {
                    // 只返回错误信息，如果成功或未完成则返回 null
                    if let Some(ref cached) = *self.cached_result.lock().unwrap() {
                        match cached {
                            Ok(_) => f(&OnionObject::Null),
                            Err(err) => f(&OnionObject::String(format!("{}", err).into())),
                        }
                    } else {
                        f(&OnionObject::Null)
                    }
                }
                _ => Err(RuntimeError::InvalidOperation(
                    format!("Attribute '{}' not found in ThreadHandle", s).into(),
                )),
            },
            _ => Err(RuntimeError::InvalidOperation(
                format!("Attribute {:?} not found in ThreadHandle", key).into(),
            )),
        }
    }
}
