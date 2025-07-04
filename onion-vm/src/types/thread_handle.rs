use std::{
    any::Any,
    collections::VecDeque,
    fmt::{Debug, Formatter},
    sync::{Arc, Mutex},
    thread::JoinHandle,
};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    lambda::runnable::RuntimeError,
    types::object::{GCArcStorage, OnionStaticObject},
};

use super::object::{OnionObject, OnionObjectCell, OnionObjectExt};

/// A wrapper around JoinHandle to make it work with OnionObject
pub struct OnionThreadHandle {
    inner: Arc<
        Mutex<(
            Option<JoinHandle<Result<Box<OnionStaticObject>, RuntimeError>>>,
            bool,
            GCArcWeak<OnionObjectCell>,
        )>,
    >,
}

impl OnionThreadHandle {
    pub fn new(
        handle: JoinHandle<Result<Box<OnionStaticObject>, RuntimeError>>,
        gc: &mut GC<OnionObjectCell>,
    ) -> (Self, GCArcStorage) {
        let tmp = gc.create(OnionObjectCell::from(OnionObject::Undefined(None)));
        (
            Self {
                inner: Arc::new(Mutex::new((Some(handle), false, tmp.as_weak()))),
            },
            GCArcStorage::Single(tmp),
        )
    }
    /// Check if the thread has finished without blocking
    pub fn is_finished(&self) -> bool {
        let guard = self.inner.lock().unwrap();
        guard.1 || guard.0.is_none()
    }
}

impl Clone for OnionThreadHandle {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl Debug for OnionThreadHandle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let guard = self.inner.lock().unwrap();
        write!(
            f,
            "OnionThreadHandle(finished: {}, has_handle: {})",
            guard.1 || guard.0.is_none(),
            guard.0.is_some()
        )
    }
}

impl GCTraceable<OnionObjectCell> for OnionThreadHandle {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        let guard = self.inner.lock().unwrap();
        queue.push_back(guard.2.clone());
    }
}

impl OnionObjectExt for OnionThreadHandle {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("ThreadHandle(finished: {})", self.is_finished()))
    }

    fn equals(&self, _other: &OnionObject) -> Result<bool, RuntimeError> {
        Ok(false)
    }

    fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        let guard = self.inner.lock().unwrap();
        if let Some(strong) = guard.2.upgrade() {
            collected.push(strong);
        }
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
        let mut guard = self.inner.lock().unwrap();
        // 句柄已经被取走，意味着线程已经结束
        if guard.0.is_none() {
            return if let Some(strong_ref) = guard.2.upgrade() {
                strong_ref.as_ref().with_data(|data| Ok(data.stabilize()))
            } else {
                Err(RuntimeError::BrokenReference)
            };
        }
        // 非阻塞地检查线程是否结束，通过handle来检查
        if let Some(handle) = guard.0.as_ref() {
            if !handle.is_finished() {
                return Err(RuntimeError::Pending);
            }
        }

        // 线程已经完成，可以安全地 join 并获取结果
        if let Some(handle) = guard.0.take() {
            guard.1 = true;
            drop(guard); // 释放锁
            match handle.join() {
                Ok(result) => match result {
                    Ok(obj) => {
                        // 由于Handle是抽象的Mut容器，因此应当将结果写入GCArcWeak中
                        // 为了保证幂等律，我们对result使用with_data
                        match self.inner.lock().unwrap().2.upgrade() {
                            Some(arc) => arc.as_ref().with_data_mut(|to| {
                                obj.weak().with_data(|obj| {
                                    *to = obj.reconstruct_container()?;
                                    Ok(())
                                })
                            })?,
                            None => return Err(RuntimeError::BrokenReference),
                        }
                        return Ok(*obj);
                    }
                    Err(err) => Err(err),
                },
                Err(_) => Err(RuntimeError::DetailedError(
                    "Thread join failed".to_string().into(),
                )),
            }
        } else {
            Err(RuntimeError::DetailedError(
                "Failed to take thread handle after finished check"
                    .to_string()
                    .into(),
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
                    let guard = self.inner.lock().unwrap();
                    let has_handle = guard.0.is_some();
                    f(&OnionObject::Boolean(has_handle))
                }
                "has_result" => {
                    let guard = self.inner.lock().unwrap();
                    let has_result = guard.2.upgrade().is_some();
                    f(&OnionObject::Boolean(has_result))
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
