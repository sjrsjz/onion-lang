use std::{
    any::Any,
    collections::VecDeque,
    fmt::{Debug, Formatter},
    sync::{Arc, Mutex},
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

pub struct OnionAsyncHandle {
    inner: Mutex<(GCArcWeak<OnionObjectCell>, bool)>,
}

impl OnionAsyncHandle {
    // 创建一个新的异步句柄，此时没有缓存结果
    pub fn new(gc: &mut GC<OnionObjectCell>) -> (Arc<Self>, GCArcStorage) {
        let tmp = gc.create(OnionObjectCell::from(OnionObject::Undefined(None)));
        (
            Arc::new(Self {
                inner: Mutex::new((tmp.as_weak(), false)),
            }),
            GCArcStorage::Single(tmp),
        )
    }

    /// 检查 handle 是否完成（供调度器使用）
    pub fn is_finished(&self) -> bool {
        self.inner.lock().unwrap().1
    }

    /// 设置任务结果（供调度器使用）
    /// 为了保证幂等律，对result使用with_data
    pub fn set_result(&self, result: &OnionObject) -> Result<(), RuntimeError> {
        let mut guard = self.inner.lock().unwrap();
        guard.0.upgrade().map_or_else(
            || Err(RuntimeError::BrokenReference),
            |strong_ref| {
                result.with_data(|data| {
                    strong_ref.as_ref().with_data_mut(|strong_data| {
                        *strong_data = data.clone();
                        Ok(())
                    })
                })
            },
        )?;
        guard.1 = true; // 设置为完成状态
        Ok(())
    }

    /// 设置任务为完成状态但无结果（供调度器使用）
    pub fn set_finished(&self) {
        let mut guard = self.inner.lock().unwrap();
        guard.1 = true;
    }
}

impl Debug for OnionAsyncHandle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // 只打印 is_finished 状态，避免 Debug trait 问题
        let guard = self.inner.lock().unwrap();
        write!(f, "OnionAsyncHandle(finished: {})", guard.1)
    }
}

impl GCTraceable<OnionObjectCell> for OnionAsyncHandle {
    fn collect(&self, queue: &mut VecDeque<GCArcWeak<OnionObjectCell>>) {
        let guard = self.inner.lock().unwrap();
        queue.push_back(guard.0.clone());
    }
}

impl OnionObjectExt for OnionAsyncHandle {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn repr(&self, _ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        Ok(format!("AsyncHandle(finished: {})", self.is_finished()))
    }

    fn equals(&self, _other: &OnionObject) -> Result<bool, RuntimeError> {
        // Async handles are only equal if they're the exact same handle
        Ok(false)
    }

    fn upgrade(&self, collected: &mut Vec<GCArc<OnionObjectCell>>) {
        let guard = self.inner.lock().unwrap();
        if let Some(strong_ref) = guard.0.upgrade() {
            collected.push(strong_ref);
        }
    }

    fn is_same(&self, other: &OnionObject) -> Result<bool, RuntimeError> {
        if let OnionObject::Custom(other_custom) = other {
            if let Some(other_handle) = other_custom.as_any().downcast_ref::<OnionAsyncHandle>() {
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
        // A async handle is "truthy" if it hasn't finished yet
        Ok(!self.is_finished())
    }

    fn type_of(&self) -> Result<String, RuntimeError> {
        Ok("AsyncHandle".to_string())
    }

    fn value_of(&self) -> Result<OnionStaticObject, RuntimeError> {
        let guard = self.inner.lock().unwrap();
        let (ref weak_result, is_finished) = *guard;
        if is_finished {
            if let Some(strong_ref) = weak_result.upgrade() {
                // 获取对象内容并 stabilize
                strong_ref.as_ref().with_data(|data| Ok(data.stabilize()))
            } else {
                // 这种情况发生在用户尝试跨GC堆传递等情况，考虑到我们没有分布式GC，直接返回 BrokenReference
                Err(RuntimeError::BrokenReference)
            }
        } else {
            Err(RuntimeError::Pending)
        }
    }

    fn to_string(&self, ptrs: &Vec<*const OnionObject>) -> Result<String, RuntimeError> {
        let finished = self.is_finished();
        if finished {
            match self.value_of() {
                Ok(val) => Ok(format!(
                    "AsyncHandle(finished: true, result: {})",
                    val.weak().to_string(ptrs)?
                )),
                Err(RuntimeError::BrokenReference) => {
                    Ok("AsyncHandle(finished: true, result: <broken reference>)".to_string())
                }
                Err(_) => Ok("AsyncHandle(finished: true, result: <unknown error>)".to_string()),
            }
        } else {
            let guard = self.inner.lock().unwrap();
            let (ref weak_result, _) = *guard;
            match weak_result.upgrade() {
                Some(v) => Ok(format!(
                    "AsyncHandle(finished: false, result: {:?})",
                    v.as_ref().0.read().unwrap().to_string(ptrs)
                )),
                None => Ok("AsyncHandle(finished: false, result: <broken reference>)".to_string()),
            }
        }
    }

    fn with_attribute(
        &self,
        key: &OnionObject,
        f: &mut dyn FnMut(&OnionObject) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        match key {
            OnionObject::String(s) => match s.as_ref() {
                "is_finished" => f(&OnionObject::Boolean(self.is_finished())),
                "has_result" => {
                    let guard = self.inner.lock().unwrap();
                    let has_result = guard.0.upgrade().is_some();
                    f(&OnionObject::Boolean(has_result))
                }
                _ => Err(RuntimeError::InvalidOperation(
                    format!("Attribute '{}' not found in AsyncHandle", s).into(),
                )),
            },
            _ => Err(RuntimeError::InvalidOperation(
                format!("Attribute {:?} not found in AsyncHandle", key).into(),
            )),
        }
    }
}
