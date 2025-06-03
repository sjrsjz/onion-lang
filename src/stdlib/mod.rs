use std::{cell::RefCell, sync::Arc};

use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    onion_tuple,
    types::{
        lambda::definition::{LambdaBody, OnionLambdaDefinition},
        named::OnionNamed,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    GC,
};

mod http;
mod io;
mod json;
mod math;
mod string;
mod time;
mod types;
mod tuple;


pub fn build_named_dict(dict: IndexMap<String, OnionStaticObject>) -> OnionStaticObject {
    let mut pairs = vec![];
    for (key, value) in dict {
        pairs.push(OnionNamed::new_static(
            &OnionObject::String(key).stabilize(),
            &value,
        ));
    }
    OnionTuple::new_static_no_ref(pairs)
}

pub fn get_attr_direct(obj: &OnionObject, key: String) -> Result<OnionStaticObject, RuntimeError> {
    obj.with_attribute(&OnionObject::String(key), &|obj| {
        Ok(obj.clone().stabilize())
    })
}

pub struct NativeFunctionGenerator<F>
where
    F: Fn(OnionStaticObject, &mut GC<OnionObjectCell>) -> Result<OnionStaticObject, RuntimeError>
        + 'static,
{
    argument: OnionStaticObject,
    function: Arc<RefCell<F>>,
}

impl<F> Runnable for NativeFunctionGenerator<F>
where
    F: Fn(OnionStaticObject, &mut GC<OnionObjectCell>) -> Result<OnionStaticObject, RuntimeError>
        + 'static,
{
    fn set_argument(
        &mut self,
        argument: OnionStaticObject,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        self.argument = argument;
        Ok(())
    }

    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> Result<StepResult, RuntimeError> {
        let argument = self.argument.clone();
        match &self.function.borrow_mut()(argument, gc) {
            Ok(result) => Ok(StepResult::Return(result.clone())),
            Err(err) => Ok(StepResult::Error(err.clone())),
        }
    }

    fn receive(
        &mut self,
        _step_result: StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::DetailedError(
            "receive not implemented".to_string(),
        ))
    }

    fn copy(&self, _gc: &mut onion_vm::GC<OnionObjectCell>) -> Box<dyn Runnable> {
        Box::new(NativeFunctionGenerator {
            argument: self.argument.clone(),
            function: Arc::clone(&self.function),
        })
    }

    fn format_context(&self) -> Result<serde_json::Value, RuntimeError> {
        Ok(serde_json::json!({
            "type": "NativeFunctionGenerator",
            "argument": self.argument.to_string(),
        }))
    }
}

pub fn wrap_native_function<F>(
    params: &OnionStaticObject,
    capture: Option<&OnionStaticObject>,
    self_object: Option<&OnionStaticObject>,
    signature: String,
    function: F,
) -> OnionStaticObject
where
    F: Fn(OnionStaticObject, &mut GC<OnionObjectCell>) -> Result<OnionStaticObject, RuntimeError>
        + 'static,
{
    OnionLambdaDefinition::new_static(
        params,
        LambdaBody::NativeFunction(Arc::new(RefCell::new(NativeFunctionGenerator {
            argument: onion_tuple!(),
            function: Arc::new(RefCell::new(function)),
        }))),
        capture,
        self_object,
        signature,
    )
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();
    module.insert("io".to_string(), io::build_module());
    module.insert("types".to_string(), types::build_module());
    module.insert("math".to_string(), math::build_module());
    module.insert("string".to_string(), string::build_module());
    module.insert("http".to_string(), http::build_module());
    module.insert("time".to_string(), time::build_module());
    module.insert("json".to_string(), json::build_module());
    build_named_dict(module)
}
