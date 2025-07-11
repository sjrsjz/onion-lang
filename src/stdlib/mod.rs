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
    unwrap_step_result, GC,
};

mod bytes;
mod ffi;
mod fs;
mod http;
mod io;
mod json;
mod math;
mod os;
mod string;
mod sys;
mod time;
mod tuple;
mod types;

pub fn build_named_dict(dict: IndexMap<String, OnionStaticObject>) -> OnionStaticObject {
    let mut pairs = vec![];
    for (key, value) in dict {
        pairs.push(OnionNamed::new_static(
            &OnionObject::String(key.into()).stabilize(),
            &value,
        ));
    }
    OnionTuple::new_static_no_ref(&pairs)
}

pub fn get_attr_direct(obj: &OnionObject, key: String) -> Result<OnionStaticObject, RuntimeError> {
    obj.with_attribute(&OnionObject::String(key.into()), &|obj| Ok(obj.stabilize()))
}

pub struct NativeFunctionGenerator<F>
where
    F: Fn(&OnionStaticObject, &mut GC<OnionObjectCell>) -> Result<OnionStaticObject, RuntimeError>
        + 'static,
{
    argument: OnionStaticObject,
    self_object: Option<OnionStaticObject>,
    function: &'static F,
}

impl<F> Runnable for NativeFunctionGenerator<F>
where
    F: Fn(&OnionStaticObject, &mut GC<OnionObjectCell>) -> Result<OnionStaticObject, RuntimeError>
        + Send
        + Sync
        + 'static,
{
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        unwrap_step_result!(
            (self.function)(&self.argument, gc).map(|result| StepResult::Return(result.into()))
        )
    }

    fn receive(
        &mut self,
        step_result: &StepResult,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        match step_result {
            StepResult::Return(result) => {
                self.argument = result.as_ref().clone();
                Ok(())
            }
            StepResult::SetSelfObject(self_object) => {
                self.self_object = Some(self_object.as_ref().clone());
                Ok(())
            }
            _ => Err(RuntimeError::DetailedError(
                "NativeFunctionGenerator received unexpected step result"
                    .to_string()
                    .into(),
            )),
        }
    }

    fn copy(&self) -> Box<dyn Runnable> {
        Box::new(NativeFunctionGenerator {
            argument: self.argument.clone(),
            self_object: self.self_object.clone(),
            function: self.function,
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
    function: &'static F,
) -> OnionStaticObject
where
    F: Fn(&OnionStaticObject, &mut GC<OnionObjectCell>) -> Result<OnionStaticObject, RuntimeError>
        + Send
        + Sync
        + 'static,
{
    OnionLambdaDefinition::new_static(
        params,
        LambdaBody::NativeFunction(Box::new(NativeFunctionGenerator {
            argument: onion_tuple!(),
            self_object: self_object.cloned(),
            function: function,
        })),
        capture,
        self_object,
        signature,
    )
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();
    module.insert("bytes".to_string(), bytes::build_module());
    module.insert("fs".to_string(), fs::build_module());
    module.insert("io".to_string(), io::build_module());
    module.insert("types".to_string(), types::build_module());
    module.insert("math".to_string(), math::build_module());
    module.insert("string".to_string(), string::build_module());
    module.insert("http".to_string(), http::build_module());
    module.insert("time".to_string(), time::build_module());
    module.insert("json".to_string(), json::build_module());
    module.insert("os".to_string(), os::build_module());
    module.insert("sys".to_string(), sys::build_module());
    module.insert("ffi".to_string(), ffi::build_module());
    build_named_dict(module)
}
