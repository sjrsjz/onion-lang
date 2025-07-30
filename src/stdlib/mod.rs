use std::sync::Arc;

use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    onion_tuple,
    types::{
        lambda::definition::{LambdaBody, LambdaType, OnionLambdaDefinition},
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        pair::OnionPair,
        tuple::OnionTuple,
    },
    unwrap_step_result,
};
use rustc_hash::FxHashMap;

// mod bytes;
// mod ffi;
mod fs;
// mod http;
mod io;
mod json;
// mod math;
// mod os;
// mod string;
// mod sys;
// mod time;
mod tuple;
mod types;

pub fn build_dict(dict: IndexMap<String, OnionStaticObject>) -> OnionStaticObject {
    let mut pairs = vec![];
    for (key, value) in dict {
        pairs.push(OnionPair::new_static(
            &OnionObject::String(key.into()).stabilize(),
            &value,
        ));
    }
    OnionTuple::new_static_no_ref(&pairs)
}

pub fn build_string_tuple(strings: &[&str]) -> OnionStaticObject {
    let mut elements = vec![];
    for s in strings {
        elements.push(OnionObject::String(s.to_string().into()).consume_and_stabilize());
    }
    OnionTuple::new_static_no_ref(&elements)
}

pub fn get_attr_direct(obj: &OnionObject, key: String) -> Result<OnionStaticObject, RuntimeError> {
    obj.with_attribute(&OnionObject::String(key.into()), &|obj| Ok(obj.stabilize()))
}

pub struct NativeFunctionGenerator<F>
where
    F: Fn(
            &FxHashMap<String, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + 'static,
{
    captured: FxHashMap<String, OnionStaticObject>,
    self_object: Option<OnionStaticObject>,
    function: &'static F,
}

impl<F> Runnable for NativeFunctionGenerator<F>
where
    F: Fn(
            &FxHashMap<String, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + Send
        + Sync
        + 'static,
{
    fn step(&mut self, gc: &mut GC<OnionObjectCell>) -> StepResult {
        unwrap_step_result!(
            (self.function)(&self.captured, gc).map(|result| StepResult::Return(result.into()))
        )
    }

    fn capture(
        &mut self,
        argument: &FxHashMap<String, OnionStaticObject>,
        captured_vars: &FxHashMap<String, OnionObject>,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        self.captured = argument.clone();
        for (key, value) in captured_vars {
            self.captured.insert(key.clone(), value.stabilize());
        }
        Ok(())
    }

    fn bind_self_object(
        &mut self,
        self_object: &OnionObject,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        self.self_object = Some(self_object.stabilize());
        Ok(())
    }

    fn format_context(&self) -> String {
        // Use the type name of the function/closure to identify the native code.
        let full_type_name = std::any::type_name_of_val(self.function);

        // Provide a shorter, more readable version of the type name.
        let short_type_name = full_type_name.split("::").last().unwrap_or(full_type_name);

        // Format the 'self' object, which acts as context for this runnable.
        let self_info = match &self.self_object {
            Some(obj) => format!("{:?}", obj),
            None => "(None)".to_string(),
        };

        // Assemble all the information into a clear, structured block.
        format!(
            "-> Executing Native Function:\n   - Function: {} (Full Type: {})\n   - Argument: {:?}\n   - Context (self): {}",
            short_type_name,
            full_type_name, // Include full name for disambiguation
            self.captured,
            self_info
        )
    }
}

pub fn wrap_native_function<F>(
    params: &OnionStaticObject,
    capture: &FxHashMap<String, OnionObject>,
    signature: String,
    function: &'static F,
) -> OnionStaticObject
where
    F: Fn(
            &FxHashMap<String, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + Send
        + Sync
        + 'static,
{
    OnionLambdaDefinition::new_static(
        params,
        LambdaBody::NativeFunction(Arc::new(|| {
            Box::new(NativeFunctionGenerator {
                captured: FxHashMap::default(),
                self_object: None,
                function: function,
            })
        })),
        capture,
        signature,
        LambdaType::Normal,
    )
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();
    // module.insert("bytes".to_string(), bytes::build_module());
    module.insert("fs".to_string(), fs::build_module());
    module.insert("io".to_string(), io::build_module());
    module.insert("types".to_string(), types::build_module());
    // module.insert("math".to_string(), math::build_module());
    // module.insert("string".to_string(), string::build_module());
    // module.insert("http".to_string(), http::build_module());
    // module.insert("time".to_string(), time::build_module());
    module.insert("json".to_string(), json::build_module());
    // module.insert("os".to_string(), os::build_module());
    // module.insert("sys".to_string(), sys::build_module());
    // module.insert("ffi".to_string(), ffi::build_module());
    build_dict(module)
}
