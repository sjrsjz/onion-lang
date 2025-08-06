use std::sync::Arc;

use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinition},
            parameter::LambdaParameter,
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        pair::OnionPair,
        tuple::OnionTuple,
    },
    unwrap_step_result,
    utils::fastmap::{OnionFastMap, OnionKeyPool},
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

pub struct NativeFunctionGenerator<F>
where
    F: Fn(
        &OnionFastMap<Box<str>, OnionStaticObject>,
        &mut GC<OnionObjectCell>,
    ) -> Result<OnionStaticObject, RuntimeError>,
{
    captured: OnionFastMap<Box<str>, OnionStaticObject>,
    function: F,
}

impl<F> Runnable for NativeFunctionGenerator<F>
where
    F: Fn(
            &OnionFastMap<Box<str>, OnionStaticObject>,
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
    fn format_context(&self) -> String {
        // Use the type name of the function/closure to identify the native code.
        let full_type_name = std::any::type_name_of_val(&self.function);

        // Provide a shorter, more readable version of the type name.
        let short_type_name = full_type_name.split("::").last().unwrap_or(full_type_name);

        // Assemble all the information into a clear, structured block.
        format!(
            "-> Executing Native Function:\n   - Function: {} (Full Type: {})\n   - Argument: {:?}",
            short_type_name,
            full_type_name, // Include full name for disambiguation
            self.captured,
        )
    }
}

pub fn wrap_native_function<F>(
    params: LambdaParameter,
    capture: OnionFastMap<Box<str>, OnionObject>,
    signature: &str,
    string_pool: OnionKeyPool<Box<str>>,
    function: &'static F,
) -> OnionStaticObject
where
    F: Fn(
            &OnionFastMap<Box<str>, OnionStaticObject>,
            &mut GC<OnionObjectCell>,
        ) -> Result<OnionStaticObject, RuntimeError>
        + Send
        + Sync
        + 'static,
{
    let cloned_pool = string_pool.clone();
    OnionLambdaDefinition::new_static(
        params,
        LambdaBody::NativeFunction((
            Arc::new(
                move |_,
                      argument: &OnionFastMap<Box<str>, OnionStaticObject>,
                      capture: &OnionFastMap<Box<str>, OnionObject>,
                      _| {
                    let mut captured = argument.clone();
                    for (key, value) in capture.pairs() {
                        captured.push_with_index(*key, value.stabilize());
                    }
                    Box::new(NativeFunctionGenerator { captured, function })
                },
            ),
            cloned_pool,
        )),
        capture,
        signature.into(),
        LambdaType::Atomic,
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
    build_dict(module)
}
