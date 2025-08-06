use std::sync::Arc;

use onion_vm::{
    GC,
    lambda::runnable::{Runnable, RuntimeError, StepResult},
    types::{
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinition},
            parameter::LambdaParameter,
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    unwrap_step_result,
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

pub struct NativeFunctionGenerator<F>
where
    F: Fn(
        &OnionFastMap<Box<str>, OnionStaticObject>,
        &mut GC<OnionObjectCell>,
    ) -> Result<OnionStaticObject, RuntimeError>,
{
    captured: OnionFastMap<Box<str>, OnionStaticObject>,
    function: Arc<F>,
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
    function: Arc<F>,
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
                    Box::new(NativeFunctionGenerator {
                        captured,
                        function: function.clone(),
                    })
                },
            ),
            cloned_pool,
        )),
        capture,
        signature.into(),
        LambdaType::Atomic,
    )
}
