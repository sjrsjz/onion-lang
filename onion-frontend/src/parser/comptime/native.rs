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
    self_object: Option<OnionStaticObject>,
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

    fn capture(
        &mut self,
        argument: &OnionFastMap<Box<str>, OnionStaticObject>,
        captured_vars: &OnionFastMap<Box<str>, OnionObject>,
        _gc: &mut GC<OnionObjectCell>,
    ) -> Result<(), RuntimeError> {
        self.captured = argument.clone();
        for (key, value) in captured_vars.pairs() {
            self.captured.push_with_index(*key, value.stabilize());
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
        let full_type_name = std::any::type_name_of_val(&self.function);

        // Provide a shorter, more readable version of the type name.
        let short_type_name = full_type_name.split("::").last().unwrap_or(full_type_name);

        // Format the 'self' object, which acts as context for this runnable.
        let self_info = match &self.self_object {
            Some(obj) => format!("{obj:?}"),
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
    params: LambdaParameter,
    capture: OnionFastMap<Box<str>, OnionObject>,
    signature: &'static str,
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
            Arc::new(move || {
                Box::new(NativeFunctionGenerator {
                    captured: OnionFastMap::new(string_pool.clone()),
                    self_object: None,
                    function: function.clone(),
                })
            }),
            cloned_pool,
        )),
        capture,
        signature.into(),
        LambdaType::Normal,
    )
}
