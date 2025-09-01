use std::vec;

use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    smallvec::SmallVec,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObjectCell, OnionStaticObject},
        string_value::OnionStringValue,
        undefined::OnionUndefined,
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

use crate::stdlib::tuple;

use super::{build_dict, wrap_native_function};

/// Convert object to string
fn display(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        // Get parameter directly
        return Err(RuntimeError::DetailedError(
            "to_string requires a 'value' argument".into(),
        ));
    };
    let string_representation = value.weak().display(&vec![])?;
    Ok(OnionStringValue::new_static(string_representation))
}

// get attr or undefined
fn find(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>, // Changed signature
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(obj) = argument.get("obj") else {
        return Err(RuntimeError::DetailedError(
            "find requires an 'obj' argument".into(),
        ));
    };
    let Some(key) = argument.get("key") else {
        return Err(RuntimeError::DetailedError(
            "find requires a 'key' argument".into(),
        ));
    };

    let key_borrowed = key.weak();
    match obj
        .weak()
        .with_attribute(key_borrowed, &mut SmallVec::new(), &|_, obj| {
            Ok(obj.stabilize())
        }) {
        Ok(value) => Ok(value),
        Err(RuntimeError::InvalidOperation(ref err)) => {
            // If the attribute is not found, return undefined
            Ok(OnionUndefined::new_static(Some(err.as_ref())))
        }
        Err(e) => {
            // If any other error occurs, propagate it
            Err(e)
        }
    }
}

/// Build the type conversion module
pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // Type conversion functions
    module.insert(
        "display".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "types::display",
            OnionKeyPool::create(vec!["value".into()]),
            &display,
        ),
    );

    // Find attribute function (has two parameters)
    module.insert(
        "find".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [LambdaParameter::top("obj"), LambdaParameter::top("key")].into(),
            ),
            OnionFastMap::default(), // No default arguments for these
            "types::find",
            OnionKeyPool::create(vec!["obj".into(), "key".into()]),
            &find,
        ),
    );

    // Assuming tuple::build_module() is already updated and compatible
    module.insert("tuple".to_string(), tuple::build_module());

    build_dict(module)
}
