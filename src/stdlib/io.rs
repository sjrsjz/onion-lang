use std::io::Write;

use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError, types::{lambda::parameter::LambdaParameter, object::{OnionObject, OnionObjectCell, OnionStaticObject}}, utils::fastmap::{OnionFastMap, OnionKeyPool}, GC
};

use super::{build_dict, wrap_native_function};

fn println(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"values".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "println requires a 'values' argument".into(),
        ));
    };

    println!("{}", value.weak().to_string(&vec![])?);
    Ok(OnionObject::Undefined(Some("Print completed".into())).stabilize())
}

fn print(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"values".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "print requires a 'values' argument".into(),
        ));
    };

    print!("{}", value.weak().to_string(&vec![])?);
    Ok(OnionObject::Undefined(Some("Print completed".into())).stabilize())
}

fn input(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(hint) = argument.get(&"hint".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "input requires a 'hint' argument".into(),
        ));
    };

    print!("{}", hint.weak().to_string(&vec![])?);
    // flush
    std::io::stdout()
        .flush()
        .map_err(|e| RuntimeError::DetailedError(format!("Failed to flush stdout: {e}").into()))?;
    let input = {
        let mut buffer = String::new();
        if let Err(e) = std::io::stdin().read_line(&mut buffer) {
            return Err(RuntimeError::DetailedError(
                format!("Failed to read input: {e}").into(),
            ));
        }
        buffer.trim().to_string()
    };
    Ok(OnionObject::String(input.into()).stabilize())
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // io.println(values)
    module.insert(
        "println".to_string(),
        wrap_native_function(
            LambdaParameter::top("values"), // <-- Changed here
            OnionFastMap::default(),
            "io::println".to_string(),
            OnionKeyPool::create(vec!["values".to_string()]),
            &println,
        ),
    );

    // io.print(values)
    module.insert(
        "print".to_string(),
        wrap_native_function(
            LambdaParameter::top("values"), // <-- Changed here
            OnionFastMap::default(),
            "io::print".to_string(),
            OnionKeyPool::create(vec!["values".to_string()]),
            &print,
        ),
    );

    // io.input(hint)
    module.insert(
        "input".to_string(),
        wrap_native_function(
            LambdaParameter::top("hint"), // <-- Changed here
            OnionFastMap::default(),
            "io::input".to_string(),
            OnionKeyPool::create(vec!["hint".to_string()]),
            &input,
        ),
    );

    build_dict(module)
}
