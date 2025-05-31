use std::{
    collections::HashMap,
    io::{stdout, Write},
};

use onion_vm::{
    lambda::runnable::RuntimeError,
    onion_tuple,
    types::{
        object::{ObjectError, OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    unwrap_object, GC,
};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

fn println(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| {
            unwrap_object!(data, OnionObject::Tuple).map(|tuple| {
                println!(
                    "{}",
                    tuple
                        .elements
                        .iter()
                        .map(|element| match element.try_borrow() {
                            Ok(obj) => obj.to_string().unwrap_or("<unknown>".to_string()),
                            Err(_) => "<borrow error>".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(" ")
                );
                Ok(OnionObject::Undefined(Some("Print completed".to_string())).stabilize())
            })
        })
        .map_err(RuntimeError::ObjectError)?
}

fn print(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| {
            unwrap_object!(data, OnionObject::Tuple).map(|tuple| {
                print!(
                    "{}",
                    tuple
                        .elements
                        .iter()
                        .map(|element| match element.try_borrow() {
                            Ok(obj) => obj.to_string().unwrap_or("<unknown>".to_string()),
                            Err(_) => "<borrow error>".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(" ")
                );
                Ok(OnionObject::Undefined(Some("Print completed".to_string())).stabilize())
            })
        })
        .map_err(RuntimeError::ObjectError)?
}

fn input(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| {
            let hint = get_attr_direct(data, "hint".to_string())?
                .weak()
                .try_borrow()?
                .to_string()?;
            print!("{}", hint);
            stdout()
                .flush()
                .map_err(|e| ObjectError::InvalidOperation(e.to_string()))?;
            let mut input = String::new();
            std::io::stdin()
                .read_line(&mut input)
                .map_err(|e| ObjectError::InvalidOperation(e.to_string()))?;
            Ok(OnionObject::String(input.trim().to_string()).stabilize())
        })
        .map_err(RuntimeError::ObjectError)
}

pub fn build_module() -> OnionStaticObject {
    let mut module = HashMap::new();
    module.insert(
        "println".to_string(),
        wrap_native_function(
            &onion_tuple!(),
            None,
            None,
            "io::println".to_string(),
            println,
        ),
    );
    module.insert(
        "print".to_string(),
        wrap_native_function(&onion_tuple!(), None, None, "io::print".to_string(), print),
    );

    let mut input_params = HashMap::new();
    input_params.insert(
        "hint".to_string(),
        OnionObject::String("Input: ".to_string()).stabilize(),
    );
    module.insert(
        "input".to_string(),
        wrap_native_function(
            &build_named_dict(input_params),
            None,
            None,
            "io::input".to_string(),
            input,
        ),
    );

    build_named_dict(module)
}
