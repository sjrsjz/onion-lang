use std::collections::HashMap;

use onion_vm::{
    lambda::runnable::RuntimeError,
    onion_tuple,
    types::{object::{ObjectError, OnionObject, OnionStaticObject}, tuple::OnionTuple},
    GC,
};

use super::{build_dict, wrap_native_function};

fn print(
    argument: OnionStaticObject,
    _gc: &mut GC<OnionObject>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument
        .weak()
        .with_data(|data| match data {
            OnionObject::Tuple(tuple) => {
                for item in tuple.elements.iter() {
                    println!("{:?}", item);
                }
                Ok(OnionObject::Undefined("Print completed".to_string()).stabilize())
            }
            _ => Err(ObjectError::InvalidOperation(
                "Expected a tuple for print".to_string(),
            )),
        })
        .map_err(RuntimeError::ObjectError)
}

pub fn build_module() -> OnionStaticObject {
    let mut module = HashMap::new();
    module.insert(
        "print".to_string(),
        wrap_native_function(&onion_tuple!(), None, None, "io::print".to_string(), print),
    );
    build_dict(module)
}
