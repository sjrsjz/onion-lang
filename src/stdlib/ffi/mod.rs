pub mod ctypes;
pub mod lib;
pub mod mem;

use indexmap::IndexMap;
use onion_vm::types::object::OnionStaticObject;

use super::build_named_dict;

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();
    module.insert("ctypes".to_string(), ctypes::build_module());
    module.insert("mem".to_string(), mem::build_module());
    module.insert("lib".to_string(), lib::build_module());
    build_named_dict(module)
}
