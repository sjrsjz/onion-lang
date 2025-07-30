use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
};
use rustc_hash::FxHashMap;

use super::{build_dict, get_attr_direct, wrap_native_function};

fn abs(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => Ok(OnionObject::Integer(n.abs()).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Float(f.abs()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "abs requires numeric value".to_string().into(),
            )),
        })
    })
}

fn sin(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).sin()).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Float(f.sin()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "sin requires numeric value".to_string().into(),
            )),
        })
    })
}

fn cos(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).cos()).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Float(f.cos()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "cos requires numeric value".to_string().into(),
            )),
        })
    })
}

fn tan(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).tan()).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Float(f.tan()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "tan requires numeric value".to_string().into(),
            )),
        })
    })
}

fn log(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n <= 0 {
                    Err(RuntimeError::InvalidOperation(
                        "log requires positive value".to_string().into(),
                    ))
                } else {
                    Ok(OnionObject::Float((*n as f64).ln()).stabilize())
                }
            }
            OnionObject::Float(f) => {
                if *f <= 0.0 {
                    Err(RuntimeError::InvalidOperation(
                        "log requires positive value".to_string().into(),
                    ))
                } else {
                    Ok(OnionObject::Float(f.ln()).stabilize())
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "log requires numeric value".to_string().into(),
            )),
        })
    })
}

fn sqrt(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                if *n < 0 {
                    Err(RuntimeError::InvalidOperation(
                        "Cannot take square root of negative number"
                            .to_string()
                            .into(),
                    ))
                } else {
                    Ok(OnionObject::Float((*n as f64).sqrt()).stabilize())
                }
            }
            OnionObject::Float(f) => {
                if *f < 0.0 {
                    Err(RuntimeError::InvalidOperation(
                        "Cannot take square root of negative number"
                            .to_string()
                            .into(),
                    ))
                } else {
                    Ok(OnionObject::Float(f.sqrt()).stabilize())
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "sqrt requires numeric value".to_string().into(),
            )),
        })
    })
}

fn pow(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let base = get_attr_direct(data, "base".to_string())?;
        let exponent = get_attr_direct(data, "exponent".to_string())?;

        base.weak().with_data(|base_data| {
            exponent
                .weak()
                .with_data(|exp_data| match (base_data, exp_data) {
                    (OnionObject::Integer(base), OnionObject::Integer(exp)) => {
                        if *exp >= 0 {
                            Ok(OnionObject::Integer(base.pow(*exp as u32)).stabilize())
                        } else {
                            Ok(OnionObject::Float((*base as f64).powf(*exp as f64)).stabilize())
                        }
                    }
                    (OnionObject::Float(base), OnionObject::Float(exp)) => {
                        Ok(OnionObject::Float(base.powf(*exp)).stabilize())
                    }
                    (OnionObject::Integer(base), OnionObject::Float(exp)) => {
                        Ok(OnionObject::Float((*base as f64).powf(*exp)).stabilize())
                    }
                    (OnionObject::Float(base), OnionObject::Integer(exp)) => {
                        Ok(OnionObject::Float(base.powf(*exp as f64)).stabilize())
                    }
                    _ => Err(RuntimeError::InvalidOperation(
                        "pow requires numeric values".to_string().into(),
                    )),
                })
        })
    })
}

fn exp(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).exp()).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Float(f.exp()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "exp requires numeric value".to_string().into(),
            )),
        })
    })
}

fn floor(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => Ok(OnionObject::Integer(*n).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Integer(f.floor() as i64).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "floor requires numeric value".to_string().into(),
            )),
        })
    })
}

fn ceil(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => Ok(OnionObject::Integer(*n).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Integer(f.ceil() as i64).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "ceil requires numeric value".to_string().into(),
            )),
        })
    })
}

fn round(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => Ok(OnionObject::Integer(*n).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Integer(f.round() as i64).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "round requires numeric value".to_string().into(),
            )),
        })
    })
}

fn asin(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                let val = *n as f64;
                if val < -1.0 || val > 1.0 {
                    Err(RuntimeError::InvalidOperation(
                        "asin requires value between -1 and 1".to_string().into(),
                    ))
                } else {
                    Ok(OnionObject::Float(val.asin()).stabilize())
                }
            }
            OnionObject::Float(f) => {
                if *f < -1.0 || *f > 1.0 {
                    Err(RuntimeError::InvalidOperation(
                        "asin requires value between -1 and 1".to_string().into(),
                    ))
                } else {
                    Ok(OnionObject::Float(f.asin()).stabilize())
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "asin requires numeric value".to_string().into(),
            )),
        })
    })
}

fn acos(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => {
                let val = *n as f64;
                if val < -1.0 || val > 1.0 {
                    Err(RuntimeError::InvalidOperation(
                        "acos requires value between -1 and 1".to_string().into(),
                    ))
                } else {
                    Ok(OnionObject::Float(val.acos()).stabilize())
                }
            }
            OnionObject::Float(f) => {
                if *f < -1.0 || *f > 1.0 {
                    Err(RuntimeError::InvalidOperation(
                        "acos requires value between -1 and 1".to_string().into(),
                    ))
                } else {
                    Ok(OnionObject::Float(f.acos()).stabilize())
                }
            }
            _ => Err(RuntimeError::InvalidOperation(
                "acos requires numeric value".to_string().into(),
            )),
        })
    })
}

fn atan(
    argument: &OnionStaticObject,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    argument.weak().with_data(|data| {
        let value = get_attr_direct(data, "value".to_string())?;
        value.weak().with_data(|value_data| match value_data {
            OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).atan()).stabilize()),
            OnionObject::Float(f) => Ok(OnionObject::Float(f.atan()).stabilize()),
            _ => Err(RuntimeError::InvalidOperation(
                "atan requires numeric value".to_string().into(),
            )),
        })
    })
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // 数学常量
    module.insert(
        "PI".to_string(),
        OnionObject::Float(std::f64::consts::PI).stabilize(),
    );
    module.insert(
        "E".to_string(),
        OnionObject::Float(std::f64::consts::E).stabilize(),
    );

    // 统一参数定义
    let single_arg = &OnionObject::String("value".to_string().into()).stabilize();
    let pow_args = &["base", "exponent"];

    // 注册所有实现，单参数统一，pow多参数
    module.insert(
        "abs".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::abs".to_string(),
            &abs,
        ),
    );
    module.insert(
        "sin".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::sin".to_string(),
            &sin,
        ),
    );
    module.insert(
        "cos".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::cos".to_string(),
            &cos,
        ),
    );
    module.insert(
        "tan".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::tan".to_string(),
            &tan,
        ),
    );
    module.insert(
        "log".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::log".to_string(),
            &log,
        ),
    );
    module.insert(
        "sqrt".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::sqrt".to_string(),
            &sqrt,
        ),
    );
    module.insert(
        "pow".to_string(),
        wrap_native_function(
            &super::build_string_tuple(pow_args),
            &FxHashMap::default(),
            "math::pow".to_string(),
            &pow,
        ),
    );
    module.insert(
        "exp".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::exp".to_string(),
            &exp,
        ),
    );
    module.insert(
        "floor".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::floor".to_string(),
            &floor,
        ),
    );
    module.insert(
        "ceil".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::ceil".to_string(),
            &ceil,
        ),
    );
    module.insert(
        "round".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::round".to_string(),
            &round,
        ),
    );
    module.insert(
        "asin".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::asin".to_string(),
            &asin,
        ),
    );
    module.insert(
        "acos".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::acos".to_string(),
            &acos,
        ),
    );
    module.insert(
        "atan".to_string(),
        wrap_native_function(
            single_arg,
            &FxHashMap::default(),
            "math::atan".to_string(),
            &atan,
        ),
    );

    build_dict(module)
}
