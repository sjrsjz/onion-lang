use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        lambda::parameter::LambdaParameter,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
    },
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

// 引入所需的辅助函数
use super::{build_dict, wrap_native_function};

fn abs(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "abs requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => Ok(OnionObject::Integer(n.abs()).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Float(f.abs()).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "abs requires a numeric value".into(),
        )),
    })
}

fn sin(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "sin requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).sin()).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Float(f.sin()).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "sin requires a numeric value".into(),
        )),
    })
}

fn cos(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "cos requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).cos()).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Float(f.cos()).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "cos requires a numeric value".into(),
        )),
    })
}

fn tan(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "tan requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).tan()).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Float(f.tan()).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "tan requires a numeric value".into(),
        )),
    })
}

fn log(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "log requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => {
            if *n <= 0 {
                Err(RuntimeError::InvalidOperation(
                    "log requires a positive value".into(),
                ))
            } else {
                Ok(OnionObject::Float((*n as f64).ln()).stabilize())
            }
        }
        OnionObject::Float(f) => {
            if *f <= 0.0 {
                Err(RuntimeError::InvalidOperation(
                    "log requires a positive value".into(),
                ))
            } else {
                Ok(OnionObject::Float(f.ln()).stabilize())
            }
        }
        _ => Err(RuntimeError::InvalidType(
            "log requires a numeric value".into(),
        )),
    })
}

fn sqrt(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "sqrt requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => {
            if *n < 0 {
                Err(RuntimeError::InvalidOperation(
                    "Cannot take square root of a negative number"
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
                    "Cannot take square root of a negative number"
                        .to_string()
                        .into(),
                ))
            } else {
                Ok(OnionObject::Float(f.sqrt()).stabilize())
            }
        }
        _ => Err(RuntimeError::InvalidType(
            "sqrt requires a numeric value".into(),
        )),
    })
}

fn pow(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(base_obj) = argument.get(&"base".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "pow requires a 'base' argument".into(),
        ));
    };
    let Some(exp_obj) = argument.get(&"exponent".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "pow requires an 'exponent' argument".into(),
        ));
    };

    base_obj.weak().with_data(|base_data| {
        exp_obj
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
                _ => Err(RuntimeError::InvalidType(
                    "pow requires numeric values for base and exponent"
                        .to_string()
                        .into(),
                )),
            })
    })
}

fn exp(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "exp requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).exp()).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Float(f.exp()).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "exp requires a numeric value".into(),
        )),
    })
}

fn floor(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "floor requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => Ok(OnionObject::Integer(*n).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Integer(f.floor() as i64).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "floor requires a numeric value".into(),
        )),
    })
}

fn ceil(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "ceil requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => Ok(OnionObject::Integer(*n).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Integer(f.ceil() as i64).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "ceil requires a numeric value".into(),
        )),
    })
}

fn round(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "round requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => Ok(OnionObject::Integer(*n).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Integer(f.round() as i64).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "round requires a numeric value".into(),
        )),
    })
}

fn asin(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "asin requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| {
        let val_f64 = match value_data {
            OnionObject::Integer(n) => *n as f64,
            OnionObject::Float(f) => *f,
            _ => {
                return Err(RuntimeError::InvalidType(
                    "asin requires a numeric value".into(),
                ));
            }
        };
        if !(-1.0..=1.0).contains(&val_f64) {
            Err(RuntimeError::InvalidOperation(
                "asin requires a value between -1 and 1".into(),
            ))
        } else {
            Ok(OnionObject::Float(val_f64.asin()).stabilize())
        }
    })
}

fn acos(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "acos requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| {
        let val_f64 = match value_data {
            OnionObject::Integer(n) => *n as f64,
            OnionObject::Float(f) => *f,
            _ => {
                return Err(RuntimeError::InvalidType(
                    "acos requires a numeric value".into(),
                ));
            }
        };
        if !(-1.0..=1.0).contains(&val_f64) {
            Err(RuntimeError::InvalidOperation(
                "acos requires a value between -1 and 1".into(),
            ))
        } else {
            Ok(OnionObject::Float(val_f64.acos()).stabilize())
        }
    })
}

fn atan(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get(&"value".to_string()) else {
        return Err(RuntimeError::DetailedError(
            "atan requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::Integer(n) => Ok(OnionObject::Float((*n as f64).atan()).stabilize()),
        OnionObject::Float(f) => Ok(OnionObject::Float(f.atan()).stabilize()),
        _ => Err(RuntimeError::InvalidType(
            "atan requires a numeric value".into(),
        )),
    })
}

pub fn build_module() -> OnionStaticObject {
    let mut module = IndexMap::new();

    // --- Constants ---
    module.insert(
        "PI".to_string(),
        OnionObject::Float(std::f64::consts::PI).stabilize(),
    );
    module.insert(
        "E".to_string(),
        OnionObject::Float(std::f64::consts::E).stabilize(),
    );

    // --- Single-argument functions (all take "value") ---
    module.insert(
        "abs".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::abs".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &abs,
        ),
    );
    module.insert(
        "sin".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::sin".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &sin,
        ),
    );
    module.insert(
        "cos".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::cos".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &cos,
        ),
    );
    module.insert(
        "tan".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::tan".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &tan,
        ),
    );
    module.insert(
        "log".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::log".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &log,
        ),
    );
    module.insert(
        "sqrt".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::sqrt".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &sqrt,
        ),
    );
    module.insert(
        "exp".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::exp".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &exp,
        ),
    );
    module.insert(
        "floor".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::floor".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &floor,
        ),
    );
    module.insert(
        "ceil".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::ceil".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &ceil,
        ),
    );
    module.insert(
        "round".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::round".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &round,
        ),
    );
    module.insert(
        "asin".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::asin".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &asin,
        ),
    );
    module.insert(
        "acos".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::acos".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &acos,
        ),
    );
    module.insert(
        "atan".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::atan".to_string(),
            OnionKeyPool::create(vec!["value".to_string()]),
            &atan,
        ),
    );

    // --- Multi-argument functions ---
    module.insert(
        "pow".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(vec![
                LambdaParameter::top("base"),
                LambdaParameter::top("exponent"),
            ]),
            OnionFastMap::default(),
            "math::pow".to_string(),
            OnionKeyPool::create(vec!["base".to_string(), "exponent".to_string()]),
            &pow,
        ),
    );

    build_dict(module)
}
