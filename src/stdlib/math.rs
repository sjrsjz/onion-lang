use indexmap::IndexMap;
use onion_vm::{
    GC,
    lambda::runnable::RuntimeError,
    types::{
        float_value::OnionFloatValue,
        integer_value::OnionIntegerValue,
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
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "abs requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => Ok(OnionIntegerValue::new_static(n.value().abs())),
        OnionObject::FloatValue(f) => Ok(OnionFloatValue::new_static(f.value().abs())),
        _ => Err(RuntimeError::InvalidType(
            "abs requires a numeric value".into(),
        )),
    })
}

fn sin(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "sin requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => Ok(OnionFloatValue::new_static((n.value() as f64).sin())),
        OnionObject::FloatValue(f) => Ok(OnionFloatValue::new_static(f.value().sin())),
        _ => Err(RuntimeError::InvalidType(
            "sin requires a numeric value".into(),
        )),
    })
}

fn cos(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "cos requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => Ok(OnionFloatValue::new_static((n.value() as f64).cos())),
        OnionObject::FloatValue(f) => Ok(OnionFloatValue::new_static(f.value().cos())),
        _ => Err(RuntimeError::InvalidType(
            "cos requires a numeric value".into(),
        )),
    })
}

fn tan(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "tan requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => Ok(OnionFloatValue::new_static((n.value() as f64).tan())),
        OnionObject::FloatValue(f) => Ok(OnionFloatValue::new_static(f.value().tan())),
        _ => Err(RuntimeError::InvalidType(
            "tan requires a numeric value".into(),
        )),
    })
}

fn log(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "log requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => {
            if n.value() <= 0 {
                Err(RuntimeError::InvalidOperation(
                    "log requires a positive value".into(),
                ))
            } else {
                Ok(OnionFloatValue::new_static((n.value() as f64).ln()))
            }
        }
        OnionObject::FloatValue(f) => {
            if f.value() <= 0.0 {
                Err(RuntimeError::InvalidOperation(
                    "log requires a positive value".into(),
                ))
            } else {
                Ok(OnionFloatValue::new_static(f.value().ln()))
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
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "sqrt requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => {
            if n.value() < 0 {
                Err(RuntimeError::InvalidOperation(
                    "Cannot take square root of a negative number"
                        .to_string()
                        .into(),
                ))
            } else {
                Ok(OnionFloatValue::new_static((n.value() as f64).sqrt()))
            }
        }
        OnionObject::FloatValue(f) => {
            if f.value() < 0.0 {
                Err(RuntimeError::InvalidOperation(
                    "Cannot take square root of a negative number"
                        .to_string()
                        .into(),
                ))
            } else {
                Ok(OnionFloatValue::new_static(f.value().sqrt()))
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
    let Some(base_obj) = argument.get("base") else {
        return Err(RuntimeError::DetailedError(
            "pow requires a 'base' argument".into(),
        ));
    };
    let Some(exp_obj) = argument.get("exponent") else {
        return Err(RuntimeError::DetailedError(
            "pow requires an 'exponent' argument".into(),
        ));
    };

    base_obj.weak().with_data(|base_data| {
        exp_obj
            .weak()
            .with_data(|exp_data| match (base_data, exp_data) {
                (OnionObject::IntegerValue(base), OnionObject::IntegerValue(exp)) => {
                    if exp.value() >= 0 {
                        Ok(OnionIntegerValue::new_static(
                            base.value().pow(exp.value() as u32),
                        ))
                    } else {
                        Ok(OnionFloatValue::new_static(
                            (base.value() as f64).powf(exp.value() as f64),
                        ))
                    }
                }
                (OnionObject::FloatValue(base), OnionObject::FloatValue(exp)) => {
                    Ok(OnionFloatValue::new_static(base.value().powf(exp.value())))
                }
                (OnionObject::IntegerValue(base), OnionObject::FloatValue(exp)) => Ok(
                    OnionFloatValue::new_static((base.value() as f64).powf(exp.value())),
                ),
                (OnionObject::FloatValue(base), OnionObject::IntegerValue(exp)) => Ok(
                    OnionFloatValue::new_static(base.value().powf(exp.value() as f64)),
                ),
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
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "exp requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => Ok(OnionFloatValue::new_static((n.value() as f64).exp())),
        OnionObject::FloatValue(f) => Ok(OnionFloatValue::new_static(f.value().exp())),
        _ => Err(RuntimeError::InvalidType(
            "exp requires a numeric value".into(),
        )),
    })
}

fn floor(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "floor requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => Ok(OnionIntegerValue::new_static(n.value())),
        OnionObject::FloatValue(f) => Ok(OnionFloatValue::new_static(f.value().floor())),
        _ => Err(RuntimeError::InvalidType(
            "floor requires a numeric value".into(),
        )),
    })
}

fn ceil(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "ceil requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => Ok(OnionIntegerValue::new_static(n.value())),
        OnionObject::FloatValue(f) => Ok(OnionIntegerValue::new_static(f.value().ceil() as i64)),
        _ => Err(RuntimeError::InvalidType(
            "ceil requires a numeric value".into(),
        )),
    })
}

fn round(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "round requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => Ok(OnionIntegerValue::new_static(n.value())),
        OnionObject::FloatValue(f) => Ok(OnionIntegerValue::new_static(f.value().round() as i64)),
        _ => Err(RuntimeError::InvalidType(
            "round requires a numeric value".into(),
        )),
    })
}

fn asin(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "asin requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| {
        let val_f64 = match value_data {
            OnionObject::IntegerValue(n) => n.value() as f64,
            OnionObject::FloatValue(f) => f.value(),
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
            Ok(OnionFloatValue::new_static(val_f64.asin()))
        }
    })
}

fn acos(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "acos requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| {
        let val_f64 = match value_data {
            OnionObject::IntegerValue(n) => n.value() as f64,
            OnionObject::FloatValue(f) => f.value(),
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
            Ok(OnionFloatValue::new_static(val_f64.acos()))
        }
    })
}

fn atan(
    argument: &OnionFastMap<Box<str>, OnionStaticObject>,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<OnionStaticObject, RuntimeError> {
    let Some(value) = argument.get("value") else {
        return Err(RuntimeError::DetailedError(
            "atan requires a 'value' argument".into(),
        ));
    };

    value.weak().with_data(|value_data| match value_data {
        OnionObject::IntegerValue(n) => Ok(OnionFloatValue::new_static((n.value() as f64).atan())),
        OnionObject::FloatValue(f) => Ok(OnionFloatValue::new_static(f.value().atan())),
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
        OnionFloatValue::new_static(std::f64::consts::PI),
    );
    module.insert(
        "E".to_string(),
        OnionFloatValue::new_static(std::f64::consts::E),
    );

    // --- Single-argument functions (all take "value") ---
    module.insert(
        "abs".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::abs",
            OnionKeyPool::create(vec!["value".into()]),
            &abs,
        ),
    );
    module.insert(
        "sin".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::sin",
            OnionKeyPool::create(vec!["value".into()]),
            &sin,
        ),
    );
    module.insert(
        "cos".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::cos",
            OnionKeyPool::create(vec!["value".into()]),
            &cos,
        ),
    );
    module.insert(
        "tan".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::tan",
            OnionKeyPool::create(vec!["value".into()]),
            &tan,
        ),
    );
    module.insert(
        "log".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::log",
            OnionKeyPool::create(vec!["value".into()]),
            &log,
        ),
    );
    module.insert(
        "sqrt".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::sqrt",
            OnionKeyPool::create(vec!["value".into()]),
            &sqrt,
        ),
    );
    module.insert(
        "exp".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::exp",
            OnionKeyPool::create(vec!["value".into()]),
            &exp,
        ),
    );
    module.insert(
        "floor".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::floor",
            OnionKeyPool::create(vec!["value".into()]),
            &floor,
        ),
    );
    module.insert(
        "ceil".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::ceil",
            OnionKeyPool::create(vec!["value".into()]),
            &ceil,
        ),
    );
    module.insert(
        "round".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::round",
            OnionKeyPool::create(vec!["value".into()]),
            &round,
        ),
    );
    module.insert(
        "asin".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::asin",
            OnionKeyPool::create(vec!["value".into()]),
            &asin,
        ),
    );
    module.insert(
        "acos".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::acos",
            OnionKeyPool::create(vec!["value".into()]),
            &acos,
        ),
    );
    module.insert(
        "atan".to_string(),
        wrap_native_function(
            LambdaParameter::top("value"),
            OnionFastMap::default(),
            "math::atan",
            OnionKeyPool::create(vec!["value".into()]),
            &atan,
        ),
    );

    // --- Multi-argument functions ---
    module.insert(
        "pow".to_string(),
        wrap_native_function(
            LambdaParameter::Multiple(
                [
                    LambdaParameter::top("base"),
                    LambdaParameter::top("exponent"),
                ]
                .into(),
            ),
            OnionFastMap::default(),
            "math::pow",
            OnionKeyPool::create(vec!["base".into(), "exponent".into()]),
            &pow,
        ),
    );

    build_dict(module)
}
