use indexmap::IndexMap;
use onion_vm::{
    lambda::runnable::RuntimeError,
    types::object::{OnionObject, OnionObjectCell, OnionStaticObject},
    GC,
};

use super::{build_named_dict, get_attr_direct, wrap_native_function};

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

    // abs 函数
    let mut abs_params = IndexMap::new();
    abs_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Number to get absolute value".to_string().into())).stabilize(),
    );
    module.insert(
        "abs".to_string(),
        wrap_native_function(
            &build_named_dict(abs_params),
            None,
            None,
            "math::abs".to_string(),
            &abs,
        ),
    ); // sin 函数
    let mut sin_params = IndexMap::new();
    sin_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Angle in radians".to_string().into())).stabilize(),
    );
    module.insert(
        "sin".to_string(),
        wrap_native_function(
            &build_named_dict(sin_params),
            None,
            None,
            "math::sin".to_string(),
            &sin,
        ),
    );

    // cos 函数
    let mut cos_params = IndexMap::new();
    cos_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Angle in radians".to_string().into())).stabilize(),
    );
    module.insert(
        "cos".to_string(),
        wrap_native_function(
            &build_named_dict(cos_params),
            None,
            None,
            "math::cos".to_string(),
            &cos,
        ),
    );

    // tan 函数
    let mut tan_params = IndexMap::new();
    tan_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Angle in radians".to_string().into())).stabilize(),
    );
    module.insert(
        "tan".to_string(),
        wrap_native_function(
            &build_named_dict(tan_params),
            None,
            None,
            "math::tan".to_string(),
            &tan,
        ),
    );

    // log 函数
    let mut log_params = IndexMap::new();
    log_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some(
            "Number to calculate natural logarithm".to_string().into(),
        ))
        .stabilize(),
    );
    module.insert(
        "log".to_string(),
        wrap_native_function(
            &build_named_dict(log_params),
            None,
            None,
            "math::log".to_string(),
            &log,
        ),
    );

    // exp 函数
    let mut exp_params = IndexMap::new();
    exp_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Exponent for e^x".to_string().into())).stabilize(),
    );
    module.insert(
        "exp".to_string(),
        wrap_native_function(
            &build_named_dict(exp_params),
            None,
            None,
            "math::exp".to_string(),
            &exp,
        ),
    );

    // floor 函数
    let mut floor_params = IndexMap::new();
    floor_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Number to floor".to_string().into())).stabilize(),
    );
    module.insert(
        "floor".to_string(),
        wrap_native_function(
            &build_named_dict(floor_params),
            None,
            None,
            "math::floor".to_string(),
            &floor,
        ),
    );

    // ceil 函数
    let mut ceil_params = IndexMap::new();
    ceil_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Number to ceil".to_string().into())).stabilize(),
    );
    module.insert(
        "ceil".to_string(),
        wrap_native_function(
            &build_named_dict(ceil_params),
            None,
            None,
            "math::ceil".to_string(),
            &ceil,
        ),
    );

    // round 函数
    let mut round_params = IndexMap::new();
    round_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Number to round".to_string().into())).stabilize(),
    );
    module.insert(
        "round".to_string(),
        wrap_native_function(
            &build_named_dict(round_params),
            None,
            None,
            "math::round".to_string(),
            &round,
        ),
    );

    // asin 函数
    let mut asin_params = IndexMap::new();
    asin_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value between -1 and 1".to_string().into())).stabilize(),
    );
    module.insert(
        "asin".to_string(),
        wrap_native_function(
            &build_named_dict(asin_params),
            None,
            None,
            "math::asin".to_string(),
            &asin,
        ),
    );

    // acos 函数
    let mut acos_params = IndexMap::new();
    acos_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value between -1 and 1".to_string().into())).stabilize(),
    );
    module.insert(
        "acos".to_string(),
        wrap_native_function(
            &build_named_dict(acos_params),
            None,
            None,
            "math::acos".to_string(),
            &acos,
        ),
    );

    // atan 函数
    let mut atan_params = IndexMap::new();
    atan_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value for arctangent".to_string().into())).stabilize(),
    );
    module.insert(
        "atan".to_string(),
        wrap_native_function(
            &build_named_dict(atan_params),
            None,
            None,
            "math::atan".to_string(),
            &atan,
        ),
    );

    // sqrt 函数
    let mut sqrt_params = IndexMap::new();
    sqrt_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Number to calculate square root".to_string().into()))
            .stabilize(),
    );
    module.insert(
        "sqrt".to_string(),
        wrap_native_function(
            &build_named_dict(sqrt_params),
            None,
            None,
            "math::sqrt".to_string(),
            &sqrt,
        ),
    );

    // pow 函数
    let mut pow_params = IndexMap::new();
    pow_params.insert(
        "base".to_string(),
        OnionObject::Undefined(Some("Base number".to_string().into())).stabilize(),
    );
    pow_params.insert(
        "exponent".to_string(),
        OnionObject::Undefined(Some("Exponent (power)".to_string().into())).stabilize(),
    );
    module.insert(
        "pow".to_string(),
        wrap_native_function(
            &build_named_dict(pow_params),
            None,
            None,
            "math::pow".to_string(),
            &pow,
        ),
    );

    // exp 函数
    let mut exp_params = IndexMap::new();
    exp_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Number to calculate exponent".to_string().into())).stabilize(),
    );
    module.insert(
        "exp".to_string(),
        wrap_native_function(
            &build_named_dict(exp_params),
            None,
            None,
            "math::exp".to_string(),
            &exp,
        ),
    );

    // floor 函数
    let mut floor_params = IndexMap::new();
    floor_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Number to round down".to_string().into())).stabilize(),
    );
    module.insert(
        "floor".to_string(),
        wrap_native_function(
            &build_named_dict(floor_params),
            None,
            None,
            "math::floor".to_string(),
            &floor,
        ),
    );

    // ceil 函数
    let mut ceil_params = IndexMap::new();
    ceil_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Number to round up".to_string().into())).stabilize(),
    );
    module.insert(
        "ceil".to_string(),
        wrap_native_function(
            &build_named_dict(ceil_params),
            None,
            None,
            "math::ceil".to_string(),
            &ceil,
        ),
    );

    // round 函数
    let mut round_params = IndexMap::new();
    round_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Number to round".to_string().into())).stabilize(),
    );
    module.insert(
        "round".to_string(),
        wrap_native_function(
            &build_named_dict(round_params),
            None,
            None,
            "math::round".to_string(),
            &round,
        ),
    );

    // asin 函数
    let mut asin_params = IndexMap::new();
    asin_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value in radians".to_string().into())).stabilize(),
    );
    module.insert(
        "asin".to_string(),
        wrap_native_function(
            &build_named_dict(asin_params),
            None,
            None,
            "math::asin".to_string(),
            &asin,
        ),
    );

    // acos 函数
    let mut acos_params = IndexMap::new();
    acos_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value in radians".to_string().into())).stabilize(),
    );
    module.insert(
        "acos".to_string(),
        wrap_native_function(
            &build_named_dict(acos_params),
            None,
            None,
            "math::acos".to_string(),
            &acos,
        ),
    );

    // atan 函数
    let mut atan_params = IndexMap::new();
    atan_params.insert(
        "value".to_string(),
        OnionObject::Undefined(Some("Value in radians".to_string().into())).stabilize(),
    );
    module.insert(
        "atan".to_string(),
        wrap_native_function(
            &build_named_dict(atan_params),
            None,
            None,
            "math::atan".to_string(),
            &atan,
        ),
    );

    build_named_dict(module)
}
