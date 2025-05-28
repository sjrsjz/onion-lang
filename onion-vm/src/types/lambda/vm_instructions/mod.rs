use instruction_set::VMInstructionPackage;
use rustc_hash::FxHashMap as HashMap;

use arc_gc::gc::GC;
use opcode::{OpcodeArgument, ProcessedOpcode};

use crate::{
    lambda::{
        runnable::{RuntimeError, StepResult},
        scheduler::{async_scheduler::AsyncScheduler, map::Mapping, scheduler::Scheduler},
    },
    types::{
        lazy_set::OnionLazySet,
        named::OnionNamed,
        object::{ObjectError, OnionObject},
        pair::OnionPair,
        tuple::OnionTuple,
    },
};

use super::{
    context::Frame,
    definition::{LambdaBody, OnionLambdaDefinition},
    runnable::OnionLambdaRunnable as LambdaRunnable,
};

pub mod instruction_set;
pub mod ir;
pub mod ir_translator;
pub mod opcode;

pub fn load_int(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::Int64(value) = opcode.operand1 {
        runnable
            .context
            .push_object(OnionObject::Integer(value).stabilize())?;
        Ok(StepResult::Continue)
    } else {
        Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for LoadInt: {:?}",
            opcode.operand1
        )))
    }
}

pub fn load_null(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    runnable
        .context
        .push_object(OnionObject::Null.stabilize())?;
    Ok(StepResult::Continue)
}

pub fn load_undefined(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    runnable
        .context
        .push_object(OnionObject::Undefined("".to_string()).stabilize())?;
    Ok(StepResult::Continue)
}

pub fn load_float(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::Float64(value) = opcode.operand1 {
        runnable
            .context
            .push_object(OnionObject::Float(value).stabilize())?;
        Ok(StepResult::Continue)
    } else {
        Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for LoadFloat: {:?}",
            opcode.operand1
        )))
    }
}

pub fn load_string(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::String(value) = opcode.operand1 {
        let string = OnionObject::String(
            runnable.borrow_instruction()?.get_string_pool()[value as usize].clone(),
        )
        .stabilize();
        runnable.context.push_object(string)?;
        Ok(StepResult::Continue)
    } else {
        Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for LoadString: {:?}",
            opcode.operand1
        )))
    }
}

pub fn load_bytes(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::ByteArray(value) = opcode.operand1 {
        let bytes = OnionObject::Bytes(
            runnable.borrow_instruction()?.get_bytes_pool()[value as usize].clone(),
        )
        .stabilize();
        runnable.context.push_object(bytes)?;
        Ok(StepResult::Continue)
    } else {
        Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for LoadBytes: {:?}",
            opcode.operand1
        )))
    }
}

pub fn load_bool(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::Int32(value) = opcode.operand1 {
        runnable
            .context
            .push_object(OnionObject::Boolean(value != 0).stabilize())?;
        Ok(StepResult::Continue)
    } else {
        Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for LoadBoolean: {:?}",
            opcode.operand1
        )))
    }
}

pub fn discard_top(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    runnable.context.pop()?;
    Ok(StepResult::Continue)
}

pub fn build_tuple(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::Int64(size) = opcode.operand1 {
        let mut tuple = Vec::with_capacity(size as usize);
        for i in 1..=size as usize {
            tuple.push(runnable.context.get_object_rev(size as usize - i)?);
        }
        let tuple = OnionTuple::new_static(tuple);
        runnable.context.discard_objects(size as usize)?;
        runnable.context.push_object(tuple)?;
        Ok(StepResult::Continue)
    } else {
        Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for BuildTuple: {:?}",
            opcode.operand1
        )))
    }
}

pub fn build_keyval(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let key = runnable.context.get_object_rev(1)?;
    let value = runnable.context.get_object_rev(0)?;
    let keyval = OnionPair::new_static(key, value);
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(keyval)?;
    Ok(StepResult::Continue)
}

pub fn build_named(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let key = runnable.context.get_object_rev(1)?;
    let value = runnable.context.get_object_rev(0)?;
    let named = OnionNamed::new_static(key, value);
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(named)?;
    Ok(StepResult::Continue)
}

pub fn build_range(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let left = runnable.context.get_object_rev(1)?;
    let right = runnable.context.get_object_rev(0)?;
    let range = OnionObject::Range(
        left.weak()
            .to_integer()
            .map_err(|e| RuntimeError::DetailedError(format!("Failed to build range: {}", e)))?,
        right
            .weak()
            .to_integer()
            .map_err(|e| RuntimeError::DetailedError(format!("Failed to build range: {}", e)))?,
    )
    .stabilize();

    runnable.context.discard_objects(2)?;
    runnable.context.push_object(range)?;
    Ok(StepResult::Continue)
}

pub fn build_set(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let container = runnable.context.get_object_rev(1)?;
    let filter = runnable.context.get_object_rev(0)?;
    let set = OnionLazySet::new_static(container, filter);
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(set)?;
    Ok(StepResult::Continue)
}

pub fn load_lambda(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::String(signature_index) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for Lambda's signature index: {:?}",
            opcode.operand1
        )));
    };
    let OpcodeArgument::Int64(_code_position) = opcode.operand2 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for Lambda's code position: {:?}",
            opcode.operand2
        )));
    };
    let OpcodeArgument::Int32(flags) = opcode.operand3 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for Lambda's flags: {:?}",
            opcode.operand3
        )));
    };

    let signature = runnable
        .borrow_instruction()?
        .get_string_pool()
        .get(signature_index as usize)
        .ok_or_else(|| {
            RuntimeError::DetailedError(format!(
                "Signature index out of bounds: {}",
                signature_index
            ))
        })?
        .clone();

    let should_capture = flags & 0x01 != 0;

    let captured_value = if should_capture {
        Some(runnable.context.get_object_rev(1)?)
    } else {
        None
    };

    let default_parameters = runnable
        .context
        .get_object_rev(if should_capture { 2 } else { 1 })?
        .weak()
        .clone_value()
        .map_err(RuntimeError::ObjectError)?;

    let OnionObject::Tuple(_) = default_parameters.weak() else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid object type for default parameters: {}",
            default_parameters
        )));
    };

    let instruction_package = runnable
        .context
        .get_object_rev(0)?
        .weak()
        .clone_value()
        .map_err(RuntimeError::ObjectError)?;
    let OnionObject::InstructionPackage(_) = instruction_package.weak() else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid object type for instruction package: {}",
            instruction_package
        )));
    };

    let lambda = OnionLambdaDefinition::new_static(
        &default_parameters,
        LambdaBody::Instruction(Box::new(instruction_package.weak().clone())),
        captured_value,
        None,
        signature,
    );

    runnable
        .context
        .discard_objects(if should_capture { 3 } else { 2 })?;
    runnable.context.push_object(lambda)?;
    Ok(StepResult::Continue)
}

pub fn let_var(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::String(index) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for LetVariable: {:?}",
            opcode.operand1
        )));
    };

    let value = runnable.context.get_object_rev(0)?;
    let name = runnable
        .borrow_instruction()?
        .get_string_pool()
        .get(index as usize)
        .ok_or_else(|| RuntimeError::DetailedError(format!("Name index out of bounds: {}", index)))?
        .clone();

    runnable
        .context
        .let_variable(&name, value.clone())
        .map_err(RuntimeError::ObjectError)?;
    Ok(StepResult::Continue)
}

pub fn get_var(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::String(index) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for GetVariable: {:?}",
            opcode.operand1
        )));
    };

    let name = runnable
        .borrow_instruction()?
        .get_string_pool()
        .get(index as usize)
        .cloned();
    let Some(name) = name else {
        return Err(RuntimeError::DetailedError(format!(
            "Name index out of bounds: {}",
            index
        )));
    };

    let value = runnable.context.get_variable(&name)?;
    runnable.context.push_object(value.clone())?;
    Ok(StepResult::Continue)
}

pub fn set_var(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?.clone();
    let left = runnable.context.get_object_rev_mut(1)?;
    left.weak_mut()
        .assign(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    // 保留右侧对象的引用
    runnable.context.discard_objects_offset(1, 1)?;
    Ok(StepResult::Continue)
}

pub fn get_attr(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let attr = runnable.context.get_object_rev(0)?.weak();
    let obj = runnable.context.get_object_rev(1)?;
    let element = attr
        .with_data(|attr| {
            Ok(
                match obj.weak().with_attribute(attr, &|attr| Ok(attr.clone())) {
                    Ok(value) => match value {
                        OnionObject::Lambda(mut lambda) => {
                            lambda.self_object = Box::new(obj.weak().clone());
                            OnionObject::Lambda(lambda)
                        }
                        _ => value,
                    },
                    Err(e) => {
                        return Err(ObjectError::InvalidOperation(format!(
                            "Failed to get attribute {:?} from object {:?}: {}",
                            attr, obj, e
                        )));
                    }
                },
            )
        })
        .map_err(RuntimeError::ObjectError)?
        .stabilize();

    runnable.context.discard_objects(2)?;
    runnable.context.push_object(element)?;
    Ok(StepResult::Continue)
}

pub fn index_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let index = runnable.context.get_object_rev(0)?;
    let OnionObject::Integer(index) = index.weak() else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid index type for IndexOf: {}",
            index
        )));
    };
    let obj = runnable.context.get_object_rev(1)?.weak();
    let element = obj.at(*index).map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(element)?;
    Ok(StepResult::Continue)
}

pub fn key_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let key = obj.weak().key_of().map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(key)?;
    Ok(StepResult::Continue)
}

pub fn value_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let value = obj.weak().value_of().map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(value)?;
    Ok(StepResult::Continue)
}

pub fn type_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let type_name = obj.weak().type_of().map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable
        .context
        .push_object(OnionObject::String(type_name).stabilize())?;
    Ok(StepResult::Continue)
}

pub fn copy(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let copied_obj = obj.weak().copy().map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(copied_obj)?;
    Ok(StepResult::Continue)
}

pub fn check_is_same_object(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj1 = runnable.context.get_object_rev(0)?;
    let obj2 = runnable.context.get_object_rev(1)?;
    let is_same = obj1
        .weak()
        .is_same(obj2.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable
        .context
        .push_object(OnionObject::Boolean(is_same).stabilize())?;
    Ok(StepResult::Continue)
}

pub fn binary_add(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_add(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_subtract(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_sub(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_multiply(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_mul(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_divide(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_div(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_modulus(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_mod(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_power(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_pow(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_bitwise_or(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_or(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_bitwise_and(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_and(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_bitwise_xor(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_xor(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_shift_left(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_shl(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_shift_right(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_shr(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn binary_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_eq(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable
        .context
        .push_object(OnionObject::Boolean(result).stabilize())?;
    Ok(StepResult::Continue)
}
pub fn binary_not_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_eq(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable
        .context
        .push_object(OnionObject::Boolean(!result).stabilize())?;
    Ok(StepResult::Continue)
}
pub fn binary_greater(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_gt(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable
        .context
        .push_object(OnionObject::Boolean(result).stabilize())?;
    Ok(StepResult::Continue)
}
pub fn binary_greater_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_lt(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable
        .context
        .push_object(OnionObject::Boolean(!result).stabilize())?;
    Ok(StepResult::Continue)
}
pub fn binary_less(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_lt(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable
        .context
        .push_object(OnionObject::Boolean(result).stabilize())?;
    Ok(StepResult::Continue)
}
pub fn binary_less_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .weak()
        .binary_gt(right.weak())
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable
        .context
        .push_object(OnionObject::Boolean(!result).stabilize())?;
    Ok(StepResult::Continue)
}

pub fn unary_bitwise_not(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let result = obj.weak().unary_not().map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn unary_plus(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let result = obj.weak().unary_plus().map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn unary_minus(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let result = obj.weak().unary_neg().map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(result)?;
    Ok(StepResult::Continue)
}
pub fn swap(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::Int64(index) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for Swap: {:?}",
            opcode.operand1
        )));
    };
    runnable.context.swap(0, index as usize)?;
    Ok(StepResult::Continue)
}
pub fn jump(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::Int64(offset) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for Jump: {:?}",
            opcode.operand1
        )));
    };
    runnable.ip += offset as isize;
    Ok(StepResult::Continue)
}

pub fn jump_if_false(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::Int64(offset) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for JumpIfNot: {:?}",
            opcode.operand1
        )));
    };
    let condition = runnable.context.get_object_rev(0)?;
    if !condition
        .weak()
        .to_boolean()
        .map_err(RuntimeError::ObjectError)?
    {
        runnable.ip += offset as isize;
    }
    runnable.context.discard_objects(1)?;
    Ok(StepResult::Continue)
}
pub fn get_length(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let length = obj.weak().len().map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(length)?;
    Ok(StepResult::Continue)
}

pub fn new_frame(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    runnable
        .context
        .push_frame(Frame::Normal(HashMap::default(), vec![]));
    Ok(StepResult::Continue)
}

pub fn pop_frame(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    runnable.context.concat_last_frame()?;
    Ok(StepResult::Continue)
}

pub fn clear_stack(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    runnable.context.clear_stack();
    Ok(StepResult::Continue)
}

pub fn assert(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let condition = runnable.context.get_object_rev(0)?;
    if !condition
        .weak()
        .to_boolean()
        .map_err(RuntimeError::ObjectError)?
    {
        return Err(RuntimeError::DetailedError(format!(
            "Assertion failed: {}",
            condition
        )));
    }
    runnable.context.discard_objects(1)?;
    Ok(StepResult::Continue)
}

pub fn is_in(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let element = runnable.context.get_object_rev(1)?;
    let container = runnable.context.get_object_rev(0)?;

    let OnionObject::LazySet(lazy_set) = container.weak() else {
        return Err(RuntimeError::DetailedError(format!(
            "Container is not a LazySet: {}",
            container
        )));
    };

    // 先检查元素是否在惰性集合的容器中
    let is_in = lazy_set
        .get_container()
        .contains(element.weak())
        .map_err(|e| RuntimeError::DetailedError(format!("Failed to check containment: {}", e)))?;

    if !is_in {
        // 元素不在容器中，直接返回 false
        runnable.context.discard_objects(2)?;
        runnable
            .context
            .push_object(OnionObject::Boolean(false).stabilize())?;
        return Ok(StepResult::Continue);
    }

    // 如果在容器中，则调用惰性集合的过滤器
    let (args, filter) = lazy_set
        .get_filter()
        .with_data(|filter| {
            let OnionObject::Lambda(_) = filter else {
                // 过滤器不是 Lambda 对象，认定为惰性求值结果为 filter
                let filter = filter.clone().stabilize();
                return Ok((None, filter));
            };
            let args = OnionTuple::new_static(vec![element]);
            let filter = filter.clone().stabilize();
            return Ok((Some(args), filter));
        })
        .map_err(RuntimeError::ObjectError)?;

    runnable.context.discard_objects(2)?;

    if let Some(args) = args {
        runnable.context.push_object(filter)?;
        runnable.context.push_object(args)?;
        // 运行过滤器
        return call_lambda(runnable, opcode, gc);
    };

    runnable.context.push_object(filter)?;
    return Ok(StepResult::Continue);
}

pub fn call_lambda(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let lambda = runnable.context.get_object_rev(1)?;
    let args = runnable.context.get_object_rev(0)?;
    let new_runnable = lambda
        .weak()
        .with_data(|lambda| {
            if let OnionObject::Lambda(lambda) = lambda {
                let assigned = lambda.parameter.as_ref().with_data(|parameter| {
                    let OnionObject::Tuple(parameters) = parameter else {
                        return Err(ObjectError::InvalidType(format!(
                            "Lambda parameters are not a Tuple: {:?}",
                            lambda
                        )));
                    };

                    let OnionObject::Tuple(args_tuple) = args.weak() else {
                        return Err(ObjectError::InvalidType(format!(
                            "Arguments are not a Tuple: {}",
                            args
                        )));
                    };
                    parameters.clone_and_named_assignment(args_tuple)
                })?;
                lambda.create_runnable(assigned, gc).map_err(|e| {
                    ObjectError::InvalidType(format!(
                        "Failed to create runnable from lambda: {}",
                        e
                    ))
                })
            } else {
                Err(ObjectError::InvalidType(format!(
                    "Object is not a Lambda: {:?}",
                    lambda
                )))
            }
        })
        .map_err(RuntimeError::ObjectError)?;

    runnable.context.discard_objects(2)?;
    Ok(StepResult::NewRunnable(new_runnable))
}

pub fn mutablize(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let heap = runnable
        .context
        .get_object_rev(0)?
        .mutablize(gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(heap)?;
    Ok(StepResult::Continue)
}

pub fn immutablize(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let immutable_obj = obj
        .weak()
        .clone_value()
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(immutable_obj)?;
    Ok(StepResult::Continue)
}

pub fn return_value(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let return_value = runnable.context.get_object_rev(0)?;
    Ok(StepResult::Return(return_value.clone()))
}

pub fn fork_instruction(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let forked =
        OnionObject::InstructionPackage(runnable.borrow_instruction()?.clone()).stabilize();
    runnable.context.push_object(forked)?;
    Ok(StepResult::Continue)
}

pub fn import(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let path = runnable.context.get_object_rev(0)?;
    let OnionObject::String(path) = path.weak() else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid path type for `import`: {}",
            path
        )));
    };
    let package = VMInstructionPackage::read_from_file(path).map_err(|e| {
        RuntimeError::DetailedError(format!(
            "Failed to read instruction package from file: {}",
            e
        ))
    })?;
    runnable.context.discard_objects(1)?;
    runnable
        .context
        .push_object(OnionObject::InstructionPackage(package).stabilize())?;
    Ok(StepResult::Continue)
}

pub fn run_lambda(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let lambda = runnable.context.get_object_rev(0)?;
    let OnionObject::Lambda(lambda) = lambda.weak() else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid object type for RunLambda: {}",
            lambda
        )));
    };

    let scheduler = Scheduler::new(vec![lambda
        .create_runnable(lambda.parameter.as_ref().clone().stabilize(), gc)
        .map_err(RuntimeError::ObjectError)?]);
    runnable.context.discard_objects(1)?;
    return Ok(StepResult::NewRunnable(Box::new(scheduler)));
}

pub fn map_to(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let container = runnable.context.get_object_rev(1)?;
    let map = runnable.context.get_object_rev(0)?;
    let mapping = Mapping {
        container: container.clone(),
        map: map.clone(),
        collected: vec![],
        current_index: 0,
    };
    runnable.context.discard_objects(2)?;
    return Ok(StepResult::NewRunnable(Box::new(mapping)));
}

pub fn async_call(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObject>,
) -> Result<StepResult, RuntimeError> {
    let lambda = runnable.context.get_object_rev(1)?;
    let args = runnable.context.get_object_rev(0)?;
    let new_runnable = lambda
        .weak()
        .with_data(|lambda| {
            if let OnionObject::Lambda(lambda) = lambda {
                let assigned = lambda.parameter.as_ref().with_data(|parameter| {
                    let OnionObject::Tuple(parameters) = parameter else {
                        return Err(ObjectError::InvalidType(format!(
                            "Lambda parameters are not a Tuple: {:?}",
                            lambda
                        )));
                    };

                    let OnionObject::Tuple(args_tuple) = args.weak() else {
                        return Err(ObjectError::InvalidType(format!(
                            "Arguments are not a Tuple: {}",
                            args
                        )));
                    };
                    parameters.clone_and_named_assignment(args_tuple)
                })?;
                lambda.create_runnable(assigned, gc).map_err(|e| {
                    ObjectError::InvalidType(format!(
                        "Failed to create runnable from lambda: {}",
                        e
                    ))
                })
            } else {
                Err(ObjectError::InvalidType(format!(
                    "Object is not a Lambda: {:?}",
                    lambda
                )))
            }
        })
        .map_err(RuntimeError::ObjectError)?;

    runnable.context.discard_objects(2)?;
    let async_scheduler = AsyncScheduler::new(vec![new_runnable]);
    Ok(StepResult::NewRunnable(Box::new(async_scheduler)))
}
