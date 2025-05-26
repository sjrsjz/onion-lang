use std::collections::HashMap;

use arc_gc::gc::GC;
use opcode::{OpcodeArgument, ProcessedOpcode};

use crate::{lambda::runnable::{RuntimeError, StepResult}, types::{lazy_set::OnionLazySet, named::OnionNamed, object::OnionObject, pair::OnionPair, tuple::OnionTuple}};

use super::{context::StackObject, runnable::OnionLambdaRunnable as LambdaRunnable};



pub mod instruction_set;
pub mod ir;
pub mod ir_translator;
pub mod opcode;

pub fn load_int(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::Int64(value) = opcode.operand1 {
        runnable.context.push_object(OnionObject::Integer(value).stabilize())?;
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
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    runnable.context.push_object(OnionObject::Null.stabilize())?;
    Ok(StepResult::Continue)
}

pub fn load_undefined(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    runnable
        .context
        .push_object(OnionObject::Undefined("".to_string()).stabilize())?;
    Ok(StepResult::Continue)
}

pub fn load_float(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::Float64(value) = opcode.operand1 {
        runnable.context.push_object(OnionObject::Float(value).stabilize())?;
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
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::String(value) = opcode.operand1 {
        let string = OnionObject::String(
            runnable.borrow_instruction()?.get_string_pool()[value as usize].clone(),
        ).stabilize();
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
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::ByteArray(value) = opcode.operand1 {
        let bytes = OnionObject::Bytes(
            runnable.borrow_instruction()?.get_bytes_pool()[value as usize].clone(),
        ).stabilize();
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
    gc: &mut GC,
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
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    runnable.context.pop()?;
    Ok(StepResult::Continue)
}

pub fn build_tuple(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
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
    opcode: &ProcessedOpcode,
    gc: &mut GC,
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
    opcode: &ProcessedOpcode,
    gc: &mut GC,
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
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let left = runnable.context.get_object_rev(1)?;
    let right = runnable.context.get_object_rev(0)?;
    let range = OnionObject::Range(
        left.weak().to_integer()
            .map_err(|e| RuntimeError::DetailedError(format!("Failed to build range: {}", e)))?,
        right
            .weak().to_integer()
            .map_err(|e| RuntimeError::DetailedError(format!("Failed to build range: {}", e)))?,
    ).stabilize();

    runnable.context.discard_objects(2)?;
    runnable.context.push_object(range)?;
    Ok(StepResult::Continue)
}

pub fn build_set(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let container = runnable.context.get_object_rev(1)?;
    let filter = runnable.context.get_object_rev(0)?;
    let set =  OnionLazySet::new_static(
        container,
        filter,
    );  
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(set)?;
    Ok(StepResult::Continue)
}

pub fn load_lambda(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::Int64(signature_index) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for Lambda's signature index: {:?}",
            opcode.operand1
        )));
    };
    let OpcodeArgument::Int64(code_position) = opcode.operand2 else {
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
        })?.clone();

    let should_capture = flags & 0x01 != 0;
    let dynamic_params = flags & 0x02 != 0;

    let captured_value = if should_capture {
        Some(
            runnable
                .context
                .get_object_rev(1)?
        )
    } else {
        None
    };

    let default_parameters = runnable
        .context
        .get_object_rev(if should_capture { 2 } else { 1 })?
        .weak().clone_value().map_err(RuntimeError::ObjectError)?;

    let OnionObject::Tuple(_) = default_parameters.weak() else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid object type for default parameters: {}",
            default_parameters
        )));
    };

    let instruction_package = runnable
        .context
        .get_object_rev(0)?
        .weak().clone_value().map_err(RuntimeError::ObjectError)?;
    let OnionObject::InstructionPackage(_) = instruction_package.weak() else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid object type for instruction package: {}",
            instruction_package
        )));
    };

    Err(RuntimeError::DetailedError(format!(
        "LoadLambda is not implemented yet. Signature: {}, Code Position: {}, Flags: {}",
        signature, code_position, flags
    )))
}

pub fn let_var(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::Int64(index) = opcode.operand1 else {
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
        .ok_or_else(|| {
            RuntimeError::DetailedError(format!("Name index out of bounds: {}", index))
        })?.clone();

    runnable.context.let_variable(&name, value.clone())?;
    Ok(StepResult::Continue)
}

pub fn get_var(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::Int64(index) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for GetVariable: {:?}",
            opcode.operand1
        )));
    };

    let name = runnable
        .borrow_instruction()?
        .get_string_pool()
        .get(index as usize)
        .ok_or_else(|| {
            RuntimeError::DetailedError(format!("Name index out of bounds: {}", index))
        })?.clone();

    let value = runnable.context.get_variable(&name)?;
    runnable.context.push_object(value.clone())?;
    Ok(StepResult::Continue)
}

pub fn set_var(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?.clone();
    let left = runnable.context.get_object_rev_mut(1)?;
    left.weak_mut().assign(right.weak()).map_err(RuntimeError::ObjectError)?;
    // 保留右侧对象的引用
    runnable.context.discard_objects_offset(1, 1)?;
    Ok(StepResult::Continue)
}

pub fn get_attr(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let attr = runnable.context.get_object_rev(0)?;
    let obj = runnable.context.get_object_rev(1)?;
    let element = match obj.get_attr(attr) {
        Some(element) => element,
        None => Object::Undefined(format!("Attribute {} not found in object {}", attr, obj)),
    };
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(element, None)?;
    Ok(None)
}

pub fn index_of(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let index = runnable.context.get_object_rev(0)?;
    let obj = runnable.context.get_object_rev(1)?;
    let element = obj.at(index.as_int().map_err(RuntimeError::ObjectError)? as usize);
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(element, None)?;
    Ok(None)
}

pub fn key_of(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let key = obj.key_of();
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(key, None)?;
    Ok(None)
}

pub fn value_of(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let value = obj.value_of();
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(value, None)?;
    Ok(None)
}

pub fn type_of(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let type_name = obj.type_of();
    runnable.context.discard_objects(1)?;
    runnable
        .context
        .push_object(Object::String(type_name), None)?;
    Ok(None)
}

pub fn copy(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let copied_obj = obj.copy();
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(copied_obj, None)?;
    Ok(None)
}

pub fn check_is_same_object(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let obj1 = runnable.context.get_object_rev(0)?;
    let obj2 = runnable.context.get_object_rev(1)?;
    let is_same = obj1.is_same_object(obj2);
    runnable.context.discard_objects(2)?;
    runnable
        .context
        .push_object(Object::Boolean(is_same), None)?;
    Ok(None)
}

pub fn binary_add(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left.add(right, gc).map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_subtract(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .subtract(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_multiply(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .multiply(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_divide(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left.divide(right, gc).map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_modulus(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left.modulus(right, gc).map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_power(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left.power(right, gc).map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_bitwise_or(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .bitwise_or(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_bitwise_and(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .bitwise_and(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_bitwise_xor(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .bitwise_xor(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_shift_left(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .shift_left(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_shift_right(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .shift_right(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_equal(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left.equal(right, gc).map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_not_equal(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .not_equal(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_greater(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .greater_than(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_greater_equal(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .greater_equal(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_less(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .less_than(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn binary_less_equal(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    let result = left
        .less_equal(right, gc)
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}

pub fn unary_bitwise_not(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let result = obj.bitwise_not(gc).map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn unary_plus(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let result = obj.plus(gc).map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn unary_minus(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let result = obj.minus(gc).map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(result.0, result.1)?;
    Ok(None)
}
pub fn swap(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::Int64(index) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for Swap: {:?}",
            opcode.operand1
        )));
    };
    runnable.context.swap(0, index as usize)?;
    Ok(None)
}
pub fn jump(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::Int64(offset) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for Jump: {:?}",
            opcode.operand1
        )));
    };
    runnable.ip += offset as isize;
    Ok(None)
}

pub fn jump_if_false(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::Int64(offset) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for JumpIfNot: {:?}",
            opcode.operand1
        )));
    };
    let condition = runnable.context.get_object_rev(0)?;
    if !condition.as_bool().map_err(RuntimeError::ObjectError)? {
        runnable.ip += offset as isize;
    }
    runnable.context.discard_objects(1)?;
    Ok(None)
}
pub fn get_length(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let length = obj.len().map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable
        .context
        .push_object(Object::Integer(length as i64), None)?;
    Ok(None)
}

pub fn new_frame(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    runnable
        .context
        .push_frame(Frame::Normal(HashMap::new(), vec![]));
    Ok(None)
}

pub fn pop_frame(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    runnable.context.pop_frame();
    Ok(None)
}

pub fn clear_stack(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    runnable.context.clear_stack();
    Ok(None)
}

pub fn assert(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let condition = runnable.context.get_object_rev(0)?;
    if !condition.as_bool().map_err(RuntimeError::ObjectError)? {
        return Err(RuntimeError::DetailedError(format!(
            "Assertion failed: {}",
            condition
        )));
    }
    runnable.context.discard_objects(1)?;
    Ok(None)
}

pub fn is_in(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    let element = runnable.context.get_object_rev(0)?;
    let container = runnable.context.get_object_rev(1)?;

    let Object::LazySet(lazy_set) = container else {
        return Err(RuntimeError::DetailedError(format!(
            "Container is not a LazySet: {}",
            container
        )));
    };

    // 先检查元素是否在惰性集合的容器中
    let is_in = lazy_set
        .get_container()
        .contains(&element)
        .map_err(|e| RuntimeError::DetailedError(format!("Failed to check containment: {}", e)))?;

    // 如果在容器中，则调用惰性集合的过滤器
    let filter = lazy_set
        .get_filter()
        .value()
        .map_err(|e| RuntimeError::DetailedError(format!("Failed to get filter: {}", e)))?;
    let Object::Lambda(filter) = filter else {
        // 过滤器不是 Lambda 对象，认定为惰性求值结果为 filter
        runnable.context.discard_objects_offset(1, 1)?;
        return Ok(None);
    };
    let (args, arcs) = Tuple::from_vec(
        &vec![element.value().map_err(RuntimeError::ObjectError)?],
        gc,
    )
    .map_err(|e| RuntimeError::DetailedError(format!("Failed to create tuple: {}", e)))?;
    runnable.context.discard_objects(2)?;
    let filter_arcs = filter.upgrade_values();
    runnable
        .context
        .push_object(Object::Lambda(filter), Some(filter_arcs))?;
    runnable
        .context
        .push_object(Object::Tuple(args), Some(arcs))?;

    // 运行过滤器
    return call_lambda(runnable, opcode, gc);
}

pub fn call_lambda(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC,
) -> Result<StepResult, RuntimeError> {
    Ok(None)
}
