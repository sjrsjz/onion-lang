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
        object::{ObjectError, OnionObject, OnionObjectCell},
        pair::OnionPair,
        tuple::OnionTuple,
    },
};

use super::{
    context::{Context, Frame},
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
    _gc: &mut GC<OnionObjectCell>,
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
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    runnable
        .context
        .push_object(OnionObject::Null.stabilize())?;
    Ok(StepResult::Continue)
}

pub fn load_undefined(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    runnable
        .context
        .push_object(OnionObject::Undefined(None).stabilize())?;
    Ok(StepResult::Continue)
}

pub fn load_float(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
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
    _gc: &mut GC<OnionObjectCell>,
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
    _gc: &mut GC<OnionObjectCell>,
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
    _gc: &mut GC<OnionObjectCell>,
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
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：直接获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    Context::discard_from_stack(stack, 1)?;
    Ok(StepResult::Continue)
}

pub fn build_tuple(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    if let OpcodeArgument::Int64(size) = opcode.operand1 {
        // 快速路径：一次获取栈的可变引用，避免重复查找
        let stack = runnable.context.get_current_stack_mut()?;
        let mut tuple = Vec::with_capacity(size as usize);
        for i in 1..=size as usize {
            tuple.push(Context::get_object_from_stack(stack, size as usize - i)?);
        }
        let tuple = OnionTuple::new_static(tuple);
        Context::discard_from_stack(stack, size as usize)?;
        Context::push_to_stack(stack, tuple);
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
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let key = Context::get_object_from_stack(stack, 1)?;
    let value = Context::get_object_from_stack(stack, 0)?;
    let keyval = OnionPair::new_static(key, value);
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, keyval);
    Ok(StepResult::Continue)
}

pub fn build_named(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let key = Context::get_object_from_stack(stack, 1)?;
    let value = Context::get_object_from_stack(stack, 0)?;
    let named = OnionNamed::new_static(key, value);
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, named);
    Ok(StepResult::Continue)
}

pub fn build_range(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let range = OnionObject::Range(
        left.weak()
            .try_borrow()
            .map_err(RuntimeError::ObjectError)?
            .to_integer()
            .map_err(|e| RuntimeError::DetailedError(format!("Failed to build range: {}", e)))?,
        right
            .weak()
            .try_borrow()
            .map_err(RuntimeError::ObjectError)?
            .to_integer()
            .map_err(|e| RuntimeError::DetailedError(format!("Failed to build range: {}", e)))?,
    )
    .stabilize();

    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, range);
    Ok(StepResult::Continue)
}

pub fn build_set(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let container = Context::get_object_from_stack(stack, 1)?;
    let filter = Context::get_object_from_stack(stack, 0)?;
    let set = OnionLazySet::new_static(container, filter);
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, set);
    Ok(StepResult::Continue)
}

pub fn load_lambda(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
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
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .clone_value()
        .map_err(RuntimeError::ObjectError)?;

    let OnionObject::Tuple(_) = &*default_parameters
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
    else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid object type for default parameters: {}",
            default_parameters
        )));
    };

    let instruction_package = runnable
        .context
        .get_object_rev(0)?
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .clone_value()
        .map_err(RuntimeError::ObjectError)?;
    let OnionObject::InstructionPackage(_) = &*instruction_package
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
    else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid object type for instruction package: {}",
            instruction_package
        )));
    };

    let lambda = OnionLambdaDefinition::new_static(
        &default_parameters,
        LambdaBody::Instruction(Box::new(
            instruction_package
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?
                .clone(),
        )),
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
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::String(index) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for LetVariable: {:?}",
            opcode.operand1
        )));
    };

    let value = runnable.context.get_object_rev(0)?;

    // let name = {
    //     let instruction = runnable.borrow_instruction()?;
    //     instruction
    //         .get_string_pool()
    //         .get(index as usize)
    //         .ok_or_else(|| {
    //             RuntimeError::DetailedError(format!("Name index out of bounds: {}", index))
    //         })?
    //         .clone()
    // };

    runnable
        .context
        .let_variable(index as usize, value.clone())
        .map_err(RuntimeError::ObjectError)?;
    Ok(StepResult::Continue)
}

pub fn get_var(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let OpcodeArgument::String(index) = opcode.operand1 else {
        return Err(RuntimeError::DetailedError(format!(
            "Invalid argument type for GetVariable: {:?}",
            opcode.operand1
        )));
    };

    // let value = {
    //     let instruction = runnable.borrow_instruction()?;
    //     let name = instruction
    //         .get_string_pool()
    //         .get(index as usize)
    //         .ok_or_else(|| {
    //             RuntimeError::DetailedError(format!("Name index out of bounds: {}", index))
    //         })?;
    //     runnable.context.get_variable(name)?
    // };

    let value = runnable.context.get_variable(index as usize)?;

    runnable.context.push_object(value.clone())?;
    Ok(StepResult::Continue)
}

pub fn set_var(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let right = runnable.context.get_object_rev(0)?;
    let left = runnable.context.get_object_rev(1)?;
    left.weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .assign(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    // 保留左侧对象的引用，由于其是不可变的可变对象的引用，assign不会导致GC错误回收
    runnable.context.discard_objects(1)?;
    Ok(StepResult::Continue)
}

pub fn get_attr(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
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
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // Get both objects first
    let index_obj = runnable.context.get_object_rev(0)?;
    let container_obj = runnable.context.get_object_rev(1)?;

    // Extract the integer index value
    let index_value = {
        let index_ref = index_obj
            .weak()
            .try_borrow()
            .map_err(RuntimeError::ObjectError)?;
        if let OnionObject::Integer(index) = &*index_ref {
            *index
        } else {
            return Err(RuntimeError::DetailedError(format!(
                "Invalid index type for IndexOf: {}",
                index_obj
            )));
        }
    };

    // Get the element at the index
    let element = {
        let obj_ref = container_obj
            .weak()
            .try_borrow()
            .map_err(RuntimeError::ObjectError)?;
        obj_ref.at(index_value).map_err(RuntimeError::ObjectError)?
    };

    // Now modify the context after all borrows are dropped
    runnable.context.discard_objects(2)?;
    runnable.context.push_object(element)?;
    Ok(StepResult::Continue)
}

pub fn key_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let stack = runnable.context.get_current_stack_mut()?;
    let obj = Context::get_object_from_stack(stack, 0)?;
    let key = obj
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .key_of()
        .map_err(RuntimeError::ObjectError)?;
    Context::replace_last_object(stack, key);
    Ok(StepResult::Continue)
}

pub fn value_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let stack = runnable.context.get_current_stack_mut()?;
    let obj = Context::get_object_from_stack(stack, 0)?;
    let value = obj
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .value_of()
        .map_err(RuntimeError::ObjectError)?;
    Context::replace_last_object(stack, value);
    Ok(StepResult::Continue)
}

pub fn type_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let obj = Context::get_object_from_stack(stack, 0)?;
    let type_name = obj
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .type_of()
        .map_err(RuntimeError::ObjectError)?;
    Context::replace_last_object(stack, OnionObject::String(type_name).stabilize());
    Ok(StepResult::Continue)
}

pub fn copy(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let obj = Context::get_object_from_stack(stack, 0)?;
    let copied_obj = obj
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .copy()
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 1)?;
    Context::push_to_stack(stack, copied_obj);
    Ok(StepResult::Continue)
}

pub fn check_is_same_object(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let obj1 = runnable.context.get_object_rev(0)?;
    let obj2 = runnable.context.get_object_rev(1)?;
    let is_same = obj1
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .is_same(
            &*obj2
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
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
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_add(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_subtract(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_sub(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_multiply(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_mul(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_divide(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_div(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_modulus(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_mod(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_power(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_pow(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_bitwise_or(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_or(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_bitwise_and(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_and(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_bitwise_xor(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_xor(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_shift_left(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_shl(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_shift_right(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_shr(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}

pub fn binary_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_eq(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, OnionObject::Boolean(result).stabilize());
    Ok(StepResult::Continue)
}

pub fn binary_not_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_eq(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, OnionObject::Boolean(!result).stabilize());
    Ok(StepResult::Continue)
}

pub fn binary_greater(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_gt(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, OnionObject::Boolean(result).stabilize());
    Ok(StepResult::Continue)
}

pub fn binary_greater_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_lt(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, OnionObject::Boolean(!result).stabilize());
    Ok(StepResult::Continue)
}

pub fn binary_less(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_lt(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, OnionObject::Boolean(result).stabilize());
    Ok(StepResult::Continue)
}

pub fn binary_less_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let right = Context::get_object_from_stack(stack, 0)?;
    let left = Context::get_object_from_stack(stack, 1)?;
    let result = left
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .binary_gt(
            &*right
                .weak()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?,
        )
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 2)?;
    Context::push_to_stack(stack, OnionObject::Boolean(!result).stabilize());
    Ok(StepResult::Continue)
}

pub fn unary_bitwise_not(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let obj = Context::get_object_from_stack(stack, 0)?;
    let result = obj
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .unary_not()
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 1)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}
pub fn unary_plus(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let obj = Context::get_object_from_stack(stack, 0)?;
    let result = obj
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .unary_plus()
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 1)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}
pub fn unary_minus(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = runnable.context.get_current_stack_mut()?;
    let obj = Context::get_object_from_stack(stack, 0)?;
    let result = obj
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .unary_neg()
        .map_err(RuntimeError::ObjectError)?;
    Context::discard_from_stack(stack, 1)?;
    Context::push_to_stack(stack, result);
    Ok(StepResult::Continue)
}
pub fn swap(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
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
    _gc: &mut GC<OnionObjectCell>,
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
    _gc: &mut GC<OnionObjectCell>,
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
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
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
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let length = obj
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .len()
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(length)?;
    Ok(StepResult::Continue)
}

pub fn new_frame(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    runnable.context.push_frame(Frame {
        variables: HashMap::default(),
        stack: vec![],
    });
    Ok(StepResult::Continue)
}

pub fn pop_frame(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    runnable.context.concat_last_frame()?;
    Ok(StepResult::Continue)
}

pub fn clear_stack(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    runnable.context.clear_stack();
    Ok(StepResult::Continue)
}

pub fn assert(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let condition = runnable.context.get_object_rev(0)?;
    if !condition
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
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
    gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let element = runnable.context.get_object_rev(1)?;
    let container = runnable.context.get_object_rev(0)?;

    // 先检查元素是否在惰性集合的容器中
    let is_in = {
        match &*container
            .weak()
            .try_borrow()
            .map_err(RuntimeError::ObjectError)?
        {
            OnionObject::LazySet(lazy_set) => lazy_set
                .get_container()
                .try_borrow()
                .map_err(RuntimeError::ObjectError)?
                .contains(
                    &*element
                        .weak()
                        .try_borrow()
                        .map_err(RuntimeError::ObjectError)?,
                )
                .map_err(|e| {
                    RuntimeError::DetailedError(format!("Failed to check containment: {}", e))
                })?,
            _ => {
                return Err(RuntimeError::DetailedError(format!(
                    "Container is not a LazySet: {}",
                    container
                )));
            }
        }
    };

    if !is_in {
        // 元素不在容器中，直接返回 false
        runnable.context.discard_objects(2)?;
        runnable
            .context
            .push_object(OnionObject::Boolean(false).stabilize())?;
        return Ok(StepResult::Continue);
    }

    // 如果在容器中，则调用惰性集合的过滤器
    let (args, filter) = match &*container
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
    {
        OnionObject::LazySet(lazy_set) => lazy_set
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
            .map_err(RuntimeError::ObjectError)?,
        _ => {
            return Err(RuntimeError::DetailedError(format!(
                "Container is not a LazySet: {}",
                container
            )));
        }
    };

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
    gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let lambda = runnable.context.get_object_rev(1)?;
    let args = runnable.context.get_object_rev(0)?;
    let new_runnable = lambda
        .weak()
        .with_data(|lambda_ref| {
            if let OnionObject::Lambda(lambda_ref) = lambda_ref {
                let assigned = lambda_ref.parameter.as_ref().with_data(|parameter| {
                    let OnionObject::Tuple(parameters) = parameter else {
                        return Err(ObjectError::InvalidType(format!(
                            "Lambda parameters are not a Tuple: {:?}",
                            lambda_ref
                        )));
                    };

                    let OnionObject::Tuple(args_tuple) = &*args.weak().try_borrow()? else {
                        return Err(ObjectError::InvalidType(format!(
                            "Arguments are not a Tuple: {}",
                            args
                        )));
                    };
                    parameters.clone_and_named_assignment(args_tuple)
                })?;
                lambda_ref
                    .create_runnable(assigned, lambda, gc)
                    .map_err(|e| {
                        ObjectError::InvalidType(format!(
                            "Failed to create runnable from lambda: {}",
                            e
                        ))
                    })
            } else {
                Err(ObjectError::InvalidType(format!(
                    "Object is not a Lambda: {:?}",
                    lambda_ref
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
    gc: &mut GC<OnionObjectCell>,
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
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let obj = runnable.context.get_object_rev(0)?;
    let immutable_obj = obj
        .weak()
        .try_borrow()
        .map_err(RuntimeError::ObjectError)?
        .clone_value()
        .map_err(RuntimeError::ObjectError)?;
    runnable.context.discard_objects(1)?;
    runnable.context.push_object(immutable_obj)?;
    Ok(StepResult::Continue)
}

pub fn return_value(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let return_value = runnable.context.get_object_rev(0)?;
    Ok(StepResult::Return(return_value.clone()))
}

pub fn fork_instruction(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let forked =
        OnionObject::InstructionPackage(runnable.borrow_instruction()?.clone()).stabilize();
    runnable.context.push_object(forked)?;
    Ok(StepResult::Continue)
}

pub fn import(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let path = runnable.context.get_object_rev(0)?;

    let package = {
        let OnionObject::String(path) = &*path
            .weak()
            .try_borrow()
            .map_err(RuntimeError::ObjectError)?
        else {
            return Err(RuntimeError::DetailedError(format!(
                "Invalid path type for `import`: {}",
                path
            )));
        };
        VMInstructionPackage::read_from_file(path).map_err(|e| {
            RuntimeError::DetailedError(format!(
                "Failed to read instruction package from file: {}",
                e
            ))
        })?
    };
    match VMInstructionPackage::validate(&package) {
        Err(e) => {
            return Err(RuntimeError::DetailedError(format!(
                "Invalid VM instruction package: {}",
                e
            )))
        }
        Ok(_) => {}
    }
    runnable.context.discard_objects(1)?;
    runnable
        .context
        .push_object(OnionObject::InstructionPackage(package).stabilize())?;
    Ok(StepResult::Continue)
}

pub fn sync_call(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let lambda = runnable.context.get_object_rev(1)?;
    let args = runnable.context.get_object_rev(0)?;
    let new_runnable = lambda
        .weak()
        .with_data(|lambda_ref| {
            if let OnionObject::Lambda(lambda_ref) = lambda_ref {
                let assigned = lambda_ref.parameter.as_ref().with_data(|parameter| {
                    let OnionObject::Tuple(parameters) = parameter else {
                        return Err(ObjectError::InvalidType(format!(
                            "Lambda parameters are not a Tuple: {:?}",
                            lambda_ref
                        )));
                    };

                    let OnionObject::Tuple(args_tuple) = &*args.weak().try_borrow()? else {
                        return Err(ObjectError::InvalidType(format!(
                            "Arguments are not a Tuple: {}",
                            args
                        )));
                    };
                    parameters.clone_and_named_assignment(args_tuple)
                })?;
                lambda_ref
                    .create_runnable(assigned, lambda, gc)
                    .map_err(|e| {
                        ObjectError::InvalidType(format!(
                            "Failed to create runnable from lambda: {}",
                            e
                        ))
                    })
            } else {
                Err(ObjectError::InvalidType(format!(
                    "Object is not a Lambda: {:?}",
                    lambda_ref
                )))
            }
        })
        .map_err(RuntimeError::ObjectError)?;

    runnable.context.discard_objects(2)?;
    let async_scheduler = Scheduler::new(vec![new_runnable]);
    Ok(StepResult::NewRunnable(Box::new(async_scheduler)))
}

pub fn map_to(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
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
    gc: &mut GC<OnionObjectCell>,
) -> Result<StepResult, RuntimeError> {
    let lambda = runnable.context.get_object_rev(1)?;
    let args = runnable.context.get_object_rev(0)?;
    let new_runnable = lambda
        .weak()
        .with_data(|lambda_ref| {
            if let OnionObject::Lambda(lambda_ref) = lambda_ref {
                let assigned = lambda_ref.parameter.as_ref().with_data(|parameter| {
                    let OnionObject::Tuple(parameters) = parameter else {
                        return Err(ObjectError::InvalidType(format!(
                            "Lambda parameters are not a Tuple: {:?}",
                            lambda_ref
                        )));
                    };

                    let OnionObject::Tuple(args_tuple) = &*args.weak().try_borrow()? else {
                        return Err(ObjectError::InvalidType(format!(
                            "Arguments are not a Tuple: {}",
                            args
                        )));
                    };
                    parameters.clone_and_named_assignment(args_tuple)
                })?;
                lambda_ref
                    .create_runnable(assigned, lambda, gc)
                    .map_err(|e| {
                        ObjectError::InvalidType(format!(
                            "Failed to create runnable from lambda: {}",
                            e
                        ))
                    })
            } else {
                Err(ObjectError::InvalidType(format!(
                    "Object is not a Lambda: {:?}",
                    lambda_ref
                )))
            }
        })
        .map_err(RuntimeError::ObjectError)?;

    runnable.context.discard_objects(2)?;
    let async_scheduler = AsyncScheduler::new(vec![new_runnable]);
    Ok(StepResult::NewRunnable(Box::new(async_scheduler)))
}
