use std::sync::Arc;

use instruction_set::VMInstructionPackage;
use rustc_hash::FxHashMap as HashMap;

use arc_gc::gc::GC;
use opcode::{OpcodeArgument, ProcessedOpcode};

use crate::{
    lambda::{
        runnable::{RuntimeError, StepResult},
        scheduler::{
            async_scheduler::{AsyncScheduler, Task},
            map_scheduler::Mapping,
            scheduler::Scheduler,
        },
    },
    onion_tuple,
    types::{
        async_handle::OnionAsyncHandle,
        lambda::launcher::OnionLambdaRunnableLauncher,
        lazy_set::OnionLazySet,
        named::OnionNamed,
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        pair::OnionPair,
        thread_handle::OnionThreadHandle,
        tuple::OnionTuple,
    },
    unwrap_step_result,
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
) -> StepResult {
    if let OpcodeArgument::Int64(value) = opcode.operand1 {
        unwrap_step_result!(runnable
            .context
            .push_object(OnionObject::Integer(value).consume_and_stabilize()));
        StepResult::Continue
    } else {
        StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected Int64 operand for LoadInt instruction, but found {:?}",
                opcode.operand1
            )
            .into(),
        ))
    }
}

pub fn load_null(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    unwrap_step_result!(runnable
        .context
        .push_object(OnionObject::Null.consume_and_stabilize()));
    StepResult::Continue
}

pub fn load_undefined(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    unwrap_step_result!(runnable
        .context
        .push_object(OnionObject::Undefined(None).consume_and_stabilize()));
    StepResult::Continue
}

pub fn load_float(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    if let OpcodeArgument::Float64(value) = opcode.operand1 {
        unwrap_step_result!(runnable
            .context
            .push_object(OnionObject::Float(value).consume_and_stabilize()));
        StepResult::Continue
    } else {
        StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected Float64 operand for LoadFloat instruction, but found {:?}",
                opcode.operand1
            )
            .into(),
        ))
    }
}

pub fn load_string(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    if let OpcodeArgument::String(value) = opcode.operand1 {
        let string = OnionObject::String(Arc::new(
            runnable.instruction.get_string_pool()[value as usize].clone(),
        ))
        .consume_and_stabilize();
        unwrap_step_result!(runnable.context.push_object(string));
        StepResult::Continue
    } else {
        StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected String operand for LoadString instruction, but found {:?}",
                opcode.operand1
            )
            .into(),
        ))
    }
}

pub fn load_bytes(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    if let OpcodeArgument::ByteArray(value) = opcode.operand1 {
        let bytes = OnionObject::Bytes(Arc::new(
            runnable.instruction.get_bytes_pool()[value as usize].clone(),
        ))
        .consume_and_stabilize();
        unwrap_step_result!(runnable.context.push_object(bytes));
        StepResult::Continue
    } else {
        StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected ByteArray operand for LoadBytes instruction, but found {:?}",
                opcode.operand1
            )
            .into(),
        ))
    }
}

pub fn load_bool(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    if let OpcodeArgument::Int32(value) = opcode.operand1 {
        unwrap_step_result!(runnable
            .context
            .push_object(OnionObject::Boolean(value != 0).consume_and_stabilize()));
        StepResult::Continue
    } else {
        StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected Int32 operand for LoadBool instruction, but found {:?}",
                opcode.operand1
            )
            .into(),
        ))
    }
}

pub fn discard_top(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：直接获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    unwrap_step_result!(Context::discard_from_stack(stack, 1));
    StepResult::Continue
}

pub fn build_tuple(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    if let OpcodeArgument::Int64(size) = opcode.operand1 {
        // 快速路径：一次获取栈的可变引用，避免重复查找
        let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
        let mut tuple = Vec::with_capacity(size as usize);
        for i in 1..=size as usize {
            tuple.push(unwrap_step_result!(Context::get_object_from_stack(
                stack,
                size as usize - i
            )));
        }
        let tuple = OnionTuple::new_static(tuple);
        unwrap_step_result!(Context::discard_from_stack(stack, size as usize));
        Context::push_to_stack(stack, tuple);
        StepResult::Continue
    } else {
        StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected Int64 operand for BuildTuple instruction, but found {:?}",
                opcode.operand1
            )
            .into(),
        ))
    }
}

pub fn build_keyval(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let key = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let value = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let keyval = OnionPair::new_static(key, value);
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, keyval);
    StepResult::Continue
}

pub fn build_named(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let key = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let value = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let named = OnionNamed::new_static(key, value);
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, named);
    StepResult::Continue
}

pub fn build_range(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let range =
        OnionObject::Range(
            unwrap_step_result!(left
                .weak()
                .to_integer()
                .map_err(|e| RuntimeError::DetailedError(
                    format!("Failed to build range: {}", e).into()
                ))),
            unwrap_step_result!(right.weak().to_integer().map_err(
                |e| RuntimeError::DetailedError(format!("Failed to build range: {}", e).into())
            )),
        )
        .consume_and_stabilize();

    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, range);
    StepResult::Continue
}

pub fn build_set(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let container = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let filter = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let set = OnionLazySet::new_static(container, filter);
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, set);
    StepResult::Continue
}

pub fn load_lambda(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let OpcodeArgument::String(signature_index) = opcode.operand1 else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected String operand for lambda signature index, but found {:?}",
                opcode.operand1
            )
            .into(),
        ));
    };
    let OpcodeArgument::Int64(_code_position) = opcode.operand2 else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected Int64 operand for lambda code position, but found {:?}",
                opcode.operand2
            )
            .into(),
        ));
    };
    let OpcodeArgument::Int32(flags) = opcode.operand3 else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected Int32 operand for lambda flags, but found {:?}",
                opcode.operand3
            )
            .into(),
        ));
    };

    let signature = unwrap_step_result!(runnable
        .instruction
        .get_string_pool()
        .get(signature_index as usize)
        .ok_or_else(|| {
            RuntimeError::DetailedError(
                format!(
                    "Lambda signature index {} is out of bounds (pool size: {})",
                    signature_index,
                    runnable.instruction.get_string_pool().len()
                )
                .into(),
            )
        }))
    .clone();

    let should_capture = flags & 0x01 != 0;

    let captured_value = if should_capture {
        Some(unwrap_step_result!(runnable.context.get_object_rev(1)))
    } else {
        None
    };

    let default_parameters = unwrap_step_result!(runnable
        .context
        .get_object_rev(if should_capture { 2 } else { 1 }))
    .weak()
    .stabilize();

    let OnionObject::Tuple(_) = default_parameters.weak() else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Lambda default parameters must be a Tuple, but found {}",
                default_parameters
            )
            .into(),
        ));
    };

    let instruction_package = unwrap_step_result!(runnable.context.get_object_rev(0))
        .weak()
        .stabilize();
    let OnionObject::InstructionPackage(package) = instruction_package.weak() else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Lambda instruction must be an InstructionPackage, but found {}",
                instruction_package
            )
            .into(),
        ));
    };

    let lambda = OnionLambdaDefinition::new_static(
        &default_parameters,
        LambdaBody::Instruction(package.clone()),
        captured_value,
        None,
        signature,
    );

    unwrap_step_result!(runnable
        .context
        .discard_objects(if should_capture { 3 } else { 2 }));
    unwrap_step_result!(runnable.context.push_object(lambda));
    StepResult::Continue
}

pub fn let_var(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let OpcodeArgument::String(index) = opcode.operand1 else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected String operand for variable index in LetVar instruction, but found {:?}",
                opcode.operand1
            )
            .into(),
        ));
    };

    let value = unwrap_step_result!(runnable.context.get_object_rev(0));

    // let name = {
    //     let instruction = runnable.borrow_instruction());
    //     instruction
    //         .get_string_pool()
    //         .get(index as usize)
    //         .ok_or_else(|| {
    //             RuntimeError::DetailedError(format!("Name index out of bounds: {}", index))
    //         }))
    //         .clone()
    // };

    unwrap_step_result!(runnable.context.let_variable(index as usize, value.clone()));
    StepResult::Continue
}

pub fn get_var(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let OpcodeArgument::String(index) = opcode.operand1 else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Expected String operand for variable index in GetVar instruction, but found {:?}",
                opcode.operand1
            )
            .into(),
        ));
    };

    // let value = {
    //     let instruction = runnable.borrow_instruction());
    //     let name = instruction
    //         .get_string_pool()
    //         .get(index as usize)
    //         .ok_or_else(|| {
    //             RuntimeError::DetailedError(format!("Name index out of bounds: {}", index))
    //         }));
    //     runnable.context.get_variable(name))
    // };

    let value = runnable.context.get_variable(index as usize);
    match value {
        Some(v) => {
            unwrap_step_result!(runnable.context.push_object(v.clone()));
            StepResult::Continue
        }
        None => {
            let var_name = runnable
                .instruction
                .get_string_pool()
                .get(index as usize)
                .cloned()
                .unwrap_or_else(|| format!("Variable at index {}", index));
            return StepResult::Error(RuntimeError::DetailedError(
                format!("Variable '{}' not found in context", var_name).into(),
            ));
        }
    }
}

pub fn set_var(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let right = unwrap_step_result!(runnable.context.get_object_rev(0));
    let left = unwrap_step_result!(runnable.context.get_object_rev(1));
    unwrap_step_result!(left.weak().assign(right.weak()));
    // 保留左侧对象的引用，由于其是不可变的可变对象的引用，assign不会导致GC错误回收
    unwrap_step_result!(runnable.context.discard_objects(1));
    StepResult::Continue
}

pub fn get_attr(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let attr = unwrap_step_result!(runnable.context.get_object_rev(0)).weak();
    let obj = unwrap_step_result!(runnable.context.get_object_rev(1));
    let element = unwrap_step_result!(attr.with_data(|attr| {
        Ok(
            match obj.weak().with_attribute(attr, &|attr| Ok(attr.clone())) {
                Ok(ref value) => match value {
                    OnionObject::Lambda(ref lambda) => lambda.clone_and_replace_self_object(obj),
                    _ => value.stabilize(),
                },
                Err(e) => {
                    return Err(RuntimeError::InvalidOperation(
                        format!(
                            "Cannot access attribute {:?} on object {}: {}",
                            attr, obj, e
                        )
                        .into(),
                    ));
                }
            },
        )
    }));

    unwrap_step_result!(runnable.context.discard_objects(2));
    unwrap_step_result!(runnable.context.push_object(element));
    StepResult::Continue
}

pub fn index_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // Get both objects first
    let index_obj = unwrap_step_result!(runnable.context.get_object_rev(0));
    let container_obj = unwrap_step_result!(runnable.context.get_object_rev(1));

    // Extract the integer index value
    let index_value = unwrap_step_result!(index_obj.weak().with_data(|index_ref| {
        if let OnionObject::Integer(index) = index_ref {
            Ok(*index)
        } else {
            Err(RuntimeError::DetailedError(
                format!("Index must be an integer, but found {}", index_obj).into(),
            ))
        }
    }));

    // Get the element at the index
    let element = {
        let obj_ref = container_obj.weak();
        unwrap_step_result!(obj_ref.at(index_value))
    };

    // Now modify the context after all borrows are dropped
    unwrap_step_result!(runnable.context.discard_objects(2));
    unwrap_step_result!(runnable.context.push_object(element));
    StepResult::Continue
}

pub fn key_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let obj = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let key = unwrap_step_result!(obj.weak().key_of());
    Context::replace_last_object(stack, key);
    StepResult::Continue
}

pub fn value_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let obj = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    // 这里可能触发 Pending 错误，交给调用者处理
    let value = unwrap_step_result!(obj.weak().value_of());
    Context::replace_last_object(stack, value);
    StepResult::Continue
}

pub fn type_of(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let obj = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let type_name = unwrap_step_result!(obj.weak().type_of());
    Context::replace_last_object(
        stack,
        OnionObject::String(Arc::new(type_name)).consume_and_stabilize(),
    );
    StepResult::Continue
}

pub fn copy(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let obj = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let copied_obj = unwrap_step_result!(obj.weak().copy());
    unwrap_step_result!(Context::discard_from_stack(stack, 1));
    Context::push_to_stack(stack, copied_obj);
    StepResult::Continue
}

pub fn check_is_same_object(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let obj1 = unwrap_step_result!(runnable.context.get_object_rev(0));
    let obj2 = unwrap_step_result!(runnable.context.get_object_rev(1));
    let is_same = unwrap_step_result!(obj1.weak().is_same(obj2.weak()));
    unwrap_step_result!(runnable.context.discard_objects(2));
    unwrap_step_result!(runnable
        .context
        .push_object(OnionObject::Boolean(is_same).consume_and_stabilize()));
    StepResult::Continue
}

pub fn binary_add(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_add(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_subtract(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_sub(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_multiply(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_mul(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_divide(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_div(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_modulus(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_mod(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_power(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_pow(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_bitwise_or(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_or(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_bitwise_and(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_and(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_bitwise_xor(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_xor(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_shift_left(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_shl(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_shift_right(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_shr(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}

pub fn binary_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_eq(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, OnionObject::Boolean(result).consume_and_stabilize());
    StepResult::Continue
}

pub fn binary_not_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_eq(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, OnionObject::Boolean(!result).consume_and_stabilize());
    StepResult::Continue
}

pub fn binary_greater(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_gt(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, OnionObject::Boolean(result).consume_and_stabilize());
    StepResult::Continue
}

pub fn binary_greater_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_lt(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, OnionObject::Boolean(!result).consume_and_stabilize());
    StepResult::Continue
}

pub fn binary_less(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_lt(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, OnionObject::Boolean(result).consume_and_stabilize());
    StepResult::Continue
}

pub fn binary_less_equal(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let right = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let left = unwrap_step_result!(Context::get_object_from_stack(stack, 1));
    let result = unwrap_step_result!(left.weak().binary_gt(right.weak()));
    unwrap_step_result!(Context::discard_from_stack(stack, 2));
    Context::push_to_stack(stack, OnionObject::Boolean(!result).consume_and_stabilize());
    StepResult::Continue
}

pub fn unary_bitwise_not(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let obj = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let result = unwrap_step_result!(obj.weak().unary_not());
    unwrap_step_result!(Context::discard_from_stack(stack, 1));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}
pub fn unary_plus(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let obj = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let result = unwrap_step_result!(obj.weak().unary_plus());
    unwrap_step_result!(Context::discard_from_stack(stack, 1));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}
pub fn unary_minus(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 快速路径：一次获取栈的可变引用，避免重复查找
    let stack = unwrap_step_result!(runnable.context.get_current_stack_mut());
    let obj = unwrap_step_result!(Context::get_object_from_stack(stack, 0));
    let result = unwrap_step_result!(obj.weak().unary_neg());
    unwrap_step_result!(Context::discard_from_stack(stack, 1));
    Context::push_to_stack(stack, result);
    StepResult::Continue
}
pub fn swap(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let OpcodeArgument::Int64(index) = opcode.operand1 else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!("Invalid argument type for Swap: {:?}", opcode.operand1).into(),
        ));
    };
    unwrap_step_result!(runnable.context.swap(0, index as usize));
    StepResult::Continue
}
pub fn jump(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let OpcodeArgument::Int64(offset) = opcode.operand1 else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!("Invalid argument type for Jump: {:?}", opcode.operand1).into(),
        ));
    };
    runnable.ip += offset as isize;
    StepResult::Continue
}

pub fn jump_if_false(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let OpcodeArgument::Int64(offset) = opcode.operand1 else {
        return StepResult::Error(RuntimeError::DetailedError(
            format!("Invalid argument type for JumpIfNot: {:?}", opcode.operand1).into(),
        ));
    };
    let condition = unwrap_step_result!(runnable.context.get_object_rev(0));
    if !unwrap_step_result!(condition.weak().to_boolean()) {
        runnable.ip += offset as isize;
    }
    unwrap_step_result!(runnable.context.discard_objects(1));
    StepResult::Continue
}
pub fn get_length(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let obj = unwrap_step_result!(runnable.context.get_object_rev(0));
    let length = unwrap_step_result!(obj.weak().len());
    unwrap_step_result!(runnable.context.discard_objects(1));
    unwrap_step_result!(runnable.context.push_object(length));
    StepResult::Continue
}

pub fn new_frame(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    runnable.context.push_frame(Frame {
        variables: HashMap::default(),
        stack: vec![],
    });
    StepResult::Continue
}

pub fn pop_frame(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    unwrap_step_result!(runnable.context.concat_last_frame());
    StepResult::Continue
}

pub fn clear_stack(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    runnable.context.clear_stack();
    StepResult::Continue
}

pub fn assert(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let condition = unwrap_step_result!(runnable.context.get_object_rev(0));
    if !unwrap_step_result!(condition.weak().to_boolean()) {
        return StepResult::Error(RuntimeError::DetailedError(
            format!(
                "Assertion failed: condition evaluated to false (value: {})",
                condition
            )
            .into(),
        ));
    }
    StepResult::Continue
}

pub fn is_in(
    runnable: &mut LambdaRunnable,
    opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let element = unwrap_step_result!(runnable.context.get_object_rev(1));
    let container = unwrap_step_result!(runnable.context.get_object_rev(0));

    // 先检查元素是否在惰性集合的容器中
    let is_in =
        unwrap_step_result!(container
            .weak()
            .with_data(|container_ref| match container_ref {
                OnionObject::LazySet(lazy_set) => lazy_set.get_container().contains(element.weak()),
                _ => {
                    return Err(RuntimeError::DetailedError(
                        format!("Container is not a LazySet: {}", container).into(),
                    ));
                }
            }));

    if !is_in {
        // 元素不在容器中，直接返回 false
        unwrap_step_result!(runnable.context.discard_objects(2));
        unwrap_step_result!(runnable
            .context
            .push_object(OnionObject::Boolean(false).consume_and_stabilize()));
        return StepResult::Continue;
    }

    // 如果在容器中，则调用惰性集合的过滤器
    let (args, filter) = unwrap_step_result!(container.weak().with_data(|container_ref| {
        match container_ref {
            OnionObject::LazySet(lazy_set) => {
                let filter = lazy_set.get_filter();
                let OnionObject::Lambda(_) = filter else {
                    // 过滤器不是 Lambda 对象，认定为惰性求值结果为 filter
                    let filter = filter.stabilize();
                    return Ok((None, filter));
                };
                let args = OnionTuple::new_static(vec![element]);
                let filter = filter.stabilize();
                return Ok((Some(args), filter));
            }
            _ => {
                return Err(RuntimeError::DetailedError(
                    format!("Container is not a LazySet: {}", container).into(),
                ));
            }
        }
    }));

    unwrap_step_result!(runnable.context.discard_objects(2));

    if let Some(args) = args {
        unwrap_step_result!(runnable.context.push_object(filter));
        unwrap_step_result!(runnable.context.push_object(args));
        // 运行过滤器
        return call_lambda(runnable, opcode, gc);
    };

    unwrap_step_result!(runnable.context.push_object(filter));
    return StepResult::Continue;
}

pub fn call_lambda(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let lambda = unwrap_step_result!(runnable.context.get_object_rev(1));
    let args = unwrap_step_result!(runnable.context.get_object_rev(0));
    let new_runnable = Box::new(unwrap_step_result!(
        OnionLambdaRunnableLauncher::new_static(lambda, args, &|r| Ok(r),)
    ));
    unwrap_step_result!(runnable.context.discard_objects(2));
    StepResult::NewRunnable(new_runnable)
}

pub fn mutablize(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let obj = unwrap_step_result!(runnable.context.pop());
    unwrap_step_result!(runnable.context.push_object(obj.mutablize(gc)));
    StepResult::Continue
}

pub fn immutablize(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let obj = unwrap_step_result!(runnable.context.pop());
    unwrap_step_result!(runnable
        .context
        .push_object(unwrap_step_result!(obj.immutablize())));
    StepResult::Continue
}

pub fn return_value(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let return_value = unwrap_step_result!(runnable.context.get_object_rev(0));
    StepResult::Return(return_value.clone().into())
}

pub fn fork_instruction(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let forked =
        OnionObject::InstructionPackage(runnable.instruction.clone()).consume_and_stabilize();
    unwrap_step_result!(runnable.context.push_object(forked));
    StepResult::Continue
}

pub fn import(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let path = unwrap_step_result!(runnable.context.get_object_rev(0));

    let package = unwrap_step_result!(path.weak().with_data(|path_ref| {
        let OnionObject::String(path) = path_ref else {
            return Err(RuntimeError::DetailedError(
                format!("Invalid path type for `import`: {}", path).into(),
            ));
        };
        VMInstructionPackage::read_from_file(&path).map_err(|e| {
            RuntimeError::DetailedError(
                format!("Failed to load instruction package from '{}': {}", path, e).into(),
            )
        })
    }));
    match VMInstructionPackage::validate(&package) {
        Err(e) => {
            return StepResult::Error(RuntimeError::DetailedError(
                format!("Invalid VM instruction package: {}", e).into(),
            ))
        }
        Ok(_) => {}
    }
    unwrap_step_result!(runnable.context.discard_objects(1));
    unwrap_step_result!(runnable
        .context
        .push_object(OnionObject::InstructionPackage(Arc::new(package)).consume_and_stabilize()));
    StepResult::Continue
}

pub fn sync_call(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let lambda = unwrap_step_result!(runnable.context.get_object_rev(1));
    let args = unwrap_step_result!(runnable.context.get_object_rev(0));
    let new_runnable = Box::new(Scheduler::new(vec![Box::new(unwrap_step_result!(
        OnionLambdaRunnableLauncher::new_static(lambda, args, |r| Ok(r))
    ))]));
    unwrap_step_result!(runnable.context.discard_objects(2));
    StepResult::NewRunnable(new_runnable)
}

pub fn map_to(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let container = unwrap_step_result!(runnable.context.get_object_rev(1));
    let mapper = unwrap_step_result!(runnable.context.get_object_rev(0));
    let mapping = Mapping {
        container: container.clone(),
        mapper: mapper.clone(),
        collected: vec![],
        current_index: 0,
    };
    unwrap_step_result!(runnable.context.discard_objects(2));
    return StepResult::NewRunnable(Box::new(mapping));
}

pub fn async_call(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let lambda = unwrap_step_result!(runnable.context.get_object_rev(1));
    let args = unwrap_step_result!(runnable.context.get_object_rev(0));
    let new_task_handler = OnionAsyncHandle::new(gc);
    let new_runnable = Box::new(AsyncScheduler::new(Task::new(
        Box::new(Scheduler::new(vec![Box::new(unwrap_step_result!(
            OnionLambdaRunnableLauncher::new_static(lambda, args, |r| Ok(r))
        ))])),
        new_task_handler,
        0,
    )));
    unwrap_step_result!(runnable.context.discard_objects(2));
    StepResult::NewRunnable(new_runnable)
}

pub fn raise(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    _gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    // 获取抛出对象
    let value = unwrap_step_result!(runnable.context.pop());
    return StepResult::Error(RuntimeError::CustomValue(Box::new(value)));
}

pub fn spawn_task(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let lambda = unwrap_step_result!(runnable.context.get_object_rev(0));

    let lambda_obj = unwrap_step_result!(lambda.weak().with_data(|data| {
        match data {
            OnionObject::Lambda(_) => Ok(lambda.clone()),
            _ => Err(RuntimeError::DetailedError(
                format!("spawn_task requires a Lambda object, but found {}", lambda).into(),
            )),
        }
    }));
    let new_runnable = Box::new(Scheduler::new(vec![Box::new(unwrap_step_result!(
        OnionLambdaRunnableLauncher::new_static(&lambda_obj, &onion_tuple!(), &|r| Ok(r))
    ))]));
    let task_handler = OnionAsyncHandle::new(gc);
    let task_object = OnionObject::Custom(task_handler.0.clone()).consume_and_stabilize();
    let task = Task::new(new_runnable, task_handler, 0);
    unwrap_step_result!(runnable.context.discard_objects(1));
    unwrap_step_result!(runnable.context.push_object(task_object));
    StepResult::SpawnRunnable(task.into())
}

pub fn launch_thread(
    runnable: &mut LambdaRunnable,
    _opcode: &ProcessedOpcode,
    gc: &mut GC<OnionObjectCell>,
) -> StepResult {
    let value = unwrap_step_result!(runnable.context.get_object_rev(0));

    // 检查 value 是否为 Lambda 对象
    let lambda_obj = unwrap_step_result!(value.weak().with_data(|data| {
        match data {
            OnionObject::Lambda(_) => Ok(value.clone()),
            _ => Err(RuntimeError::DetailedError(
                format!(
                    "launch_thread requires a Lambda object, but found {}",
                    value
                )
                .into(),
            )),
        }
    }));

    // 在新线程中启动 Lambda
    let handle: std::thread::JoinHandle<Result<Box<OnionStaticObject>, RuntimeError>> =
        std::thread::spawn(move || {
            use crate::lambda::runnable::Runnable;
            use crate::lambda::scheduler::scheduler::Scheduler;
            use crate::types::tuple::OnionTuple;

            let mut gc = GC::new_with_memory_threshold(1024 * 1024); // 1 MB threshold

            // 创建空参数元组
            let args = OnionTuple::new_static(vec![]);

            // 创建调度器运行 Lambda
            let mut scheduler: Box<dyn Runnable> = Box::new(Scheduler::new(vec![Box::new(
                OnionLambdaRunnableLauncher::new_static(&lambda_obj, &args, |r| Ok(r))?,
            )]));

            // 执行循环
            loop {
                #[cfg(debug_assertions)]
                gc.collect();
                match scheduler.step(&mut gc) {
                    StepResult::Continue => {
                        // 继续执行下一步
                    }
                    StepResult::SetSelfObject(_) => {
                        // 设置 self 对象，这里无需额外操作
                    }
                    StepResult::SpawnRunnable(_) => {
                        // 处理 SpawnRunnable 结果，由于同步调度器上级没有异步调度器，应当直接报错
                        return Err(RuntimeError::InvalidOperation(
                            "Cannot spawn async task in sync context".to_string().into(),
                        ));
                    }
                    StepResult::Error(ref error) => match error {
                        RuntimeError::Pending => {
                            // 处理挂起错误，继续等待
                            continue;
                        }
                        _ => {
                            // 返回错误
                            return Err(error.clone());
                        }
                    },
                    StepResult::NewRunnable(_) => {
                        unreachable!()
                    }
                    StepResult::ReplaceRunnable(_) => {
                        unreachable!()
                    }
                    StepResult::Return(v) => {
                        // 线程执行完成，退出
                        return Ok(v);
                    }
                }
            }
        });

    // 创建线程句柄对象并推入栈
    let thread_handle = OnionThreadHandle::new(handle, gc);
    let handle_object = OnionObject::Custom(Arc::new(thread_handle.0)).consume_and_stabilize();
    unwrap_step_result!(runnable.context.discard_objects(1));
    unwrap_step_result!(runnable.context.push_object(handle_object));
    StepResult::Continue
}
