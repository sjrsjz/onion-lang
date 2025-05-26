use std::{cell::RefCell, sync::Arc};

use onion_frontend::compile::build_code;
use onion_vm::{lambda::{runnable::{Runnable, StepResult}, scheduler::scheduler::Scheduler}, types::{lambda::{runnable::OnionLambdaRunnable, vm_instructions::ir_translator::IRTranslator}, object::OnionObject, tuple::OnionTuple}, GC};

fn main() {
    let code = r#"
    foo := (a?, b?) -> {
        i := mut 0;
        while (i < 1000000) {
            i = i + 1;
        };

        return a + b + i
    };
    return foo(1, 2);

    // obj := {
    //     "name": mut "Onion",
    //     "version": 1.0,
    //     "features": ["fast", "reliable", "secure"]
    // };

    // // obj[1] = "updated value"; // panic: cannot assign to immutable field
    // obj.name = "updated value"; // this is allowed, as the field is mutable
    // return obj;

    "#;
    let dir_stack = onion_frontend::dir_stack::DirStack::new(None);
    if let Err(e) = dir_stack {
        panic!("Failed to push directory: {}", e);
    }
    // Test the build_code function
    let mut dir_stack = dir_stack.unwrap();
    let ir_package = build_code(code, &mut dir_stack);
    match &ir_package {
        Ok(_) => {}
        Err(e) => {
            println!("Failed to build code: {}", e);
            return;
        }
    }
    let ir_package = ir_package.unwrap();

    // transform the IR package to a runtime package
    let mut vm_instructions_package = IRTranslator::new(&ir_package);
    match vm_instructions_package.translate() {
        Ok(_) => {}
        Err(e) => {
            println!("Failed to translate IR package: {:?}", e);
            return;
        }
    }
    let vm_instructions_package = vm_instructions_package.get_result();

    // println!("IR Package: {:?}", ir_package);
    // println!("VM Instructions Package: {:?}", vm_instructions_package);


    let entry = *vm_instructions_package.get_table().get("__main__").expect("Entry point '__main__' not found in VM instructions package");

    let lambda = OnionLambdaRunnable::new(OnionObject::Tuple(OnionTuple::new(vec![])).stabilize(), Arc::new(RefCell::new(vm_instructions_package)), entry as isize); 
    let Ok(lambda) = lambda else {
        println!("Failed to create runnable lambda");
        return;
    };

    let mut scheduler = Scheduler::new(vec![Box::new(lambda)]);

    let mut gc = GC::new();

    let mut counter = 0;
    loop {
        match scheduler.step(&mut gc) {
            Ok(step_result) => {
                match step_result {
                    StepResult::Continue => {
                        // Continue to the next step
                    },
                    StepResult::NewRunnable(_) => {
                        // Add the new runnable to the scheduler
                        unreachable!()
                    },
                    StepResult::Return(result) => {
                        // Print the result and exit
                        println!("Execution completed with result: {:?}", result);
                        break;
                    },
                    StepResult::Error(err) => {
                        println!("Runtime error: {}", err);
                        break;
                    }
                }
            }
            Err(e) => {
                println!("Error during execution: {}", e);
                break;
            }            
        }
        counter += 1;
        if counter > 100000000 {
            gc.collect();
        }
    }


}
