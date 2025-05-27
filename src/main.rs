use onion_frontend::compile::build_code;
use onion_vm::{
    lambda::{
        runnable::{Runnable, StepResult},
        scheduler::scheduler::Scheduler,
    },
    types::{
        lambda::{
            definition::{LambdaBody, OnionLambdaDefinition},
            vm_instructions::ir_translator::IRTranslator,
        },
        object::OnionObject,
        tuple::OnionTuple,
    },
    GC,
};

fn main() {
    let code = r#"
    // This is a simple Onion code example
    fib := mut (n?) -> {
        if (n <= 1) {
            return n;
        };
        return this(n - 1) + this(n - 2);
    };
    assert(fib(10) == 55);

    obj := {
        "name": mut "Onion",
        "version": 1.0,
        "features": ["fast", "reliable", "secure"]
    };

    // obj[1] = "updated value"; // panic: cannot assign to immutable field
    obj.name = "updated value"; // this is allowed, as the field is mutable
    assert(obj.name == "updated value");


    interface := mut {
        greet => (name?) -> {
            self.member = "Hello, Onion Prototype!";
            return "Greeting: " + name + " " + self.member;
        },
    };

    A := {
        'member' : mut "Hello, Onion from A!",
    } : interface;

    B := {
        'member' : mut "Hello Onion from B!",
    } : interface;

    isinstance := (obj?, interface?) -> (valueof obj) is interface;

    assert isinstance(A, interface);
    assert isinstance(B, interface);

    assert(A.greet("World") == "Greeting: World Hello, Onion Prototype!");

    foo := (a?, b?) -> return arguments.a;
    assert(foo(1, 2) == 1);

    n := mut 10;
    foo := (v?) -> (v = 5);
    foo(n);
    assert(n == 5);

    lazy_set1 := "abc" | (x?) -> x;
    lazy_set2 := "abc" | true;
    assert("a" in lazy_set1 == "a");
    assert("a" in lazy_set2 == true);
    assert("d" in lazy_set1 == false);

    lazy_set3 := ["a", "b", "a", "b", "c"] | (x?) -> x == "a";
    lazy_set3.collect()

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

    let lambda = OnionLambdaDefinition::new_static(
        &OnionObject::Tuple(OnionTuple::new(vec![])).stabilize(),
        LambdaBody::Instruction(Box::new(OnionObject::InstructionPackage(
            vm_instructions_package,
        ))),
        None,
        None,
        "__main__".to_string(),
    );

    let OnionObject::Lambda(lambda) = lambda.weak() else {
        println!("Failed to create lambda definition");
        return;
    };

    let lambda = lambda.create_runnable(
        OnionObject::Tuple(OnionTuple::new(vec![])).stabilize(),
        &mut GC::new(),
    );

    let Ok(lambda) = lambda else {
        println!("Failed to create runnable lambda");
        return;
    };

    let mut scheduler = Scheduler::new(vec![lambda]);

    let mut gc = GC::new();

    loop {
        match scheduler.step(&mut gc) {
            Ok(step_result) => {
                match step_result {
                    StepResult::Continue => {
                        // Continue to the next step
                    }
                    StepResult::NewRunnable(_) => {
                        // Add the new runnable to the scheduler
                        unreachable!()
                    }
                    StepResult::Return(result) => {
                        // Print the result and exit
                        println!("Execution completed with result: {:?}", result);
                        break;
                    }
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
    }
}
