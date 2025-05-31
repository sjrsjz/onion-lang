# 🧅 Onion Virtual Machine

The runtime virtual machine for the Onion programming language. This package provides the execution environment, type system, and runtime services for Onion programs.

## 📚 Overview

The `onion-vm` package implements a register-based virtual machine designed specifically for functional programming with asynchronous execution capabilities. It features a sophisticated type system, garbage collection, and a generator-based execution model that enables seamless async/await functionality.

## 🏗️ Architecture

```
Bytecode → VM Instructions → Lambda Scheduler → Type System → Runtime Execution
```

### Core Components

- **Type System** (`types/`) - Comprehensive type definitions and operations
- **Lambda Runtime** (`lambda/`) - Function execution and scheduling
- **Scheduler** (`lambda/scheduler/`) - Asynchronous task coordination
- **Garbage Collection** - Automatic memory management with reference counting

## ✨ Features

- 🚀 **Asynchronous Execution** - Generator-based coroutines with seamless async/await
- 🔒 **Memory Safety** - Automatic garbage collection and safe reference handling  
- 🎯 **Dynamic Type System** - Flexible types with runtime type checking
- 📦 **Modular Design** - Clean separation between types, execution, and scheduling
- 🌊 **Lazy Evaluation** - Built-in support for lazy collections and streams
- 🔧 **Extensible Runtime** - Plugin architecture for custom types and operations

## 🎮 Type System

### Core Types

- **OnionObject** - Base type for all Onion values
- **OnionTuple** - Immutable sequences and arrays
- **OnionPair** - Key-value pairs and associations  
- **OnionNamed** - Named values and identifiers
- **OnionLambda** - Functions and closures
- **OnionLazySet** - Lazy collections with streaming operations

### Type Features

```rust
use onion_vm::types::object::OnionObject;
use onion_vm::types::tuple::OnionTuple;
use onion_vm::types::lambda::OnionLambda;

// Creating and manipulating types
let obj = OnionObject::new_integer(42);
let tuple = OnionTuple::from_vec(vec![obj]);
let lambda = OnionLambda::new(/* lambda definition */);
```

## 🔄 Execution Model

### Lambda Scheduler

The VM uses a sophisticated scheduler that manages:

- **Coroutines** - Lightweight tasks with cooperative scheduling
- **Generators** - Pausable functions with yield points  
- **Async Operations** - Non-blocking I/O and computation
- **Resource Management** - Automatic cleanup and lifecycle management

### Execution Flow

```rust
use onion_vm::lambda::scheduler::Scheduler;
use onion_vm::lambda::runnable::Runnable;

// Create and run a task
let mut scheduler = Scheduler::new();
let task = /* create runnable task */;
scheduler.spawn(task);
scheduler.run_until_complete();
```

## 🚀 Usage

### Basic VM Operation

```rust
use onion_vm::types::object::OnionObject;
use onion_vm::lambda::scheduler::Scheduler;

// Create VM instance
let mut scheduler = Scheduler::new();

// Load and execute bytecode
let bytecode = /* load compiled bytecode */;
let result = scheduler.execute(bytecode)?;
```

### Custom Type Integration

```rust
use onion_vm::types::object::{OnionObject, RuntimeError};

// Implement custom type
impl CustomType {
    fn to_onion_object(&self) -> OnionObject {
        // Convert to VM representation
    }
    
    fn from_onion_object(obj: &OnionObject) -> Result<Self, RuntimeError> {
        // Convert from VM representation
    }
}
```

### Async Programming

```rust
use onion_vm::lambda::runnable::{Runnable, StepResult};

struct AsyncTask {
    state: TaskState,
}

impl Runnable for AsyncTask {
    fn step(&mut self) -> StepResult {
        match self.state {
            TaskState::Running => {
                // Perform work
                StepResult::Yield
            }
            TaskState::Completed => StepResult::Return(result),
        }
    }
}
```

## 📋 API Reference

### Core Types

- `OnionObject` - Universal value type with runtime type information
- `OnionTuple` - Immutable sequence type for collections
- `OnionLambda` - Function type with closure support
- `OnionLazySet` - Lazy collection with functional operations

### Runtime Services

- `Scheduler` - Task scheduling and async coordination
- `Runnable` - Trait for executable tasks and coroutines
- `GC` - Garbage collection and memory management

### Instruction Set

- `VMInstructionPackage` - Compiled bytecode container
- `IRPackage` - Intermediate representation format
- `IRTranslator` - Converts IR to executable instructions

## 🔧 Dependencies

- `rust-arc-gc` - Garbage collection implementation
- `serde` - Serialization support for bytecode and types
- `bincode` - Binary serialization format
- `rustc-hash` - High-performance hashing

## 🏗️ Module Structure

```
onion-vm/
├── src/
│   ├── lib.rs              # Public API exports
│   ├── types/              # Type system implementation
│   │   ├── mod.rs
│   │   ├── object.rs       # Base object type
│   │   ├── tuple.rs        # Sequence types
│   │   ├── pair.rs         # Key-value pairs
│   │   ├── named.rs        # Named values
│   │   ├── lazy_set.rs     # Lazy collections
│   │   └── lambda/         # Function types
│   │       ├── mod.rs
│   │       ├── definition.rs
│   │       └── vm_instructions/
│   └── lambda/             # Execution runtime
│       ├── mod.rs
│       ├── runnable.rs     # Task execution interface
│       └── scheduler/      # Async scheduler
│           └── scheduler.rs
└── Cargo.toml
```

## 🎯 Design Principles

1. **Safety First** - Memory safety through garbage collection and type checking
2. **Performance** - Efficient execution with minimal overhead
3. **Concurrency** - Native async support without blocking
4. **Simplicity** - Clean abstractions that are easy to understand and extend

## 🔄 Execution Lifecycle

1. **Loading** - Bytecode is loaded and validated
2. **Initialization** - VM state and scheduler are set up
3. **Execution** - Instructions are executed by the scheduler
4. **Yielding** - Tasks can yield control for async operations
5. **Completion** - Results are collected and returned
6. **Cleanup** - Garbage collection reclaims unused memory

## 🤝 Integration

This package provides the runtime foundation for:

- **onion-frontend** - Executes compiled bytecode
- **Standard Library** - Implements built-in functions and types
- **Language Extensions** - Supports custom types and operations
- **Debugging Tools** - Provides introspection and profiling

## 📄 License

This package is part of the Onion programming language project and is licensed under the MIT License.
