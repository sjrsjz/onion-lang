# 🧅 Onion Programming Language

A modern functional programming language with a minimalist yet powerful type system, asynchronous execution capabilities, and flexible memory management.

The name `Onion` is inspired by the layered structure of onions, which mirrors the VM's execution model with state isolation and abstraction between layers.

## 🎯 What is Onion?

**Onion** refers to this programming language that embodies the "layered execution" philosophy:

- **Multi-level Nesting**: Schedulers can be nested arbitrarily deep - `AsyncScheduler` → `Scheduler` → `AsyncScheduler` → `Scheduler`
- **State Isolation**: Each layer maintains its own execution context, preventing interference between different scheduling levels  
- **Layer Communication**: Layers communicate through well-defined interfaces (`StepResult`, `Runnable` trait) while maintaining isolation
- **Flexible Composition**: You can compose different execution strategies (sync/async) at any depth

## ✨ Features

- 🚀 **Functional Programming Paradigm** - Support for higher-order functions, closures, and function composition
- ⚡ **Asynchronous Execution Model** - VM based on pure generator design, supporting asynchronous programming
- 🧵 **Multi-threading Support** - Native thread support with `launch` and `valueof` for concurrent execution, **No GIL**
- 🔒 **Safe Memory Management** - Mutability control and reference safety checks
- 🎯 **Minimalist yet Powerful Dynamic Type System** - Powerful functionality through composition of a few built-in types (such as prototype classes)
- 📦 **Modular Design** - Support for module imports and compilation caching
- 🌊 **Lazy Evaluation** - Built-in lazy collections and streaming operations
- ⚠ **Parameter Constraints** - Advanced parameter validation with custom constraint functions
- 🔧 **LSP Support** - Language Server Protocol support
- 🚀 **High Performance** - e.g., A 10 million iteration `while` count loop takes 1.17s, approx. 40% faster than RustPython's 1.91s.

## 🚀 Quick Start

### Installation

Make sure you have Rust toolchain installed, then clone and build the project:

```bash
git clone https://github.com/sjrsjz/onion-lang.git
cd onion-lang
cargo install --path .
```

### Hello World

Create a `hello.onion` file:

```onion
@required stdlib;

main := () -> {
    stdlib.io.println("Hello, Onion World!");
    return "Program completed";
};

main();
```

Run the program:

```bash
./target/release/onion run hello.onion
```

## 📚 Language Features Examples

### Function Definition and Invocation

```onion
// Basic function definition
square := (x?) -> x * x;

// Recursive function (Fibonacci sequence)
fib := (n => 0) -> {
    if (n <= 1) {
        return n;
    };
    return this(n - 1) + this(n - 2);
};

stdlib.io.println("Square of 5:", square(5));
stdlib.io.println("10th Fibonacci number:", fib(10));
```

### Mutability Control

```onion
// Create object with mutable and immutable elements
obj := [
    mut 0,  // mutable element
    1,      // immutable element
];

// Modify mutable element - success
obj[0] = 42;

// Try to modify immutable element - fails
// obj[1] = 100; // runtime error

// References and const references
ref := obj[0];        // mutable reference
const_ref := const obj[0];  // const reference
```

### Asynchronous Programming

```onion
// Asynchronous task with spawn
task := () -> {
    return (0..10).elements() |> (x?) -> {
        stdlib.io.println("Processing element:", x);
        return spawn () -> {
            stdlib.io.println("Async processing element:", x);
            n := mut 0;
            while (n < 5) {
                stdlib.io.println("Async element:", x, "count:", n);
                n = n + 1;
                stdlib.time.sleep_seconds(seconds => 0.01);
            };
            return x * 2;
        };
    };
};

// Execute asynchronously and get results
tasks := async task();
stdlib.io.println("Processing results:", valueof tasks);
```

### Multi-threading Support

```onion
// Create concurrent threads
thread1 := () -> {
    stdlib.io.println("Thread 1 starting");
    stdlib.time.sleep_seconds(seconds => 2);
    stdlib.io.println("Thread 1 completed");
    return "Result from thread 1";
};

thread2 := () -> {
    stdlib.io.println("Thread 2 starting");
    stdlib.time.sleep_seconds(seconds => 1);
    stdlib.io.println("Thread 2 completed");
    return "Result from thread 2";
};

// Launch threads
handle1 := launch thread1;
handle2 := launch thread2;

// Wait for results
result1 := valueof handle1;
result2 := valueof handle2;
stdlib.io.println("Results:", result1, result2);
```

### Parameter Constraints

```onion
// Define constraint functions
Positive := (x?) -> x > 0;
NonEmpty := (s?) -> stdlib.string.length(string => s) > 0;

// Functions with parameter constraints
add_positive := (a => 0 | Positive, b => 0 | Positive) -> {
    return a + b;
};

greet := (name => "World" | NonEmpty) -> {
    return "Hello, " + name + "!";
};

// Usage with automatic validation
result := add_positive(5, 3);  // ✓ Valid
// add_positive(-1, 5);        // ✗ Runtime error: constraint violation
```

### Lazy Collections and Stream Processing

```onion
// Lazy filtering with in-place filtering
filtered_set := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] | (x?) -> x < 5;
stdlib.io.println("Filtered set:", filtered_set);

// Check membership in lazy sets
stdlib.io.println("Is 3 in set?", 3 in filtered_set);  // true
stdlib.io.println("Is 7 in set?", 7 in filtered_set);  // false

// Lazy mapping
mapped_set := [1, 2, 3, 4, 5] | (x?) -> x * 2;
stdlib.io.println("Mapped set:", mapped_set);

// Collect lazy results
collected := filtered_set.collect();
stdlib.io.println("Collected results:", collected);

// Stream processing with ranges
range_result := (0..100).elements() |> (x?) -> {
    return x * x;
};
```

### Prototype Inheritance

```onion
// Define class prototype
ClassA := (struct?, interface => mut {
    print => () -> {
        stdlib.io.println("This is an instance of Class A");
    },
    to_string => () -> "ClassA: " + stdlib.types.to_string(self)
}) -> struct : interface;

// Create instance
instance := #ClassA mut {
    "name": "Test instance",
    "value": 42
};

instance.print();
```

### Enhanced String Literals

```onion
// Multiple string literal formats
stdlib.io.println("Double quoted string");
stdlib.io.println('Single quoted string');
stdlib.io.println("Escaped characters: \"Hello\", \n newline, \t tab");
stdlib.io.println("""Multi-line string
with line breaks""");
stdlib.io.println(R"(Raw string: \n no escaping needed)");

// String interpolation and concatenation
name := "World";
stdlib.io.println("Hello, " + name + "!");

// Unicode support
stdlib.io.println("Unicode: \u6b63\u5e38\u5b57\u7b26\u4e32");
```

### Advanced Type System

```onion
// Built-in type conversions
number := "42".int();
stdlib.io.println("Converted number:", number);

// Custom type conversion chains
int := (x?) -> x.int();
abs := (x?) -> if (x < 0) { -x } else { x };
result := "-42" as int as abs;
stdlib.io.println("Chained conversion:", result);

// Tuple operations
tuple := (1, 2, 3);
stdlib.io.println("Tuple:", tuple);
stdlib.io.println("Tuple length:", tuple.length());

// Tuples with methods
enhanced_tuple := (1, 2, 3, length => () -> "custom method");
stdlib.io.println("Custom method result:", enhanced_tuple.length());
```

### Modular Programming

```onion
// Module definition (module.onion)
return {
    get_version => () -> "1.0.0",
    utils => {
        add => (a?, b?) -> a + b,
        multiply => (a?, b?) -> a * b
    }
};

// Module import
@compile "./module.onion";
module := () -> dyn {
    import "./module.onionc"
};
module := module();
stdlib.io.println("Module version:", module.get_version());
```

```onion
@import "./ast.onion"; // Importing AST of onion source code
```

### Result and Option Types

```onion
Modules := mut ();
@import "../std/result.onion";
@import "../std/option.onion";

// Result type for error handling
safe_divide := (a?, b?) -> {
    if (b == 0) {
        return Err("Division by zero");
    } else {
        return Ok(a / b);
    }
};

result := safe_divide(10, 2);
if (result.is_ok()) {
    stdlib.io.println("Division result:", result.unwrap());
} else {
    stdlib.io.println("Error:", result.unwrap_err());
}

// Option type for nullable values
find_user := (id?) -> {
    if (id == 1) {
        return Some("Alice");
    } else {
        return None();
    }
};

user := find_user(1);
name := user.unwrap_or("Unknown");
stdlib.io.println("User name:", name);
```

### Macro System

```onion
@required stdlib;

@macro A := (
    add(_A, _B)
) : _A + _B;

@A stdlib.io.println(add(1, 2) + add(3, add(1,2)));

@macro combine := (
    _A, _B
) : _A(_B);

@combine stdlib.io.println, "Hello, World!";

@macro if := (
    _condition, _then, _else
) : if (_condition) { _then } else { _else };

@if false, stdlib.io.println("Condition is true"), stdlib.io.println("Condition is false", (@if true, "This is true", "This is false"));

@macro Pair := (
    pair(_A, _B)
): _A : _B;
@macro Named := (
    named(_A, _B)
): {_A} => _B;

@Named @Pair pair(1, named("A", "B"))
```


## 🛠️ Command Line Tools

Onion provides a complete set of command line tools:

```bash
# Run source code file
onion run file.onion

# Compile to bytecode
onion compile file.onion -o file.onionc

# Start interactive REPL
onion repl

# Start language server
onion lsp
```

## 🏗️ Project Structure

```
onion-lang/
├── src/                    # Main program source code
│   ├── main.rs            # CLI entry point
│   ├── repl.rs            # Interactive interpreter (not yet complete)
│   ├── lsp/               # Language Server Protocol
│   └── stdlib/            # Standard library
├── onion-frontend/        # Compilation frontend
│   └── src/
│       ├── parser/        # Lexer and parser
│       └── ir_generator/  # Intermediate code generator
├── onion-vm/             # Virtual machine runtime
│   └── src/
│       ├── lambda/        # Lambda computation and scheduler
│       └── types/         # Type system implementation
└── examples/             # Example code
```

## 🎯 Design Philosophy

Onion programming language is designed around the following core principles:

1. **Safety First** - Prevent common errors through type system and mutability control
2. **Expressiveness** - Functional programming paradigm provides concise and elegant syntax, supporting asynchronous execution and lazy evaluation
3. **Developer Friendly** - Provides LSP support

## 🔧 Development Status

This is an experimental programming language project, currently including:

- ✅ Basic syntax and type system
- ✅ Functional programming features
- ✅ Asynchronous execution model
- ✅ Multi-threading support with `launch` and `valueof`
- ✅ Parameter constraints and validation
- ✅ Enhanced string literals and Unicode support
- ✅ Lazy collections and stream processing
- ✅ Result and Option types for error handling
- ✅ Module system (static and dynamic imports)
- ✅ Simple macro system
- ✅ Language Server Protocol support
- ✅ Performance optimization
- ✅ REPL and command line tools
- 🚧 Standard library extensions
- 🚧 Documentation and toolchain refinement

## 📄 License

This project is licensed under the [MIT License](LICENSE).

## 🔗 Related Links

- [Language Specification](docs/language-spec.typ)
- [Example Code](examples/)
