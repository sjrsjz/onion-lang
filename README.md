# 🧅 Onion Programming Language

A modern functional programming language with a minimalist yet powerful type system, asynchronous execution capabilities, and flexible memory management.

The name `Onion` is inspired by the layered structure of onions, which mirrors the VM's execution model with state isolation and abstraction between layers.

> **Note**: This project is significantly improved from [XLang-Rust](https://github.com/sjrsjz/XLang-Rust.git).

## ✨ Features

- 🚀 **Functional Programming Paradigm** - Support for higher-order functions, closures, and function composition
- ⚡ **Asynchronous Execution Model** - VM based on pure generator design, seamlessly supporting asynchronous programming
- 🔒 **Safe Memory Management** - Mutability control and reference safety checks
- 🎯 **Minimalist yet Powerful Dynamic Type System** - Powerful functionality through composition of a few built-in types (such as prototype classes)
- 📦 **Modular Design** - Support for module imports and compilation caching
- 🌊 **Lazy Evaluation** - Built-in lazy collections and streaming operations
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
task1 := () -> {
    i := mut 0;
    while (i < 10) {
        stdlib.io.println("Task 1:", i);
        i = i + 1;
    }
};

task2 := () -> {
    i := mut 0;
    while (i < 10) {
        stdlib.io.println("Task 2:", i);
        i = i + 1;
    }
};

main := () -> {
    sync task1();  // asynchronous execution
    sync task2();  // asynchronous execution
};

async main()  // asynchronous main function
```

### Lazy Collections and Stream Processing

```onion
// Lazy filtering
filtered_set := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] | (x?) -> x < 5;
stdlib.io.println("Filtered set:", filtered_set);

// Stream mapping
mapped_values := [1, 2, 3, 4, 5] |> (x?) -> x * x;
stdlib.io.println("Squared mapping:", mapped_values);
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

- [Language Specification](docs/language-spec.md)
- [Example Code](examples/)
