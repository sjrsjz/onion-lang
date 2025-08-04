# 🧅 Onion Programming Language

A modern, functional programming language featuring an expressive syntax, a powerful layered concurrency model, and a strong emphasis on safety and developer productivity.

The name `Onion` is inspired by its core design philosophy: a layered execution model that provides natural state isolation and abstraction, much like the layers of an onion.

## 🎯 What is Onion?

**Onion** is built on a "layered execution" philosophy, which means:

- **Deeply Nested Schedulers**: Schedulers can be nested to any depth (e.g., `AsyncScheduler` → `SyncScheduler` → `AsyncScheduler`).
- **State Isolation**: Each execution layer maintains its own isolated context, preventing state interference between different scheduling levels.
- **Controlled Communication**: Layers communicate through well-defined interfaces, ensuring that isolation is maintained.
- **Flexible Composition**: Developers can mix and match execution strategies (e.g., synchronous and asynchronous) at any layer to fit their needs.

## ✨ Features

- **Expressive Functional Paradigm**: Embraces higher-order functions, closures, pattern matching, and composition as first-class citizens.
- **Generator-based Asynchronous Core**: The VM is built on generators, providing native, efficient support for asynchronous operations (`async`, `pool`).
- **GIL-Free Multi-threading**: True parallel execution with native threads (`launch`, `valueof`), free from a Global Interpreter Lock.
- **Safe & Explicit Memory Management**: Fine-grained control over mutability (`mut`, `const`) and reference safety to prevent common bugs.
- **Powerful Metaprogramming**: A compile-time macro system (`@def`, `@ast`) allows for syntax extension and code generation.
- **Advanced Parameter Constraints**: Define custom validation rules directly in function signatures for robust, self-documenting APIs.
- **Lazy Collections and Streams**: Built-in support for lazy evaluation and efficient stream processing using the `|` and `|>` operators.
- **Built-in Interface System**: A flexible, prototype-based system for defining and implementing contracts.
- **Rich Data Types and Literals**: A comprehensive set of built-in types and an enhanced syntax for strings, tuples, and collections.
- **High Performance**: The Onion VM is optimized for speed. For example, a 10 million iteration `while` loop completes in approximately 1.17s, about 40% faster than the same loop in RustPython (1.91s).
- **Excellent Tooling**: Includes a command-line interface, REPL, and Language Server Protocol (LSP) support for a modern development experience.

## 🚀 Quick Start

### Installation

Ensure you have the Rust toolchain installed, then clone and build the project:

```bash
git clone https://github.com/sjrsjz/onion-lang.git
cd onion-lang
cargo install --path .
```

### Hello, World!

Create a file named `hello.onion`:

```onion
@required 'stdlib';

main := () -> {
    stdlib.io.println("Hello, Onion World!");
};

main();
```

Run it from your terminal:

```bash
onion run hello.onion
```

## 🚀 Language Tour

Explore the core features of Onion through these examples.

### 1. Data Types & Literals

Onion supports a rich set of data types with a flexible literal syntax.

```onion
@required 'stdlib';

// --- Strings ---
stdlib.io.println("Standard double-quoted string");
stdlib.io.println('Standard single-quoted string');
stdlib.io.println("Escapes for newlines \\n and tabs \\t");
stdlib.io.println(R"(Raw strings don't process escapes like \n)");
stdlib.io.println("""Supports multi-line strings with ease""");
stdlib.io.println("String " + "concatenation is simple.");
stdlib.io.println("Unicode support: \u6d0b\u8471"); // "洋葱"

// --- Numbers, Booleans, and Nulls ---
stdlib.io.println("Integer:", 42);
stdlib.io.println("Hex:", 0x2A);
stdlib.io.println("Float:", 3.14);
stdlib.io.println("Boolean:", true);
stdlib.io.println("Null:", null);

// --- Tuples ---
tuple := (1, "hello", true);
stdlib.io.println("Tuple:", tuple);
stdlib.io.println("Tuple length:", tuple.length());

// --- Type Conversion ---
num_from_string := "42".int();
stdlib.io.println("Converted to integer:", num_from_string);
```

### 2. Functions & Closures

Functions are the cornerstone of Onion. The syntax is minimal, powerful, and supports closures that capture their environment.

```onion
@required 'stdlib';

// --- Basic Definition and Calls ---
// f(x), f x, and f[x] are equivalent calls
square := (x?) -> x * x;
stdlib.io.println("Square of 5 is:", square 5);

// Functions can accept tuples as arguments, which are automatically unpacked
add := (x?, y?) -> x + y;
stdlib.io.println("Sum of 3 and 4 is:", add(3, 4));

// --- Closures and Captured Variables ---
x := 10;
y := mut 20;

// This function automatically captures `x` and `y` from its environment
f := () -> {
    y = 30; // Modifies the captured `y`
    stdlib.io.println("f executed. y is now:", y);
};

f();
stdlib.io.println("After f, global y is:", y); // Prints 30

// You can also explicitly declare captured variables
g := () -> &[x, y] {
    y = 40;
    stdlib.io.println("g executed. y is now:", y);
};

g();
stdlib.io.println("After g, global y is:", y); // Prints 40
```

### 3. Mutability & References

Onion provides explicit control over mutability to enhance program safety.

```onion
@required 'stdlib';

obj := [
    mut 0, // This element is mutable
    1,     // This element is immutable
];

// Modification is wrapped in a sync block to manage concurrent access safely
(sync () -> { obj[0] = 42; })(); // Success
stdlib.io.println("obj[0] has been changed to:", obj[0]);

// Attempting to modify an immutable element will fail at runtime
(sync () -> { obj[1] = 100; })(); // Fails!
stdlib.io.println("obj[1] remains:", obj[1]);

// A reference to a mutable value can change it
ref := obj[0];
ref = 99;
stdlib.io.println("obj[0] is now:", obj[0]); // Prints 99

// A const reference cannot be modified
const_data := const obj[0];
// (sync () -> { const_data = 100; })(); // Fails!
```

### 4. Parameter Constraints (Guards)

Enforce contracts at the function boundary with parameter constraints, making code more robust and self-documenting.

```onion
@required 'stdlib';

// Define constraint functions (guards)
Positive := (x?) -> x > 0;
NonEmpty := (s?) -> stdlib.string.length(s) > 0;

// Apply guards to function parameters
add_positives := (a => Positive, b => Positive) -> a + b;
greet := (name => NonEmpty) -> "Hello, " + name + "!";

// Valid calls
stdlib.io.println(add_positives(5, 3)); // 8
stdlib.io.println(greet("Onion"));      // "Hello, Onion!"

// Invalid calls will raise a runtime error
// add_positives(-1, 5); // Throws an error
// greet("");            // Throws an error
```

### 5. Lazy Collections & Streams

Process collections of data efficiently with lazy filters and maps. The `|` operator creates a lazy filtered set, while `|>` creates a lazy mapped stream.

```onion
@required 'stdlib';

// Lazy filtering: the predicate is evaluated only when an element is accessed
small_numbers := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] | (x?) -> x < 5;
stdlib.io.println("Is 3 in the set?", 3 in small_numbers); // true
stdlib.io.println("Is 7 in the set?", 7 in small_numbers); // false

// To get a concrete list, use .collect()
collected := small_numbers.collect();
stdlib.io.println("Collected small numbers:", collected);

// Lazy mapping with the stream operator |>
squared_stream := [1, 2, 3, 4, 5] |> (x?) -> x * x;
stdlib.io.println("Stream of squared numbers:", squared_stream);
```

### 6. Interfaces (Prototypes)

Onion uses a prototype-based interface system to define and check object capabilities.

```onion
@required 'stdlib';

// Factory for creating new interface types
interface := (interface_definition?) -> (
    new => (structure?) -> structure : mut interface_definition,
    check => (instance?) -> (valueof instance) is (valueof interface_definition),
);

// Define an interface contract
Printable := interface {
    print => () -> stdlib.io.println(self.data),
};

// Create an instance that fulfills the contract
my_instance := Printable.new {
    data => "This is my data!",
};

my_instance.print(); // "This is my data!"

// Check if an instance conforms to an interface
stdlib.io.println("Is instance of Printable?", Printable.check(my_instance)); // true

// This check can be used as a function parameter constraint
print_if_printable := (p => Printable.check) -> {
    p.print();
};

print_if_printable(my_instance); // Works
```

### 7. Asynchronous Programming

The `async` and `pool` keywords provide a powerful way to manage concurrent tasks.

```onion
@required 'stdlib';

// A pool defines a generator of tasks
pool := () -> {
    return (0..5).elements() |> (x?) -> {
        stdlib.io.println("Scheduling task for:", x);
        stdlib.time.sleep_seconds(0.1);
        return spawn () -> { // `spawn` creates an asynchronous computation
            stdlib.io.println("Started async processing for:", x);
            stdlib.time.sleep_seconds(1);
            stdlib.io.println("Finished async processing for:", x);
            return x * x;
        };
    };
};

// `async` executes the pool, creating a handle to the group of running tasks
tasks_handle := (async pool)();
stdlib.io.println("All tasks have been scheduled.");

// `valueof` blocks until all tasks in the handle complete and returns their results
results := valueof tasks_handle;
stdlib.io.println("All tasks completed. Results:", results);
```

### 8. Multi-threading

For CPU-bound work, `launch` starts a new OS thread, enabling true parallelism.

```onion
@required 'stdlib';

// Define two functions to be run in parallel
thread1 := () -> {
    stdlib.io.println("Thread 1: Starting to compute...");
    stdlib.time.sleep_seconds(2);
    return "Result from Thread 1";
};

thread2 := () -> {
    stdlib.io.println("Thread 2: Starting to compute...");
    stdlib.time.sleep_seconds(1);
    return "Result from Thread 2";
};

// `launch` starts each function in a new thread and returns a handle
handle1 := launch thread1;
handle2 := launch thread2;

stdlib.io.println("Threads launched. Waiting for results...");

// `valueof` blocks on a handle and retrieves the return value
result1 := valueof handle1;
result2 := valueof handle2;

stdlib.io.println("Thread 1 Result:", result1);
stdlib.io.println("Thread 2 Result:", result2);
```

### 9. Advanced Data Handling (JSON)

The standard library includes a robust JSON module for real-world data manipulation.

```onion
@required 'stdlib';

config_text := '{
    "database": {
        "host": "localhost",
        "port": 5432,
        "users": ["admin", "guest"]
    },
    "logging": {
        "level": "info"
    }
}';

// Parse a JSON string into an Onion object
parsed_config := stdlib.json.parse(config_text);

stdlib.io.println("Database host:", parsed_config.database.host); // localhost
stdlib.io.println("Log level:", parsed_config.logging.level);   // info

// Modify the object and stringify it back to JSON with pretty printing
parsed_config.logging.level = "debug";
pretty_json := stdlib.json.stringify_pretty(parsed_config);

stdlib.io.println("Updated Config JSON:\n", pretty_json);
```

### 10. Metaprogramming

Onion's macro system operates at compile-time, allowing you to define new keywords and transform code before execution.

```onion
@required 'stdlib';

// @def defines a simple constant expression macro
@def(add => (x?, y?) -> x + y);
const_value := @add(1, 2); // This is replaced with 3 at compile time
stdlib.io.println("has add macro defined:", @ifdef "add");
stdlib.io.println("const_value:", const_value);
@undef "add";

// @ast lets you build Abstract Syntax Trees programmatically
lambda := @ast.lambda_def(false, ()) << (
    ("x", "y"),
    ast.operation("+") << (
        ast.variable("x"),
        ast.variable("y")
    )
);

// The `lambda` variable is now a function equivalent to `(x?, y?) -> x + y`
stdlib.io.println("AST-generated lambda(10, 20) =", lambda(10, 20));
```

<details>
<summary><b>🔬 Advanced Example: Functional Purity with Church Numerals</b></summary>

Onion's functional capabilities are expressive enough to implement theoretical computer science concepts like the Lambda Calculus directly. This example encodes numbers and arithmetic using only functions.

```onion
@required 'stdlib';

// --- Church Numeral Encoding (Numbers as functions) ---
zero := (f?) -> (x?) -> x;
one := (f?) -> (x?) -> f(x);
two := (f?) -> (x?) -> f(f(x));

// --- Operations (Functions that operate on other functions) ---
// Successor: succ(n) = n + 1
succ := (n?) -> (f?) -> (x?) -> f(n(f)(x));

// Addition: add(m, n) = m + n
add := (m?) -> (n?) -> (f?) -> (x?) -> m(f)(n(f)(x));

// Multiplication: mult(m, n) = m * n
mult := (m?) -> (n?) -> (f?) -> m(n(f));

// --- Helper to convert a Church numeral to a regular number ---
to_number := (church_num?) -> church_num((x?) -> x + 1)(0);

// --- Tests ---
four := succ(add(one)(two));
stdlib.io.println("one + two =", to_number(add(one)(two))); // 3
stdlib.io.println("two * (one + two) =", to_number(mult(two)(add(one)(two)))); // 6
```
</details>

## 🛠️ Command Line Tools

Onion ships with a complete toolchain:

```bash
# Run a source code file
onion run file.onion

# Compile a file to bytecode
onion compile file.onion -o file.onionc

# Start the interactive Read-Eval-Print Loop (REPL)
onion repl

# Start the Language Server for IDE integration
onion lsp
```

## 🏗️ Project Structure

```
onion-lang/
├── src/                    # Main program source code
│   ├── main.rs            # CLI entry point
│   ├── repl/              # Interactive interpreter
│   ├── lsp/               # Language Server Protocol
│   └── stdlib/            # Standard library source
├── onion-frontend/         # Compilation frontend
│   └── src/
│       ├── parser/        # Lexer and parser
│       │   └── comptime/  # Compile-time solver
│       ├── utils/         # Utility functions
│       └── ir_generator/  # Intermediate Representation generator
├── onion-vm/               # Virtual Machine runtime
│   └── src/
│       ├── lambda/        # Lambda computation and scheduler
│       ├── utils/         # VM utilities
│       └── types/         # Type system implementation
└── examples/               # Example Onion code
```

## 🎯 Design Philosophy

1.  **Safety First**: Prevent common errors through explicit mutability, reference controls, and a strong interface system.
2.  **Maximum Expressiveness**: Provide developers with the tools (functional, asynchronous, metaprogramming) to write concise, powerful, and elegant code.
3.  **Developer-Friendly**: A modern toolchain, including LSP for IDEs and a simple command-line interface, is a core part of the project.

## 🔧 Development Status

Onion is an experimental but rapidly developing project.

- ✅ Core syntax and dynamic type system
- ✅ Functional programming features (closures, HOFs)
- ✅ Layered asynchronous execution model (`async`, `pool`, `spawn`)
- ✅ GIL-free multi-threading (`launch`, `valueof`)
- ✅ Advanced parameter constraints (guards)
- ✅ Rich data types and enhanced string literals
- ✅ Lazy collections and stream processing
- ✅ Prototype-based interface system
- ✅ Metaprogramming and macro system (`@def`, `@ast`)
- ✅ Robust standard library (JSON, time, io)
- ✅ Language Server Protocol (LSP) for IDE support
- ✅ Performance optimizations
- ✅ CLI tools (run, compile, repl)
- 🚧 Expanded standard library
- 🚧 Refined documentation and toolchain

## 📄 License

This project is licensed under the [MIT License](LICENSE).

## 🔗 Related Links

- [Language Specification](docs/language-spec.typ)
- [Examples Folder](examples/)