#import "@preview/zebraw:0.5.5": *
#show raw: zebraw
#show raw.where(block: false): it => box(it, fill: luma(230), outset: (x: 0.25em, y: 0.35em))
#set text(font: ("JetBrains Mono", "Microsoft YaHei UI"))

= Onion 语言文档


= 原子表达式

原子表达式是 Onion 语言语法分析中的基本单元，它将输入的 token 流分组为可独立处理的语法单元。

== 定义

*原子表达式组* 是满足以下条件的 token 序列：

1. *括号平衡*：组内所有的 `{}`、`[]`、`()` 都完全匹配
2. *最小完整性*：从当前位置开始，当所有括号栈为空时结束
3. *独立性*：每个组可以独立进行语法分析，不依赖其他组

== 术语定义

在后续语法描述中，我们使用以下术语：

- `<atom>` - 表示一个原子表达式组
- `<atoms>` - 表示多个原子表达式组的序列
- `<atom-expr>` - 表示原子表达式（同 `<atom>`）

== 示例

```onion
// 输入表达式
a + (b * c) + {x: y} [1, 2]

// 分组结果（原子表达式组）
[a]        // 组1: 单个标识符
[+]        // 组2: 运算符
[(b * c)]  // 组3: 括号表达式（完整的乘法运算）
[+]        // 组4: 运算符
[{x: y}]   // 组5: 对象字面量（完整的键值对）
[[1, 2]]   // 组6: 数组字面量（完整的数组定义）
```

```onion
// 嵌套括号示例
f({a: [1, 2, (x + y)]})

// 分组结果
[f]                      // 组1: 函数名
[({a: [1, 2, (x + y)]})] // 组2: 完整的函数调用参数
```

== 错误处理

当括号不匹配时，会产生 `UnmatchedParenthesis` 错误：

```rust
// 错误示例
a + (b * c]  // 括号类型不匹配
{x: y        // 缺少闭括号
```
= 类型系统

Onion 语言的类型系统在Rust中实现为一个枚举类型，包含了所有的基本类型和复合类型。每个类型都对应于一个枚举变体。
```rust
pub enum OnionObject {
    // immutable basic types
    Integer(i64),
    Float(f64),
    String(Arc<String>),
    Bytes(Arc<Vec<u8>>),
    Boolean(bool),
    Range(i64, i64),
    Null,
    Undefined(Option<Arc<String>>),
    InstructionPackage(Arc<VMInstructionPackage>),

    Tuple(Arc<OnionTuple>),
    Pair(Arc<OnionPair>),
    LazySet(Arc<OnionLazySet>),
    Lambda(Arc<OnionLambdaDefinition>),
    Custom(Arc<dyn OnionObjectExt>),

    Mut(GCArcWeak<OnionObjectCell>),
}

```
其中各自在 Onion 语言中的定义如下：
- `Integer`: 整数类型，64 位有符号整数。语法：`42`, `-123`
- `Float`: 浮点数类型，64 位双精度浮点数。语法：`3.14`, `-0.5`
- `String`: 字符串类型，UTF-8 编码的字符串。语法：`"hello"`, `'world'`, `"""multiline string"""`
  - 支持转义字符，如 `\"`, `\\`, `\n` 等
  - 直接支持多行字符串，可以不显著使用 `\n` 换行
- `Bytes`: 字节数组类型，存储原始字节数据。语法：`$"base64"`
- `Boolean`: 布尔类型，表示真或假。语法：`true` 或 `false`
- `Range`: 范围类型，表示一个整数范围，包含起始和结束值。语法：`<start-atom>..<end-atom>`，例如 `1..10`
- `Null`: 空类型，表示无值。语法：`null`
- `Undefined`: 未定义类型，表示一个未初始化的值，可以包含一个可选的错误信息。语法：`undefined`
- `Tuple`: 元组类型，包含多个值的有序集合。语法：`<atom>, <atom>, ...`，例如 `1, "hello", true`
- `Pair`: 键值对类型，包含一个键和一个值。语法：`<key-atom> : <value-atom>` 或 `<key-atom> => <value-atom>`，例如 `"name" : "Alice"` `name => "Alice"`，注意 `=>` 会自动将键为变量名的转换为字符串
- `LazySet`: 懒加载集合类型。语法：`<container-atom> | <filter-atom>`，例如 `numbers | x > 5`
- `InstructionPackage`: 指令包类型，包含一组指令和相关数据。语法：`import "path/to/package"`
- `Lambda`: Lambda 函数类型，表示一个匿名函数。语法：
  - `<params-atom> -> <body-atom>` （普通 lambda）
  - `<params-atom> -> &<capture-atom> <body-atom>` （带捕获的 lambda）
- `Custom`: 自定义类型，表示由宿主环境定义的类型，可以是任何实现了 `OnionObjectExt` trait 的类型。
- `Mut`: 可变引用类型，表示*将值装载到堆上（受GC控制管理生命周期）并获得可变引用*。语法：`mut <atoms>`，例如 `mut x`。如果要卸载对象并获得不可变值，可以使用 `const <atoms>`，例如 `const x`。*可变对象一定是在堆上分配的*。

= 表达式
Onion 语言是表达式导向的编程语言，所有的代码都是由表达式组成的。
`;` 用于将子表达式组合成顺序执行的表达式组，该表达式执行的结果是最后一个子表达式的结果。
```onion
x := (
  1; 2
);// 结果: 2

y := (
  3; x + 2;
);// 结果: undefined，因为末尾是空表达式
```

= 运算符

== 优先级改变
可以使用 `()` 或 `[]` 来改变运算符的优先级。Onion 语言在改变优先级的语义上不区分 `()` 和 `[]`，它们都可以用于改变运算符的优先级。

`{}` 也可以用于改变运算符的优先级，但它通常用于表示一个新的作用域，而不是单纯的优先级改变。

```onion
// 使用 () 改变优先级
(2 + 3) * 4      // 结果: 20
// 使用 [] 改变优先级
[2 + 3] * 4      // 结果: 20
// 使用 {} 改变优先级
{2 + 3} * 4      // 结果: 20
// 使用 () 和 [] 混合
(2 + [3 * 4])    // 结果: 14
```

== 算术运算符

Onion 语言支持以下算术运算符：

=== 加法运算符 `+`

加法运算符用于执行数值相加、字符串连接、字节数组连接等操作。

*支持的类型组合：*
- `Integer + Integer` → `Integer`
- `Float + Float` → `Float`
- `Integer + Float` → `Float`
- `Float + Integer` → `Float`
- `String + String` → `String` (字符串连接)
- `Bytes + Bytes` → `Bytes` (字节数组连接)
- `Range + Range` → `Range` (范围相加)
- `Tuple + Tuple` → `Tuple` (元组相加)

*示例：*
```onion
// 数值运算
42 + 8          // 结果: 50
3.14 + 2        // 结果: 5.14
1.5 + 2.5       // 结果: 4.0

// 字符串连接
"Hello" + " World"  // 结果: "Hello World"

// 字节数组连接
$"SGVsbG8=" + $"V29ybGQ="  // 结果: 连接后的字节数组

// 范围相加
1..5 + 10..15   // 结果: 11..20

// 元组相加
(1, 2) + (3, 4)  // 结果: (1, 2, 3, 4)
```

=== 减法运算符 `-`

减法运算符用于数值相减操作。

*支持的类型组合：*
- `Integer - Integer` → `Integer`
- `Float - Float` → `Float`
- `Integer - Float` → `Float`
- `Float - Integer` → `Float`

*示例：*
```onion
100 - 25        // 结果: 75
5.5 - 2.3       // 结果: 3.2
10 - 3.5        // 结果: 6.5
```

=== 乘法运算符 `*`

乘法运算符用于数值相乘操作。

*支持的类型组合：*
- `Integer * Integer` → `Integer`
- `Float * Float` → `Float`
- `Integer * Float` → `Float`
- `Float * Integer` → `Float`

*示例：*
```onion
6 * 7           // 结果: 42
2.5 * 4         // 结果: 10.0
3.14 * 2.0      // 结果: 6.28
```

=== 除法运算符 `/`

除法运算符用于数值相除操作。

*支持的类型组合：*
- `Integer / Integer` → `Integer` (整数除法)
- `Float / Float` → `Float`
- `Integer / Float` → `Float`
- `Float / Integer` → `Float`

*示例：*
```onion
15 / 3          // 结果: 5
10.0 / 4.0      // 结果: 2.5
7 / 2.0         // 结果: 3.5

// 错误示例
5 / 0           // 错误: Division by zero
```

=== 取模运算符 `%`

取模运算符用于获取除法的余数。

*支持的类型组合：*
- `Integer % Integer` → `Integer`
- `Float % Float` → `Float`
- `Integer % Float` → `Float`
- `Float % Integer` → `Float`

*示例：*
```onion
10 % 3          // 结果: 1
7.5 % 2.0       // 结果: 1.5
15 % 4.5        // 结果: 1.5
```

=== 幂运算符 `**`

幂运算符用于数值的幂次方运算。

*支持的类型组合：*
- `Integer ** Integer` → `Integer`
- `Float ** Float` → `Float`
- `Integer ** Float` → `Float`
- `Float ** Integer` → `Float`

*示例：*
```onion
2 ** 8          // 结果: 256
2.0 ** 3.0      // 结果: 8.0
4 ** 0.5        // 结果: 2.0 (平方根)
```

== 位运算符

位运算符用于对整数进行位级操作。

=== 按位与 `and`

*支持的类型组合：*
- `Integer and Integer` → `Integer`
- `Boolean and Boolean` → `Boolean` (逻辑与)

*示例：*
```onion
12 and 10         // 结果: 8 (1100 and 1010 = 1000)
true and false    // 结果: false
```

=== 按位或 `or`

*支持的类型组合：*
- `Integer or Integer` → `Integer`
- `Boolean or Boolean` → `Boolean` (逻辑或)

*示例：*
```onion
12 or 10         // 结果: 14 (1100 or 1010 = 1110)
true or false    // 结果: true
```

=== 按位异或 `xor`

*支持的类型组合：*
- `Integer xor Integer` → `Integer`

*示例：*
```onion
12 xor 10         // 结果: 6 (1100 xor 1010 = 0110)
```

=== 左移 `<<`

*支持的类型组合：*
- `Integer << Integer` → `Integer`

*示例：*
```onion
5 << 2          // 结果: 20 (101 << 2 = 10100)
```

=== 右移 `>>`

*支持的类型组合：*
- `Integer >> Integer` → `Integer`

*示例：*
```onion
20 >> 2         // 结果: 5 (10100 >> 2 = 101)
```

== 一元运算符

=== 负号 `-`

对数值取负。

*支持的类型：*
- `-Integer` → `Integer`
- `-Float` → `Float`

*示例：*
```onion
-42             // 结果: -42
-3.14           // 结果: -3.14
```

=== 正号 `+`

对数值取绝对值。

*支持的类型：*
- `+Integer` → `Integer` (绝对值)
- `+Float` → `Float` (绝对值)

*示例：*
```onion
+(-42)          // 结果: 42
+(-3.14)        // 结果: 3.14
```

=== 逻辑非 `not`

对布尔值或整数取反。

*支持的类型：*
- `not Boolean` → `Boolean`
- `not Integer` → `Integer` (按位取反)

*示例：*
```onion
not true           // 结果: false
not false          // 结果: true
not 5              // 结果: -6 (按位取反)
```

== 比较运算符

=== 等于 `==`

检查两个值是否相等。支持所有类型的比较，包括跨类型的数值比较。

*示例：*
```onion
42 == 42        // 结果: true
3.0 == 3        // 结果: true (跨类型比较)
"hello" == "hello"  // 结果: true
```

=== 小于 `<`

数值大小比较。

*支持的类型组合：*
- `Integer < Integer` → `Boolean`
- `Float < Float` → `Boolean`
- `Integer < Float` → `Boolean`
- `Float < Integer` → `Boolean`

*示例：*
```onion
5 < 10          // 结果: true
3.14 < 3        // 结果: false
```

=== 大于 `>`

数值大小比较。

*支持的类型组合：*
- `Integer > Integer` → `Boolean`
- `Float > Float` → `Boolean`
- `Integer > Float` → `Boolean`
- `Float > Integer` → `Boolean`

*示例：*
```onion
10 > 5          // 结果: true
2.5 > 3         // 结果: false
```

== 类型转换规则

当不同数值类型参与运算时，遵循以下转换规则：

1. `Integer` 与 `Float` 运算时，`Integer` 自动转换为 `Float`
2. 结果类型为参与运算的最宽类型（`Float` > `Integer`）
3. 字符串和字节数组不参与自动类型转换

== 错误处理

运算符可能产生以下运行时错误：

- `Division by zero` - 除零错误
- `InvalidOperation` - 不支持的运算类型组合
- `BrokenReference` - 可变引用已失效（理论上不可能发生）
- `InvalidType` - 无效的类型操作

= 作用域
作用域是 Onion 语言中用于管理变量和函数可见性的机制。它定义了变量和函数的生命周期以及它们在代码中的可访问范围。
作用域分为以下几种类型：
- *函数作用域*：每个函数都有自己的作用域，函数内部定义的变量在函数外部不可见。
- *帧作用域*：使用 `{}` 包围的表达式会创建一个新的作用域，变量在该作用域内可见，但在外部不可见。

*注*：
- Onion 不存在全局作用域，所有变量和函数都必须在某个作用域内定义。允许跨帧作用域但不允许跨函数作用域访问变量。
- “跨函数作用域访问变量”是 Onion 语言前端通过静态分析AST并尝试在参数中隐式传递变量来实现的。Onion 语言不存在动态作用域。
- 帧作用域从不被隐式创建，只有在显式使用 `{}` 包围代码块时才会创建。
- 函数作用域在函数调用时创建，并在函数返回时销毁。

== 变量
一旦新的作用域被创建，所有在该作用域内定义的变量都将被添加到该作用域的上下文中。

变量定义使用 `:=` 语法，表示在当前作用域内*遮蔽*创建一个新的变量。
```onion
// 定义变量
x := 42;
x := "hello"; // 在同一作用域内重新定义变量 x，类型从 Integer 变为 String
```

函数的实参也会直接绑定到当前函数作用域，因此实参也能被遮蔽。

=== 可变性
在 Onion 语言中，变量默认是不可变的。如果需要创建一个“可变”变量，可以使用 `mut` 关键字。
```onion
// 定义一个可变变量
x := mut 42;
// 替换可变变量的值
x = 100; // 允许替换

// 定义一个不可变变量
y := 50; // 默认不可变
// y := const 50; // 结果定价于上面的定义，不需要显式使用 const
// 尝试修改不可变变量会导致错误
y = 60; // 错误: Cannot assign to immutable variable
```

`mut` 关键字用于创建一个可变变量，允许在后续代码中修改其值。可变变量的生命周期由垃圾回收器管理。

当值被 `mut` 修饰时，Onion 语言会将其存储在堆上，并返回一个隐式的可变引用。

如果需要将可变变量转换为不可变变量，可以使用 `const` 修饰符。该操作会拷贝当前值并返回一个不可变的共享。

```onion
// 可变元组
x := mut (1, 2, 3); // 创建一个可变元组，让其移动到堆上并获得一个mut指针
// x[0] = 10; // panic!，元组元素不可变，无法直接修改
x = (10, 2, 3); // 允许替换mut指向的槽位的值

x := (mut 1, 2, 3); // 创建一个不可变元组，但第一个元素是可变的
x[0] = 10; // 允许替换第一个元素的值
// x = (10, 2, 3); // panic!，无法替换x的值，因为x本身是不可变的
```

=== 幂等律

Onion VM 在对象设计上严格遵循不可变性，`mut` 关键字用于创建一个指针，其值指向一个被GC管理的堆对象。而 `const` 关键字用于创建一个栈上的不可变对象。

为了保证幂等性，Onion VM的设计确保了以下几点：
- mut (mut x) 等价于 mut x
- const (const x) 等价于 const x

这意味着
- `mut` 操作符: 接受一个值。如果该值已经是 mut 引用，则直接返回该引用（幂等律）。如果该值是不可变的，则创建一个新的 mut 引用，使其指向该值的共享，并返回这个新的 mut 引用。
- `const` 操作符: 接受一个值。如果该值已经是不可变的，则直接返回该值（幂等律）。如果该值是一个 mut 引用，则读取该引用指向的值，并返回该值的共享。

*共享*是因为OnionObject内部的`Arc`设计，它允许多个引用共享同一个堆对象，而不会导致数据的意外修改。在 `const` 和 `mut` 的过程中，原始对象从不产生副本，产生副本的是对原始对象的引用（除了数值类型）

尽管如此，不应当认为 `mut` 是可变对象，其本质是一个地址不可变的指针，指向一个允许被*替换*的堆对象。应当将 `mut` 理解为一个万能容器。

幂等律严格确保了堆上对象不可能存在直接的 `mut` 容器，如果出现了 `mut` 容器，则VM会直接抛出内部逻辑异常的错误。

同时幂等律和不可变性相结合，确保了在任何情况下，原始值都不会被意外修改。并且允许以极高的效率共享原始值。

== 元组
元组是 Onion 语言中的一种复合数据类型，用于存储多个值的有序集合。元组可以包含不同类型的值，并且支持嵌套。
Onion 的元组定义和 Python 一致，不需要使用括号包围。
尽管如此，由于运算符的优先级，元组在大部分情况下需要使用括号来确保正确的解析顺序。

```onion
// 定义一个元组
x := (1, 2, "hello", true); // 包含整数、字符串和布尔值
```

空元组可以使用 `()` 或 `[]` 来定义。单值元组则使用 `value,` 来定义。

== 键值对
键值对是 Onion 语言中的一种数据结构，用于存储关联的键和值。它们可以用于表示原型对象、字典等数据结构。
键值对可以使用 `:` 或 `=>` 来定义，键可以是任意原子表达式，值可以是任意原子表达式组。

```onion
// 定义一个键值对
pair := "name" : "Alice"; // 键是字符串，值是字符串
```

可以使用 `keyof pair` 来获取键的值，使用 `valueof pair` 来获取值的值。
```onion
// 定义一个键值对
pair := "name" : "Alice"; // 键是字符串，值是字符串
// 获取键和值
key := keyof pair; // 结果: "name"
value := valueof pair; // 结果: "Alice"
```

当元组和键值对混合使用时，可以表示字典
```onion
// 定义一个字典
dict := ("name" : "Alice", "age" : 30, "is_student" : false); // 包含多个键值对
```

可以使用 `tuple.key` 来查找元组中的键值对中键和 `key` 的值相同的键值对的值。默认情况下， `key` 被转换为字符串等价于 `tuple."key"`。使用 `{}` 可以阻止这种转换，如 `tuple.{key}` 会将 `key` 作为原子表达式组处理，而不是字符串。单纯的使用括号改变优先级不会影响默认的字符串转换。
```onion
// 定义一个元组
tuple := ("name" : "Alice", "age" : 30, "is_student" : false);
name := tuple.name; // 结果: "Alice"
age := tuple.age; // 结果: 30
// 使用 {} 阻止字符串转换
name := tuple.{
  A := "name";
  A
};
```

可以使用 `pair.key` 来视键值对为原型对象，其会先尝试查找值的匹配值，如果没有找到，则会查找键的匹配值。
```onion
// 定义一个键值对，其被视为原型对象
obj := (
    "name" : "Alice",
    "age" : 30,
    "is_student" : false
) : (
    "info" : "This is a prototype object.",
);
info := obj.info; // 结果: "This is a prototype object."
name := obj.name; // 结果: "Alice"
```

*注*
通过 `obj.key` 访问得到的不可变函数会将 `obj` 自身绑定到函数的 `self` 上
```onion
obj := {
  name => "Alice",
} : {
  greet => () -> "Hello, " + self.name,
};
greeting := obj.greet(); // 结果: "Hello, Alice"
```

Onion 提供两种语法糖：`x?` 表示 `x => true`，用于在函数定义时表示永真约束。

= 函数定义与调用

== 自动捕获
Onion 语言会尝试将函数作用域内未定义但被引用的变量通过隐式参数定义的方式传递给函数。
```onion
x := 42;
foo := () -> {
    // 自动捕获 x
    x + 1
};

// 上述函数定义等价如下定义
foo := () -> &x {
    x + 1
};
```

== this 变量
Onion 语言中的 `this` 变量的值为调用当前函数的调用者调用的函数对象（不会解引用，保留可变性）。

可以通过 `this` 来递归调用函数或访问函数自身的属性。

== self 变量
Onion 语言中的 `self` 变量的值由成员访问操作符 `.` 绑定到当前对象的值。
可以通过 `self` 来访问当前对象的属性或方法。

== parameter 属性
Onion 语言中的 `$parameter` 属性用于获取函数形参元组

== signature 属性
Onion 语言中的 `$signature` 属性用于获取函数的签名字符串

== 被捕获的变量
当键不为 `$parameter`、`$signature` 时，`f.key` 会返回函数 `f` 中被捕获的名称为 `key` 的变量的值。