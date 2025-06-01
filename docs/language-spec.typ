#import "@preview/zebraw:0.5.5": *
#show raw: zebraw

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
    Integer(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    Boolean(bool),
    Range(i64, i64),
    Null,
    Undefined(Option<String>),
    Tuple(OnionTuple),
    Pair(OnionPair),
    Named(OnionNamed),
    LazySet(OnionLazySet),
    InstructionPackage(VMInstructionPackage),
    Lambda(OnionLambdaDefinition),
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
- `Pair`: 键值对类型，包含一个键和一个值。语法：`<key-atom> : <value-atom>`，例如 `name : "Alice"`
- `Named`: 命名键值对类型，包含一个键和一个值。语法：`<key-atom> => <value-atom>`，例如 `name => "Alice"`。其中 `<key-atom>` 如果是变量名则默认解析成字符串（即使没有引号），也可以用 `{}` 阻止字符串化
- `LazySet`: 懒加载集合类型。语法：`<container-atom> | <filter-atom>`，例如 `numbers | x > 5`
- `InstructionPackage`: 指令包类型，包含一组指令和相关数据。语法：`import "path/to/package"`
- `Lambda`: Lambda 函数类型，表示一个匿名函数。语法：
  - `<params-atom> -> <body-atom>` （普通 lambda）
  - `<params-atom> -> &<capture-atom> <body-atom>` （带捕获的 lambda）
- `Mut`: 可变引用类型，表示*将值装载到堆上（受GC控制管理生命周期）并获得可变引用*。语法：`mut <atoms>`，例如 `mut x`。如果要卸载对象并获得不可变值，可以使用 `const <atoms>`，例如 `const x`。*可变对象一定是在堆上分配的*。



