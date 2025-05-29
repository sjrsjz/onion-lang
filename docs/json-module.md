# JSON 模块文档

JSON 模块为 Onion 编程语言提供了 JSON 数据格式的解析和序列化功能。

## 模块导入

```onion
@required stdlib;
```

## 可用函数

### `stdlib.json.parse(json_string => string)`

将 JSON 字符串解析为 Onion 对象。

**参数：**
- `json_string`: 要解析的 JSON 字符串

**返回值：**
- 解析后的 Onion 对象
- JSON 对象会被转换为包含 Pair 的 Tuple（字典结构）
- JSON 数组会被转换为 Tuple
- JSON 基本类型（string, number, boolean, null）会被转换为对应的 Onion 类型

**示例：**
```onion
// 解析 JSON 对象
obj := stdlib.json.parse(json_string => '{"name": "Alice", "age": 30}');
// 结果: (age : 30, name : Alice)

// 解析 JSON 数组
arr := stdlib.json.parse(json_string => '[1, "hello", true]');
// 结果: (1, hello, true)
```

### `stdlib.json.stringify(object => object)`

将 Onion 对象序列化为 JSON 字符串。

**参数：**
- `object`: 要序列化的 Onion 对象

**返回值：**
- JSON 字符串

**对象转换规则：**
- 包含 Pair 的 Tuple 会被转换为 JSON 对象
- 其他 Tuple 会被转换为 JSON 数组
- 基本类型保持对应转换

**示例：**
```onion
// 序列化对象
obj := {"name": "Bob", "score": 95.5};
json_str := stdlib.json.stringify(object => obj);
// 结果: {"name":"Bob","score":95.5}

// 序列化数组
arr := [1, 2, 3];
json_str := stdlib.json.stringify(object => arr);
// 结果: [1,2,3]
```

### `stdlib.json.stringify_pretty(object => object)`

将 Onion 对象序列化为格式化的 JSON 字符串。

**参数：**
- `object`: 要序列化的 Onion 对象

**返回值：**
- 格式化的 JSON 字符串（包含缩进和换行）

**示例：**
```onion
obj := {"name": "Alice", "details": {"age": 30, "city": "Beijing"}};
pretty_json := stdlib.json.stringify_pretty(object => obj);
// 结果:
// {
//   "details": {
//     "age": 30,
//     "city": "Beijing"
//   },
//   "name": "Alice"
// }
```

## 完整示例

```onion
@required stdlib;

// 创建一个对象
person := {
    "name": "张三",
    "age": 25,
    "skills": ["JavaScript", "Onion", "Rust"],
    "employed": true
};

// 序列化为 JSON
json_str := stdlib.json.stringify(object => person);
stdlib.io.println("JSON 字符串:", json_str);

// 解析回对象
parsed := stdlib.json.parse(json_string => json_str);
stdlib.io.println("解析后的对象:", parsed);

// 格式化输出
pretty := stdlib.json.stringify_pretty(object => person);
stdlib.io.println("格式化 JSON:");
stdlib.io.println(pretty);
```

## 数据类型映射

| Onion 类型 | JSON 类型 | 说明 |
|-----------|-----------|------|
| String | string | 直接对应 |
| Integer | number | 整数 |
| Float | number | 浮点数 |
| Boolean | boolean | 布尔值 |
| Null | null | 空值 |
| Tuple (所有元素都是 Pair) | object | 字典结构 |
| Tuple (其他情况) | array | 数组结构 |
| Pair | N/A | 作为对象的键值对 |

## 注意事项

1. Onion 的字典结构使用 Tuple 包含 Pair 的方式实现
2. JSON 对象的键必须是字符串类型
3. 解析 JSON 时，数字会根据是否包含小数点自动转换为 Integer 或 Float
4. 往返转换（parse → stringify）可能会改变键的顺序，但数据内容保持一致
5. 不支持 JSON 中的注释或扩展语法
