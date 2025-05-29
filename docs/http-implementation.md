# Onion语言HTTP模块 - 完整实现

## 实现概览

Onion语言的HTTP模块现已完全实现，提供了功能齐全的HTTP客户端支持。这个模块基于Rust的`reqwest`库和`tokio`异步运行时，与Onion VM的异步调度器完美集成。

## 核心特性

### 1. 异步HTTP支持
- 完全异步的HTTP请求实现
- 与Onion VM调度器集成
- 支持并发请求处理
- 非阻塞式操作

### 2. 完整的HTTP方法支持
- **GET** - 获取资源
- **POST** - 创建资源
- **PUT** - 更新资源
- **DELETE** - 删除资源
- **PATCH** - 部分更新资源
- **通用请求** - 自定义方法和参数

### 3. 同步HTTP支持
- 提供同步版本用于简单场景
- 适合测试和快速原型开发
- 直接返回结果，无需异步处理

### 4. 高级功能
- 自定义请求头支持
- JSON自动格式化
- 完整的错误处理
- 请求超时控制（30秒）
- 详细的响应信息

## 技术架构

### 核心组件

1. **AsyncHttpRequest结构**
   - 管理HTTP请求状态
   - 实现Runnable trait用于异步调度
   - 支持状态跟踪：Pending -> InProgress -> Completed

2. **HTTP客户端实现**
   - 基于reqwest库
   - Tokio异步运行时
   - 自动错误分类和处理

3. **API包装层**
   - 将HTTP功能包装为Onion函数
   - 参数解析和验证
   - 结果格式化

### 响应格式

所有HTTP请求返回标准化的JSON响应：

```json
{
    "status_code": 200,
    "status": "OK", 
    "method": "GET",
    "url": "https://example.com",
    "body": "响应内容",
    "success": true
}
```

### 错误处理

完整的错误分类和处理：
- 连接错误
- 超时错误
- 请求格式错误
- 服务器错误
- 网络错误

## API文档

### 异步方法

```onion
// GET请求
get_request := stdlib.http.get(url => "https://api.example.com/data");

// POST请求
post_request := stdlib.http.post(
    url => "https://api.example.com/data",
    body => "{\"key\":\"value\"}"
);

// 通用请求
custom_request := stdlib.http.request(
    url => "https://api.example.com/endpoint",
    method => "PATCH",
    headers => {},
    body => "{\"update\":\"data\"}"
);

// 执行异步请求
result := mut undefined;
task := () -> {
    run () -> {
        result = get_request();
    };
};
async task();
```

### 同步方法

```onion
// 同步GET请求
result := stdlib.http.get_sync(url => "https://api.example.com/data");

// 同步POST请求  
result := stdlib.http.post_sync(
    url => "https://api.example.com/data",
    body => "{\"message\":\"hello\"}"
);
```

## 测试示例

项目包含完整的测试示例：

1. **http_simple.onion** - 基础使用示例
2. **http_test.onion** - 完整功能测试
3. **http_sync_test.onion** - 同步API测试
4. **http_demo.onion** - 演示各种用法

## 依赖项

HTTP模块需要以下Rust依赖：

```toml
reqwest = { version = "0.11", features = ["json"] }
tokio = { version = "1.0", features = ["full"] }
serde_json = "1.0"
```

## 使用建议

### 异步vs同步

- **异步方法**：适用于需要并发处理的场景，如API服务、批量请求等
- **同步方法**：适用于脚本、测试、简单的一次性请求

### 性能考虑

- 异步方法提供更好的并发性能
- 同步方法更简单但会阻塞执行
- 合理选择方法类型以获得最佳性能

### 错误处理

```onion
// 建议的错误处理模式
result := stdlib.http.get_sync(url => "https://api.example.com");
if (stdlib.string.contains(string => result, substring => "HTTP Error")) {
    stdlib.io.println("请求失败:", result);
} else {
    stdlib.io.println("请求成功:", result);
};
```

## 扩展性

HTTP模块设计为可扩展的架构：

1. **自定义客户端**：可以添加自定义配置选项
2. **中间件支持**：可以添加请求/响应中间件
3. **认证机制**：可以扩展支持各种认证方式
4. **缓存支持**：可以添加HTTP缓存机制

## 总结

Onion语言的HTTP模块现已完整实现，提供了：

✅ 完整的HTTP方法支持  
✅ 异步和同步API  
✅ 错误处理机制  
✅ JSON响应格式化  
✅ 超时控制  
✅ 自定义请求头  
✅ 完整的文档和示例  

这个实现为Onion语言提供了强大的网络通信能力，支持现代Web开发和API集成需求。
