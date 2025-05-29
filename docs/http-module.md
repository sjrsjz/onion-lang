# HTTP模块文档

Onion语言的HTTP模块提供了完整的HTTP客户端功能，支持异步和同步请求。

## 功能特性

- 支持所有标准HTTP方法：GET, POST, PUT, DELETE, PATCH
- 异步请求支持，适用于并发场景
- 同步请求支持，适用于简单测试
- 自动JSON格式化响应
- 完整的错误处理
- 自定义请求头支持
- 请求超时控制

## API参考

### 异步HTTP方法

#### stdlib.http.get(url => "...")

执行异步GET请求

```onion
get_request := stdlib.http.get(url => "https://httpbin.org/get");
result := mut undefined;
task := () -> {
    sync (() -> {
        result = get_request();
    })();
};
async task();
```

#### stdlib.http.post(url => "...", body => "...")

执行异步POST请求

```onion
post_request := stdlib.http.post(
    url => "https://httpbin.org/post",
    body => "{\"name\":\"test\"}"
);
```

#### stdlib.http.put(url => "...", body => "...")

执行异步PUT请求

#### stdlib.http.delete(url => "...")

执行异步DELETE请求

#### stdlib.http.patch(url => "...", body => "...")

执行异步PATCH请求

#### stdlib.http.request(url => "...", method => "...", headers => {}, body => "...")

执行异步通用请求

```onion
custom_request := stdlib.http.request(
    url => "https://httpbin.org/anything",
    method => "POST",
    headers => {},
    body => "{\"custom\":\"data\"}"
);
```

### 同步HTTP方法

#### stdlib.http.get_sync(url => "...")

执行同步GET请求

```onion
result := stdlib.http.get_sync(url => "https://httpbin.org/get");
stdlib.io.println("结果:", result);
```

#### stdlib.http.post_sync(url => "...", body => "...")

执行同步POST请求

```onion
result := stdlib.http.post_sync(
    url => "https://httpbin.org/post",
    body => "{\"message\":\"Hello!\"}"
);
```

## 响应格式

HTTP模块返回JSON格式的响应，包含以下字段：

```json
{
    "status_code": 200,
    "status": "OK",
    "method": "GET",
    "url": "https://httpbin.org/get",
    "body": "响应体内容",
    "success": true
}
```

## 错误处理

当请求失败时，会返回错误信息字符串，如：

- "Request timeout" - 请求超时
- "Connection error: ..." - 连接错误
- "HTTP request failed: ..." - 其他HTTP错误

## 使用示例

完整的使用示例请参考：

- `examples/http_simple.onion` - 基础HTTP请求示例
- `examples/http_test.onion` - 完整的HTTP功能测试
- `examples/http_sync_test.onion` - 同步HTTP请求测试

## 技术实现

HTTP模块基于以下技术栈：

- `reqwest` - HTTP客户端库
- `tokio` - 异步运行时
- `serde_json` - JSON序列化
- Onion VM异步调度器 - 协程支持
