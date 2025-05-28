use std::collections::HashMap;

use super::protocol::*;

/// 初始化LSP服务器能力
pub fn initialize_capabilities() -> ServerCapabilities {
    let mut semantic_tokens_legend = HashMap::new();

    // 定义语义标记类型
    let token_types = vec![
        "namespace".to_string(),
        "type".to_string(),
        "class".to_string(),
        "enum".to_string(),
        "interface".to_string(),
        "struct".to_string(),
        "typeParameter".to_string(),
        "parameter".to_string(),
        "variable".to_string(),
        "property".to_string(),
        "enumMember".to_string(),
        "event".to_string(),
        "function".to_string(),
        "method".to_string(),
        "macro".to_string(),
        "keyword".to_string(),
        "modifier".to_string(),
        "comment".to_string(),
        "string".to_string(),
        "number".to_string(),
        "regexp".to_string(),
        "operator".to_string(),
        // 自定义语义标记类型
        "null".to_string(),
        "boolean".to_string(),
        "base64".to_string(),
        "let".to_string(),
        "body".to_string(),
        "boundary".to_string(),
        "assign".to_string(),
        "lambdaDef".to_string(),
        "expressions".to_string(),
        "lambdaCall".to_string(),
        "asyncLambdaCall".to_string(),
        "operation".to_string(),
        "tuple".to_string(),
        "assumeTuple".to_string(),
        "keyValue".to_string(),
        "indexOf".to_string(),
        "getAttr".to_string(),
        "return".to_string(),
        "raise".to_string(),
        "if".to_string(),
        "while".to_string(),
        "namedTo".to_string(),
        "break".to_string(),
        "continue".to_string(),
        "range".to_string(),
        "in".to_string(),
        "emit".to_string(),
        "alias".to_string(),
        "set".to_string(),
        "map".to_string(),
    ];

    // 定义语义标记修饰符
    let token_modifiers = vec![
        "declaration".to_string(),
        "definition".to_string(),
        "readonly".to_string(),
        "static".to_string(),
        "deprecated".to_string(),
        "abstract".to_string(),
        "async".to_string(),
        "modification".to_string(),
        "documentation".to_string(),
        "defaultLibrary".to_string(),
    ];

    semantic_tokens_legend.insert(
        "tokenTypes".to_string(),
        serde_json::Value::Array(
            token_types
                .iter()
                .map(|s| serde_json::Value::String(s.clone()))
                .collect(),
        ),
    );

    semantic_tokens_legend.insert(
        "tokenModifiers".to_string(),
        serde_json::Value::Array(
            token_modifiers
                .iter()
                .map(|s| serde_json::Value::String(s.clone()))
                .collect(),
        ),
    );

    // 语义标记选项
    let semantic_tokens_options = serde_json::json!({
        "legend": semantic_tokens_legend,
        "full": true,
        "range": false
    });

    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::Full),
            will_save: Some(false),
            will_save_wait_until: Some(false),
            save: Some(SaveOptions {
                include_text: Some(false),
            }),
        }),
        hover_provider: Some(false), // 暂不支持悬停
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![
                ".".to_string(),
                ":".to_string(),
                "(".to_string(),
                ",".to_string(),
            ]),
        }),
        signature_help_provider: None,         // 暂不支持函数签名帮助
        definition_provider: Some(false),      // 暂不支持跳转到定义
        type_definition_provider: Some(false), // 暂不支持类型定义
        implementation_provider: Some(false),  // 暂不支持实现查找
        references_provider: Some(false),      // 暂不支持引用查找
        document_highlight_provider: Some(false), // 暂不支持文档高亮
        document_symbol_provider: Some(false), // 暂不支持文档符号
        workspace_symbol_provider: Some(false), // 暂不支持工作区符号
        code_action_provider: Some(false),     // 暂不支持代码操作
        document_formatting_provider: Some(false), // 暂不支持文档格式化
        document_range_formatting_provider: Some(false), // 暂不支持范围格式化
        rename_provider: Some(false),          // 暂不支持重命名
        semantic_tokens_provider: Some(serde_json::to_value(semantic_tokens_options).unwrap()),
        other: std::collections::HashMap::new(),
    }
}
