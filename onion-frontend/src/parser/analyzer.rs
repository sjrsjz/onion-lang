//! AST 语义分析模块：提供变量定义检查、类型推断、作用域管理等功能。
//!
//! 本模块实现了 Onion 语言 AST 的语义分析，包括变量定义检查、类型推断、
//! 作用域管理、以及自动捕获变量分析。主要用于编译期的静态检查和优化。
//!
//! # 主要类型
//! - `AssumedType`：推断的变量类型枚举
//! - `Variable`：变量信息，包含类型推断
//! - `VariableFrame` 和 `VariableContext`：作用域和变量上下文管理
//! - `ASTAnalysisDiagnostic`：分析过程中的诊断信息
//!
//! # 核心功能
//! - 变量定义和使用检查
//! - 作用域管理（Frame 和 Context）
//! - 类型推断（基于 AST 节点类型）
//! - 自动捕获变量分析和重构
//! - 断点词法上下文分析
//!
//! # 用法示例
//! ```ignore
//! let mut diagnostics = DiagnosticCollector::new();
//! let context = analyze_ast(&ast, &mut diagnostics, &None)?;
//! let (captured_vars, rebuilt_ast) = auto_capture_and_rebuild(&ast);
//! ```

use std::collections::{HashMap, HashSet};

use crate::{
    diagnostics::{Diagnostic, SourceLocation, collector::DiagnosticCollector},
    parser::{
        Source,
        ast::{ASTNodeOperation, ASTNodeType},
    },
};

use onion_vm::types::lambda::vm_instructions::ir_translator::PRE_ALLOCATED_VARIABLE_STRINGS;

use super::ast::ASTNode;

/// 推断的变量类型枚举。
///
/// 用于在语义分析过程中推断变量的可能类型，帮助进行类型检查和优化。
#[derive(Debug, Clone, PartialEq)]
pub enum AssumedType {
    /// 未知类型。
    Unknown,
    /// Lambda 函数类型。
    Lambda,
    /// 字符串类型。
    String,
    /// 数值类型。
    Number,
    /// 布尔类型。
    Boolean,
    /// Base64 编码的字节类型。
    Base64,
    /// 空值类型。
    Null,
    /// 未定义类型。
    Undefined,
    /// 范围类型。
    Range,
    /// 元组类型。
    Tuple,
    /// 集合类型。
    Set,
    /// 键值对类型。
    KeyVal,
}

/// 变量信息结构。
///
/// 存储变量的推断类型信息，用于语义分析和类型检查。
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Variable {
    pub assumed_type: AssumedType,
}

/// 变量作用域帧。
///
/// 表示单个作用域内的变量定义，支持变量的定义和查找。
#[derive(Debug, Clone)]
pub struct VariableFrame {
    pub variables: HashMap<String, Variable>,
}

impl VariableFrame {
    /// 定义或更新变量。
    ///
    /// # 参数
    /// - `name`：变量名。
    /// - `var`：变量信息。
    ///
    /// # 返回
    /// 成功时返回 `Ok(())`，失败时返回错误信息。
    pub fn define_variable(&mut self, name: String, var: Variable) -> Result<(), String> {
        if let Some(existing_var) = self.variables.get_mut(&name) {
            existing_var.assumed_type = var.assumed_type;
        } else {
            self.variables.insert(name, var);
        }
        Ok(())
    }
}

/// 变量上下文管理器。
///
/// 管理多层嵌套的作用域，支持上下文和帧的推入/弹出操作。
/// 用于跟踪不同作用域中的变量定义和可见性。
#[derive(Debug, Clone)]
pub struct VariableContext {
    contexts: Vec<Vec<VariableFrame>>,
}

impl VariableContext {
    /// 获取所有上下文的引用。
    pub fn all_contexts(&self) -> &Vec<Vec<VariableFrame>> {
        &self.contexts
    }
}

/// AST 分析诊断信息类型。
///
/// 封装语义分析过程中可能出现的错误和警告信息。
#[derive(Debug, Clone)]
pub enum ASTAnalysisDiagnostic {
    /// 未定义变量错误。
    UndefinedVariable(Option<SourceLocation>, String),
    /// 详细错误信息。
    DetailedError(Option<SourceLocation>, String),
}

impl Diagnostic for ASTAnalysisDiagnostic {
    fn severity(&self) -> crate::diagnostics::ReportSeverity {
        match self {
            ASTAnalysisDiagnostic::UndefinedVariable(_, _) => {
                crate::diagnostics::ReportSeverity::Error
            }
            ASTAnalysisDiagnostic::DetailedError(_, _) => crate::diagnostics::ReportSeverity::Error,
        }
    }

    fn title(&self) -> String {
        "AST Analysis Error".into()
    }

    fn message(&self) -> String {
        match self {
            ASTAnalysisDiagnostic::UndefinedVariable(_, var_name) => {
                format!("Variable '{}' was used before it was defined.", var_name)
            }
            ASTAnalysisDiagnostic::DetailedError(_, msg) => msg.clone(),
        }
    }

    fn location(&self) -> Option<SourceLocation> {
        match self {
            ASTAnalysisDiagnostic::UndefinedVariable(loc, _) => loc.clone(),
            ASTAnalysisDiagnostic::DetailedError(loc, _) => loc.clone(),
        }
    }

    fn help(&self) -> Option<String> {
        match self {
            ASTAnalysisDiagnostic::UndefinedVariable(_, var_name) => Some(format!(
                "Define the variable '{}' before using it. If the variable is dynamic, use \"@required '{}'\" to ensure it is defined at runtime.",
                var_name, var_name
            )),
            ASTAnalysisDiagnostic::DetailedError(_, _) => None,
        }
    }

    fn copy(&self) -> Box<dyn Diagnostic> {
        Box::new(self.clone())
    }
}

/// 分析输出结果。
///
/// 包含断点处的变量上下文信息，用于调试支持。
#[derive(Debug)]
pub struct AnalysisOutput {
    pub context_at_break: Option<VariableContext>,
}

impl VariableContext {
    /// 创建新的变量上下文。
    pub fn new() -> Self {
        VariableContext {
            contexts: vec![vec![VariableFrame {
                variables: HashMap::new(),
            }]],
        }
    }

    /// 获取当前上下文的可变引用。
    fn current_context_mut(&mut self) -> &mut Vec<VariableFrame> {
        self.contexts
            .last_mut()
            .expect("Context stack should never be empty")
    }

    /// 获取当前上下文的引用。
    fn current_context(&self) -> &Vec<VariableFrame> {
        self.contexts
            .last()
            .expect("Context stack should never be empty")
    }

    /// 在当前作用域定义变量。
    pub fn define_variable(&mut self, name: String, var: Variable) {
        self.current_context_mut()
            .last_mut()
            .expect("Current context should always have at least one frame")
            .define_variable(name, var)
            .unwrap_or_else(|e| {
                panic!("Failed to define variable: {}", e);
            });
    }

    /// 查找变量（搜索所有上下文和帧）。
    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        for frames in self.contexts.iter().rev() {
            for frame in frames.iter().rev() {
                if let Some(var) = frame.variables.get(name) {
                    return Some(var);
                }
            }
        }
        None
    }

    /// 在当前上下文中查找变量。
    pub fn get_variable_current_context(&self, name: &str) -> Option<&Variable> {
        for frame in self.current_context().iter().rev() {
            if let Some(var) = frame.variables.get(name) {
                return Some(var);
            }
        }
        None
    }

    /// 推入新的作用域帧。
    pub fn push_frame(&mut self) {
        self.current_context_mut().push(VariableFrame {
            variables: HashMap::new(),
        });
    }

    /// 弹出作用域帧。
    pub fn pop_frame(&mut self) -> Result<(), String> {
        let current_context = self.current_context_mut();
        if current_context.len() > 1 {
            current_context.pop();
            Ok(())
        } else {
            Err("Cannot pop the initial frame of a context".to_string())
        }
    }

    /// 推入新的上下文。
    pub fn push_context(&mut self) {
        self.contexts.push(vec![VariableFrame {
            variables: HashMap::new(),
        }]);
    }

    /// 弹出上下文。
    pub fn pop_context(&mut self) -> Result<(), String> {
        if self.contexts.len() > 1 {
            self.contexts.pop();
            Ok(())
        } else {
            Err("Cannot pop the global context".to_string())
        }
    }
}

// --- 主要分析逻辑 ---

/// 检查子节点数量是否精确匹配的辅助宏。
macro_rules! check_children_exact {
    ($diagnostics:expr, $node:expr, $expected:expr, $name:expr) => {
        if $node.children.len() != $expected {
            $diagnostics.report(ASTAnalysisDiagnostic::DetailedError(
                $node.source_location.clone(),
                format!(
                    "'{}' node expects exactly {} child(ren), but found {}",
                    $name,
                    $expected,
                    $node.children.len()
                ),
            ));
            return AssumedType::Unknown;
        }
    };
}

/// 检查子节点数量是否在范围内的辅助宏。
macro_rules! check_children_range {
    ($diagnostics:expr, $node:expr, $range:expr, $name:expr) => {
        let range_clone = $range.clone();
        if !range_clone.contains(&$node.children.len()) {
            $diagnostics.report(ASTAnalysisDiagnostic::DetailedError(
                $node.source_location.clone(),
                format!(
                    "'{}' node expects between {} and {} children, but found {}",
                    $name,
                    $range.start(),
                    $range.end(),
                    $node.children.len()
                ),
            ));
            return AssumedType::Unknown;
        }
    };
}

/// 分析 AST 的主入口函数。
///
/// # 参数
/// - `ast`：要分析的 AST 根节点。
/// - `diagnostics_collector`：诊断信息收集器。
/// - `break_at_position`：可选的断点位置，用于调试支持。
///
/// # 返回
/// 成功时返回断点处的变量上下文，失败时返回 `Err(())`。
pub fn analyze_ast(
    ast: &ASTNode,
    diagnostics_collector: &mut DiagnosticCollector,
    break_at_position: &Option<(Source, usize)>,
) -> Result<Option<VariableContext>, ()> {
    let mut context = VariableContext::new();
    context.push_context();
    for builtin in PRE_ALLOCATED_VARIABLE_STRINGS {
        context.define_variable(
            builtin.to_string(),
            Variable {
                assumed_type: AssumedType::Unknown,
            },
        );
    }

    let mut context_at_break: Option<VariableContext> = None;

    analyze_node(
        ast,
        &mut context,
        diagnostics_collector,
        false,
        break_at_position,
        &mut context_at_break,
    );

    Ok(context_at_break)
}

/// 检查是否在断点位置，并保存上下文。
fn check_postorder_break(
    node: &ASTNode,
    break_at_position: &Option<(Source, usize)>,
    context: &VariableContext,
    context_at_break: &mut Option<VariableContext>,
) {
    if break_at_position.is_none() || context_at_break.is_some() {
        return;
    }
    if let Some((source, break_pos)) = break_at_position {
        if let Some(location) = &node.source_location {
            if location.source.ne(source) {
                return;
            }
            let (start_char, end_char) = location.span;
            let source_code = location.source.content_str();
            let start_byte = source_code
                .char_indices()
                .nth(start_char)
                .map(|(b, _)| b)
                .unwrap_or(source_code.len());
            let end_byte = source_code
                .char_indices()
                .nth(end_char)
                .map(|(b, _)| b)
                .unwrap_or(source_code.len());
            if *break_pos >= start_byte && *break_pos <= end_byte {
                *context_at_break = Some(context.clone());
            }
        }
    }
}

/// 递归分析单个 AST 节点。
///
/// # 参数
/// - `node`：要分析的 AST 节点。
/// - `context`：当前变量上下文。
/// - `diagnostics`：诊断信息收集器。
/// - `dynamic`：是否在动态模式下分析。
/// - `break_at_position`：断点位置。
/// - `context_at_break`：断点处的上下文输出。
///
/// # 返回
/// 节点的推断类型。
#[stacksafe::stacksafe]
fn analyze_node(
    node: &ASTNode,
    context: &mut VariableContext,
    diagnostics: &mut DiagnosticCollector,
    dynamic: bool,
    break_at_position: &Option<(Source, usize)>,
    context_at_break: &mut Option<VariableContext>,
) -> AssumedType {
    if context_at_break.is_some() {
        return AssumedType::Unknown;
    }

    match &node.node_type {
        ASTNodeType::Let(var_name) => {
            check_children_exact!(diagnostics, node, 1, "Let");
            let assumed_type = analyze_node(
                &node.children[0],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            if context_at_break.is_some() {
                return AssumedType::Unknown;
            }
            context.define_variable(
                var_name.clone(),
                Variable {
                    assumed_type: assumed_type.clone(),
                },
            );
            check_postorder_break(node, break_at_position, context, context_at_break);
            assumed_type
        }
        ASTNodeType::Variable(var_name) => {
            check_children_exact!(diagnostics, node, 0, "Variable");
            if !dynamic && context.get_variable_current_context(var_name).is_none() {
                diagnostics.report(ASTAnalysisDiagnostic::UndefinedVariable(
                    node.source_location.clone(),
                    var_name.clone(),
                ));
            }
            check_postorder_break(node, break_at_position, context, context_at_break);
            context
                .get_variable(var_name)
                .map_or(AssumedType::Unknown, |v| v.assumed_type.clone())
        }
        ASTNodeType::Required(var_name) => {
            check_children_exact!(diagnostics, node, 0, "Required");
            context.define_variable(
                var_name.clone(),
                Variable {
                    assumed_type: AssumedType::Unknown,
                },
            );
            check_postorder_break(node, break_at_position, context, context_at_break);
            AssumedType::Unknown
        }
        ASTNodeType::Frame => {
            check_children_exact!(diagnostics, node, 1, "Frame");
            context.push_frame();
            let assumed_type = analyze_node(
                &node.children[0],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            if context_at_break.is_none() {
                let _ = context.pop_frame();
            }
            check_postorder_break(node, break_at_position, context, context_at_break);
            assumed_type
        }
        ASTNodeType::LambdaDef(is_dynamic_gen, captured_vars) => {
            check_children_exact!(diagnostics, node, 2, "LambdaDef");
            let params = &node.children[0];
            let body = &node.children[1];
            let mut captured_vars = captured_vars.clone();
            if !dynamic {
                // for var_name in captured_vars.iter() {
                //     if context.get_variable_current_context(var_name).is_none() {
                //         diagnostics.report(ASTAnalysisDiagnostic::UndefinedVariable(
                //             node.source_location.clone(),
                //             var_name.clone(),
                //         ));
                //     }
                // }
                captured_vars.retain(|v| context.get_variable_current_context(v).is_some());
            }
            let params_def = analyze_tuple_params(
                params,
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            if context_at_break.is_some() {
                return AssumedType::Lambda;
            }

            if *is_dynamic_gen {
                analyze_node(
                    body,
                    context,
                    diagnostics,
                    true,
                    break_at_position,
                    context_at_break,
                );
            } else {
                let params_def = params_def.unwrap_or_default();
                context.push_context();
                for var in PRE_ALLOCATED_VARIABLE_STRINGS {
                    context.define_variable(
                        var.to_string(),
                        Variable {
                            assumed_type: AssumedType::Unknown,
                        },
                    );
                }
                let mut all_vars = captured_vars.clone();
                all_vars.extend(params_def);
                for var in all_vars {
                    context.define_variable(
                        var,
                        Variable {
                            assumed_type: AssumedType::Unknown,
                        },
                    );
                }
                analyze_node(
                    body,
                    context,
                    diagnostics,
                    false,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_none() {
                    let _ = context.pop_context();
                }
            }
            check_postorder_break(node, break_at_position, context, context_at_break);
            AssumedType::Lambda
        }
        ASTNodeType::Assign => {
            check_children_exact!(diagnostics, node, 2, "Assign");
            let assumed_type = analyze_node(
                &node.children[1],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            if context_at_break.is_some() {
                return AssumedType::Unknown;
            }
            analyze_node(
                &node.children[0],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            check_postorder_break(node, break_at_position, context, context_at_break);
            assumed_type
        }
        ASTNodeType::Pair => {
            check_children_exact!(diagnostics, node, 2, "Pair");
            analyze_node(
                &node.children[0],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            if context_at_break.is_some() {
                return AssumedType::KeyVal;
            }
            analyze_node(
                &node.children[1],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            check_postorder_break(node, break_at_position, context, context_at_break);
            AssumedType::KeyVal
        }
        ASTNodeType::Operation(_) => {
            // This case was correctly identified as needing a nested match
            if let ASTNodeType::Operation(op) = &node.node_type {
                match op {
                    ASTNodeOperation::Not | ASTNodeOperation::Abs | ASTNodeOperation::Minus => {
                        check_children_exact!(diagnostics, node, 1, "Unary Operation 'not'")
                    }
                    _ => check_children_exact!(diagnostics, node, 2, "Binary Operation"),
                }
            }
            let mut last_type = AssumedType::Unknown;
            for child in &node.children {
                last_type = analyze_node(
                    child,
                    context,
                    diagnostics,
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            check_postorder_break(node, break_at_position, context, context_at_break);
            last_type
        }
        ASTNodeType::Modifier(_) => {
            check_children_exact!(diagnostics, node, 1, "Modifier");
            analyze_node(
                &node.children[0],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            )
        }
        ASTNodeType::Return => {
            check_children_range!(diagnostics, node, 0..=1, "Return");
            if let Some(child) = node.children.first() {
                analyze_node(
                    child,
                    context,
                    diagnostics,
                    dynamic,
                    break_at_position,
                    context_at_break,
                )
            } else {
                AssumedType::Null
            }
        }
        ASTNodeType::If => {
            check_children_range!(diagnostics, node, 2..=3, "If");
            analyze_node(
                &node.children[0],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            if context_at_break.is_some() {
                return AssumedType::Unknown;
            }
            let then_type = analyze_node(
                &node.children[1],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            if context_at_break.is_some() {
                return AssumedType::Unknown;
            }
            if node.children.len() == 3 {
                let _else_type = analyze_node(
                    &node.children[2],
                    context,
                    diagnostics,
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
            }
            then_type
        }
        ASTNodeType::While => {
            check_children_exact!(diagnostics, node, 2, "While");
            analyze_node(
                &node.children[0],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            if context_at_break.is_some() {
                return AssumedType::Unknown;
            }
            analyze_node(
                &node.children[1],
                context,
                diagnostics,
                dynamic,
                break_at_position,
                context_at_break,
            );
            AssumedType::Unknown
        }
        ASTNodeType::String(_) => {
            check_children_exact!(diagnostics, node, 0, "String Literal");
            AssumedType::String
        }
        ASTNodeType::Boolean(_) => {
            check_children_exact!(diagnostics, node, 0, "Boolean Literal");
            AssumedType::Boolean
        }
        ASTNodeType::Number(_) => {
            check_children_exact!(diagnostics, node, 0, "Number Literal");
            AssumedType::Number
        }
        ASTNodeType::Base64(_) => {
            check_children_exact!(diagnostics, node, 0, "Base64 Literal");
            AssumedType::Base64
        }
        ASTNodeType::Null => {
            check_children_exact!(diagnostics, node, 0, "Null");
            AssumedType::Null
        }
        ASTNodeType::Undefined => {
            check_children_exact!(diagnostics, node, 0, "Undefined");
            AssumedType::Undefined
        }
        ASTNodeType::Break => {
            check_children_exact!(diagnostics, node, 1, "Break");
            AssumedType::Unknown
        }
        ASTNodeType::Continue => {
            check_children_exact!(diagnostics, node, 1, "Continue");
            AssumedType::Unknown
        }
        _ => {
            // Default for variable-child nodes or unhandled ones
            let mut last_type = AssumedType::Unknown;
            for child in &node.children {
                last_type = analyze_node(
                    child,
                    context,
                    diagnostics,
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            last_type
        }
    }
}

/// 分析元组参数定义，提取参数名称。
///
/// # 参数
/// - `params`：参数定义节点。
/// - `context`：变量上下文。
/// - `diagnostics`：诊断收集器。
/// - `dynamic`：是否动态模式。
/// - `break_at_position`：断点位置。
/// - `context_at_break`：断点上下文输出。
///
/// # 返回
/// 参数名称集合。
#[stacksafe::stacksafe]
fn analyze_tuple_params(
    params: &ASTNode,
    context: &mut VariableContext,
    diagnostics: &mut DiagnosticCollector,
    dynamic: bool,
    break_at_position: &Option<(Source, usize)>,
    context_at_break: &mut Option<VariableContext>,
) -> Option<HashSet<String>> {
    if context_at_break.is_some() {
        return None;
    }
    analyze_node(
        params,
        context,
        diagnostics,
        dynamic,
        break_at_position,
        context_at_break,
    );
    if context_at_break.is_some() {
        return None;
    }

    let mut param_names = HashSet::new();

    fn inner(node: &ASTNode, param_names: &mut HashSet<String>) {
        match &node.node_type {
            ASTNodeType::String(name) => {
                param_names.insert(name.clone());
            }
            ASTNodeType::Tuple => {
                for child in &node.children {
                    inner(child, param_names);
                }
            }
            ASTNodeType::Pair => {
                if let Some(key_node) = node.children.first() {
                    if let ASTNodeType::String(key_name) = &key_node.node_type {
                        param_names.insert(key_name.clone());
                    }
                }
            }
            _ => {}
        }
    }

    inner(params, &mut param_names);

    Some(param_names)
}

/// 自动捕获变量并重构 AST。
///
/// # 参数
/// - `node`：要分析的 AST 节点。
///
/// # 返回
/// (需要捕获的变量集合, 重构后的 AST)
pub fn auto_capture_and_rebuild(node: &ASTNode) -> (HashSet<String>, ASTNode) {
    let mut context = VariableContext::new();
    context.push_context();
    for var in PRE_ALLOCATED_VARIABLE_STRINGS {
        context.define_variable(
            var.to_string(),
            Variable {
                assumed_type: AssumedType::Unknown,
            },
        );
    }
    auto_capture(&mut context, node, false)
}

/// 自动捕获变量分析的递归实现。
///
/// # 参数
/// - `context`：变量上下文。
/// - `node`：当前 AST 节点。
/// - `dynamic`：是否动态模式。
///
/// # 返回
/// (需要捕获的变量集合, 重构后的 AST 节点)
#[stacksafe::stacksafe]
pub fn auto_capture(
    context: &mut VariableContext,
    node: &ASTNode,
    dynamic: bool,
) -> (HashSet<String>, ASTNode) {
    let empty_result = || (HashSet::new(), node.clone());

    match &node.node_type {
        ASTNodeType::Variable(var_name) => {
            if dynamic || context.get_variable_current_context(var_name).is_some() {
                return (HashSet::new(), node.clone());
            }
            let mut required_vars = HashSet::new();
            required_vars.insert(var_name.clone());
            (required_vars, node.clone())
        }

        ASTNodeType::Let(name) => {
            if node.children.len() != 1 {
                return empty_result();
            } // Robustness check
            let value_node = &node.children[0];

            let (value_req_vars, new_value_node) = auto_capture(context, value_node, dynamic);

            context.define_variable(
                name.clone(),
                Variable {
                    assumed_type: AssumedType::Unknown,
                },
            );

            let mut new_node = node.clone();
            new_node.children = vec![new_value_node];
            (value_req_vars, new_node)
        }

        ASTNodeType::Frame => {
            if node.children.len() != 1 {
                return empty_result();
            } // Robustness check
            let child = &node.children[0];

            context.push_frame();
            let (child_req_vars, new_child_node) = auto_capture(context, child, dynamic);
            context.pop_frame().unwrap();

            let mut new_node = node.clone();
            new_node.children = vec![new_child_node];
            (child_req_vars, new_node)
        }

        ASTNodeType::LambdaDef(is_dynamic_gen, captured_vars) => {
            if node.children.len() != 2 {
                return empty_result();
            } // Robustness check
            let params = &node.children[0];
            let body = &node.children[1];

            let (mut required_vars, new_params_node) =
                auto_capture(context, params, *is_dynamic_gen);

            if !dynamic {
                for v in captured_vars {
                    if context.get_variable_current_context(v).is_none() {
                        required_vars.insert(v.clone());
                    }
                }
            }

            if *is_dynamic_gen {
                let (body_req_vars, new_body_node) = auto_capture(context, body, true);
                required_vars.extend(body_req_vars);
                let mut new_node = node.clone();
                new_node.children = vec![new_params_node, new_body_node];
                return (required_vars, new_node);
            }

            context.push_context();
            for var in PRE_ALLOCATED_VARIABLE_STRINGS {
                context.define_variable(
                    var.to_string(),
                    Variable {
                        assumed_type: AssumedType::Unknown,
                    },
                );
            }

            for var in captured_vars {
                context.define_variable(
                    var.clone(),
                    Variable {
                        assumed_type: AssumedType::Unknown,
                    },
                );
            }

            fn define_params(context: &mut VariableContext, params: &ASTNode) {
                match &params.node_type {
                    ASTNodeType::String(var_name) => context.define_variable(
                        var_name.clone(),
                        Variable {
                            assumed_type: AssumedType::Unknown,
                        },
                    ),
                    ASTNodeType::Tuple => {
                        for param in &params.children {
                            define_params(context, param);
                        }
                    }
                    ASTNodeType::Pair => {
                        if let Some(key_node) = params.children.first() {
                            if let ASTNodeType::String(var_name) = &key_node.node_type {
                                context.define_variable(
                                    var_name.clone(),
                                    Variable {
                                        assumed_type: AssumedType::Unknown,
                                    },
                                )
                            }
                        }
                    }
                    _ => (),
                }
            }

            // Define parameters in the new context
            define_params(context, params);
            let (body_req_vars, new_body_node) = auto_capture(context, body, false);
            context.pop_context().unwrap();

            for var in &body_req_vars {
                if context.get_variable_current_context(var).is_none() {
                    required_vars.insert(var.clone());
                }
            }

            let mut final_captured = captured_vars.clone();
            final_captured.extend(body_req_vars);

            let rebuilt_ast_node = ASTNode {
                node_type: ASTNodeType::LambdaDef(*is_dynamic_gen, final_captured),
                source_location: node.source_location.clone(),
                children: vec![new_params_node, new_body_node],
            };
            (required_vars, rebuilt_ast_node)
        }

        ASTNodeType::Dynamic | ASTNodeType::Static => {
            let dynamic_flag = node.node_type == ASTNodeType::Dynamic;
            let mut required_vars = HashSet::new();
            let mut new_children = Vec::with_capacity(node.children.len());
            for child in &node.children {
                let (child_req_vars, new_child_node) = auto_capture(context, child, dynamic_flag);
                required_vars.extend(child_req_vars);
                new_children.push(new_child_node);
            }
            let mut new_node = node.clone();
            new_node.children = new_children;
            (required_vars, new_node)
        }
        _ => {
            let mut required_vars = HashSet::new();
            let mut new_children = Vec::with_capacity(node.children.len());
            for child in &node.children {
                let (child_req_vars, new_child_node) = auto_capture(context, child, dynamic);
                required_vars.extend(child_req_vars);
                new_children.push(new_child_node);
            }
            let mut new_node = node.clone();
            new_node.children = new_children;
            (required_vars, new_node)
        }
    }
}
