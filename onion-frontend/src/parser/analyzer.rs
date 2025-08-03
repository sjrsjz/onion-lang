use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use crate::parser::{
    ast::{ASTNodeType, ParserError},
    diagnostics,
    lexer::Source,
};

use colored::*;
use onion_vm::types::lambda::vm_instructions::ir_translator::PRE_ALLOCATED_VARIABLE_STRINGS;

use super::ast::ASTNode;
#[derive(Debug, Clone, PartialEq)] // Added PartialEq for comparison if needed later
pub enum AssumedType {
    Unknown,
    Lambda,
    String,
    Number,
    Boolean,
    Base64,
    Null,
    Undefined,
    Range,
    Tuple,
    Set,
    KeyVal,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Variable {
    pub assumed_type: AssumedType,
}

#[derive(Debug, Clone)] // Added Clone
pub struct VariableFrame {
    pub variables: HashMap<String, Variable>,
}

impl VariableFrame {
    pub fn define_variable(&mut self, name: String, var: Variable) -> Result<(), String> {
        if let Some(existing_var) = self.variables.get_mut(&name) {
            existing_var.assumed_type = var.assumed_type;
        } else {
            self.variables.insert(name, var);
        }
        Ok(())
    }
}

#[derive(Debug, Clone)] // Added Clone
pub struct VariableContext {
    // 改为上下文堆栈，每个上下文包含其自己的帧堆栈
    contexts: Vec<Vec<VariableFrame>>,
}

impl VariableContext {
    pub fn last_context(&self) -> Option<&Vec<VariableFrame>> {
        self.contexts.last()
    }
    pub fn all_contexts(&self) -> &Vec<Vec<VariableFrame>> {
        &self.contexts
    }
}

#[derive(Debug, Clone)]
pub enum AnalyzeError {
    UndefinedVariable(ASTNode),
    InvalidMacroDefinition(ASTNode, String), // 添加宏定义错误
    ParserError(ParserError),                // 用于解析错误
    DetailedError(ASTNode, String),          // 用于其他类型的错误
}

impl Display for AnalyzeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalyzeError::UndefinedVariable(node) => {
                write!(f, "Undefined variable at node: {:?}", node)
            }
            AnalyzeError::InvalidMacroDefinition(node, message) => {
                write!(
                    f,
                    "Invalid macro definition at node: {:?}, message: {}",
                    node, message
                )
            }
            AnalyzeError::DetailedError(node, message) => {
                write!(
                    f,
                    "Detailed error at node: {:?}, message: {}",
                    node, message
                )
            }
            AnalyzeError::ParserError(err) => {
                write!(f, "Parser error: {:?}", err)
            }
        }
    }
}

impl AnalyzeError {
    /// 将分析错误格式化为用户友好的字符串。
    pub fn format(&self) -> String {
        match self {
            // 对于 ParserError，继续委托给它自己的 format 方法
            Self::ParserError(parser_err) => parser_err.format(),

            Self::UndefinedVariable(node) => {
                let var_name = node
                    .start_token
                    .as_ref()
                    .map_or("".to_string(), |t| t.origin_token());
                diagnostics::format_node_based_report(
                    diagnostics::ReportSeverity::Error, // 传入严重性
                    "Semantic Error",
                    &format!("Undefined variable '{}'", var_name.yellow()),
                    node,
                    "Ensure the variable is defined in the current scope before use.",
                )
            }
            Self::InvalidMacroDefinition(node, msg) => diagnostics::format_node_based_report(
                diagnostics::ReportSeverity::Error,
                "Macro Error",
                &format!("Invalid macro definition: {}", msg.yellow()),
                node,
                "Please check the macro's syntax and structure.",
            ),
            Self::DetailedError(node, msg) => diagnostics::format_node_based_report(
                diagnostics::ReportSeverity::Error,
                "Analysis Error",
                msg,
                node,
                "",
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AnalyzeWarn {
    CompileError(ASTNode, String),
}

impl Display for AnalyzeWarn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalyzeWarn::CompileError(node, message) => {
                write!(f, "Compile error at node: {:?}, message: {}", node, message)
            }
        }
    }
}

impl AnalyzeWarn {
    /// 将分析警告格式化为用户友好的字符串。
    pub fn format(&self) -> String {
        match self {
            Self::CompileError(node, msg) => {
                // 同样调用我们新模块中的函数，但传入 Warning 级别
                diagnostics::format_node_based_report(
                    diagnostics::ReportSeverity::Warning, // 传入严重性
                    "Warning",
                    msg,
                    node,
                    "",
                )
            }
        }
    }
}

pub trait AnalysisResult {
    fn errors(&self) -> &[AnalyzeError];
    fn warnings(&self) -> &[AnalyzeWarn];
}

#[derive(Debug)]
pub struct AnalysisOutput {
    pub errors: Vec<AnalyzeError>,
    pub warnings: Vec<AnalyzeWarn>,
    pub context_at_break: Option<VariableContext>,
}

#[derive(Debug)]
pub struct MacroAnalysisOutput {
    pub errors: Vec<AnalyzeError>,
    pub warnings: Vec<AnalyzeWarn>,
    pub result_node: ASTNode,
}

impl AnalysisResult for AnalysisOutput {
    fn errors(&self) -> &[AnalyzeError] {
        &self.errors
    }

    fn warnings(&self) -> &[AnalyzeWarn] {
        &self.warnings
    }
}

impl AnalysisResult for MacroAnalysisOutput {
    fn errors(&self) -> &[AnalyzeError] {
        &self.errors
    }

    fn warnings(&self) -> &[AnalyzeWarn] {
        &self.warnings
    }
}

impl VariableContext {
    pub fn new() -> Self {
        VariableContext {
            // 初始化时包含一个全局上下文，该上下文包含一个初始帧
            contexts: vec![vec![VariableFrame {
                variables: HashMap::new(),
            }]],
        }
    }

    // Helper to get the current context stack mutably
    fn current_context_mut(&mut self) -> &mut Vec<VariableFrame> {
        self.contexts
            .last_mut()
            .expect("Context stack should never be empty")
    }

    // Helper to get the current context stack immutably
    fn current_context(&self) -> &Vec<VariableFrame> {
        self.contexts
            .last()
            .expect("Context stack should never be empty")
    }

    pub fn define_variable(&mut self, name: String, var: Variable) {
        self.current_context_mut()
            .last_mut()
            .expect("Current context should always have at least one frame")
            .define_variable(name, var)
            .unwrap_or_else(|e| {
                panic!("Failed to define variable: {}", e);
            });
    }

    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        for frames in self.contexts.iter().rev() {
            for frame in frames.iter().rev() {
                for (var_name, var) in frame.variables.iter() {
                    if var_name == name {
                        return Some(var);
                    }
                }
            }
        }
        None
    }

    pub fn get_variable_current_context(&self, name: &str) -> Option<&Variable> {
        // 只在当前上下文的最后一个帧中查找变量
        let frame = self.current_context();
        for var in frame.iter() {
            for (var_name, v) in var.variables.iter() {
                if var_name == name {
                    return Some(v);
                }
            }
        }
        None
    }

    // 在当前上下文中推入一个新的帧
    pub fn push_frame(&mut self) {
        self.current_context_mut().push(VariableFrame {
            variables: HashMap::new(),
        });
    }

    // 从当前上下文中弹出一个帧
    pub fn pop_frame(&mut self) -> Result<(), String> {
        let current_context = self.current_context_mut();
        if current_context.len() > 1 {
            current_context.pop();
            Ok(())
        } else {
            Err("Cannot pop the initial frame of a context".to_string())
        }
    }

    // 推入一个新的、空的上下文（用于 LambdaDef）
    pub fn push_context(&mut self) {
        // 新上下文从一个空帧开始
        self.contexts.push(vec![VariableFrame {
            variables: HashMap::new(),
        }]);
    }

    // 弹出一个上下文（用于退出 LambdaDef）
    pub fn pop_context(&mut self) -> Result<(), String> {
        if self.contexts.len() > 1 {
            self.contexts.pop();
            Ok(())
        } else {
            Err("Cannot pop the global context".to_string())
        }
    }
}

pub fn analyze_ast(ast: &ASTNode, break_at_position: Option<usize>) -> AnalysisOutput {
    let mut context = VariableContext::new();
    // 向context里初始化内置函数
    context.push_context();

    for builtin in PRE_ALLOCATED_VARIABLE_STRINGS {
        let var = Variable {
            assumed_type: AssumedType::Unknown,
        };
        context.define_variable(builtin.to_string(), var);
    }

    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    let mut context_at_break: Option<VariableContext> = None;

    analyze_node(
        &ast.start_token.as_ref().map(|t| t.source_code().clone()),
        ast,
        &mut context,
        &mut errors,   // 传递 errors Vec
        &mut warnings, // 传递 warnings Vec
        false,
        break_at_position,
        &mut context_at_break,
    );

    AnalysisOutput {
        errors,
        warnings,
        context_at_break: context_at_break,
    }
}

// Helper function for post-order traversal break check
fn check_postorder_break(
    source: &Option<Source>,
    node: &ASTNode,
    break_at_position: Option<usize>,
    context: &VariableContext,
    context_at_break: &mut Option<VariableContext>,
) {
    if source.is_none() || context_at_break.is_some() {
        return; // 如果没有源代码或已经找到断点，直接返回
    }
    let source = source.as_ref().unwrap();
    // Post-order traversal check: After analyzing the current node and its children,
    // check if we've reached or passed the break position
    if let Some(break_pos) = break_at_position {
        if let Some(ref token) = node.start_token {
            if token.source_code().ne(source) {
                return; // 如果 token 的源代码与当前源代码不匹配，跳过检查
            }

            let (start_char, end_char) = token.origin_token_span();
            let source_code = token.source_code_str();
            // 将 char span 转为字节 span
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

            // 检查 break_pos 是否在字节范围内
            if break_pos >= start_byte && break_pos <= end_byte {
                *context_at_break = Some(context.clone());
            }
        }
    }
}

#[stacksafe::stacksafe]
fn analyze_node(
    source: &Option<Source>,
    node: &ASTNode,
    context: &mut VariableContext,
    errors: &mut Vec<AnalyzeError>,
    warnings: &mut Vec<AnalyzeWarn>,
    dynamic: bool,
    break_at_position: Option<usize>,
    context_at_break: &mut Option<VariableContext>,
) -> AssumedType {
    // 1. Check if analysis should stop globally (break point already found)
    if context_at_break.is_some() {
        return AssumedType::Unknown;
    }

    use super::ast::ASTNodeType; // Removed old auto_break check
    match &node.node_type {
        ASTNodeType::Let(var_name) => {
            if let Some(value_node) = node.children.first() {
                // Analyze the value expression first
                let assumed_type = analyze_node(
                    source,
                    value_node,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                } // Check if break occurred during value analysis
                let var = Variable {
                    assumed_type: assumed_type.clone(),
                };
                context.define_variable(var_name.clone(), var);

                // Post-order check after processing the let statement
                check_postorder_break(source, node, break_at_position, context, context_at_break);
                assumed_type
            } else {
                // Post-order check for incomplete let statements
                check_postorder_break(source, node, break_at_position, context, context_at_break);
                AssumedType::Unknown
            }
        }
        ASTNodeType::Variable(var_name) => {
            // Check definition before potentially breaking
            if context.get_variable_current_context(var_name).is_none() {
                if !dynamic {
                    errors.push(AnalyzeError::UndefinedVariable(node.clone())); // 使用 errors Vec
                }
            }

            // Post-order check after processing the variable
            check_postorder_break(source, node, break_at_position, context, context_at_break);

            // Return variable type or Unknown
            if let Some(var) = context.get_variable_current_context(var_name) {
                var.assumed_type.clone()
            } else {
                AssumedType::Unknown
            }
        }
        ASTNodeType::Frame => {
            // Body 和 Boundary 在当前上下文中创建新的作用域（帧）
            context.push_frame();
            let mut assumed_type = AssumedType::Unknown;
            for child in &node.children {
                assumed_type = analyze_node(
                    source,
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    // Don't pop frame if break happened inside, context needs it
                    return AssumedType::Unknown;
                }
            }
            // Only pop frame if break didn't happen inside this scope
            if context_at_break.is_none() {
                let _ = context.pop_frame(); // 弹出当前上下文的帧
                check_postorder_break(source, node, break_at_position, context, context_at_break);
            }
            return assumed_type;
        }
        ASTNodeType::LambdaDef(is_dynamic_gen, captured_vars) => {
            let mut captured_vars = captured_vars.clone();
            if !dynamic {
                // 可以选择在这里报告未定义的捕获变量，但是为了避免过多错误，不做处理而是交给 Lambda 体分析
                captured_vars.retain(|v| context.get_variable_current_context(v).is_some());
            }
            // 在新的上下文中处理参数（参数定义在第一个帧中）
            let Some(params) = node.children.first() else {
                errors.push(AnalyzeError::DetailedError(
                    node.clone(),
                    "LambdaDef has no parameters".to_string(),
                ));
                check_postorder_break(source, node, break_at_position, context, context_at_break);
                return AssumedType::Lambda;
            };
            let params_def = analyze_tuple_params(
                source,
                params,
                context,
                errors,   // Pass errors
                warnings, // Pass warnings
                dynamic,
                break_at_position,
                context_at_break,
            );
            if context_at_break.is_some() {
                // 如果在参数分析中中断，不弹出上下文
                return AssumedType::Lambda;
            }

            if *is_dynamic_gen {
                analyze_node(
                    source,
                    &node.children.last().unwrap(),
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    // 如果在 Lambda 体分析中中断，不弹出上下文
                    return AssumedType::Lambda;
                }
                check_postorder_break(source, node, break_at_position, context, context_at_break);
                return AssumedType::Lambda;
            } else {
                if params_def.is_none() {
                    // 如果没有参数定义，仍然需要创建一个空的参数帧
                    errors.push(AnalyzeError::DetailedError(
                        node.clone(),
                        "LambdaDef without parameters".to_string(),
                    ));
                    check_postorder_break(
                        source,
                        node,
                        break_at_position,
                        context,
                        context_at_break,
                    );
                    return AssumedType::Lambda;
                }
                let params_def = params_def.unwrap();
                context.push_context();

                for var in PRE_ALLOCATED_VARIABLE_STRINGS {
                    context.define_variable(
                        var.to_string(),
                        Variable {
                            assumed_type: AssumedType::Unknown,
                        },
                    );
                }

                let mut captured_vars = captured_vars.clone();
                captured_vars.extend(params_def);

                for var in captured_vars {
                    // 将捕获的变量添加到 Lambda 的上下文中
                    context.define_variable(
                        var,
                        Variable {
                            assumed_type: AssumedType::Unknown,
                        },
                    );
                }

                // 在新的上下文中分析 Lambda 体
                if node.children.len() > 1 {
                    // Lambda 体可能创建自己的帧 (push_frame/pop_frame)
                    analyze_node(
                        source,
                        &node.children.last().unwrap(),
                        context,
                        errors,   // Pass errors
                        warnings, // Pass warnings
                        dynamic,
                        break_at_position,
                        context_at_break,
                    );
                    if context_at_break.is_some() {
                        // 如果在 Lambda 体分析中中断，不弹出上下文
                        return AssumedType::Lambda;
                    }
                }

                // 只有在没有中断的情况下才弹出 Lambda 的上下文
                if context_at_break.is_none() {
                    let _ = context.pop_context(); // 退出 Lambda，恢复到父上下文
                    check_postorder_break(
                        source,
                        node,
                        break_at_position,
                        context,
                        context_at_break,
                    );
                }
            }

            return AssumedType::Lambda;
        }
        ASTNodeType::Expressions => {
            let mut last_type = AssumedType::Unknown;
            for child in &node.children {
                last_type = analyze_node(
                    source,
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return last_type;
        }
        ASTNodeType::Assign => {
            if node.children.len() >= 2 {
                // Analyze RHS first
                let assumed_type = analyze_node(
                    source,
                    &node.children[1],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }

                // Analyze LHS (e.g., variable, index, attr) to check for errors, but its type doesn't override RHS
                analyze_node(
                    source,
                    &node.children[0],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }

                // TODO: Potentially update variable type in context if LHS is a simple variable?
                // This requires LHS analysis to return info about what was assigned to.
                // For now, assignment doesn't update context type info here.

                check_postorder_break(source, node, break_at_position, context, context_at_break);
                return assumed_type;
            }
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return AssumedType::Unknown;
        }
        ASTNodeType::Pair => {
            let _ = analyze_node(
                source,
                &node.children[0],
                context,
                errors,   // Pass errors
                warnings, // Pass warnings
                dynamic,
                break_at_position,
                context_at_break,
            ); // Key
            if context_at_break.is_some() {
                return AssumedType::KeyVal; // Return KeyVal even if break occurred inside
            }
            let _ = analyze_node(
                source,
                &node.children[1],
                context,
                errors,   // Pass errors
                warnings, // Pass warnings
                dynamic,
                break_at_position,
                context_at_break,
            ); // Value
            if context_at_break.is_some() {
                return AssumedType::KeyVal; // Return KeyVal even if break occurred inside
            }
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return AssumedType::KeyVal; // Return KeyVal type
        }
        ASTNodeType::Set => {
            for child in &node.children {
                // Iterate through all children for Set
                analyze_node(
                    source,
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Set; // Return Set even if break occurred inside
                }
            }
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return AssumedType::Set; // Return Set type
        }
        ASTNodeType::String(_) => {
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            AssumedType::String
        }
        ASTNodeType::Boolean(_) => {
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            AssumedType::Boolean
        }
        ASTNodeType::Number(_) => {
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            AssumedType::Number
        }
        ASTNodeType::Base64(_) => {
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            AssumedType::Base64
        }
        ASTNodeType::Null => {
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            AssumedType::Null
        }
        ASTNodeType::Undefined => {
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            AssumedType::Undefined
        }
        ASTNodeType::Range => {
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            AssumedType::Range
        }
        ASTNodeType::Is => {
            for child in &node.children {
                let _ = analyze_node(
                    source,
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return AssumedType::Boolean; // 'is' checks return a boolean
        }
        ASTNodeType::Dynamic => {
            let mut last_type = AssumedType::Unknown;
            for child in &node.children {
                last_type = analyze_node(
                    source,
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    true,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return last_type;
        }

        ASTNodeType::Static => {
            let mut last_type = AssumedType::Unknown;
            for child in &node.children {
                last_type = analyze_node(
                    source,
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    false,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return last_type;
        }
        _ => {
            let mut last_type = AssumedType::Unknown;
            for child in &node.children {
                last_type = analyze_node(
                    source,
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return last_type;
        }
    }
}

#[stacksafe::stacksafe]
fn analyze_tuple_params(
    source: &Option<Source>,
    params: &ASTNode,
    context: &mut VariableContext,
    errors: &mut Vec<AnalyzeError>,  // Renamed
    warnings: &mut Vec<AnalyzeWarn>, // Added
    dynamic: bool,
    break_at_position: Option<usize>,
    context_at_break: &mut Option<VariableContext>,
) -> Option<HashSet<String>> {
    // Check break condition before processing parameters
    if context_at_break.is_some() {
        return None;
    }

    analyze_node(
        source,
        params,
        context,
        errors,   // Pass errors
        warnings, // Pass warnings
        dynamic,
        break_at_position,
        context_at_break,
    );
    if context_at_break.is_some() {
        return None; // Return early if break occurred
    }

    let mut param_names = HashSet::new();

    match &params.node_type {
        ASTNodeType::Tuple => {
            for param in &params.children {
                match &param.node_type {
                    ASTNodeType::Pair => {
                        let key = &param.children[0];
                        if let ASTNodeType::String(key_name) = &key.node_type {
                            param_names.insert(key_name.clone());
                        }
                    }
                    ASTNodeType::String(name) => {
                        // Handle simple string parameters
                        param_names.insert(name.clone());
                    }
                    _ => {}
                }
            }
        }
        ASTNodeType::String(name) => {
            // Handle single string parameter
            param_names.insert(name.clone());
        }
        ASTNodeType::Pair => {
            let key = &params.children[0];
            // Handle key-value pairs
            if let ASTNodeType::String(key_name) = &key.node_type {
                param_names.insert(key_name.clone());
            }
        }
        _ => {}
    }
    Some(param_names)
}

pub fn auto_capture_and_rebuild(node: &ASTNode) -> (HashSet<String>, ASTNode) {
    // return a set of required variables and reconstructed ASTNode
    let mut context = VariableContext::new();
    context.push_context(); // Create a new context for the frame

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

/// 自动捕获上下文中的变量
#[stacksafe::stacksafe]
pub fn auto_capture(
    context: &mut VariableContext,
    node: &ASTNode,
    dynamic: bool,
) -> (HashSet<String>, ASTNode) {
    // return a set of required variables and reconstructed ASTNode

    match &node.node_type {
        ASTNodeType::Variable(var_name) => {
            if dynamic {
                return (HashSet::new(), node.clone()); // Dynamic variables are not captured
            }
            if context.get_variable_current_context(var_name).is_some() {
                // If the variable is already defined in the current context, we don't need to capture it
                return (HashSet::new(), node.clone());
            }
            let mut required_vars = HashSet::new();
            required_vars.insert(var_name.clone());
            (required_vars, node.clone())
        }

        ASTNodeType::Let(name) => {
            let mut required_vars = HashSet::new();
            let mut new_node = node.clone();

            let value_node = &node.children[0];
            new_node.children = Vec::new();
            // Analyze the value expression
            let (value_req_vars, new_value_node) = auto_capture(context, value_node, dynamic);
            required_vars.extend(value_req_vars);

            new_node.children.push(new_value_node);
            // Define the variable in the current context
            context.define_variable(
                name.clone(),
                Variable {
                    assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                },
            );
            (required_vars, new_node)
        }

        ASTNodeType::Frame => {
            // Create a new scope frame
            let mut required_vars = HashSet::new();
            let mut new_node = node.clone();

            context.push_frame();
            let (child_req_vars, new_child_node) =
                auto_capture(context, &node.children[0], dynamic);
            context
                .pop_frame()
                .unwrap_or_else(|_| panic!("Failed to pop frame after Body analysis"));

            required_vars.extend(child_req_vars);
            new_node.children = vec![new_child_node];

            (required_vars, new_node)
        }

        ASTNodeType::LambdaDef(is_dynamic_gen, captured_vars) => {
            let mut required_vars = HashSet::new();
            let params = &node.children[0];
            let body = &node.children[1];
            // 分析形参所需要的变量
            let (params_req_vars, new_params_node) = auto_capture(context, params, *is_dynamic_gen);
            required_vars.extend(params_req_vars);

            if !dynamic {
                for v in captured_vars {
                    if context.get_variable_current_context(&v).is_none() {
                        // 如果捕获的变量在当前上下文中未定义，添加到 required_vars
                        required_vars.insert(v.clone());
                    }
                }
            }

            if *is_dynamic_gen {
                // 分析body
                let (body_req_vars, new_body_node) = auto_capture(context, body, *is_dynamic_gen);
                required_vars.extend(body_req_vars);
                // 返回捕获的变量和重建的 ASTNode
                let rebuilt_ast_node = ASTNode {
                    node_type: ASTNodeType::LambdaDef(*is_dynamic_gen, captured_vars.clone()),
                    start_token: node.start_token.clone(),
                    end_token: node.end_token.clone(),
                    children: vec![new_params_node, new_body_node],
                };
                return (required_vars, rebuilt_ast_node);
            }

            // 然后我们分析 Lambda 体
            context.push_context(); // Create a new context for the Lambda body
            for var in PRE_ALLOCATED_VARIABLE_STRINGS {
                context.define_variable(
                    var.to_string(),
                    Variable {
                        assumed_type: AssumedType::Unknown,
                    },
                )
            }

            // 我们通过把形参定义和捕获的变量添加到上下文中来处理 Lambda 的捕获
            for var in captured_vars {
                context.define_variable(
                    var.clone(),
                    Variable {
                        assumed_type: AssumedType::Unknown,
                    },
                )
            }

            // 处理形参
            match &params.node_type {
                ASTNodeType::Pair => {
                    let param_name = &params.children[0];
                    if let ASTNodeType::String(var_name) = &param_name.node_type {
                        context.define_variable(
                            var_name.clone(),
                            Variable {
                                assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                            },
                        );
                    }
                }
                ASTNodeType::String(var_name) => {
                    // 如果是单个字符串参数，直接定义变量
                    context.define_variable(
                        var_name.clone(),
                        Variable {
                            assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                        },
                    );
                }
                ASTNodeType::Tuple => {
                    // 如果是元组参数，分析每个元素
                    for param in &params.children {
                        match &param.node_type {
                            ASTNodeType::Pair => {
                                let param_name = &param.children[0];
                                if let ASTNodeType::String(var_name) = &param_name.node_type {
                                    context.define_variable(
                                        var_name.clone(),
                                        Variable {
                                            assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                                        },
                                    );
                                }
                            }
                            ASTNodeType::String(var_name) => {
                                // 如果是单个字符串参数，直接定义变量
                                context.define_variable(
                                    var_name.clone(),
                                    Variable {
                                        assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                                    },
                                );
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }

            // 分析 Lambda 体
            let (body_req_vars, new_body_node) = auto_capture(context, body, *is_dynamic_gen);
            // 弹出 Lambda 的上下文帧
            context.pop_context().unwrap_or_else(|_| {
                panic!("Failed to pop context after Lambda definition");
            });

            // 如果不是动态生成的 Lambda，我们需要处理 Lambda 需要的捕获变量
            let mut rebuilt_lambda_required_vars = HashSet::new();
            rebuilt_lambda_required_vars.extend(captured_vars.clone());
            rebuilt_lambda_required_vars.extend(body_req_vars.clone());

            if !dynamic {
                for var in body_req_vars {
                    if context.get_variable_current_context(&var).is_none() {
                        // 如果 Lambda 体需要的变量在当前上下文中未定义，添加到 required_vars
                        required_vars.insert(var.clone());
                    }
                }
            }

            let rebuilt_ast_node = ASTNode {
                node_type: ASTNodeType::LambdaDef(
                    *is_dynamic_gen,
                    rebuilt_lambda_required_vars.into_iter().collect(),
                ),
                start_token: node.start_token.clone(),
                end_token: node.end_token.clone(),
                children: vec![new_params_node, new_body_node],
            };

            // 返回捕获的变量和重建的 ASTNode
            (required_vars, rebuilt_ast_node)
        }

        ASTNodeType::Dynamic => {
            let mut required_vars = HashSet::new();
            let mut new_node = node.clone();
            new_node.children = Vec::new();
            for child in &node.children {
                let (child_req_vars, new_child_node) = auto_capture(context, child, true);
                required_vars.extend(child_req_vars);
                new_node.children.push(new_child_node);
            }
            (required_vars, new_node)
        }

        ASTNodeType::Static => {
            let mut required_vars = HashSet::new();
            let mut new_node = node.clone();
            new_node.children = Vec::new();
            for child in &node.children {
                let (child_req_vars, new_child_node) = auto_capture(context, child, false);
                required_vars.extend(child_req_vars);
                new_node.children.push(new_child_node);
            }
            (required_vars, new_node)
        }

        // Default case for all other node types
        _ => {
            // Recursively analyze children and collect required variables
            let mut required_vars = HashSet::new();
            let mut new_node = node.clone();
            new_node.children = Vec::new();
            for child in &node.children {
                let (child_req_vars, new_child_node) = auto_capture(context, child, dynamic);
                required_vars.extend(child_req_vars);
                new_node.children.push(new_child_node);
            }
            (required_vars, new_node)
        }
    }
}
