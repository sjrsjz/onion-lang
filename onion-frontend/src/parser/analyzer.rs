use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    path::Path,
};

use crate::{
    compile::{build_code, compile_to_bytecode},
    dir_stack::DirectoryStack,
    parser::{
        ast::{ASTNodeType, ParserError, ast_token_stream, build_ast},
        diagnostics,
        lexer::{Source, lexer},
    },
    utils::cycle_detector::CycleDetector,
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
    pub name: String,
    pub assumed_type: AssumedType,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ASTMacro {
    pub name: String,
    pub matcher: ASTNode,
    pub replacement: ASTNode,
}

impl ASTMacro {
    pub fn if_matches(
        matcher: &ASTNode,
        node: &ASTNode,
        matched: &mut HashMap<String, ASTNode>,
    ) -> bool {
        // 检查 matcher 是否与 node 匹配
        if let ASTNodeType::Variable(name) = &matcher.node_type {
            if name.starts_with('_') {
                // 如果是变量名，检查是否已经匹配过
                if let Some(existing_node) = matched.get(name) {
                    // 如果已经匹配过，检查是否与当前节点相同
                    return existing_node == node;
                } else {
                    // 如果没有匹配过，将当前节点记录下来
                    matched.insert(name.clone(), node.clone());
                    return true; // 匹配成功
                }
            }
        }
        if matcher.node_type == node.node_type {
            // 检查子节点数量是否相同
            if matcher.children.len() == node.children.len() {
                // 递归检查每个子节点
                for (m_child, n_child) in matcher.children.iter().zip(&node.children) {
                    if !Self::if_matches(m_child, n_child, matched) {
                        return false; // 如果任何子节点不匹配，则返回 false
                    }
                }
                return true; // 所有子节点都匹配
            }
        }
        false
    }

    pub fn fill_matched(node: &ASTNode, matched: &mut HashMap<String, ASTNode>) -> ASTNode {
        // 检查 matcher 是否与 node 匹配
        if let ASTNodeType::Variable(name) = &node.node_type {
            if name.starts_with('_') {
                // 如果是变量名，检查是否已经匹配过
                if let Some(existing_node) = matched.get(name) {
                    // 如果已经获得了匹配的节点，返回它
                    return existing_node.clone();
                } else {
                    return node.clone(); // 返回当前节点
                }
            }
        }

        let mut new_children = Vec::new();
        // 递归处理子节点
        for child in &node.children {
            let filled_child = Self::fill_matched(child, matched);
            new_children.push(filled_child);
        }
        ASTNode {
            node_type: node.node_type.clone(),
            start_token: node.start_token.clone(),
            end_token: node.end_token.clone(),
            children: new_children,
        }
    }

    pub fn replace(matcher: &ASTNode, replacement: &ASTNode, node: &ASTNode) -> Option<ASTNode> {
        // 检查子节点
        let mut new_node = node.clone();
        let mut new_children = Vec::new();
        for child in &node.children {
            if let Some(replaced_child) = Self::replace(matcher, replacement, child) {
                new_children.push(replaced_child);
            } else {
                new_children.push(child.clone());
            }
        }
        new_node.children = new_children;

        let mut matched: HashMap<String, ASTNode> = HashMap::new();
        // 遍历所有匹配对
        if Self::if_matches(matcher, &new_node, &mut matched) {
            // 如果匹配成功，返回替换后的节点
            new_node = Self::fill_matched(replacement, &mut matched);
        }
        if new_node != *node {
            return Some(new_node);
        }
        None
    }

    pub fn replace_all(matcher: &ASTNode, replacement: &ASTNode, node: &ASTNode) -> ASTNode {
        Self::replace(matcher, replacement, &node).unwrap_or(node.clone())
    }

    pub fn build_from(node: &ASTNode) -> Result<ASTMacro, AnalyzeError> {
        // 强制要求为 ASTNodeType::Let
        if let ASTNodeType::Let(var_name) = &node.node_type {
            // 解析 ASTNodeType::Let 的子节点
            let ast_def = node.children.get(0).ok_or_else(|| {
                AnalyzeError::InvalidMacroDefinition(
                    node.clone(),
                    "Expected a child node for ASTDefinition".to_string(),
                )
            })?;

            // 确保子节点是 ASTNodeType::KeyValue
            if let ASTNodeType::KeyValue = &ast_def.node_type {
                // node[0] 作为匹配节点，node[1] 作为替换节点
                let matcher = ast_def.children.get(0).ok_or_else(|| {
                    AnalyzeError::InvalidMacroDefinition(
                        node.clone(),
                        "Expected a matching node in ASTDefinition".to_string(),
                    )
                })?;
                let replacement = ast_def.children.get(1).ok_or_else(|| {
                    AnalyzeError::InvalidMacroDefinition(
                        node.clone(),
                        "Expected a replacement node in ASTDefinition".to_string(),
                    )
                })?;
                // 返回 ASTDefinition 实例
                return Ok(ASTMacro {
                    name: var_name.clone(),
                    matcher: matcher.clone(),
                    replacement: replacement.clone(),
                });
            }
        }
        Err(AnalyzeError::InvalidMacroDefinition(
            node.clone(),
            format!(
                "Except ASTNodeType::Let with KeyValue child, found: {:?}",
                node
            ),
        ))
    }
}

#[derive(Debug, Clone)] // Added Clone
pub struct VariableFrame {
    pub variables: Vec<Variable>,
    pub ast_macros: Vec<ASTMacro>, // 存储 AST 定义
}

impl VariableFrame {
    pub fn define_variable(&mut self, var: Variable) -> Result<(), String> {
        if let Some(existing_var) = self.variables.iter_mut().find(|v| v.name == var.name) {
            existing_var.assumed_type = var.assumed_type;
        } else {
            self.variables.push(var);
        }
        Ok(())
    }

    pub fn define_macro(&mut self, macro_def: ASTMacro) -> Result<(), String> {
        if let Some(existing_macro) = self
            .ast_macros
            .iter_mut()
            .find(|m| m.name == macro_def.name)
        {
            // 如果宏已存在，更新其内容
            existing_macro.matcher = macro_def.matcher;
            existing_macro.replacement = macro_def.replacement;
        } else {
            // 否则添加新的宏定义
            self.ast_macros.push(macro_def);
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

#[derive(Debug)]
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

#[derive(Debug)]
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
                variables: Vec::new(),
                ast_macros: Vec::new(),
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

    pub fn define_variable(&mut self, var: &Variable) -> Result<(), String> {
        // 在当前上下文的最后一个（即当前）帧中定义变量
        if let Some(frame) = self.current_context_mut().last_mut() {
            frame
                .define_variable(var.clone())
                .map_err(|e| format!("Failed to define variable '{}': {}", var.name, e))?;
        } else {
            // This case should ideally not happen if a context always has at least one frame
            return Err("No frame available in the current context to define variable".to_string());
        }
        Ok(())
    }

    pub fn define_macro(&mut self, macro_def: &ASTMacro) -> Result<(), String> {
        // 在当前上下文的最后一个（即当前）帧中定义 AST 宏
        if let Some(frame) = self.current_context_mut().last_mut() {
            frame
                .define_macro(macro_def.clone())
                .map_err(|e| format!("Failed to define macro '{}': {}", macro_def.name, e))?;
        } else {
            // This case should ideally not happen if a context always has at least one frame
            return Err("No frame available in the current context to define variable".to_string());
        }
        Ok(())
    }

    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        for frames in self.contexts.iter().rev() {
            for frame in frames.iter().rev() {
                for var in frame.variables.iter().rev() {
                    if var.name == name {
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
            for v in var.variables.iter() {
                if v.name == name {
                    return Some(v);
                }
            }
        }
        None
    }

    pub fn get_marco(&self, name: &str) -> Option<&ASTMacro> {
        // 反向遍历所有上下文
        for context_frames in self.contexts.iter().rev() {
            // 在每个上下文中反向遍历所有帧
            for frame in context_frames.iter().rev() {
                for macro_def in frame.ast_macros.iter() {
                    if macro_def.name == name {
                        return Some(macro_def);
                    }
                }
            }
        }
        None
    }

    // 在当前上下文中推入一个新的帧
    pub fn push_frame(&mut self) {
        self.current_context_mut().push(VariableFrame {
            variables: Vec::new(),
            ast_macros: Vec::new(),
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
            variables: Vec::new(),
            ast_macros: Vec::new(),
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

pub fn expand_macro(
    node: &ASTNode,
    cycle_detector: &mut CycleDetector<String>,
    dir_stack: &mut DirectoryStack,
) -> MacroAnalysisOutput {
    let new_node = match expand_import_to_node(node, cycle_detector, dir_stack) {
        Ok(n) => n,
        Err(e) => {
            return MacroAnalysisOutput {
                errors: vec![AnalyzeError::DetailedError(node.clone(), e.to_string())],
                warnings: Vec::new(),
                result_node: node.clone(),
            };
        }
    };
    // 遍历所有宏定义，尝试应用到当前节点
    let mut context = VariableContext::new();
    context.push_context();

    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    let result_node = expand_macros_to_node(&new_node, &mut errors, &mut warnings, &mut context);
    MacroAnalysisOutput {
        errors,
        warnings,
        result_node,
    }
}

pub fn expand_macros_to_node(
    node: &ASTNode,
    errors: &mut Vec<AnalyzeError>,
    warnings: &mut Vec<AnalyzeWarn>,
    context: &mut VariableContext,
) -> ASTNode {
    match &node.node_type {
        ASTNodeType::Annotation(annotation) => {
            match annotation.as_str() {
                "macro" => {
                    let def = match node.children.get(0) {
                        Some(v) => v,
                        None => {
                            errors.push(AnalyzeError::InvalidMacroDefinition(
                                node.clone(),
                                "Macro has no child".to_string(),
                            ));
                            return ASTNode {
                                node_type: ASTNodeType::Undefined,
                                start_token: None,
                                end_token: None,
                                children: Vec::new(),
                            };
                        }
                    };
                    match ASTMacro::build_from(def) {
                        Ok(ast_def) => {
                            // 将 AST 定义添加到当前上下文的最后一个帧中
                            if let Err(e) = context.define_macro(&ast_def) {
                                // 使用 errors Vec 记录宏定义错误
                                errors.push(AnalyzeError::InvalidMacroDefinition(node.clone(), e));
                            }
                        }
                        Err(e) => {
                            errors.push(e); // 使用 errors Vec
                        }
                    }
                    return ASTNode {
                        node_type: ASTNodeType::Undefined,
                        start_token: None,
                        end_token: None,
                        children: Vec::new(),
                    };
                }
                "compile" | "dynamic" | "static" | "required" => {}
                _ => {
                    // 对于其他注解，应用宏
                    let sub_node = match node.children.first() {
                        Some(v) => v,
                        None => {
                            errors.push(AnalyzeError::DetailedError(
                                node.clone(),
                                format!("Annotation '{}' has no child", annotation),
                            ));
                            return ASTNode {
                                node_type: ASTNodeType::Undefined,
                                start_token: None,
                                end_token: None,
                                children: Vec::new(),
                            };
                        }
                    };
                    // 默认认定为macro应用
                    match context.get_marco(annotation) {
                        Some(ast_macro) => {
                            // 使用 ASTMacro 替换当前节点
                            let replaced_node = ASTMacro::replace_all(
                                &ast_macro.matcher,
                                &ast_macro.replacement,
                                sub_node,
                            );
                            return expand_macros_to_node(
                                &replaced_node,
                                errors,
                                warnings,
                                context,
                            );
                        }
                        None => {
                            // 如果没有找到宏定义，记录警告
                            let error_message = format!(
                                "Macro '{}' not defined in the current context",
                                annotation
                            );
                            errors.push(AnalyzeError::InvalidMacroDefinition(
                                node.clone(),
                                error_message,
                            ));
                            return ASTNode {
                                node_type: ASTNodeType::Undefined,
                                start_token: None,
                                end_token: None,
                                children: Vec::new(),
                            };
                        }
                    }
                }
            }
        }
        ASTNodeType::Body => {
            context.push_frame(); // 进入新的上下文帧
            let mut new_children = Vec::new();
            for child in &node.children {
                let applied_child = expand_macros_to_node(child, errors, warnings, context);
                new_children.push(applied_child);
            }
            context.pop_frame().unwrap_or_else(|e| {
                errors.push(AnalyzeError::DetailedError(node.clone(), e.to_string()));
            });
            return ASTNode {
                node_type: ASTNodeType::Body,
                start_token: node.start_token.clone(),
                end_token: node.end_token.clone(),
                children: new_children,
            };
        }
        ASTNodeType::LambdaDef(is_dynamic_gen, capture_vars) => {
            // 在新的上下文中处理参数（参数定义在第一个帧中）
            let Some(params) = node.children.first() else {
                errors.push(AnalyzeError::DetailedError(
                    node.clone(),
                    "LambdaDef has no parameters".to_string(),
                ));
                return ASTNode {
                    node_type: ASTNodeType::Undefined,
                    start_token: None,
                    end_token: None,
                    children: Vec::new(),
                };
            };
            let args_node = expand_macros_to_node(
                params, errors,   // Pass errors
                warnings, // Pass warnings
                context,
            );

            let body_node;
            if *is_dynamic_gen {
                body_node = expand_macros_to_node(
                    node.children.last().unwrap(),
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    context,
                );
            } else {
                context.push_context();
                body_node = expand_macros_to_node(
                    node.children.last().unwrap(),
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    context,
                );
                context.pop_context().unwrap_or_else(|e| {
                    errors.push(AnalyzeError::DetailedError(node.clone(), e.to_string()));
                });
            }
            return ASTNode {
                node_type: ASTNodeType::LambdaDef(*is_dynamic_gen, capture_vars.clone()),
                start_token: node.start_token.clone(),
                end_token: node.end_token.clone(),
                children: vec![args_node, body_node],
            };
        }
        _ => {}
    }
    // 对于其他节点类型，递归应用宏
    let mut new_children = Vec::new();
    for child in &node.children {
        let applied_child = expand_macros_to_node(child, errors, warnings, context);
        new_children.push(applied_child);
    }
    return ASTNode {
        node_type: node.node_type.clone(),
        start_token: node.start_token.clone(),
        end_token: node.end_token.clone(),
        children: new_children,
    };
}

pub fn analyze_ast(
    ast: &ASTNode,
    break_at_position: Option<usize>,
    cycle_detector: &mut CycleDetector<String>,
    dir_stack: &mut DirectoryStack,
) -> AnalysisOutput {
    let mut context = VariableContext::new();
    // 向context里初始化内置函数
    context.push_context();
    context.push_frame();
    let _ = context.define_variable(&Variable {
        name: "this".to_string(),
        assumed_type: AssumedType::Lambda,
    });
    let _ = context.define_variable(&Variable {
        name: "self".to_string(),
        assumed_type: AssumedType::Tuple,
    });
    let _ = context.define_variable(&Variable {
        name: "arguments".to_string(),
        assumed_type: AssumedType::Tuple,
    });

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
        cycle_detector,
        dir_stack,
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

fn analyze_node(
    source: &Option<Source>,
    node: &ASTNode,
    context: &mut VariableContext,
    errors: &mut Vec<AnalyzeError>,
    warnings: &mut Vec<AnalyzeWarn>,
    dynamic: bool,
    break_at_position: Option<usize>,
    context_at_break: &mut Option<VariableContext>,
    cycle_detector: &mut CycleDetector<String>,
    dir_stack: &mut DirectoryStack,
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
                    cycle_detector,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                } // Check if break occurred during value analysis
                let var = Variable {
                    name: var_name.clone(),
                    assumed_type: assumed_type.clone(),
                };
                context.define_variable(&var).unwrap_or_else(|e| {
                    errors.push(AnalyzeError::DetailedError(node.clone(), e.to_string()));
                });

                // Post-order check after processing the let statement
                check_postorder_break(source, node, break_at_position, context, context_at_break);
                assumed_type
            } else {
                // Post-order check for incomplete let statements
                check_postorder_break(source, node, break_at_position, context, context_at_break);
                AssumedType::Unknown
            }
        }
        ASTNodeType::Annotation(annotation) => {
            let mut assumed_type = AssumedType::Unknown;
            let is_dynamic = match annotation.as_str() {
                "dynamic" => true,
                "static" => false,
                _ => dynamic,
            };

            match annotation.as_str() {
                "macro" => {
                    return AssumedType::Unknown; // @macro 注解本身不贡献类型，并且不会继续分析子节点
                }
                "compile" => {
                    if break_at_position.is_none() {
                        // 特殊处理 @compile 注解 (当非断点分析时)
                        for child in &node.children {
                            // 检查子节点是否为字符串
                            if let ASTNodeType::String(file_path) = &child.node_type {
                                let absolute_path = dir_stack.translate(&Path::new(file_path));
                                let Ok(absolute_path) = absolute_path else {
                                    // 使用 AnalyzeWarn::CompileError 记录路径解析错误
                                    let error_message = format!(
                                        "Failed to resolve path '{}': Invalid path",
                                        file_path
                                    );
                                    warnings.push(AnalyzeWarn::CompileError(
                                        child.clone(),
                                        error_message,
                                    ));
                                    continue; // 跳过此子节点
                                };

                                match cycle_detector
                                    .visit(absolute_path.to_str().unwrap_or("").to_string())
                                {
                                    Ok(mut guard) => {
                                        let read_file = std::fs::read_to_string(absolute_path);
                                        if let Err(e) = read_file {
                                            // 使用 AnalyzeWarn::CompileError 记录读取错误
                                            let error_message = format!(
                                                "Failed to read file '{}': {}",
                                                file_path, e
                                            );
                                            warnings.push(AnalyzeWarn::CompileError(
                                                child.clone(),
                                                error_message,
                                            ));
                                        // 使用 child 作为错误关联的节点
                                        } else {
                                            let _ = dir_stack.push_file(&Path::new(file_path));

                                            let code = read_file.unwrap();
                                            // 调用 build_code 函数编译代码
                                            let compile_result = build_code(
                                                &code,
                                                guard.get_detector_mut(),
                                                dir_stack,
                                            );
                                            // 弹出目录栈
                                            let _ = dir_stack.pop();

                                            match compile_result {
                                                Ok(ir_package) => {
                                                    if context_at_break.is_none() {
                                                        // 替换文件扩展名为onionc
                                                        let onionc_file_path = if let Some(pos) =
                                                            file_path.rfind('.')
                                                        {
                                                            format!("{}.onionc", &file_path[..pos])
                                                        } else {
                                                            format!("{}.onionc", file_path)
                                                        };
                                                        let byte_code =
                                                            compile_to_bytecode(&ir_package);
                                                        match byte_code {
                                                            Ok(byte_code) => {
                                                                // 将字节码写入文件
                                                                if let Err(e) = byte_code
                                                                    .write_to_file(
                                                                        &onionc_file_path,
                                                                    )
                                                                {
                                                                    // 使用 AnalyzeWarn::CompileError 记录写入错误
                                                                    let error_message = format!(
                                                                        "Failed to write bytecode to file '{}': {}",
                                                                        onionc_file_path, e
                                                                    );
                                                                    warnings.push(
                                                                        AnalyzeWarn::CompileError(
                                                                            child.clone(),
                                                                            error_message,
                                                                        ),
                                                                    ); // 使用 child 作为错误关联的节点
                                                                }
                                                            }
                                                            Err(e) => {
                                                                // 使用 AnalyzeWarn::CompileError 记录编译错误
                                                                let error_message = format!(
                                                                    "Compilation failed for file '{}': {}",
                                                                    file_path, e
                                                                );
                                                                warnings.push(
                                                                    AnalyzeWarn::CompileError(
                                                                        child.clone(),
                                                                        error_message,
                                                                    ),
                                                                ); // 使用 child 作为错误关联的节点
                                                            }
                                                        }
                                                    }
                                                }
                                                Err(e) => {
                                                    // 使用 AnalyzeWarn::CompileError 记录编译错误
                                                    let error_message = format!(
                                                        "Compilation failed for file '{}': {}",
                                                        file_path, e
                                                    );
                                                    warnings.push(AnalyzeWarn::CompileError(
                                                        child.clone(),
                                                        error_message,
                                                    ));
                                                    // 使用 child 作为错误关联的节点
                                                }
                                            }
                                        }
                                    }
                                    Err(e) => {
                                        // 使用 AnalyzeWarn::CompileError 记录循环检测错误
                                        let error_message = format!(
                                            "Cycle detected in file '{}': {}",
                                            file_path, e
                                        );
                                        warnings.push(AnalyzeWarn::CompileError(
                                            child.clone(),
                                            error_message,
                                        ));
                                        continue;
                                    }
                                }
                            } else {
                                // @compile 后面应该跟一个字符串字面量
                                let error_message = format!(
                                    "@compile annotation expects a string literal file path, found: {:?}",
                                    child.node_type
                                );
                                warnings
                                    .push(AnalyzeWarn::CompileError(child.clone(), error_message));
                                // 使用 child 作为错误关联的节点
                            }
                            // 仍然分析子节点本身，即使它是 @compile 的参数
                            analyze_node(
                                source,
                                child,
                                context,
                                errors,   // Pass errors
                                warnings, // Pass warnings
                                dynamic,  // 继承 dynamic 状态或根据需要调整
                                break_at_position,
                                context_at_break,
                                cycle_detector,
                                dir_stack,
                            );
                            if context_at_break.is_some() {
                                return AssumedType::Unknown;
                            }
                        }
                        // @compile 注解本身不贡献类型
                        return AssumedType::Unknown;
                    } else {
                        // 仅检查文件是否存在 (断点分析模式)
                        for child in &node.children {
                            if let ASTNodeType::String(file_path) = &child.node_type {
                                if !std::path::Path::new(file_path).exists() {
                                    let error_message = format!(
                                        "File specified in @compile not found: '{}'",
                                        file_path
                                    );
                                    warnings.push(AnalyzeWarn::CompileError(
                                        child.clone(),
                                        error_message,
                                    ));
                                    // 使用 child 作为错误关联的节点
                                }
                            } else {
                                // @compile 后面应该跟一个字符串字面量
                                let error_message = format!(
                                    "@compile annotation expects a string literal file path, found: {:?}",
                                    child.node_type
                                );
                                warnings
                                    .push(AnalyzeWarn::CompileError(child.clone(), error_message));
                                // 使用 child 作为错误关联的节点
                            }
                            // 仍然分析子节点，以允许在路径字符串内部设置断点
                            analyze_node(
                                source,
                                child,
                                context,
                                errors,
                                warnings,
                                dynamic, // 继承 dynamic 状态
                                break_at_position,
                                context_at_break,
                                cycle_detector,
                                dir_stack,
                            );
                            if context_at_break.is_some() {
                                return AssumedType::Unknown;
                            }
                        }
                        // 在断点模式下，@compile 也不贡献类型
                        return AssumedType::Unknown;
                    }
                }

                "required" => {
                    // 处理 @required 注解的特殊情况，如果变量不存在，添加占位符
                    if let Some(first_child) = node.children.first() {
                        if let ASTNodeType::Variable(var_name) = &first_child.node_type {
                            if context.get_variable_current_context(var_name).is_none() {
                                // 如果变量不存在，添加一个占位符
                                let var = Variable {
                                    name: var_name.clone(),
                                    assumed_type: AssumedType::Unknown,
                                };
                                let _ = context.define_variable(&var);
                                return AssumedType::Unknown; // @required 注解本身不贡献类型
                            }
                        } else {
                            // @required 后面应该跟一个变量名
                            let error_message = format!(
                                "@required annotation expects a variable name, found: {:?}",
                                first_child.node_type
                            );
                            warnings.push(AnalyzeWarn::CompileError(
                                first_child.clone(),
                                error_message,
                            ));
                            // 使用 first_child 作为错误关联的节点
                        }
                    }
                }

                "dynamic" | "static" => {}

                _ => {
                    // 对于其他注解，记录警告
                    let error_message = format!(
                        "Unsolved macro '{}' in node {:?}",
                        annotation, node.node_type
                    );
                    errors.push(AnalyzeError::InvalidMacroDefinition(
                        node.clone(),
                        error_message,
                    ));
                }
            } // 对非 @compile 注解或在断点分析时的 @compile 注解执行常规分析
            for child in &node.children {
                assumed_type = analyze_node(
                    source,
                    child,
                    context,
                    errors,     // Pass errors
                    warnings,   // Pass warnings
                    is_dynamic, // 使用计算出的 dynamic 状态
                    break_at_position,
                    context_at_break,
                    cycle_detector,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }

            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return assumed_type;
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
        ASTNodeType::Body => {
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
                    cycle_detector,
                    dir_stack,
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
                cycle_detector,
                dir_stack,
            );
            if context_at_break.is_some() {
                // 如果在参数分析中中断，不弹出上下文
                return AssumedType::Lambda;
            }

            if *is_dynamic_gen {
                context.push_frame();
                analyze_node(
                    source,
                    &node.children.last().unwrap(),
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    cycle_detector,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    // 如果在 Lambda 体分析中中断，不弹出上下文
                    return AssumedType::Lambda;
                }
                // 弹出 Lambda 定义的帧
                if context.pop_frame().is_err() {
                    panic!("Failed to pop frame after Lambda definition");
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

                for vars in PRE_ALLOCATED_VARIABLE_STRINGS {
                    context
                        .define_variable(&Variable {
                            name: vars.to_string(),
                            assumed_type: AssumedType::Unknown,
                        })
                        .unwrap_or_else(|err| {
                            panic!(
                                "Failed to define pre-allocated variable '{}': {}",
                                vars, err
                            );
                        });
                }

                let mut captured_vars = captured_vars.clone();
                captured_vars.extend(params_def);

                for var in captured_vars {
                    // 将捕获的变量添加到 Lambda 的上下文中
                    context
                        .define_variable(&Variable {
                            name: var.clone(),
                            assumed_type: AssumedType::Unknown,
                        })
                        .unwrap_or_else(|err| {
                            panic!("Failed to define captured variable '{}': {}", var, err);
                        });
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
                        cycle_detector,
                        dir_stack,
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
                    cycle_detector,
                    dir_stack,
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
                    cycle_detector,
                    dir_stack,
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
                    cycle_detector,
                    dir_stack,
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
        ASTNodeType::KeyValue => {
            let _ = analyze_node(
                source,
                &node.children[0],
                context,
                errors,   // Pass errors
                warnings, // Pass warnings
                dynamic,
                break_at_position,
                context_at_break,
                cycle_detector,
                dir_stack,
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
                cycle_detector,
                dir_stack,
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
                    cycle_detector,
                    dir_stack,
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
                    cycle_detector,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            check_postorder_break(source, node, break_at_position, context, context_at_break);
            return AssumedType::Boolean; // 'is' checks return a boolean
        } // Default case for other node types
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
                    cycle_detector,
                    dir_stack,
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

fn analyze_tuple_params(
    source: &Option<Source>,
    params: &ASTNode,
    context: &mut VariableContext,
    errors: &mut Vec<AnalyzeError>,  // Renamed
    warnings: &mut Vec<AnalyzeWarn>, // Added
    dynamic: bool,
    break_at_position: Option<usize>,
    context_at_break: &mut Option<VariableContext>,
    cycle_detector: &mut CycleDetector<String>,
    dir_stack: &mut DirectoryStack,
) -> Option<HashSet<String>> {
    // Check break condition before processing parameters
    if context_at_break.is_some() {
        return None;
    }

    for node in &params.children {
        analyze_node(
            source,
            node,
            context,
            errors,   // Pass errors
            warnings, // Pass warnings
            dynamic,
            break_at_position,
            context_at_break,
            cycle_detector,
            dir_stack,
        );
        if context_at_break.is_some() {
            return None; // Return early if break occurred
        }
    }

    let mut param_names = HashSet::new();

    match &params.node_type {
        ASTNodeType::Tuple => {
            for param in &params.children {
                match &param.node_type {
                    ASTNodeType::KeyValue => {
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
        ASTNodeType::KeyValue => {
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
    context.push_frame(); // Create a new frame for the context

    for var in PRE_ALLOCATED_VARIABLE_STRINGS {
        context
            .define_variable(&Variable {
                name: var.to_string(),
                assumed_type: AssumedType::Unknown,
            })
            .unwrap_or_else(|err| {
                panic!("Failed to define pre-allocated variable '{}': {}", var, err);
            });
    }

    auto_capture(&mut context, node, false)
}

/// 筛查掉当前上下文中已定义的变量
pub fn filter_required_variables(
    context: &VariableContext,
    required_vars: HashSet<String>,
) -> HashSet<String> {
    // Filter out variables that are already defined in the current context
    required_vars
        .into_iter()
        .filter(|var| context.get_variable_current_context(var).is_none())
        .collect()
}

/// 自动捕获上下文中的变量
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
            let mut required_vars = HashSet::new();
            required_vars.insert(var_name.clone());
            required_vars = filter_required_variables(context, required_vars);
            (required_vars, node.clone())
        }

        ASTNodeType::Annotation(annotation) => {
            let dynamic_node = match annotation.as_str() {
                "dynamic" => true,
                "static" => false,
                _ => dynamic, // Inherit dynamic status for other annotations
            };
            if annotation == "required" {
                // If it's a required annotation, we need to capture the variable
                if let Some(var_name) = node.children.get(0) {
                    if let ASTNodeType::Variable(var_name_str) = &var_name.node_type {
                        let mut required_vars = HashSet::new();
                        required_vars.insert(var_name_str.clone());
                        required_vars = filter_required_variables(context, required_vars);
                        context
                            .define_variable(&Variable {
                                name: var_name_str.clone(),
                                assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                            })
                            .unwrap_or_else(|err| {
                                panic!(
                                    "Failed to define captured variable '{}' in Annotation: {}",
                                    var_name_str, err
                                );
                            });

                        return (required_vars, node.clone()); // Still return required_vars because it's required
                    }
                }
            }
            let mut required_vars = HashSet::new();
            let mut new_node = node.clone();
            new_node.children = Vec::new();
            // Analyze children with the inherited dynamic status
            for child in &node.children {
                let (child_req_vars, new_child_node) = auto_capture(context, child, dynamic_node);
                required_vars.extend(child_req_vars);
                new_node.children.push(new_child_node);
            }
            required_vars = filter_required_variables(context, required_vars);
            (required_vars, new_node)
        }

        ASTNodeType::Let(name) => {
            let mut required_vars = HashSet::new();
            let mut new_node = node.clone();

            let value_node = &node.children[0];
            new_node.children = Vec::new();
            // Analyze the value expression
            let (value_req_vars, new_value_node) = auto_capture(context, value_node, dynamic);
            required_vars.extend(value_req_vars);
            required_vars = filter_required_variables(context, required_vars); // 先筛选后定义

            new_node.children.push(new_value_node);
            // Define the variable in the current context
            let var = Variable {
                name: name.clone(),
                assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
            };
            context.define_variable(&var).unwrap_or_else(|err| {
                panic!("Failed to define variable '{}' in Let: {}", var.name, err);
            });
            (required_vars, new_node)
        }

        ASTNodeType::Body => {
            // Create a new scope frame
            context.push_frame();
            let mut required_vars = HashSet::new();
            let mut new_node = node.clone();
            new_node.children = Vec::new();

            // Analyze children within the new scope
            for child in &node.children {
                let (child_req_vars, new_child_node) = auto_capture(context, child, dynamic);
                required_vars.extend(child_req_vars);
                new_node.children.push(new_child_node);
            }
            // Pop the scope frame
            context
                .pop_frame()
                .unwrap_or_else(|_| panic!("Failed to pop frame after Body analysis"));
            required_vars = filter_required_variables(context, required_vars);
            (required_vars, new_node)
        }

        ASTNodeType::LambdaDef(is_dynamic_gen, captured_vars) => {
            let mut required_vars = HashSet::new();
            let params = &node.children[0];
            let body = &node.children[1];
            // 分析形参所需要的变量
            let (params_req_vars, new_params_node) = auto_capture(context, params, *is_dynamic_gen);
            required_vars.extend(params_req_vars);
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
                required_vars = filter_required_variables(context, required_vars);
                return (required_vars, rebuilt_ast_node);
            }

            // 如果不是动态生成的 Lambda，我们需要处理捕获的变量
            let mut lambda_body_required_vars = HashSet::new();
            lambda_body_required_vars.extend(captured_vars.iter().cloned());

            // 然后我们分析 Lambda 体
            context.push_context(); // Create a new context for the Lambda body
            for var in PRE_ALLOCATED_VARIABLE_STRINGS {
                context
                    .define_variable(&Variable {
                        name: var.to_string(),
                        assumed_type: AssumedType::Unknown,
                    })
                    .unwrap_or_else(|err| {
                        panic!("Failed to define pre-allocated variable '{}': {}", var, err);
                    });
            }

            // 我们通过把形参定义和捕获的变量添加到上下文中来处理 Lambda 的捕获
            for var in captured_vars {
                context
                    .define_variable(&Variable {
                        name: var.clone(),
                        assumed_type: AssumedType::Unknown,
                    })
                    .unwrap_or_else(|err| {
                        panic!(
                            "Failed to define captured variable '{}' in Lambda: {}",
                            var, err
                        );
                    });
            }

            // 处理形参
            // 我们这里
            match &params.node_type {
                ASTNodeType::KeyValue => {
                    let param_name = &params.children[0];
                    if let ASTNodeType::String(var_name) = &param_name.node_type {
                        let var = Variable {
                            name: var_name.clone(),
                            assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                        };
                        context.define_variable(&var).unwrap_or_else(|err| {
                            panic!(
                                "Failed to define variable '{}' in Lambda parameters: {}",
                                var_name, err
                            );
                        });
                    }
                }
                ASTNodeType::String(var_name) => {
                    // 如果是单个字符串参数，直接定义变量
                    let var = Variable {
                        name: var_name.clone(),
                        assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                    };
                    context.define_variable(&var).unwrap_or_else(|err| {
                        panic!(
                            "Failed to define variable '{}' in Lambda parameters: {}",
                            var_name, err
                        );
                    });
                }
                ASTNodeType::Tuple => {
                    // 如果是元组参数，分析每个元素
                    for param in &params.children {
                        match &param.node_type {
                            ASTNodeType::KeyValue => {
                                let param_name = &param.children[0];
                                if let ASTNodeType::String(var_name) = &param_name.node_type {
                                    let var = Variable {
                                        name: var_name.clone(),
                                        assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                                    };
                                    context.define_variable(&var).unwrap_or_else(|err| {
                                        panic!(
                                            "Failed to define variable '{}' in Lambda parameters: {}",
                                            var_name, err
                                        );
                                    });
                                }
                            }
                            ASTNodeType::String(var_name) => {
                                // 如果是单个字符串参数，直接定义变量
                                let var = Variable {
                                    name: var_name.clone(),
                                    assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                                };
                                context.define_variable(&var).unwrap_or_else(|err| {
                                    panic!(
                                        "Failed to define variable '{}' in Lambda parameters: {}",
                                        var_name, err
                                    );
                                });
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }

            // 分析 Lambda 体
            let (body_req_vars, new_body_node) = auto_capture(context, body, *is_dynamic_gen);

            lambda_body_required_vars.extend(body_req_vars);
            required_vars.extend(lambda_body_required_vars.clone());

            let rebuilt_ast_node = ASTNode {
                node_type: ASTNodeType::LambdaDef(
                    *is_dynamic_gen,
                    lambda_body_required_vars.into_iter().collect(),
                ),
                start_token: node.start_token.clone(),
                end_token: node.end_token.clone(),
                children: vec![new_params_node, new_body_node],
            };
            // 弹出 Lambda 的上下文帧
            context.pop_context().unwrap_or_else(|_| {
                panic!("Failed to pop context after Lambda definition");
            });
            required_vars = filter_required_variables(context, required_vars);
            // 返回捕获的变量和重建的 ASTNode
            (required_vars, rebuilt_ast_node)
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
            required_vars = filter_required_variables(context, required_vars);
            (required_vars, new_node)
        }
    }
}

fn import_ast_node(
    code: &str,
    cycle_detector: &mut CycleDetector<String>,
    dir_stack: &mut DirectoryStack,
) -> Result<ASTNode, AnalyzeError> {
    let tokens = lexer::tokenize(code);
    let tokens = lexer::reject_comment(&tokens);
    let gathered = ast_token_stream::from_stream(&tokens);
    let ast = build_ast(gathered).map_err(AnalyzeError::ParserError)?;
    let new_ast = expand_import_to_node(&ast, cycle_detector, dir_stack)
        .map_err(|err| AnalyzeError::DetailedError(ast, err))?;
    return Ok(new_ast.into());
}

pub fn expand_import_to_node(
    node: &ASTNode,
    cycle_detector: &mut CycleDetector<String>,
    dir_stack: &mut DirectoryStack,
) -> Result<ASTNode, String> {
    match &node.node_type {
        ASTNodeType::Annotation(annotation) => {
            match annotation.as_str() {
                "import" => {
                    // 处理 import 注解
                    if node.children.len() != 1 {
                        return Err("Import annotation requires one argument".to_string());
                    }
                    let import_path = &node.children[0];
                    if let ASTNodeType::String(path) = &import_path.node_type {
                        let absolute_path =
                            dir_stack.translate(&Path::new(path)).map_err(|err| {
                                format!(
                                    "Failed to get absolute path for import '{}': {}",
                                    path, err
                                )
                            })?;
                        match cycle_detector.visit(
                            absolute_path
                                .to_str()
                                .ok_or("Failed to convert path to string")?
                                .to_string(),
                        ) {
                            Ok(mut guard) => {
                                let read_file = std::fs::read_to_string(absolute_path);
                                if let Err(e) = read_file {
                                    return Err(format!(
                                        "Failed to read import file '{}': {}",
                                        path, e
                                    ));
                                }
                                let code_str = read_file.unwrap();

                                dir_stack.push_file(&Path::new(path)).map_err(|err| {
                                    format!("Failed to push import path '{}': {}", path, err)
                                })?;
                                let node = match import_ast_node(
                                    &code_str,
                                    guard.get_detector_mut(),
                                    dir_stack,
                                ) {
                                    Ok(imported_node) => {
                                        // 将导入的 ASTNode 添加到当前节点的子节点中
                                        let mut new_node: ASTNode = imported_node.into();
                                        new_node.start_token = node.start_token.clone();
                                        new_node.end_token = node.end_token.clone();
                                        new_node
                                    }
                                    Err(err) => {
                                        return Err(format!(
                                            "Failed to import '{}': {}",
                                            path, err
                                        ));
                                    }
                                };
                                dir_stack.pop().ok_or_else(|| {
                                    format!("Failed to pop import path '{}'", path)
                                })?;
                                return Ok(node);
                            }
                            Err(cycle_node) => {
                                return Err(format!(
                                    "Cycle detected while importing '{}': {}",
                                    path, cycle_node
                                ));
                            }
                        }
                    } else {
                        return Err(format!(
                            "Import path must be a string, found: {:?}",
                            import_path.node_type
                        ));
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }
    let mut new_children = Vec::new();
    for child in &node.children {
        let applied_child = expand_import_to_node(child, cycle_detector, dir_stack)?;
        new_children.push(applied_child);
    }
    return Ok(ASTNode {
        node_type: node.node_type.clone(),
        start_token: node.start_token.clone(),
        end_token: node.end_token.clone(),
        children: new_children,
    });
}
