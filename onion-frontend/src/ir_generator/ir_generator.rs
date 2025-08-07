//! IR 生成器模块：将 Onion 语言的 AST 转换为虚拟机可执行的中间表示（IR）指令。
//!
//! 本模块负责遍历 AST，生成带有调试信息的 IR 指令序列，并处理作用域、标签、跳转等控制流结构。
//! 主要类型包括 `IRGenerator`（IR 生成主结构）、`NameSpace`（命名空间管理）、`LabelGenerator`（标签生成器）等。
//! 支持多种 AST 节点类型、表达式、控制流、作用域、闭包、字面量等。
//!
//! # 主要结构
//! - `IRGenerator`：IR 生成主结构，负责递归遍历 AST 并生成 IR 指令。
//! - `NameSpace`：命名空间路径管理，便于唯一标识函数、标签等。
//! - `LabelGenerator`：标签生成器，确保跳转标签唯一。
//! - `IRGeneratorError`：IR 生成过程中的错误类型。
//!
//! # 主要方法
//! - `generate_without_redirect`：递归生成带有逻辑跳转标签的 IR 指令。
//! - `redirect_jump`：将逻辑跳转标签重定向为实际的偏移量跳转。
//! - `generate`：完整生成 IR 指令（含跳转重定向）。
//!
//! # 用法示例
//! ```ignore
//! let mut functions = Functions::new();
//! let mut generator = IRGenerator::new(&mut functions, NameSpace::new("main".to_string(), None));
//! let irs = generator.generate(&mut collector, &ast_node)?;
//! ```
use crate::{
    diagnostics::{Diagnostic, collector::DiagnosticCollector},
    parser::ast::{ASTNode, ASTNodeModifier, ASTNodeOperation, ASTNodeType},
};
use base64::{self, Engine};
use onion_vm::types::lambda::vm_instructions::ir::{DebugInfo, Functions, IR, IROperation};
use std::rc::Rc;

/// Helper macro to check for an exact number of children.
/// If the check fails, it returns an `InvalidASTNodeType` error.
macro_rules! check_children_exact {
    ($collector:expr, $node:expr, $expected:expr) => {
        if $node.children.len() != $expected {
            return $collector.fatal(IRGeneratorError::InvalidASTNode($node.clone()));
        }
    };
}

/// Parse integer literals in various formats: decimal, hex (0x), octal (0o), binary (0b)
fn parse_integer_literal(number_str: &str) -> Option<i64> {
    let trimmed = number_str.trim();

    if trimmed.len() < 2 {
        // Too short for any prefix, try as decimal
        return trimmed.parse::<i64>().ok();
    }

    // Check for prefixes using character iteration for UTF-8 safety
    let chars: Vec<char> = trimmed.chars().collect();
    if chars.len() >= 2 && chars[0] == '0' {
        match chars[1] {
            'x' | 'X' => {
                // Hexadecimal
                let hex_part = &trimmed[2..];
                return i64::from_str_radix(hex_part, 16).ok();
            }
            'o' | 'O' => {
                // Octal
                let octal_part = &trimmed[2..];
                return i64::from_str_radix(octal_part, 8).ok();
            }
            'b' | 'B' => {
                // Binary
                let binary_part = &trimmed[2..];
                return i64::from_str_radix(binary_part, 2).ok();
            }
            _ => {
                // No recognized prefix, fall through to decimal
            }
        }
    }

    // Default to decimal parsing
    trimmed.parse::<i64>().ok()
}

/// 作用域类型。
///
/// - `Frame`：普通作用域帧。
/// - `Loop`：循环作用域，包含循环头和结束标签。
#[derive(Debug)]
enum Scope {
    Frame,
    Loop(String, String), // loop head label, loop end label
}

/// 命名空间路径管理。
///
/// 用于唯一标识函数、标签等，支持多级嵌套。
#[derive(Debug, Clone)]
pub struct NameSpace {
    path: Vec<String>,
}

impl NameSpace {
    pub fn new(name: String, parent: Option<&NameSpace>) -> Self {
        let mut path = Vec::new();
        if let Some(parent) = parent {
            path.extend(parent.path.clone());
        }
        path.push(name);
        NameSpace { path }
    }

    pub fn get_full_name(&self) -> String {
        self.path.join("::")
    }
}

/// 标签生成器。
///
/// 用于生成唯一的跳转标签，结合命名空间和类型名。
#[derive(Debug)]
pub struct LabelGenerator {
    label_counter: usize,
    namespace: Rc<NameSpace>,
    type_name: String,
}

impl LabelGenerator {
    pub fn new(namespace: Rc<NameSpace>, type_name: String) -> Self {
        LabelGenerator {
            label_counter: 0,
            namespace,
            type_name,
        }
    }

    pub fn new_label(&mut self) -> (String, String) {
        let full_label = format!(
            "{}::{}_{}",
            self.namespace.get_full_name(),
            self.type_name,
            self.label_counter
        );
        let label = format!("{}_{}", self.type_name, self.label_counter);
        self.label_counter += 1;
        (full_label, label)
    }
}

/// IR 生成器主结构。
///
/// 负责递归遍历 AST，生成带有调试信息的 IR 指令序列，并处理作用域、标签、跳转等。
#[derive(Debug)]
pub struct IRGenerator<'t> {
    namespace: Rc<NameSpace>,
    functions: &'t mut Functions,
    scope_stack: Vec<Scope>,
    label_generator: LabelGenerator,
    function_signature_generator: LabelGenerator,
}

/// IR 生成过程中的错误类型。
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum IRGeneratorError {
    /// AST 节点类型无效。
    InvalidASTNode(ASTNode),
    /// 作用域操作无效。
    InvalidScope,
    /// 跳转标签无效。
    InvalidLabel(String),
    /// Base64 数据无效。
    InvalidBase64(String),
}

impl Diagnostic for IRGeneratorError {
    fn severity(&self) -> crate::diagnostics::ReportSeverity {
        match self {
            IRGeneratorError::InvalidASTNode(_) => crate::diagnostics::ReportSeverity::Error,
            IRGeneratorError::InvalidScope => crate::diagnostics::ReportSeverity::Error,
            IRGeneratorError::InvalidLabel(_) => crate::diagnostics::ReportSeverity::Error,
            IRGeneratorError::InvalidBase64(_) => crate::diagnostics::ReportSeverity::Error,
        }
    }

    fn title(&self) -> String {
        "IR Generation Error".to_string()
    }

    fn message(&self) -> String {
        match self {
            IRGeneratorError::InvalidASTNode(node_type) => {
                format!("Invalid AST node type encountered: {:?}", node_type)
            }
            IRGeneratorError::InvalidScope => "Invalid scope operation encountered".to_string(),
            IRGeneratorError::InvalidLabel(label) => {
                format!("Invalid label operation encountered: {}", label)
            }
            IRGeneratorError::InvalidBase64(data) => {
                format!("Invalid Base64 data encountered: {}", data)
            }
        }
    }

    fn location(&self) -> Option<crate::diagnostics::SourceLocation> {
        None
    }

    fn help(&self) -> Option<String> {
        None
    }

    fn copy(&self) -> Box<dyn Diagnostic> {
        Box::new(self.clone())
    }
}

impl<'t> IRGenerator<'t> {
    pub fn new(functions: &'t mut Functions, namespace: NameSpace) -> Self {
        let namespace_rc = Rc::new(namespace);
        let label_generator = LabelGenerator::new(Rc::clone(&namespace_rc), "label".to_string());
        let function_signature_generator =
            LabelGenerator::new(Rc::clone(&namespace_rc), "function".to_string());
        IRGenerator {
            namespace: namespace_rc,
            functions,
            scope_stack: Vec::new(),
            label_generator,
            function_signature_generator,
        }
    }

    fn new_label(&mut self) -> (String, String) {
        self.label_generator.new_label()
    }

    fn new_function_signature(&mut self) -> (String, String) {
        self.function_signature_generator.new_label()
    }

    fn generate_debug_info(&mut self, ast_node: &ASTNode) -> DebugInfo {
        DebugInfo::new(match ast_node.source_location {
            Some(ref token) => token.span,
            None => (0, 0),
        })
    }

    #[stacksafe::stacksafe]
    pub fn generate_without_redirect(
        &mut self,
        collector: &mut DiagnosticCollector,
        ast_node: &ASTNode,
    ) -> Result<Vec<(DebugInfo, IR)>, ()> {
        match &ast_node.node_type {
            ASTNodeType::Frame => {
                // Expects any number of children
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::NewFrame));
                self.scope_stack.push(Scope::Frame);
                let children_len = ast_node.children.len();
                for i in 0..children_len {
                    let child = &ast_node.children[i];
                    let child_instructions = self.generate_without_redirect(collector, child)?;
                    instructions.extend(child_instructions);
                }
                self.scope_stack.pop();
                instructions.push((self.generate_debug_info(ast_node), IR::PopFrame));
                Ok(instructions)
            }
            ASTNodeType::LambdaDef(is_dyn, captured_vars) => {
                // Expects 2 children: args and body
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                let args = &ast_node.children[0];
                let args_instructions = self.generate_without_redirect(collector, args)?;

                if *is_dyn {
                    let expr_instructions = self
                        .generate_without_redirect(collector, &ast_node.children.last().unwrap())?;
                    instructions.extend(args_instructions);
                    instructions.extend(expr_instructions);

                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::LoadLambda(
                            "__main__".to_string(),
                            captured_vars.iter().cloned().collect(),
                        ),
                    ));
                } else {
                    let (full_signature, signature) = self.new_function_signature();

                    let mut generator = IRGenerator::new(
                        self.functions,
                        NameSpace::new(signature.clone(), Some(&self.namespace)),
                    );

                    let mut body_instructions =
                        generator.generate(collector, &ast_node.children.last().unwrap())?; // body, compute redirect jump directly
                    body_instructions.push((self.generate_debug_info(ast_node), IR::Return));

                    self.functions
                        .append(full_signature.clone(), body_instructions);

                    instructions.extend(args_instructions);
                    instructions.push((self.generate_debug_info(ast_node), IR::ForkInstruction));
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::LoadLambda(full_signature, captured_vars.iter().cloned().collect()),
                    ));
                }
                Ok(instructions)
            }
            ASTNodeType::Assign => {
                // Expects 2 children: target and value
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::Set));
                Ok(instructions)
            }
            ASTNodeType::Variable(name) => {
                // Expects 0 children
                check_children_exact!(collector, ast_node, 0);
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::Get(name.clone())));
                Ok(instructions)
            }
            ASTNodeType::Let(name) => {
                // Expects 1 child: value
                check_children_exact!(collector, ast_node, 1);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions.push((self.generate_debug_info(ast_node), IR::Let(name.clone())));
                Ok(instructions)
            }
            ASTNodeType::Apply => {
                // Expects any number of children
                let mut instructions = Vec::new();
                for child in &ast_node.children {
                    instructions.extend(self.generate_without_redirect(collector, child)?);
                }
                instructions.push((self.generate_debug_info(ast_node), IR::Apply));
                Ok(instructions)
            }
            ASTNodeType::Operation(operation) => {
                match operation {
                    ASTNodeOperation::Add
                    | ASTNodeOperation::Subtract
                    | ASTNodeOperation::Multiply
                    | ASTNodeOperation::Divide
                    | ASTNodeOperation::Modulus
                    | ASTNodeOperation::And
                    | ASTNodeOperation::Or
                    | ASTNodeOperation::Xor
                    | ASTNodeOperation::Equal
                    | ASTNodeOperation::NotEqual
                    | ASTNodeOperation::Greater
                    | ASTNodeOperation::Less
                    | ASTNodeOperation::GreaterEqual
                    | ASTNodeOperation::LessEqual
                    | ASTNodeOperation::Power
                    | ASTNodeOperation::LeftShift
                    | ASTNodeOperation::RightShift => {
                        check_children_exact!(collector, ast_node, 2);
                    }
                    ASTNodeOperation::Abs | ASTNodeOperation::Minus | ASTNodeOperation::Not => {
                        check_children_exact!(collector, ast_node, 1);
                    }
                }

                let mut instructions = Vec::new();
                for child in &ast_node.children {
                    instructions.extend(self.generate_without_redirect(collector, child)?);
                }
                match operation {
                    ASTNodeOperation::Add => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Add),
                        ));
                    }
                    ASTNodeOperation::Abs => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::UnaryOp(IROperation::Abs),
                        ));
                    }
                    ASTNodeOperation::Subtract => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Subtract),
                        ));
                    }
                    ASTNodeOperation::Minus => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::UnaryOp(IROperation::Minus),
                        ));
                    }
                    ASTNodeOperation::Multiply => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Multiply),
                        ));
                    }
                    ASTNodeOperation::Divide => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Divide),
                        ));
                    }
                    ASTNodeOperation::Modulus => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Modulus),
                        ));
                    }
                    ASTNodeOperation::And => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::And),
                        ));
                    }
                    ASTNodeOperation::Or => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Or),
                        ));
                    }
                    ASTNodeOperation::Xor => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Xor),
                        ));
                    }
                    ASTNodeOperation::Not => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::UnaryOp(IROperation::Not),
                        ));
                    }
                    ASTNodeOperation::Equal => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Equal),
                        ));
                    }
                    ASTNodeOperation::NotEqual => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::NotEqual),
                        ));
                    }
                    ASTNodeOperation::Greater => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Greater),
                        ));
                    }
                    ASTNodeOperation::Less => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Less),
                        ));
                    }
                    ASTNodeOperation::GreaterEqual => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::GreaterEqual),
                        ));
                    }
                    ASTNodeOperation::LessEqual => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::LessEqual),
                        ));
                    }
                    ASTNodeOperation::Power => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::Power),
                        ));
                    }
                    ASTNodeOperation::LeftShift => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::ShiftLeft),
                        ));
                    }
                    ASTNodeOperation::RightShift => {
                        instructions.push((
                            self.generate_debug_info(ast_node),
                            IR::BinaryOp(IROperation::ShiftRight),
                        ));
                    }
                }
                Ok(instructions)
            }
            ASTNodeType::GetAttr => {
                // Expects 2 children: object and attribute
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::GetAttr));
                Ok(instructions)
            }
            ASTNodeType::Return => {
                // Expects 1 child: return value
                check_children_exact!(collector, ast_node, 1);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions.push((self.generate_debug_info(ast_node), IR::Return));
                Ok(instructions)
            }
            ASTNodeType::Raise => {
                // Expects 1 child: error value
                check_children_exact!(collector, ast_node, 1);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions.push((self.generate_debug_info(ast_node), IR::Raise));
                Ok(instructions)
            }
            ASTNodeType::Number(number_str) => {
                // Expects 0 children
                check_children_exact!(collector, ast_node, 0);
                let mut instructions = Vec::new();

                if let Some(integer_value) = parse_integer_literal(number_str) {
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::LoadInt(integer_value),
                    ));
                } else if let Ok(number) = number_str.parse::<f64>() {
                    instructions.push((self.generate_debug_info(ast_node), IR::LoadFloat(number)));
                } else {
                    return collector.fatal(IRGeneratorError::InvalidASTNode(ast_node.clone()));
                }
                Ok(instructions)
            }
            ASTNodeType::Tuple => {
                // Expects any number of children
                let mut instructions = Vec::new();
                let mut tuple_size = 0;
                for child in &ast_node.children {
                    instructions.extend(self.generate_without_redirect(collector, child)?);
                    tuple_size += 1;
                }
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::BuildTuple(tuple_size),
                ));
                Ok(instructions)
            }
            ASTNodeType::Pair => {
                // Expects 2 children: key and value
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::BuildKeyValue));
                Ok(instructions)
            }
            ASTNodeType::String(str) => {
                // Expects 0 children
                check_children_exact!(collector, ast_node, 0);
                let mut instructions = Vec::new();
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::LoadString(str.clone()),
                ));
                Ok(instructions)
            }
            ASTNodeType::Expressions => {
                // Expects any number of children
                let mut instructions = Vec::new();
                for (i,child) in ast_node.children.iter().enumerate() {
                    // instructions.push((self.generate_debug_info(child), IR::ResetStack)); // 这个是严重错误的，会导致(;)把栈清空
                    instructions.extend(self.generate_without_redirect(collector, child)?);
                    if i < ast_node.children.len() - 1 {
                        instructions.push((self.generate_debug_info(child), IR::Pop));
                    }
                }
                Ok(instructions)
            }
            ASTNodeType::Null => {
                // Expects 0 children
                check_children_exact!(collector, ast_node, 0);
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::LoadNull));
                Ok(instructions)
            }
            ASTNodeType::Undefined => {
                // Expects 0 children
                check_children_exact!(collector, ast_node, 0);
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::LoadUndefined));
                Ok(instructions)
            }
            ASTNodeType::Boolean(value) => {
                // Expects 0 children
                check_children_exact!(collector, ast_node, 0);
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::LoadBool(*value)));
                Ok(instructions)
            }
            ASTNodeType::If => match ast_node.children.len() {
                2 => {
                    let mut instructions = Vec::new();
                    instructions
                        .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                    let body_instructions =
                        self.generate_without_redirect(collector, &ast_node.children[1])?;
                    let (if_label, _) = self.new_label();
                    let (else_label, _) = self.new_label();
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::RedirectJumpIfFalse(if_label.clone()),
                    ));
                    instructions.extend(body_instructions);
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::RedirectJump(else_label.clone()),
                    ));
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::RedirectLabel(if_label.clone()),
                    ));
                    instructions.push((self.generate_debug_info(ast_node), IR::LoadUndefined));
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::RedirectLabel(else_label.clone()),
                    ));
                    Ok(instructions)
                }
                3 => {
                    let mut instructions = Vec::new();
                    instructions
                        .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                    let body_instructions =
                        self.generate_without_redirect(collector, &ast_node.children[1])?;
                    let else_body_instructions =
                        self.generate_without_redirect(collector, &ast_node.children[2])?;
                    let (if_label, _) = self.new_label();
                    let (else_label, _) = self.new_label();
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::RedirectJumpIfFalse(if_label.clone()),
                    ));
                    instructions.extend(body_instructions);
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::RedirectJump(else_label.clone()),
                    ));
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::RedirectLabel(if_label.clone()),
                    ));
                    instructions.extend(else_body_instructions);
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::RedirectLabel(else_label.clone()),
                    ));
                    Ok(instructions)
                }
                _ => collector.fatal(IRGeneratorError::InvalidASTNode(ast_node.clone())),
            },
            ASTNodeType::Break => {
                // Expects 1 child: break value
                check_children_exact!(collector, ast_node, 1);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                let mut frames_to_pop = 0;
                let mut found_loop = false;
                let mut loop_label = None;
                for scope in self.scope_stack.iter().rev() {
                    match scope {
                        Scope::Frame => {
                            frames_to_pop += 1;
                        }
                        Scope::Loop(_, end_label) => {
                            found_loop = true;
                            loop_label = Some(end_label.clone());
                            break;
                        }
                    }
                }
                if !found_loop {
                    return collector.fatal(IRGeneratorError::InvalidScope);
                }

                for _ in 0..frames_to_pop {
                    instructions.push((self.generate_debug_info(ast_node), IR::PopFrame));
                }
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::RedirectJump(loop_label.unwrap()),
                ));

                Ok(instructions)
            }
            ASTNodeType::Continue => {
                // Expects 1 child: continue value (often undefined)
                check_children_exact!(collector, ast_node, 1);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                let mut found_loop = false;
                let mut loop_label = None;
                for scope in self.scope_stack.iter().rev() {
                    match scope {
                        Scope::Frame => {}
                        Scope::Loop(head_label, _) => {
                            found_loop = true;
                            loop_label = Some(head_label.clone());
                            break;
                        }
                    }
                }
                if !found_loop {
                    return collector.fatal(IRGeneratorError::InvalidScope);
                }

                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::RedirectJump(loop_label.unwrap()),
                ));
                Ok(instructions)
            }
            ASTNodeType::While => {
                // Expects 2 children: condition and body
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                let (head_label, _) = self.new_label();
                let (med_label, _) = self.new_label();
                let (end_label, _) = self.new_label();
                self.scope_stack
                    .push(Scope::Loop(head_label.clone(), end_label.clone()));
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::RedirectLabel(head_label.clone()),
                ));
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::RedirectJumpIfFalse(med_label.clone()),
                ));
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::Pop));
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::RedirectJump(head_label.clone()),
                ));
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::RedirectLabel(med_label.clone()),
                ));
                instructions.push((self.generate_debug_info(ast_node), IR::LoadUndefined));
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::RedirectLabel(end_label.clone()),
                ));
                self.scope_stack.pop();
                Ok(instructions)
            }
            ASTNodeType::Modifier(modifier) => {
                // All modifiers expect exactly 1 child
                check_children_exact!(collector, ast_node, 1);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);

                match modifier {
                    ASTNodeModifier::KeyOf => {
                        instructions.push((self.generate_debug_info(ast_node), IR::KeyOf));
                    }
                    ASTNodeModifier::ValueOf => {
                        instructions.push((self.generate_debug_info(ast_node), IR::ValueOf));
                    }
                    ASTNodeModifier::Mut => {
                        instructions.push((self.generate_debug_info(ast_node), IR::Mut));
                    }
                    ASTNodeModifier::Const => {
                        instructions.push((self.generate_debug_info(ast_node), IR::Const));
                    }
                    ASTNodeModifier::Assert => {
                        instructions.push((self.generate_debug_info(ast_node), IR::Assert));
                    }
                    ASTNodeModifier::Import => {
                        instructions.push((self.generate_debug_info(ast_node), IR::Import));
                    }
                    ASTNodeModifier::TypeOf => {
                        instructions.push((self.generate_debug_info(ast_node), IR::TypeOf));
                    }
                    ASTNodeModifier::LengthOf => {
                        instructions.push((self.generate_debug_info(ast_node), IR::LengthOf));
                    }
                    ASTNodeModifier::Launch => {
                        instructions.push((self.generate_debug_info(ast_node), IR::Launch));
                    }
                    ASTNodeModifier::Spawn => {
                        instructions.push((self.generate_debug_info(ast_node), IR::Spawn));
                    }
                    ASTNodeModifier::Async => {
                        instructions.push((self.generate_debug_info(ast_node), IR::MakeAsync));
                    }
                    ASTNodeModifier::Sync => {
                        instructions.push((self.generate_debug_info(ast_node), IR::MakeSync));
                    }
                    ASTNodeModifier::Atomic => {
                        instructions.push((self.generate_debug_info(ast_node), IR::MakeAtomic));
                    }
                }
                Ok(instructions)
            }
            ASTNodeType::Range => {
                // Expects 2 children: start and end
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::BuildRange));
                Ok(instructions)
            }
            ASTNodeType::In => {
                // Expects 2 children: element and container
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::In));
                Ok(instructions)
            }
            ASTNodeType::Is => {
                // Expects 2 children: value1 and value2
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::IsSameObject));
                Ok(instructions)
            }
            ASTNodeType::Namespace(name) => {
                // Expects 1 child: expression
                check_children_exact!(collector, ast_node, 1);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::Namespace(name.clone()),
                ));
                Ok(instructions)
            }
            ASTNodeType::Base64(base64_str) => {
                // Expects 0 children
                check_children_exact!(collector, ast_node, 0);
                let mut instructions = Vec::new();
                let decoded_bytes =
                    match base64::engine::general_purpose::STANDARD.decode(base64_str) {
                        Ok(bytes) => bytes,
                        Err(_) => {
                            return collector
                                .fatal(IRGeneratorError::InvalidBase64(base64_str.clone()));
                        }
                    };
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::LoadBytes(decoded_bytes),
                ));
                Ok(instructions)
            }
            ASTNodeType::AssumeTuple => {
                // Expects 1 child
                check_children_exact!(collector, ast_node, 1);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                Ok(instructions)
            }
            ASTNodeType::LazySet => {
                // Expects 2 children
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::BuildLazySet));
                Ok(instructions)
            }
            ASTNodeType::Map => {
                // Expects 2 children
                check_children_exact!(collector, ast_node, 2);
                let mut instructions = Vec::new();
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[0])?);
                instructions
                    .extend(self.generate_without_redirect(collector, &ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::MapTo));
                Ok(instructions)
            }
            ASTNodeType::Required(name) => {
                // Expects 0 children
                check_children_exact!(collector, ast_node, 0);
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::Get(name.clone())));
                Ok(instructions)
            }
            _ => collector.fatal(IRGeneratorError::InvalidASTNode(ast_node.clone())),
        }
    }
    /// 重定向所有跳转指令，将RedirectJump和RedirectJumpIfFalse转换为JumpOffset和JumpIfFalse
    ///
    /// # Arguments
    ///
    /// * `irs` - IR指令列表
    ///
    /// # Returns
    ///
    /// 处理后的IR指令列表
    fn redirect_jump(
        &self,
        collector: &mut DiagnosticCollector,
        irs: Vec<(DebugInfo, IR)>,
    ) -> Result<Vec<(DebugInfo, IR)>, ()> {
        let mut reduced_irs = Vec::new();
        let mut label_map = std::collections::HashMap::new();

        // 首先收集所有标签的位置
        for ir in irs.iter() {
            if let (_, IR::RedirectLabel(label)) = ir {
                label_map.insert(label.clone(), reduced_irs.len());
            } else {
                reduced_irs.push(ir.clone());
            }
        }

        // 转换所有跳转指令
        for i in 0..reduced_irs.len() {
            match &reduced_irs[i] {
                (debug_info, IR::RedirectJump(label)) => {
                    if let Some(&target_pos) = label_map.get(label) {
                        let offset = target_pos as isize - i as isize - 1;
                        reduced_irs[i] = (debug_info.clone(), IR::JumpOffset(offset));
                    } else {
                        collector.report(IRGeneratorError::InvalidLabel(label.clone()));
                    }
                }
                (debug_info, IR::RedirectJumpIfFalse(label)) => {
                    if let Some(&target_pos) = label_map.get(label) {
                        let offset = target_pos as isize - i as isize - 1;
                        reduced_irs[i] = (debug_info.clone(), IR::JumpIfFalseOffset(offset));
                    } else {
                        collector.report(IRGeneratorError::InvalidLabel(label.clone()));
                    }
                }
                _ => {}
            }
        }

        Ok(reduced_irs)
    }

    /// 生成IR指令
    #[stacksafe::stacksafe]
    pub fn generate(
        &mut self,
        collector: &mut DiagnosticCollector,
        ast_node: &ASTNode,
    ) -> Result<Vec<(DebugInfo, IR)>, ()> {
        let irs = self.generate_without_redirect(collector, ast_node)?;
        self.redirect_jump(collector, irs)
    }
}
