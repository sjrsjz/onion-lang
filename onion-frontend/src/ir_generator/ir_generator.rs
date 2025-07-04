use crate::parser::ast::{ASTNode, ASTNodeModifier, ASTNodeOperation, ASTNodeType};
use base64::{self, Engine};
use onion_vm::types::lambda::vm_instructions::ir::{DebugInfo, Functions, IROperation, IR};

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

#[derive(Debug)]
enum Scope {
    Frame,
    Loop(String, String), // loop head label, loop end label
}

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

#[derive(Debug)]
pub struct IRGenerator<'t> {
    namespace: Rc<NameSpace>,
    functions: &'t mut Functions,
    scope_stack: Vec<Scope>,
    label_generator: LabelGenerator,
    function_signature_generator: LabelGenerator,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum IRGeneratorError {
    InvalidASTNodeType(ASTNodeType),
    InvalidScope,
    InvalidLabel,
}

use std::rc::Rc;

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
        DebugInfo {
            code_position: match ast_node.start_token {
                Some(token) => token.position,
                None => 0,
            },
        }
    }

    pub fn generate_without_redirect(
        &mut self,
        ast_node: &ASTNode,
    ) -> Result<Vec<(DebugInfo, IR)>, IRGeneratorError> {
        match &ast_node.node_type {
            ASTNodeType::Body => {
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::NewFrame));
                self.scope_stack.push(Scope::Frame);
                let children_len = ast_node.children.len();
                for i in 0..children_len {
                    let child = &ast_node.children[i];
                    let child_instructions = self.generate_without_redirect(child)?;
                    instructions.extend(child_instructions);
                }
                self.scope_stack.pop();
                instructions.push((self.generate_debug_info(ast_node), IR::PopFrame));
                Ok(instructions)
            }
            ASTNodeType::Annotation(annotation) => {
                if !vec!["static", "dynamic", "compile", "required"].contains(&annotation.as_str())
                {
                    return Ok(vec![]);
                }
                let mut instructions = Vec::new();
                for child in &ast_node.children {
                    instructions.extend(self.generate_without_redirect(child)?);
                }
                Ok(instructions)
            }
            ASTNodeType::LambdaDef(is_dyn, is_capture, is_dynamic_params) => {
                let mut instructions = Vec::new();
                let args = &ast_node.children[0];
                let args_instructions = self.generate_without_redirect(args)?;

                if *is_dyn {
                    let expr_instructions =
                        self.generate_without_redirect(&ast_node.children.last().unwrap())?;
                    instructions.extend(args_instructions);
                    if *is_capture {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                    }
                    instructions.extend(expr_instructions);

                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::LoadLambda(
                            "__main__".to_string(),
                            match ast_node.start_token {
                                Some(token) => token.position,
                                None => 0,
                            },
                            *is_capture,
                            *is_dynamic_params,
                        ),
                    ));
                } else {
                    let (full_signature, signature) = self.new_function_signature();

                    let mut generator = IRGenerator::new(
                        self.functions,
                        NameSpace::new(signature.clone(), Some(&self.namespace)),
                    );

                    let mut body_instructions =
                        generator.generate(&ast_node.children.last().unwrap())?; // body, compute redirect jump directly
                    body_instructions.push((self.generate_debug_info(ast_node), IR::Return));

                    self.functions
                        .append(full_signature.clone(), body_instructions);

                    instructions.extend(args_instructions);
                    if *is_capture {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                    }
                    instructions.push((self.generate_debug_info(ast_node), IR::ForkInstruction));
                    instructions.push((
                        self.generate_debug_info(ast_node),
                        IR::LoadLambda(
                            full_signature,
                            match ast_node.start_token {
                                Some(token) => token.position,
                                None => 0,
                            },
                            *is_capture,
                            *is_dynamic_params,
                        ),
                    ));
                }
                Ok(instructions)
            }
            ASTNodeType::Assign => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::Set));
                Ok(instructions)
            }
            ASTNodeType::Variable(var_name) => {
                let mut instructions = Vec::new();
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::Get(var_name.clone()),
                ));
                Ok(instructions)
            }
            ASTNodeType::Let(var_name) => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::Let(var_name.clone()),
                ));
                Ok(instructions)
            }
            ASTNodeType::LambdaCall => {
                let mut instructions = Vec::new();
                for child in &ast_node.children {
                    instructions.extend(self.generate_without_redirect(child)?);
                }
                instructions.push((self.generate_debug_info(ast_node), IR::CallLambda));
                Ok(instructions)
            }
            ASTNodeType::AsyncLambdaCall => {
                let mut instructions = Vec::new();
                for child in &ast_node.children {
                    instructions.extend(self.generate_without_redirect(child)?);
                }
                instructions.push((self.generate_debug_info(ast_node), IR::AsyncCallLambda));
                Ok(instructions)
            }
            ASTNodeType::SyncLambdaCall => {
                let mut instructions = Vec::new();
                for child in &ast_node.children {
                    instructions.extend(self.generate_without_redirect(child)?);
                }
                instructions.push((self.generate_debug_info(ast_node), IR::SyncCallLambda));
                Ok(instructions)
            }
            ASTNodeType::Operation(opeartion) => {
                let mut instructions = Vec::new();
                for child in &ast_node.children {
                    instructions.extend(self.generate_without_redirect(child)?);
                }
                match opeartion {
                    ASTNodeOperation::Add => {
                        if ast_node.children.len() == 1 {
                            instructions.push((
                                self.generate_debug_info(ast_node),
                                IR::UnaryOp(IROperation::Add),
                            ));
                        } else {
                            instructions.push((
                                self.generate_debug_info(ast_node),
                                IR::BinaryOp(IROperation::Add),
                            ));
                        }
                    }
                    ASTNodeOperation::Subtract => {
                        if ast_node.children.len() == 1 {
                            instructions.push((
                                self.generate_debug_info(ast_node),
                                IR::UnaryOp(IROperation::Subtract),
                            ));
                        } else {
                            instructions.push((
                                self.generate_debug_info(ast_node),
                                IR::BinaryOp(IROperation::Subtract),
                            ));
                        }
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
            ASTNodeType::IndexOf => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::IndexOf));
                Ok(instructions)
            }
            ASTNodeType::GetAttr => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::GetAttr));
                Ok(instructions)
            }
            ASTNodeType::Return => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.push((self.generate_debug_info(ast_node), IR::Return));
                Ok(instructions)
            }
            ASTNodeType::Emit => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.push((self.generate_debug_info(ast_node), IR::Emit));
                Ok(instructions)
            }
            ASTNodeType::Raise => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.push((self.generate_debug_info(ast_node), IR::Raise));
                Ok(instructions)
            }
            ASTNodeType::Number(number_str) => {
                // check if number_str is float or int, supporting hex, octal, binary, and decimal
                let mut instructions = Vec::new();

                // First try to parse as various integer formats
                if let Some(integer_value) = parse_integer_literal(number_str) {
                    instructions.push((self.generate_debug_info(ast_node), IR::LoadInt(integer_value)));
                } else if let Ok(number) = number_str.parse::<f64>() {
                    // Fall back to float parsing for decimal floats and scientific notation
                    instructions.push((self.generate_debug_info(ast_node), IR::LoadFloat(number)));
                } else {
                    return Err(IRGeneratorError::InvalidASTNodeType(
                        ast_node.node_type.clone(),
                    ));
                }
                Ok(instructions)
            }
            ASTNodeType::Tuple => {
                let mut instructions = Vec::new();
                let mut tuple_size = 0;
                for child in &ast_node.children {
                    if child.node_type == ASTNodeType::None {
                        continue;
                    }
                    instructions.extend(self.generate_without_redirect(child)?);
                    tuple_size += 1;
                }
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::BuildTuple(tuple_size),
                ));
                Ok(instructions)
            }
            ASTNodeType::KeyValue => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::BuildKeyValue));
                Ok(instructions)
            }
            ASTNodeType::NamedTo => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::BuildNamed));
                Ok(instructions)
            }
            ASTNodeType::String(str) => {
                let mut instructions = Vec::new();
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::LoadString(str.clone()),
                ));
                Ok(instructions)
            }
            ASTNodeType::Expressions => {
                let mut instructions = Vec::new();
                for child in &ast_node.children {
                    instructions.push((self.generate_debug_info(child), IR::ResetStack));
                    instructions.extend(self.generate_without_redirect(child)?);
                }
                Ok(instructions)
            }
            ASTNodeType::Null => {
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::LoadNull));
                Ok(instructions)
            }
            ASTNodeType::None => {
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::LoadUndefined));
                Ok(instructions)
            }
            ASTNodeType::Undefined => {
                let mut instructions = Vec::new();
                instructions.push((self.generate_debug_info(ast_node), IR::LoadUndefined));
                Ok(instructions)
            }
            ASTNodeType::Boolean(bool_str) => {
                let mut instructions = Vec::new();
                if bool_str == "true" {
                    instructions.push((self.generate_debug_info(ast_node), IR::LoadBool(true)));
                } else if bool_str == "false" {
                    instructions.push((self.generate_debug_info(ast_node), IR::LoadBool(false)));
                } else {
                    return Err(IRGeneratorError::InvalidASTNodeType(
                        ast_node.node_type.clone(),
                    ));
                }
                Ok(instructions)
            }
            ASTNodeType::If => match ast_node.children.len() {
                0..=1 => Err(IRGeneratorError::InvalidASTNodeType(
                    ast_node.node_type.clone(),
                )),
                2 => {
                    let mut instructions = Vec::new();
                    instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                    let body_instructions =
                        self.generate_without_redirect(&ast_node.children[1])?;
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
                    instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                    let body_instructions =
                        self.generate_without_redirect(&ast_node.children[1])?;
                    let else_body_instructions =
                        self.generate_without_redirect(&ast_node.children[2])?;
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
                _ => Err(IRGeneratorError::InvalidASTNodeType(
                    ast_node.node_type.clone(),
                )),
            },
            ASTNodeType::Break => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
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
                    return Err(IRGeneratorError::InvalidScope);
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
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
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
                    return Err(IRGeneratorError::InvalidScope);
                }

                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::RedirectJump(loop_label.unwrap()),
                ));
                Ok(instructions)
            }
            ASTNodeType::While => {
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
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::RedirectJumpIfFalse(med_label.clone()),
                ));
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
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
                let mut instructions = Vec::new();
                match modifier {
                    ASTNodeModifier::KeyOf => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::KeyOf));
                    }
                    ASTNodeModifier::ValueOf => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::ValueOf));
                    }
                    ASTNodeModifier::Mut => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::Mut));
                    }
                    ASTNodeModifier::Const => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::Const));
                    }
                    ASTNodeModifier::Assert => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::Assert));
                    }
                    ASTNodeModifier::Copy => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::CopyValue));
                    }
                    ASTNodeModifier::DeepCopy => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::DeepCopyValue));
                    }
                    ASTNodeModifier::Import => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::Import));
                    }
                    ASTNodeModifier::TypeOf => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::TypeOf));
                    }
                    ASTNodeModifier::Await => {
                        // TODO: handle await in the future
                        return Err(IRGeneratorError::InvalidASTNodeType(
                            ast_node.node_type.clone(),
                        ));
                    }
                    ASTNodeModifier::BindSelf => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::BindSelf));
                    }
                    ASTNodeModifier::LengthOf => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::LengthOf));
                    }
                    ASTNodeModifier::Share => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::Share));
                    }
                    ASTNodeModifier::Launch => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::Launch));
                    }
                    ASTNodeModifier::Spawn => {
                        instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                        instructions.push((self.generate_debug_info(ast_node), IR::Spawn));
                    }
                }
                Ok(instructions)
            }
            ASTNodeType::Range => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::BuildRange));
                Ok(instructions)
            }
            ASTNodeType::In => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::In));
                Ok(instructions)
            }
            ASTNodeType::Is => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::IsSameObject));
                Ok(instructions)
            }
            ASTNodeType::Namespace(alias) => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::Namespace(alias.clone()),
                ));
                Ok(instructions)
            }
            ASTNodeType::Base64(base64_str) => {
                let mut instructions = Vec::new();
                // decode base64 string to bytes using the recommended Engine approach
                let decoded_bytes = base64::engine::general_purpose::STANDARD
                    .decode(base64_str)
                    .map_err(|_| {
                        IRGeneratorError::InvalidASTNodeType(ast_node.node_type.clone())
                    })?;
                instructions.push((
                    self.generate_debug_info(ast_node),
                    IR::LoadBytes(decoded_bytes),
                ));
                Ok(instructions)
            }
            ASTNodeType::AssumeTuple => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                Ok(instructions)
            }
            ASTNodeType::Set => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::BuildSet));
                Ok(instructions)
            }
            ASTNodeType::Map => {
                let mut instructions = Vec::new();
                instructions.extend(self.generate_without_redirect(&ast_node.children[0])?);
                instructions.extend(self.generate_without_redirect(&ast_node.children[1])?);
                instructions.push((self.generate_debug_info(ast_node), IR::MapTo));
                Ok(instructions)
            }
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
        irs: Vec<(DebugInfo, IR)>,
    ) -> Result<Vec<(DebugInfo, IR)>, IRGeneratorError> {
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
                        return Err(IRGeneratorError::InvalidLabel);
                    }
                }
                (debug_info, IR::RedirectJumpIfFalse(label)) => {
                    if let Some(&target_pos) = label_map.get(label) {
                        let offset = target_pos as isize - i as isize - 1;
                        reduced_irs[i] = (debug_info.clone(), IR::JumpIfFalseOffset(offset));
                    } else {
                        return Err(IRGeneratorError::InvalidLabel);
                    }
                }
                _ => {}
            }
        }

        Ok(reduced_irs)
    }

    /// 生成并优化IR指令
    ///
    /// # Arguments
    ///
    /// * `ast_node` - AST节点
    ///
    /// # Returns
    ///
    /// 优化后的IR指令列表
    pub fn generate(
        &mut self,
        ast_node: &ASTNode<'t>,
    ) -> Result<Vec<(DebugInfo, IR)>, IRGeneratorError> {
        let irs = self.generate_without_redirect(ast_node)?;
        self.redirect_jump(irs)
    }
}
