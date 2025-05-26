use std::collections::HashSet;

use crate::{
    compile::{build_code, compile_to_bytecode},
    dir_stack::DirStack,
};

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
    Range,
    Tuple,
    Set,
    KeyVal,
    NamedArgument,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Variable {
    pub name: String,
    pub assumed_type: AssumedType,
}

#[derive(Debug, Clone)] // Added Clone
pub struct VariableFrame {
    pub variables: Vec<Variable>,
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
pub enum AnalyzeError<'t> {
    UndefinedVariable(&'t ASTNode<'t>),
}

impl AnalyzeError<'_> {
    pub fn format(&self, source_code: String) -> String {
        use super::ast::ASTNodeType::Variable;
        use colored::*;
        use unicode_segmentation::UnicodeSegmentation;

        // 分割源代码为行
        let lines: Vec<&str> = source_code.lines().collect();

        // Helper function to find line and column from position
        let find_position = |byte_pos: usize| -> (usize, usize) {
            let mut current_byte = 0;
            for (line_num, line) in lines.iter().enumerate() {
                // 计算行长度（包括换行符）
                // Windows通常使用CRLF (\r\n)，而Unix使用LF (\n)
                // 我们需要检测使用的是哪种换行符
                let eol_len = if source_code.contains("\r\n") { 2 } else { 1 };
                let line_bytes = line.len() + eol_len; // 加上实际的换行符长度

                if current_byte + line_bytes > byte_pos {
                    // 计算行内的字节偏移
                    let line_offset = byte_pos - current_byte;

                    // 边界检查
                    if line_offset > line.len() {
                        return (line_num, line.graphemes(true).count()); // 位置在行尾
                    }

                    // 找到有效的字符边界
                    let valid_offset = line
                        .char_indices()
                        .map(|(i, _)| i)
                        .take_while(|&i| i <= line_offset)
                        .last()
                        .unwrap_or(0);

                    // 使用有效的字节偏移获取文本
                    let column_text = &line[..valid_offset];
                    let column = column_text.graphemes(true).count();
                    return (line_num, column);
                }
                current_byte += line_bytes;
            }
            (lines.len().saturating_sub(1), 0) // Default to last line
        };

        match self {
            AnalyzeError::UndefinedVariable(node) => {
                let (line_num, col) = find_position(match node.start_token {
                    Some(node) => node.position,
                    None => 0,
                });
                let line = if line_num < lines.len() {
                    lines[line_num]
                } else {
                    ""
                };

                let var_name = if let Variable(name) = &node.node_type {
                    name
                } else {
                    "unknown"
                };

                let mut warning_msg = format!(
                    "{}: {}\n\n",
                    "Analysis Error".bright_red().bold(),
                    format!("Undefined variable '{}'", var_name).red()
                );
                warning_msg.push_str(&format!(
                    "{} {}:{}\n",
                    "Position".bright_blue(),
                    (line_num + 1).to_string().bright_cyan(),
                    (col + 1).to_string().bright_cyan()
                ));
                warning_msg.push_str(&format!("{}\n", line.white()));

                // 计算节点在源代码中的长度
                let node_length = var_name.len();
                warning_msg.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(col),
                    "^".repeat(node_length).bright_red().bold()
                ));

                // 添加建议提示
                warning_msg.push_str(&format!(
                    "\n{} {}\n",
                    "Hint:".bright_green().bold(),
                    format!("Variable '{}' is used but not defined in the current scope, if the variable is dynamic, use `dynamic` annotation.", var_name).bright_white()
                        .italic()
                ));

                warning_msg
            }
        }
    }
}

#[derive(Debug)]
pub enum AnalyzeWarn<'t> {
    CompileError(&'t ASTNode<'t>, String),
}

impl AnalyzeWarn<'_> {
    pub fn format(&self, source_code: String) -> String {
        use colored::*;
        use unicode_segmentation::UnicodeSegmentation;

        // 分割源代码为行
        let lines: Vec<&str> = source_code.lines().collect();

        // Helper function to find line and column from position (same as in AnalyzeError)
        let find_position = |byte_pos: usize| -> (usize, usize) {
            let mut current_byte = 0;
            for (line_num, line) in lines.iter().enumerate() {
                let eol_len = if source_code.contains("\r\n") { 2 } else { 1 };
                let line_bytes = line.len() + eol_len;

                if current_byte + line_bytes > byte_pos {
                    let line_offset = byte_pos - current_byte;
                    if line_offset > line.len() {
                        return (line_num, line.graphemes(true).count());
                    }
                    let valid_offset = line
                        .char_indices()
                        .map(|(i, _)| i)
                        .take_while(|&i| i <= line_offset)
                        .last()
                        .unwrap_or(0);
                    let column_text = &line[..valid_offset];
                    let column = column_text.graphemes(true).count();
                    return (line_num, column);
                }
                current_byte += line_bytes;
            }
            (lines.len().saturating_sub(1), 0) // Default to last line
        };

        match self {
            AnalyzeWarn::CompileError(node, message) => {
                let (line_num, col) = find_position(match node.start_token {
                    Some(token) => token.position,
                    None => 0,
                });
                let line = if line_num < lines.len() {
                    lines[line_num]
                } else {
                    ""
                };

                // 尝试获取节点文本，如果节点是字符串字面量，则使用其内容
                let node_text = match node.start_token {
                    Some(token) => token.origin_token.clone(),
                    None => "".to_string(),
                };

                let mut warning_msg = format!(
                    "{}: {}\n\n",
                    "Analysis Warning".bright_yellow().bold(),
                    message.yellow() // 使用传入的 message
                );
                warning_msg.push_str(&format!(
                    "{} {}:{}\n",
                    "Position".bright_blue(),
                    (line_num + 1).to_string().bright_cyan(),
                    (col + 1).to_string().bright_cyan()
                ));
                warning_msg.push_str(&format!("{}\n", line.white()));

                // 计算节点在源代码中的长度
                let node_length = node_text.graphemes(true).count(); // 使用 graphemes 确保正确处理多字节字符
                warning_msg.push_str(&format!(
                    "{}{}\n",
                    " ".repeat(col),
                    "^".repeat(node_length.max(1)).bright_yellow().bold() // 至少一个 ^
                ));

                // 可以根据需要添加特定的提示
                // warning_msg.push_str(&format!(
                //     "\n{} {}\n",
                //     "Hint:".bright_green().bold(),
                //     format!("Check the path or content for '@compile' annotation.").bright_white().italic()
                // ));

                warning_msg
            }
        }
    }
}

// New struct to hold analysis results including context at break point
#[derive(Debug)]
pub struct AnalysisOutput<'t> {
    pub errors: Vec<AnalyzeError<'t>>,
    pub warnings: Vec<AnalyzeWarn<'t>>,
    pub context_at_break: Option<VariableContext>,
}

impl VariableContext {
    pub fn new() -> Self {
        VariableContext {
            // 初始化时包含一个全局上下文，该上下文包含一个初始帧
            contexts: vec![vec![VariableFrame {
                variables: Vec::new(),
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
            if let Some(existing_var) = frame.variables.iter_mut().find(|v| v.name == var.name) {
                existing_var.assumed_type = var.assumed_type.clone();
            } else {
                frame.variables.push(var.clone());
            }
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

    // 在当前上下文中推入一个新的帧
    pub fn push_frame(&mut self) {
        self.current_context_mut().push(VariableFrame {
            variables: Vec::new(),
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

// Modified function signature and return type
pub fn analyze_ast<'t>(
    ast: &'t ASTNode,
    break_at_position: Option<usize>,
    dir_stack: &mut DirStack,
) -> AnalysisOutput<'t> {
    let mut context = VariableContext::new();
    // 向context里初始化内置函数
    context.push_context();
    context.push_frame();
    // let _ = context.define_variable(&Variable {
    //     name: "print".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "input".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "len".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "int".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "float".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "bytes".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "string".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "bool".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "load_clambda".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "json_encode".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
    // let _ = context.define_variable(&Variable {
    //     name: "json_decode".to_string(),
    //     assumed_type: AssumedType::Lambda,
    // });
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

    let mut errors = Vec::new(); // 重命名 warnings 为 errors 以匹配 AnalysisOutput 字段
    let mut warnings = Vec::new(); // 添加一个新的 Vec 用于存储 AnalyzeWarn
    let mut context_at_break: Option<VariableContext> = None; // Store context here

    analyze_node(
        ast,
        &mut context,
        &mut errors,   // 传递 errors Vec
        &mut warnings, // 传递 warnings Vec
        false,
        break_at_position,
        &mut context_at_break,
        dir_stack,
    );

    AnalysisOutput {
        errors,   // 使用更新后的名称
        warnings, // 添加 warnings 字段
        context_at_break,
    }
}

// Modified function signature
fn analyze_node<'t>(
    node: &'t ASTNode,
    context: &mut VariableContext,
    errors: &mut Vec<AnalyzeError<'t>>, // 重命名 warnings 为 errors
    warnings: &mut Vec<AnalyzeWarn<'t>>, // 添加 warnings 参数
    dynamic: bool,
    break_at_position: Option<usize>,
    context_at_break: &mut Option<VariableContext>,
    dir_stack: &mut DirStack,
) -> AssumedType {
    // 1. Check if analysis should stop globally (break point already found)
    if context_at_break.is_some() {
        return AssumedType::Unknown;
    }

    // 2. Check if the *start* of the current node is at or beyond the break position
    if let Some(break_pos) = break_at_position {
        if let Some(token) = node.start_token {
            // 使用 token.position (开始字节偏移)
            // 检查断点是否在 token 的范围内 [start, end)
            if token.position + token.origin_token.len() >= break_pos {
                // 我们已经到达或超过了目标位置。在分析此节点之前捕获上下文。
                *context_at_break = Some(context.clone()); // 克隆上下文状态
                return AssumedType::Unknown; // 停止此分支的分析
            }
        }
        // 可选：如果需要，处理没有 start_token 的节点，尽管大多数相关节点应该有它。
    }

    use super::ast::ASTNodeType;
    // Removed old auto_break check
    match &node.node_type {
        ASTNodeType::Let(var_name) => {
            if let Some(value_node) = node.children.first() {
                // Analyze the value expression first
                let assumed_type = analyze_node(
                    value_node,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                } // Check if break occurred during value analysis
                let var = Variable {
                    name: var_name.clone(),
                    assumed_type: assumed_type.clone(),
                };
                let _ = context.define_variable(&var);

                return assumed_type;
            }
            return AssumedType::Unknown;
        }
        ASTNodeType::Annotation(annotation) => {
            let mut assumed_type = AssumedType::Unknown;
            let is_dynamic = match annotation.as_str() {
                "dynamic" => true,
                "static" => false,
                "compile" => {
                    if break_at_position.is_none() {
                        // 特殊处理 @compile 注解 (当非断点分析时)
                        for child in &node.children {
                            // 检查子节点是否为字符串
                            if let ASTNodeType::String(file_path) = &child.node_type {
                                let read_file = std::fs::read_to_string(file_path);
                                if let Err(e) = read_file {
                                    // 使用 AnalyzeWarn::CompileError 记录读取错误
                                    let error_message =
                                        format!("Failed to read file '{}': {}", file_path, e);
                                    warnings.push(AnalyzeWarn::CompileError(child, error_message));
                                // 使用 child 作为错误关联的节点
                                } else {
                                    let parent_dir = std::path::Path::new(file_path)
                                        .parent()
                                        .unwrap_or_else(|| std::path::Path::new("."))
                                        .to_str()
                                        .unwrap_or("");
                                    let _ = dir_stack.push(&parent_dir);

                                    let code = read_file.unwrap();
                                    // 调用 build_code 函数编译代码
                                    let compile_result = build_code(&code, dir_stack);
                                    // 弹出目录栈
                                    let _ = dir_stack.pop();

                                    match compile_result {
                                        Ok(ir_package) => {
                                            // 替换文件扩展名为xbc
                                            let xbc_file_path =
                                                if let Some(pos) = file_path.rfind('.') {
                                                    format!("{}.xbc", &file_path[..pos])
                                                } else {
                                                    format!("{}.xbc", file_path)
                                                };
                                            let byte_code = compile_to_bytecode(&ir_package);
                                            match byte_code {
                                                Ok(byte_code) => {
                                                    // 将字节码写入文件
                                                    if let Err(e) =
                                                        byte_code.write_to_file(&xbc_file_path)
                                                    {
                                                        // 使用 AnalyzeWarn::CompileError 记录写入错误
                                                        let error_message = format!(
                                                        "Failed to write bytecode to file '{}': {}",
                                                        xbc_file_path, e
                                                    );
                                                        warnings.push(AnalyzeWarn::CompileError(
                                                            child,
                                                            error_message,
                                                        )); // 使用 child 作为错误关联的节点
                                                    }
                                                }
                                                Err(e) => {
                                                    // 使用 AnalyzeWarn::CompileError 记录编译错误
                                                    let error_message = format!(
                                                        "Compilation failed for file '{}': {}",
                                                        file_path, e
                                                    );
                                                    warnings.push(AnalyzeWarn::CompileError(
                                                        child,
                                                        error_message,
                                                    )); // 使用 child 作为错误关联的节点
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
                                                child,
                                                error_message,
                                            ));
                                            // 使用 child 作为错误关联的节点
                                        }
                                    }
                                }
                            } else {
                                // @compile 后面应该跟一个字符串字面量
                                let error_message = format!("@compile annotation expects a string literal file path, found: {:?}", child.node_type);
                                warnings.push(AnalyzeWarn::CompileError(child, error_message));
                                // 使用 child 作为错误关联的节点
                            }
                            // 仍然分析子节点本身，即使它是 @compile 的参数
                            analyze_node(
                                child,
                                context,
                                errors,   // Pass errors
                                warnings, // Pass warnings
                                dynamic,  // 继承 dynamic 状态或根据需要调整
                                break_at_position,
                                context_at_break,
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
                                    warnings.push(AnalyzeWarn::CompileError(child, error_message));
                                    // 使用 child 作为错误关联的节点
                                }
                            } else {
                                // @compile 后面应该跟一个字符串字面量
                                let error_message = format!("@compile annotation expects a string literal file path, found: {:?}", child.node_type);
                                warnings.push(AnalyzeWarn::CompileError(child, error_message));
                                // 使用 child 作为错误关联的节点
                            }
                            // 仍然分析子节点，以允许在路径字符串内部设置断点
                            analyze_node(
                                child,
                                context,
                                errors,
                                warnings,
                                dynamic, // 继承 dynamic 状态
                                break_at_position,
                                context_at_break,
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
                _ => dynamic, // Inherit dynamic status for other annotations
            };

            if annotation == "required" {
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
                        warnings.push(AnalyzeWarn::CompileError(first_child, error_message));
                        // 使用 first_child 作为错误关联的节点
                    }
                }
            }

            // 对非 @compile 注解或在断点分析时的 @compile 注解执行常规分析
            for child in &node.children {
                assumed_type = analyze_node(
                    child,
                    context,
                    errors,     // Pass errors
                    warnings,   // Pass warnings
                    is_dynamic, // 使用计算出的 dynamic 状态
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }

            return assumed_type;
        }

        ASTNodeType::Variable(var_name) => {
            // Check definition before potentially breaking
            if context.get_variable_current_context(var_name).is_none() {
                if !dynamic {
                    errors.push(AnalyzeError::UndefinedVariable(node)); // 使用 errors Vec
                }
            }
            // Return variable type or Unknown
            if let Some(var) = context.get_variable_current_context(var_name) {
                return var.assumed_type.clone();
            } else {
                return AssumedType::Unknown;
            }
        }
        ASTNodeType::Body => {
            // Body 和 Boundary 在当前上下文中创建新的作用域（帧）
            context.push_frame();
            let mut assumed_type = AssumedType::Unknown;
            for child in &node.children {
                assumed_type = analyze_node(
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
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
            }
            return assumed_type;
        }
        ASTNodeType::LambdaDef(is_dynamic_gen, is_capture, _) => {
            if *is_capture {
                // 检查 child[1]
                if let Some(func_node) = node.children.get(1) {
                    analyze_node(
                        func_node,
                        context,
                        errors,   // Pass errors
                        warnings, // Pass warnings
                        dynamic,
                        break_at_position,
                        context_at_break,
                        dir_stack,
                    );
                    if context_at_break.is_some() {
                        return AssumedType::Unknown;
                    }
                }
            }
            // 进入 Lambda 定义，创建一个全新的、隔离的上下文
            context.push_frame();
            // 在新的上下文中处理参数（参数定义在第一个帧中）
            if let Some(params) = node.children.first() {
                analyze_tuple_params(
                    params,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    // 如果在参数分析中中断，不弹出上下文
                    return AssumedType::Lambda;
                }
            }
            let args = context.last_context().unwrap().last().unwrap().clone();
            if context.pop_frame().is_err() {
                // 弹出 Lambda 定义的帧
                panic!("Failed to pop frame after Lambda definition");
            }
            if *is_dynamic_gen {
                context.push_frame();
                analyze_node(
                    &node.children.last().unwrap(),
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
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
                return AssumedType::Lambda;
            } else {
                context.push_context();
                // 将参数添加到新的上下文中
                for arg in args.variables {
                    let _ = context.define_variable(&arg);
                }
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

                // 在新的上下文中分析 Lambda 体
                if node.children.len() > 1 {
                    // Lambda 体可能创建自己的帧 (push_frame/pop_frame)
                    analyze_node(
                        &node.children.last().unwrap(),
                        context,
                        errors,   // Pass errors
                        warnings, // Pass warnings
                        dynamic,
                        break_at_position,
                        context_at_break,
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
                }
            }

            return AssumedType::Lambda;
        }
        ASTNodeType::LambdaCall => {
            if let Some(func_node) = node.children.first() {
                analyze_node(
                    func_node,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }

            if node.children.len() > 1 {
                analyze_node(
                    &node.children[1],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }

            return AssumedType::Unknown;
        }
        ASTNodeType::Expressions => {
            let mut last_type = AssumedType::Unknown;
            for child in &node.children {
                last_type = analyze_node(
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            return last_type;
        }
        ASTNodeType::Assign => {
            if node.children.len() >= 2 {
                // Analyze RHS first
                let assumed_type = analyze_node(
                    &node.children[1],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }

                // Analyze LHS (e.g., variable, index, attr) to check for errors, but its type doesn't override RHS
                analyze_node(
                    &node.children[0],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }

                // TODO: Potentially update variable type in context if LHS is a simple variable?
                // This requires LHS analysis to return info about what was assigned to.
                // For now, assignment doesn't update context type info here.

                return assumed_type;
            }
            return AssumedType::Unknown;
        }
        ASTNodeType::If => {
            if let Some(condition) = node.children.first() {
                analyze_node(
                    condition,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }

            let mut then_type = AssumedType::Unknown;
            if node.children.len() > 1 {
                then_type = analyze_node(
                    &node.children[1],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }

            if node.children.len() > 2 {
                analyze_node(
                    &node.children[2],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
                // TODO: Type could be union of then/else, for now just return then type
            }

            return then_type;
        }
        ASTNodeType::While => {
            if let Some(condition) = node.children.first() {
                analyze_node(
                    condition,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }

            let mut body_type = AssumedType::Unknown;
            if node.children.len() > 1 {
                body_type = analyze_node(
                    &node.children[1],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }

            return body_type; // Or Null/Unknown?
        }
        ASTNodeType::Return | ASTNodeType::Emit => {
            if let Some(value) = node.children.first() {
                let ret_type = analyze_node(
                    value,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
                return ret_type;
            }
            return AssumedType::Unknown; // Or Null?
        }
        ASTNodeType::Operation(_) => {
            let mut last_type = AssumedType::Unknown;
            for child in &node.children {
                last_type = analyze_node(
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            // TODO: Infer operation result type based on operator and operand types
            return last_type;
        }
        ASTNodeType::Tuple => {
            for child in &node.children {
                analyze_node(
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Tuple;
                } // Return Tuple type even if break occurred inside
            }
            return AssumedType::Tuple;
        }
        ASTNodeType::GetAttr => {
            if node.children.len() >= 2 {
                analyze_node(
                    &node.children[0],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                ); // Object
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
                // Don't analyze the attribute name itself as an expression usually
                // analyze_node(&node.children[1], context, errors, warnings, dynamic, break_at_position, context_at_break); // Attribute
                // if context_at_break.is_some() { return AssumedType::Unknown; }
            }
            // TODO: Could try to infer attribute type if object type is known
            return AssumedType::Unknown;
        }
        ASTNodeType::IndexOf => {
            if node.children.len() >= 2 {
                analyze_node(
                    &node.children[0],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                ); // Object
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
                analyze_node(
                    &node.children[1],
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                ); // Index
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            // TODO: Could try to infer element type if object type is known (e.g., Tuple, String)
            return AssumedType::Unknown;
        }
        ASTNodeType::Modifier(_) => {
            if let Some(target) = node.children.first() {
                let target_type = analyze_node(
                    target,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
                return target_type; // Modifier usually doesn't change the type
            }
            return AssumedType::Unknown;
        }
        ASTNodeType::KeyValue => {
            let _ = analyze_node(
                &node.children[0],
                context,
                errors,   // Pass errors
                warnings, // Pass warnings
                dynamic,
                break_at_position,
                context_at_break,
                dir_stack,
            ); // Key
            if context_at_break.is_some() {
                return AssumedType::KeyVal; // Return KeyVal even if break occurred inside
            }
            let _ = analyze_node(
                &node.children[1],
                context,
                errors,   // Pass errors
                warnings, // Pass warnings
                dynamic,
                break_at_position,
                context_at_break,
                dir_stack,
            ); // Value
            if context_at_break.is_some() {
                return AssumedType::KeyVal; // Return KeyVal even if break occurred inside
            }
            return AssumedType::KeyVal; // Return KeyVal type
        }
        ASTNodeType::Set => {
            for child in &node.children {
                // Iterate through all children for Set
                analyze_node(
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Set; // Return Set even if break occurred inside
                }
            }
            return AssumedType::Set; // Return Set type
        }
        ASTNodeType::NamedTo => {
            let _ = analyze_node(
                &node.children[0],
                context,
                errors,   // Pass errors
                warnings, // Pass warnings
                dynamic,
                break_at_position,
                context_at_break,
                dir_stack,
            ); // Name (usually string/identifier, not analyzed as variable)
            if context_at_break.is_some() {
                return AssumedType::NamedArgument; // Return NamedArgument even if break occurred inside
            }
            let _ = analyze_node(
                &node.children[1],
                context,
                errors,   // Pass errors
                warnings, // Pass warnings
                dynamic,
                break_at_position,
                context_at_break,
                dir_stack,
            ); // Value
            if context_at_break.is_some() {
                return AssumedType::NamedArgument; // Return NamedArgument even if break occurred inside
            }
            return AssumedType::NamedArgument; // Return NamedArgument type
        }
        // Simple types don't have children to analyze recursively
        ASTNodeType::String(_) => AssumedType::String,
        ASTNodeType::Boolean(_) => AssumedType::Boolean,
        ASTNodeType::Number(_) => AssumedType::Number,
        ASTNodeType::Base64(_) => AssumedType::Base64,
        ASTNodeType::Null => AssumedType::Null,
        ASTNodeType::Range => AssumedType::Range,
        // Default case for other node types
        _ => {
            let mut last_type = AssumedType::Unknown;
            for child in &node.children {
                last_type = analyze_node(
                    child,
                    context,
                    errors,   // Pass errors
                    warnings, // Pass warnings
                    dynamic,
                    break_at_position,
                    context_at_break,
                    dir_stack,
                );
                if context_at_break.is_some() {
                    return AssumedType::Unknown;
                }
            }
            return last_type;
        }
    }
}

// Modified function signature
fn analyze_tuple_params<'t>(
    params: &'t ASTNode,
    context: &mut VariableContext,
    errors: &mut Vec<AnalyzeError<'t>>,  // Renamed
    warnings: &mut Vec<AnalyzeWarn<'t>>, // Added
    dynamic: bool,
    break_at_position: Option<usize>,
    context_at_break: &mut Option<VariableContext>,
    dir_stack: &mut DirStack,
) {
    // Check break condition before processing parameters
    if context_at_break.is_some() {
        return;
    }
    if let Some(break_pos) = break_at_position {
        if let Some(token) = params.start_token {
            if token.position >= break_pos {
                *context_at_break = Some(context.clone());
                return;
            }
        }
    }

    use super::ast::ASTNodeType;

    if let ASTNodeType::Tuple = params.node_type {
        for param in &params.children {
            if context_at_break.is_some() {
                return;
            } // Check before each param
            if let Some(break_pos) = break_at_position {
                // Check start of individual param node
                if let Some(token) = param.start_token {
                    if token.position >= break_pos {
                        *context_at_break = Some(context.clone());
                        return;
                    }
                }
            }

            match &param.node_type {
                ASTNodeType::NamedTo => {
                    if param.children.len() >= 2 {
                        // Analyze default value first
                        let assumed_type = analyze_node(
                            &param.children[1],
                            context,
                            errors,   // Pass errors
                            warnings, // Pass warnings
                            dynamic,
                            break_at_position,
                            context_at_break,
                            dir_stack,
                        );
                        if context_at_break.is_some() {
                            return;
                        }

                        // Define parameter variable
                        if let ASTNodeType::String(var_name) = &param.children[0].node_type {
                            let var = Variable {
                                name: var_name.clone(),
                                assumed_type,
                            };
                            let _ = context.define_variable(&var);
                        } else {
                            // Analyze complex parameter structure (e.g., destructuring) - might need specific handling
                            analyze_node(
                                &param.children[0],
                                context,
                                errors,   // Pass errors
                                warnings, // Pass warnings
                                dynamic,
                                break_at_position,
                                context_at_break,
                                dir_stack,
                            );
                            if context_at_break.is_some() {
                                return;
                            }
                        }
                    }
                }
                // Handle other param types if necessary
                _ => {
                    analyze_node(
                        param,
                        context,
                        errors,   // Pass errors
                        warnings, // Pass warnings
                        dynamic,
                        break_at_position,
                        context_at_break,
                        dir_stack,
                    );
                    if context_at_break.is_some() {
                        return;
                    }
                }
            }
        }
    }
}

pub fn auto_capture_and_rebuild<'t>(node: &'t ASTNode<'t>) -> (HashSet<String>, ASTNode<'t>) {
    // return a set of required variables and reconstructed ASTNode
    let mut context = VariableContext::new();
    context.push_context(); // Create a new context for the frame
    context.push_frame(); // Create a new frame for the context
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
    auto_capture(&mut context, node, false)
}
// Auto-capture variables in the context
pub fn auto_capture<'t>(
    context: &mut VariableContext,
    node: &'t ASTNode<'t>,
    dynamic: bool,
) -> (HashSet<String>, ASTNode<'t>) {
    // return a set of required variables and reconstructed ASTNode
    use super::ast::ASTNodeType;

    // Helper function to process parameters, define them, and collect requirements from default values
    fn process_params<'t>(
        context: &mut VariableContext,
        params_node: &'t ASTNode<'t>,
        dynamic: bool,
    ) -> (HashSet<String>, ASTNode<'t>) {
        let mut required_vars = HashSet::new();
        let mut new_params_node = params_node.clone();
        new_params_node.children = Vec::new();

        if let ASTNodeType::Tuple = params_node.node_type {
            for param in &params_node.children {
                match &param.node_type {
                    ASTNodeType::NamedTo => {
                        if param.children.len() >= 2 {
                            // Analyze default value first
                            let (default_vars, default_node) =
                                auto_capture(context, &param.children[1], dynamic);
                            required_vars.extend(default_vars);

                            // Define parameter variable (assuming name is String for now)
                            if let ASTNodeType::String(var_name) = &param.children[0].node_type {
                                let var = Variable {
                                    name: var_name.clone(),
                                    assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                                };
                                let _ = context.define_variable(&var);

                                // Reconstruct NamedTo node
                                let mut new_param = param.clone();
                                new_param.children = vec![param.children[0].clone(), default_node];
                                new_params_node.children.push(new_param);
                            } else {
                                // Handle potential complex destructuring in param name (might need more logic)
                                // For now, just clone and add - might miss defining vars
                                new_params_node.children.push(param.clone());
                            }
                        } else {
                            // NamedTo without default value? Clone for now.
                            new_params_node.children.push(param.clone());
                        }
                    }
                    ASTNodeType::String(var_name) => {
                        // Simple parameter name
                        let var = Variable {
                            name: var_name.clone(),
                            assumed_type: AssumedType::Unknown,
                        };
                        let _ = context.define_variable(&var);
                        new_params_node.children.push(param.clone());
                    }
                    // Potentially handle other node types if parameters can be complex (e.g., destructuring tuples)
                    _ => {
                        // If param is not a simple name or NamedTo, analyze it recursively
                        // This might be needed for destructuring patterns
                        let (param_req_vars, new_param_node) =
                            auto_capture(context, param, dynamic);
                        required_vars.extend(param_req_vars);
                        new_params_node.children.push(new_param_node);
                        // Note: Need to ensure variables within destructuring are defined correctly.
                    }
                }
            }
        } else {
            // If params_node is not a Tuple, analyze it directly (should not happen for valid LambdaDef)
            return auto_capture(context, params_node, dynamic);
        }

        (required_vars, new_params_node)
    }

    match &node.node_type {
        ASTNodeType::Variable(var_name) => {
            if dynamic {
                return (HashSet::new(), node.clone()); // Dynamic variables are not captured
            }
            // Check if the variable is defined in the current context (any frame)
            if context.get_variable_current_context(var_name).is_none() {
                // If not defined anywhere in the current context stack, it's required from outside
                let mut required_vars = HashSet::new();
                required_vars.insert(var_name.clone());
                (required_vars, node.clone())
            } else {
                // Defined locally, no requirement from outside this context
                (HashSet::new(), node.clone())
            }
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
                        if context.get_variable_current_context(var_name_str).is_none() {
                            // If the variable is not defined in the current context, add it to required_vars
                            let _ = context.define_variable(&Variable {
                                name: var_name_str.clone(),
                                assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                            });
                            return (HashSet::new(), node.clone()); // Return empty required_vars and the node itself
                        }
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
            // Reconstruct the node with the new children
            (required_vars, new_node)
        }

        ASTNodeType::Let(name) => {
            let mut required_vars = HashSet::new();
            let mut new_node = node.clone();

            if let Some(value_node) = node.children.first() {
                new_node.children = Vec::new();
                // Analyze the value expression
                let (value_req_vars, new_value_node) = auto_capture(context, value_node, dynamic);
                required_vars.extend(value_req_vars);
                new_node.children.push(new_value_node);
                // Define the variable in the current context
                let var = Variable {
                    name: name.clone(),
                    assumed_type: AssumedType::Unknown, // Type doesn't matter much for capture
                };
                let _ = context.define_variable(&var);
            }
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
            let _ = context.pop_frame();
            (required_vars, new_node)
        }

        ASTNodeType::LambdaDef(is_dynamic_gen, is_capture, is_dynmaic_params) => {
            // --- Existing LambdaDef logic ---
            // (Keeping your original logic as requested, but noting potential refinements needed
            // based on the simplified approach for other nodes and parameter handling)

            // Placeholder for your existing LambdaDef logic.
            // It should:
            // 1. Handle existing capture node if `is_capture` is true.
            // 2. Create a new frame for parameters.
            // 3. Call `process_params` to analyze defaults, define params in the frame, and get requirements.
            // 4. Store the defined parameters (args_frame).
            // 5. Pop the parameter frame.
            // 6. If dynamic_gen: push frame, analyze body, pop frame.
            // 7. If not dynamic_gen: push a *new context*, define args from args_frame, define this/self, analyze body.
            // 8. Determine captured variables (body requirements not met by args/this/self).
            // 9. Add captured variables to the outer `required_vars`.
            // 10. Reconstruct the LambdaDef node, potentially adding captured vars to its parameter list if needed by the runtime.
            // 11. Pop the context if it was pushed.
            // 12. Return the outer `required_vars` and the reconstructed node.

            // --- Start of your existing LambdaDef logic ---
            let mut required_vars = HashSet::new();
            let mut capture_node = None;
            if *is_capture {
                // 检查 child[1]
                if let Some(func_node) = node.children.get(1) {
                    let (child_vars, child_node) = auto_capture(context, func_node, dynamic);
                    required_vars.extend(child_vars);
                    capture_node = Some(child_node);
                }
            }
            // 进入 Lambda 定义，创建一个全新的、隔离的上下文
            context.push_frame();
            // 在新的上下文中处理参数（参数定义在第一个帧中）
            let mut params_node_opt = None; // To store the potentially reconstructed params node
            if let Some(params) = node.children.first() {
                // Use the refined process_params helper
                let (param_req_vars, new_params_node) = process_params(context, params, dynamic);
                required_vars.extend(param_req_vars); // Collect requirements from default values
                params_node_opt = Some(new_params_node);
                // Note: process_params now defines variables in the current frame
            }
            let args_frame = context.current_context().last().unwrap().clone(); // Get the frame with defined args
            if context.pop_frame().is_err() {
                // 弹出 Lambda 定义的帧
                panic!("Failed to pop frame after Lambda parameters");
            }
            if *is_dynamic_gen {
                context.push_frame();
                let body_index = if *is_capture { 2 } else { 1 }; // Adjust index based on capture node
                let (body_vars, new_body_node) = if node.children.len() > body_index {
                    auto_capture(context, &node.children[body_index], dynamic)
                } else {
                    // Lambda with no body? Create an empty node or handle appropriately
                    (
                        HashSet::new(),
                        ASTNode {
                            node_type: ASTNodeType::Expressions,
                            children: vec![],
                            start_token: node.start_token,
                            end_token: node.end_token,
                        },
                    )
                };
                required_vars.extend(body_vars); // Collect requirements from body

                if context.pop_frame().is_err() {
                    panic!("Failed to pop frame after dynamic Lambda body");
                }

                // Reconstruct the node
                let final_params_node =
                    params_node_opt.unwrap_or_else(|| node.children.first().unwrap().clone());
                let mut children = vec![final_params_node];
                if let Some(capt) = capture_node {
                    children.push(capt);
                }
                children.push(new_body_node);

                return (
                    required_vars,
                    ASTNode {
                        node_type: ASTNodeType::LambdaDef(
                            *is_dynamic_gen,
                            *is_capture,
                            *is_dynmaic_params,
                        ), // is_capture might need update based on actual captures
                        children,
                        start_token: node.start_token,
                        end_token: node.end_token,
                    },
                );
            } else {
                context.push_context();
                // 将参数添加到新的上下文中
                for arg in args_frame.variables {
                    // Use args from the stored frame
                    let _ = context.define_variable(&arg);
                }
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

                let mut body_node = None;
                let mut body_vars = HashSet::new();
                // 在新的上下文中分析 Lambda 体
                let body_index = if *is_capture { 2 } else { 1 }; // Adjust index based on capture node
                if node.children.len() > body_index {
                    let (child_vars, child_node) =
                        auto_capture(context, &node.children[body_index], dynamic);
                    body_vars.extend(child_vars); // These are requirements *within* the lambda's context
                    body_node = Some(child_node);
                }
                let _ = context.pop_context(); // 退出 Lambda，恢复到父上下文

                // Determine actual captured variables needed from the outer scope
                let mut captured_vars = HashSet::new();
                for var_name in body_vars {
                    // Is this variable defined within the lambda's own context (args, this, self)?
                    if context.get_variable(&var_name).is_some() {
                        captured_vars.insert(var_name.clone());
                        if context.get_variable_current_context(&var_name).is_none() {
                            // If not defined in the current context, it's a captured variable
                            required_vars.insert(var_name.clone());
                        }
                    } else {
                        // If the variable is not defined in the current context, it's required from outside
                        required_vars.insert(var_name.clone());
                    }
                }

                // Reconstruct parameter list, potentially adding captured variables
                let mut final_params_node =
                    params_node_opt.unwrap_or_else(|| node.children.first().unwrap().clone());
                for var in &captured_vars {
                    // Iterate over the *actual* captures
                    final_params_node.children.push(ASTNode {
                        node_type: ASTNodeType::NamedTo,
                        children: vec![
                            ASTNode {
                                node_type: ASTNodeType::String(var.clone()),
                                children: Vec::new(),
                                start_token: node.end_token,
                                end_token: node.end_token,
                            },
                            ASTNode {
                                // This represents reading the captured var from the outer scope
                                node_type: ASTNodeType::Variable(var.clone()),
                                children: Vec::new(),
                                start_token: node.end_token,
                                end_token: node.end_token,
                            },
                        ],
                        start_token: node.end_token,
                        end_token: node.end_token,
                    });
                }

                // Reconstruct children
                let mut children = vec![final_params_node];
                if let Some(capt) = capture_node {
                    children.push(capt);
                } // Keep original capture node if present
                if let Some(body) = body_node {
                    children.push(body);
                }

                return (
                    required_vars,
                    ASTNode {
                        node_type: ASTNodeType::LambdaDef(
                            *is_dynamic_gen,
                            *is_capture, // Use actual capture status
                            *is_dynmaic_params,
                        ),
                        children,
                        start_token: node.start_token,
                        end_token: node.end_token,
                    },
                );
            }
            // --- End of your existing LambdaDef logic ---
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
