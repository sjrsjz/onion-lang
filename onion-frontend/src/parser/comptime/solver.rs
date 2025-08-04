use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    path::PathBuf,
    sync::{Arc, RwLock},
};

use onion_vm::{
    GC,
    lambda::{
        runnable::{Runnable, RuntimeError, StepResult},
        scheduler::scheduler::Scheduler,
    },
    types::{
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinition},
            launcher::OnionLambdaRunnableLauncher,
            parameter::LambdaParameter,
            vm_instructions::{
                instruction_set::VMInstructionPackage,
                ir::{DebugInfo, Functions, IR},
                ir_translator::{IRTranslator, IRTranslatorError},
            },
        },
        object::{OnionObject, OnionObjectCell, OnionStaticObject},
        tuple::OnionTuple,
    },
    unwrap_object,
    utils::fastmap::{OnionFastMap, OnionKeyPool},
};

use crate::{
    ir_generator::ir_generator::{IRGenerator, IRGeneratorError, NameSpace},
    parser::{
        analyzer::{AnalyzeError, AnalyzeWarn, analyze_ast, auto_capture_and_rebuild},
        ast::{ASTNode, ASTNodeType},
        comptime::{OnionASTObject, ast_bindings, native::wrap_native_function},
        diagnostics::{ReportSeverity, format_node_based_report},
    },
    utils::cycle_detector::CycleDetector,
};

#[derive(Debug)]
pub struct ComptimeState {
    builtin_definitions: Arc<HashMap<String, OnionStaticObject>>,
    user_definitions: Arc<RwLock<HashMap<String, OnionStaticObject>>>,
}

impl ComptimeState {
    pub fn inject_state(
        &self,
        capture: &mut OnionFastMap<Box<str>, OnionObject>,
    ) -> Result<(), RuntimeError> {
        // Inject state logic here

        for (key, value) in self
            .user_definitions
            .read()
            .map_err(|e| RuntimeError::BorrowError(e.to_string().into()))?
            .iter()
        {
            capture.push(key.as_str(), value.weak().clone());
        }

        for (key, value) in self.builtin_definitions.iter() {
            capture.push(key.as_str(), value.weak().clone());
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ComptimeError {
    RuntimeError(RuntimeError),
    AnalysisError(Vec<AnalyzeError>),
    IRGeneratorError(IRGeneratorError),
    IRTranslatorError(IRTranslatorError),
}

impl Display for ComptimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ComptimeError::RuntimeError(err) => {
                // 直接委托给 RuntimeError 的 Display 实现
                write!(f, "Runtime Error during compile-time execution: {}", err)
            }
            ComptimeError::AnalysisError(errors) => {
                // 如果有多个分析错误，将它们都格式化并连接起来
                writeln!(f, "Analysis failed with {} error(s):", errors.len())?;
                for (i, err) in errors.iter().enumerate() {
                    // 我们使用 err.format() 来获得带颜色的、详细的用户友好输出
                    // 但对于纯文本的 Display，我们可能需要一个不带颜色的版本。
                    // 这里为了简单，我们假设 err.to_string() 也能提供有用的信息。
                    writeln!(f, "[{}] {}", i + 1, err.format())?;
                }
                Ok(())
            }
            ComptimeError::IRGeneratorError(err) => {
                // 委托给 IRGeneratorError 的 Display 实现
                write!(f, "IR Generation Error: {}", err)
            }
            ComptimeError::IRTranslatorError(err) => {
                // 委托给 IRTranslatorError 的 Display 实现
                write!(f, "IR Translation Error: {}", err)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ComptimeWarning {
    AnalysisWarning(Vec<AnalyzeWarn>),
}

impl Display for ComptimeWarning {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ComptimeWarning::AnalysisWarning(warnings) => {
                // 格式化并连接所有分析阶段的警告
                writeln!(f, "Analysis produced {} warning(s):", warnings.len())?;
                for (i, warn) in warnings.iter().enumerate() {
                    writeln!(f, "[{}] {}", i + 1, warn.format())?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct ComptimeSolver {
    state: ComptimeState,
    errors: Vec<(ComptimeError, ASTNode)>,
    warnings: Vec<(ComptimeWarning, ASTNode)>,
}

impl ComptimeSolver {
    pub fn new(
        user_definitions: HashMap<String, OnionStaticObject>,
        import_cycle_detector: CycleDetector<PathBuf>,
    ) -> Self {
        let user_definitions = Arc::new(RwLock::new(user_definitions));
        let cloned_ref = user_definitions.clone();
        let mut builtin_definitions = HashMap::new();
        builtin_definitions.insert(
            "def".into(),
            wrap_native_function(
                LambdaParameter::top("def"),
                OnionFastMap::default(),
                "comptime::def",
                OnionKeyPool::create(vec!["def".into()]),
                Arc::new(
                    move |argument: &OnionFastMap<Box<str>, OnionStaticObject>,
                          _gc: &mut GC<OnionObjectCell>|
                          -> Result<OnionStaticObject, RuntimeError> {
                        match argument.get("def") {
                            Some(v) => v.weak().with_data(|v| match v {
                                OnionObject::Pair(pair) => {
                                    let k = pair.get_key().to_string(&vec![])?;
                                    let v = pair.get_value().stabilize();
                                    let mut state = cloned_ref.write().map_err(|e| {
                                        RuntimeError::BorrowError(e.to_string().into())
                                    })?;
                                    state.insert(k, v);
                                    Ok(OnionObject::Undefined(None).stabilize())
                                }
                                _ => Err(RuntimeError::InvalidType("Expect Pair for 'def'".into())),
                            }),
                            None => {
                                Err(RuntimeError::DetailedError("No 'def' in arguments".into()))
                            }
                        }
                    },
                ),
            ),
        );

        let cloned_ref = user_definitions.clone();
        builtin_definitions.insert(
            "undef".into(),
            wrap_native_function(
                LambdaParameter::top("name"),
                OnionFastMap::default(),
                "comptime::undef",
                OnionKeyPool::create(vec!["name".into()]),
                Arc::new(
                    move |argument: &OnionFastMap<Box<str>, OnionStaticObject>,
                          _gc: &mut GC<OnionObjectCell>|
                          -> Result<OnionStaticObject, RuntimeError> {
                        match argument.get("name") {
                            Some(v) => v.weak().with_data(|v| match v {
                                OnionObject::String(name) => {
                                    let mut state = cloned_ref.write().map_err(|e| {
                                        RuntimeError::BorrowError(e.to_string().into())
                                    })?;
                                    state.remove(name.as_ref());
                                    Ok(OnionObject::Undefined(None).stabilize())
                                }
                                _ => Err(RuntimeError::InvalidType(
                                    "Expect String for 'name'".into(),
                                )),
                            }),
                            None => {
                                Err(RuntimeError::DetailedError("No 'name' in arguments".into()))
                            }
                        }
                    },
                ),
            ),
        );

        let cloned_ref = user_definitions.clone();
        builtin_definitions.insert(
            "ifdef".into(),
            wrap_native_function(
                LambdaParameter::top("name"),
                OnionFastMap::default(),
                "comptime::ifdef",
                OnionKeyPool::create(vec!["name".into()]),
                Arc::new(
                    move |argument: &OnionFastMap<Box<str>, OnionStaticObject>,
                          _gc: &mut GC<OnionObjectCell>|
                          -> Result<OnionStaticObject, RuntimeError> {
                        match argument.get("name") {
                            Some(v) => v.weak().with_data(|v| match v {
                                OnionObject::String(name) => {
                                    let state = cloned_ref.read().map_err(|e| {
                                        RuntimeError::BorrowError(e.to_string().into())
                                    })?;
                                    Ok(OnionObject::Boolean(state.contains_key(name.as_ref()))
                                        .stabilize())
                                }
                                _ => Err(RuntimeError::InvalidType(
                                    "Expect String for 'name'".into(),
                                )),
                            }),
                            None => {
                                Err(RuntimeError::DetailedError("No 'name' in arguments".into()))
                            }
                        }
                    },
                ),
            ),
        );

        builtin_definitions.insert(
            "required".into(),
            wrap_native_function(
                LambdaParameter::top("name"),
                OnionFastMap::default(),
                "comptime::required",
                OnionKeyPool::create(vec!["name".into()]),
                Arc::new(
                    move |argument: &OnionFastMap<Box<str>, OnionStaticObject>,
                          _gc: &mut GC<OnionObjectCell>|
                          -> Result<OnionStaticObject, RuntimeError> {
                        match argument.get("name") {
                            Some(v) => v.weak().with_data(|v| match v {
                                OnionObject::String(name) => Ok(OnionObject::Custom(Arc::new(
                                    OnionASTObject::new(ASTNode {
                                        node_type: ASTNodeType::Required(name.to_string()),
                                        start_token: None,
                                        end_token: None,
                                        children: vec![],
                                    }),
                                ))
                                .stabilize()),
                                _ => Err(RuntimeError::InvalidType(
                                    "Expect String for 'name'".into(),
                                )),
                            }),
                            None => {
                                Err(RuntimeError::DetailedError("No 'name' in arguments".into()))
                            }
                        }
                    },
                ),
            ),
        );

        let cloned_ref = user_definitions.clone();
        builtin_definitions.insert(
            "include".into(),
            wrap_native_function(
                LambdaParameter::top("path"),
                OnionFastMap::default(),
                "comptime::include",
                OnionKeyPool::create(vec!["path".into()]),
                Arc::new(
                    move |argument: &OnionFastMap<Box<str>, OnionStaticObject>,
                          _gc: &mut GC<OnionObjectCell>|
                          -> Result<OnionStaticObject, RuntimeError> {
                        match argument.get("path") {
                            Some(v) => v.weak().with_data(|v| match v {
                                OnionObject::String(path) => {
                                    let abs_path =
                                        match import_cycle_detector.last() {
                                            Some(file) => {
                                                let dir = file.parent().unwrap_or(file.as_path());
                                                let mut target_path = PathBuf::from(path.as_ref());
                                                if !target_path.is_absolute() {
                                                    target_path = dir.join(&target_path);
                                                }
                                                target_path.canonicalize().map_err(|e| {
                                                    RuntimeError::DetailedError(
                                                        format!("Failed to canonicalize path: {e}")
                                                            .into(),
                                                    )
                                                })?
                                            }
                                            None => {
                                                let mut target_path = PathBuf::from(path.as_ref());
                                                if !target_path.is_absolute() {
                                                    target_path =
                                                        std::env::current_dir()
                                                            .map_err(|e| {
                                                                RuntimeError::DetailedError(
                        format!("Failed to get current dir: {e}").into(),
                    )
                                                            })?
                                                            .join(&target_path);
                                                }
                                                target_path.canonicalize().map_err(|e| {
                                                    RuntimeError::DetailedError(
                                                        format!("Failed to canonicalize path: {e}")
                                                            .into(),
                                                    )
                                                })?
                                            }
                                        };
                                    // 读取文件内容
                                    let code = std::fs::read_to_string(&abs_path).map_err(|e| {
                                        RuntimeError::DetailedError(
                                            format!(
                                                "Failed to read file {}: {e}",
                                                abs_path.display()
                                            )
                                            .into(),
                                        )
                                    })?;

                                    // 词法分析、AST 构建
                                    use crate::parser::ast::{ast_token_stream, build_ast};
                                    use crate::parser::lexer::lexer;
                                    let tokens = lexer::tokenize(&code);
                                    let tokens = lexer::reject_comment(&tokens);
                                    let gathered = ast_token_stream::from_stream(&tokens);
                                    let ast = build_ast(gathered).map_err(|err_token| {
                                        RuntimeError::DetailedError(
                                            err_token.format().to_string().into(),
                                        )
                                    })?;

                                    let mut sub_solver = ComptimeSolver::new(
                                        cloned_ref
                                            .read()
                                            .map_err(|e| {
                                                RuntimeError::DetailedError(e.to_string().into())
                                            })?
                                            .clone(),
                                        import_cycle_detector.enter(abs_path).map_err(|path| {
                                            RuntimeError::DetailedError(
                                                format!("Cyclic reference detected: {:?}", path)
                                                    .into(),
                                            )
                                        })?,
                                    );
                                    let result = sub_solver.solve(&ast);
                                    if result.is_err() {
                                        let mut error_text = String::new();
                                        for (error, ast) in sub_solver.errors() {
                                            error_text.push_str(&format_node_based_report(
                                                ReportSeverity::Error,
                                                "Sub solver error",
                                                &error.to_string(),
                                                ast,
                                                "You may need to fix the error in the included file.",
                                            ));
                                            error_text.push('\n');
                                        }
                                        return Err(RuntimeError::DetailedError(
                                            error_text.into(),
                                        ));
                                    }
                                    Ok(OnionObject::Custom(Arc::new(OnionASTObject::new(
                                        result.unwrap(),
                                    )))
                                    .consume_and_stabilize())
                                }
                                _ => Err(RuntimeError::InvalidType(
                                    "Expect String for 'path'".into(),
                                )),
                            }),
                            None => {
                                Err(RuntimeError::DetailedError("No 'path' in arguments".into()))
                            }
                        }
                    },
                ),
            ),
        );

        builtin_definitions.insert("ast".to_string(), ast_bindings::build_module());

        ComptimeSolver {
            state: ComptimeState {
                builtin_definitions: Arc::new(builtin_definitions),
                user_definitions,
            },
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn errors(&self) -> &[(ComptimeError, ASTNode)] {
        &self.errors
    }

    pub fn warnings(&self) -> &[(ComptimeWarning, ASTNode)] {
        &self.warnings
    }

    #[stacksafe::stacksafe]
    pub fn solve(&mut self, ast: &ASTNode) -> Result<ASTNode, ()> {
        let mut iteration_result = ast.clone();
        loop {
            let mut children = Vec::new();
            for child in &iteration_result.children {
                children.push(self.solve(child)?);
            }
            let result = ASTNode {
                node_type: iteration_result.node_type.clone(),
                start_token: iteration_result.start_token.clone(),
                end_token: iteration_result.end_token.clone(),
                children,
            };
            match self.expand(&result)? {
                Some(expanded_ast) => {
                    if expanded_ast == iteration_result {
                        return Ok(expanded_ast);
                    } else {
                        iteration_result = expanded_ast;
                    }
                }
                None => {
                    return Ok(result);
                }
            }
        }
    }

    /// Expand the given AST node using the current state.
    pub fn expand(&mut self, ast: &ASTNode) -> Result<Option<ASTNode>, ()> {
        let ASTNodeType::Comptime = ast.node_type else {
            return Ok(None);
        };

        let mut context = vec![];
        context.extend(
            self.state
                .builtin_definitions
                .iter()
                .map(|(name, _)| ASTNode {
                    node_type: ASTNodeType::Required(name.clone()),
                    start_token: None,
                    end_token: None,
                    children: vec![],
                })
                .collect::<Vec<_>>(),
        );
        context.extend(
            self.state
                .user_definitions
                .read()
                .map_err(|e| {
                    self.errors.push((
                        ComptimeError::RuntimeError(RuntimeError::BorrowError(
                            e.to_string().into(),
                        )),
                        ast.clone(),
                    ));
                    ()
                })?
                .iter()
                .map(|(name, _)| ASTNode {
                    node_type: ASTNodeType::Required(name.clone()),
                    start_token: None,
                    end_token: None,
                    children: vec![],
                })
                .collect::<Vec<_>>(),
        );
        context.extend(ast.children.iter().cloned());

        let ast_with_context = ASTNode {
            node_type: ASTNodeType::Expressions,
            start_token: None,
            end_token: None,
            children: context,
        };

        let (_required_vars, rebuilt_ast) = auto_capture_and_rebuild(&ast_with_context);

        let analyse_result = analyze_ast(&rebuilt_ast, None);

        let mut errors = "".to_string();
        for error in &analyse_result.errors {
            //println!("{}", error.format(code.to_string()).bright_red());
            errors.push_str(&error.format());
            errors.push_str("\n");
        }
        if !analyse_result.errors.is_empty() {
            self.errors.push((
                ComptimeError::AnalysisError(analyse_result.errors),
                ast.clone(),
            ));
            return Err(());
        }
        if !analyse_result.warnings.is_empty() {
            self.warnings.push((
                ComptimeWarning::AnalysisWarning(analyse_result.warnings),
                ast.clone(),
            ));
        }

        let namespace = NameSpace::new("Main".to_string(), None);
        let mut functions = Functions::new();
        let mut ir_generator = IRGenerator::new(&mut functions, namespace);

        let ir = match ir_generator.generate(&rebuilt_ast) {
            Ok(ir) => ir,
            Err(err) => {
                self.errors
                    .push((ComptimeError::IRGeneratorError(err), ast.clone()));
                return Err(());
            }
        };

        let mut ir = ir;
        ir.push((DebugInfo::new((0, 0)), IR::Return));
        functions.append("__main__".to_string(), ir);

        let package = functions.build_instructions(None);
        let mut translator = IRTranslator::new(&package);

        let byte_code = match translator.translate() {
            Ok(_) => translator.get_result(),
            Err(e) => {
                self.errors
                    .push((ComptimeError::IRTranslatorError(e), ast.clone()));
                return Err(());
            }
        };

        let result = match self.execute(&byte_code) {
            Ok(result) => result,
            Err(e) => {
                self.errors
                    .push((ComptimeError::RuntimeError(e), ast.clone()));
                return Err(());
            }
        };
        match OnionASTObject::from_onion(result.weak()) {
            Ok(ast_object) => Ok(Some(ast_object)),
            Err(err) => {
                self.errors
                    .push((ComptimeError::RuntimeError(err), ast.clone()));
                Err(())
            }
        }
    }

    fn execute(
        &mut self, // 我们声明 '&mut self' 是因为我们需要修改 'ComptimeSolver' 的状态，而直接写成 '&self' 虽然可以工作，但与函数会引发的副作用不符
        vm_instructions_package: &VMInstructionPackage,
    ) -> Result<OnionStaticObject, RuntimeError> {
        let mut gc = GC::new_with_memory_threshold(1024 * 1024); // 1 MB threshold

        if let Err(e) = VMInstructionPackage::validate(vm_instructions_package) {
            return Err(RuntimeError::DetailedError(
                format!("Invalid VM instruction package: {e}").into(),
            ));
        }
        let mut capture = OnionFastMap::new(vm_instructions_package.create_key_pool());

        self.state.inject_state(&mut capture)?;

        // Create Lambda definition
        let lambda = OnionLambdaDefinition::new_static(
            LambdaParameter::Multiple(Box::new([])),
            LambdaBody::Instruction(Arc::new(vm_instructions_package.clone())),
            capture,
            "__main__".into(),
            LambdaType::Normal,
        );

        let args = OnionTuple::new_static(vec![]);

        let mut scheduler: Box<dyn Runnable> = Box::new(Scheduler::new(vec![Box::new(
            OnionLambdaRunnableLauncher::new_static(lambda.weak(), args, Ok)?,
        )]));
        // Execute code
        loop {
            match scheduler.step(&mut gc) {
                StepResult::Continue => {
                    // Continue to next step
                }
                StepResult::SpawnRunnable(_) => {
                    return Err(RuntimeError::DetailedError(
                        "Cannot spawn async task in sync context".into(),
                    ));
                }
                StepResult::Error(ref error) => {
                    if let RuntimeError::Pending = error {
                        // Pending 状态，正常继续
                        continue;
                    }
                    return Err(error.clone());
                }
                StepResult::NewRunnable(_) => {
                    unreachable!()
                }
                StepResult::ReplaceRunnable(_) => {
                    unreachable!()
                }
                StepResult::Return(ref result) => {
                    let result_borrowed = result.weak();
                    let result = unwrap_object!(result_borrowed, OnionObject::Pair)?;
                    let success = *unwrap_object!(result.get_key(), OnionObject::Boolean)?;
                    if !success {
                        let value_text = result.get_value().with_data(|data| match data {
                            OnionObject::Undefined(Some(str)) => Ok(str.to_string()),
                            _ => Ok(data.to_string(&vec![]).unwrap_or_else(|e| {
                                format!("[Failed to convert value to string: {e}]")
                            })),
                        })?;

                        let mut error_text =
                            format!("{} {}", "Execution returned a failure value:", value_text);

                        // 打印上下文以帮助调试为什么会返回失败
                        error_text.push_str(&format!("\n{}", "Context at Time of Failure Return:"));
                        error_text.push_str(&scheduler.format_context());

                        return Err(RuntimeError::DetailedError(error_text.into()));
                    }
                    return Ok(result.get_value().stabilize());
                }
            }
        }
    }
}
