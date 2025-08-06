//! 编译时求解器模块：提供 Onion 语言编译期表达式求值、AST 扩展和状态管理。
//!
//! 本模块实现了编译时代码执行机制，支持在编译期运行 Onion 代码、管理编译时状态、
//! 处理模块导入、以及 AST 的动态扩展。主要用于实现宏、编译时计算、条件编译等功能。
//!
//! # 核心功能
//! - 编译时表达式求值（`solve` 方法）
//! - AST 节点扩展（`expand` 方法）
//! - 状态管理和变量定义（def/undef/ifdef）
//! - 模块导入和循环检测（include）
//! - 内置函数和 AST 构造绑定
//!
//! # 用法示例
//! ```ignore
//! let mut solver = ComptimeSolver::new(user_defs, cycle_detector);
//! let expanded_ast = solver.solve(&original_ast)?;
//! ```

use std::{
    collections::HashMap,
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
    diagnostics::{collector::DiagnosticCollector, Diagnostic, SourceLocation},
    ir_generator::ir_generator::{IRGenerator, NameSpace},
    parser::{
        analyzer::{analyze_ast, auto_capture_and_rebuild},
        ast::{ASTNode, ASTNodeType},
        comptime::{ast_bindings, native::wrap_native_function, OnionASTObject}, Source,
    },
    utils::cycle_detector::CycleDetector,
};

/// 编译时状态管理器。
///
/// 维护编译时的内置定义和用户定义，支持状态注入到 VM 执行环境。
#[derive(Debug)]
pub struct ComptimeState {
    builtin_definitions: Arc<HashMap<String, OnionStaticObject>>,
    user_definitions: Arc<RwLock<HashMap<String, OnionStaticObject>>>,
}

impl ComptimeState {
    /// 将编译时状态注入到 VM 捕获环境中。
    ///
    /// # 参数
    /// - `capture`：VM 捕获环境映射。
    ///
    /// # 返回
    /// 成功时返回 `Ok(())`，失败时返回 `RuntimeError`。
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

/// 编译时诊断信息类型。
///
/// 封装编译时过程中可能出现的各种错误，包括运行时错误、借用错误、IR 翻译错误等。
#[derive(Debug, Clone)]
pub enum ComptimeDiagnostic {
    /// 运行时错误。
    RuntimeError(Option<SourceLocation>, RuntimeError),
    /// 借用检查错误。
    BorrowError(Option<SourceLocation>, String),
    /// IR 翻译错误。
    IRTranslatorError(Option<SourceLocation>, IRTranslatorError),
}

impl Diagnostic for ComptimeDiagnostic {
    fn severity(&self) -> crate::diagnostics::ReportSeverity {
        match self {
            ComptimeDiagnostic::RuntimeError(_, _) => crate::diagnostics::ReportSeverity::Error,
            ComptimeDiagnostic::BorrowError(_, _) => crate::diagnostics::ReportSeverity::Error,
            ComptimeDiagnostic::IRTranslatorError(_, _) => {
                crate::diagnostics::ReportSeverity::Error
            }
        }
    }

    fn title(&self) -> String {
        "Comptime Error".to_string()
    }

    fn message(&self) -> String {
        match self {
            ComptimeDiagnostic::RuntimeError(_, err) => format!("Runtime Error: {}", err),
            ComptimeDiagnostic::BorrowError(_, msg) => format!("Borrow Error: {}", msg),
            ComptimeDiagnostic::IRTranslatorError(_, err) => {
                format!("IR Translator Error: {}", err)
            }
        }
    }

    fn location(&self) -> Option<crate::diagnostics::SourceLocation> {
        match self {
            ComptimeDiagnostic::RuntimeError(loc, _) => loc.clone(),
            ComptimeDiagnostic::BorrowError(loc, _) => loc.clone(),
            ComptimeDiagnostic::IRTranslatorError(loc, _) => loc.clone(),
        }
    }

    fn help(&self) -> Option<String> {
        None
    }

    fn copy(&self) -> Box<dyn Diagnostic> {
        Box::new(self.clone())
    }
}

/// 编译时求解器主结构。
///
/// 负责编译时 AST 的求解、扩展、以及编译时代码的执行。
/// 集成了状态管理、诊断收集、VM 执行等功能。
#[derive(Debug)]
pub struct ComptimeSolver {
    state: ComptimeState,
    diagnostics: Arc<RwLock<DiagnosticCollector>>,
}

impl ComptimeSolver {
    /// 创建新的编译时求解器。
    ///
    /// # 参数
    /// - `user_definitions`：用户定义的变量映射。
    /// - `import_cycle_detector`：导入循环检测器。
    ///
    /// # 返回
    /// 新的 `ComptimeSolver` 实例。
    pub fn new(
        user_definitions: Arc<RwLock<HashMap<String, OnionStaticObject>>>,
        import_cycle_detector: CycleDetector<PathBuf>,
    ) -> Self {
        let user_definitions_ref = user_definitions.clone();

        let diagnostics = Arc::new(RwLock::new(DiagnosticCollector::new()));
        let diagnostics_ref = diagnostics.clone();

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
                                    let mut state = user_definitions_ref.write().map_err(|e| {
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

        let user_definitions_ref = user_definitions.clone();
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
                                    let mut state = user_definitions_ref.write().map_err(|e| {
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

        let user_definitions_ref = user_definitions.clone();
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
                                    let state = user_definitions_ref.read().map_err(|e| {
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
                                        source_location: None,
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
                                    let source = Source::from_file(&abs_path).map_err(|e| {
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
                                    use crate::parser::lexer::tokenizer;
                                    let tokens = tokenizer::tokenize(&source);
                                    let tokens = tokenizer::reject_comment(&tokens);
                                    let gathered = ast_token_stream::from_stream(&tokens);

                                    let mut collector = diagnostics_ref.write().map_err(|e| {
                                        RuntimeError::BorrowError(e.to_string().into())
                                    })?;

                                    let ast =
                                        build_ast(&mut collector, gathered).map_err(|_| {
                                            RuntimeError::DetailedError(
                                                "Solver failed to build AST".into(),
                                            )
                                        })?;

                                    if collector.has_errors() {
                                        return Err(RuntimeError::DetailedError(
                                            "Solver produced errors while building AST".into(),
                                        ));
                                    }

                                    let mut sub_solver = ComptimeSolver::new(
                                        cloned_ref.clone(),
                                        import_cycle_detector.enter(abs_path).map_err(|path| {
                                            RuntimeError::DetailedError(
                                                format!("Cyclic reference detected: {:?}", path)
                                                    .into(),
                                            )
                                        })?,
                                    );

                                    let result = sub_solver.solve(&ast);
                                    for diagnostic in sub_solver
                                        .diagnostics
                                        .read()
                                        .map_err(|e| {
                                            RuntimeError::BorrowError(e.to_string().into())
                                        })?
                                        .diagnostics()
                                    {
                                        collector.report(diagnostic.copy());
                                    }

                                    if result.is_err() {
                                        return Err(RuntimeError::DetailedError(
                                            "Sub-solver failed".into(),
                                        ));
                                    }

                                    if sub_solver
                                        .diagnostics
                                        .read()
                                        .map_err(|e| {
                                            RuntimeError::BorrowError(e.to_string().into())
                                        })?
                                        .has_errors()
                                    {
                                        return Err(RuntimeError::DetailedError(
                                            "Sub-solver produced errors while executing expression"
                                                .into(),
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
            diagnostics: diagnostics,
        }
    }

    /// 获取诊断收集器的引用。
    pub fn diagnostics(&self) -> &Arc<RwLock<DiagnosticCollector>> {
        &self.diagnostics
    }

    /// 递归求解 AST 节点，处理编译时表达式。
    ///
    /// # 参数
    /// - `ast`：要求解的 AST 节点。
    ///
    /// # 返回
    /// 求解后的 AST 节点，或错误。
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
                source_location: iteration_result.source_location.clone(),
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

    /// 扩展给定的 AST 节点（如果是编译时节点）。
    ///
    /// # 参数
    /// - `ast`：要扩展的 AST 节点。
    ///
    /// # 返回
    /// - `Ok(Some(expanded))`：节点已扩展
    /// - `Ok(None)`：节点无需扩展
    /// - `Err(())`：扩展失败
    pub fn expand(&mut self, ast: &ASTNode) -> Result<Option<ASTNode>, ()> {
        let ASTNodeType::Comptime = ast.node_type else {
            return Ok(None);
        };

        let mut collector = self
            .diagnostics
            .write()
            .expect("Failed to lock diagnostics collector");
        let mut context = vec![];
        context.extend(
            self.state
                .builtin_definitions
                .iter()
                .map(|(name, _)| ASTNode {
                    node_type: ASTNodeType::Required(name.clone()),
                    source_location: None,
                    children: vec![],
                })
                .collect::<Vec<_>>(),
        );
        context.extend(
            self.state
                .user_definitions
                .read()
                .map_err(|e| {
                    collector.report(ComptimeDiagnostic::BorrowError(
                        ast.source_location.clone(),
                        e.to_string(),
                    ));
                    ()
                })?
                .iter()
                .map(|(name, _)| ASTNode {
                    node_type: ASTNodeType::Required(name.clone()),
                    source_location: None,
                    children: vec![],
                })
                .collect::<Vec<_>>(),
        );
        context.extend(ast.children.iter().cloned());

        let ast_with_context = ASTNode {
            node_type: ASTNodeType::Expressions,
            source_location: ast.source_location.clone(),
            children: context,
        };

        let (_required_vars, rebuilt_ast) = auto_capture_and_rebuild(&ast_with_context);

        let _ = analyze_ast(&rebuilt_ast, &mut collector, &None)?;

        let namespace = NameSpace::new("Main".to_string(), None);
        let mut functions = Functions::new();
        let mut ir_generator = IRGenerator::new(&mut functions, namespace);

        let mut ir = ir_generator.generate(&mut collector, &rebuilt_ast)?;

        ir.push((DebugInfo::new((0, 0)), IR::Return));
        functions.append("__main__".to_string(), ir);

        let package = functions.build_instructions(None);
        let mut translator = IRTranslator::new(&package);

        let byte_code = match translator.translate() {
            Ok(_) => translator.get_result(),
            Err(e) => {
                collector.report(ComptimeDiagnostic::IRTranslatorError(
                    ast.source_location.clone(),
                    e,
                ));
                return Err(());
            }
        };

        drop(collector); // 释放锁，允许其他线程访问 diagnostics
        let result = match self.execute(&byte_code) {
            Ok(result) => result,
            Err(err) => {
                let mut collector = self
                    .diagnostics
                    .write()
                    .expect("Failed to lock diagnostics collector");
                collector.report(ComptimeDiagnostic::RuntimeError(
                    ast.source_location.clone(),
                    err,
                ));
                return Err(());
            }
        };
        match OnionASTObject::from_onion(result.weak()) {
            Ok(ast_object) => Ok(Some(ast_object)),
            Err(err) => {
                let mut collector = self
                    .diagnostics
                    .write()
                    .expect("Failed to lock diagnostics collector");
                collector.report(ComptimeDiagnostic::RuntimeError(
                    ast.source_location.clone(),
                    err,
                ));
                Err(())
            }
        }
    }

    /// 执行编译时代码并返回结果。
    ///
    /// # 参数
    /// - `vm_instructions_package`：要执行的 VM 指令包。
    ///
    /// # 返回
    /// 执行结果的静态对象，或运行时错误。
    fn execute(
        &mut self,
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
            LambdaType::Atomic,
        );

        let args = OnionTuple::new_static(vec![]);

        let mut scheduler: Box<dyn Runnable> = Box::new(Scheduler::new(vec![Box::new(
            OnionLambdaRunnableLauncher::new(lambda.weak(), args, Ok)?,
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
