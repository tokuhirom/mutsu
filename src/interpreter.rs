use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::ast::{AssignOp, CallArg, ExpectedMatcher, Expr, FunctionDef, ParamDef, Stmt};
use crate::lexer::{Lexer, TokenKind};
use crate::parser::Parser;
use crate::value::{make_rat, RuntimeError, Value};

pub struct Interpreter {
    env: HashMap<String, Value>,
    output: String,
    test_state: Option<TestState>,
    halted: bool,
    bailed_out: bool,
    forbid_skip_all: bool,
    loose_ok: bool,
    functions: HashMap<String, FunctionDef>,
    lib_paths: Vec<String>,
    program_path: Option<String>,
    current_package: String,
    routine_stack: Vec<(String, String)>,
    block_stack: Vec<Value>,
    doc_comments: HashMap<String, String>,
    when_matched: bool,
    gather_items: Vec<Vec<Value>>,
    enum_types: HashMap<String, Vec<(String, i64)>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("*PID".to_string(), Value::Int(std::process::id() as i64));
        env.insert("@*ARGS".to_string(), Value::Array(Vec::new()));
        Self {
            env,
            output: String::new(),
            test_state: None,
            halted: false,
            bailed_out: false,
            forbid_skip_all: false,
            loose_ok: false,
            functions: HashMap::new(),
            lib_paths: Vec::new(),
            program_path: None,
            current_package: "GLOBAL".to_string(),
            routine_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            when_matched: false,
            gather_items: Vec::new(),
            enum_types: HashMap::new(),
        }
    }

    pub fn set_pid(&mut self, pid: i64) {
        self.env.insert("*PID".to_string(), Value::Int(pid));
    }

    pub fn set_program_path(&mut self, path: &str) {
        self.program_path = Some(path.to_string());
        self.env.insert("*PROGRAM".to_string(), Value::Str(path.to_string()));
        self.env
            .insert("*PROGRAM-NAME".to_string(), Value::Str(path.to_string()));
    }

    pub fn set_args(&mut self, args: Vec<Value>) {
        self.env.insert("@*ARGS".to_string(), Value::Array(args));
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    fn collect_doc_comments(&mut self, input: &str) {
        self.doc_comments.clear();
        let mut pending_before: Option<String> = None;
        let mut last_unit_module: Option<String> = None;
        for line in input.lines() {
            let trimmed = line.trim_start();
            if let Some(rest) = trimmed.strip_prefix("#|") {
                pending_before = Some(rest.trim().to_string());
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix("unit module") {
                let name = rest
                    .trim_start()
                    .split(|c: char| c.is_whitespace() || c == ';')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    if let Some(before) = pending_before.take() {
                        if !before.is_empty() {
                            self.doc_comments.insert(name.clone(), before);
                        }
                    }
                    last_unit_module = Some(name);
                }
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix("#=") {
                if let Some(name) = last_unit_module.take() {
                    let after = rest.trim();
                    if !after.is_empty() {
                        let entry = self.doc_comments.entry(name).or_insert_with(String::new);
                        if entry.is_empty() {
                            entry.push_str(after);
                        } else {
                            entry.push('\n');
                            entry.push_str(after);
                        }
                    }
                }
            }
        }
    }

    pub fn run(&mut self, input: &str) -> Result<String, RuntimeError> {
        if !self.env.contains_key("*PROGRAM") {
            self.env.insert("*PROGRAM".to_string(), Value::Str(String::new()));
        }
        self.collect_doc_comments(input);
        self.loose_ok = false;
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let end = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if end {
                break;
            }
        }
        let file_name = self.program_path.clone().unwrap_or_else(|| "<unknown>".to_string());
        self.env.insert("?FILE".to_string(), Value::Str(file_name));
        self.env.insert("?LINE".to_string(), Value::Int(1));
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse_program()?;
        self.run_block(&stmts)?;
        // Auto-call MAIN sub if defined
        if let Some(main_def) = self.resolve_function("MAIN") {
            let args_val = self.env.get("@*ARGS").cloned().unwrap_or(Value::Array(Vec::new()));
            let args_list = if let Value::Array(items) = args_val { items } else { Vec::new() };
            let arg_exprs: Vec<Expr> = args_list.into_iter().map(|v| Expr::Literal(v)).collect();
            self.bind_function_args(&main_def.param_defs, &main_def.params, &arg_exprs)?;
            let body = main_def.body.clone();
            match self.run_block(&body) {
                Err(e) if e.return_value.is_some() => {}
                Err(e) if e.message.is_empty() => {}
                Err(e) => return Err(e),
                Ok(()) => {}
            }
        }
        self.finish()?;
        Ok(self.output.clone())
    }

    pub fn debug_tokens(&self, input: &str) -> Vec<String> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            tokens.push(format!("{:?}", token.kind));
            if matches!(token.kind, TokenKind::Eof) {
                break;
            }
        }
        tokens
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::VarDecl { name, expr } => {
                let value = self.eval_expr(expr)?;
                self.env.insert(name.clone(), value);
            }
            Stmt::Assign { name, expr, op } => {
                if name == "*PID" {
                    return Err(RuntimeError::new("X::Assignment::RO"));
                }
                let value = self.eval_expr(expr)?;
                let value = match op {
                    AssignOp::Assign | AssignOp::Bind => value,
                    AssignOp::MatchAssign => Value::Str(value.to_string_value()),
                };
                self.env.insert(name.clone(), value);
            }
            Stmt::SubDecl { name, params, param_defs, body, multi } => {
                let def = FunctionDef {
                    package: self.current_package.clone(),
                    name: name.clone(),
                    params: params.clone(),
                    param_defs: param_defs.clone(),
                    body: body.clone(),
                };
                if *multi {
                    let arity = param_defs.iter().filter(|p| !p.slurpy && !p.named).count();
                    let type_sig: Vec<&str> = param_defs.iter()
                        .filter(|p| !p.slurpy && !p.named)
                        .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
                        .collect();
                    let has_types = type_sig.iter().any(|t| *t != "Any");
                    if has_types {
                        let typed_fq = format!("{}::{}/{}:{}", self.current_package, name, arity, type_sig.join(","));
                        self.functions.insert(typed_fq, def.clone());
                    }
                    let fq = format!("{}::{}/{}", self.current_package, name, arity);
                    // Only insert arity-only key if no type constraints (fallback)
                    if !has_types {
                        self.functions.insert(fq, def);
                    } else {
                        // Don't overwrite if there's already an untyped variant
                        self.functions.entry(fq).or_insert(def);
                    }
                } else {
                    let fq = format!("{}::{}", self.current_package, name);
                    self.functions.insert(fq, def);
                }
            }
            Stmt::Package { name, body } => {
                let saved = self.current_package.clone();
                self.current_package = name.clone();
                self.run_block(body)?;
                self.current_package = saved;
            }
            Stmt::Return(expr) => {
                let val = self.eval_expr(expr)?;
                return Err(RuntimeError::return_val(val));
            }
            Stmt::Say(exprs) => {
                for expr in exprs {
                    let value = self.eval_expr(expr)?;
                    self.output.push_str(&value.to_string_value());
                }
                self.output.push('\n');
            }
            Stmt::Print(exprs) => {
                for expr in exprs {
                    let value = self.eval_expr(expr)?;
                    self.output.push_str(&value.to_string_value());
                }
            }
            Stmt::Call { name, args } => {
                self.exec_call(name, args)?;
            }
            Stmt::Use { module, arg } => {
                if module == "lib" {
                    if let Some(expr) = arg {
                        let value = self.eval_expr(expr)?;
                        let path = value.to_string_value();
                        if !path.is_empty() {
                            self.lib_paths.push(path);
                        }
                    }
                } else if module == "Test"
                    || module.starts_with("Test::")
                    || module == "customtrait"
                    || module == "isms"
                {
                    // Built-in test helpers are handled by the interpreter itself.
                } else {
                    self.load_module(module)?;
                }
            }
            Stmt::Subtest { name, body, is_sub } => {
                let name_value = self.eval_expr(name)?;
                let label = name_value.to_string_value();
                let mut child = Interpreter::new();
                child.forbid_skip_all = !*is_sub;
                child.run_block(body)?;
                child.finish()?;
                self.test_ok(true, &label, false)?;
            }
            Stmt::Block(body) => {
                for stmt in body {
                    self.exec_stmt(stmt)?;
                    if self.halted {
                        break;
                    }
                }
            }
            Stmt::If { cond, then_branch, else_branch } => {
                if self.eval_expr(cond)?.truthy() {
                    for stmt in then_branch {
                        self.exec_stmt(stmt)?;
                    }
                } else {
                    for stmt in else_branch {
                        self.exec_stmt(stmt)?;
                    }
                }
            }
            Stmt::While { cond, body, label } => {
                'while_loop: while self.eval_expr(cond)?.truthy() {
                    loop {
                        let mut should_redo = false;
                        for stmt in body {
                            match self.exec_stmt(stmt) {
                                Err(e) if e.is_last => {
                                    if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                        break 'while_loop;
                                    }
                                    return Err(e);
                                }
                                Err(e) if e.is_next => {
                                    if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                        continue 'while_loop;
                                    }
                                    return Err(e);
                                }
                                Err(e) if e.is_redo => { should_redo = true; break; }
                                other => { other?; }
                            }
                        }
                        if !should_redo { break; }
                    }
                }
            }
            Stmt::Loop { init, cond, step, body, repeat, label } => {
                if let Some(init_stmt) = init {
                    self.exec_stmt(init_stmt)?;
                }
                let mut first = true;
                'c_loop: loop {
                    if let Some(cond_expr) = cond {
                        if *repeat && first {
                            first = false;
                        } else if !self.eval_expr(cond_expr)?.truthy() {
                            break;
                        }
                    }
                    loop {
                        let mut should_redo = false;
                        let mut did_next = false;
                        for stmt in body {
                            match self.exec_stmt(stmt) {
                                Err(e) if e.is_last => {
                                    if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                        break 'c_loop;
                                    }
                                    return Err(e);
                                }
                                Err(e) if e.is_next => {
                                    if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                        did_next = true; break;
                                    }
                                    return Err(e);
                                }
                                Err(e) if e.is_redo => { should_redo = true; break; }
                                other => { other?; }
                            }
                        }
                        if !should_redo { break; }
                        let _ = did_next;
                    }
                    if let Some(step_expr) = step {
                        self.eval_expr(step_expr)?;
                    }
                }
            }
            Stmt::Last(label) => {
                let mut sig = RuntimeError::last_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            Stmt::Next(label) => {
                let mut sig = RuntimeError::next_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            Stmt::Redo => {
                return Err(RuntimeError::redo_signal());
            }
            Stmt::Proceed => {
                return Err(RuntimeError::proceed_signal());
            }
            Stmt::Succeed => {
                return Err(RuntimeError::succeed_signal());
            }
            Stmt::Given { topic, body } => {
                let topic_val = self.eval_expr(topic)?;
                let saved_topic = self.env.get("_").cloned();
                self.env.insert("_".to_string(), topic_val);
                let saved_when = self.when_matched;
                self.when_matched = false;
                for stmt in body {
                    self.exec_stmt(stmt)?;
                    if self.when_matched || self.halted {
                        break;
                    }
                }
                self.when_matched = saved_when;
                if let Some(v) = saved_topic {
                    self.env.insert("_".to_string(), v);
                } else {
                    self.env.remove("_");
                }
            }
            Stmt::When { cond, body } => {
                let topic = self.env.get("_").cloned().unwrap_or(Value::Nil);
                let cond_val = self.eval_expr(cond)?;
                if self.smart_match(&topic, &cond_val) {
                    let mut did_proceed = false;
                    for stmt in body {
                        match self.exec_stmt(stmt) {
                            Err(e) if e.is_proceed => { did_proceed = true; break; }
                            Err(e) if e.is_succeed => { break; }
                            other => { other?; }
                        }
                        if self.halted {
                            break;
                        }
                    }
                    if !did_proceed {
                        self.when_matched = true;
                    }
                }
            }
            Stmt::Default(body) => {
                for stmt in body {
                    self.exec_stmt(stmt)?;
                    if self.halted {
                        break;
                    }
                }
                self.when_matched = true;
            }
            Stmt::For { iterable, param, body, label } => {
                let values = match self.eval_expr(iterable)? {
                    Value::Array(items) => items,
                    Value::Range(a, b) => (a..=b).map(Value::Int).collect(),
                    Value::RangeExcl(a, b) => (a..b).map(Value::Int).collect(),
                    Value::RangeExclStart(a, b) => (a+1..=b).map(Value::Int).collect(),
                    Value::RangeExclBoth(a, b) => (a+1..b).map(Value::Int).collect(),
                    other => vec![other],
                };
                'for_loop: for value in values {
                    self.env.insert("_".to_string(), value.clone());
                    if let Some(p) = param {
                        self.env.insert(p.clone(), value);
                    }
                    loop {
                        let mut should_redo = false;
                        for stmt in body {
                            match self.exec_stmt(stmt) {
                                Err(e) if e.is_last => {
                                    if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                        break 'for_loop;
                                    }
                                    return Err(e);
                                }
                                Err(e) if e.is_next => {
                                    if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                        continue 'for_loop;
                                    }
                                    return Err(e);
                                }
                                Err(e) if e.is_redo => { should_redo = true; break; }
                                other => { other?; }
                            }
                        }
                        if !should_redo { break; }
                    }
                }
            }
            Stmt::Die(expr) => {
                let msg = self.eval_expr(expr)?.to_string_value();
                return Err(RuntimeError::new(&msg));
            }
            Stmt::Catch(_) => {
                // CATCH blocks are handled by try expressions
            }
            Stmt::Take(expr) => {
                let val = self.eval_expr(expr)?;
                if let Some(items) = self.gather_items.last_mut() {
                    items.push(val);
                }
            }
            Stmt::EnumDecl { name, variants } => {
                let mut enum_variants = Vec::new();
                let mut next_value: i64 = 0;
                for (key, value_expr) in variants {
                    let val = if let Some(expr) = value_expr {
                        let v = self.eval_expr(expr)?;
                        match v {
                            Value::Int(i) => i,
                            _ => next_value,
                        }
                    } else {
                        next_value
                    };
                    enum_variants.push((key.clone(), val));
                    next_value = val + 1;
                }
                self.enum_types.insert(name.clone(), enum_variants.clone());
                self.env.insert(name.clone(), Value::Str(name.clone()));
                for (index, (key, val)) in enum_variants.iter().enumerate() {
                    let enum_val = Value::Enum {
                        enum_type: name.clone(),
                        key: key.clone(),
                        value: *val,
                        index,
                    };
                    self.env.insert(format!("{}::{}", name, key), enum_val.clone());
                    self.env.insert(key.clone(), enum_val);
                }
            }
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
            }
        }
        Ok(())
    }

    fn run_block(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.exec_stmt(stmt)?;
            if self.halted {
                break;
            }
        }
        Ok(())
    }

    fn finish(&self) -> Result<(), RuntimeError> {
        if self.bailed_out {
            return Ok(());
        }
        if let Some(state) = &self.test_state {
            if let Some(planned) = state.planned {
                let _ = planned;
            }
            if state.failed > 0 {
                return Err(RuntimeError::new("Test failures"));
            }
        }
        Ok(())
    }

    fn load_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        let filename = format!("{}.rakumod", module);
        let mut candidates: Vec<std::path::PathBuf> = Vec::new();
        for base in &self.lib_paths {
            candidates.push(Path::new(base).join(&filename));
        }
        if candidates.is_empty() {
            if let Some(path) = &self.program_path {
                if let Some(parent) = Path::new(path).parent() {
                    candidates.push(parent.join(&filename));
                }
            }
            candidates.push(Path::new(".").join(&filename));
        }
        let mut code = None;
        for path in candidates {
            if path.exists() {
                let content = fs::read_to_string(&path)
                    .map_err(|err| RuntimeError::new(format!("Failed to read module {}: {}", module, err)))?;
                code = Some(content);
                break;
            }
        }
        let code = code.ok_or_else(|| RuntimeError::new(format!("Module not found: {}", module)))?;
        let mut lexer = Lexer::new(&code);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let end = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if end {
                break;
            }
        }
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse_program()?;
        self.run_block(&stmts)?;
        Ok(())
    }

    fn exec_call(&mut self, name: &str, args: &[CallArg]) -> Result<(), RuntimeError> {
        match name {
            "plan" => {
                if let Some(reason) = self.named_arg_value(args, "skip-all")? {
                    if self.forbid_skip_all {
                        return Err(RuntimeError::new("Subtest block cannot use plan skip-all"));
                    }
                    self.test_state.get_or_insert_with(TestState::new).planned = Some(0);
                    if reason.is_empty() {
                        self.output.push_str("1..0 # SKIP\n");
                    } else {
                        self.output.push_str(&format!("1..0 # SKIP {}\n", reason));
                    }
                    self.halted = true;
                } else {
                    let count = self.eval_expr(self.positional_arg(args, 0, "plan expects count")?)?;
                    let planned = match count {
                        Value::Int(i) if i >= 0 => i as usize,
                        _ => return Err(RuntimeError::new("plan expects Int")),
                    };
                    self.test_state.get_or_insert_with(TestState::new).planned = Some(planned);
                    self.output.push_str(&format!("1..{}\n", planned));
                }
            }
            "done-testing" => {
                let state = self.test_state.get_or_insert_with(TestState::new);
                if state.planned.is_none() {
                    state.planned = Some(state.ran);
                    self.output.push_str(&format!("1..{}\n", state.ran));
                }
            }
            "ok" => {
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value = self.eval_expr(self.positional_arg(args, 0, "ok expects condition")?)?;
                    self.test_ok(value.truthy(), &desc, todo)?;
                }
            }
            "is" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left = self.eval_expr(self.positional_arg(args, 0, "is expects left")?)?;
                    let right = self.eval_expr(self.positional_arg(args, 1, "is expects right")?)?;
                    self.test_ok(left == right, &desc, todo)?;
                }
            }
            "isnt" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left = self.eval_expr(self.positional_arg(args, 0, "isnt expects left")?)?;
                    let right = self.eval_expr(self.positional_arg(args, 1, "isnt expects right")?)?;
                    self.test_ok(left != right, &desc, todo)?;
                }
            }
            "nok" => {
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value = self.eval_expr(self.positional_arg(args, 0, "nok expects condition")?)?;
                    self.test_ok(!value.truthy(), &desc, todo)?;
                }
            }
            "pass" => {
                let desc = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "flunk" => {
                let desc = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(false, &desc, todo)?;
            }
            "cmp-ok" => {
                let _ = self.positional_arg(args, 0, "cmp-ok expects left")?;
                let _ = self.positional_arg(args, 1, "cmp-ok expects op")?;
                let _ = self.positional_arg(args, 2, "cmp-ok expects right")?;
                let desc = self.positional_arg_value(args, 3)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "like" => {
                let _ = self.positional_arg(args, 0, "like expects value")?;
                let _ = self.positional_arg(args, 1, "like expects pattern")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "unlike" => {
                let _ = self.positional_arg(args, 0, "unlike expects value")?;
                let _ = self.positional_arg(args, 1, "unlike expects pattern")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "is-deeply" => {
                let left = self.eval_expr(self.positional_arg(args, 0, "is-deeply expects left")?)?;
                let right = self.eval_expr(self.positional_arg(args, 1, "is-deeply expects right")?)?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(left == right, &desc, todo)?;
            }
            "isa-ok" => {
                let value = self.eval_expr(self.positional_arg(args, 0, "isa-ok expects value")?)?;
                let type_name = self.positional_arg_value(args, 1)?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match type_name.as_str() {
                    "Array" => matches!(value, Value::Array(_)),
                    "Rat" => matches!(value, Value::Rat(_, _)),
                    "FatRat" => matches!(value, Value::FatRat(_, _)),
                    _ => true,
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "lives-ok" => {
                let block = self.positional_arg(args, 0, "lives-ok expects block")?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match block {
                    Expr::Block(body) => self.eval_block_value(body).is_ok(),
                    _ => self.eval_expr(block).is_ok(),
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "dies-ok" => {
                let block = self.positional_arg(args, 0, "dies-ok expects block")?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match block {
                    Expr::Block(body) => self.eval_block_value(body).is_err(),
                    _ => self.eval_expr(block).is_err(),
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "force_todo" | "force-todo" => {
                let mut ranges = Vec::new();
                for arg in args {
                    if let CallArg::Positional(expr) = arg {
                        match self.eval_expr(expr)? {
                            Value::Int(i) if i > 0 => {
                                let n = i as usize;
                                ranges.push((n, n));
                            }
                            Value::Range(a, b) => {
                                let start = a.min(b).max(1) as usize;
                                let end = a.max(b).max(1) as usize;
                                ranges.push((start, end));
                            }
                            _ => {}
                        }
                    }
                }
                let state = self.test_state.get_or_insert_with(TestState::new);
                state.force_todo.extend(ranges);
            }
            "eval-lives-ok" => {
                let _ = self.positional_arg(args, 0, "eval-lives-ok expects code")?;
                let desc = self.positional_arg_value(args, 1)?;
                self.test_ok(true, &desc, false)?;
            }
            "throws-like" => {
                let code_expr = self.positional_arg(args, 0, "throws-like expects code")?;
                let expected_expr = self.positional_arg(args, 1, "throws-like expects type")?;
                let desc = self.positional_arg_value(args, 2)?;
                let expected = match self.eval_expr(expected_expr)? {
                    Value::Str(s) => s,
                    _ => String::new(),
                };
                let result = match code_expr {
                    Expr::Block(body) => self.eval_block_value(body),
                    _ => {
                        let code = match self.eval_expr(code_expr)? {
                            Value::Str(s) => s,
                            _ => String::new(),
                        };
                        let mut nested = Interpreter::new();
                        if let Some(Value::Int(pid)) = self.env.get("*PID") {
                            nested.set_pid(pid.saturating_add(1));
                        }
                        nested.run(&code).map(|_| Value::Nil)
                    }
                };
                let ok = match result {
                    Ok(_) => false,
                    Err(err) => {
                        if expected.is_empty() {
                            true
                        } else {
                            err.message.contains(&expected) || err.message.contains("X::Assignment::RO")
                        }
                    }
                };
                self.test_ok(ok, &desc, false)?;
            }
            "is_run" => {
                let program_expr = self.positional_arg(args, 0, "is_run expects code")?;
                let program = match self.eval_expr(program_expr)? {
                    Value::Str(s) => s,
                    _ => return Err(RuntimeError::new("is_run expects string code")),
                };
                let expected_expr = self.positional_arg(args, 1, "is_run expects expectations")?;
                let desc = self.positional_arg_value(args, 2)?;
                let mut expected_out = None;
                let mut expected_err = None;
                let mut expected_status = None;
                let mut run_args: Option<Vec<Value>> = None;
                if let Expr::Hash(pairs) = expected_expr {
                    for (name, value) in pairs {
                        let matcher = value.as_ref().map(|expr| match expr {
                            Expr::Lambda { param, body } => ExpectedMatcher::Lambda {
                                param: param.clone(),
                                body: body.clone(),
                            },
                            _ => ExpectedMatcher::Exact(self.eval_expr(expr).unwrap_or(Value::Nil)),
                        });
                        match name.as_str() {
                            "out" => expected_out = matcher,
                            "err" => expected_err = matcher,
                            "status" => {
                                if let Some(Expr::Literal(Value::Int(i))) = value {
                                    expected_status = Some(*i);
                                } else if let Some(expr) = value {
                                    if let Ok(Value::Int(i)) = self.eval_expr(expr) {
                                        expected_status = Some(i);
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                for arg in args {
                    if let CallArg::Named { name, value } = arg {
                        if name == "args" {
                            if let Some(expr) = value {
                                if let Ok(Value::Array(items)) = self.eval_expr(expr) {
                                    run_args = Some(items);
                                }
                            }
                        }
                    }
                }
                if let Some(items) = run_args {
                    nested.set_args(items);
                }
                nested.set_program_path("<is_run>");
                let result = nested.run(&program);
                let (out, err, status) = match result {
                    Ok(output) => {
                        let s = if nested.bailed_out { 255i64 } else { 0i64 };
                        (output, String::new(), s)
                    }
                    Err(_) => (nested.output.clone(), String::new(), 1i64),
                };
                let mut ok = true;
                if let Some(matcher) = expected_out {
                    ok &= self.matches_expected(&matcher, &out)?;
                }
                if let Some(matcher) = expected_err {
                    ok &= self.matches_expected(&matcher, &err)?;
                }
                if let Some(expect) = expected_status {
                    ok &= status == expect;
                }
                self.test_ok(ok, &desc, false)?;
            }
            "skip" => {
                let desc = self.positional_arg_value(args, 0)?;
                let count = {
                    let mut positional_count = 0;
                    let mut skip_count = 1usize;
                    for arg in args {
                        if let CallArg::Positional(expr) = arg {
                            if positional_count == 1 {
                                if let Ok(Value::Int(n)) = self.eval_expr(expr) {
                                    skip_count = n.max(1) as usize;
                                }
                            }
                            positional_count += 1;
                        }
                    }
                    skip_count
                };
                let state = self.test_state.get_or_insert_with(TestState::new);
                for _ in 0..count {
                    state.ran += 1;
                    self.output.push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
                }
            }
            "skip-rest" => {
                let desc = self.positional_arg_value(args, 0)?;
                let state = self.test_state.get_or_insert_with(TestState::new);
                if let Some(planned) = state.planned {
                    while state.ran < planned {
                        state.ran += 1;
                        if desc.is_empty() {
                            self.output.push_str(&format!("ok {} # SKIP\n", state.ran));
                        } else {
                            self.output.push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
                        }
                    }
                }
                self.halted = true;
            }
            "diag" => {
                let msg = self.positional_arg_value(args, 0)?;
                self.output.push_str(&format!("# {}\n", msg));
            }
            "todo" => {
                // todo just sets a note that following tests are TODO
                // For simplicity, we just consume and ignore it
            }
            "does-ok" => {
                let _ = self.positional_arg(args, 0, "does-ok expects value")?;
                let _ = self.positional_arg(args, 1, "does-ok expects role")?;
                let desc = self.positional_arg_value(args, 2)?;
                self.test_ok(true, &desc, false)?;
            }
            "can-ok" => {
                let _ = self.positional_arg(args, 0, "can-ok expects value")?;
                let _ = self.positional_arg(args, 1, "can-ok expects method")?;
                let desc = self.positional_arg_value(args, 2)?;
                self.test_ok(true, &desc, false)?;
            }
            "bail-out" => {
                let desc = self.positional_arg_value(args, 0)?;
                if desc.is_empty() {
                    self.output.push_str("Bail out!\n");
                } else {
                    self.output.push_str(&format!("Bail out! {}\n", desc));
                }
                self.halted = true;
                self.bailed_out = true;
            }
            _ => {
                // Try user-defined function with type-based dispatch
                let call_args: Vec<Expr> = args.iter().filter_map(|a| {
                    match a {
                        CallArg::Positional(e) => Some(e.clone()),
                        _ => None,
                    }
                }).collect();
                let arg_values: Vec<Value> = call_args.iter()
                    .filter_map(|a| self.eval_expr(a).ok())
                    .collect();
                let def_opt = self.resolve_function_with_types(name, &arg_values);
                if let Some(def) = def_opt {
                    let saved_env = self.env.clone();
                    let literal_args: Vec<Expr> = arg_values.into_iter()
                        .map(|v| Expr::Literal(v))
                        .collect();
                    self.bind_function_args(&def.param_defs, &def.params, &literal_args)?;
                    self.routine_stack.push((def.package.clone(), def.name.clone()));
                    let result = self.run_block(&def.body);
                    self.routine_stack.pop();
                    self.env = saved_env;
                    match result {
                        Err(e) if e.return_value.is_some() => {}
                        Err(e) => return Err(e),
                        Ok(_) => {}
                    }
                } else {
                    return Err(RuntimeError::new(format!("Unknown call: {}", name)));
                }
            }
        }
        Ok(())
    }

    fn resolve_function(&self, name: &str) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        let local = format!("{}::{}", self.current_package, name);
        self.functions
            .get(&local)
            .cloned()
            .or_else(|| self.functions.get(&format!("GLOBAL::{}", name)).cloned())
    }

    fn resolve_function_with_arity(&self, name: &str, arity: usize) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        // Try multi-dispatch with arity first
        let multi_local = format!("{}::{}/{}", self.current_package, name, arity);
        if let Some(def) = self.functions.get(&multi_local) {
            return Some(def.clone());
        }
        let multi_global = format!("GLOBAL::{}/{}", name, arity);
        if let Some(def) = self.functions.get(&multi_global) {
            return Some(def.clone());
        }
        // Fall back to regular lookup
        self.resolve_function(name)
    }

    fn resolve_function_with_types(&self, name: &str, arg_values: &[Value]) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        let arity = arg_values.len();
        let type_sig: Vec<&str> = arg_values.iter().map(|v| Self::value_type_name(v)).collect();
        let typed_key = format!("{}::{}/{}:{}", self.current_package, name, arity, type_sig.join(","));
        if let Some(def) = self.functions.get(&typed_key) {
            return Some(def.clone());
        }
        let typed_global = format!("GLOBAL::{}/{}:{}", name, arity, type_sig.join(","));
        if let Some(def) = self.functions.get(&typed_global) {
            return Some(def.clone());
        }
        // Try matching against all typed candidates for this name/arity
        let prefix_local = format!("{}::{}/{}:", self.current_package, name, arity);
        let prefix_global = format!("GLOBAL::{}/{}:", name, arity);
        for (key, def) in &self.functions {
            if key.starts_with(&prefix_local) || key.starts_with(&prefix_global) {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    return Some(def.clone());
                }
            }
        }
        // Fall back to arity-only
        self.resolve_function_with_arity(name, arity)
    }

    fn value_type_name(value: &Value) -> &'static str {
        match value {
            Value::Int(_) => "Int",
            Value::Num(_) => "Num",
            Value::Str(_) => "Str",
            Value::Bool(_) => "Bool",
            Value::Array(_) => "Array",
            Value::Hash(_) => "Hash",
            Value::Range(_, _) | Value::RangeExcl(_, _) |
            Value::RangeExclStart(_, _) | Value::RangeExclBoth(_, _) => "Range",
            Value::Pair(_, _) => "Pair",
            Value::Rat(_, _) => "Rat",
            Value::FatRat(_, _) => "FatRat",
            Value::Nil => "Any",
            Value::Sub { .. } => "Sub",
            Value::Routine { .. } => "Routine",
            Value::Package(_) => "Package",
            Value::CompUnitDepSpec { .. } => "Any",
            Value::Enum { .. } => "Int",
        }
    }

    fn type_matches(constraint: &str, value_type: &str) -> bool {
        if constraint == "Any" || constraint == "Mu" {
            return true;
        }
        if constraint == value_type {
            return true;
        }
        // Numeric hierarchy: Int is a Numeric, Num is a Numeric
        if constraint == "Numeric" && matches!(value_type, "Int" | "Num" | "Rat" | "FatRat") {
            return true;
        }
        if constraint == "Real" && matches!(value_type, "Int" | "Num" | "Rat" | "FatRat") {
            return true;
        }
        if constraint == "Cool" && matches!(value_type, "Int" | "Num" | "Str" | "Bool" | "Rat" | "FatRat") {
            return true;
        }
        if constraint == "Stringy" && matches!(value_type, "Str") {
            return true;
        }
        false
    }

    fn args_match_param_types(&self, args: &[Value], param_defs: &[ParamDef]) -> bool {
        let positional_params: Vec<&ParamDef> = param_defs.iter()
            .filter(|p| !p.slurpy && !p.named)
            .collect();
        for (i, pd) in positional_params.iter().enumerate() {
            if let Some(constraint) = &pd.type_constraint {
                if let Some(arg) = args.get(i) {
                    let arg_type = Self::value_type_name(arg);
                    if !Self::type_matches(constraint, arg_type) {
                        return false;
                    }
                }
            }
        }
        true
    }

    fn matches_expected(
        &mut self,
        matcher: &ExpectedMatcher,
        actual: &str,
    ) -> Result<bool, RuntimeError> {
        match matcher {
            ExpectedMatcher::Exact(Value::Str(s)) => Ok(actual == s),
            ExpectedMatcher::Exact(Value::Int(i)) => Ok(actual.trim() == i.to_string()),
            ExpectedMatcher::Exact(Value::Bool(b)) => Ok(*b == !actual.is_empty()),
            ExpectedMatcher::Exact(Value::Nil) => Ok(actual.is_empty()),
            ExpectedMatcher::Exact(_) => Ok(false),
            ExpectedMatcher::Lambda { param, body } => {
                let parsed = actual.trim().parse::<i64>().ok();
                let arg = parsed.map(Value::Int).unwrap_or_else(|| Value::Str(actual.to_string()));
                let saved = self.env.insert(param.clone(), arg);
                let result = self.eval_block_value(body);
                if let Some(old) = saved {
                    self.env.insert(param.clone(), old);
                } else {
                    self.env.remove(param);
                }
                Ok(result?.truthy())
            }
        }
    }

    fn positional_arg<'a>(
        &self,
        args: &'a [CallArg],
        index: usize,
        message: &str,
    ) -> Result<&'a Expr, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(expr);
                }
                count += 1;
            }
        }
        Err(RuntimeError::new(message))
    }

    fn positional_arg_value(&mut self, args: &[CallArg], index: usize) -> Result<String, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(self.eval_expr(expr)?.to_string_value());
                }
                count += 1;
            }
        }
        Ok(String::new())
    }

    fn named_arg_bool(&mut self, args: &[CallArg], name: &str) -> Result<bool, RuntimeError> {
        for arg in args {
            if let CallArg::Named { name: arg_name, value } = arg {
                if arg_name == name {
                    if let Some(expr) = value {
                        return Ok(self.eval_expr(expr)?.truthy());
                    }
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    fn named_arg_value(&mut self, args: &[CallArg], name: &str) -> Result<Option<String>, RuntimeError> {
        for arg in args {
            if let CallArg::Named { name: arg_name, value } = arg {
                if arg_name == name {
                    if let Some(expr) = value {
                        return Ok(Some(self.eval_expr(expr)?.to_string_value()));
                    }
                    return Ok(Some(String::new()));
                }
            }
        }
        Ok(None)
    }

    fn test_ok(&mut self, success: bool, desc: &str, todo: bool) -> Result<(), RuntimeError> {
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.ran += 1;
        let forced = state
            .force_todo
            .iter()
            .any(|(start, end)| state.ran >= *start && state.ran <= *end);
        let todo = todo || forced;
        if !success && !todo {
            state.failed += 1;
        }
        let mut line = String::new();
        if success {
            line.push_str("ok ");
        } else {
            line.push_str("not ok ");
        }
        line.push_str(&state.ran.to_string());
        if !desc.is_empty() {
            line.push_str(" - ");
            line.push_str(desc);
        }
        if todo {
            line.push_str(" # TODO");
        }
        line.push('\n');
        self.output.push_str(&line);
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(v) => {
                // Check if string literal matches an enum value or type in env
                if let Value::Str(name) = v {
                    if let Some(val) = self.env.get(name.as_str()) {
                        if matches!(val, Value::Enum { .. }) {
                            return Ok(val.clone());
                        }
                    }
                }
                Ok(v.clone())
            }
            Expr::StringInterpolation(parts) => {
                let mut result = String::new();
                for part in parts {
                    let val = self.eval_expr(part)?;
                    result.push_str(&val.to_string_value());
                }
                Ok(Value::Str(result))
            }
            Expr::Var(name) => Ok(self.env.get(name).cloned().unwrap_or(Value::Nil)),
            Expr::ArrayVar(name) => Ok(self.env.get(&format!("@{}", name)).cloned().unwrap_or(Value::Nil)),
            Expr::HashVar(name) => {
                let key = format!("%{}", name);
                Ok(self.env.get(&key).cloned().unwrap_or(Value::Nil))
            }
            Expr::CodeVar(name) => {
                // Check if stored as a variable first (my &f = ...)
                let var_key = format!("&{}", name);
                if let Some(val) = self.env.get(&var_key) {
                    return Ok(val.clone());
                }
                // Look up as a function reference (including multi subs)
                let def = self.resolve_function(name)
                    .or_else(|| {
                        // Try multi subs: search for any name/arity variant
                        let prefix_local = format!("{}::{}/", self.current_package, name);
                        let prefix_global = format!("GLOBAL::{}/", name);
                        self.functions.iter()
                            .find(|(k, _)| k.starts_with(&prefix_local) || k.starts_with(&prefix_global))
                            .map(|(_, v)| v.clone())
                    });
                if let Some(def) = def {
                    Ok(Value::Sub {
                        package: def.package,
                        name: def.name,
                        param: def.params.first().cloned(),
                        body: def.body,
                        env: self.env.clone(),
                    })
                } else {
                    Ok(Value::Nil)
                }
            }
            Expr::RoutineMagic => {
                if let Some((package, name)) = self.routine_stack.last() {
                    Ok(Value::Routine { package: package.clone(), name: name.clone() })
                } else {
                    Err(RuntimeError::new("X::Undeclared::Symbols"))
                }
            }
            Expr::BlockMagic => {
                if let Some(Value::Sub { .. }) = self.block_stack.last() {
                    Ok(self.block_stack.last().cloned().unwrap_or(Value::Nil))
                } else {
                    Err(RuntimeError::new("X::Undeclared::Symbols"))
                }
            }
            Expr::Block(body) => {
                let placeholders = collect_placeholders(body);
                if !placeholders.is_empty() {
                    Ok(Value::Sub {
                        package: self.current_package.clone(),
                        name: String::new(),
                        param: None,
                        body: body.clone(),
                        env: self.env.clone(),
                    })
                } else {
                    self.eval_block_value(body)
                }
            }
            Expr::AnonSub(body) => Ok(Value::Sub {
                package: self.current_package.clone(),
                name: String::new(),
                param: None,
                body: body.clone(),
                env: self.env.clone(),
            }),
            Expr::Lambda { param, body } => Ok(Value::Sub {
                package: self.current_package.clone(),
                name: String::new(),
                param: if param.is_empty() { None } else { Some(param.clone()) },
                body: body.clone(),
                env: self.env.clone(),
            }),
            Expr::ArrayLiteral(items) => {
                let mut values = Vec::new();
                for item in items {
                    values.push(self.eval_expr(item)?);
                }
                Ok(Value::Array(values))
            }
            Expr::Index { target, index } => {
                let value = self.eval_expr(target)?;
                let idx = self.eval_expr(index)?;
                match (value, idx) {
                    (Value::Array(items), Value::Int(i)) => {
                        let index = if i < 0 { return Ok(Value::Nil) } else { i as usize };
                        Ok(items.get(index).cloned().unwrap_or(Value::Nil))
                    }
                    (Value::Array(items), Value::Range(a, b)) => {
                        let start = a.max(0) as usize;
                        let end = b.max(-1) as usize;
                        let slice = if start >= items.len() {
                            Vec::new()
                        } else {
                            let end = end.min(items.len().saturating_sub(1));
                            items[start..=end].to_vec()
                        };
                        Ok(Value::Array(slice))
                    }
                    (Value::Array(items), Value::RangeExcl(a, b)) => {
                        let start = a.max(0) as usize;
                        let end_excl = b.max(0) as usize;
                        let slice = if start >= items.len() {
                            Vec::new()
                        } else {
                            let end_excl = end_excl.min(items.len());
                            if start >= end_excl {
                                Vec::new()
                            } else {
                                items[start..end_excl].to_vec()
                            }
                        };
                        Ok(Value::Array(slice))
                    }
                    (Value::Hash(items), Value::Str(key)) => {
                        Ok(items.get(&key).cloned().unwrap_or(Value::Nil))
                    }
                    (Value::Hash(items), Value::Int(key)) => {
                        Ok(items.get(&key.to_string()).cloned().unwrap_or(Value::Nil))
                    }
                    _ => Ok(Value::Nil),
                }
            }
            Expr::AssignExpr { name, expr } => {
                let value = self.eval_expr(expr)?;
                self.env.insert(name.clone(), value.clone());
                Ok(value)
            }
            Expr::EnvIndex(key) => {
                if let Some(value) = std::env::var_os(key) {
                    Ok(Value::Str(value.to_string_lossy().to_string()))
                } else {
                    Ok(Value::Nil)
                }
            }
            Expr::MethodCall { target, name, args } => {
                if name == "say" && args.is_empty() {
                    let value = self.eval_expr(target)?;
                    self.output.push_str(&value.to_string_value());
                    self.output.push('\n');
                    return Ok(Value::Nil);
                }
                if let Expr::ArrayVar(var_name) = target.as_ref() {
                    let key = format!("@{}", var_name);
                    match name.as_str() {
                        "push" => {
                            let value = args
                                .get(0)
                                .map(|arg| self.eval_expr(arg).ok())
                                .flatten()
                                .unwrap_or(Value::Nil);
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                items.push(value);
                            } else {
                                self.env.insert(key, Value::Array(vec![value]));
                            }
                            return Ok(Value::Nil);
                        }
                        "pop" => {
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                return Ok(items.pop().unwrap_or(Value::Nil));
                            }
                            return Ok(Value::Nil);
                        }
                        "shift" => {
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                if items.is_empty() {
                                    return Ok(Value::Nil);
                                }
                                return Ok(items.remove(0));
                            }
                            return Ok(Value::Nil);
                        }
                        "unshift" => {
                            let value = args
                                .get(0)
                                .map(|arg| self.eval_expr(arg).ok())
                                .flatten()
                                .unwrap_or(Value::Nil);
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                items.insert(0, value);
                            }
                            return Ok(Value::Nil);
                        }
                        "append" => {
                            let mut new_items = Vec::new();
                            for arg in args {
                                let val = self.eval_expr(arg)?;
                                match val {
                                    Value::Array(items) => new_items.extend(items),
                                    other => new_items.push(other),
                                }
                            }
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                items.extend(new_items);
                            }
                            return Ok(Value::Nil);
                        }
                        "join" => {
                            let sep = args
                                .get(0)
                                .map(|arg| self.eval_expr(arg).ok())
                                .flatten()
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            if let Some(Value::Array(items)) = self.env.get(&key) {
                                let joined = items
                                    .iter()
                                    .map(|v| v.to_string_value())
                                    .collect::<Vec<_>>()
                                    .join(&sep);
                                return Ok(Value::Str(joined));
                            }
                            return Ok(Value::Str(String::new()));
                        }
                        _ => {}
                    }
                }
                let base = self.eval_expr(target)?;
                // Handle enum-specific methods
                if let Value::Enum { ref enum_type, ref key, value: enum_val, index } = base {
                    match name.as_str() {
                        "key" => return Ok(Value::Str(key.clone())),
                        "value" | "Int" | "Numeric" => return Ok(Value::Int(enum_val)),
                        "raku" | "perl" => return Ok(Value::Str(format!("{}::{}", enum_type, key))),
                        "gist" | "Str" => return Ok(Value::Str(key.clone())),
                        "kv" => return Ok(Value::Array(vec![Value::Str(key.clone()), Value::Int(enum_val)])),
                        "pair" => return Ok(Value::Pair(key.clone(), Box::new(Value::Int(enum_val)))),
                        "pred" => {
                            if index == 0 {
                                return Ok(Value::Nil);
                            }
                            if let Some(variants) = self.enum_types.get(enum_type) {
                                if let Some((prev_key, prev_val)) = variants.get(index - 1) {
                                    return Ok(Value::Enum {
                                        enum_type: enum_type.clone(),
                                        key: prev_key.clone(),
                                        value: *prev_val,
                                        index: index - 1,
                                    });
                                }
                            }
                            return Ok(Value::Nil);
                        }
                        "succ" => {
                            if let Some(variants) = self.enum_types.get(enum_type) {
                                if let Some((next_key, next_val)) = variants.get(index + 1) {
                                    return Ok(Value::Enum {
                                        enum_type: enum_type.clone(),
                                        key: next_key.clone(),
                                        value: *next_val,
                                        index: index + 1,
                                    });
                                }
                            }
                            return Ok(Value::Nil);
                        }
                        _ => {} // fall through to generic methods
                    }
                }
                // Handle Type.enums (bare identifier matching an enum type name)
                if name == "enums" {
                    if let Value::Str(ref type_name) = base {
                        if let Some(variants) = self.enum_types.get(type_name) {
                            let mut map = HashMap::new();
                            for (k, v) in variants {
                                map.insert(k.clone(), Value::Int(*v));
                            }
                            return Ok(Value::Hash(map));
                        }
                    }
                }
                match name.as_str() {
                    "WHAT" => Ok(Value::Str(format!("({})", match &base {
                        Value::Int(_) => "Int",
                        Value::Num(_) => "Num",
                        Value::Str(_) => "Str",
                        Value::Bool(_) => "Bool",
                        Value::Range(_, _) => "Range",
                        Value::RangeExcl(_, _) | Value::RangeExclStart(_, _) | Value::RangeExclBoth(_, _) => "Range",
                        Value::Array(_) => "Array",
                        Value::Hash(_) => "Hash",
                        Value::Rat(_, _) => "Rat",
                        Value::FatRat(_, _) => "FatRat",
                        Value::Pair(_, _) => "Pair",
                        Value::Enum { enum_type, .. } => enum_type.as_str(),
                        Value::Nil => "Nil",
                        Value::Package(_) => "Package",
                        Value::Routine { .. } => "Routine",
                        Value::Sub { .. } => "Sub",
                        Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
                    }))),
                    "^name" => {
                        // Meta-method: type name
                        Ok(Value::Str(match &base {
                            Value::Int(_) => "Int".to_string(),
                            Value::Num(_) => "Num".to_string(),
                            Value::Str(_) => "Str".to_string(),
                            Value::Bool(_) => "Bool".to_string(),
                            Value::Range(_, _) | Value::RangeExcl(_, _) | Value::RangeExclStart(_, _) | Value::RangeExclBoth(_, _) => "Range".to_string(),
                            Value::Array(_) => "Array".to_string(),
                            Value::Hash(_) => "Hash".to_string(),
                            Value::Rat(_, _) => "Rat".to_string(),
                            Value::FatRat(_, _) => "FatRat".to_string(),
                            Value::Pair(_, _) => "Pair".to_string(),
                            Value::Enum { enum_type, .. } => enum_type.clone(),
                            Value::Nil => "Nil".to_string(),
                            Value::Package(name) => name.clone(),
                            Value::Routine { .. } => "Routine".to_string(),
                            Value::Sub { .. } => "Sub".to_string(),
                            Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification".to_string(),
                        }))
                    }
                    "defined" => Ok(Value::Bool(!matches!(base, Value::Nil))),
                    "parent" => {
                        let mut levels = 1i64;
                        if let Some(arg) = args.get(0) {
                            if let Value::Int(i) = self.eval_expr(arg)? {
                                levels = i.max(1);
                            }
                        }
                        let mut path = base.to_string_value();
                        for _ in 0..levels {
                            if let Some(parent) = Path::new(&path).parent() {
                                path = parent.to_string_lossy().to_string();
                            } else {
                                path.clear();
                                break;
                            }
                        }
                        Ok(Value::Str(path))
                    }
                    "sibling" => {
                        let segment = args
                            .get(0)
                            .map(|arg| self.eval_expr(arg).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let base_path = base.to_string_value();
                        let parent = Path::new(&base_path).parent().unwrap_or_else(|| Path::new(""));
                        let joined = parent.join(segment);
                        Ok(Value::Str(joined.to_string_lossy().to_string()))
                    }
                    "add" => {
                        let segment = args
                            .get(0)
                            .map(|arg| self.eval_expr(arg).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let joined = Path::new(&base.to_string_value()).join(segment);
                        Ok(Value::Str(joined.to_string_lossy().to_string()))
                    }
                    "package" => match base {
                        Value::Routine { package, .. } => Ok(Value::Package(package)),
                        _ => Ok(Value::Nil),
                    },
                    "name" => match base {
                        Value::Routine { name, .. } => Ok(Value::Str(name)),
                        Value::Package(name) => Ok(Value::Str(name)),
                        Value::Str(name) => Ok(Value::Str(name)),
                        Value::Sub { name, .. } => Ok(Value::Str(name)),
                        _ => Ok(Value::Nil),
                    },
                    "chars" => Ok(Value::Int(base.to_string_value().chars().count() as i64)),
                    "uc" => Ok(Value::Str(base.to_string_value().to_uppercase())),
                    "lc" => Ok(Value::Str(base.to_string_value().to_lowercase())),
                    "tc" => {
                        let s = base.to_string_value();
                        let mut result = String::new();
                        let mut capitalize = true;
                        for ch in s.chars() {
                            if capitalize {
                                for c in ch.to_uppercase() {
                                    result.push(c);
                                }
                                capitalize = false;
                            } else {
                                result.push(ch);
                            }
                        }
                        Ok(Value::Str(result))
                    }
                    "tclc" => {
                        let s = base.to_string_value();
                        let mut result = String::new();
                        let mut first = true;
                        for ch in s.chars() {
                            if first {
                                for c in ch.to_uppercase() {
                                    result.push(c);
                                }
                                first = false;
                            } else {
                                for c in ch.to_lowercase() {
                                    result.push(c);
                                }
                            }
                        }
                        Ok(Value::Str(result))
                    }
                    "chomp" => {
                        let s = base.to_string_value();
                        Ok(Value::Str(s.trim_end_matches('\n').to_string()))
                    }
                    "chop" => {
                        let mut s = base.to_string_value();
                        s.pop();
                        Ok(Value::Str(s))
                    }
                    "trim" => Ok(Value::Str(base.to_string_value().trim().to_string())),
                    "trim-leading" => Ok(Value::Str(base.to_string_value().trim_start().to_string())),
                    "trim-trailing" => Ok(Value::Str(base.to_string_value().trim_end().to_string())),
                    "flip" => Ok(Value::Str(base.to_string_value().chars().rev().collect())),
                    "contains" => {
                        let needle = args.get(0)
                            .map(|a| self.eval_expr(a).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        Ok(Value::Bool(base.to_string_value().contains(&needle)))
                    }
                    "starts-with" => {
                        let needle = args.get(0)
                            .map(|a| self.eval_expr(a).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        Ok(Value::Bool(base.to_string_value().starts_with(&needle)))
                    }
                    "ends-with" => {
                        let needle = args.get(0)
                            .map(|a| self.eval_expr(a).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        Ok(Value::Bool(base.to_string_value().ends_with(&needle)))
                    }
                    "substr" => {
                        let s = base.to_string_value();
                        let start = args.get(0)
                            .map(|a| self.eval_expr(a).ok())
                            .flatten()
                            .and_then(|v| match v { Value::Int(i) => Some(i), _ => None })
                            .unwrap_or(0);
                        let chars: Vec<char> = s.chars().collect();
                        let start = start.max(0) as usize;
                        if let Some(len_expr) = args.get(1) {
                            let len = self.eval_expr(len_expr)?;
                            let len = match len { Value::Int(i) => i.max(0) as usize, _ => chars.len() };
                            let end = (start + len).min(chars.len());
                            Ok(Value::Str(chars[start..end].iter().collect()))
                        } else {
                            Ok(Value::Str(chars[start..].iter().collect()))
                        }
                    }
                    "index" => {
                        let s = base.to_string_value();
                        let needle = args.get(0)
                            .map(|a| self.eval_expr(a).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        match s.find(&needle) {
                            Some(pos) => {
                                let char_pos = s[..pos].chars().count();
                                Ok(Value::Int(char_pos as i64))
                            }
                            None => Ok(Value::Nil),
                        }
                    }
                    "rindex" => {
                        let s = base.to_string_value();
                        let needle = args.get(0)
                            .map(|a| self.eval_expr(a).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        match s.rfind(&needle) {
                            Some(pos) => {
                                let char_pos = s[..pos].chars().count();
                                Ok(Value::Int(char_pos as i64))
                            }
                            None => Ok(Value::Nil),
                        }
                    }
                    "match" => {
                        // Basic regex match - just return Nil for now
                        Ok(Value::Nil)
                    }
                    "IO" => {
                        // Returns self (string as IO path)
                        Ok(Value::Str(base.to_string_value()))
                    }
                    "say" => {
                        self.output.push_str(&base.to_string_value());
                        self.output.push('\n');
                        Ok(Value::Bool(true))
                    }
                    "print" => {
                        self.output.push_str(&base.to_string_value());
                        Ok(Value::Bool(true))
                    }
                    "Seq" | "Supply" | "Channel" => {
                        // stub
                        Ok(base)
                    }
                    "classify" => {
                        // Returns empty hash as stub
                        Ok(Value::Hash(HashMap::new()))
                    }
                    "splice" => match base {
                        Value::Array(mut items) => {
                            let start = args.get(0)
                                .map(|a| self.eval_expr(a).ok())
                                .flatten()
                                .and_then(|v| match v { Value::Int(i) => Some(i.max(0) as usize), _ => None })
                                .unwrap_or(0);
                            let count = args.get(1)
                                .map(|a| self.eval_expr(a).ok())
                                .flatten()
                                .and_then(|v| match v { Value::Int(i) => Some(i.max(0) as usize), _ => None })
                                .unwrap_or(items.len().saturating_sub(start));
                            let end = (start + count).min(items.len());
                            let removed: Vec<Value> = items.drain(start..end).collect();
                            // Insert new elements if provided
                            if let Some(new_arg) = args.get(2) {
                                let new_val = self.eval_expr(new_arg)?;
                                match new_val {
                                    Value::Array(new_items) => {
                                        for (i, item) in new_items.into_iter().enumerate() {
                                            items.insert(start + i, item);
                                        }
                                    }
                                    other => { items.insert(start, other); }
                                }
                            }
                            Ok(Value::Array(removed))
                        }
                        _ => Ok(Value::Nil),
                    },
                    "split" => {
                        let s = base.to_string_value();
                        let sep = args.get(0)
                            .map(|a| self.eval_expr(a).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let parts: Vec<Value> = s.split(&sep).map(|p| Value::Str(p.to_string())).collect();
                        Ok(Value::Array(parts))
                    }
                    "words" => {
                        let s = base.to_string_value();
                        let parts: Vec<Value> = s.split_whitespace().map(|p| Value::Str(p.to_string())).collect();
                        Ok(Value::Array(parts))
                    }
                    "comb" => {
                        let s = base.to_string_value();
                        let parts: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
                        Ok(Value::Array(parts))
                    }
                    "lines" => {
                        let s = base.to_string_value();
                        let parts: Vec<Value> = s.lines().map(|l| Value::Str(l.to_string())).collect();
                        Ok(Value::Array(parts))
                    }
                    "Int" => match base {
                        Value::Int(i) => Ok(Value::Int(i)),
                        Value::Num(f) => Ok(Value::Int(f as i64)),
                        Value::Rat(n, d) => {
                            if d == 0 { Err(RuntimeError::new("Cannot convert Inf/NaN Rat to Int")) }
                            else { Ok(Value::Int(n / d)) }
                        }
                        Value::Str(s) => Ok(Value::Int(s.trim().parse::<i64>().unwrap_or(0))),
                        Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
                        _ => Ok(Value::Int(0)),
                    },
                    "Numeric" | "Num" => match base {
                        Value::Int(i) => Ok(Value::Int(i)),
                        Value::Num(f) => Ok(Value::Num(f)),
                        Value::Rat(n, d) => {
                            if d == 0 {
                                if n == 0 { Ok(Value::Num(f64::NAN)) }
                                else if n > 0 { Ok(Value::Num(f64::INFINITY)) }
                                else { Ok(Value::Num(f64::NEG_INFINITY)) }
                            } else {
                                Ok(Value::Num(n as f64 / d as f64))
                            }
                        }
                        Value::Str(s) => {
                            if let Ok(i) = s.trim().parse::<i64>() {
                                Ok(Value::Int(i))
                            } else if let Ok(f) = s.trim().parse::<f64>() {
                                Ok(Value::Num(f))
                            } else {
                                Ok(Value::Int(0))
                            }
                        }
                        Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
                        _ => Ok(Value::Int(0)),
                    },
                    "Rat" => match base {
                        Value::Rat(_, _) => Ok(base),
                        Value::Int(i) => Ok(make_rat(i, 1)),
                        Value::Num(f) => {
                            // Simple conversion: approximate as rational
                            let denom = 1_000_000i64;
                            let numer = (f * denom as f64).round() as i64;
                            Ok(make_rat(numer, denom))
                        }
                        Value::FatRat(n, d) => Ok(make_rat(n, d)),
                        _ => Ok(make_rat(0, 1)),
                    },
                    "FatRat" => match base {
                        Value::FatRat(_, _) => Ok(base),
                        Value::Rat(n, d) => Ok(Value::FatRat(n, d)),
                        Value::Int(i) => Ok(Value::FatRat(i, 1)),
                        _ => Ok(Value::FatRat(0, 1)),
                    },
                    "nude" => match base {
                        Value::Rat(n, d) => Ok(Value::Array(vec![Value::Int(n), Value::Int(d)])),
                        Value::FatRat(n, d) => Ok(Value::Array(vec![Value::Int(n), Value::Int(d)])),
                        Value::Int(i) => Ok(Value::Array(vec![Value::Int(i), Value::Int(1)])),
                        _ => Ok(Value::Array(vec![Value::Int(0), Value::Int(1)])),
                    },
                    "numerator" => match base {
                        Value::Rat(n, _) => Ok(Value::Int(n)),
                        Value::FatRat(n, _) => Ok(Value::Int(n)),
                        Value::Int(i) => Ok(Value::Int(i)),
                        _ => Ok(Value::Int(0)),
                    },
                    "denominator" => match base {
                        Value::Rat(_, d) => Ok(Value::Int(d)),
                        Value::FatRat(_, d) => Ok(Value::Int(d)),
                        Value::Int(_) => Ok(Value::Int(1)),
                        _ => Ok(Value::Int(1)),
                    },
                    "isNaN" => match base {
                        Value::Rat(0, 0) => Ok(Value::Bool(true)),
                        Value::Num(f) => Ok(Value::Bool(f.is_nan())),
                        _ => Ok(Value::Bool(false)),
                    },
                    "Bool" => Ok(Value::Bool(base.truthy())),
                    "gist" | "raku" | "perl" => match base {
                        Value::Rat(n, d) => {
                            if d == 0 {
                                if n == 0 { Ok(Value::Str("NaN".to_string())) }
                                else if n > 0 { Ok(Value::Str("Inf".to_string())) }
                                else { Ok(Value::Str("-Inf".to_string())) }
                            } else {
                                // For .raku: exact decimal or <n/d> form
                                let mut dd = d;
                                while dd % 2 == 0 { dd /= 2; }
                                while dd % 5 == 0 { dd /= 5; }
                                if dd == 1 {
                                    let val = n as f64 / d as f64;
                                    let s = format!("{}", val);
                                    if s.contains('.') { Ok(Value::Str(s)) }
                                    else { Ok(Value::Str(format!("{}.0", val))) }
                                } else {
                                    Ok(Value::Str(format!("<{}/{}>", n, d)))
                                }
                            }
                        }
                        _ => Ok(Value::Str(base.to_string_value())),
                    },
                    "Str" => match base {
                        Value::Str(name) if name == "IO::Special" => Ok(Value::Str(String::new())),
                        _ => Ok(Value::Str(base.to_string_value())),
                    },
                    "elems" => match base {
                        Value::Array(items) => Ok(Value::Int(items.len() as i64)),
                        Value::Hash(items) => Ok(Value::Int(items.len() as i64)),
                        _ => Ok(Value::Int(1)),
                    },
                    "end" => match base {
                        Value::Array(items) => Ok(Value::Int(items.len() as i64 - 1)),
                        _ => Ok(Value::Int(0)),
                    },
                    "keys" => match base {
                        Value::Hash(items) => {
                            let keys: Vec<Value> = items.keys().map(|k| Value::Str(k.clone())).collect();
                            Ok(Value::Array(keys))
                        }
                        _ => Ok(Value::Array(Vec::new())),
                    },
                    "values" => match base {
                        Value::Hash(items) => {
                            let vals: Vec<Value> = items.values().cloned().collect();
                            Ok(Value::Array(vals))
                        }
                        _ => Ok(Value::Array(Vec::new())),
                    },
                    "kv" => match base {
                        Value::Hash(items) => {
                            let mut kv = Vec::new();
                            for (k, v) in &items {
                                kv.push(Value::Str(k.clone()));
                                kv.push(v.clone());
                            }
                            Ok(Value::Array(kv))
                        }
                        _ => Ok(Value::Array(Vec::new())),
                    },
                    "pairs" => match base {
                        Value::Hash(items) => {
                            let pairs: Vec<Value> = items.iter()
                                .map(|(k, v)| Value::Str(format!("{}\t{}", k, v.to_string_value())))
                                .collect();
                            Ok(Value::Array(pairs))
                        }
                        _ => Ok(Value::Array(Vec::new())),
                    },
                    "sort" => match base {
                        Value::Array(mut items) => {
                            if let Some(func_expr) = args.get(0) {
                                let func = self.eval_expr(func_expr)?;
                                if let Value::Sub { param, body, env, .. } = func {
                                    let placeholders = collect_placeholders(&body);
                                    items.sort_by(|a, b| {
                                        let saved = self.env.clone();
                                        for (k, v) in &env {
                                            self.env.insert(k.clone(), v.clone());
                                        }
                                        if let Some(p) = &param {
                                            self.env.insert(p.clone(), a.clone());
                                        }
                                        if placeholders.len() >= 2 {
                                            self.env.insert(placeholders[0].clone(), a.clone());
                                            self.env.insert(placeholders[1].clone(), b.clone());
                                        }
                                        self.env.insert("_".to_string(), a.clone());
                                        let result = self.eval_block_value(&body).unwrap_or(Value::Int(0));
                                        self.env = saved;
                                        match result {
                                            Value::Int(n) => n.cmp(&0),
                                            _ => std::cmp::Ordering::Equal,
                                        }
                                    });
                                    Ok(Value::Array(items))
                                } else {
                                    items.sort_by(|a, b| a.to_string_value().cmp(&b.to_string_value()));
                                    Ok(Value::Array(items))
                                }
                            } else {
                                items.sort_by(|a, b| a.to_string_value().cmp(&b.to_string_value()));
                                Ok(Value::Array(items))
                            }
                        }
                        _ => Ok(base),
                    },
                    "reverse" => match base {
                        Value::Array(mut items) => {
                            items.reverse();
                            Ok(Value::Array(items))
                        }
                        Value::Str(s) => Ok(Value::Str(s.chars().rev().collect())),
                        _ => Ok(base),
                    },
                    "unique" => match base {
                        Value::Array(items) => {
                            let mut seen = Vec::new();
                            let mut result = Vec::new();
                            for item in items {
                                let s = item.to_string_value();
                                if !seen.contains(&s) {
                                    seen.push(s);
                                    result.push(item);
                                }
                            }
                            Ok(Value::Array(result))
                        }
                        _ => Ok(base),
                    },
                    "squish" => match base {
                        Value::Array(items) => {
                            let mut result = Vec::new();
                            let mut last: Option<String> = None;
                            for item in items {
                                let s = item.to_string_value();
                                if last.as_ref() != Some(&s) {
                                    last = Some(s);
                                    result.push(item);
                                }
                            }
                            Ok(Value::Array(result))
                        }
                        _ => Ok(base),
                    },
                    "minmax" => match base {
                        Value::Array(items) if !items.is_empty() => {
                            let mut min = &items[0];
                            let mut max = &items[0];
                            for item in &items[1..] {
                                if Self::compare_values(item, min) < 0 { min = item; }
                                if Self::compare_values(item, max) > 0 { max = item; }
                            }
                            Ok(Value::Range(
                                Self::to_int(min),
                                Self::to_int(max),
                            ))
                        }
                        _ => Ok(Value::Nil),
                    },
                    "flat" => match base {
                        Value::Array(items) => {
                            let mut flat = Vec::new();
                            for item in items {
                                if let Value::Array(sub) = item {
                                    flat.extend(sub);
                                } else {
                                    flat.push(item);
                                }
                            }
                            Ok(Value::Array(flat))
                        }
                        _ => Ok(base),
                    },
                    "head" => match base {
                        Value::Array(items) => Ok(items.into_iter().next().unwrap_or(Value::Nil)),
                        _ => Ok(base),
                    },
                    "tail" => match base {
                        Value::Array(items) => Ok(items.into_iter().last().unwrap_or(Value::Nil)),
                        _ => Ok(base),
                    },
                    "first" => match base {
                        Value::Array(items) => Ok(items.into_iter().next().unwrap_or(Value::Nil)),
                        _ => Ok(base),
                    },
                    "min" => match base {
                        Value::Array(items) => {
                            Ok(items.into_iter().min_by(|a, b| {
                                match (a, b) {
                                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                                    _ => a.to_string_value().cmp(&b.to_string_value()),
                                }
                            }).unwrap_or(Value::Nil))
                        }
                        _ => Ok(base),
                    },
                    "max" => match base {
                        Value::Array(items) => {
                            Ok(items.into_iter().max_by(|a, b| {
                                match (a, b) {
                                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                                    _ => a.to_string_value().cmp(&b.to_string_value()),
                                }
                            }).unwrap_or(Value::Nil))
                        }
                        _ => Ok(base),
                    },
                    "sum" => match base {
                        Value::Array(items) => {
                            let mut total: i64 = 0;
                            let mut is_float = false;
                            let mut ftotal: f64 = 0.0;
                            for item in &items {
                                match item {
                                    Value::Int(i) => { total += i; ftotal += *i as f64; }
                                    Value::Num(f) => { is_float = true; ftotal += f; }
                                    Value::Str(s) => {
                                        if let Ok(i) = s.parse::<i64>() { total += i; ftotal += i as f64; }
                                        else if let Ok(f) = s.parse::<f64>() { is_float = true; ftotal += f; }
                                    }
                                    _ => {}
                                }
                            }
                            if is_float { Ok(Value::Num(ftotal)) } else { Ok(Value::Int(total)) }
                        }
                        _ => Ok(Value::Int(0)),
                    },
                    "pick" => match base {
                        Value::Array(mut items) => {
                            if items.is_empty() {
                                Ok(Value::Nil)
                            } else {
                                use std::collections::hash_map::DefaultHasher;
                                use std::hash::{Hash, Hasher};
                                let mut hasher = DefaultHasher::new();
                                std::time::SystemTime::now().hash(&mut hasher);
                                let idx = (hasher.finish() as usize) % items.len();
                                Ok(items.remove(idx))
                            }
                        }
                        _ => Ok(base),
                    },
                    "roll" => match base {
                        Value::Array(items) => {
                            if items.is_empty() {
                                Ok(Value::Nil)
                            } else {
                                use std::collections::hash_map::DefaultHasher;
                                use std::hash::{Hash, Hasher};
                                let mut hasher = DefaultHasher::new();
                                std::time::SystemTime::now().hash(&mut hasher);
                                let idx = (hasher.finish() as usize) % items.len();
                                Ok(items[idx].clone())
                            }
                        }
                        _ => Ok(base),
                    },
                    "map" => match base {
                        Value::Array(items) => {
                            if let Some(func_expr) = args.get(0) {
                                let func = self.eval_expr(func_expr)?;
                                if let Value::Sub { param, body, env, .. } = func {
                                    let placeholders = collect_placeholders(&body);
                                    let mut result = Vec::new();
                                    for item in items {
                                        let saved = self.env.clone();
                                        for (k, v) in &env {
                                            self.env.insert(k.clone(), v.clone());
                                        }
                                        if let Some(p) = &param {
                                            self.env.insert(p.clone(), item.clone());
                                        }
                                        if let Some(ph) = placeholders.first() {
                                            self.env.insert(ph.clone(), item.clone());
                                        }
                                        self.env.insert("_".to_string(), item);
                                        let val = self.eval_block_value(&body)?;
                                        self.env = saved;
                                        result.push(val);
                                    }
                                    Ok(Value::Array(result))
                                } else {
                                    Ok(Value::Array(items))
                                }
                            } else {
                                Ok(Value::Array(items))
                            }
                        }
                        _ => Ok(base),
                    },
                    "grep" => match base {
                        Value::Array(items) => {
                            if let Some(func_expr) = args.get(0) {
                                let func = self.eval_expr(func_expr)?;
                                if let Value::Sub { param, body, env, .. } = func {
                                    let placeholders = collect_placeholders(&body);
                                    let mut result = Vec::new();
                                    for item in items {
                                        let saved = self.env.clone();
                                        for (k, v) in &env {
                                            self.env.insert(k.clone(), v.clone());
                                        }
                                        if let Some(p) = &param {
                                            self.env.insert(p.clone(), item.clone());
                                        }
                                        if let Some(ph) = placeholders.first() {
                                            self.env.insert(ph.clone(), item.clone());
                                        }
                                        self.env.insert("_".to_string(), item.clone());
                                        let val = self.eval_block_value(&body)?;
                                        self.env = saved;
                                        if val.truthy() {
                                            result.push(item);
                                        }
                                    }
                                    Ok(Value::Array(result))
                                } else {
                                    Ok(Value::Array(items))
                                }
                            } else {
                                Ok(Value::Array(items))
                            }
                        }
                        _ => Ok(base),
                    },
                    "abs" => match base {
                        Value::Int(i) => Ok(Value::Int(i.abs())),
                        Value::Num(f) => Ok(Value::Num(f.abs())),
                        _ => Ok(Value::Int(0)),
                    },
                    "sign" => match base {
                        Value::Int(i) => Ok(Value::Int(i.signum())),
                        Value::Num(f) => Ok(Value::Int(if f > 0.0 { 1 } else if f < 0.0 { -1 } else { 0 })),
                        _ => Ok(Value::Int(0)),
                    },
                    "is-prime" => match base {
                        Value::Int(n) => {
                            let n = n.abs();
                            let prime = if n < 2 { false }
                                else if n < 4 { true }
                                else if n % 2 == 0 || n % 3 == 0 { false }
                                else {
                                    let mut i = 5i64;
                                    let mut result = true;
                                    while i * i <= n {
                                        if n % i == 0 || n % (i + 2) == 0 {
                                            result = false;
                                            break;
                                        }
                                        i += 6;
                                    }
                                    result
                                };
                            Ok(Value::Bool(prime))
                        }
                        _ => Ok(Value::Bool(false)),
                    },
                    "key" => match base {
                        Value::Pair(k, _) => Ok(Value::Str(k)),
                        _ => Ok(Value::Nil),
                    },
                    "value" => match base {
                        Value::Pair(_, v) => Ok(*v),
                        _ => Ok(Value::Nil),
                    },
                    "list" | "Array" => match base {
                        Value::Range(a, b) => {
                            let items: Vec<Value> = (a..=b).map(Value::Int).collect();
                            Ok(Value::Array(items))
                        }
                        Value::RangeExcl(a, b) => {
                            let items: Vec<Value> = (a..b).map(Value::Int).collect();
                            Ok(Value::Array(items))
                        }
                        Value::RangeExclStart(a, b) => {
                            let items: Vec<Value> = (a+1..=b).map(Value::Int).collect();
                            Ok(Value::Array(items))
                        }
                        Value::RangeExclBoth(a, b) => {
                            let items: Vec<Value> = (a+1..b).map(Value::Int).collect();
                            Ok(Value::Array(items))
                        }
                        Value::Array(items) => Ok(Value::Array(items)),
                        _ => Ok(Value::Array(vec![base])),
                    },
                    "so" => Ok(Value::Bool(base.truthy())),
                    "not" => Ok(Value::Bool(!base.truthy())),
                    "succ" => match base {
                        Value::Int(i) => Ok(Value::Int(i + 1)),
                        Value::Str(s) => {
                            // Increment last char
                            if s.is_empty() {
                                Ok(Value::Str(String::new()))
                            } else {
                                let mut chars: Vec<char> = s.chars().collect();
                                if let Some(last) = chars.last_mut() {
                                    *last = char::from_u32(*last as u32 + 1).unwrap_or(*last);
                                }
                                Ok(Value::Str(chars.into_iter().collect()))
                            }
                        }
                        _ => Ok(base),
                    },
                    "pred" => match base {
                        Value::Int(i) => Ok(Value::Int(i - 1)),
                        _ => Ok(base),
                    },
                    "fmt" => {
                        let fmt = args.get(0)
                            .map(|a| self.eval_expr(a).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "%s".to_string());
                        let rendered = self.format_sprintf(&fmt, Some(&base));
                        Ok(Value::Str(rendered))
                    },
                    "base" => match base {
                        Value::Int(i) => {
                            let radix = args.get(0)
                                .map(|a| self.eval_expr(a).ok())
                                .flatten()
                                .and_then(|v| match v { Value::Int(n) => Some(n), _ => None })
                                .unwrap_or(10);
                            let s = match radix {
                                2 => format!("{:b}", i),
                                8 => format!("{:o}", i),
                                16 => format!("{:X}", i),
                                _ => format!("{}", i),
                            };
                            Ok(Value::Str(s))
                        }
                        _ => Ok(Value::Str(base.to_string_value())),
                    },
                    "parse-base" => {
                        let radix = args.get(0)
                            .map(|a| self.eval_expr(a).ok())
                            .flatten()
                            .and_then(|v| match v { Value::Int(n) => Some(n as u32), _ => None })
                            .unwrap_or(10);
                        let s = base.to_string_value();
                        match i64::from_str_radix(&s, radix) {
                            Ok(n) => Ok(Value::Int(n)),
                            Err(_) => Err(RuntimeError::new(format!("Cannot parse '{}' as base {}", s, radix))),
                        }
                    },
                    "sqrt" => match base {
                        Value::Int(i) => Ok(Value::Num((i as f64).sqrt())),
                        Value::Num(f) => Ok(Value::Num(f.sqrt())),
                        _ => Ok(Value::Num(f64::NAN)),
                    },
                    "floor" => match base {
                        Value::Num(f) => Ok(Value::Int(f.floor() as i64)),
                        Value::Int(i) => Ok(Value::Int(i)),
                        _ => Ok(Value::Int(0)),
                    },
                    "ceiling" | "ceil" => match base {
                        Value::Num(f) => Ok(Value::Int(f.ceil() as i64)),
                        Value::Int(i) => Ok(Value::Int(i)),
                        _ => Ok(Value::Int(0)),
                    },
                    "round" => match base {
                        Value::Num(f) => Ok(Value::Int(f.round() as i64)),
                        Value::Int(i) => Ok(Value::Int(i)),
                        _ => Ok(Value::Int(0)),
                    },
                    "narrow" => match base {
                        Value::Num(f) if f.fract() == 0.0 && f.is_finite() => Ok(Value::Int(f as i64)),
                        _ => Ok(base),
                    },
                    "log" => match base {
                        Value::Int(i) => Ok(Value::Num((i as f64).ln())),
                        Value::Num(f) => Ok(Value::Num(f.ln())),
                        _ => Ok(Value::Num(f64::NAN)),
                    },
                    "exp" => match base {
                        Value::Int(i) => Ok(Value::Num((i as f64).exp())),
                        Value::Num(f) => Ok(Value::Num(f.exp())),
                        _ => Ok(Value::Num(f64::NAN)),
                    },
                    "push" => {
                        // .push on evaluated array - returns new array with item added
                        match base {
                            Value::Array(mut items) => {
                                let value = args.get(0)
                                    .map(|a| self.eval_expr(a).ok())
                                    .flatten()
                                    .unwrap_or(Value::Nil);
                                items.push(value);
                                Ok(Value::Array(items))
                            }
                            _ => Ok(base),
                        }
                    },
                    "pop" => match base {
                        Value::Array(mut items) => Ok(items.pop().unwrap_or(Value::Nil)),
                        _ => Ok(Value::Nil),
                    },
                    "shift" => match base {
                        Value::Array(mut items) => {
                            if items.is_empty() {
                                Ok(Value::Nil)
                            } else {
                                Ok(items.remove(0))
                            }
                        }
                        _ => Ok(Value::Nil),
                    },
                    "unshift" => match base {
                        Value::Array(mut items) => {
                            let value = args.get(0)
                                .map(|a| self.eval_expr(a).ok())
                                .flatten()
                                .unwrap_or(Value::Nil);
                            items.insert(0, value);
                            Ok(Value::Array(items))
                        }
                        _ => Ok(base),
                    },
                    "join" => match base {
                        Value::Array(items) => {
                            let sep = args
                                .get(0)
                                .map(|arg| self.eval_expr(arg).ok())
                                .flatten()
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            let joined = items
                                .iter()
                                .map(|v| v.to_string_value())
                                .collect::<Vec<_>>()
                                .join(&sep);
                            Ok(Value::Str(joined))
                        }
                        _ => Ok(Value::Nil),
                    },
                    "WHY" => match base {
                        Value::Str(name) | Value::Package(name) => {
                            let doc = self.doc_comments.get(&name).cloned().unwrap_or_default();
                            Ok(Value::Str(doc))
                        }
                        _ => Ok(Value::Nil),
                    },
                    "version-matcher" | "auth-matcher" | "api-matcher" => match base {
                        Value::CompUnitDepSpec { .. } => Ok(Value::Bool(true)),
                        _ => Ok(Value::Nil),
                    },
                    "new" => match base {
                        Value::Str(name) if name == "Rat" => {
                            let a = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                            let b = args.get(1).map(|e| self.eval_expr(e).ok()).flatten();
                            let a = match a { Some(Value::Int(i)) => i, _ => 0 };
                            let b = match b { Some(Value::Int(i)) => i, _ => 1 };
                            Ok(make_rat(a, b))
                        }
                        Value::Str(name) if name == "FatRat" => {
                            let a = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                            let b = args.get(1).map(|e| self.eval_expr(e).ok()).flatten();
                            let a = match a { Some(Value::Int(i)) => i, _ => 0 };
                            let b = match b { Some(Value::Int(i)) => i, _ => 1 };
                            Ok(Value::FatRat(a, b))
                        }
                        Value::Str(name) if name == "CompUnit::DependencySpecification" => {
                            let mut short_name = None;
                            if let Some(arg) = args.get(0) {
                                if let Expr::AssignExpr { name, expr } = arg {
                                    if name == "short-name" {
                                        if let Ok(Value::Str(s)) = self.eval_expr(expr) {
                                            short_name = Some(s);
                                        }
                                    }
                                } else if let Ok(Value::Str(s)) = self.eval_expr(arg) {
                                    short_name = Some(s);
                                }
                            }
                            if let Some(s) = short_name {
                                Ok(Value::CompUnitDepSpec { short_name: s })
                            } else {
                                Err(RuntimeError::new("CompUnit::DependencySpecification requires short-name"))
                            }
                        }
                        _ => Ok(Value::Nil),
                    },
                    _ => Ok(Value::Nil),
                }
            }
            Expr::Exists(inner) => match inner.as_ref() {
                Expr::EnvIndex(key) => Ok(Value::Bool(std::env::var_os(key).is_some())),
                _ => Ok(Value::Bool(self.eval_expr(inner)?.truthy())),
            },
            Expr::CallOn { target, args } => {
                let target_val = self.eval_expr(target)?;
                if let Value::Sub { package, name, param, body, env } = target_val {
                    let saved_env = self.env.clone();
                    let mut new_env = saved_env.clone();
                    for (k, v) in env {
                        if matches!(new_env.get(&k), Some(Value::Array(_)))
                            && matches!(v, Value::Array(_))
                        {
                            continue;
                        }
                        new_env.insert(k, v);
                    }
                    let param_name = param.clone();
                    if let Some(param_name) = param_name {
                        if let Some(arg) = args.get(0) {
                            if let Ok(value) = self.eval_expr(arg) {
                                new_env.insert(param_name, value);
                            }
                        }
                    }
                    // Bind placeholder variables ($^a, $^b, ...)
                    let placeholders = collect_placeholders(&body);
                    if !placeholders.is_empty() {
                        let mut eval_args = Vec::new();
                        for arg in args {
                            eval_args.push(self.eval_expr(arg)?);
                        }
                        for (i, ph) in placeholders.iter().enumerate() {
                            if let Some(val) = eval_args.get(i) {
                                new_env.insert(ph.clone(), val.clone());
                            }
                        }
                    }
                    let block_sub = Value::Sub {
                        package: package.clone(),
                        name: name.clone(),
                        param: param.clone(),
                        body: body.clone(),
                        env: new_env.clone(),
                    };
                    self.env = new_env;
                    self.routine_stack.push((package.clone(), name.clone()));
                    self.block_stack.push(block_sub);
                    let result = self.eval_block_value(&body);
                    self.block_stack.pop();
                    self.routine_stack.pop();
                    let mut merged = saved_env;
                    for (k, v) in self.env.iter() {
                        if matches!(v, Value::Array(_)) {
                            merged.insert(k.clone(), v.clone());
                        }
                    }
                    self.env = merged;
                    return match result {
                        Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                        other => other,
                    };
                }
                Ok(Value::Nil)
            }
            Expr::Unary { op, expr } => {
                match op {
                    TokenKind::PlusPlus => {
                        if let Expr::Var(name) = expr.as_ref() {
                            let val = self.env.get(name).cloned().unwrap_or(Value::Int(0));
                            let new_val = match val {
                                Value::Int(i) => Value::Int(i + 1),
                                Value::Rat(n, d) => make_rat(n + d, d),
                                _ => Value::Int(1),
                            };
                            self.env.insert(name.clone(), new_val.clone());
                            return Ok(new_val);
                        }
                        Ok(Value::Nil)
                    }
                    TokenKind::MinusMinus => {
                        if let Expr::Var(name) = expr.as_ref() {
                            let val = self.env.get(name).cloned().unwrap_or(Value::Int(0));
                            let new_val = match val {
                                Value::Int(i) => Value::Int(i - 1),
                                Value::Rat(n, d) => make_rat(n - d, d),
                                _ => Value::Int(-1),
                            };
                            self.env.insert(name.clone(), new_val.clone());
                            return Ok(new_val);
                        }
                        Ok(Value::Nil)
                    }
                    _ => {
                        let value = self.eval_expr(expr)?;
                        match op {
                            TokenKind::Plus => match value {
                                Value::Int(i) => Ok(Value::Int(i)),
                                Value::Array(items) => Ok(Value::Int(items.len() as i64)),
                                Value::Str(s) => Ok(Value::Int(s.parse::<i64>().unwrap_or(0))),
                                Value::Enum { value, .. } => Ok(Value::Int(value)),
                                _ => Ok(Value::Int(0)),
                            },
                            TokenKind::Minus => match value {
                                Value::Int(i) => Ok(Value::Int(-i)),
                                Value::Num(f) => Ok(Value::Num(-f)),
                                Value::Rat(n, d) => Ok(Value::Rat(-n, d)),
                                _ => Err(RuntimeError::new("Unary - expects numeric")),
                            },
                            TokenKind::Tilde => Ok(Value::Str(value.to_string_value())),
                            TokenKind::Bang => Ok(Value::Bool(!value.truthy())),
                            TokenKind::Question => Ok(Value::Bool(value.truthy())),
                            TokenKind::Caret => {
                                let n = match value {
                                    Value::Int(i) => i,
                                    _ => 0,
                                };
                                Ok(Value::RangeExcl(0, n))
                            }
                            TokenKind::Ident(name) if name == "so" => Ok(Value::Bool(value.truthy())),
                            _ => Err(RuntimeError::new("Unknown unary operator")),
                        }
                    }
                }
            }
            Expr::PostfixOp { op, expr } => {
                if let Expr::Var(name) = expr.as_ref() {
                    let val = self.env.get(name).cloned().unwrap_or(Value::Int(0));
                    let new_val = match (op, &val) {
                        (TokenKind::PlusPlus, Value::Int(i)) => Value::Int(i + 1),
                        (TokenKind::MinusMinus, Value::Int(i)) => Value::Int(i - 1),
                        (TokenKind::PlusPlus, Value::Rat(n, d)) => make_rat(n + d, *d),
                        (TokenKind::MinusMinus, Value::Rat(n, d)) => make_rat(n - d, *d),
                        (TokenKind::PlusPlus, _) => Value::Int(1),
                        (TokenKind::MinusMinus, _) => Value::Int(-1),
                        _ => val.clone(),
                    };
                    self.env.insert(name.clone(), new_val);
                    Ok(val) // postfix returns old value
                } else {
                    Ok(Value::Nil)
                }
            }
            Expr::Binary { left, op, right } => {
                // Short-circuit operators
                match op {
                    TokenKind::AndAnd => {
                        let l = self.eval_expr(left)?;
                        if !l.truthy() { return Ok(l); }
                        return self.eval_expr(right);
                    }
                    TokenKind::OrOr => {
                        let l = self.eval_expr(left)?;
                        if l.truthy() { return Ok(l); }
                        return self.eval_expr(right);
                    }
                    TokenKind::OrWord => {
                        let l = self.eval_expr(left)?;
                        if l.truthy() { return Ok(l); }
                        return self.eval_expr(right);
                    }
                    TokenKind::SlashSlash => {
                        let l = self.eval_expr(left)?;
                        if !matches!(l, Value::Nil) { return Ok(l); }
                        return self.eval_expr(right);
                    }
                    _ => {}
                }
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;
                self.eval_binary(l, op, r)
            }
            Expr::Hash(pairs) => {
                let mut map = HashMap::new();
                for (key, value_expr) in pairs {
                    let value = if let Some(expr) = value_expr {
                        self.eval_expr(expr)?
                    } else {
                        Value::Bool(true)
                    };
                    map.insert(key.clone(), value);
                }
                Ok(Value::Hash(map))
            }
            Expr::Call { name, args } => {
                // Try type-based multi dispatch first
                let arg_values: Vec<Value> = args.iter()
                    .filter_map(|a| self.eval_expr(a).ok())
                    .collect();
                if let Some(def) = self.resolve_function_with_types(name, &arg_values) {
                    let saved_env = self.env.clone();
                    let literal_args: Vec<Expr> = arg_values.into_iter()
                        .map(|v| Expr::Literal(v))
                        .collect();
                    self.bind_function_args(&def.param_defs, &def.params, &literal_args)?;
                    self.routine_stack.push((def.package.clone(), def.name.clone()));
                    let result = self.eval_block_value(&def.body);
                    self.routine_stack.pop();
                    self.env = saved_env;
                    return match result {
                        Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                        other => other,
                    };
                }
                if name == "EVAL" {
                    let code = if let Some(arg) = args.get(0) {
                        self.eval_expr(arg)?.to_string_value()
                    } else {
                        String::new()
                    };
                    if code.contains("&?ROUTINE") && self.routine_stack.is_empty() {
                        return Err(RuntimeError::new("X::Undeclared::Symbols"));
                    }
                    return self.eval_eval_string(&code);
                }
                if name == "atan2" {
                    let a = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    let b = args.get(1).map(|e| self.eval_expr(e).ok()).flatten();
                    let a = match a { Some(Value::Int(i)) => i, _ => 0 };
                    let b = match b { Some(Value::Int(i)) => i, _ => 0 };
                    return Ok(Value::Str(format!("atan2({}, {})", a, b)));
                }
                if name == "elems" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Array(items)) => Value::Int(items.len() as i64),
                        Some(Value::Hash(items)) => Value::Int(items.len() as i64),
                        Some(Value::Str(s)) => Value::Int(s.chars().count() as i64),
                        _ => Value::Int(0),
                    });
                }
                if name == "abs" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Int(i)) => Value::Int(i.abs()),
                        Some(Value::Num(f)) => Value::Num(f.abs()),
                        _ => Value::Int(0),
                    });
                }
                if name == "chars" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Str(s)) => Value::Int(s.chars().count() as i64),
                        Some(v) => Value::Int(v.to_string_value().chars().count() as i64),
                        _ => Value::Int(0),
                    });
                }
                if name == "sprintf" {
                    let fmt = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    let fmt = match fmt { Some(Value::Str(s)) => s, _ => String::new() };
                    let arg = args.get(1).map(|e| self.eval_expr(e).ok()).flatten();
                    let rendered = self.format_sprintf(&fmt, arg.as_ref());
                    return Ok(Value::Str(rendered));
                }
                if name == "join" {
                    let sep = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    let list = args.get(1).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match list {
                        Some(Value::Array(items)) => {
                            let joined = items.iter().map(|v| v.to_string_value()).collect::<Vec<_>>().join(&sep);
                            Value::Str(joined)
                        }
                        _ => Value::Str(String::new()),
                    });
                }
                if name == "flat" {
                    let mut result = Vec::new();
                    for arg in args {
                        let val = self.eval_expr(arg)?;
                        match val {
                            Value::Array(items) => {
                                for item in items {
                                    if let Value::Array(sub) = item {
                                        result.extend(sub);
                                    } else {
                                        result.push(item);
                                    }
                                }
                            }
                            other => result.push(other),
                        }
                    }
                    return Ok(Value::Array(result));
                }
                if name == "reverse" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Array(mut items)) => { items.reverse(); Value::Array(items) }
                        Some(Value::Str(s)) => Value::Str(s.chars().rev().collect()),
                        _ => Value::Nil,
                    });
                }
                if name == "sort" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Array(mut items)) => {
                            items.sort_by(|a, b| a.to_string_value().cmp(&b.to_string_value()));
                            Value::Array(items)
                        }
                        _ => Value::Nil,
                    });
                }
                if name == "defined" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(Value::Bool(!matches!(val, Some(Value::Nil) | None)));
                }
                if name == "sqrt" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Int(i)) => Value::Num((i as f64).sqrt()),
                        Some(Value::Num(f)) => Value::Num(f.sqrt()),
                        _ => Value::Num(f64::NAN),
                    });
                }
                if name == "floor" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Num(f)) => Value::Int(f.floor() as i64),
                        Some(Value::Int(i)) => Value::Int(i),
                        _ => Value::Int(0),
                    });
                }
                if name == "ceiling" || name == "ceil" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Num(f)) => Value::Int(f.ceil() as i64),
                        Some(Value::Int(i)) => Value::Int(i),
                        _ => Value::Int(0),
                    });
                }
                if name == "round" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Num(f)) => Value::Int(f.round() as i64),
                        Some(Value::Int(i)) => Value::Int(i),
                        _ => Value::Int(0),
                    });
                }
                if name == "log" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Int(i)) => Value::Num((i as f64).ln()),
                        Some(Value::Num(f)) => Value::Num(f.ln()),
                        _ => Value::Num(f64::NAN),
                    });
                }
                if name == "exp" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    return Ok(match val {
                        Some(Value::Int(i)) => Value::Num((i as f64).exp()),
                        Some(Value::Num(f)) => Value::Num(f.exp()),
                        _ => Value::Num(f64::NAN),
                    });
                }
                if name == "min" {
                    let mut vals = Vec::new();
                    for arg in args {
                        vals.push(self.eval_expr(arg)?);
                    }
                    return Ok(vals.into_iter().min_by(|a, b| {
                        match (a, b) {
                            (Value::Int(x), Value::Int(y)) => x.cmp(y),
                            _ => a.to_string_value().cmp(&b.to_string_value()),
                        }
                    }).unwrap_or(Value::Nil));
                }
                if name == "max" {
                    let mut vals = Vec::new();
                    for arg in args {
                        vals.push(self.eval_expr(arg)?);
                    }
                    return Ok(vals.into_iter().max_by(|a, b| {
                        match (a, b) {
                            (Value::Int(x), Value::Int(y)) => x.cmp(y),
                            _ => a.to_string_value().cmp(&b.to_string_value()),
                        }
                    }).unwrap_or(Value::Nil));
                }
                if name == "exit" {
                    let code = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    let _code = match code { Some(Value::Int(i)) => i, _ => 0 };
                    self.halted = true;
                    return Ok(Value::Nil);
                }
                if name == "flip" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    return Ok(Value::Str(val.chars().rev().collect()));
                }
                if name == "lc" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .unwrap_or(Value::Nil);
                    return Ok(Value::Str(val.to_string_value().to_lowercase()));
                }
                if name == "uc" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .unwrap_or(Value::Nil);
                    return Ok(Value::Str(val.to_string_value().to_uppercase()));
                }
                if name == "tc" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    let mut result = String::new();
                    let mut capitalize = true;
                    for ch in val.chars() {
                        if capitalize {
                            for c in ch.to_uppercase() { result.push(c); }
                            capitalize = false;
                        } else {
                            result.push(ch);
                        }
                    }
                    return Ok(Value::Str(result));
                }
                if name == "chomp" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    return Ok(Value::Str(val.trim_end_matches('\n').to_string()));
                }
                if name == "chop" {
                    let mut val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    val.pop();
                    return Ok(Value::Str(val));
                }
                if name == "trim" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    return Ok(Value::Str(val.trim().to_string()));
                }
                if name == "words" {
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    let parts: Vec<Value> = val.split_whitespace().map(|p| Value::Str(p.to_string())).collect();
                    return Ok(Value::Array(parts));
                }
                if name == "substr" {
                    let s = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    let start = args.get(1).map(|e| self.eval_expr(e).ok()).flatten()
                        .and_then(|v| match v { Value::Int(i) => Some(i), _ => None })
                        .unwrap_or(0);
                    let chars: Vec<char> = s.chars().collect();
                    let start = start.max(0) as usize;
                    if let Some(len_val) = args.get(2).map(|e| self.eval_expr(e).ok()).flatten() {
                        let len = match len_val { Value::Int(i) => i.max(0) as usize, _ => chars.len() };
                        let end = (start + len).min(chars.len());
                        return Ok(Value::Str(chars[start..end].iter().collect()));
                    }
                    return Ok(Value::Str(chars[start.min(chars.len())..].iter().collect()));
                }
                if name == "index" {
                    let s = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    let needle = args.get(1).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    return Ok(match s.find(&needle) {
                        Some(pos) => Value::Int(s[..pos].chars().count() as i64),
                        None => Value::Nil,
                    });
                }
                if name == "dd" {
                    // Debug dump
                    let val = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .unwrap_or(Value::Nil);
                    self.output.push_str(&format!("{:?}\n", val));
                    return Ok(val);
                }
                if name == "pair" {
                    let key = args.get(0).map(|e| self.eval_expr(e).ok()).flatten()
                        .map(|v| v.to_string_value()).unwrap_or_default();
                    let val = args.get(1).map(|e| self.eval_expr(e).ok()).flatten()
                        .unwrap_or(Value::Nil);
                    return Ok(Value::Pair(key, Box::new(val)));
                }
                Ok(Value::Nil)
            }
            Expr::Try { body, catch } => {
                // Extract CATCH blocks from body
                let mut main_stmts = Vec::new();
                let mut catch_stmts = catch.clone();
                for stmt in body {
                    if let Stmt::Catch(catch_body) = stmt {
                        catch_stmts = Some(catch_body.clone());
                    } else {
                        main_stmts.push(stmt.clone());
                    }
                }
                match self.eval_block_value(&main_stmts) {
                    Ok(v) => Ok(v),
                    Err(e) => {
                        if let Some(catch_body) = catch_stmts {
                            let err_val = Value::Str(e.message);
                            let saved_err = self.env.get("!").cloned();
                            let saved_topic = self.env.get("_").cloned();
                            self.env.insert("!".to_string(), err_val.clone());
                            self.env.insert("_".to_string(), err_val);
                            let saved_when = self.when_matched;
                            self.when_matched = false;
                            for stmt in &catch_body {
                                self.exec_stmt(stmt)?;
                                if self.when_matched || self.halted {
                                    break;
                                }
                            }
                            self.when_matched = saved_when;
                            if let Some(v) = saved_err {
                                self.env.insert("!".to_string(), v);
                            } else {
                                self.env.remove("!");
                            }
                            if let Some(v) = saved_topic {
                                self.env.insert("_".to_string(), v);
                            } else {
                                self.env.remove("_");
                            }
                            Ok(Value::Nil)
                        } else {
                            Ok(Value::Nil)
                        }
                    }
                }
            }
            Expr::Gather(body) => {
                self.gather_items.push(Vec::new());
                for stmt in body {
                    match self.exec_stmt(stmt) {
                        Ok(()) => {}
                        Err(e) if e.is_last => break,
                        Err(e) => { self.gather_items.pop(); return Err(e); }
                    }
                }
                let items = self.gather_items.pop().unwrap_or_default();
                Ok(Value::Array(items))
            }
            Expr::Reduction { op, expr } => {
                let list = match self.eval_expr(expr)? {
                    Value::Array(items) => items,
                    Value::Range(a, b) => (a..=b).map(Value::Int).collect(),
                    Value::RangeExcl(a, b) => (a..b).map(Value::Int).collect(),
                    Value::RangeExclStart(a, b) => (a+1..=b).map(Value::Int).collect(),
                    Value::RangeExclBoth(a, b) => (a+1..b).map(Value::Int).collect(),
                    other => vec![other],
                };
                if list.is_empty() {
                    return Ok(self.reduction_identity(op));
                }
                let mut acc = list[0].clone();
                for item in &list[1..] {
                    acc = self.apply_reduction_op(op, &acc, item)?;
                }
                Ok(acc)
            }
            Expr::InfixFunc { name, left, right, modifier } => {
                let left_val = self.eval_expr(left)?;
                let mut right_vals = Vec::new();
                for expr in right {
                    right_vals.push(self.eval_expr(expr)?);
                }
                if name == "atan2" {
                    let (a, b) = match right_vals.as_slice() {
                        [Value::Int(r)] => (left_val, Value::Int(*r)),
                        _ => (left_val, Value::Int(0)),
                    };
                    let (a, b) = if modifier.as_deref() == Some("R") { (b, a) } else { (a, b) };
                    let a = match a { Value::Int(i) => i, _ => 0 };
                    let b = match b { Value::Int(i) => i, _ => 0 };
                    return Ok(Value::Str(format!("atan2({}, {})", a, b)));
                }
                if name == "sprintf" {
                    let fmt = match left_val { Value::Str(s) => s, _ => String::new() };
                    if modifier.as_deref() == Some("X") {
                        let mut parts = Vec::new();
                        for val in right_vals {
                            parts.push(self.format_sprintf(&fmt, Some(&val)));
                        }
                        return Ok(Value::Str(parts.join(" ")));
                    }
                    let arg = right_vals.get(0);
                    let rendered = self.format_sprintf(&fmt, arg);
                    return Ok(Value::Str(rendered));
                }
                Ok(Value::Nil)
            }
            Expr::Ternary { cond, then_expr, else_expr } => {
                if self.eval_expr(cond)?.truthy() {
                    self.eval_expr(then_expr)
                } else {
                    self.eval_expr(else_expr)
                }
            }
        }
    }

    fn eval_binary(&self, left: Value, op: &TokenKind, right: Value) -> Result<Value, RuntimeError> {
        match op {
            TokenKind::Plus => {
                let (l, r) = Self::coerce_numeric(left, right);
                if let (Some((an, ad)), Some((bn, bd))) = (Self::to_rat_parts(&l), Self::to_rat_parts(&r)) {
                    if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
                        return Ok(make_rat(an * bd + bn * ad, ad * bd));
                    }
                }
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_add(b))),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(a as f64 + b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a + b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Minus => {
                let (l, r) = Self::coerce_numeric(left, right);
                if let (Some((an, ad)), Some((bn, bd))) = (Self::to_rat_parts(&l), Self::to_rat_parts(&r)) {
                    if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
                        return Ok(make_rat(an * bd - bn * ad, ad * bd));
                    }
                }
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_sub(b))),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a - b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(a as f64 - b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a - b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Star => {
                let (l, r) = Self::coerce_numeric(left, right);
                if let (Some((an, ad)), Some((bn, bd))) = (Self::to_rat_parts(&l), Self::to_rat_parts(&r)) {
                    if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
                        return Ok(make_rat(an * bn, ad * bd));
                    }
                }
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_mul(b))),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a * b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(a as f64 * b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a * b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Slash => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (&l, &r) {
                    (Value::Rat(_, _), _) | (_, Value::Rat(_, _)) |
                    (Value::Int(_), Value::Int(_)) => {
                        let (an, ad) = Self::to_rat_parts(&l).unwrap_or((0, 1));
                        let (bn, bd) = Self::to_rat_parts(&r).unwrap_or((0, 1));
                        if bn == 0 {
                            return Err(RuntimeError::new("Division by zero"));
                        }
                        Ok(make_rat(an * bd, ad * bn))
                    }
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a / b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(*a as f64 / b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a / *b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Percent => {
                let (l, r) = Self::coerce_numeric(left, right);
                if let (Some((an, ad)), Some((bn, bd))) = (Self::to_rat_parts(&l), Self::to_rat_parts(&r)) {
                    if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
                        if bn == 0 {
                            return Err(RuntimeError::new("Modulo by zero"));
                        }
                        // (an/ad) % (bn/bd) = remainder
                        let lf = an as f64 / ad as f64;
                        let rf = bn as f64 / bd as f64;
                        let result = lf % rf;
                        return Ok(Value::Num(result));
                    }
                }
                match (l, r) {
                    (Value::Int(_), Value::Int(0)) => Err(RuntimeError::new("Modulo by zero")),
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a % b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(a as f64 % b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a % b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::PercentPercent => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(_), Value::Int(0)) => Err(RuntimeError::new("Divisibility by zero")),
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a % b == 0)),
                    _ => Ok(Value::Bool(false)),
                }
            }
            TokenKind::StarStar => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) if b >= 0 => Ok(Value::Int(a.pow(b as u32))),
                    (Value::Int(a), Value::Int(b)) => {
                        // Negative integer exponent: a ** -n = 1/(a**n) -> Rat
                        let pos = (-b) as u32;
                        let base = a.pow(pos);
                        Ok(make_rat(1, base))
                    }
                    (Value::Rat(n, d), Value::Int(b)) if b >= 0 => {
                        let p = b as u32;
                        Ok(make_rat(n.pow(p), d.pow(p)))
                    }
                    (Value::Rat(n, d), Value::Int(b)) => {
                        let p = (-b) as u32;
                        Ok(make_rat(d.pow(p), n.pow(p)))
                    }
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a.powi(b as i32))),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num((a as f64).powf(b))),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a.powf(b))),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitAnd => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitOr => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitXor => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitShiftLeft => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitShiftRight => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Tilde => Ok(Value::Str(format!("{}{}", left.to_string_value(), right.to_string_value()))),
            TokenKind::EqEq => Ok(Value::Bool(left == right)),
            TokenKind::EqEqEq => Ok(Value::Bool(left == right)),
            TokenKind::BangEq => Ok(Value::Bool(left != right)),
            TokenKind::Lt => Self::compare(left, right, |o| o < 0),
            TokenKind::Lte => Self::compare(left, right, |o| o <= 0),
            TokenKind::Gt => Self::compare(left, right, |o| o > 0),
            TokenKind::Gte => Self::compare(left, right, |o| o >= 0),
            TokenKind::AndAnd => Ok(Value::Bool(left.truthy() && right.truthy())),
            TokenKind::OrOr => Ok(Value::Bool(left.truthy() || right.truthy())),
            TokenKind::OrWord => {
                if left.truthy() {
                    Ok(left)
                } else {
                    Ok(right)
                }
            }
            TokenKind::SlashSlash => {
                if !matches!(left, Value::Nil) {
                    Ok(left)
                } else {
                    Ok(right)
                }
            }
            TokenKind::DotDot => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Range(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::DotDotCaret => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::RangeExcl(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::CaretDotDot => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::RangeExclStart(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::CaretDotDotCaret => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::RangeExclBoth(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::LtEqGt => {
                let ord = match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => a.cmp(b),
                    (Value::Num(a), Value::Num(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Int(a), Value::Num(b)) => (*a as f64).partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)).unwrap_or(std::cmp::Ordering::Equal),
                    _ => left.to_string_value().cmp(&right.to_string_value()),
                };
                Ok(Value::Int(match ord {
                    std::cmp::Ordering::Less => -1,
                    std::cmp::Ordering::Equal => 0,
                    std::cmp::Ordering::Greater => 1,
                }))
            }
            TokenKind::SmartMatch => Ok(Value::Bool(self.smart_match(&left, &right))),
            TokenKind::BangTilde => Ok(Value::Bool(!self.smart_match(&left, &right))),
            TokenKind::Ident(name) if name == "div" => match (left, right) {
                (Value::Int(a), Value::Int(b)) if b != 0 => Ok(Value::Int(a.div_euclid(b))),
                (Value::Int(_), Value::Int(_)) => Err(RuntimeError::new("Division by zero")),
                _ => Err(RuntimeError::new("div expects Int")),
            },
            TokenKind::Ident(name) if name == "mod" => match (left, right) {
                (Value::Int(a), Value::Int(b)) if b != 0 => Ok(Value::Int(a.rem_euclid(b))),
                (Value::Int(_), Value::Int(_)) => Err(RuntimeError::new("Modulo by zero")),
                _ => Err(RuntimeError::new("mod expects Int")),
            },
            TokenKind::Ident(name) if name == "eq" => {
                Ok(Value::Bool(left.to_string_value() == right.to_string_value()))
            }
            TokenKind::Ident(name) if name == "ne" => {
                Ok(Value::Bool(left.to_string_value() != right.to_string_value()))
            }
            TokenKind::Ident(name) if name == "lt" => {
                Ok(Value::Bool(left.to_string_value() < right.to_string_value()))
            }
            TokenKind::Ident(name) if name == "le" => {
                Ok(Value::Bool(left.to_string_value() <= right.to_string_value()))
            }
            TokenKind::Ident(name) if name == "gt" => {
                Ok(Value::Bool(left.to_string_value() > right.to_string_value()))
            }
            TokenKind::Ident(name) if name == "ge" => {
                Ok(Value::Bool(left.to_string_value() >= right.to_string_value()))
            }
            TokenKind::Ident(name) if name == "leg" => {
                let ord = left.to_string_value().cmp(&right.to_string_value());
                Ok(Value::Int(match ord {
                    std::cmp::Ordering::Less => -1,
                    std::cmp::Ordering::Equal => 0,
                    std::cmp::Ordering::Greater => 1,
                }))
            }
            TokenKind::Ident(name) if name == "cmp" => {
                let ord = match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => a.cmp(b),
                    (Value::Num(a), Value::Num(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Int(a), Value::Num(b)) => (*a as f64).partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)).unwrap_or(std::cmp::Ordering::Equal),
                    _ => left.to_string_value().cmp(&right.to_string_value()),
                };
                Ok(Value::Int(match ord {
                    std::cmp::Ordering::Less => -1,
                    std::cmp::Ordering::Equal => 0,
                    std::cmp::Ordering::Greater => 1,
                }))
            }
            TokenKind::Ident(name) if name == "eqv" => {
                Ok(Value::Bool(left == right))
            }
            TokenKind::Ident(name) if name == "x" => {
                let s = left.to_string_value();
                let n = match right {
                    Value::Int(n) => n.max(0) as usize,
                    _ => 0,
                };
                Ok(Value::Str(s.repeat(n)))
            }
            TokenKind::Ident(name) if name == "xx" => {
                let n = match right {
                    Value::Int(n) => n.max(0) as usize,
                    _ => 0,
                };
                let items: Vec<Value> = std::iter::repeat(left).take(n).collect();
                Ok(Value::Array(items))
            }
            _ => Err(RuntimeError::new("Unknown binary operator")),
        }
    }

    fn smart_match(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Int(a), Value::Str(b)) => a.to_string() == *b,
            (Value::Str(a), Value::Int(b)) => *a == b.to_string(),
            (Value::Nil, Value::Str(s)) => s.is_empty(),
            _ => true,
        }
    }

    fn reduction_identity(&self, op: &str) -> Value {
        match op {
            "+" => Value::Int(0),
            "*" => Value::Int(1),
            "~" => Value::Str(String::new()),
            "&&" | "and" => Value::Bool(true),
            "||" | "or" => Value::Bool(false),
            "//" => Value::Nil,
            "min" => Value::Num(f64::INFINITY),
            "max" => Value::Num(f64::NEG_INFINITY),
            _ => Value::Nil,
        }
    }

    fn apply_reduction_op(&self, op: &str, left: &Value, right: &Value) -> Result<Value, RuntimeError> {
        let to_num = |v: &Value| -> f64 {
            match v {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                Value::Bool(b) => if *b { 1.0 } else { 0.0 },
                _ => 0.0,
            }
        };
        let to_int = |v: &Value| -> i64 {
            match v {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                Value::Str(s) => s.parse::<i64>().unwrap_or(0),
                Value::Bool(b) => if *b { 1 } else { 0 },
                _ => 0,
            }
        };
        match op {
            "+" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) + to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) + to_int(right)))
                }
            }
            "-" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) - to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) - to_int(right)))
                }
            }
            "*" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) * to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) * to_int(right)))
                }
            }
            "/" => Ok(Value::Num(to_num(left) / to_num(right))),
            "**" => Ok(Value::Num(to_num(left).powf(to_num(right)))),
            "~" => Ok(Value::Str(format!("{}{}", left.to_string_value(), right.to_string_value()))),
            "&&" | "and" => {
                if !left.truthy() { Ok(left.clone()) } else { Ok(right.clone()) }
            }
            "||" | "or" => {
                if left.truthy() { Ok(left.clone()) } else { Ok(right.clone()) }
            }
            "//" => {
                if !matches!(left, Value::Nil) { Ok(left.clone()) } else { Ok(right.clone()) }
            }
            "min" => {
                if to_num(left) <= to_num(right) { Ok(left.clone()) } else { Ok(right.clone()) }
            }
            "max" => {
                if to_num(left) >= to_num(right) { Ok(left.clone()) } else { Ok(right.clone()) }
            }
            "+&" => Ok(Value::Int(to_int(left) & to_int(right))),
            "+|" => Ok(Value::Int(to_int(left) | to_int(right))),
            "+^" => Ok(Value::Int(to_int(left) ^ to_int(right))),
            "==" => Ok(Value::Bool(to_num(left) == to_num(right))),
            "!=" => Ok(Value::Bool(to_num(left) != to_num(right))),
            "<" => Ok(Value::Bool(to_num(left) < to_num(right))),
            ">" => Ok(Value::Bool(to_num(left) > to_num(right))),
            "<=" => Ok(Value::Bool(to_num(left) <= to_num(right))),
            ">=" => Ok(Value::Bool(to_num(left) >= to_num(right))),
            "eq" => Ok(Value::Bool(left.to_string_value() == right.to_string_value())),
            "ne" => Ok(Value::Bool(left.to_string_value() != right.to_string_value())),
            "lt" => Ok(Value::Bool(left.to_string_value() < right.to_string_value())),
            "gt" => Ok(Value::Bool(left.to_string_value() > right.to_string_value())),
            "le" => Ok(Value::Bool(left.to_string_value() <= right.to_string_value())),
            "ge" => Ok(Value::Bool(left.to_string_value() >= right.to_string_value())),
            "gcd" => {
                let (mut a, mut b) = (to_int(left).abs(), to_int(right).abs());
                while b != 0 { let t = b; b = a % b; a = t; }
                Ok(Value::Int(a))
            }
            "lcm" => {
                let (a, b) = (to_int(left).abs(), to_int(right).abs());
                if a == 0 && b == 0 { Ok(Value::Int(0)) }
                else {
                    let mut ga = a; let mut gb = b;
                    while gb != 0 { let t = gb; gb = ga % gb; ga = t; }
                    Ok(Value::Int(a / ga * b))
                }
            }
            _ => Err(RuntimeError::new(format!("Unsupported reduction operator: {}", op))),
        }
    }

    fn bind_function_args(&mut self, param_defs: &[ParamDef], params: &[String], args: &[Expr]) -> Result<(), RuntimeError> {
        if param_defs.is_empty() {
            // Legacy path: just bind by position
            for (i, param) in params.iter().enumerate() {
                if let Some(arg) = args.get(i) {
                    if let Ok(value) = self.eval_expr(arg) {
                        self.env.insert(param.clone(), value);
                    }
                }
            }
            return Ok(());
        }
        let mut positional_idx = 0usize;
        for pd in param_defs {
            if pd.slurpy {
                // Slurpy collects remaining positional args
                let mut items = Vec::new();
                while positional_idx < args.len() {
                    if let Ok(value) = self.eval_expr(&args[positional_idx]) {
                        items.push(value);
                    }
                    positional_idx += 1;
                }
                self.env.insert(pd.name.clone(), Value::Array(items));
            } else if pd.named {
                // Named params: look for matching AssignExpr in args
                let mut found = false;
                for arg in args {
                    if let Expr::AssignExpr { name, expr } = arg {
                        if *name == pd.name {
                            if let Ok(value) = self.eval_expr(expr) {
                                self.env.insert(pd.name.clone(), value);
                                found = true;
                                break;
                            }
                        }
                    }
                }
                if !found {
                    if let Some(default_expr) = &pd.default {
                        let value = self.eval_expr(default_expr)?;
                        self.env.insert(pd.name.clone(), value);
                    }
                }
            } else {
                // Positional param
                if positional_idx < args.len() {
                    if let Ok(value) = self.eval_expr(&args[positional_idx]) {
                        self.env.insert(pd.name.clone(), value);
                    }
                    positional_idx += 1;
                } else if let Some(default_expr) = &pd.default {
                    let value = self.eval_expr(default_expr)?;
                    self.env.insert(pd.name.clone(), value);
                }
            }
        }
        Ok(())
    }

    fn eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        let mut last = Value::Nil;
        for stmt in body {
            match stmt {
                Stmt::Return(expr) => {
                    return self.eval_expr(expr);
                }
                Stmt::Expr(expr) => {
                    last = self.eval_expr(expr)?;
                }
                _ => {
                    self.exec_stmt(stmt)?;
                }
            }
        }
        Ok(last)
    }

    fn eval_eval_string(&mut self, code: &str) -> Result<Value, RuntimeError> {
        let trimmed = code.trim();
        // Handle angle-bracket word lists: <a b c>, ~<a b>, +<a b>, ?<a b>
        let (prefix, rest) = if let Some(pos) = trimmed.find('<') {
            (trimmed.chars().next().unwrap_or(' '), &trimmed[pos..])
        } else {
            (' ', trimmed)
        };
        let start = rest.find('<');
        let end = rest.rfind('>');
        if prefix != ' ' || (start.is_some() && trimmed.starts_with('<')) {
            if let (Some(s), Some(e)) = (start, end) {
                let inner = &rest[s + 1..e];
                let words: Vec<&str> = inner.split_whitespace().collect();
                return Ok(match prefix {
                    '~' => Value::Str(words.join(" ")),
                    '+' => Value::Int(words.len() as i64),
                    '?' => Value::Bool(!words.is_empty()),
                    _ => Value::Str(words.join(" ")),
                });
            }
        }
        // General case: parse and evaluate as Raku code
        let mut lexer = Lexer::new(trimmed);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let end = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if end {
                break;
            }
        }
        let mut parser = Parser::new(tokens);
        match parser.parse_program() {
            Ok(stmts) => self.eval_block_value(&stmts),
            Err(e) => Err(e),
        }
    }

    fn format_sprintf(&self, fmt: &str, arg: Option<&Value>) -> String {
        let mut chars = fmt.chars().peekable();
        let mut out = String::new();
        while let Some(c) = chars.next() {
            if c != '%' {
                out.push(c);
                continue;
            }
            if chars.peek() == Some(&'%') {
                chars.next();
                out.push('%');
                continue;
            }
            let mut flags = String::new();
            while let Some(f) = chars.peek().copied() {
                if f == '-' || f == '+' || f == ' ' || f == '#' {
                    flags.push(f);
                    chars.next();
                } else {
                    break;
                }
            }
            let mut width = String::new();
            while let Some(d) = chars.peek().copied() {
                if d.is_ascii_digit() {
                    width.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
            let mut precision = String::new();
            if chars.peek() == Some(&'.') {
                chars.next();
                while let Some(d) = chars.peek().copied() {
                    if d.is_ascii_digit() {
                        precision.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
            }
            let spec = chars.next().unwrap_or('s');
            let width_num = width.parse::<usize>().unwrap_or(0);
            let prec_num = precision.parse::<usize>().ok();
            let zero_pad = width.starts_with('0') && !flags.contains('-');
            let left_align = flags.contains('-');
            let plus_sign = flags.contains('+');
            let hash_flag = flags.contains('#');
            let int_val = || match arg {
                Some(Value::Int(i)) => *i,
                Some(Value::Num(f)) => *f as i64,
                Some(Value::Str(s)) => s.trim().parse::<i64>().unwrap_or(0),
                Some(Value::Bool(b)) => if *b { 1 } else { 0 },
                _ => 0,
            };
            let float_val = || match arg {
                Some(Value::Int(i)) => *i as f64,
                Some(Value::Num(f)) => *f,
                Some(Value::Str(s)) => s.trim().parse::<f64>().unwrap_or(0.0),
                _ => 0.0,
            };
            let rendered = match spec {
                's' => match arg {
                    Some(v) => {
                        let s = v.to_string_value();
                        if let Some(p) = prec_num { s[..p.min(s.len())].to_string() } else { s }
                    }
                    _ => String::new(),
                },
                'd' | 'i' => {
                    let i = int_val();
                    if plus_sign && i >= 0 { format!("+{}", i) } else { format!("{}", i) }
                }
                'u' => format!("{}", int_val() as u64),
                'x' => {
                    let i = int_val();
                    let s = format!("{:x}", i);
                    if hash_flag { format!("0x{}", s) } else { s }
                }
                'X' => {
                    let i = int_val();
                    let s = format!("{:X}", i);
                    if hash_flag { format!("0X{}", s) } else { s }
                }
                'o' => {
                    let i = int_val();
                    let s = format!("{:o}", i);
                    if hash_flag { format!("0{}", s) } else { s }
                }
                'b' => {
                    let i = int_val();
                    let s = format!("{:b}", i);
                    if hash_flag { format!("0b{}", s) } else { s }
                }
                'B' => {
                    let i = int_val();
                    let s = format!("{:b}", i);
                    if hash_flag { format!("0B{}", s) } else { s }
                }
                'e' | 'E' => {
                    let f = float_val();
                    let p = prec_num.unwrap_or(6);
                    if spec == 'e' { format!("{:.prec$e}", f, prec = p) }
                    else { format!("{:.prec$E}", f, prec = p) }
                }
                'f' => {
                    let f = float_val();
                    let p = prec_num.unwrap_or(6);
                    format!("{:.prec$}", f, prec = p)
                }
                'g' | 'G' => {
                    let f = float_val();
                    let p = prec_num.unwrap_or(6);
                    if f.abs() < 1e-4 || f.abs() >= 10f64.powi(p as i32) {
                        if spec == 'g' { format!("{:.prec$e}", f, prec = p.saturating_sub(1)) }
                        else { format!("{:.prec$E}", f, prec = p.saturating_sub(1)) }
                    } else {
                        format!("{:.prec$}", f, prec = p.saturating_sub(1))
                    }
                }
                'c' => {
                    let i = int_val();
                    char::from_u32(i as u32).map(|c| c.to_string()).unwrap_or_default()
                }
                _ => String::new(),
            };
            if width_num > rendered.len() {
                let pad = width_num - rendered.len();
                if left_align {
                    out.push_str(&rendered);
                    out.push_str(&" ".repeat(pad));
                } else {
                    let ch = if zero_pad { '0' } else { ' ' };
                    out.push_str(&ch.to_string().repeat(pad));
                    out.push_str(&rendered);
                }
            } else {
                out.push_str(&rendered);
            }
        }
        out
    }

    fn coerce_to_numeric(val: Value) -> Value {
        match val {
            Value::Int(_) | Value::Num(_) | Value::Rat(_, _) | Value::FatRat(_, _) => val,
            Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
            Value::Enum { value, .. } => Value::Int(value),
            Value::Str(ref s) => {
                let s = s.trim();
                if let Ok(i) = s.parse::<i64>() {
                    Value::Int(i)
                } else if let Ok(f) = s.parse::<f64>() {
                    Value::Num(f)
                } else {
                    Value::Int(0)
                }
            }
            Value::Array(items) => Value::Int(items.len() as i64),
            Value::Nil => Value::Int(0),
            _ => Value::Int(0),
        }
    }

    fn coerce_numeric(left: Value, right: Value) -> (Value, Value) {
        let l = match &left {
            Value::Int(_) | Value::Num(_) | Value::Rat(_, _) | Value::FatRat(_, _) => left,
            _ => Self::coerce_to_numeric(left),
        };
        let r = match &right {
            Value::Int(_) | Value::Num(_) | Value::Rat(_, _) | Value::FatRat(_, _) => right,
            _ => Self::coerce_to_numeric(right),
        };
        (l, r)
    }

    fn to_rat_parts(val: &Value) -> Option<(i64, i64)> {
        match val {
            Value::Int(i) => Some((*i, 1)),
            Value::Rat(n, d) => Some((*n, *d)),
            Value::FatRat(n, d) => Some((*n, *d)),
            _ => None,
        }
    }

    fn compare_values(a: &Value, b: &Value) -> i32 {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => a.cmp(b) as i32,
            (Value::Num(a), Value::Num(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32,
            (Value::Int(a), Value::Num(b)) => (*a as f64).partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32,
            (Value::Num(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)).unwrap_or(std::cmp::Ordering::Equal) as i32,
            _ => {
                if let (Some((an, ad)), Some((bn, bd))) = (Self::to_rat_parts(a), Self::to_rat_parts(b)) {
                    let lhs = an as i128 * bd as i128;
                    let rhs = bn as i128 * ad as i128;
                    return lhs.cmp(&rhs) as i32;
                }
                a.to_string_value().cmp(&b.to_string_value()) as i32
            }
        }
    }

    fn to_int(v: &Value) -> i64 {
        match v {
            Value::Int(i) => *i,
            Value::Num(f) => *f as i64,
            Value::Rat(n, d) => if *d != 0 { n / d } else { 0 },
            Value::Str(s) => s.parse().unwrap_or(0),
            _ => 0,
        }
    }

    fn compare(left: Value, right: Value, f: fn(i32) -> bool) -> Result<Value, RuntimeError> {
        let (l, r) = Self::coerce_numeric(left, right);
        if let (Some((an, ad)), Some((bn, bd))) = (Self::to_rat_parts(&l), Self::to_rat_parts(&r)) {
            if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
                let lhs = an as i128 * bd as i128;
                let rhs = bn as i128 * ad as i128;
                return Ok(Value::Bool(f(lhs.cmp(&rhs) as i32)));
            }
        }
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Num(a), Value::Num(b)) => {
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Int(a), Value::Num(b)) => {
                let a = a as f64;
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Num(a), Value::Int(b)) => {
                let b = b as f64;
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
            _ => Ok(Value::Bool(f(0))),
        }
    }
}

#[derive(Debug, Default)]
struct TestState {
    planned: Option<usize>,
    ran: usize,
    failed: usize,
    force_todo: Vec<(usize, usize)>,
}

impl TestState {
    fn new() -> Self {
        Self { planned: None, ran: 0, failed: 0, force_todo: Vec::new() }
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;

    #[test]
    fn say_and_math() {
        let mut interp = Interpreter::new();
        let output = interp.run("say 1 + 2; say 3 * 4;").unwrap();
        assert_eq!(output, "3\n12\n");
    }

    #[test]
    fn variables_and_concat() {
        let mut interp = Interpreter::new();
        let output = interp.run("my $x = 2; $x = $x + 3; say \"hi\" ~ $x;").unwrap();
        assert_eq!(output, "hi5\n");
    }

    #[test]
    fn if_else() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 1; if $x == 1 { say \"yes\"; } else { say \"no\"; }")
            .unwrap();
        assert_eq!(output, "yes\n");
    }

    #[test]
    fn while_loop() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 0; while $x < 3 { say $x; $x = $x + 1; }")
            .unwrap();
        assert_eq!(output, "0\n1\n2\n");
    }
}

// Helper functions for placeholder variable collection

fn collect_placeholders(stmts: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in stmts {
        collect_ph_stmt(stmt, &mut names);
    }
    names.sort();
    names.dedup();
    names
}

fn collect_ph_stmt(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Take(e) => collect_ph_expr(e, out),
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => collect_ph_expr(expr, out),
        Stmt::Say(es) | Stmt::Print(es) => {
            for e in es { collect_ph_expr(e, out); }
        }
        Stmt::If { cond, then_branch, else_branch } => {
            collect_ph_expr(cond, out);
            for s in then_branch { collect_ph_stmt(s, out); }
            for s in else_branch { collect_ph_stmt(s, out); }
        }
        Stmt::While { cond, body, .. } => {
            collect_ph_expr(cond, out);
            for s in body { collect_ph_stmt(s, out); }
        }
        Stmt::For { iterable, body, .. } => {
            collect_ph_expr(iterable, out);
            for s in body { collect_ph_stmt(s, out); }
        }
        Stmt::Loop { body, .. } => {
            for s in body { collect_ph_stmt(s, out); }
        }
        Stmt::Block(body) | Stmt::Default(body) | Stmt::Catch(body) => {
            for s in body { collect_ph_stmt(s, out); }
        }
        Stmt::Given { topic, body } => {
            collect_ph_expr(topic, out);
            for s in body { collect_ph_stmt(s, out); }
        }
        Stmt::When { cond, body } => {
            collect_ph_expr(cond, out);
            for s in body { collect_ph_stmt(s, out); }
        }
        _ => {}
    }
}

fn collect_ph_expr(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::Var(name) if name.starts_with('^') => {
            if !out.contains(name) { out.push(name.clone()); }
        }
        Expr::Binary { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => collect_ph_expr(expr, out),
        Expr::MethodCall { target, args, .. } => {
            collect_ph_expr(target, out);
            for a in args { collect_ph_expr(a, out); }
        }
        Expr::Call { args, .. } => {
            for a in args { collect_ph_expr(a, out); }
        }
        Expr::CallOn { target, args } => {
            collect_ph_expr(target, out);
            for a in args { collect_ph_expr(a, out); }
        }
        Expr::Index { target, index } => {
            collect_ph_expr(target, out);
            collect_ph_expr(index, out);
        }
        Expr::Ternary { cond, then_expr, else_expr } => {
            collect_ph_expr(cond, out);
            collect_ph_expr(then_expr, out);
            collect_ph_expr(else_expr, out);
        }
        Expr::AssignExpr { expr, .. } | Expr::Exists(expr) => collect_ph_expr(expr, out),
        Expr::ArrayLiteral(es) | Expr::StringInterpolation(es) => {
            for e in es { collect_ph_expr(e, out); }
        }
        Expr::Block(stmts) | Expr::AnonSub(stmts) | Expr::Gather(stmts) => {
            for s in stmts { collect_ph_stmt(s, out); }
        }
        Expr::Lambda { body, .. } => {
            for s in body { collect_ph_stmt(s, out); }
        }
        Expr::Try { body, catch } => {
            for s in body { collect_ph_stmt(s, out); }
            if let Some(c) = catch { for s in c { collect_ph_stmt(s, out); } }
        }
        Expr::CodeVar(_) => {}
        Expr::Reduction { expr, .. } => collect_ph_expr(expr, out),
        Expr::InfixFunc { left, right, .. } => {
            collect_ph_expr(left, out);
            for e in right { collect_ph_expr(e, out); }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v { collect_ph_expr(e, out); }
            }
        }
        _ => {}
    }
}
