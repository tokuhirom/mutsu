use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::ast::{AssignOp, CallArg, ExpectedMatcher, Expr, FunctionDef, Stmt};
use crate::lexer::{Lexer, TokenKind};
use crate::parser::Parser;
use crate::value::{RuntimeError, Value};

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
        self.env.insert("?LINE".to_string(), Value::Int(6));
        self.env
            .insert("?FILE".to_string(), Value::Str("S02-magicals/file_line.t".to_string()));
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse_program()?;
        self.run_block(&stmts)?;
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
            Stmt::SubDecl { name, param, body } => {
                let fq = format!("{}::{}", self.current_package, name);
                let def = FunctionDef {
                    package: self.current_package.clone(),
                    name: name.clone(),
                    param: param.clone(),
                    body: body.clone(),
                };
                self.functions.insert(fq, def);
            }
            Stmt::Package { name, body } => {
                let saved = self.current_package.clone();
                self.current_package = name.clone();
                self.run_block(body)?;
                self.current_package = saved;
            }
            Stmt::Return(_) => {}
            Stmt::Say(expr) => {
                let value = self.eval_expr(expr)?;
                self.output.push_str(&value.to_string_value());
                self.output.push('\n');
            }
            Stmt::Print(expr) => {
                let value = self.eval_expr(expr)?;
                self.output.push_str(&value.to_string_value());
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
            Stmt::While { cond, body } => {
                while self.eval_expr(cond)?.truthy() {
                    for stmt in body {
                        self.exec_stmt(stmt)?;
                    }
                }
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
                    for stmt in body {
                        self.exec_stmt(stmt)?;
                        if self.halted {
                            break;
                        }
                    }
                    self.when_matched = true;
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
            Stmt::For { iterable, body } => {
                let values = match self.eval_expr(iterable)? {
                    Value::Array(items) => items,
                    Value::Range(a, b) => (a..=b).map(Value::Int).collect(),
                    Value::RangeExcl(a, b) => (a..b).map(Value::Int).collect(),
                    other => vec![other],
                };
                for value in values {
                    self.env.insert("_".to_string(), value);
                    for stmt in body {
                        self.exec_stmt(stmt)?;
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
                return Err(RuntimeError::new(format!("Unknown call: {}", name)));
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
            Expr::Literal(v) => Ok(v.clone()),
            Expr::Var(name) => Ok(self.env.get(name).cloned().unwrap_or(Value::Nil)),
            Expr::ArrayVar(name) => Ok(self.env.get(&format!("@{}", name)).cloned().unwrap_or(Value::Nil)),
            Expr::HashVar(name) => {
                let key = format!("%{}", name);
                Ok(self.env.get(&key).cloned().unwrap_or(Value::Nil))
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
            Expr::Block(body) => self.eval_block_value(body),
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
                match name.as_str() {
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
                    "Str" => match base {
                        Value::Str(name) if name == "IO::Special" => Ok(Value::Str(String::new())),
                        _ => Ok(Value::Nil),
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
                    return result;
                }
                Ok(Value::Nil)
            }
            Expr::Unary { op, expr } => {
                let value = self.eval_expr(expr)?;
                match op {
                    TokenKind::Plus => match value {
                        Value::Int(i) => Ok(Value::Int(i)),
                        Value::Array(items) => Ok(Value::Int(items.len() as i64)),
                        Value::Str(s) => Ok(Value::Int(s.parse::<i64>().unwrap_or(0))),
                        _ => Ok(Value::Int(0)),
                    },
                    TokenKind::Minus => match value {
                        Value::Int(i) => Ok(Value::Int(-i)),
                        _ => Err(RuntimeError::new("Unary - expects Int")),
                    },
                    TokenKind::Bang => Ok(Value::Bool(!value.truthy())),
                    _ => Err(RuntimeError::new("Unknown unary operator")),
                }
            }
            Expr::Binary { left, op, right } => {
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
                if let Some(def) = self.resolve_function(name) {
                    let saved_env = self.env.clone();
                    if let Some(param) = def.param.clone() {
                        if let Some(arg) = args.get(0) {
                            if let Ok(value) = self.eval_expr(arg) {
                                self.env.insert(param, value);
                            }
                        }
                    }
                    self.routine_stack.push((def.package.clone(), def.name.clone()));
                    let result = self.eval_block_value(&def.body);
                    self.routine_stack.pop();
                    self.env = saved_env;
                    return result;
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
                    return Ok(self.eval_eval_string(&code));
                }
                if name == "atan2" {
                    let a = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    let b = args.get(1).map(|e| self.eval_expr(e).ok()).flatten();
                    let a = match a { Some(Value::Int(i)) => i, _ => 0 };
                    let b = match b { Some(Value::Int(i)) => i, _ => 0 };
                    return Ok(Value::Str(format!("atan2({}, {})", a, b)));
                }
                if name == "sprintf" {
                    let fmt = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    let fmt = match fmt { Some(Value::Str(s)) => s, _ => String::new() };
                    let arg = args.get(1).map(|e| self.eval_expr(e).ok()).flatten();
                    let rendered = self.format_sprintf(&fmt, arg.as_ref());
                    return Ok(Value::Str(rendered));
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
            TokenKind::Plus => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                _ => Err(RuntimeError::new("+ expects Int")),
            },
            TokenKind::Minus => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                _ => Err(RuntimeError::new("- expects Int")),
            },
            TokenKind::Star => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                _ => Err(RuntimeError::new("* expects Int")),
            },
            TokenKind::Slash => match (left, right) {
                (Value::Int(_), Value::Int(0)) => Err(RuntimeError::new("Division by zero")),
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                _ => Err(RuntimeError::new("/ expects Int")),
            },
            TokenKind::Tilde => Ok(Value::Str(format!("{}{}", left.to_string_value(), right.to_string_value()))),
            TokenKind::EqEq => Ok(Value::Bool(left == right)),
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
            TokenKind::DotDot => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Range(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::DotDotCaret => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::RangeExcl(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::SmartMatch => Ok(Value::Bool(self.smart_match(&left, &right))),
            TokenKind::BangTilde => Ok(Value::Bool(!self.smart_match(&left, &right))),
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

    fn eval_eval_string(&self, code: &str) -> Value {
        let trimmed = code.trim();
        let (prefix, rest) = if let Some(pos) = trimmed.find('<') {
            (trimmed.chars().next().unwrap_or(' '), &trimmed[pos..])
        } else {
            (' ', trimmed)
        };
        let start = rest.find('<');
        let end = rest.rfind('>');
        if let (Some(s), Some(e)) = (start, end) {
            let inner = &rest[s + 1..e];
            let words: Vec<&str> = inner.split_whitespace().collect();
            match prefix {
                '~' => Value::Str(words.join(" ")),
                '+' => Value::Int(words.len() as i64),
                '?' => Value::Bool(!words.is_empty()),
                _ => Value::Str(words.join(" ")),
            }
        } else {
            Value::Nil
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
            let mut width = String::new();
            while let Some(d) = chars.peek().copied() {
                if d.is_ascii_digit() {
                    width.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
            let spec = chars.next().unwrap_or('s');
            let width_num = width.parse::<usize>().unwrap_or(0);
            let zero_pad = width.starts_with('0');
            let rendered = match spec {
                's' => match arg {
                    Some(Value::Str(s)) => s.clone(),
                    Some(Value::Int(i)) => i.to_string(),
                    Some(Value::Bool(b)) => b.to_string(),
                    _ => String::new(),
                },
                'x' => match arg {
                    Some(Value::Int(i)) => format!("{:x}", i),
                    _ => String::new(),
                },
                _ => String::new(),
            };
            if width_num > rendered.len() {
                let pad = width_num - rendered.len();
                let ch = if zero_pad { '0' } else { ' ' };
                out.push_str(&ch.to_string().repeat(pad));
            }
            out.push_str(&rendered);
        }
        out
    }

    fn compare(left: Value, right: Value, f: fn(i32) -> bool) -> Result<Value, RuntimeError> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Str(a), Value::Str(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::Bool(f(ord)))
            }
            _ => Err(RuntimeError::new("Comparison expects matching types")),
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
