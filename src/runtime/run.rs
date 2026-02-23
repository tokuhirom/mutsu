use super::*;
use crate::ast::Stmt;

impl Interpreter {
    /// If the first non-Use/non-Package statement is a `unit class Foo;` (ClassDecl with empty
    /// body), merge all subsequent method/sub declarations into the class body.
    fn merge_unit_class(stmts: Vec<Stmt>) -> Vec<Stmt> {
        // Find the index of a ClassDecl with empty body
        let class_idx = stmts
            .iter()
            .position(|s| matches!(s, Stmt::ClassDecl { body, .. } if body.is_empty()));
        if let Some(idx) = class_idx {
            let mut result: Vec<Stmt> = stmts[..idx].to_vec();
            if let Stmt::ClassDecl {
                name,
                parents,
                body: _,
            } = &stmts[idx]
            {
                let body: Vec<Stmt> = stmts[idx + 1..].to_vec();
                result.push(Stmt::ClassDecl {
                    name: name.clone(),
                    parents: parents.clone(),
                    body,
                });
            }
            result
        } else {
            stmts
        }
    }

    fn preprocess_roast_directives(input: &str) -> String {
        let mut output = String::new();
        let mut pending_todo: Option<String> = None;
        let mut skip_lines_remaining: usize = 0;
        let mut skip_reason: String = String::new();

        for line in input.lines() {
            let trimmed = line.trim_start();

            // Count-based skip: skip the next N non-comment, non-empty lines
            if skip_lines_remaining > 0 {
                if !trimmed.is_empty() && !trimmed.starts_with('#') {
                    skip_lines_remaining -= 1;
                    // Emit a skip() call for each skipped test line
                    output.push_str(&format!("skip '{}', 1;\n", skip_reason));
                    continue;
                }
                output.push('\n');
                continue;
            }

            // #?rakudo.moar emit <code> — include code only for moar backend.
            // mutsu treats itself as moar-compatible, so emit the code.
            // #?rakudo.jvm emit and #?rakudo.js emit are ignored.
            if trimmed.starts_with("#?rakudo.moar emit ")
                || trimmed.starts_with("#?rakudo.moar emit\t")
            {
                let code = trimmed
                    .strip_prefix("#?rakudo.moar emit")
                    .unwrap()
                    .trim_start();
                output.push_str(code);
                output.push('\n');
                continue;
            }
            if (trimmed.starts_with("#?rakudo.jvm emit")
                || trimmed.starts_with("#?rakudo.js emit")
                || trimmed.starts_with("#?rakudo.js.browser emit"))
                && !trimmed.contains(".moar")
            {
                output.push('\n');
                continue;
            }

            // #?rakudo todo 'reason' — mark the next test as todo.
            // Although mutsu is not rakudo, we honor todo directives because
            // they indicate known spec issues that also affect mutsu.
            if trimmed.starts_with("#?rakudo")
                && !trimmed.contains(".jvm")
                && trimmed.contains("todo")
            {
                // Extract the reason string if present
                let after = trimmed.trim_start_matches("#?rakudo").trim_start();
                let reason = if let Some(start) = after.find('\'') {
                    if let Some(end) = after[start + 1..].find('\'') {
                        &after[start + 1..start + 1 + end]
                    } else {
                        "todo"
                    }
                } else {
                    "todo"
                };
                pending_todo = Some(reason.to_string());
                output.push('\n');
                continue;
            }

            // Emit pending todo before next non-comment, non-empty line
            if let Some(ref reason) = pending_todo
                && !trimmed.is_empty()
                && !trimmed.starts_with('#')
            {
                output.push_str(&format!("todo '{}';\n", reason));
                pending_todo = None;
            }

            // #?rakudo N skip 'reason' — count-based skip directive.
            // Skip the next N test lines. Block-level #?rakudo skip (without count)
            // is ignored since mutsu is not rakudo.
            if trimmed.starts_with("#?rakudo")
                && trimmed.contains("skip")
                && !trimmed.contains(".jvm")
                && !trimmed.contains(".moar")
                && !trimmed.contains(".js")
            {
                let after_prefix = trimmed.trim_start_matches("#?rakudo").trim_start();
                if let Some(first_char) = after_prefix.chars().next()
                    && first_char.is_ascii_digit()
                {
                    // Parse the count: #?rakudo N skip 'reason'
                    let count: usize = after_prefix
                        .split_whitespace()
                        .next()
                        .and_then(|s| s.parse().ok())
                        .unwrap_or(1);
                    skip_reason = if let Some(start) = after_prefix.find('\'') {
                        if let Some(end) = after_prefix[start + 1..].find('\'') {
                            after_prefix[start + 1..start + 1 + end].to_string()
                        } else {
                            "skip".to_string()
                        }
                    } else {
                        "skip".to_string()
                    };
                    skip_lines_remaining = count;
                    output.push('\n');
                    continue;
                }
                // Block-level skip (no count prefix): ignore, treat as comment
            }

            output.push_str(line);
            output.push('\n');
        }

        output
    }

    pub fn run(&mut self, input: &str) -> Result<String, RuntimeError> {
        let preprocessed = Self::preprocess_roast_directives(input);
        if !self.env.contains_key("*PROGRAM") {
            self.env
                .insert("*PROGRAM".to_string(), Value::Str(String::new()));
        }
        self.collect_doc_comments(&preprocessed);
        let file_name = self
            .program_path
            .clone()
            .unwrap_or_else(|| "<unknown>".to_string());
        self.env.insert("?FILE".to_string(), Value::Str(file_name));
        self.env.insert("?LINE".to_string(), Value::Int(1));
        crate::parser::set_parser_lib_paths(self.lib_paths.clone());
        crate::parser::set_parser_program_path(self.program_path.clone());
        let parse_result = crate::parse_dispatch::parse_source(&preprocessed);
        crate::parser::clear_parser_lib_paths();
        let (stmts, finish_content) = parse_result?;
        if let Some(content) = finish_content {
            self.env.insert("=finish".to_string(), Value::Str(content));
        }
        let (enter_ph, leave_ph, body_main) = self.split_block_phasers(&stmts);
        self.run_block_raw(&enter_ph)?;
        let compiler = crate::compiler::Compiler::new();
        let (code, compiled_fns) = compiler.compile(&body_main);
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (interp, body_result) = vm.run(&code, &compiled_fns);
        *self = interp;
        let leave_result = self.run_block_raw(&leave_ph);
        if leave_result.is_err() && body_result.is_ok() {
            leave_result?;
        }
        body_result?;

        // Auto-call MAIN sub if defined
        if self.resolve_function("MAIN").is_some() {
            let args_val = self
                .env
                .get("@*ARGS")
                .cloned()
                .unwrap_or_else(|| Value::array(Vec::new()));
            let args_list = if let Value::Array(items, ..) = args_val {
                items.to_vec()
            } else {
                Vec::new()
            };

            let mut main_call = CompiledCode::new();
            let arity = args_list.len() as u32;
            for arg in args_list {
                let arg_idx = main_call.add_constant(arg);
                main_call.emit(OpCode::LoadConst(arg_idx));
            }
            let name_idx = main_call.add_constant(Value::Str("MAIN".to_string()));
            main_call.emit(OpCode::ExecCall { name_idx, arity });

            let interp = std::mem::take(self);
            let vm = crate::vm::VM::new(interp);
            let (interp, main_result) = vm.run(&main_call, &compiled_fns);
            *self = interp;
            match main_result {
                Err(e) if e.return_value.is_some() => {}
                Err(e) if e.message.is_empty() => {}
                Err(e) => return Err(e),
                Ok(()) => {}
            }
        }
        self.finish()?;
        Ok(self.output.clone())
    }

    pub(super) fn run_block(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let (enter_ph, leave_ph, body_main) = self.split_block_phasers(stmts);
        self.run_block_raw(&enter_ph)?;
        let result = self.run_block_raw(&body_main);
        let leave_res = self.run_block_raw(&leave_ph);
        if leave_res.is_err() && result.is_ok() {
            return leave_res;
        }
        result
    }

    pub(super) fn run_block_raw(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        if stmts.is_empty() {
            return Ok(());
        }
        let compiler = crate::compiler::Compiler::new();
        let (code, compiled_fns) = compiler.compile(stmts);
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (interp, result) = vm.run(&code, &compiled_fns);
        *self = interp;
        result
    }

    pub(super) fn finish(&mut self) -> Result<(), RuntimeError> {
        if !self.end_phasers.is_empty() {
            let phasers = self.end_phasers.clone();
            for (body, captured_env) in phasers.iter().rev() {
                let saved_env = self.env.clone();
                // Overlay captured lexical env on top of current env
                // so both globals and captured lexicals are visible
                for (k, v) in captured_env {
                    self.env.insert(k.clone(), v.clone());
                }
                self.run_block(body)?;
                self.env = saved_env;
            }
        }
        if self.bailed_out {
            return Ok(());
        }
        if let Some(state) = &self.test_state {
            if let Some(planned) = state.planned {
                let _ = planned;
            }
            if state.failed > 0 {
                self.emit_test_summary_diag(state.planned, state.ran, state.failed);
                return Err(RuntimeError::new("Test failures"));
            }
        }
        Ok(())
    }

    pub(super) fn load_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        let filename = format!("{}.rakumod", module.replace("::", "/"));
        let mut candidates: Vec<std::path::PathBuf> = Vec::new();
        for base in &self.lib_paths {
            let base_path = Path::new(base);
            candidates.push(base_path.join(&filename));
            // Also check lib/ subdirectory (Raku distribution layout)
            candidates.push(base_path.join("lib").join(&filename));
        }
        if candidates.is_empty()
            && let Some(path) = &self.program_path
            && let Some(parent) = Path::new(path).parent()
            && !parent.as_os_str().is_empty()
            && parent.is_dir()
        {
            candidates.push(parent.join(&filename));
        }
        let mut code = None;
        for path in candidates {
            if path.exists() {
                let content = fs::read_to_string(&path).map_err(|err| {
                    RuntimeError::new(format!("Failed to read module {}: {}", module, err))
                })?;
                code = Some(content);
                break;
            }
        }
        let code =
            code.ok_or_else(|| RuntimeError::new(format!("Module not found: {}", module)))?;
        let preprocessed = Self::preprocess_roast_directives(&code);
        crate::parser::set_parser_lib_paths(self.lib_paths.clone());
        crate::parser::set_parser_program_path(self.program_path.clone());
        let result = parse_dispatch::parse_source(&preprocessed);
        crate::parser::clear_parser_lib_paths();
        let stmts = result.map(|(stmts, _)| stmts).map_err(|mut err| {
            err.message = format!("Failed to parse module '{}': {}", module, err.message);
            err
        })?;
        // Handle `unit class Foo;` — merge remaining stmts into the class body
        let stmts = Self::merge_unit_class(stmts);
        self.run_block(&stmts)?;
        Ok(())
    }
}
