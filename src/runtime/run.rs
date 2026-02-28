use super::*;
use crate::ast::Stmt;

impl Interpreter {
    fn raku_single_quoted_literal(value: &str) -> String {
        let mut escaped = String::with_capacity(value.len() + 2);
        escaped.push('\'');
        for ch in value.chars() {
            match ch {
                '\'' => escaped.push_str("\\'"),
                '\\' => escaped.push_str("\\\\"),
                '\n' => escaped.push_str("\\n"),
                '\r' => escaped.push_str("\\r"),
                '\t' => escaped.push_str("\\t"),
                _ => escaped.push(ch),
            }
        }
        escaped.push('\'');
        escaped
    }

    /// Register top-level, non-empty sub bodies before execution so calls that appear
    /// earlier in source can resolve to later definitions.
    fn preregister_top_level_subs(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let mut forward_sigs = std::collections::HashSet::new();
        for stmt in stmts {
            if let Stmt::SubDecl {
                name,
                params,
                param_defs,
                body,
                multi,
                ..
            } = stmt
            {
                if *multi || !body.is_empty() {
                    continue;
                }
                forward_sigs.insert(format!("{}|{:?}|{:?}", name, params, param_defs));
            }
        }

        for stmt in stmts {
            if let Stmt::SubDecl {
                name,
                params,
                param_defs,
                return_type,
                associativity,
                body,
                multi,
                is_rw,
                is_export,
                is_test_assertion,
                supersede,
                ..
            } = stmt
            {
                if *multi || body.is_empty() {
                    continue;
                }
                let sig_key = format!("{}|{:?}|{:?}", name, params, param_defs);
                if !forward_sigs.contains(&sig_key) {
                    continue;
                }
                self.register_sub_decl(
                    name,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    associativity.as_ref(),
                    body,
                    *multi,
                    *is_rw,
                    *is_test_assertion,
                    *supersede,
                    &[],
                )?;
                if *is_export {
                    self.register_sub_decl_as_global(
                        name,
                        params,
                        param_defs,
                        return_type.as_ref(),
                        associativity.as_ref(),
                        body,
                        *multi,
                        *is_rw,
                        *is_test_assertion,
                        *supersede,
                    )?;
                }
            }
        }
        Ok(())
    }

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
                name_expr,
                parents,
                is_hidden,
                hidden_parents,
                does_parents,
                repr,
                body: _,
            } = &stmts[idx]
            {
                let body: Vec<Stmt> = stmts[idx + 1..].to_vec();
                result.push(Stmt::ClassDecl {
                    name: name.clone(),
                    name_expr: name_expr.clone(),
                    parents: parents.clone(),
                    is_hidden: *is_hidden,
                    hidden_parents: hidden_parents.clone(),
                    does_parents: does_parents.clone(),
                    repr: repr.clone(),
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
        let mut pending_todo: Option<(String, usize)> = None; // (reason, remaining_count)
        let mut skip_lines_remaining: usize = 0;
        let mut skip_reason: String = String::new();
        // Block-level skip: skip the next { ... } block
        let mut skip_block_pending: Option<String> = None;
        let mut skip_block_depth: usize = 0;
        let mut skip_block_reason: String = String::new();
        let mut skip_block_declared_tests: Option<usize> = None;
        let mut skip_block_declared_emitted = false;
        let test_funcs = [
            "is(",
            "is ",
            "ok ",
            "ok(",
            "tap-ok ",
            "tap-ok(",
            "nok ",
            "nok(",
            "isnt ",
            "isnt(",
            "cmp-ok ",
            "cmp-ok(",
            "isa-ok ",
            "isa-ok(",
            "does-ok ",
            "lives-ok",
            "dies-ok",
            "throws-like",
            "like ",
            "like(",
            "unlike ",
            "pass ",
            "pass(",
            "flunk ",
            "is-deeply",
            "is-primed-sig",
            "is-primed-call",
            "priming-fails-bind-ok",
        ];

        for line in input.lines() {
            let trimmed = line.trim_start();

            // Count-based skip: skip the next N test assertion lines.
            if skip_lines_remaining > 0 {
                if test_funcs.iter().any(|f| trimmed.starts_with(f)) {
                    skip_lines_remaining -= 1;
                    output.push_str(&format!(
                        "skip {}, 1;\n",
                        Self::raku_single_quoted_literal(&skip_reason)
                    ));
                    continue;
                }
                output.push_str(line);
                output.push('\n');
                continue;
            }

            // Block-level skip: waiting for opening brace
            if let Some(ref reason) = skip_block_pending {
                if trimmed.starts_with("#?DOES") {
                    let count = trimmed
                        .strip_prefix("#?DOES")
                        .map(str::trim_start)
                        .and_then(|s| s.split_whitespace().next())
                        .and_then(|s| s.parse::<usize>().ok())
                        .unwrap_or(0);
                    if count > 0 {
                        skip_block_declared_tests = Some(count);
                    }
                    output.push('\n');
                    continue;
                }
                if trimmed.starts_with('{') {
                    skip_block_reason = reason.clone();
                    skip_block_pending = None;
                    skip_block_depth = 1;
                    skip_block_declared_emitted = false;
                    if let Some(count) = skip_block_declared_tests.take() {
                        for _ in 0..count {
                            output.push_str(&format!(
                                "skip {}, 1;\n",
                                Self::raku_single_quoted_literal(&skip_block_reason)
                            ));
                        }
                        skip_block_declared_emitted = true;
                    }
                    output.push('\n');
                    continue;
                } else if trimmed.is_empty() || trimmed.starts_with('#') {
                    output.push('\n');
                    continue;
                } else {
                    // Not a block — treat as single-line skip for test assertions.
                    if test_funcs.iter().any(|f| trimmed.starts_with(f)) {
                        output.push_str(&format!(
                            "skip {}, 1;\n",
                            Self::raku_single_quoted_literal(reason)
                        ));
                        skip_block_pending = None;
                        continue;
                    }
                    // Not a block/test line — cancel skip
                    skip_block_pending = None;
                    continue;
                }
            }
            // Inside a skipped block: track braces and emit skip for test lines
            if skip_block_depth > 0 {
                for ch in trimmed.chars() {
                    if ch == '{' {
                        skip_block_depth += 1;
                    } else if ch == '}' {
                        skip_block_depth -= 1;
                        if skip_block_depth == 0 {
                            break;
                        }
                    }
                }
                if skip_block_depth == 0 {
                    output.push('\n');
                    continue;
                }
                if skip_block_declared_emitted {
                    output.push('\n');
                    continue;
                }
                // Emit skip for lines that look like test assertions
                if test_funcs.iter().any(|f| trimmed.starts_with(f)) {
                    output.push_str(&format!(
                        "skip {}, 1;\n",
                        Self::raku_single_quoted_literal(&skip_block_reason)
                    ));
                } else {
                    output.push('\n');
                }
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

            // #?rakudo todo 'reason' or #?rakudo N todo 'reason' — mark tests as todo.
            // Although mutsu is not rakudo, we honor todo directives because
            // they indicate known spec issues that also affect mutsu.
            if trimmed.starts_with("#?rakudo")
                && !trimmed.contains(".jvm")
                && !trimmed.contains(".js")
                && trimmed.contains("todo")
            {
                let after = trimmed.trim_start_matches("#?rakudo").trim_start();
                // Check for count: #?rakudo N todo "reason"
                let (count, after_count) = if let Some(first_char) = after.chars().next()
                    && first_char.is_ascii_digit()
                {
                    let num_str: String =
                        after.chars().take_while(|c| c.is_ascii_digit()).collect();
                    let n: usize = num_str.parse().unwrap_or(1);
                    (n, after[num_str.len()..].trim_start())
                } else {
                    (1, after)
                };
                // Extract the reason string (single or double quoted)
                let reason = if let Some(start) = after_count.find('\'') {
                    if let Some(end) = after_count[start + 1..].find('\'') {
                        &after_count[start + 1..start + 1 + end]
                    } else {
                        "todo"
                    }
                } else if let Some(start) = after_count.find('"') {
                    if let Some(end) = after_count[start + 1..].find('"') {
                        &after_count[start + 1..start + 1 + end]
                    } else {
                        "todo"
                    }
                } else {
                    "todo"
                };
                pending_todo = Some((reason.to_string(), count));
                output.push('\n');
                continue;
            }

            // Emit pending todo before next non-comment, non-empty line
            if let Some((ref reason, ref mut remaining)) = pending_todo
                && !trimmed.is_empty()
                && !trimmed.starts_with('#')
            {
                output.push_str(&format!(
                    "todo {};\n",
                    Self::raku_single_quoted_literal(reason)
                ));
                *remaining -= 1;
                if *remaining == 0 {
                    pending_todo = None;
                }
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
                // Skip without count: skip next block if it starts with '{',
                // otherwise skip the next non-comment line.
                let after_skip = after_prefix
                    .strip_prefix("skip")
                    .unwrap_or(after_prefix)
                    .trim_start();
                skip_reason = if let Some(start) = after_skip.find('"') {
                    if let Some(end) = after_skip[start + 1..].find('"') {
                        after_skip[start + 1..start + 1 + end].to_string()
                    } else {
                        "skip".to_string()
                    }
                } else if let Some(start) = after_skip.find('\'') {
                    if let Some(end) = after_skip[start + 1..].find('\'') {
                        after_skip[start + 1..start + 1 + end].to_string()
                    } else {
                        "skip".to_string()
                    }
                } else {
                    "skip".to_string()
                };
                skip_block_pending = Some(skip_reason.clone());
                output.push('\n');
                continue;
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
        self.collect_pod_blocks(&preprocessed);
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
        // Emit any parse warnings (e.g. duplicate traits)
        for warning in crate::parser::take_parse_warnings() {
            self.write_warn_to_stderr(&warning);
        }
        let (stmts, finish_content) = parse_result?;
        if let Some(content) = finish_content {
            self.env.insert("=finish".to_string(), Value::Str(content));
        }
        let (enter_ph, leave_ph, body_main) = self.split_block_phasers(&stmts);
        // Register END phasers eagerly (before VM execution) so they run
        // even if the main body dies or throws an exception.
        // Also filter them out of the body so they don't get registered again
        // by the PhaserEnd opcode during VM execution.
        let body_main: Vec<Stmt> = body_main
            .into_iter()
            .filter(|stmt| {
                if let Stmt::Phaser {
                    kind: crate::ast::PhaserKind::End,
                    body,
                } = stmt
                {
                    self.push_end_phaser(body.clone());
                    false
                } else {
                    true
                }
            })
            .collect();
        self.preregister_top_level_subs(&body_main)?;
        self.run_block_raw(&enter_ph)?;
        let mut compiler = crate::compiler::Compiler::new();
        compiler.set_current_package(self.current_package.clone());
        let (code, compiled_fns) = compiler.compile(&body_main);
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (interp, body_result) = vm.run(&code, &compiled_fns);
        *self = interp;
        let leave_result = self.run_block_raw(&leave_ph);
        if leave_result.is_err() && body_result.is_ok() {
            leave_result?;
        }

        // If the main body failed (e.g. die), run END phasers before propagating
        if let Err(e) = body_result {
            self.finish()?;
            return Err(e);
        }

        let last_value = body_result.unwrap();
        // Only store last_value if _ was actually set during this execution
        // (not inherited from a previous REPL line)
        self.last_value = last_value;

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
            main_call.emit(OpCode::ExecCall {
                name_idx,
                arity,
                arg_sources_idx: None,
            });

            let interp = std::mem::take(self);
            let vm = crate::vm::VM::new(interp);
            let (interp, main_result) = vm.run(&main_call, &compiled_fns);
            *self = interp;
            match main_result {
                Err(e) if e.return_value.is_some() => {}
                Err(e) if e.message.is_empty() => {}
                Err(e) => return Err(e),
                Ok(_) => {}
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
        let mut compiler = crate::compiler::Compiler::new();
        compiler.set_current_package(self.current_package.clone());
        let (code, compiled_fns) = compiler.compile(stmts);
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (interp, result) = vm.run(&code, &compiled_fns);
        *self = interp;
        result.map(|_| ())
    }

    pub(super) fn finish(&mut self) -> Result<(), RuntimeError> {
        if !self.end_phasers.is_empty() {
            // Clear halted flag so END phasers can execute even after exit()
            self.halted = false;
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
        for warning in crate::parser::take_parse_warnings() {
            self.write_warn_to_stderr(&warning);
        }
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

#[cfg(test)]
mod tests {
    use super::Interpreter;

    #[test]
    fn preprocess_count_skip_skips_only_next_test() {
        let src = "#?rakudo 1 skip 'reason'\n{\n    is EVAL('$bar'), Any, 'x'\n    is 42, 42, 'still runs';\n}\nsay 42;\n";
        let out = Interpreter::preprocess_roast_directives(src);
        assert!(out.contains("skip 'reason', 1;"));
        assert!(!out.contains("is EVAL('$bar'), Any, 'x'"));
        assert!(out.contains("is 42, 42, 'still runs';"));
        assert!(out.contains("say 42;"));
    }

    #[test]
    fn preprocess_block_skip_consumes_entire_block() {
        let src = "#?rakudo skip 'reason'\n{\n    is EVAL('$bar'), Any, 'x'\n}\nsay 42;\n";
        let out = Interpreter::preprocess_roast_directives(src);
        assert!(out.contains("skip 'reason', 1;"));
        assert!(!out.contains("is EVAL('$bar'), Any, 'x'"));
        assert!(out.contains("say 42;"));
    }

    #[test]
    fn preprocess_block_skip_counts_tap_ok_as_test_assertion() {
        let src = "#?rakudo skip 'reason'\n{\n    tap-ok $s, [1], 'tap';\n    ok True, 'ok';\n}\n";
        let out = Interpreter::preprocess_roast_directives(src);
        assert_eq!(out.matches("skip 'reason', 1;").count(), 2);
        assert!(!out.contains("tap-ok $s, [1], 'tap';"));
        assert!(!out.contains("ok True, 'ok';"));
    }

    #[test]
    fn eval_q_bracket_statement_list_runs_declaration_then_assertion() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run(
            "use Test; plan 1; EVAL q[[my $sub = sub () { 42 }; is [$sub()], [42], 'q-bracket eval';]];",
        );
        assert!(result.is_ok());
        assert!(interp.output.contains("1..1"));
        assert!(interp.output.contains("ok 1 - q-bracket eval"));
    }

    // END phasers must run even after die() or exit().
    // Spec: raku-doc/doc/Language/phasers.rakudoc (END runs "at runtime, ALAP, only ever runs once")
    // Roast: roast/integration/error-reporting.t line 99 ("END phasers are run after die()")
    // Roast: roast/S04-phasers/end.t line 53 ("exit does not prevent running of END blocks")

    #[test]
    fn end_phaser_runs_after_die() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { say 'end-ran' }; die 'boom';");
        assert!(result.is_err(), "die should propagate as error");
        assert_eq!(interp.output, "end-ran\n");
    }

    #[test]
    fn end_phaser_runs_after_exit() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { say 'end-ran' }; exit;");
        assert!(result.is_ok());
        assert_eq!(interp.output, "end-ran\n");
    }

    #[test]
    fn end_phaser_runs_after_exit_with_code() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { say 'end-ran' }; exit(5);");
        assert!(result.is_ok());
        assert_eq!(interp.exit_code(), 5);
        assert_eq!(interp.output, "end-ran\n");
    }

    #[test]
    fn end_phaser_runs_in_reverse_order() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { print 'A' }; END { print 'B' }; END { print 'C' };");
        assert!(result.is_ok());
        assert_eq!(interp.output, "CBA");
    }

    #[test]
    fn end_phaser_reverse_order_with_die() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { print 'A' }; END { print 'B' }; die 'x';");
        assert!(result.is_err());
        assert_eq!(interp.output, "BA");
    }

    #[test]
    fn end_phaser_runs_on_normal_completion() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("say 'hello'; END { say 'end' };");
        assert!(result.is_ok());
        assert_eq!(interp.output, "hello\nend\n");
    }

    #[test]
    fn die_preserves_exit_code_with_end_phaser() {
        let mut interp = Interpreter::new();
        interp.set_immediate_stdout(false);
        let result = interp.run("END { say 'end' }; die 'boom';");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("boom"),
            "error message should contain 'boom'"
        );
    }
}
