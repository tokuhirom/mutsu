use super::*;
use crate::ast::Stmt;

/// Source for builtin parametric roles that are not yet representable as native
/// `RoleDef`s. These are parsed once and prepended to programs that reference
/// them, so they register through the ordinary RoleDecl path.
pub(super) const RATIONAL_ROLE_PRELUDE: &str = r#"
role Rational[::NuT = Int, ::DeT = Int] does Real {
    has NuT $.numerator = 0;
    has DeT $.denominator = 1;
    method new(NuT \nu = 0, DeT \de = 1) {
        my $gcd = (nu gcd de) || 1;
        my $n = nu div $gcd;
        my $d = de div $gcd;
        if $d < 0 { $n = -$n; $d = -$d; }
        self.bless(numerator => NuT.new($n), denominator => DeT.new($d));
    }
    method nude { self.numerator, self.denominator }
    method Bool { self.numerator != 0 }
}
"#;

/// Builtin `Pointer` type for NativeCall. Stored as a plain class with a single
/// `address` attribute; the NativeCall marshalling layer reads/writes that
/// attribute in place (an `is rw Pointer` out-parameter writes the resolved C
/// address back here). The `.gist`/`.Str` form mirrors Rakudo's
/// `NativeCall::Types::Pointer<...>` rendering.
pub(super) const NATIVECALL_POINTER_PRELUDE: &str = r#"
class Pointer {
    has $.address = 0;
    method Int(--> Int) { $!address }
    method Numeric(--> Int) { $!address }
    method Bool(--> Bool) { $!address != 0 }
    method gist(--> Str) {
        $!address == 0
            ?? 'NativeCall::Types::Pointer<NULL>'
            !! sprintf('NativeCall::Types::Pointer<0x%x>', $!address)
    }
    method Str(--> Str) { self.gist }
}
"#;

impl Interpreter {
    pub fn run(&mut self, input: &str) -> Result<String, RuntimeError> {
        let preprocessed = Self::maybe_preprocess_roast_directives(input);
        if !self.env.contains_key("*PROGRAM") {
            self.env
                .insert("*PROGRAM".to_string(), Value::str(String::new()));
        }
        self.collect_doc_comments(&preprocessed);
        self.collect_pod_blocks(&preprocessed);
        self.add_declarator_pod_entries();
        let file_name = self
            .program_path
            .clone()
            .unwrap_or_else(|| "<unknown>".to_string());
        self.env.insert("?FILE".to_string(), Value::str(file_name));
        self.env.insert("?LINE".to_string(), Value::Int(1));
        crate::parser::set_parser_lib_paths(self.lib_paths.clone());
        crate::parser::set_parser_program_path(self.program_path.clone());
        let parse_result = crate::parse_dispatch::parse_source(&preprocessed);
        crate::parser::clear_parser_lib_paths();
        // Emit any parse warnings (e.g. duplicate traits)
        for warning in crate::parser::take_parse_warnings() {
            self.write_warn_to_stderr(&warning);
        }
        let (mut stmts, finish_content) = parse_result?;
        if let Some(content) = finish_content {
            self.env.insert("=finish".to_string(), Value::str(content));
        }
        // Inject builtin prelude role definitions (e.g. the parametric `Rational`
        // role) when the program references them. These are registered through the
        // normal RoleDecl/VM path by prepending their statements to the body.
        Self::inject_prelude_roles(&preprocessed, &mut stmts);
        Self::inject_nativecall_prelude(&preprocessed, &mut stmts);
        let (_pre_ph, enter_ph, success_ph, failure_ph, _post_ph, body_main) =
            self.split_block_phasers(&stmts);
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
        // Reorder phasers: BEGIN (forward), CHECK (reverse), INIT (forward)
        // are moved before the main body within each block scope.
        let mut body_main = body_main;
        // Re-insert ENTER phasers into body_main so they are compiled and
        // executed by the VM instead of the legacy interpreter.  Running them
        // through the interpreter would use a separate variable scope, causing
        // ENTER body assignments to not be visible to the rest of the mainline.
        if !enter_ph.is_empty() {
            let enter_stmts: Vec<Stmt> = enter_ph
                .into_iter()
                .map(|s| {
                    if let Stmt::Block(body) = s {
                        Stmt::Phaser {
                            kind: crate::ast::PhaserKind::Enter,
                            body,
                        }
                    } else {
                        s
                    }
                })
                .collect();
            body_main.splice(0..0, enter_stmts);
        }
        crate::runtime::phasers::reorder_phasers(&mut body_main);
        self.update_raku_version_from_parser();
        self.check_eval_param_type_constraints(&body_main)?;
        self.preregister_top_level_subs(&body_main)?;
        let mut compiler = crate::compiler::Compiler::new();
        compiler.set_current_package(self.current_package());
        compiler.is_mainline = true;
        let (code, compiled_fns) = compiler.compile(&body_main);
        // Seed the escaping-our-sub lexical names from the compiled top-level code
        // (and its nested closures), so a free-variable read inside such an `our`
        // sub resolves through the persisted shared cell — known BEFORE the
        // declaring block runs. See `escaping_our_lexical_names`.
        self.collect_escaping_our_lexical_names(&code);
        // CP-3 collapse: the Interpreter *is* the bytecode VM now, so run the
        // compiled mainline directly (outermost run → fresh registers) instead of
        // the `mem::take(self)` + `VM::new` + `*self = interp` ping-pong.
        let body_result = self.run_top(&code, &compiled_fns);
        let queue_result = if Self::should_run_success_queue_vm(&body_result, self.env.get("_")) {
            self.run_block_raw(&success_ph)
        } else {
            self.run_block_raw(&failure_ph)
        };
        if queue_result.is_err() && body_result.is_ok() {
            queue_result?;
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

        // Check for unresolved package/class stubs (X::Package::Stubbed)
        self.check_unresolved_stubs()?;

        // Auto-call MAIN sub if defined, with CLI argument parsing
        self.dispatch_main(&compiled_fns)?;
        self.finish()?;
        Ok(self.output_sink().output.clone())
    }

    pub(super) fn run_block(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let (pre_ph, enter_ph, success_ph, failure_ph, post_ph, body_main) =
            self.split_block_phasers(stmts);
        // Run PRE phasers (before ENTER)
        for pre in &pre_ph {
            let result = self.eval_block_value(std::slice::from_ref(pre))?;
            if !result.truthy() {
                return Err(Self::make_phaser_prepost_error(true));
            }
        }
        self.run_block_raw(&enter_ph)?;
        let body_result = self.run_block_raw(&body_main);
        let queue_res = if Self::should_run_success_queue_raw(&body_result, self.env.get("_")) {
            self.run_block_raw(&success_ph)
        } else {
            self.run_block_raw(&failure_ph)
        };
        // Run POST phasers (after LEAVE, in reverse source order)
        // Set $_ to the return value so POST can check it
        if !post_ph.is_empty() {
            let ret_val = match &body_result {
                Ok(()) => self.env.get("_").cloned().unwrap_or(Value::Nil),
                Err(e) => e.return_value.clone().unwrap_or(Value::Nil),
            };
            let saved_topic = self.env.get("_").cloned();
            self.env.insert("_".to_string(), ret_val);
            let post_res = self.run_post_phasers(&post_ph);
            if let Some(t) = saved_topic {
                self.env.insert("_".to_string(), t);
            }
            if post_res.is_err() && body_result.is_ok() && queue_res.is_ok() {
                return post_res;
            }
            // POST failure should override a successful return but not a body error
            if post_res.is_err()
                && queue_res.is_ok()
                && matches!(&body_result, Err(e) if e.return_value.is_some())
            {
                return post_res;
            }
        }
        if queue_res.is_err() && body_result.is_ok() {
            return queue_res;
        }
        body_result
    }

    /// Create an X::Phaser::PrePost error.
    pub(super) fn make_phaser_prepost_error(is_pre: bool) -> RuntimeError {
        let phaser_name = if is_pre { "PRE" } else { "POST" };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("phaser".to_string(), Value::str(phaser_name.to_string()));
        attrs.insert("condition".to_string(), Value::str(String::new()));
        let exception =
            Value::make_instance(crate::symbol::Symbol::intern("X::Phaser::PrePost"), attrs);
        let mut err = RuntimeError::new(format!("Precondition '{}' failed", phaser_name));
        err.exception = Some(Box::new(exception));
        err
    }

    /// Run POST phasers (already in reverse source order). Each phaser's result
    /// is checked; if falsy, an X::Phaser::PrePost error is returned immediately.
    fn run_post_phasers(&mut self, post_ph: &[Stmt]) -> Result<(), RuntimeError> {
        for post in post_ph {
            let result = self.eval_block_value(std::slice::from_ref(post))?;
            if !result.truthy() {
                return Err(Self::make_phaser_prepost_error(false));
            }
        }
        Ok(())
    }

    /// Compile a raw statement block with the same compiler context as
    /// `run_block_raw` (mainline placeholder scope, current package,
    /// distribution), without executing it. Pure compilation — touches no `env`
    /// and runs no user code — so the VM can call it without an env loan and then
    /// execute the result in-place via `VM::run_nested` (CP-3 collapse PoC),
    /// avoiding the `mem::take`/`VM::new` ping-pong.
    pub(crate) fn compile_block_raw(
        &self,
        stmts: &[Stmt],
    ) -> (
        crate::opcode::CompiledCode,
        std::collections::HashMap<String, crate::opcode::CompiledFunction>,
    ) {
        let mut compiler = crate::compiler::Compiler::new();
        compiler.is_routine = !self.routine_stack.is_empty();
        compiler.lexically_in_routine = !self.routine_stack.is_empty();
        // The outermost compilation unit is the mainline: placeholder variables
        // ($^x, @_, ...) appearing here are outside any sub or block.
        compiler.is_mainline = self.routine_stack.is_empty();
        compiler.set_current_package(self.current_package());
        // `$?PACKAGE`/`$?MODULE` resolve to the *lexical* package the routine was
        // declared in, not `current_package` (which is reset to GLOBAL during a
        // method body re-compile to avoid falsely qualifying class-level `my`
        // vars). Seed `enclosing_package` from the active routine frame's clean
        // package name — matching `compile_block_value` — so a re-compiled method
        // body (e.g. a role submethod invoked via WALK) keeps the correct
        // `$?PACKAGE`. `enclosing_package` only feeds `$?PACKAGE`/`$?MODULE`, so
        // this does not affect call/variable qualification.
        if let Some(frame) = self.routine_stack.last() {
            compiler.enclosing_package = Some(frame.package.clone());
        }
        // Resolve distribution context: prefer the current one, then look up
        // by the current package name in case we're running a function body
        // from a module that had a distribution.
        compiler.current_distribution = self.current_distribution.clone().or_else(|| {
            self.package_distributions
                .get(&self.current_package())
                .cloned()
        });
        compiler.compile(stmts)
    }

    pub(crate) fn run_block_raw(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        if stmts.is_empty() {
            return Ok(());
        }
        let (code, compiled_fns) = self.compile_block_raw(stmts);
        // CP-3 collapse: run the compiled block re-entrantly in place (no
        // `mem::take(self)` + `VM::new` ping-pong), exactly like the VM-side
        // `vm_run_block_raw`. `run_nested` saves/resets/restores the per-execution
        // registers and flags env_dirty so the outer locals re-sync from env.
        let result = self.run_nested(&code, &compiled_fns);
        // Slice F (env<->locals coherence): a deferred class/role body that
        // mutates an outer lexical (`class C { $tracker = 99 }`) writes it into
        // `env` by name; record those names so the caller (the class/role
        // registration opcode, which holds the outer `code`) writes them through
        // to the outer frame's local slots, dropping the dependency on the
        // reverse pull. The topic is excluded as a per-call alias. Mirrors the
        // VM-side `vm_run_block_raw`.
        // A class/role body runs with `current_package` set to the package name,
        // so a write to an outer lexical `$tracker` is recorded in
        // `free_var_writes` as the qualified `Pkg::tracker` even though the
        // caller's local slot (and the copied-back env entry) is the bare
        // `tracker`. Strip the active package prefix so the drain matches the
        // caller slot.
        let pkg_prefix = {
            let pkg = self.current_package();
            if pkg == "GLOBAL" {
                String::new()
            } else {
                format!("{pkg}::")
            }
        };
        for sym in &code.free_var_writes {
            sym.with_str(|fname| {
                let unqualified = if !pkg_prefix.is_empty() {
                    fname.strip_prefix(&pkg_prefix).unwrap_or(fname)
                } else {
                    fname
                };
                if unqualified != "_" && unqualified != "@_" && unqualified != "%_" {
                    self.pending_rw_writeback_sources
                        .push(unqualified.to_string());
                }
            });
        }
        self.run_pending_instance_destroys()?;
        result.map(|_| ())
    }

    pub(super) fn finish(&mut self) -> Result<(), RuntimeError> {
        // Drain any output produced by fire-and-forget `start` threads that was
        // never collected by an `await` (those drain on join). A `start` block
        // that ran to a `say` and then blocked/exited still left its text in the
        // shared buffer; at program exit it must reach real stdout, matching Raku
        // where completed thread output is not lost.
        self.drain_shared_thread_output();
        if !self.end_phasers.is_empty() {
            // Clear halted flag so END phasers can execute even after exit()
            self.halted = false;
            let phasers = self.end_phasers.clone();
            // Save the env state before any END phasers run.  This lets us
            // distinguish between variables set by prior END phasers (which
            // should propagate) and stale captured values (which should not
            // override live values).
            let original_env = self.env.clone();
            for (body, captured_env) in phasers.iter().rev() {
                // Track which keys are being added from captured env
                // (not already present) so we can remove them after.
                let mut overlay_keys: Vec<String> = Vec::new();
                // Overlay captured lexical env on top of current env.
                // For keys already in the current env, only overlay if the
                // key was NOT in the original env (meaning it was injected
                // by a prior END phaser's overlay, not a live program
                // variable).  This preserves live values and prior-END
                // modifications while still providing access to captured
                // lexicals from dead scopes.
                for (k, v) in captured_env.iter() {
                    if !self.env.contains_key_sym(*k) {
                        overlay_keys.push(k.resolve());
                        self.env.insert_sym(*k, v.clone());
                    } else if let Some(orig_v) = original_env.get_sym(*k) {
                        // Key exists in both current and original env.
                        // Overlay with captured value only if the captured
                        // value differs from the original — this indicates
                        // the captured value comes from a different lexical
                        // scope (e.g. { my $a = 42; END { ... } }).
                        if v != orig_v {
                            self.env.insert_sym(*k, v.clone());
                        }
                        // If captured == original, it's the same variable —
                        // keep the current (possibly mutated) value.
                    } else {
                        // Key was added by a prior END phaser's overlay.
                        // Override with this phaser's captured value.
                        self.env.insert_sym(*k, v.clone());
                    }
                }
                self.run_block(body)?;
                // Remove only the overlay keys (captured lexicals not in
                // the current scope), keeping mutations to shared variables.
                for k in &overlay_keys {
                    self.env.remove(k);
                }
            }
        }
        self.run_pending_instance_destroys()?;
        // Print deprecation report to stderr at program exit
        if let Some(report) = super::deprecation::take_report() {
            self.output_sink_mut().stderr_output.push_str(&report);
            self.output_sink_mut().stderr_output.push('\n');
        }
        if self.tap.bailed_out() {
            return Ok(());
        }
        if let Some(state) = self.tap.state() {
            // Use the shared-atomic-aware count: tests run on a spawned thread
            // (start blocks, Promise callbacks) bump the shared counter but not
            // this state's local `ran` field.
            let ran = state.effective_ran();
            let plan_mismatch = matches!(state.planned, Some(planned) if planned != ran);
            if state.failed > 0 {
                self.emit_test_summary_diag(state.planned, ran, state.failed);
                return Err(RuntimeError::new("Test failures"));
            }
            if plan_mismatch {
                if let Some(planned) = state.planned {
                    self.output_sink_mut().stderr_output.push_str(&format!(
                        "# You planned {} test, but ran {}\n",
                        planned, ran
                    ));
                }
                // Dubious: exit code 255
                self.exit_code = 255;
                return Err(RuntimeError::new("Test failures"));
            }
        }
        Ok(())
    }

    fn should_run_success_queue_raw(
        body_result: &Result<(), RuntimeError>,
        current_topic: Option<&Value>,
    ) -> bool {
        match body_result {
            Ok(()) => current_topic.cloned().unwrap_or(Value::Nil).truthy(),
            Err(e) if !Self::is_exceptional_block_exit(e) => {
                e.return_value.clone().unwrap_or(Value::Nil).truthy()
            }
            Err(_) => false,
        }
    }

    fn should_run_success_queue_vm(
        body_result: &Result<Option<Value>, RuntimeError>,
        current_topic: Option<&Value>,
    ) -> bool {
        match body_result {
            Ok(value) => value
                .clone()
                .or_else(|| current_topic.cloned())
                .unwrap_or(Value::Nil)
                .truthy(),
            Err(e) if !Self::is_exceptional_block_exit(e) => {
                e.return_value.clone().unwrap_or(Value::Nil).truthy()
            }
            Err(_) => false,
        }
    }
}
