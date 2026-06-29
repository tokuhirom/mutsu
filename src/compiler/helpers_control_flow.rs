use super::*;
use crate::ast::CallArg;

impl Compiler {
    /// Check if the body uses @_ or %_ legacy argument variables.
    pub(super) fn body_uses_legacy_args(body: &[Stmt]) -> bool {
        let body_str = format!("{:?}", body);
        body_str.contains("\"@_\"") || body_str.contains("\"%_\"")
    }

    pub(super) fn is_definite_return_spec(spec: &str) -> bool {
        let s = spec.trim();
        if s.is_empty() {
            return false;
        }
        if s.starts_with('$')
            || s.starts_with('\"')
            || s.starts_with('\'')
            || s.chars().next().is_some_and(|c| c.is_ascii_digit())
            || (s.starts_with('-') && s[1..].chars().next().is_some_and(|c| c.is_ascii_digit()))
        {
            return true;
        }
        matches!(s, "Nil" | "True" | "False" | "Empty" | "pi" | "e" | "tau")
            || s.chars().next().is_some_and(|c| c.is_ascii_lowercase())
    }

    pub(super) fn emit_nil_value(&mut self) {
        let nil_idx = self.code.add_constant(Value::Nil);
        self.code.emit(OpCode::LoadConst(nil_idx));
    }

    pub(super) fn compile_stmts_value(&mut self, stmts: &[Stmt]) {
        let saved = self.push_dynamic_scope_lexical();
        if stmts.is_empty() {
            self.emit_nil_value();
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        // If the block contains CATCH/CONTROL, wrap in implicit try so
        // exceptions are handled (any Raku block can act as a try block).
        if Self::has_catch_or_control(stmts) {
            self.compile_try(stmts, &None);
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            if is_last {
                match stmt {
                    Stmt::Expr(expr) => self.compile_expr(expr),
                    Stmt::If {
                        cond,
                        then_branch,
                        else_branch,
                        binding_var,
                    } if binding_var.is_none() => {
                        self.compile_if_value(cond, then_branch, else_branch)
                    }
                    Stmt::Block(inner) | Stmt::SyntheticBlock(inner) => {
                        self.compile_block_inline(inner)
                    }
                    Stmt::VarDecl { name, .. } => {
                        self.compile_stmt(stmt);
                        // VarDecl returns the variable value (like Raku)
                        if let Some(&slot) = self.local_map.get(name) {
                            self.code.emit(OpCode::GetLocal(slot));
                        } else {
                            self.emit_nil_value();
                        }
                    }
                    // A `given` in branch-final position must yield its value
                    // (the Given statement leaves it on the stack), just like in
                    // a `do {}` block (see `compile_block_inline`). This keeps
                    // `if $c { given $v { ... } }` and statement-form `with $v {
                    // ... }` (lowered to `if { given }`) value-producing instead
                    // of falling through to Nil.
                    Stmt::Given { .. } => {
                        self.compile_stmt(stmt);
                    }
                    _ => {
                        self.compile_stmt(stmt);
                        self.emit_nil_value();
                    }
                }
            } else {
                self.compile_stmt(stmt);
            }
        }
        self.pop_dynamic_scope_lexical(saved);
    }

    pub(super) fn compile_if_value(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
    ) {
        // Check for heredoc scope violations before compiling
        if let Some(err) = self.check_heredoc_scope_errors(then_branch) {
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        if let Some(err) = self.check_heredoc_scope_errors(else_branch) {
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        let needs_at_underscore = Self::body_uses_legacy_args(then_branch);
        self.compile_expr(cond);
        if needs_at_underscore {
            // Duplicate condition for @_ (bare if blocks receive condition as @_).
            self.code.emit(OpCode::Dup);
        }
        let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
        if needs_at_underscore {
            // Flatten the duplicated condition into @_.
            self.code.emit(OpCode::FlattenSlurpy);
            self.emit_set_named_var("@_");
        }
        self.compile_stmts_value(then_branch);
        let jump_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(jump_else);
        if needs_at_underscore {
            // Pop leftover duplicated condition on the false branch.
            self.code.emit(OpCode::Pop);
        }
        if else_branch.is_empty() {
            let empty_idx = self.code.add_constant(Value::slip(vec![]));
            self.code.emit(OpCode::LoadConst(empty_idx));
        } else {
            self.compile_stmts_value(else_branch);
        }
        self.code.patch_jump(jump_end);
    }

    /// Check if a list of statements contains a CATCH or CONTROL block.
    pub(super) fn has_catch_or_control(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Catch(_) | Stmt::Control(_)))
    }

    /// The env key for the container variable an element subscript targets, used
    /// when topicalizing an element (`given %h<k>` / `given @a[i]`) so the
    /// mutated `$_` can be written back. Returns `%name` for a hash variable,
    /// `@name` for an array variable, and the bare name for a scalar variable
    /// (holding a container). `None` for any other (non-simple-var) target.
    pub(super) fn container_var_name(target: &Expr) -> Option<String> {
        match target {
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::Var(name) => Some(name.clone()),
            _ => None,
        }
    }

    pub(super) fn body_mutates_topic(stmts: &[Stmt]) -> bool {
        // Only check if the *first* non-SetLine statement assigns `$_` directly.
        // This detects `with`-style topic switches (where the parser inserts a
        // `$_ = <expr>` as the first statement of the then-branch) but avoids
        // falsely triggering on user code like `if COND { ...; $_ = 2 }` where
        // the assignment is NOT at the start. The Block wrapping created when
        // this returns true causes BlockScope to save/restore `$_`, isolating
        // the `with` topic from the outer scope.
        let first_real = stmts.iter().find(|s| !matches!(s, Stmt::SetLine(_)));
        matches!(first_real, Some(Stmt::Assign { name, .. }) if name == "_")
    }

    /// Returns true only if the body contains a `$_ :=` rebind — used by the
    /// `while` loop to decide whether to wrap the body in a `Stmt::Block` so
    /// that the rebind is lexically scoped per iteration, without clobbering
    /// the outer topic. Unlike `body_mutates_topic`, plain `$_ =` (assignment)
    /// does NOT trigger Block wrapping for while loops.
    pub(super) fn body_rebinds_topic(stmts: &[Stmt]) -> bool {
        fn expr_rebinds_topic(expr: &Expr) -> bool {
            match expr {
                Expr::AssignExpr { name, is_bind, .. } => name == "_" && *is_bind,
                Expr::Unary { expr, .. } => expr_rebinds_topic(expr),
                Expr::Binary { left, right, .. } => {
                    expr_rebinds_topic(left) || expr_rebinds_topic(right)
                }
                Expr::MethodCall { target, args, .. } => {
                    expr_rebinds_topic(target) || args.iter().any(expr_rebinds_topic)
                }
                Expr::Call { args, .. } => args.iter().any(expr_rebinds_topic),
                _ => false,
            }
        }

        fn stmt_rebinds_topic(stmt: &Stmt) -> bool {
            match stmt {
                Stmt::Assign { name, op, .. } => {
                    name == "_" && matches!(op, crate::ast::AssignOp::Bind)
                }
                Stmt::Expr(expr) => expr_rebinds_topic(expr),
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    super::Compiler::body_rebinds_topic(then_branch)
                        || super::Compiler::body_rebinds_topic(else_branch)
                }
                Stmt::While { body, .. }
                | Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::Catch(body)
                | Stmt::Control(body)
                | Stmt::When { body, .. }
                | Stmt::Given { body, .. }
                | Stmt::Default(body) => super::Compiler::body_rebinds_topic(body),
                Stmt::For { body, .. } => super::Compiler::body_rebinds_topic(body),
                _ => false,
            }
        }

        stmts.iter().any(stmt_rebinds_topic)
    }

    /// Returns true if a branch body declares a block-local `my` variable
    /// directly in its top-level statement list. Such a declaration shadows an
    /// enclosing same-named binding and, without scoping, would *clobber* it
    /// (`my $x=99; if c { my $x=5 }; say $x` would wrongly print `5`). When true,
    /// the `if`/`unless`/`else` branch is wrapped in a `BlockLocalScope` so the
    /// loop bodies' shadow-only restore re-exposes the outer binding on exit.
    ///
    /// Descends into `SyntheticBlock` (the parser's inlined wrapper for
    /// destructuring `my ($a, $b) = ...`, which is NOT a separate scope) but not
    /// into nested `Block`/`if`/loops/subs, which introduce their own scopes.
    /// `state`/`our`/dynamic declarations are excluded: they are not plain
    /// lexical shadows and have their own scoping/restore rules.
    pub(super) fn branch_declares_block_local(stmts: &[Stmt]) -> bool {
        stmts.iter().any(|s| match s {
            Stmt::VarDecl {
                is_state,
                is_our,
                is_dynamic,
                ..
            } => !*is_state && !*is_our && !*is_dynamic,
            Stmt::SyntheticBlock(inner) => Self::branch_declares_block_local(inner),
            _ => false,
        })
    }

    /// Compile an `if`/`unless`/`else` branch body wrapped in a `BlockLocalScope`
    /// opcode (see `branch_declares_block_local`). The opcode runs the body once
    /// under the loop bodies' shadow-only restore, fixing the body-local `my`
    /// clobber without the full env restore of `BlockScope` (which would revert a
    /// `:=` binding the branch makes to an outer variable).
    pub(super) fn compile_block_local_branch(&mut self, stmts: &[Stmt]) {
        let idx = self.code.emit(OpCode::BlockLocalScope { body_end: 0 });
        self.compile_body_with_implicit_try(stmts);
        self.code.patch_block_local_body_end(idx);
    }

    /// Compile a block body, automatically wrapping in implicit try if it contains
    /// CATCH or CONTROL blocks. This should be used for any block context (bare blocks,
    /// if branches, loop bodies, sub bodies) to ensure CATCH/CONTROL are not silently ignored.
    pub(super) fn compile_body_with_implicit_try(&mut self, stmts: &[Stmt]) {
        let saved = self.push_dynamic_scope_lexical();
        if Self::has_catch_or_control(stmts) {
            self.compile_try(stmts, &None);
            self.code.emit(OpCode::Pop);
        } else {
            for s in stmts {
                self.compile_stmt(s);
            }
        }
        self.pop_dynamic_scope_lexical(saved);
    }

    /// Compile Expr::Try { body, catch } to TryCatch opcode.
    pub(super) fn compile_try(&mut self, body: &[Stmt], catch: &Option<Vec<Stmt>>) {
        let saved = self.push_dynamic_scope_lexical();
        // Detect duplicate CATCH/CONTROL phasers in the same block: Raku
        // requires at most one of each per block (X::Phaser::Multiple).
        let catch_count = body.iter().filter(|s| matches!(s, Stmt::Catch(_))).count();
        let control_count = body
            .iter()
            .filter(|s| matches!(s, Stmt::Control(_)))
            .count();
        if catch_count > 1 || control_count > 1 {
            let kind = if catch_count > 1 { "CATCH" } else { "CONTROL" };
            let msg = format!("Only one {} block is allowed per block", kind);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg));
            attrs.insert("block".to_string(), Value::str(kind.to_string()));
            let exc =
                Value::make_instance(crate::symbol::Symbol::intern("X::Phaser::Multiple"), attrs);
            let idx = self.code.add_constant(exc);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        // Separate CATCH/CONTROL blocks from body.
        let mut main_stmts = Vec::new();
        let mut catch_stmts = catch.clone();
        let mut control_stmts: Option<Vec<Stmt>> = None;
        for stmt in body {
            if let Stmt::Catch(catch_body) = stmt {
                catch_stmts = Some(catch_body.clone());
            } else if let Stmt::Control(control_body) = stmt {
                control_stmts = Some(control_body.clone());
            } else {
                main_stmts.push(stmt.clone());
            }
        }
        let has_explicit_catch = catch_stmts.is_some();
        let resume_safe = control_stmts
            .as_deref()
            .map(Self::control_block_is_resume_safe)
            .unwrap_or(false);
        // Emit TryCatch placeholder. Mark it a bare-block callframe only when the
        // `Stmt::Block` arm requested it for a genuine source `{ ...; CATCH { } }`.
        let is_bare_block = std::mem::take(&mut self.next_try_is_bare_block);
        let try_idx = self.code.emit(OpCode::TryCatch {
            catch_start: 0,
            control_start: 0,
            body_end: 0,
            explicit_catch: has_explicit_catch,
            resume_safe,
            is_bare_block,
        });
        // Compile main body (last Stmt::Expr/Call leaves value on stack)
        let mut main_leaves_value = false;
        if Self::has_block_enter_leave_phasers(&main_stmts) {
            self.synthetic_block_body = true;
            self.compile_stmt(&Stmt::Block(main_stmts.clone()));
            self.compile_expr(&Expr::Var("_".to_string()));
            main_leaves_value = true;
        } else {
            for (i, stmt) in main_stmts.iter().enumerate() {
                let is_last = i == main_stmts.len() - 1;
                // Keep the final expression's value on the stack so the try
                // block evaluates to it (the value of a `do`/sub/closure body).
                // This holds even with an explicit CATCH block: on the success
                // path Raku still yields the last expression. compile_try always
                // leaves exactly one value (LoadNil below when none), so stack
                // discipline is unchanged for statement-context callers.
                if is_last {
                    if let Stmt::Expr(expr) = stmt {
                        self.compile_expr(expr);
                        main_leaves_value = true;
                        continue;
                    } else if let Stmt::Call { name, args } = stmt {
                        let rewritten_args = Self::rewrite_stmt_call_args(&name.resolve(), args);
                        let positional_only = rewritten_args
                            .iter()
                            .all(|arg| matches!(arg, CallArg::Positional(_)));

                        if positional_only {
                            let expr_args: Vec<Expr> = rewritten_args
                                .iter()
                                .filter_map(|arg| match arg {
                                    CallArg::Positional(expr) => Some(expr.clone()),
                                    _ => None,
                                })
                                .collect();
                            self.compile_expr(&Expr::Call {
                                name: *name,
                                args: expr_args,
                            });
                            main_leaves_value = true;
                            continue;
                        }

                        let has_slip = rewritten_args
                            .iter()
                            .any(|arg| matches!(arg, CallArg::Slip(_)));
                        if has_slip {
                            let mut regular_count = 0u32;
                            let mut slip_pos: Option<u32> = None;
                            let mut stack_idx = 0u32;
                            for arg in &rewritten_args {
                                match arg {
                                    CallArg::Slip(expr) => {
                                        slip_pos = Some(stack_idx);
                                        self.compile_expr(expr);
                                        stack_idx += 1;
                                    }
                                    CallArg::Positional(expr) => {
                                        self.compile_call_arg(expr);
                                        regular_count += 1;
                                        stack_idx += 1;
                                    }
                                    CallArg::Named {
                                        name: n,
                                        value: Some(expr),
                                    } => {
                                        self.compile_expr(&Expr::Literal(Value::str(n.clone())));
                                        self.compile_expr(expr);
                                        self.code.emit(OpCode::MakePair);
                                        regular_count += 1;
                                        stack_idx += 1;
                                    }
                                    CallArg::Named {
                                        name: n,
                                        value: None,
                                    } => {
                                        self.compile_expr(&Expr::Literal(Value::str(n.clone())));
                                        self.compile_expr(&Expr::Literal(Value::Bool(true)));
                                        self.code.emit(OpCode::MakePair);
                                        regular_count += 1;
                                        stack_idx += 1;
                                    }
                                    CallArg::Invocant(_) => {}
                                }
                            }
                            let name_idx = self.code.add_constant(Value::str(name.resolve()));
                            self.code.emit(OpCode::CallFuncSlip {
                                name_idx,
                                regular_arity: regular_count,
                                arg_sources_idx: None,
                                slip_pos,
                            });
                            main_leaves_value = true;
                            continue;
                        }

                        for arg in &rewritten_args {
                            match arg {
                                CallArg::Positional(expr) => self.compile_call_arg(expr),
                                CallArg::Named {
                                    name,
                                    value: Some(expr),
                                } => {
                                    self.compile_expr(&Expr::Literal(Value::str(name.clone())));
                                    self.compile_expr(expr);
                                    self.code.emit(OpCode::MakePair);
                                }
                                CallArg::Named { name, value: None } => {
                                    self.compile_expr(&Expr::Literal(Value::str(name.clone())));
                                    self.compile_expr(&Expr::Literal(Value::Bool(true)));
                                    self.code.emit(OpCode::MakePair);
                                }
                                CallArg::Slip(_) | CallArg::Invocant(_) => unreachable!(),
                            }
                        }
                        let name_idx = self.code.add_constant(Value::str(name.resolve()));
                        let arg_sources_idx = rewritten_args
                            .iter()
                            .map(|arg| match arg {
                                CallArg::Positional(expr) => Some(expr),
                                _ => None,
                            })
                            .collect::<Option<Vec<&Expr>>>()
                            .and_then(|exprs| {
                                let owned: Vec<Expr> = exprs.into_iter().cloned().collect();
                                self.add_arg_sources_constant(&owned)
                            });
                        self.code.emit(OpCode::CallFunc {
                            name_idx,
                            arity: rewritten_args.len() as u32,
                            arg_sources_idx,
                        });
                        main_leaves_value = true;
                        continue;
                    }
                }
                self.compile_stmt(stmt);
            }
        }
        if !main_leaves_value {
            self.code.emit(OpCode::LoadNil);
        }
        // A trailing unhandled Failure *value* on the stack is thrown into this
        // block/routine's CATCH (or `try`) handler — `ThrowIfFailure` peeks and
        // keeps the value so a normal trailing value is still the result. This
        // matches Raku for both blocks (`try { @a.elems; CATCH {...} }`) and
        // routines (`sub { s2(); CATCH {...} }` where `s2` returns a Failure).
        // A direct `fail` raises a control signal (not a stack value) handled by
        // the routine boundary, so it is unaffected and still returned.
        self.code.emit(OpCode::ThrowIfFailure);
        // Jump over catch/control on success.
        let jump_end = self.code.emit(OpCode::Jump(0));
        // Patch catch_start.
        self.code.patch_try_catch_start(try_idx);
        // Compile catch block.
        let mut jump_after_catch = None;
        if let Some(ref catch_body) = catch_stmts {
            // If the catch body itself contains a nested CATCH/CONTROL,
            // wrap it in an implicit try so exceptions thrown inside the
            // outer CATCH can be handled by the nested CATCH.
            if Self::has_catch_or_control(catch_body) {
                self.compile_try(catch_body, &None);
                self.code.emit(OpCode::Pop);
            } else {
                for stmt in catch_body {
                    self.compile_stmt(stmt);
                }
            }
            if control_stmts.is_some() {
                jump_after_catch = Some(self.code.emit(OpCode::Jump(0)));
            }
        }
        // catch result is Nil
        self.code.emit(OpCode::LoadNil);
        // Patch control_start.
        self.code.patch_try_control_start(try_idx);
        // Compile control block.
        if let Some(ref control_body) = control_stmts {
            for stmt in control_body {
                self.compile_stmt(stmt);
            }
            // control result is Nil
            self.code.emit(OpCode::LoadNil);
        }
        // Patch body_end and jump targets.
        self.code.patch_try_body_end(try_idx);
        self.code.patch_jump(jump_end);
        if let Some(j) = jump_after_catch {
            self.code.patch_jump(j);
        }
        self.pop_dynamic_scope_lexical(saved);
    }

    /// Classify a CONTROL block as "resume-safe": it always `.resume`s and
    /// never `succeed`s/`when`-exits, so a `warn` caught by it can be handled
    /// *inline* at the deep raise site (see `Interpreter::builtin_warn`) without
    /// unwinding the Rust call stack — the mechanism behind cross-frame
    /// resumable warns. Conservative: anything it does not recognise → false
    /// (falls back to the existing unwinding path).
    fn control_block_is_resume_safe(stmts: &[Stmt]) -> bool {
        let meaningful: Vec<&Stmt> = stmts
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        if meaningful.is_empty() {
            return false;
        }
        // A single `default { ... }` delegates to its body — the common
        // `CONTROL { default { ...; .resume } }` shape.
        if meaningful.len() == 1
            && let Stmt::Default(body) = meaningful[0]
        {
            return Self::control_block_is_resume_safe(body);
        }
        // Any `when` arm or `succeed` escapes the block via a control signal
        // (it does NOT resume) — the #3372 killer case. Reject.
        if meaningful.iter().any(|s| Self::stmt_exits_control_block(s)) {
            return false;
        }
        // The tail statement must be a `.resume` method call.
        matches!(meaningful.last(), Some(Stmt::Expr(e)) if Self::expr_is_resume_call(e))
    }

    fn expr_is_resume_call(e: &Expr) -> bool {
        matches!(e, Expr::MethodCall { name, .. } if name.resolve() == "resume")
    }

    fn stmt_exits_control_block(s: &Stmt) -> bool {
        match s {
            Stmt::When { .. } => true,
            Stmt::Call { name, .. } => name.resolve() == "succeed",
            Stmt::Expr(Expr::Call { name, .. }) => name.resolve() == "succeed",
            _ => false,
        }
    }
}
