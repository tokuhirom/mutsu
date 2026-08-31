use super::*;
use crate::ast::CallArg;

impl Compiler {
    /// Check if the body uses @_ or %_ legacy argument variables.
    pub(super) fn body_uses_legacy_args(body: &[Stmt]) -> bool {
        let body_str = format!("{:?}", body);
        body_str.contains("\"@_\"") || body_str.contains("\"%_\"")
    }

    /// Whether `--> spec` names a **definite return value** (a literal or a
    /// lowercase term the sub returns regardless of its body, as in Rakudo's own
    /// `sub refresh($obj --> 1)`) rather than a return *type*. A definite return
    /// makes the body's last expression sink.
    ///
    /// Lowercase alone cannot decide it: Raku's native types are lowercase too,
    /// so `sub f($x --> ulong) { $x }` was read as "return the term `ulong`",
    /// sank the body and answered Nil for every one of `int`/`num`/`str` and the
    /// `NativeCall::Types` C-width aliases. Native type names are therefore
    /// excluded, which is what `is_known_type_constraint` decides.
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
            || (s.chars().next().is_some_and(|c| c.is_ascii_lowercase())
                && !crate::runtime::utils::is_known_type_constraint(s))
    }

    pub(super) fn emit_nil_value(&mut self) {
        let nil_idx = self.code.add_constant(Value::NIL);
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
            self.compile_implicit_try(stmts);
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
                        is_statement_modifier,
                    } => self.compile_if_value(
                        cond,
                        then_branch,
                        else_branch,
                        binding_var,
                        *is_statement_modifier,
                    ),
                    Stmt::Block(inner) => self.compile_block_inline(inner),
                    Stmt::SyntheticBlock(inner) => self.compile_synthetic_block_inline(inner),
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
                    // A bare assignment (`$s += $_`, desugared to `Stmt::Assign`)
                    // in value-final position must yield the assigned container,
                    // not Nil. Raku assignment is an expression returning the
                    // lvalue container, so a value-collecting `for` body whose
                    // last statement is `$s += $_` collects the `$s` container
                    // each iteration (`(for 1..3 { $s += $_ })` is `(6, 6, 6)`,
                    // not `(1, 3, 6)`). Route it through `AssignExpr` so it
                    // leaves the same container the parenthesized form
                    // `($s += $_)` would.
                    Stmt::Assign {
                        name,
                        expr,
                        op: op @ (crate::ast::AssignOp::Assign | crate::ast::AssignOp::Bind),
                    } => {
                        self.compile_expr(&Expr::AssignExpr {
                            name: name.clone(),
                            expr: Box::new(expr.clone()),
                            is_bind: matches!(op, crate::ast::AssignOp::Bind),
                        });
                    }
                    // A bare call the parser resolved to a *statement* call
                    // (`Stmt::Call`, chosen when the name is a known routine —
                    // e.g. a sub imported by an already-parsed `use`) must still
                    // yield its return value in value-final position. Without
                    // this arm it fell to the sink path below (`SinkPop` + Nil),
                    // so `do for ^2 { imported_sub() }` collected `Nil` per
                    // iteration while the parenthesized form — which parses as
                    // `Stmt::Expr` — collected correctly.
                    Stmt::Call { name, args } => {
                        self.compile_tail_stmt_call_value(*name, args);
                    }
                    // `BEGIN` runs at compile time but is still an ordinary
                    // value-producing statement: in value-final position the
                    // block's value is the phaser body's.
                    Stmt::Phaser {
                        kind: crate::ast::PhaserKind::Begin,
                        body,
                        ..
                    } => {
                        self.compile_check_phaser_value(body);
                    }
                    _ => {
                        self.compile_stmt(stmt);
                        self.emit_nil_value();
                    }
                }
            } else {
                self.compile_stmt(stmt);
                // A non-final statement `given` nets one stack value that would
                // shadow the block's real tail value — pop it (the final one is
                // the block value and is kept, see the `Stmt::Given` arm above).
                if matches!(stmt, Stmt::Given { .. }) {
                    self.code.emit(OpCode::Pop);
                }
            }
        }
        self.pop_dynamic_scope_lexical(saved);
    }

    /// Compile the `if EXPR -> v { }` binding declaration and return the
    /// expression that reads the bound name back (for the truth test and the
    /// branch body). A `\`-prefixed binding names a sigilless pointy
    /// (`if EXPR -> \r { }`): it is registered as a sigilless local BEFORE the
    /// decl compiles, so the store binds the condition value itself instead of
    /// itemizing it into a scalar container (DBIish's `if self._row -> \r
    /// { r.Array }` must not nest the row array), and it reads back as a bare
    /// word rather than a `$`-variable.
    /// The `@`/`%` case additionally returns a declaration the caller must emit
    /// INSIDE the then-branch (see the comment below), so this returns
    /// `(test_expr, deferred_container_decl)`.
    pub(super) fn compile_if_binding_decl(
        &mut self,
        var_name: &str,
        cond: &Expr,
    ) -> (Expr, Option<(String, Expr)>) {
        // `if EXPR -> @a { }` / `-> %h { }` tests EXPR itself, not the bound
        // container. Assigning the condition into an `@`/`%` container changes
        // its truthiness — `my @a = Any` is a one-element `[Any]`, which is
        // true — so a missing hash element (`if %cache{$k} -> @avail { }`, Cro's
        // connection cache) wrongly ran the block and then handed out `Any`.
        // Evaluate the condition once into a hidden scalar and test that; the
        // container itself is only bound once the branch is known to be taken,
        // because binding a non-Positional condition (`if 0 -> @a`) is a type
        // error that must not fire when the branch is not entered.
        if var_name.starts_with('@') || var_name.starts_with('%') {
            let tmp = format!("__mutsu_tmp_if_cond_{}", self.code.constants.len());
            self.compile_stmt(&Self::plain_var_decl(tmp.clone(), cond.clone()));
            return (
                Expr::Var(tmp.clone()),
                Some((var_name.to_string(), Expr::Var(tmp))),
            );
        }
        let (bare_name, read_expr) = if let Some(bare) = var_name.strip_prefix('\\') {
            self.sigilless_locals.insert(bare.to_string());
            (bare.to_string(), Expr::BareWord(bare.to_string()))
        } else {
            let bare = var_name.trim_start_matches('$').to_string();
            (bare.clone(), Expr::Var(bare))
        };
        let var_decl = Stmt::VarDecl {
            name: bare_name,
            expr: cond.clone(),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: vec![],
            custom_traits: Vec::new(),
            where_constraint: None,
        };
        self.compile_stmt(&var_decl);
        (read_expr, None)
    }

    /// Emit the deferred `@`/`%` pointy-parameter binding returned by
    /// [`Self::compile_if_binding_decl`]. A pointy parameter BINDS its argument,
    /// so an `Array` condition becomes that array rather than a single-element
    /// copy of it (`if %cache{$k} -> @avail` must see all the elements).
    pub(super) fn compile_if_binding_container_decl(&mut self, decl: &Option<(String, Expr)>) {
        let Some((name, source)) = decl else { return };
        self.bind_vardecl = true;
        self.compile_stmt(&Self::plain_var_decl(name.clone(), source.clone()));
        self.bind_vardecl = false;
    }

    /// A `my NAME = EXPR;` declaration with no traits/constraints.
    fn plain_var_decl(name: String, expr: Expr) -> Stmt {
        Stmt::VarDecl {
            name,
            expr,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: vec![],
            custom_traits: Vec::new(),
            where_constraint: None,
        }
    }

    pub(super) fn compile_if_value(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
        binding_var: &Option<String>,
        is_statement_modifier: bool,
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
        // A bare `if EXPR { ... $^a ... }` whose block has a scalar placeholder
        // receives the condition value as that placeholder (like `-> $a`), so
        // `if 42 { $^a.say }` prints 42. The bind (and the arity failure when the
        // branch declares more placeholders than the single condition value
        // satisfies) is ADR-0048 D3's shared emitter.
        //
        // An `if`/`unless`/`with`/`without` STATEMENT MODIFIER (including the
        // synthetic `If` `with`/`without` desugar to) has no block of its own,
        // so this binding does not apply: `sub f { $^a if $^n }` must bind
        // `$^a` to the sub's own placeholder argument, not to the modifier's
        // boolean condition result (`$^n.defined`/truthiness).
        let bind_cond_placeholders = binding_var.is_none() && !is_statement_modifier;
        let binds_cond_placeholder =
            bind_cond_placeholders && Self::inlined_body_binds_supplied_value(then_branch);
        let needs_cond_value = needs_at_underscore || binds_cond_placeholder;
        // A topic-binding `if EXPR -> $v { ... }` (or a pointy `elsif`): bind the
        // condition value to `$v` and test the bound variable, mirroring the
        // statement-form desugar (`{ my $v = EXPR; if $v { ... } }`) that
        // `compile_do_if_expr_bound` uses. Without this a value-position pointy
        // `if`/`elsif` fell through to a Nil result (its branch value was lost).
        // A pointy `if EXPR -> $_ { }` binds a FRESH lexical `$_` (like `for -> $_`,
        // not like `my $_ = EXPR`), so its topic must NOT flow back to an enclosing
        // `given $x`'s source variable. `EnterPointyTopic` saves + clears
        // `topic_source_var` for the block; `ExitPointyTopic` (emitted after the if)
        // restores it and the outer `$_`. Only needed when the binding var is the
        // topic `$_` — a named pointy (`-> $v`) declares its own lexical.
        let pointy_topic_scope = binding_var
            .as_deref()
            .is_some_and(|v| v.trim_start_matches('$') == "_");
        if pointy_topic_scope {
            self.code.emit(OpCode::EnterPointyTopic);
        }
        let mut deferred_container_decl = None;
        if let Some(var_name) = binding_var {
            let (read_expr, deferred) = self.compile_if_binding_decl(var_name, cond);
            deferred_container_decl = deferred;
            self.compile_expr(&read_expr);
        } else {
            self.compile_expr(cond);
        }
        if needs_cond_value {
            // Duplicate condition for @_ / the placeholder (bare if blocks
            // receive the condition value).
            self.code.emit(OpCode::Dup);
        }
        let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
        self.compile_if_binding_container_decl(&deferred_container_decl);
        if needs_at_underscore {
            // Flatten the duplicated condition into @_.
            self.code.emit(OpCode::FlattenSlurpy);
            self.emit_set_named_var("@_");
        } else if bind_cond_placeholders {
            // Bind the branch's placeholders to the (unflattened) condition value
            // -- ADR-0048 D3.
            self.emit_inlined_body_placeholder_binds(then_branch, ArgSupply::Condition);
        }
        // A branch with ENTER/LEAVE/KEEP/UNDO phasers is a real block scope:
        // its LEAVE must fire when the branch exits, with the branch value
        // still delivered on the stack (OO::Monitors' method wrapper unlocks
        // its monitor lock in a LEAVE inside `if SELF.DEFINITE { ... }`).
        // Value position changes nothing about the branch being a block literal
        // re-cloned per execution — see `OpCode::ResetStateLocals`.
        let then_state_reset = self.emit_branch_state_reset(then_branch, is_statement_modifier);
        if Self::has_block_enter_leave_phasers(then_branch) {
            self.compile_phaser_block_scope(then_branch, PhaserBlockResult::Push);
        } else {
            self.compile_stmts_value(then_branch);
        }
        self.patch_nested_block_state_reset(then_state_reset);
        let jump_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(jump_else);
        if needs_cond_value {
            // Pop leftover duplicated condition on the false branch.
            self.code.emit(OpCode::Pop);
        }
        if else_branch.is_empty() {
            let empty_idx = self.code.add_constant(Value::slip(vec![]));
            self.code.emit(OpCode::LoadConst(empty_idx));
        } else {
            let else_state_reset = self.emit_branch_state_reset(else_branch, is_statement_modifier);
            if Self::has_block_enter_leave_phasers(else_branch) {
                self.compile_phaser_block_scope(else_branch, PhaserBlockResult::Push);
            } else {
                self.compile_stmts_value(else_branch);
            }
            self.patch_nested_block_state_reset(else_state_reset);
        }
        self.code.patch_jump(jump_end);
        if pointy_topic_scope {
            self.code.emit(OpCode::ExitPointyTopic);
        }
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
    ///
    /// `%?RESOURCES` is excluded even though it parses as a `HashVar`: it is a
    /// synthesized pseudo-hash (`build_resources_for_package`, rebuilt fresh on
    /// every plain read via `GetGlobal`) rather than a real container stored in
    /// locals/env, so the element-source writeback optimization's by-name
    /// locals-store lookup (`TagElementSource`) finds nothing and binds the
    /// topic to Nil — exactly the class of bug the attribute-container filter
    /// above this function's call sites already guards against. Returning
    /// `None` falls through to evaluating the element value directly
    /// (read-only, but correct — `%?RESOURCES` is never assigned to anyway).
    pub(super) fn container_var_name(target: &Expr) -> Option<String> {
        match target {
            Expr::HashVar(name) if name == "?RESOURCES" => None,
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
                Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
                    args.iter().any(expr_rebinds_topic)
                }
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
        let idx = self.code.emit(OpCode::BlockLocalScope {
            body_end: 0,
            succeed_boundary: true,
        });
        // `succeed_boundary: true` already absorbs the succeed at exactly this
        // level, so the body does not need its own `SucceedBarrier`.
        self.in_scope_restored_body(|c| c.compile_body_with_implicit_try_inner(stmts));
        self.code.patch_block_local_body_end(idx);
    }

    /// Compile a loop body (`while`/`until`/C-style `loop`/`repeat`/`for`) as a
    /// scope whose env is restored on exit. The loop opcodes bracket the body
    /// with `push_loop_local_scope`/`pop_loop_local_scope`, which is exactly the
    /// env-restore guarantee `lexically_in_block` stands for, so a `my TYPE $x`
    /// here can use the env-only `SetVarTypeScoped` instead of also writing the
    /// enclosing scope's type metadata.
    pub(super) fn compile_scope_restored_loop_body(&mut self, stmts: &[Stmt]) {
        self.in_scope_restored_body(|c| c.compile_body_with_implicit_try(stmts));
    }

    /// [`Self::compile_scope_restored_loop_body`] for a value-collecting loop
    /// body (the `for` expression form), which compiles through
    /// `compile_stmts_value` instead.
    pub(super) fn compile_scope_restored_body_value(&mut self, stmts: &[Stmt]) {
        self.in_scope_restored_body(|c| c.compile_stmts_value(stmts));
    }

    /// Run `f` with `lexically_in_block` set, restoring the previous value
    /// afterwards. See that field's doc comment for what the flag promises.
    fn in_scope_restored_body(&mut self, f: impl FnOnce(&mut Self)) {
        let saved = std::mem::replace(&mut self.lexically_in_block, true);
        f(self);
        self.lexically_in_block = saved;
    }

    /// Emit the branch a compile-time-constant `if` condition selected, with no
    /// condition evaluation and no jumps around it (ADR-0006 §2.2). Mirrors how
    /// the ordinary `Stmt::If` arm compiles the branch it jumps to, including the
    /// `elsif` chain (which arrives as a lone nested `If` in the else position).
    pub(super) fn compile_resolved_branch(&mut self, stmts: &[Stmt], is_statement_modifier: bool) {
        if stmts.is_empty() {
            return;
        }
        // Folding the condition away does not fold the BLOCK away: the branch is
        // still a block literal the enclosing block re-clones on every run, so
        // its `state` still restarts per execution (`if 1 { state $n; ++$n }`).
        let state_reset = self.emit_branch_state_reset(stmts, is_statement_modifier);
        self.compile_resolved_branch_body(stmts);
        self.patch_nested_block_state_reset(state_reset);
    }

    fn compile_resolved_branch_body(&mut self, stmts: &[Stmt]) {
        if stmts.len() == 1 && matches!(stmts[0], Stmt::If { .. }) {
            self.compile_stmt(&stmts[0]);
        } else if Self::has_block_leave_worthy_phasers(stmts) {
            // Mirrors the ordinary `Stmt::If` arm's own check (`stmt.rs`): a
            // branch with ENTER/LEAVE/KEEP/UNDO phasers is a real block
            // scope whose LEAVE must fire when the branch exits, even though
            // folding away a compile-time-constant condition (`if True {
            // LEAVE ... }`, ADR-0006 §2.2) skips the jump/condition
            // evaluation around it. This doc comment used to just say
            // "mirrors" the ordinary arm without actually doing so for this
            // one case. Deliberately `has_block_leave_worthy_phasers`, not
            // `has_block_enter_leave_phasers` — see that function's doc.
            self.compile_phaser_block_scope(stmts, PhaserBlockResult::Discard);
        } else if Self::body_mutates_topic(stmts) {
            self.synthetic_block_body = true;
            self.compile_stmt(&Stmt::Block(stmts.to_vec()));
        } else if Self::branch_declares_block_local(stmts) {
            self.compile_block_local_branch(stmts);
        } else {
            self.compile_body_with_implicit_try(stmts);
        }
    }

    /// Returns true if any of `stmts`' own top-level statements is a
    /// `when`/`default` clause, or REACHES one through its own expression(s)
    /// without crossing into a nested scope. `do when COND { ... }` is an
    /// ordinary term and can appear at any expression-nesting depth (an
    /// assignment RHS, a call argument, a list element, string
    /// interpolation, ...), and Raku still absorbs the escaping succeed at
    /// THIS block boundary regardless of how deep the `when` is buried
    /// syntactically — a naive scan for the literal `Stmt::When` shape
    /// missed all of those (see `git blame` on this comment for the crash
    /// that motivated the deeper scan; `t/succeed-block-boundary-absorption.t`
    /// pins the fix, `news/2026-08/succeed-absorbing-block-boundary.md`
    /// records the root cause).
    ///
    /// This does NOT descend into a nested block/branch/loop/sub/`given`/
    /// `do {}`/`try` — those either compute their OWN `SucceedBarrier` need
    /// independently when THEY compile, or already absorb a succeed
    /// unconditionally at the VM level regardless of any static scan
    /// (`given`, `do {}`'s `exec_do_block_expr_op`, `try`'s
    /// `exec_try_catch_op_inner`, a loop body's own per-iteration catch, a
    /// sub call's own catch) — a `when` reached through one of those belongs
    /// to that inner boundary, not this one. Trying to see through them here
    /// would only cost an unconditional `OpCode::SucceedBarrier` wrap for
    /// every plain loop/if/block regardless of whether it can ever raise a
    /// succeed, which showed up as extra JIT bailouts for perfectly ordinary
    /// code (`tests/jit_diff.rs`'s `unsupported_opcode_bails_out_cleanly`)
    /// when this was tried unconditionally — so the scan earns its keep by
    /// staying narrow rather than by being skipped.
    pub(super) fn body_has_toplevel_when(stmts: &[Stmt]) -> bool {
        stmts.iter().any(Self::stmt_reaches_when)
    }

    fn stmt_reaches_when(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::When { .. } | Stmt::Default(_) => true,
            Stmt::SyntheticBlock(inner) => inner.iter().any(Self::stmt_reaches_when),
            Stmt::Expr(e)
            | Stmt::Return(e)
            | Stmt::Die(e)
            | Stmt::Fail(e)
            | Stmt::Take(e, _)
            | Stmt::Goto(e) => Self::expr_reaches_when(e),
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => Self::expr_reaches_when(expr),
            Stmt::Call { args, .. } => args.iter().any(|a| match a {
                CallArg::Positional(e) | CallArg::Invocant(e) | CallArg::Slip(e) => {
                    Self::expr_reaches_when(e)
                }
                CallArg::Named { value: Some(e), .. } => Self::expr_reaches_when(e),
                CallArg::Named { value: None, .. } => false,
            }),
            Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
                es.iter().any(Self::expr_reaches_when)
            }
            _ => false,
        }
    }

    /// The expression-level half of [`Self::stmt_reaches_when`]: recurses
    /// through ordinary compound expressions looking for a `do when`/`do
    /// default` term, stopping at anything that introduces its own scope or
    /// already absorbs a succeed unconditionally (a closure/sub literal,
    /// `do {}`, `gather`, `try`, `do given`, ...) — see that function's doc
    /// comment for why those are excluded rather than an oversight.
    fn expr_reaches_when(expr: &Expr) -> bool {
        match expr {
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::When { .. } | Stmt::Default(_) => true,
                Stmt::SyntheticBlock(inner) => inner.iter().any(Self::stmt_reaches_when),
                // `do {}` / `do given` already absorb a succeed
                // unconditionally at the VM level (see the doc comment on
                // `Self::stmt_reaches_when`); no need to see through them.
                _ => false,
            },
            Expr::Grouped(e)
            | Expr::PositionalPair(e)
            | Expr::ZenSlice(e)
            | Expr::Itemize(e)
            | Expr::Eager(e)
            | Expr::Unary { expr: e, .. }
            | Expr::PostfixOp { expr: e, .. }
            | Expr::AssignExpr { expr: e, .. }
            | Expr::Reduction { expr: e, .. }
            | Expr::IndirectTypeLookup(e)
            | Expr::SymbolicDeref { expr: e, .. } => Self::expr_reaches_when(e),
            Expr::Binary { left, right, .. }
            | Expr::HyperOp { left, right, .. }
            | Expr::HyperFuncOp { left, right, .. }
            | Expr::MetaOp { left, right, .. } => {
                Self::expr_reaches_when(left) || Self::expr_reaches_when(right)
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::expr_reaches_when(cond)
                    || Self::expr_reaches_when(then_expr)
                    || Self::expr_reaches_when(else_expr)
            }
            Expr::Index { target, index, .. } => {
                Self::expr_reaches_when(target) || Self::expr_reaches_when(index)
            }
            Expr::IndexAssign {
                target,
                index,
                value,
                ..
            } => {
                Self::expr_reaches_when(target)
                    || Self::expr_reaches_when(index)
                    || Self::expr_reaches_when(value)
            }
            Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
                Self::expr_reaches_when(target) || args.iter().any(Self::expr_reaches_when)
            }
            Expr::CallOn { target, args } => {
                Self::expr_reaches_when(target) || args.iter().any(Self::expr_reaches_when)
            }
            Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
                args.iter().any(Self::expr_reaches_when)
            }
            Expr::ArrayLiteral(es)
            | Expr::BracketArray(es, _)
            | Expr::CaptureLiteral(es)
            | Expr::StringInterpolation(es) => es.iter().any(Self::expr_reaches_when),
            Expr::Hash(pairs) => pairs
                .iter()
                .any(|(_, v)| v.as_ref().is_some_and(Self::expr_reaches_when)),
            Expr::InfixFunc { left, right, .. } => {
                Self::expr_reaches_when(left) || right.iter().any(Self::expr_reaches_when)
            }
            _ => false,
        }
    }

    /// Emit `body` wrapped in a `SucceedBarrier` when it can reach a
    /// `when`/`default` (see [`Self::body_has_toplevel_when`]), otherwise
    /// emit it unchanged. A `when`/`default` succeed unwinds to the nearest
    /// enclosing topicalizer (`given`/`with`) if there is one, otherwise to
    /// the nearest enclosing block-like construct — a bare block, an
    /// `if`/`unless` branch, a loop body, a sub body, or (at the true
    /// mainline) the compilation unit itself. This helper is that boundary
    /// for `if`/`unless` branches and loop bodies; `Stmt::Block`'s own
    /// `SucceedBarrier` (`stmt.rs`) is the twin for a bare `{ ... }`, and
    /// `run()` (`runtime/run.rs`) is the twin for the mainline.
    pub(super) fn with_succeed_barrier(&mut self, stmts: &[Stmt], f: impl FnOnce(&mut Self)) {
        if !Self::body_has_toplevel_when(stmts) {
            f(self);
            return;
        }
        let idx = self.code.emit(OpCode::SucceedBarrier { body_end: 0 });
        f(self);
        self.code.patch_succeed_barrier_body_end(idx);
    }

    /// Compile a block body, automatically wrapping in implicit try if it contains
    /// CATCH or CONTROL blocks. This should be used for any block context (bare blocks,
    /// if branches, loop bodies, sub bodies) to ensure CATCH/CONTROL are not silently ignored.
    pub(super) fn compile_body_with_implicit_try(&mut self, stmts: &[Stmt]) {
        self.with_succeed_barrier(stmts, |c| c.compile_body_with_implicit_try_inner(stmts));
    }

    fn compile_body_with_implicit_try_inner(&mut self, stmts: &[Stmt]) {
        let saved = self.push_dynamic_scope_lexical();
        if Self::has_catch_or_control(stmts) {
            self.compile_implicit_try(stmts);
            self.code.emit(OpCode::Pop);
        } else {
            for s in stmts {
                self.compile_stmt(s);
                // A statement `given` always nets one stack value (see
                // `exec_given_op`). This body is statement position — its value
                // is never read — so pop it, or a `with $p { ... }` (lowered to
                // `if .defined { given ... }`) leaks its block value past the
                // `if`, shadowing the enclosing block's real tail value on the
                // `eval_block_value` (stack.last()) call path.
                if matches!(s, Stmt::Given { .. }) {
                    self.code.emit(OpCode::Pop);
                }
            }
        }
        self.pop_dynamic_scope_lexical(saved);
    }

    /// Compile a genuine `try` block/expression (Expr::Try { body, catch }) to a
    /// TryCatch opcode. This region *traps*: an exception that no handler
    /// matched is swallowed into `$!`.
    pub(super) fn compile_try(&mut self, body: &[Stmt], catch: &Option<Vec<Stmt>>) {
        // The source block belonging to `try { ... }` is a genuine anonymous
        // Raku callframe, just like a standalone bare block.  The TryCatch
        // boundary carries that fact to the VM so a backtrace captured in the
        // body includes it.
        self.next_try_is_bare_block = true;
        self.compile_try_region(body, catch, true);
    }

    /// Compile the implicit TryCatch wrapper the compiler puts around any block
    /// or routine body that merely *contains* a `CATCH`/`CONTROL` phaser. The
    /// phaser needs a region to observe, but the region is not a `try`, so an
    /// exception no handler matched propagates out of it instead of being
    /// swallowed (`{ die "x"; CONTROL { } }` dies).
    pub(super) fn compile_implicit_try(&mut self, body: &[Stmt]) {
        self.compile_try_region(body, &None, false);
    }

    fn compile_try_region(&mut self, body: &[Stmt], catch: &Option<Vec<Stmt>>, traps: bool) {
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
        // Separate CATCH/CONTROL blocks from body. Also track, in the
        // ORIGINAL textual order, whether a CATCH/CONTROL phaser is the last
        // thing in the block (nothing but `SetLine` markers following it) --
        // see `discards_tail_value` below, which uses this to decide whether
        // the block's would-be-tail statement is sunk.
        let mut main_stmts = Vec::new();
        let mut catch_stmts = catch.clone();
        let mut control_stmts: Option<Vec<Stmt>> = None;
        let mut phaser_is_last_in_body = false;
        for stmt in body {
            if let Stmt::Catch(catch_body) = stmt {
                catch_stmts = Some(catch_body.clone());
                phaser_is_last_in_body = true;
            } else if let Stmt::Control(control_body) = stmt {
                control_stmts = Some(control_body.clone());
                phaser_is_last_in_body = true;
            } else {
                main_stmts.push(stmt.clone());
                if !matches!(stmt, Stmt::SetLine(_)) {
                    phaser_is_last_in_body = false;
                }
            }
        }
        // ADR-0048 Phase 2: a standalone `CATCH {}`/`CONTROL {}` phaser body
        // does not take a signature in raku. Checked here (after extraction
        // from `body`, for both a genuine `try {}` and any other block that
        // merely contains one of these phasers) rather than at the
        // `Stmt::Catch`/`Stmt::Control` catch-all no-op arm in
        // `compile_stmt`, since that arm is only reached for an orphan
        // phaser this extraction never sees.
        if let Some(ref catch_body) = catch_stmts
            && self.emit_block_placeholder_die(catch_body)
        {
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        if let Some(ref control_body) = control_stmts
            && self.emit_block_placeholder_die(control_body)
        {
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        let has_explicit_catch = catch_stmts.is_some();
        let resume_safe = control_stmts
            .as_deref()
            .map(Self::control_block_is_resume_safe)
            .unwrap_or(false);
        let control_handles_take = control_stmts
            .as_deref()
            .map(Self::control_block_handles_take)
            .unwrap_or(false);
        // Emit TryCatch placeholder. Mark it a bare-block callframe when the
        // `Stmt::Block` arm requested it for a genuine source
        // `{ ...; CATCH { } }`, or for the source block of an explicit `try`.
        let is_bare_block = std::mem::take(&mut self.next_try_is_bare_block);
        let try_idx = self.code.emit(OpCode::TryCatch {
            catch_start: 0,
            control_start: 0,
            body_end: 0,
            explicit_catch: has_explicit_catch,
            resume_safe,
            control_handles_take,
            is_bare_block,
            traps,
        });
        // Compile main body (last Stmt::Expr/Call leaves value on stack)
        let mut main_leaves_value = false;
        // Whether the trailing value is a bare container read (`$f`, `@a`,
        // `%h`) rather than a freshly computed value -- see
        // `Compiler::stmt_value_is_bare_container_read`. Raku's optimizer
        // never actually forces a pure variable mention, so a trailing bare
        // variable holding an unhandled Failure must not retroactively
        // explode it here either (`try { $f }` where `$f` was made without
        // `use fatal` lives, even though `$f` is textually the try's tail).
        let mut tail_is_bare_container_read = false;
        if Self::has_block_enter_leave_phasers(&main_stmts) {
            self.synthetic_block_body = true;
            self.compile_stmt(&Stmt::Block(main_stmts.clone()));
            self.compile_expr(&Expr::Var("_".to_string()));
            main_leaves_value = true;
        } else {
            // A CATCH/CONTROL phaser occupies a slot in the block's statement
            // sequence for tail-position purposes, exactly like an ordinary
            // statement, even though it does not run in textual order (it only
            // fires on an exception/control signal). So the would-be-tail
            // statement is sunk in place (Raku: "Useless use of ... in sink
            // context") ONLY when the phaser is textually the LAST thing in
            // the block -- i.e. it "follows" that statement, bumping it out of
            // tail position. When the phaser comes BEFORE the real last
            // statement, that statement is still the tail and its value still
            // flows through normally. Verified against `raku`:
            //   sub f { 42; CATCH { default { } } }; say f();   # Nil (phaser after)
            //   sub f { CATCH { default { } }; 42 }; say f();   # 42  (phaser before)
            // So the sink applies only when `phaser_is_last_in_body` is true.
            let discards_tail_value = phaser_is_last_in_body;
            for (i, stmt) in main_stmts.iter().enumerate() {
                let is_last = i == main_stmts.len() - 1 && !discards_tail_value;
                // Keep the final expression's value on the stack so the try
                // block evaluates to it (the value of a `do`/sub/closure body).
                // compile_try always leaves exactly one value (LoadNil below
                // when none), so stack discipline is unchanged for
                // statement-context callers.
                if is_last {
                    if let Stmt::Expr(expr) = stmt {
                        self.compile_expr(expr);
                        main_leaves_value = true;
                        tail_is_bare_container_read = Self::stmt_value_is_bare_container_read(expr);
                        continue;
                    } else if let Stmt::Call { name, args } = stmt {
                        self.compile_tail_stmt_call_value(*name, args);
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
        // the routine boundary, so it is unaffected and still returned. Skipped
        // entirely for a bare-container-read tail (see above) — there is
        // nothing to force, so omitting the check is exactly equivalent to
        // emitting it with the check disabled.
        if !tail_is_bare_container_read {
            self.code.emit(OpCode::ThrowIfFailure);
        }
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
                self.compile_implicit_try(catch_body);
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

    /// Compile a tail-position statement call (`Stmt::Call` as the last
    /// statement of a body) so its value stays on the stack — the body's
    /// result. Positional-only calls reuse the expression path
    /// (`Expr::Call`, whose `CallFunc` op spreads only `|EXPR` positions,
    /// same as `ExecCallPairs` below -- ADR-0054 Slices 1-3); calls with
    /// named/slip args compile exactly like the statement path
    /// (`MakeNamedArg` pairs, `MakeSlip`) and dispatch via `ExecCallPairs {
    /// keep_value: true }`, which pushes the call's value. That routing is
    /// needed ONLY to satisfy `keep_value` now: `ExecCallPairs`'s
    /// syntax-accurate `|EXPR` tracking is no longer a reason to prefer it
    /// over `Expr::Call`, since `CallFunc` tracks call-site syntax
    /// identically (ADR-0054 Slice 4 collapsed both call ops onto the same
    /// `arg_sources_idx` descriptor). Without the `keep_value` routing, a
    /// tail call with named args fell to the value-less statement op and
    /// the routine returned its topic instead (JSON::Marshal's
    /// `to-json($ret, :$sorted-keys, :$pretty)` tail made `marshal` return
    /// Any on the interpreter path).
    pub(super) fn compile_tail_stmt_call_value(
        &mut self,
        name: crate::symbol::Symbol,
        args: &[CallArg],
    ) {
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
                name,
                args: expr_args,
            });
            return;
        }

        let wb_base = self.index_rw_writeback_base();
        for arg in &rewritten_args {
            match arg {
                // A closure literal NAMED-argument value escapes exactly as it
                // does for a plain call's named-args branch
                // (`compile_expr_call_inner`, and the identical fix in
                // `compile_stmt`'s `Stmt::Call` arm): the callee may store it
                // rather than invoke it immediately, and this stmt-call shape
                // (a listop-style tail call whose callee is not statically
                // known, e.g. an imported routine — see `Stmt::Call`) is
                // otherwise indistinguishable from a plain call at the syntax
                // level. Without this, a closure literal's captured-and-mutated
                // free variables never get boxed into a shared cell, so a
                // same-named parameter in the callee's own call chain can
                // shadow the closure's own captured lexical when it is later
                // invoked from a nested block
                // (todo/deep/closure-capture-shadowed-by-colliding-callee-parameter.md).
                //
                // Positional args deliberately keep `compile_call_arg`'s
                // unconditional non-escaping treatment — see the identical
                // note in `compile_stmt`'s `Stmt::Call` arm
                // (t/bind-alias-chain.t regressed when this was widened).
                CallArg::Positional(expr) => self.compile_call_arg(expr),
                CallArg::Named {
                    name,
                    value: Some(expr),
                } => {
                    self.compile_expr(&Expr::Literal(Value::str(name.clone())));
                    let escaping = Self::is_closure_literal_arg(expr);
                    self.with_escape(escaping, |s| s.compile_expr(expr));
                    self.code.emit(OpCode::MakeNamedArg);
                }
                CallArg::Named { name, value: None } => {
                    self.compile_expr(&Expr::Literal(Value::str(name.clone())));
                    self.compile_expr(&Expr::Literal(Value::TRUE));
                    self.code.emit(OpCode::MakeNamedArg);
                }
                // `|EXPR` interpolates into the argument list: MakeSlip builds
                // the Slip and the slip side table spreads exactly these
                // positions.
                CallArg::Slip(expr) => {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::MakeSlip);
                }
                CallArg::Invocant(_) => unreachable!(),
            }
        }
        let name_idx = self.code.add_constant(Value::str(name.resolve()));
        let arg_sources_idx = self.add_call_arg_sources_constant(&rewritten_args);
        self.code.emit(OpCode::ExecCallPairs {
            name_idx,
            arity: rewritten_args.len() as u32,
            arg_sources_idx,
            keep_value: true,
        });
        // This dispatch shape has no writeback emit point (see
        // `index_rw_writeback_base`). Drop what this call's own arguments
        // queued rather than leaving it for the next call to emit around ITS
        // result.
        self.pending_index_rw_writebacks.truncate(wb_base);
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
        // All-arms form: `CONTROL { when CX::Warn { ...; .resume } ... }`. Only
        // warns are routed through the inline mechanism, so this is resume-safe
        // when every arm that can match a CX::Warn — an explicit `when CX::Warn`
        // arm or a `default` arm — ends in `.resume`. An arm for a different
        // CX:: type never matches a warn inside the inline run and is ignored.
        // A `when` whose matcher we cannot classify stays conservative (false).
        if meaningful
            .iter()
            .all(|s| matches!(s, Stmt::When { .. } | Stmt::Default(_)))
        {
            let mut warn_arm_seen = false;
            for s in &meaningful {
                match s {
                    Stmt::When { cond, body, .. } => match Self::when_cond_warn_class(cond) {
                        WhenWarnClass::Warn => {
                            warn_arm_seen = true;
                            if !Self::control_block_body_resumes(body) {
                                return false;
                            }
                        }
                        WhenWarnClass::OtherControl => {}
                        WhenWarnClass::Unknown => return false,
                    },
                    Stmt::Default(body) => {
                        warn_arm_seen = true;
                        if !Self::control_block_body_resumes(body) {
                            return false;
                        }
                    }
                    _ => unreachable!("filtered to When/Default above"),
                }
            }
            return warn_arm_seen;
        }
        // Any `when` arm or `succeed` escapes the block via a control signal
        // (it does NOT resume) — the #3372 killer case. Reject.
        if meaningful.iter().any(|s| Self::stmt_exits_control_block(s)) {
            return false;
        }
        // The tail statement must be a `.resume` method call.
        matches!(meaningful.last(), Some(Stmt::Expr(e)) if Self::expr_is_resume_call(e))
    }

    /// The body of a `when`/`default` arm resumes iff its last meaningful
    /// statement is a `.resume` call (and it never `succeed`s before that).
    /// A tail `if`/`unless` whose taken branch ends in `.resume` also counts
    /// (`when CX::Warn { say .message; if .message ~~ /…/ { $n++; .resume } }`
    /// — META6's t/030-versions.t): the inline mechanism treats a run that
    /// falls through without resuming as resume-with-Nil, so the non-resuming
    /// branch degrades to that existing approximation instead of losing the
    /// deep continuation entirely on the resuming branch.
    fn control_block_body_resumes(body: &[Stmt]) -> bool {
        let meaningful: Vec<&Stmt> = body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        if meaningful.iter().any(|s| Self::stmt_exits_control_block(s)) {
            return false;
        }
        match meaningful.last() {
            Some(Stmt::Expr(e)) if Self::expr_is_resume_call(e) => true,
            Some(Stmt::If {
                then_branch,
                else_branch,
                ..
            }) => {
                let branch_resumes = |b: &[Stmt]| -> bool {
                    let m: Vec<&Stmt> = b
                        .iter()
                        .filter(|s| !matches!(s, Stmt::SetLine(_)))
                        .collect();
                    !m.iter().any(|s| Self::stmt_exits_control_block(s))
                        && matches!(m.last(), Some(Stmt::Expr(e)) if Self::expr_is_resume_call(e))
                };
                let else_ok = {
                    let m: Vec<&Stmt> = else_branch
                        .iter()
                        .filter(|s| !matches!(s, Stmt::SetLine(_)))
                        .collect();
                    m.is_empty() || branch_resumes(else_branch)
                };
                branch_resumes(then_branch) && else_ok
            }
            _ => false,
        }
    }

    fn when_cond_warn_class(cond: &Expr) -> WhenWarnClass {
        match cond {
            Expr::BareWord(name) => {
                if name == "CX::Warn" {
                    WhenWarnClass::Warn
                } else if name.starts_with("CX::") {
                    WhenWarnClass::OtherControl
                } else {
                    WhenWarnClass::Unknown
                }
            }
            _ => WhenWarnClass::Unknown,
        }
    }

    /// Whether a CONTROL block has an arm that can match a `CX::Take`: an
    /// explicit `when CX::Take` clause or a catch-all `default`. Anything else
    /// (a `when` for a different `CX::` type, or an unclassifiable matcher)
    /// does not count — a `when` whose matcher we cannot read stays
    /// conservative (`false`), keeping `take`'s direct fast path.
    fn control_block_handles_take(stmts: &[Stmt]) -> bool {
        stmts.iter().any(|s| match s {
            Stmt::SetLine(_) => false,
            Stmt::Default(_) => true,
            Stmt::When { cond, .. } => matches!(cond, Expr::BareWord(n) if n == "CX::Take"),
            _ => false,
        })
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

/// How a `when` arm's matcher relates to a CX::Warn signal, for the
/// resume-safe classification above.
enum WhenWarnClass {
    /// Matches warns (`when CX::Warn`).
    Warn,
    /// A different CX:: control type — never matches a warn.
    OtherControl,
    /// Anything we cannot classify — stay conservative.
    Unknown,
}
