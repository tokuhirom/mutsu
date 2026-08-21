use super::*;

impl Compiler {
    /// Compile a CHECK phaser body wrapped in error-catching logic.
    /// If the body throws, the error is wrapped in X::Comp::BeginTime.
    pub(super) fn compile_check_phaser(&mut self, body: &[Stmt]) {
        // ADR-0048 Phase 2: BEGIN/CHECK do not take a signature in raku. This
        // is the shared primitive both statement-position phaser kinds route
        // through (see `stmt.rs`'s `Stmt::Phaser` arms), so the check lives
        // here rather than at every caller.
        if self.emit_block_placeholder_die(body) {
            return;
        }
        let start_idx = self.code.emit(OpCode::CheckPhaserStart { end_ip: 0 });
        // A `CATCH` in the phaser body handles that body's exceptions, including
        // ones thrown from a call inside it. Compiled inline into the enclosing
        // (mainline) code, the handler covered only a `die` executed at this
        // statement level — an exception unwinding out of a call escaped it and
        // surfaced as `X::Comp::BeginTime`. Giving the body its own block scope
        // installs the handler over the whole phaser, which is also the scope
        // raku gives it (a `my` inside `BEGIN { … }` is block-scoped either
        // way). Only done when a handler is present, so the common phaser keeps
        // its inline, scope-less shape.
        let has_handler = body
            .iter()
            .any(|s| matches!(s, Stmt::Catch(_) | Stmt::Control(_)));
        if has_handler {
            self.compile_stmt(&Stmt::Block(body.to_vec()));
        } else {
            for s in body {
                self.compile_stmt(s);
            }
        }
        self.code.emit(OpCode::CheckPhaserEnd);
        // Patch the end_ip to point to after the CheckPhaserEnd
        let end_ip = self.code.ops.len() as u32;
        if let OpCode::CheckPhaserStart { end_ip: ref mut e } = self.code.ops[start_idx] {
            *e = end_ip;
        }
    }

    /// Like [`Self::compile_check_phaser`], but leaves the phaser body's value on
    /// the stack.
    ///
    /// A `BEGIN` in value-final position is the block's value in Raku, and Cro
    /// leans on it for a default: `Cro::HTTP::Body::MultiPartFormData::Part`
    /// answers `content-type` with
    /// `else { BEGIN Cro::MediaType.new(type => 'text', subtype-name => 'plain') }`.
    /// Compiled through the sink-context path that fallback yielded `Nil`, so a
    /// multipart part with no `Content-Type` header had no content type at all.
    ///
    /// Like the rvalue form, the body is memoized per site rather than run at
    /// true compile time (see `Compiler::compile_phaser_expr`), so it evaluates
    /// once at first use instead of once during parsing.
    pub(super) fn compile_check_phaser_value(&mut self, body: &[Stmt]) {
        // ADR-0048 Phase 2: BEGIN does not take a signature in raku, whether
        // in statement or (this function's) value/tail position — this is
        // the shared primitive every tail-position `BEGIN` call site routes
        // through (`helpers_block_inline.rs`, `helpers_control_flow.rs`,
        // `helpers_sub_body.rs`), so the check lives here rather than at
        // every caller.
        if self.emit_block_placeholder_die(body) {
            return;
        }
        // Compiled exactly like the rvalue form (`Expr::PhaserExpr`), so a
        // statement-position `BEGIN` and an expression-position one share both
        // the value and the run-once contract: `BeginOnceExpr` memoizes the
        // body per site, otherwise a `BEGIN` in a routine tail would re-run on
        // every call.
        let site_id = self.begin_site_id(body);
        let idx = self.code.emit(OpCode::BeginOnceExpr {
            body_end: 0,
            site_id,
        });
        self.compile_block_inline(body);
        self.code.patch_body_end(idx);
    }

    pub(super) fn has_block_enter_leave_phasers(stmts: &[Stmt]) -> bool {
        stmts.iter().any(|s| {
            matches!(
                s,
                Stmt::Phaser {
                    kind: PhaserKind::Enter
                        | PhaserKind::Leave
                        | PhaserKind::Keep
                        | PhaserKind::Undo
                        | PhaserKind::Pre
                        | PhaserKind::Post,
                    ..
                }
            )
        })
    }

    /// Like [`Self::has_block_enter_leave_phasers`], but restricted to the
    /// phaser kinds that actually need `compile_phaser_block_scope`'s
    /// LEAVE-on-any-exit machinery: ENTER/LEAVE/KEEP/UNDO. PRE/POST are
    /// excluded on purpose — they are plain inline truthiness checks (see
    /// `compile_pre_phasers`/`compile_post_phasers`) with no unwind-safety
    /// need, and the loop-phaser lowering in this module synthesizes
    /// `given $topic { POST { ... } }`/`given $topic { PRE { ... } }`
    /// wrappers whose body is *solely* a re-wrapped `Stmt::Phaser` node (see
    /// `post_ph`/`pre_ph` below, which — unlike `enter_ph`/`leave_ph` — keep
    /// the wrapper because `PhaserKind::Pre`/`Post`'s own compile arm needs
    /// it). Routing that phaser-only body through `compile_phaser_block_scope`
    /// left its "value-producing statements" section empty, so the block's
    /// own topic binding was never threaded to the POST-phase run of
    /// `compile_post_phasers` — the pushed value read back as `Nil` instead
    /// of the loop's per-iteration topic.
    pub(super) fn has_block_leave_worthy_phasers(stmts: &[Stmt]) -> bool {
        stmts.iter().any(|s| {
            matches!(
                s,
                Stmt::Phaser {
                    kind: PhaserKind::Enter
                        | PhaserKind::Leave
                        | PhaserKind::Keep
                        | PhaserKind::Undo,
                    ..
                }
            )
        })
    }

    /// The body of a FIRST/NEXT/LAST phaser as a single statement. A
    /// statement-form phaser (parsed as `[SyntheticBlock([stmt])]`) shares the
    /// enclosing block's lexical scope, so it is spliced in scope-less; a
    /// block-form phaser gets its own `Stmt::Block` scope.
    fn loop_phaser_body(body: &[Stmt]) -> Stmt {
        match body {
            [stmt @ Stmt::SyntheticBlock(_)] => stmt.clone(),
            _ => Stmt::Block(body.to_vec()),
        }
    }

    fn next_targets_current_loop(
        next_label: &Option<String>,
        current_loop_label: Option<&str>,
        in_nested_loop: bool,
    ) -> bool {
        match next_label {
            Some(lbl) => current_loop_label == Some(lbl.as_str()),
            None => !in_nested_loop,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn rewrite_next_targets_in_stmt(
        stmt: &Stmt,
        current_loop_label: Option<&str>,
        next_ph: &[Stmt],
        leave_ph: &[Stmt],
        undo_ph: &[Stmt],
        in_nested_loop: bool,
    ) -> Stmt {
        match stmt {
            Stmt::Next(label)
                if Self::next_targets_current_loop(label, current_loop_label, in_nested_loop) =>
            {
                // Verified against real `raku` (`todo/tickets/loop-body-keep-undo-not-run-on-last-next.md`):
                // an explicit `next` runs its NEXT phasers FIRST (synchronously,
                // as part of the `next` transfer itself), THEN the value-based
                // KEEP/UNDO decision, THEN LEAVE. An early `next` means the
                // iteration's trailing value is undefined (`return_value` is
                // `None`), which per the definedness rule in
                // `should_run_success_queue` always routes to UNDO, never KEEP.
                // This order (NEXT, UNDO, LEAVE) is the OPPOSITE of the normal
                // (uninterrupted) fall-through order (KEEP/UNDO, LEAVE, NEXT) —
                // both verified separately against `raku`.
                let mut wrapped = Vec::new();
                wrapped.extend(next_ph.iter().cloned());
                wrapped.extend(undo_ph.iter().cloned());
                wrapped.extend(leave_ph.iter().cloned());
                wrapped.push(stmt.clone());
                Stmt::SyntheticBlock(wrapped)
            }
            Stmt::Last(label)
                if Self::next_targets_current_loop(label, current_loop_label, in_nested_loop)
                    && (!leave_ph.is_empty() || !undo_ph.is_empty()) =>
            {
                // Same reasoning as the `next` case above (verified against
                // `raku`): UNDO (never KEEP) then LEAVE before the actual `last`.
                let mut wrapped = Vec::new();
                wrapped.extend(undo_ph.iter().cloned());
                wrapped.extend(leave_ph.iter().cloned());
                wrapped.push(stmt.clone());
                Stmt::SyntheticBlock(wrapped)
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
                is_statement_modifier,
            } => Stmt::If {
                is_statement_modifier: *is_statement_modifier,
                cond: cond.clone(),
                then_branch: Self::rewrite_next_targets_in_stmts(
                    then_branch,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    undo_ph,
                    in_nested_loop,
                ),
                else_branch: Self::rewrite_next_targets_in_stmts(
                    else_branch,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    undo_ph,
                    in_nested_loop,
                ),
                binding_var: binding_var.clone(),
            },
            Stmt::Block(body) => Stmt::Block(Self::rewrite_next_targets_in_stmts(
                body,
                current_loop_label,
                next_ph,
                leave_ph,
                undo_ph,
                in_nested_loop,
            )),
            Stmt::SyntheticBlock(body) => {
                Stmt::SyntheticBlock(Self::rewrite_next_targets_in_stmts(
                    body,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    undo_ph,
                    in_nested_loop,
                ))
            }
            Stmt::Label { name, stmt } => Stmt::Label {
                name: name.clone(),
                stmt: Box::new(Self::rewrite_next_targets_in_stmt(
                    stmt,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    undo_ph,
                    in_nested_loop,
                )),
            },
            Stmt::While { cond, body, label } => Stmt::While {
                cond: cond.clone(),
                body: Self::rewrite_next_targets_in_stmts(
                    body,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    undo_ph,
                    true,
                ),
                label: label.clone(),
            },
            Stmt::For {
                iterable,
                param,
                param_def,
                params,
                params_def,
                body,
                label,
                mode,
                rw_block,
                explicit_zero_params,
                is_statement_modifier,
            } => Stmt::For {
                iterable: iterable.clone(),
                param: param.clone(),
                param_def: param_def.clone(),
                params: params.clone(),
                params_def: params_def.clone(),
                body: Self::rewrite_next_targets_in_stmts(
                    body,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    undo_ph,
                    true,
                ),
                label: label.clone(),
                mode: *mode,
                rw_block: *rw_block,
                explicit_zero_params: *explicit_zero_params,
                is_statement_modifier: *is_statement_modifier,
            },
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } => Stmt::Loop {
                init: init.clone(),
                cond: cond.clone(),
                step: step.clone(),
                body: Self::rewrite_next_targets_in_stmts(
                    body,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    undo_ph,
                    true,
                ),
                repeat: *repeat,
                label: label.clone(),
            },
            other => other.clone(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn rewrite_next_targets_in_stmts(
        stmts: &[Stmt],
        current_loop_label: Option<&str>,
        next_ph: &[Stmt],
        leave_ph: &[Stmt],
        undo_ph: &[Stmt],
        in_nested_loop: bool,
    ) -> Vec<Stmt> {
        stmts
            .iter()
            .map(|stmt| {
                Self::rewrite_next_targets_in_stmt(
                    stmt,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    undo_ph,
                    in_nested_loop,
                )
            })
            .collect()
    }

    pub(super) fn expand_loop_phasers(
        &mut self,
        body: &[Stmt],
        label: Option<&str>,
    ) -> (Vec<Stmt>, Vec<Stmt>, Vec<Stmt>) {
        if !Self::has_phasers(body) && !Self::stmts_have_enter_phaser_expr(body) {
            return (Vec::new(), body.to_vec(), Vec::new());
        }

        let mut enter_ph = Vec::new();
        let mut leave_ph = Vec::new();
        let mut keep_ph = Vec::new();
        let mut undo_ph = Vec::new();
        let mut first_ph = Vec::new();
        let mut next_ph = Vec::new();
        let mut last_ph = Vec::new();
        let mut pre_ph = Vec::new();
        let mut post_ph = Vec::new();
        let mut body_main = Vec::new();
        for stmt in body {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Enter => enter_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Leave => leave_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Keep => keep_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Undo => undo_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::First => first_ph.push(Self::loop_phaser_body(body)),
                    PhaserKind::Next => next_ph.push(Self::loop_phaser_body(body)),
                    PhaserKind::Last => last_ph.push(Self::loop_phaser_body(body)),
                    PhaserKind::Pre => pre_ph.push(stmt.clone()),
                    PhaserKind::Post => post_ph.push(stmt.clone()),
                    _ => body_main.push(stmt.clone()),
                }
            } else {
                body_main.push(stmt.clone());
            }
        }

        // Extract ENTER phaser expressions (PhaserExpr { kind: Enter }) from
        // within expressions in body_main and replace with temp variables.
        let mut enter_expr_vars: Vec<String> = Vec::new();
        if Self::stmts_have_enter_phaser_expr(&body_main) {
            let (rewritten, enter_exprs) = Self::extract_enter_phaser_exprs_from_stmts(&body_main);
            body_main = rewritten;
            for (var_name, phaser_body) in enter_exprs {
                enter_expr_vars.push(var_name.clone());
                let assign_stmt = if phaser_body.len() == 1 {
                    if let Stmt::Expr(e) = &phaser_body[0] {
                        Stmt::Assign {
                            name: var_name,
                            expr: e.clone(),
                            op: AssignOp::Assign,
                        }
                    } else {
                        Stmt::Block(phaser_body)
                    }
                } else {
                    Stmt::Block(phaser_body)
                };
                enter_ph.push(assign_stmt);
            }
        }

        let first_var = self.next_tmp_name("__mutsu_loop_first_");
        let ran_var = self.next_tmp_name("__mutsu_loop_ran_");
        let result_var = if keep_ph.is_empty() && undo_ph.is_empty() {
            None
        } else {
            Some(self.next_tmp_name("__mutsu_loop_result_"))
        };
        // Save $_ from each iteration so LAST phasers can see it
        let last_topic_var = if last_ph.is_empty() {
            None
        } else {
            Some(self.next_tmp_name("__mutsu_loop_last_topic_"))
        };
        // Capture block return value for POST phasers (POST sees $_ as block result)
        let post_topic_var = if post_ph.is_empty() {
            None
        } else {
            Some(self.next_tmp_name("__mutsu_loop_post_topic_"))
        };

        let mut pre = vec![
            Stmt::VarDecl {
                name: first_var.clone(),
                expr: Expr::Literal(Value::TRUE),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
            Stmt::VarDecl {
                name: ran_var.clone(),
                expr: Expr::Literal(Value::FALSE),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
        ];
        if let Some(result_var) = result_var.clone() {
            pre.push(Stmt::VarDecl {
                name: result_var,
                expr: Expr::Literal(Value::NIL),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }
        if let Some(last_topic_var) = last_topic_var.clone() {
            pre.push(Stmt::VarDecl {
                name: last_topic_var,
                expr: Expr::Literal(Value::NIL),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }
        if let Some(post_topic_var) = post_topic_var.clone() {
            pre.push(Stmt::VarDecl {
                name: post_topic_var,
                expr: Expr::Literal(Value::NIL),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }

        let mut loop_body = Vec::new();
        loop_body.push(Stmt::Assign {
            name: ran_var.clone(),
            expr: Expr::Literal(Value::TRUE),
            op: AssignOp::Assign,
        });
        // Save $_ at the start of each iteration so LAST phasers can see it
        // even when `last` exits the loop early (before the end of the body)
        if let Some(last_topic_var) = last_topic_var.clone() {
            loop_body.push(Stmt::Assign {
                name: last_topic_var,
                expr: Expr::Var("_".to_string()),
                op: AssignOp::Assign,
            });
        }
        // NEXT phasers run in LIFO (reverse declaration) order per Raku spec
        next_ph.reverse();
        // LEAVE phasers run in LIFO (reverse declaration) order per Raku spec
        let leave_ph_reversed: Vec<Stmt> = leave_ph.iter().rev().cloned().collect();
        // KEEP/UNDO phasers must also be dispatched when the iteration exits
        // early via `last`/`next`: per the definedness rule (see
        // `should_run_success_queue`), an interrupted iteration's trailing
        // value is undefined, which always routes to UNDO, never KEEP — see
        // `todo/tickets/loop-body-keep-undo-not-run-on-last-next.md`. So the
        // rewrite also has to run whenever UNDO phasers are present, even with
        // no NEXT/LEAVE phaser declared.
        let body_main = if next_ph.is_empty() && leave_ph_reversed.is_empty() && undo_ph.is_empty()
        {
            body_main
        } else {
            Self::rewrite_next_targets_in_stmts(
                &body_main,
                label,
                &next_ph,
                &leave_ph_reversed,
                &undo_ph,
                false,
            )
        };

        // FIRST runs before ENTER on the first iteration (per Raku spec)
        if !first_ph.is_empty() {
            let mut then_branch = first_ph;
            then_branch.push(Stmt::Assign {
                name: first_var.clone(),
                expr: Expr::Literal(Value::FALSE),
                op: AssignOp::Assign,
            });
            loop_body.push(Stmt::If {
                cond: Expr::Var(first_var.clone()),
                then_branch,
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: false,
            });
        }
        // Declare temp variables for extracted ENTER phaser expressions
        for var_name in &enter_expr_vars {
            pre.push(Stmt::VarDecl {
                name: var_name.clone(),
                expr: Expr::Literal(Value::NIL),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }
        loop_body.extend(enter_ph);
        // PRE phasers run after ENTER, in forward source order
        loop_body.extend(pre_ph);
        // When we have both result_var (KEEP/UNDO) and post_topic_var (POST),
        // we need to capture the body's last expression into both.
        let capture_var = result_var.clone().or(post_topic_var.clone());
        let body_taken = matches!(
            body_main.last(),
            Some(Stmt::Take(_, false)) if capture_var.is_some()
        );
        if let Some(cap_var) = capture_var.clone() {
            if let Some((last, prefix)) = body_main.split_last() {
                loop_body.extend(prefix.iter().cloned());
                match last {
                    Stmt::Expr(expr) => loop_body.push(Stmt::Assign {
                        name: cap_var.clone(),
                        expr: expr.clone(),
                        op: AssignOp::Assign,
                    }),
                    // A trailing `take <expr>` (the gather-lowered loop
                    // expression form) carries the iteration value: capture it
                    // for KEEP/UNDO/POST, then take the captured value so the
                    // gather still collects it.
                    Stmt::Take(expr, false) => {
                        loop_body.push(Stmt::Assign {
                            name: cap_var.clone(),
                            expr: expr.clone(),
                            op: AssignOp::Assign,
                        });
                        loop_body.push(Stmt::Take(Expr::Var(cap_var.clone()), false));
                    }
                    other => {
                        loop_body.push(other.clone());
                        loop_body.push(Stmt::Assign {
                            name: cap_var.clone(),
                            expr: Expr::Literal(Value::NIL),
                            op: AssignOp::Assign,
                        });
                    }
                }
            } else {
                loop_body.push(Stmt::Assign {
                    name: cap_var.clone(),
                    expr: Expr::Literal(Value::NIL),
                    op: AssignOp::Assign,
                });
            }
            // If we have both result_var and post_topic_var, sync them
            if result_var.is_some() && post_topic_var.is_some() {
                let rv = result_var.clone().unwrap();
                let pv = post_topic_var.clone().unwrap();
                if rv != pv {
                    loop_body.push(Stmt::Assign {
                        name: pv,
                        expr: Expr::Var(rv),
                        op: AssignOp::Assign,
                    });
                }
            }
        } else {
            loop_body.extend(body_main);
        }
        // POST phasers run after the body, in reverse source order
        // POST sees the block's return value as $_
        if !post_ph.is_empty() {
            let post_topic = post_topic_var
                .clone()
                .map(Expr::Var)
                .unwrap_or(Expr::Literal(Value::NIL));
            let mut post_body = Vec::new();
            for s in post_ph.iter().rev() {
                post_body.push(s.clone());
            }
            loop_body.push(Stmt::Given {
                topic: post_topic,
                body: post_body,
                is_statement_modifier: false,
            });
        }
        // KEEP/UNDO runs before LEAVE on normal (uninterrupted) completion,
        // verified against real `raku` (`todo/tickets/loop-body-leave-runs-before-keep-undo-instead-of-after.md`).
        // This matches the `last`/`next`-interrupted path handled by
        // `rewrite_next_targets_in_stmt` above, which also runs UNDO (the
        // only queue reachable there) before LEAVE.
        if let Some(result_var) = result_var.clone()
            && (!keep_ph.is_empty() || !undo_ph.is_empty())
        {
            loop_body.push(Stmt::If {
                cond: Expr::Var(result_var.clone()),
                then_branch: keep_ph,
                else_branch: undo_ph,
                binding_var: None,
                is_statement_modifier: false,
            });
        }
        loop_body.extend(leave_ph);
        if let Some(result_var) = result_var.clone() {
            // Preserve loop-body value for expression contexts that collect
            // iteration results. Not needed (and harmful — sinking a taken
            // Failure would throw) when the body's value was already `take`n
            // into the enclosing gather.
            if !body_taken {
                loop_body.push(Stmt::Expr(Expr::Var(result_var)));
            }
        }
        // NEXT runs after KEEP/UNDO, before the next iteration begins.
        // next_ph was already reversed above for LIFO order.
        loop_body.extend(next_ph);

        let post = if last_ph.is_empty() {
            Vec::new()
        } else if let Some(last_topic_var) = last_topic_var {
            // Wrap LAST phasers in `given $last_topic` so $_ is restored
            // from the last loop iteration
            let given_stmt = Stmt::Given {
                topic: Expr::Var(last_topic_var),
                body: last_ph,
                is_statement_modifier: false,
            };
            vec![Stmt::If {
                cond: Expr::Var(ran_var),
                then_branch: vec![given_stmt],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: false,
            }]
        } else {
            vec![Stmt::If {
                cond: Expr::Var(ran_var),
                then_branch: last_ph,
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: false,
            }]
        };

        (pre, loop_body, post)
    }

    pub(super) fn stmts_have_enter_phaser_expr(stmts: &[Stmt]) -> bool {
        stmts.iter().any(Self::stmt_has_enter_phaser_expr)
    }

    fn stmt_has_enter_phaser_expr(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Expr(e) => Self::expr_has_enter_phaser(e),
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                Self::expr_has_enter_phaser(cond)
                    || Self::stmts_have_enter_phaser_expr(then_branch)
                    || Self::stmts_have_enter_phaser_expr(else_branch)
            }
            Stmt::Assign { expr, .. } => Self::expr_has_enter_phaser(expr),
            Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                Self::stmts_have_enter_phaser_expr(body)
            }
            _ => false,
        }
    }

    fn expr_has_enter_phaser(expr: &Expr) -> bool {
        match expr {
            Expr::PhaserExpr {
                kind: PhaserKind::Enter,
                ..
            } => true,
            Expr::Binary { left, right, .. } => {
                Self::expr_has_enter_phaser(left) || Self::expr_has_enter_phaser(right)
            }
            Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => {
                Self::expr_has_enter_phaser(expr)
            }
            Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
                Self::expr_has_enter_phaser(target) || args.iter().any(Self::expr_has_enter_phaser)
            }
            Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
                args.iter().any(Self::expr_has_enter_phaser)
            }
            _ => false,
        }
    }

    fn rewrite_enter_phaser_expr(
        expr: &Expr,
        extracted: &mut Vec<(String, Vec<Stmt>)>,
        counter: &mut usize,
    ) -> Expr {
        match expr {
            Expr::PhaserExpr {
                kind: PhaserKind::Enter,
                body,
            } => {
                let tmp = format!("__mutsu_enter_expr_{}", *counter);
                *counter += 1;
                extracted.push((tmp.clone(), body.clone()));
                Expr::Var(tmp)
            }
            Expr::Binary { left, op, right } => Expr::Binary {
                left: Box::new(Self::rewrite_enter_phaser_expr(left, extracted, counter)),
                op: op.clone(),
                right: Box::new(Self::rewrite_enter_phaser_expr(right, extracted, counter)),
            },
            Expr::Unary { op, expr } => Expr::Unary {
                op: op.clone(),
                expr: Box::new(Self::rewrite_enter_phaser_expr(expr, extracted, counter)),
            },
            Expr::PostfixOp { expr, op } => Expr::PostfixOp {
                expr: Box::new(Self::rewrite_enter_phaser_expr(expr, extracted, counter)),
                op: op.clone(),
            },
            other => other.clone(),
        }
    }

    fn rewrite_enter_phaser_stmt(
        stmt: &Stmt,
        extracted: &mut Vec<(String, Vec<Stmt>)>,
        counter: &mut usize,
    ) -> Stmt {
        match stmt {
            Stmt::Expr(e) => Stmt::Expr(Self::rewrite_enter_phaser_expr(e, extracted, counter)),
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
                is_statement_modifier,
            } => Stmt::If {
                is_statement_modifier: *is_statement_modifier,
                cond: Self::rewrite_enter_phaser_expr(cond, extracted, counter),
                then_branch: Self::rewrite_enter_phaser_stmts(then_branch, extracted, counter),
                else_branch: Self::rewrite_enter_phaser_stmts(else_branch, extracted, counter),
                binding_var: binding_var.clone(),
            },
            Stmt::Assign { name, expr, op } => Stmt::Assign {
                name: name.clone(),
                expr: Self::rewrite_enter_phaser_expr(expr, extracted, counter),
                op: *op,
            },
            Stmt::Block(body) => {
                Stmt::Block(Self::rewrite_enter_phaser_stmts(body, extracted, counter))
            }
            Stmt::SyntheticBlock(body) => {
                Stmt::SyntheticBlock(Self::rewrite_enter_phaser_stmts(body, extracted, counter))
            }
            other => other.clone(),
        }
    }

    fn rewrite_enter_phaser_stmts(
        stmts: &[Stmt],
        extracted: &mut Vec<(String, Vec<Stmt>)>,
        counter: &mut usize,
    ) -> Vec<Stmt> {
        stmts
            .iter()
            .map(|s| Self::rewrite_enter_phaser_stmt(s, extracted, counter))
            .collect()
    }

    pub(super) fn extract_enter_phaser_exprs_from_stmts(
        stmts: &[Stmt],
    ) -> (Vec<Stmt>, Vec<(String, Vec<Stmt>)>) {
        let mut extracted = Vec::new();
        let mut counter = 0;
        let rewritten = Self::rewrite_enter_phaser_stmts(stmts, &mut extracted, &mut counter);
        (rewritten, extracted)
    }
}
