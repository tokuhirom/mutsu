use super::*;

impl Compiler {
    /// Compile a CHECK phaser body wrapped in error-catching logic.
    /// If the body throws, the error is wrapped in X::Comp::BeginTime.
    pub(super) fn compile_check_phaser(&mut self, body: &[Stmt], is_begin: bool) {
        // ADR-0048 Phase 2: BEGIN/CHECK do not take a signature in raku. This
        // is the shared primitive both statement-position phaser kinds route
        // through (see `stmt.rs`'s `Stmt::Phaser` arms), so the check lives
        // here rather than at every caller.
        if self.emit_block_placeholder_die(body) {
            return;
        }
        let start_idx = self.code.emit(OpCode::CheckPhaserStart {
            end_ip: 0,
            is_begin,
        });
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
        if let OpCode::CheckPhaserStart {
            end_ip: ref mut e, ..
        } = self.code.ops[start_idx]
        {
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
    pub(crate) fn has_block_leave_worthy_phasers(stmts: &[Stmt]) -> bool {
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

    /// `wants_value` is set by a caller that COLLECTS the body's trailing value
    /// (`ForParts::collect` — the expression form `do for ... { ... }`), and clear
    /// for the statement forms, which sink it.
    ///
    /// It matters because the phaser lowering appends `NEXT`/`LEAVE` bodies to the
    /// end of the loop body. In statement position that is invisible; in expression
    /// position it made the phaser body's value the iteration's result, so
    /// `do for 1,2,3 { NEXT { @seen.push: 'next' }; $_ * 2 }` collected the pushes
    /// instead of `2, 4, 6`. The capture-into-a-temp mechanism this needs already
    /// existed for KEEP/UNDO/POST; it was just keyed off which phasers were present
    /// rather than off whether anyone wanted the value.
    pub(super) fn expand_loop_phasers(
        &mut self,
        body: &[Stmt],
        label: Option<&str>,
        wants_value: bool,
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
            if let Stmt::Phaser { kind, body, .. } = stmt {
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
                            target_is_sigilless: false,
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
        // A value-collecting caller needs the user's trailing expression held
        // somewhere across the appended phaser bodies even when no KEEP/UNDO/POST
        // phaser supplies a temp of its own.
        let value_var = if wants_value && result_var.is_none() && post_topic_var.is_none() {
            Some(self.next_tmp_name("__mutsu_loop_value_"))
        } else {
            None
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
        if let Some(value_var) = value_var.clone() {
            pre.push(Stmt::VarDecl {
                name: value_var,
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
            target_is_sigilless: false,
        });
        // Save $_ at the start of each iteration so LAST phasers can see it
        // even when `last` exits the loop early (before the end of the body)
        if let Some(last_topic_var) = last_topic_var.clone() {
            loop_body.push(Stmt::Assign {
                name: last_topic_var,
                expr: Expr::Var("_".to_string()),
                op: AssignOp::Assign,
                target_is_sigilless: false,
            });
        }
        // NEXT phasers run in LIFO (reverse declaration) order per Raku spec
        next_ph.reverse();
        // LEAVE phasers run in LIFO (reverse declaration) order per Raku spec
        let leave_ph_reversed: Vec<Stmt> = leave_ph.iter().rev().cloned().collect();
        // An iteration that exits early -- `next`/`last`/`redo`/`return`, raised
        // in the body or by a closure it calls -- runs NEXT (for a `next` aimed
        // at this loop), then UNDO, then LEAVE, before the signal reaches the
        // loop: verified against real `raku`
        // (`todo/tickets/loop-body-keep-undo-not-run-on-last-next.md`). An
        // interrupted iteration's value is undefined, which per the definedness
        // rule (see `should_run_success_queue`) always routes to UNDO, never KEEP.
        // The VM queues them when the signal unwinds out of the guarded region
        // (`OpCode::LoopExitGuard`), because only then is it known whether a
        // `next` written in a closure leaves this loop (#10566).
        let exit_guard = (!next_ph.is_empty()
            || !leave_ph_reversed.is_empty()
            || !undo_ph.is_empty())
        .then(|| {
            let mut exit_ph = undo_ph.clone();
            exit_ph.extend(leave_ph_reversed.iter().cloned());
            Stmt::LoopExitGuard {
                label: label.map(str::to_string),
                next_ph: next_ph.clone(),
                exit_ph,
            }
        });

        // FIRST runs before ENTER on the first iteration (per Raku spec).
        // The "already ran" flag is cleared BEFORE the phaser body, not after:
        // a `next`/`last`/`return` thrown out of the FIRST body would otherwise
        // skip the trailing assignment and leave the flag set, so FIRST fired
        // again on EVERY later iteration. `for 1..3 { FIRST next; say $_ }`
        // then printed nothing at all instead of `2`,`3`, and
        // `gather for <a b c> { FIRST .take, next; take slip ":", .item }`
        // re-ran the FIRST `.take` each time, yielding `(a b c)` instead of
        // `(a : b : c)`.
        if !first_ph.is_empty() {
            let mut then_branch = vec![Stmt::Assign {
                name: first_var.clone(),
                expr: Expr::Literal(Value::FALSE),
                op: AssignOp::Assign,
                target_is_sigilless: false,
            }];
            then_branch.extend(first_ph);
            loop_body.push(Stmt::If {
                cond: Expr::Var(first_var),
                then_branch,
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: false,
                is_unless: false,
                with_kind: None,
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
        let capture_var = result_var.clone().or(post_topic_var.clone()).or(value_var);
        let guarded = exit_guard.is_some();
        loop_body.extend(exit_guard);
        let body_taken = matches!(
            body_main.last(),
            Some(Stmt::Take(_, false)) if capture_var.is_some()
        );
        if let Some(cap_var) = capture_var.clone() {
            if let Some((last, prefix)) = body_main.split_last() {
                loop_body.extend(prefix.iter().cloned());
                match last {
                    Stmt::Expr(expr) => loop_body.push(Stmt::Assign {
                        name: cap_var,
                        expr: expr.clone(),
                        op: AssignOp::Assign,
                        target_is_sigilless: false,
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
                            target_is_sigilless: false,
                        });
                        loop_body.push(Stmt::Take(Expr::Var(cap_var), false));
                    }
                    other => {
                        loop_body.push(other.clone());
                        loop_body.push(Stmt::Assign {
                            name: cap_var,
                            expr: Expr::Literal(Value::NIL),
                            op: AssignOp::Assign,
                            target_is_sigilless: false,
                        });
                    }
                }
            } else {
                loop_body.push(Stmt::Assign {
                    name: cap_var,
                    expr: Expr::Literal(Value::NIL),
                    op: AssignOp::Assign,
                    target_is_sigilless: false,
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
                        target_is_sigilless: false,
                    });
                }
            }
        } else {
            loop_body.extend(body_main);
        }
        if guarded {
            loop_body.push(Stmt::LoopExitGuardEnd);
        }
        // POST phasers run after the body, in reverse source order
        // POST sees the block's return value as $_
        if !post_ph.is_empty() {
            let post_topic = post_topic_var
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
                with_kind: None,
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
                cond: Expr::Var(result_var),
                then_branch: keep_ph,
                else_branch: undo_ph,
                binding_var: None,
                is_statement_modifier: false,
                is_unless: false,
                with_kind: None,
            });
        }
        loop_body.extend(leave_ph);
        // NEXT runs after KEEP/UNDO, before the next iteration begins.
        // next_ph was already reversed above for LIFO order.
        loop_body.extend(next_ph);
        // Re-emit the captured trailing value LAST, after every appended phaser
        // body. This has to come after `leave_ph`/`next_ph`, not before them: those
        // are spliced onto the end of the body, so a value emitted first is no
        // longer the body's last statement and a collecting caller takes the
        // phaser's value instead (`do for 1,2,3 { NEXT {...}; $_ * 2 }`).
        //
        // Not emitted (and harmful — sinking a taken Failure would throw) when the
        // body's value was already `take`n into the enclosing gather.
        if let Some(capture_var) = capture_var
            && !body_taken
            && (wants_value || result_var.is_some())
        {
            loop_body.push(Stmt::Expr(Expr::Var(capture_var)));
        }

        let post = if last_ph.is_empty() {
            Vec::new()
        } else if let Some(last_topic_var) = last_topic_var {
            // Wrap LAST phasers in `given $last_topic` so $_ is restored
            // from the last loop iteration
            let given_stmt = Stmt::Given {
                topic: Expr::Var(last_topic_var),
                body: last_ph,
                is_statement_modifier: false,
                with_kind: None,
            };
            vec![Stmt::If {
                cond: Expr::Var(ran_var),
                then_branch: vec![given_stmt],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: false,
                is_unless: false,
                with_kind: None,
            }]
        } else {
            vec![Stmt::If {
                cond: Expr::Var(ran_var),
                then_branch: last_ph,
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: false,
                is_unless: false,
                with_kind: None,
            }]
        };

        (pre, loop_body, post)
    }

    /// A routine/closure body whose statements embed an `ENTER` expression
    /// (`now - ENTER now`) evaluates it at block entry, before the rest of the
    /// statement. Returns the body with each such expression hoisted into a
    /// leading `ENTER` phaser that stores a temp, or `None` when there is none.
    /// A statement that is just `ENTER ...` is left alone (it supplies the
    /// block value through the trailing-ENTER path).
    // Cost: O(n), n = AST nodes of the body.
    pub(super) fn hoist_enter_phaser_exprs(stmts: &[Stmt]) -> Option<Vec<Stmt>> {
        let embedded = |s: &Stmt| {
            !matches!(s, Stmt::Expr(Expr::PhaserExpr { .. })) && Self::stmt_has_enter_phaser_expr(s)
        };
        if !stmts.iter().any(embedded) {
            return None;
        }
        let mut hoisted = Vec::new();
        let mut rest = stmts.to_vec();
        let extracted = super::enter_phaser_exprs::extract_enter_exprs(&mut rest, embedded);
        // A block-bodied `ENTER { ...; value }` keeps its in-place evaluation:
        // only single-expression phasers have a value we can store at entry.
        if extracted
            .iter()
            .any(|(_, b)| !matches!(b.as_slice(), [Stmt::Expr(_)]))
        {
            return None;
        }
        for (var_name, phaser_body) in extracted {
            let [Stmt::Expr(init)] = phaser_body.as_slice() else {
                continue;
            };
            let init = init.clone();
            let body = vec![Stmt::VarDecl {
                name: var_name,
                expr: init,
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            }];
            hoisted.push(Stmt::Phaser {
                kind: PhaserKind::Enter,
                body,
                condition: None,
                end_index: None,
            });
        }
        hoisted.extend(rest);
        Some(hoisted)
    }

    pub(super) fn stmts_have_enter_phaser_expr(stmts: &[Stmt]) -> bool {
        super::enter_phaser_exprs::stmts_have_enter_expr(stmts)
    }

    fn stmt_has_enter_phaser_expr(stmt: &Stmt) -> bool {
        super::enter_phaser_exprs::stmts_have_enter_expr(std::slice::from_ref(stmt))
    }

    pub(super) fn extract_enter_phaser_exprs_from_stmts(
        stmts: &[Stmt],
    ) -> (Vec<Stmt>, Vec<(String, Vec<Stmt>)>) {
        let mut rewritten = stmts.to_vec();
        let extracted = super::enter_phaser_exprs::extract_enter_exprs(&mut rewritten, |_| true);
        (rewritten, extracted)
    }
}
