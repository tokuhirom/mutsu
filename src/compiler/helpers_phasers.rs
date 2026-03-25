use super::*;

impl Compiler {
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

    fn rewrite_next_targets_in_stmt(
        stmt: &Stmt,
        current_loop_label: Option<&str>,
        next_ph: &[Stmt],
        leave_ph: &[Stmt],
        in_nested_loop: bool,
    ) -> Stmt {
        match stmt {
            Stmt::Next(label)
                if Self::next_targets_current_loop(label, current_loop_label, in_nested_loop) =>
            {
                let mut wrapped = Vec::new();
                wrapped.extend(leave_ph.iter().cloned());
                wrapped.extend(next_ph.iter().cloned());
                wrapped.push(stmt.clone());
                Stmt::SyntheticBlock(wrapped)
            }
            Stmt::Last(label)
                if Self::next_targets_current_loop(label, current_loop_label, in_nested_loop)
                    && !leave_ph.is_empty() =>
            {
                let mut wrapped = leave_ph.to_vec();
                wrapped.push(stmt.clone());
                Stmt::SyntheticBlock(wrapped)
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
            } => Stmt::If {
                cond: cond.clone(),
                then_branch: Self::rewrite_next_targets_in_stmts(
                    then_branch,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    in_nested_loop,
                ),
                else_branch: Self::rewrite_next_targets_in_stmts(
                    else_branch,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    in_nested_loop,
                ),
                binding_var: binding_var.clone(),
            },
            Stmt::Block(body) => Stmt::Block(Self::rewrite_next_targets_in_stmts(
                body,
                current_loop_label,
                next_ph,
                leave_ph,
                in_nested_loop,
            )),
            Stmt::SyntheticBlock(body) => {
                Stmt::SyntheticBlock(Self::rewrite_next_targets_in_stmts(
                    body,
                    current_loop_label,
                    next_ph,
                    leave_ph,
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
                    true,
                ),
                label: label.clone(),
            },
            Stmt::For {
                iterable,
                param,
                param_def,
                params,
                body,
                label,
                mode,
                rw_block,
            } => Stmt::For {
                iterable: iterable.clone(),
                param: param.clone(),
                param_def: param_def.clone(),
                params: params.clone(),
                body: Self::rewrite_next_targets_in_stmts(
                    body,
                    current_loop_label,
                    next_ph,
                    leave_ph,
                    true,
                ),
                label: label.clone(),
                mode: *mode,
                rw_block: *rw_block,
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
                    true,
                ),
                repeat: *repeat,
                label: label.clone(),
            },
            other => other.clone(),
        }
    }

    fn rewrite_next_targets_in_stmts(
        stmts: &[Stmt],
        current_loop_label: Option<&str>,
        next_ph: &[Stmt],
        leave_ph: &[Stmt],
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
        if !Self::has_phasers(body) {
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
                    PhaserKind::First => first_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Next => next_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Last => last_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Pre => pre_ph.push(stmt.clone()),
                    PhaserKind::Post => post_ph.push(stmt.clone()),
                    _ => body_main.push(stmt.clone()),
                }
            } else {
                body_main.push(stmt.clone());
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
                expr: Expr::Literal(Value::Bool(true)),
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
                expr: Expr::Literal(Value::Bool(false)),
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
                expr: Expr::Literal(Value::Nil),
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
                expr: Expr::Literal(Value::Nil),
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
                expr: Expr::Literal(Value::Nil),
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
            expr: Expr::Literal(Value::Bool(true)),
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
        let body_main = if next_ph.is_empty() && leave_ph_reversed.is_empty() {
            body_main
        } else {
            Self::rewrite_next_targets_in_stmts(
                &body_main,
                label,
                &next_ph,
                &leave_ph_reversed,
                false,
            )
        };

        // FIRST runs before ENTER on the first iteration (per Raku spec)
        if !first_ph.is_empty() {
            let mut then_branch = first_ph;
            then_branch.push(Stmt::Assign {
                name: first_var.clone(),
                expr: Expr::Literal(Value::Bool(false)),
                op: AssignOp::Assign,
            });
            loop_body.push(Stmt::If {
                cond: Expr::Var(first_var.clone()),
                then_branch,
                else_branch: Vec::new(),
                binding_var: None,
            });
        }
        loop_body.extend(enter_ph);
        // PRE phasers run after ENTER, in forward source order
        loop_body.extend(pre_ph);
        // When we have both result_var (KEEP/UNDO) and post_topic_var (POST),
        // we need to capture the body's last expression into both.
        let capture_var = result_var.clone().or(post_topic_var.clone());
        if let Some(cap_var) = capture_var.clone() {
            if let Some((last, prefix)) = body_main.split_last() {
                loop_body.extend(prefix.iter().cloned());
                match last {
                    Stmt::Expr(expr) => loop_body.push(Stmt::Assign {
                        name: cap_var.clone(),
                        expr: expr.clone(),
                        op: AssignOp::Assign,
                    }),
                    other => {
                        loop_body.push(other.clone());
                        loop_body.push(Stmt::Assign {
                            name: cap_var.clone(),
                            expr: Expr::Literal(Value::Nil),
                            op: AssignOp::Assign,
                        });
                    }
                }
            } else {
                loop_body.push(Stmt::Assign {
                    name: cap_var.clone(),
                    expr: Expr::Literal(Value::Nil),
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
                .unwrap_or(Expr::Literal(Value::Nil));
            let mut post_body = Vec::new();
            for s in post_ph.iter().rev() {
                post_body.push(s.clone());
            }
            loop_body.push(Stmt::Given {
                topic: post_topic,
                body: post_body,
            });
        }
        // next_ph was already reversed above for LIFO order
        loop_body.extend(next_ph);
        loop_body.extend(leave_ph);
        if let Some(result_var) = result_var.clone() {
            if !keep_ph.is_empty() || !undo_ph.is_empty() {
                loop_body.push(Stmt::If {
                    cond: Expr::Var(result_var.clone()),
                    then_branch: keep_ph,
                    else_branch: undo_ph,
                    binding_var: None,
                });
            }
            // Preserve loop-body value for expression contexts that collect iteration results.
            loop_body.push(Stmt::Expr(Expr::Var(result_var)));
        }

        let post = if last_ph.is_empty() {
            Vec::new()
        } else if let Some(last_topic_var) = last_topic_var {
            // Wrap LAST phasers in `given $last_topic` so $_ is restored
            // from the last loop iteration
            let given_stmt = Stmt::Given {
                topic: Expr::Var(last_topic_var),
                body: last_ph,
            };
            vec![Stmt::If {
                cond: Expr::Var(ran_var),
                then_branch: vec![given_stmt],
                else_branch: Vec::new(),
                binding_var: None,
            }]
        } else {
            vec![Stmt::If {
                cond: Expr::Var(ran_var),
                then_branch: last_ph,
                else_branch: Vec::new(),
                binding_var: None,
            }]
        };

        (pre, loop_body, post)
    }
}
