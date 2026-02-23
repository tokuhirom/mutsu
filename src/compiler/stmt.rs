use super::*;

impl Compiler {
    pub(super) fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Pop);
            }
            Stmt::Block(stmts) => {
                if Self::has_catch_or_control(stmts) {
                    self.compile_try(stmts, &None);
                    self.code.emit(OpCode::Pop);
                } else if Self::has_block_enter_leave_phasers(stmts) {
                    let idx = self.code.emit(OpCode::BlockScope {
                        enter_end: 0,
                        body_end: 0,
                        end: 0,
                    });
                    for s in stmts {
                        if let Stmt::Phaser {
                            kind: PhaserKind::Enter,
                            body,
                        } = s
                        {
                            for inner in body {
                                self.compile_stmt(inner);
                            }
                        }
                    }
                    self.code.patch_block_enter_end(idx);
                    for s in stmts {
                        match s {
                            Stmt::Phaser {
                                kind:
                                    PhaserKind::Enter
                                    | PhaserKind::Leave
                                    | PhaserKind::Keep
                                    | PhaserKind::Undo,
                                ..
                            } => {}
                            _ => self.compile_stmt(s),
                        }
                    }
                    self.code.patch_block_body_end(idx);
                    for s in stmts {
                        if let Stmt::Phaser {
                            kind: PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo,
                            body,
                        } = s
                        {
                            for inner in body {
                                self.compile_stmt(inner);
                            }
                        }
                    }
                    self.code.patch_loop_end(idx);
                } else if Self::has_let_deep(stmts) {
                    // Block contains `let` — wrap in LetBlock for save/restore
                    let idx = self.code.emit(OpCode::LetBlock { body_end: 0 });
                    for (i, s) in stmts.iter().enumerate() {
                        let is_last = i == stmts.len() - 1;
                        if is_last {
                            if let Stmt::Expr(expr) = s {
                                self.compile_expr(expr);
                                self.code.emit(OpCode::SetTopic);
                            } else {
                                self.compile_stmt(s);
                            }
                        } else {
                            self.compile_stmt(s);
                        }
                    }
                    self.code.patch_let_block_end(idx);
                } else if Self::has_use_stmt(stmts) {
                    // Block contains `use` — wrap with import scope save/restore
                    // so imports are lexically scoped to this block
                    self.code.emit(OpCode::PushImportScope);
                    for s in stmts {
                        self.compile_stmt(s);
                    }
                    self.code.emit(OpCode::PopImportScope);
                } else {
                    // Plain blocks still create a lexical routine scope.
                    let idx = self.code.emit(OpCode::BlockScope {
                        enter_end: 0,
                        body_end: 0,
                        end: 0,
                    });
                    self.code.patch_block_enter_end(idx);
                    for s in stmts {
                        self.compile_stmt(s);
                    }
                    self.code.patch_block_body_end(idx);
                    self.code.patch_loop_end(idx);
                }
            }
            Stmt::Say(exprs) => {
                self.compile_exprs(exprs);
                self.code.emit(OpCode::Say(exprs.len() as u32));
            }
            Stmt::Print(exprs) => {
                self.compile_exprs(exprs);
                self.code.emit(OpCode::Print(exprs.len() as u32));
            }
            Stmt::Note(exprs) => {
                self.compile_exprs(exprs);
                self.code.emit(OpCode::Note(exprs.len() as u32));
            }
            Stmt::VarDecl {
                name,
                expr,
                type_constraint,
                is_state,
            } => {
                self.compile_expr(expr);
                if let Some(tc) = type_constraint {
                    let tc_idx = self.code.add_constant(Value::Str(tc.clone()));
                    self.code.emit(OpCode::TypeCheck(tc_idx));
                }
                let slot = self.alloc_local(name);
                if *is_state {
                    let key = format!("__state_{}::{}::{}", self.current_package, name, slot);
                    let key_idx = self.code.add_constant(Value::Str(key.clone()));
                    self.code.state_locals.push((slot as usize, key.clone()));
                    self.code.emit(OpCode::StateVarInit(slot, key_idx));
                } else {
                    self.code.emit(OpCode::SetLocal(slot));
                }
            }
            Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign | AssignOp::Bind,
            } if name != "*PID" => {
                self.compile_expr(expr);
                self.emit_set_named_var(name);
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.compile_expr(cond);
                let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
                self.compile_body_with_implicit_try(then_branch);
                if else_branch.is_empty() {
                    self.code.patch_jump(jump_else);
                } else {
                    let jump_end = self.code.emit(OpCode::Jump(0));
                    self.code.patch_jump(jump_else);
                    self.compile_body_with_implicit_try(else_branch);
                    self.code.patch_jump(jump_end);
                }
            }
            Stmt::While { cond, body, label } => {
                let (pre_stmts, loop_body, post_stmts) = self.expand_loop_phasers(body);
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                let loop_idx = self.code.emit(OpCode::WhileLoop {
                    cond_end: 0,
                    body_end: 0,
                    label: label.clone(),
                });
                self.compile_expr(cond);
                self.code.patch_while_cond_end(loop_idx);
                self.compile_body_with_implicit_try(&loop_body);
                self.code.patch_loop_end(loop_idx);
                for s in &post_stmts {
                    self.compile_stmt(s);
                }
            }
            Stmt::For {
                iterable,
                param,
                param_def,
                params,
                body,
                label,
            } => {
                let (pre_stmts, mut loop_body, post_stmts) = self.expand_loop_phasers(body);
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                // When there's a single named param (-> $k), store its name as a constant
                // so the VM can bind $k directly without overriding $_
                let param_idx = param
                    .as_ref()
                    .map(|p| self.code.add_constant(Value::Str(p.clone())));
                let bind_stmts =
                    Self::build_for_bind_stmts(param, param_def.as_ref(), param_idx, params);
                if !bind_stmts.is_empty() {
                    let mut merged = bind_stmts;
                    merged.extend(loop_body);
                    loop_body = merged;
                }
                let arity = if !params.is_empty() {
                    params.len() as u32
                } else {
                    1
                };
                self.compile_expr(iterable);
                let loop_idx = self.code.emit(OpCode::ForLoop {
                    param_idx,
                    param_local: None,
                    body_end: 0,
                    label: label.clone(),
                    arity,
                    collect: false,
                });
                self.compile_body_with_implicit_try(&loop_body);
                self.code.patch_loop_end(loop_idx);
                for s in &post_stmts {
                    self.compile_stmt(s);
                }
            }
            // C-style loop (non-repeat, no phasers)
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } if !*repeat => {
                let (pre_stmts, loop_body, post_stmts) = self.expand_loop_phasers(body);
                // Compile init statement (if any) before the loop opcode
                if let Some(init_stmt) = init {
                    self.compile_stmt(init_stmt);
                }
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                // Layout: [CStyleLoop] [cond..] [body..] [step..]
                let loop_idx = self.code.emit(OpCode::CStyleLoop {
                    cond_end: 0,
                    step_start: 0,
                    body_end: 0,
                    label: label.clone(),
                });
                // Compile condition (or push True if none)
                if let Some(cond_expr) = cond {
                    self.compile_expr(cond_expr);
                } else {
                    self.code.emit(OpCode::LoadTrue);
                }
                self.code.patch_cstyle_cond_end(loop_idx);
                // Compile body
                self.compile_body_with_implicit_try(&loop_body);
                self.code.patch_cstyle_step_start(loop_idx);
                // Compile step (if any)
                if let Some(step_expr) = step {
                    self.compile_expr(step_expr);
                    self.code.emit(OpCode::Pop);
                }
                self.code.patch_loop_end(loop_idx);
                for s in &post_stmts {
                    self.compile_stmt(s);
                }
            }
            Stmt::Call { name, args } => {
                let rewritten_args = Self::rewrite_stmt_call_args(name, args);
                let positional_only = rewritten_args
                    .iter()
                    .all(|arg| matches!(arg, CallArg::Positional(_)));

                // Normalize mutating/structural call statements through Expr::Call
                // so they reuse call rewrites and method-based mutation paths.
                if positional_only && Self::is_normalized_stmt_call_name(name) {
                    let expr_args: Vec<Expr> = rewritten_args
                        .iter()
                        .filter_map(|arg| match arg {
                            CallArg::Positional(expr) => Some(expr.clone()),
                            _ => None,
                        })
                        .collect();
                    let call_expr = Expr::Call {
                        name: name.clone(),
                        args: expr_args,
                    };
                    self.compile_expr(&call_expr);
                    self.code.emit(OpCode::Pop);
                    return;
                }

                // Statement-level call: compile positional args only.
                // Fall back if named args or raw-expression args remain.
                if positional_only
                    && rewritten_args
                        .iter()
                        .all(|arg| matches!(arg, CallArg::Positional(_)))
                {
                    let arity = rewritten_args.len() as u32;
                    for arg in &rewritten_args {
                        if let CallArg::Positional(expr) = arg {
                            self.compile_expr(expr);
                        }
                    }
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::ExecCall { name_idx, arity });
                    return;
                }

                // Check for capture slip args (|var)
                let has_slip = rewritten_args
                    .iter()
                    .any(|arg| matches!(arg, CallArg::Slip(_)));
                if has_slip {
                    // Compile non-slip args first, then slip arg
                    let mut regular_count = 0u32;
                    for arg in &rewritten_args {
                        match arg {
                            CallArg::Positional(expr) => {
                                self.compile_expr(expr);
                                regular_count += 1;
                            }
                            CallArg::Named {
                                name: n,
                                value: Some(expr),
                            } => {
                                self.compile_expr(&Expr::Literal(Value::Str(n.clone())));
                                self.compile_expr(expr);
                                self.code.emit(OpCode::MakePair);
                                regular_count += 1;
                            }
                            CallArg::Named {
                                name: n,
                                value: None,
                            } => {
                                self.compile_expr(&Expr::Literal(Value::Str(n.clone())));
                                self.compile_expr(&Expr::Literal(Value::Bool(true)));
                                self.code.emit(OpCode::MakePair);
                                regular_count += 1;
                            }
                            CallArg::Slip(_) => {} // handled below
                        }
                    }
                    // Compile the slip expression (last one wins if multiple)
                    for arg in &rewritten_args {
                        if let CallArg::Slip(expr) = arg {
                            self.compile_expr(expr);
                        }
                    }
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::ExecCallSlip {
                        name_idx,
                        regular_arity: regular_count,
                    });
                    return;
                }

                // Statement-level call with named args: compile values and encode
                // named args as Pair(name => value), then dispatch without stmt_pool.
                for arg in &rewritten_args {
                    match arg {
                        CallArg::Positional(expr) => self.compile_expr(expr),
                        CallArg::Named {
                            name,
                            value: Some(expr),
                        } => {
                            self.compile_expr(&Expr::Literal(Value::Str(name.clone())));
                            self.compile_expr(expr);
                            self.code.emit(OpCode::MakePair);
                        }
                        CallArg::Named { name, value: None } => {
                            self.compile_expr(&Expr::Literal(Value::Str(name.clone())));
                            self.compile_expr(&Expr::Literal(Value::Bool(true)));
                            self.code.emit(OpCode::MakePair);
                        }
                        CallArg::Slip(_) => unreachable!(),
                    }
                }
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::ExecCallPairs {
                    name_idx,
                    arity: rewritten_args.len() as u32,
                });
            }
            // Loop control
            Stmt::Last(label) => {
                self.code.emit(OpCode::Last(label.clone()));
            }
            Stmt::Next(label) => {
                self.code.emit(OpCode::Next(label.clone()));
            }
            Stmt::Redo(label) => {
                self.code.emit(OpCode::Redo(label.clone()));
            }
            Stmt::Return(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Return);
            }
            Stmt::Die(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Die);
            }
            Stmt::Fail(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Fail);
            }
            Stmt::Proceed => {
                self.code.emit(OpCode::Proceed);
            }
            Stmt::Succeed => {
                self.code.emit(OpCode::Succeed);
            }
            // MatchAssign (~~=): coerce value to string
            Stmt::Assign {
                name,
                expr,
                op: AssignOp::MatchAssign,
            } if name != "*PID" => {
                self.compile_expr(expr);
                self.code.emit(OpCode::StrCoerce);
                self.emit_set_named_var(name);
            }
            Stmt::Assign { .. } => {
                self.code.emit(OpCode::AssignReadOnly);
            }
            // Given/When/Default
            Stmt::Given { topic, body } => {
                self.compile_expr(topic);
                let given_idx = self.code.emit(OpCode::Given { body_end: 0 });
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_body_end(given_idx);
            }
            Stmt::When { cond, body } => {
                self.compile_expr(cond);
                let when_idx = self.code.emit(OpCode::When { body_end: 0 });
                for (i, s) in body.iter().enumerate() {
                    let is_last = i == body.len() - 1;
                    if is_last {
                        if let Stmt::Expr(expr) = s {
                            self.compile_expr(expr);
                        } else {
                            self.compile_stmt(s);
                        }
                    } else {
                        self.compile_stmt(s);
                    }
                }
                self.code.patch_body_end(when_idx);
            }
            Stmt::Default(body) => {
                let default_idx = self.code.emit(OpCode::Default { body_end: 0 });
                for (i, s) in body.iter().enumerate() {
                    let is_last = i == body.len() - 1;
                    if is_last {
                        if let Stmt::Expr(expr) = s {
                            self.compile_expr(expr);
                        } else {
                            self.compile_stmt(s);
                        }
                    } else {
                        self.compile_stmt(s);
                    }
                }
                self.code.patch_body_end(default_idx);
            }
            // Repeat loop (repeat while / repeat until)
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } if *repeat => {
                let (pre_stmts, loop_body, post_stmts) = self.expand_loop_phasers(body);
                if let Some(init_stmt) = init {
                    self.compile_stmt(init_stmt);
                }
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                // Layout: [RepeatLoop] [body..] [cond..]
                let loop_idx = self.code.emit(OpCode::RepeatLoop {
                    cond_end: 0,
                    body_end: 0,
                    label: label.clone(),
                });
                // Compile body
                self.compile_body_with_implicit_try(&loop_body);
                self.code.patch_repeat_cond_end(loop_idx);
                // Compile condition (or push True if none)
                if let Some(cond_expr) = cond {
                    self.compile_expr(cond_expr);
                } else {
                    self.code.emit(OpCode::LoadTrue);
                }
                // Compile step (if any)
                if let Some(step_expr) = step {
                    self.compile_expr(step_expr);
                    self.code.emit(OpCode::Pop);
                }
                self.code.patch_loop_end(loop_idx);
                for s in &post_stmts {
                    self.compile_stmt(s);
                }
            }
            Stmt::Loop { .. } => unreachable!("loop repeat flag is exhaustive"),
            // --- No-ops: these statements are handled elsewhere ---
            // CATCH/CONTROL are extracted by compile_try/compile_body_with_implicit_try
            Stmt::Catch(_) | Stmt::Control(_) => {}
            // HasDecl/MethodDecl/DoesDecl outside class context are no-ops
            Stmt::HasDecl { .. } | Stmt::MethodDecl { .. } | Stmt::DoesDecl { .. } => {}

            // --- Take (gather/take) ---
            Stmt::Take(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Take);
            }

            // --- React: just run the body block ---
            Stmt::React { body } => {
                for s in body {
                    self.compile_stmt(s);
                }
            }

            // --- Package scope ---
            Stmt::Package { name, body } => {
                let qualified_name = self.qualify_package_name(name);
                if body.is_empty() {
                    // unit module/package — set package for the rest of the scope
                    self.current_package = qualified_name.clone();
                    // Register the package name so it's accessible as a value
                    let name_idx = self.code.add_constant(Value::Str(qualified_name.clone()));
                    self.code.emit(OpCode::RegisterPackage { name_idx });
                } else {
                    let name_idx = self.code.add_constant(Value::Str(qualified_name.clone()));
                    // Non-unit package declarations also produce a type object value.
                    self.code.emit(OpCode::RegisterPackage { name_idx });
                    let pkg_idx = self.code.emit(OpCode::PackageScope {
                        name_idx,
                        body_end: 0,
                    });
                    let saved_package = self.current_package.clone();
                    self.current_package = qualified_name;
                    for s in body {
                        self.compile_stmt(s);
                    }
                    self.current_package = saved_package;
                    self.code.patch_body_end(pkg_idx);
                }
            }

            // --- Phaser (BEGIN/END) ---
            Stmt::Phaser {
                kind: PhaserKind::Begin | PhaserKind::Check | PhaserKind::Init,
                body,
            } => {
                // BEGIN: compile body inline (runs immediately)
                for s in body {
                    self.compile_stmt(s);
                }
            }
            Stmt::Phaser {
                kind: PhaserKind::End,
                body,
            } => {
                // END: store body in stmt pool for deferred execution
                let end_stmt = Stmt::Phaser {
                    kind: PhaserKind::End,
                    body: body.clone(),
                };
                let idx = self.code.add_stmt(end_stmt);
                self.code.emit(OpCode::PhaserEnd(idx));
            }
            Stmt::Phaser { .. } => {}

            // --- SubDecl: delegate to interpreter AND compile body ---
            Stmt::SubDecl {
                name,
                params,
                param_defs,
                body,
                multi,
                ..
            } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterSub(idx));
                // Also compile the body to bytecode for VM-native dispatch
                self.compile_sub_body(name, params, param_defs, body, *multi);
            }
            Stmt::TokenDecl { .. } | Stmt::RuleDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterToken(idx));
            }
            Stmt::ProtoDecl {
                params,
                param_defs,
                body,
                ..
            } => {
                let _ = (params.len(), param_defs.len(), body.len());
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterProtoSub(idx));
            }
            Stmt::ProtoToken { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterProtoToken(idx));
            }
            Stmt::Use { module, arg } if module == "lib" && arg.is_some() => {
                if let Some(expr) = arg {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::UseLibPath);
                }
            }
            Stmt::Use { module, arg } if module == "lib" && arg.is_none() => {}
            Stmt::Use { module, arg } if module == "newline" => {
                if let Some(expr) = arg {
                    self.compile_expr(expr);
                    let name_idx = self
                        .code
                        .add_constant(Value::Str("__mutsu_set_newline".to_string()));
                    self.code.emit(OpCode::ExecCall { name_idx, arity: 1 });
                }
            }
            Stmt::Use { module, .. }
                if module == "v6"
                    || module == "customtrait"
                    || module == "isms"
                    || module == "MONKEY-TYPING"
                    || module == "nqp" => {}
            Stmt::Use { module, .. } if module == "Test" || module.starts_with("Test::") => {
                let name_idx = self.code.add_constant(Value::Str(module.clone()));
                self.code.emit(OpCode::UseModule(name_idx));
            }
            Stmt::Use { module, .. } => {
                let name_idx = self.code.add_constant(Value::Str(module.clone()));
                self.code.emit(OpCode::UseModule(name_idx));
            }
            Stmt::Need { module } => {
                let name_idx = self.code.add_constant(Value::Str(module.clone()));
                self.code.emit(OpCode::NeedModule(name_idx));
            }
            Stmt::EnumDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterEnum(idx));
            }
            Stmt::ClassDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterClass(idx));
            }
            Stmt::RoleDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterRole(idx));
            }
            Stmt::SubsetDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterSubset(idx));
            }
            Stmt::Subtest { name, body } => {
                self.compile_expr(name);
                let idx = self.code.emit(OpCode::SubtestScope { body_end: 0 });
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_body_end(idx);
            }
            Stmt::Whenever {
                supply,
                param,
                body,
            } => {
                self.compile_expr(supply);
                let body_idx = self.code.add_stmt(Stmt::Block(body.clone()));
                let param_idx = param
                    .as_ref()
                    .map(|p| self.code.add_constant(Value::Str(p.clone())));
                let target_var_idx = if let Expr::Var(name) = supply {
                    Some(self.code.add_constant(Value::Str(name.clone())))
                } else {
                    None
                };
                self.code.emit(OpCode::WheneverScope {
                    body_idx,
                    param_idx,
                    target_var_idx,
                });
            }
            Stmt::Let { name, index, value } => {
                // Emit LetSave: saves current value of the variable
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                let has_index = index.is_some();
                if let Some(idx_expr) = index {
                    self.compile_expr(idx_expr);
                }
                self.code.emit(OpCode::LetSave {
                    name_idx,
                    index_mode: has_index,
                });
                // Compile the assignment if value is provided
                if let Some(val_expr) = value {
                    if has_index {
                        // For array index assignment: compile as Stmt::Expr(IndexAssign)
                        let target_expr = if let Some(stripped) = name.strip_prefix('@') {
                            Expr::ArrayVar(stripped.to_string())
                        } else if let Some(stripped) = name.strip_prefix('%') {
                            Expr::Var(stripped.to_string())
                        } else {
                            Expr::Var(name.to_string())
                        };
                        let assign_expr = Expr::IndexAssign {
                            target: Box::new(target_expr),
                            index: Box::new(index.as_ref().unwrap().as_ref().clone()),
                            value: Box::new(val_expr.as_ref().clone()),
                        };
                        self.compile_expr(&assign_expr);
                        self.code.emit(OpCode::Pop);
                    } else {
                        self.compile_expr(val_expr);
                        self.emit_set_named_var(name);
                    }
                }
            }
            Stmt::TempMethodAssign {
                var_name,
                method_name,
                method_args,
                value,
            } => {
                let name_idx = self.code.add_constant(Value::Str(var_name.clone()));
                self.code.emit(OpCode::LetSave {
                    name_idx,
                    index_mode: false,
                });
                let assign_expr = Expr::Call {
                    name: "__mutsu_assign_method_lvalue".to_string(),
                    args: vec![
                        Expr::Var(var_name.clone()),
                        Expr::Literal(Value::Str(method_name.clone())),
                        Expr::ArrayLiteral(method_args.clone()),
                        value.clone(),
                        Expr::Literal(Value::Str(var_name.clone())),
                    ],
                };
                self.compile_expr(&assign_expr);
                self.code.emit(OpCode::Pop);
            }
        }
    }
}
