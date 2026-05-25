use super::*;
use crate::symbol::Symbol;

impl Compiler {
    pub(super) fn compile_expr_call(&mut self, name: &Symbol, args: &[Expr]) {
        // (state $x) = expr  /  (state @x) = expr  /  (state %x) = expr
        // The parser emits __mutsu_assign_callable_lvalue(DoStmt(VarDecl{..}), [], rhs).
        // We compile this as: declare the state var, then unconditionally assign the RHS.
        if name == "__mutsu_assign_callable_lvalue"
            && args.len() == 3
            && let Expr::DoStmt(stmt) = &args[0]
            && let Stmt::VarDecl { name: var_name, .. } = stmt.as_ref()
        {
            // 1. Compile the VarDecl (handles state init)
            self.compile_stmt(stmt);
            // VarDecl doesn't push to stack, so no pop needed.
            // 2. Compile the RHS value
            self.compile_expr(&args[2]);
            // 3. Dup so the assignment result is left on the stack
            self.code.emit(OpCode::Dup);
            // 4. Assign to the variable
            let name_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::AssignExpr(name_idx));
            return;
        } else if name == "__mutsu_assign_callable_lvalue"
            && args.len() == 3
            && let Expr::ArrayLiteral(targets) = &args[0]
            && targets.iter().all(|t| {
                matches!(
                    t,
                    Expr::Var(_)
                        | Expr::ArrayVar(_)
                        | Expr::HashVar(_)
                        | Expr::Whatever
                        | Expr::Index { .. }
                )
            })
        {
            // ($a, $b, ...) = expr -- list assignment to existing variables
            // 1. Compile the RHS and flatten to a list
            self.compile_expr(&args[2]);
            // Store RHS in a temp variable for indexing
            let tmp_name = format!(
                "__mutsu_destructure_assign_tmp_{}",
                self.code.constants.len()
            );
            let tmp_idx = self.code.add_constant(Value::str(tmp_name.clone()));
            self.code.emit(OpCode::Dup);
            self.code.emit(OpCode::SetGlobal(tmp_idx));
            // For each target variable, index into the RHS and assign
            for (i, target) in targets.iter().enumerate() {
                match target {
                    Expr::Var(var_name) => {
                        self.code.emit(OpCode::GetGlobal(tmp_idx));
                        let idx = self.code.add_constant(Value::Int(i as i64));
                        self.code.emit(OpCode::LoadConst(idx));
                        self.code.emit(OpCode::Index {
                            is_positional: true,
                        });
                        let name_idx = self.code.add_constant(Value::str(var_name.clone()));
                        self.code.emit(OpCode::AssignExpr(name_idx));
                    }
                    Expr::ArrayVar(var_name) => {
                        let rhs_expr = if i > 0 {
                            Expr::MethodCall {
                                target: Box::new(Expr::Var(tmp_name.clone())),
                                name: crate::symbol::Symbol::intern("skip"),
                                args: vec![Expr::Literal(Value::Int(i as i64))],
                                modifier: None,
                                quoted: false,
                            }
                        } else {
                            Expr::Var(tmp_name.clone())
                        };
                        self.compile_expr(&rhs_expr);
                        let name_idx = self.code.add_constant(Value::str(format!("@{}", var_name)));
                        self.code.emit(OpCode::AssignExpr(name_idx));
                    }
                    Expr::HashVar(var_name) => {
                        self.code.emit(OpCode::GetGlobal(tmp_idx));
                        let idx = self.code.add_constant(Value::Int(i as i64));
                        self.code.emit(OpCode::LoadConst(idx));
                        self.code.emit(OpCode::Index {
                            is_positional: true,
                        });
                        let name_idx = self.code.add_constant(Value::str(format!("%{}", var_name)));
                        self.code.emit(OpCode::AssignExpr(name_idx));
                    }
                    Expr::Index {
                        target,
                        index,
                        is_positional,
                    } => {
                        let rhs_item = Expr::Index {
                            target: Box::new(Expr::Var(tmp_name.clone())),
                            index: Box::new(Expr::Literal(Value::Int(i as i64))),
                            is_positional: true,
                        };
                        self.compile_expr(&Expr::IndexAssign {
                            target: target.clone(),
                            index: index.clone(),
                            value: Box::new(rhs_item),
                            is_positional: *is_positional,
                        });
                        self.code.emit(OpCode::Pop);
                    }
                    _ => {}
                }
            }
            // Leave the RHS list on the stack as the result
            self.code.emit(OpCode::GetGlobal(tmp_idx));
            return;
        } else if name == "__mutsu_assign_method_lvalue"
            && args.len() >= 4
            && let Expr::Index {
                target: index_target,
                index: index_key,
                ..
            } = &args[0]
            && let Some(var_name) = Self::postfix_index_name(index_target)
        {
            let tmp_target_name = format!(
                "__mutsu_tmp_assign_method_target_{}",
                self.code.constants.len()
            );
            let tmp_result_name = format!(
                "__mutsu_tmp_assign_method_result_{}",
                self.code.constants.len()
            );
            let tmp_target_idx = self.code.add_constant(Value::str(tmp_target_name.clone()));
            let tmp_result_idx = self.code.add_constant(Value::str(tmp_result_name.clone()));
            let call_name_idx = self.code.add_constant(Value::str(name.resolve()));
            let var_name_idx = self.code.add_constant(Value::str(var_name));

            self.compile_expr(&args[0]);
            self.code.emit(OpCode::SetGlobal(tmp_target_idx));

            self.code.emit(OpCode::GetGlobal(tmp_target_idx));
            self.compile_expr(&args[1]);
            self.compile_expr(&args[2]);
            self.compile_expr(&args[3]);
            self.code.emit(OpCode::LoadConst(tmp_target_idx));
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity: 5,
                arg_sources_idx: None,
            });
            self.code.emit(OpCode::SetGlobal(tmp_result_idx));

            self.code.emit(OpCode::GetGlobal(tmp_target_idx));
            self.compile_expr(index_key);
            self.code.emit(OpCode::IndexAssignExprNamed {
                name_idx: var_name_idx,
                is_positional: true,
            });
            self.code.emit(OpCode::Pop);
            self.code.emit(OpCode::GetGlobal(tmp_result_idx));
            return;
        } else if name == "atomic-fetch"
            && args.len() == 1
            && let Expr::Var(var_name) = &args[0]
        {
            let call_name_idx = self
                .code
                .add_constant(Value::str_from("__mutsu_atomic_fetch_var"));
            let arg_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LoadConst(arg_idx));
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity: 1,
                arg_sources_idx: None,
            });
            return;
        } else if name == "atomic-assign"
            && args.len() == 2
            && let Expr::Var(var_name) = &args[0]
        {
            let call_name_idx = self
                .code
                .add_constant(Value::str_from("__mutsu_atomic_store_var"));
            let arg_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LoadConst(arg_idx));
            self.compile_expr(&args[1]);
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity: 2,
                arg_sources_idx: None,
            });
            return;
        } else if name == "atomic-fetch-inc"
            && args.len() == 1
            && let Expr::Var(var_name) = &args[0]
        {
            let call_name_idx = self
                .code
                .add_constant(Value::str_from("__mutsu_atomic_post_inc_var"));
            let arg_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LoadConst(arg_idx));
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity: 1,
                arg_sources_idx: None,
            });
            return;
        } else if name == "atomic-inc-fetch"
            && args.len() == 1
            && let Expr::Var(var_name) = &args[0]
        {
            let call_name_idx = self
                .code
                .add_constant(Value::str_from("__mutsu_atomic_pre_inc_var"));
            let arg_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LoadConst(arg_idx));
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity: 1,
                arg_sources_idx: None,
            });
            return;
        } else if name == "atomic-fetch-dec"
            && args.len() == 1
            && let Expr::Var(var_name) = &args[0]
        {
            let call_name_idx = self
                .code
                .add_constant(Value::str_from("__mutsu_atomic_post_dec_var"));
            let arg_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LoadConst(arg_idx));
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity: 1,
                arg_sources_idx: None,
            });
            return;
        } else if name == "atomic-dec-fetch"
            && args.len() == 1
            && let Expr::Var(var_name) = &args[0]
        {
            let call_name_idx = self
                .code
                .add_constant(Value::str_from("__mutsu_atomic_pre_dec_var"));
            let arg_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LoadConst(arg_idx));
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity: 1,
                arg_sources_idx: None,
            });
            return;
        } else if name == "atomic-fetch-add"
            && args.len() == 2
            && let Expr::Var(var_name) = &args[0]
        {
            let call_name_idx = self
                .code
                .add_constant(Value::str_from("__mutsu_atomic_fetch_add_var"));
            let arg_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LoadConst(arg_idx));
            self.compile_expr(&args[1]);
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity: 2,
                arg_sources_idx: None,
            });
            return;
        } else if name == "atomic-add-fetch"
            && args.len() == 2
            && let Expr::Var(var_name) = &args[0]
        {
            let call_name_idx = self
                .code
                .add_constant(Value::str_from("__mutsu_atomic_add_var"));
            let arg_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LoadConst(arg_idx));
            self.compile_expr(&args[1]);
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity: 2,
                arg_sources_idx: None,
            });
            return;
        }
        // Rewrite cas($var, ...) -> __mutsu_cas_var($var_name_str, ...)
        if name == "cas"
            && (args.len() == 2 || args.len() == 3)
            && let Some(var_name) = Self::atomic_target_name(&args[0])
        {
            if args.len() == 2
                && let Expr::Lambda { param, body, .. } = &args[1]
                && let [Stmt::Expr(Expr::Binary { left, op, right })] = body.as_slice()
                && *op == TokenKind::Plus
            {
                let delta = match (left.as_ref(), right.as_ref()) {
                    (Expr::Var(lhs), rhs) if lhs == param => Some(rhs.clone()),
                    (lhs, Expr::Var(rhs)) if rhs == param => Some(lhs.clone()),
                    _ => None,
                };
                if let Some(delta) = delta {
                    let call_name_idx = self
                        .code
                        .add_constant(Value::str_from("__mutsu_atomic_add_var"));
                    let name_idx = self.code.add_constant(Value::str(var_name.clone()));
                    self.code.emit(OpCode::LoadConst(name_idx));
                    self.compile_expr(&delta);
                    self.code.emit(OpCode::CallFunc {
                        name_idx: call_name_idx,
                        arity: 2,
                        arg_sources_idx: None,
                    });
                    return;
                }
            }
            // For attribute variables (!attr), sync the local to env before
            // the CAS call so the builtin can read the current value from env.
            if var_name.starts_with('!')
                && let Some(&slot) = self.local_map.get(var_name.as_str())
            {
                self.code.emit(OpCode::GetLocal(slot));
                let sync_name_idx = self.code.add_constant(Value::str(var_name.clone()));
                self.code.emit(OpCode::SetGlobal(sync_name_idx));
                self.code.emit(OpCode::Pop);
            }
            let call_name_idx = self.code.add_constant(Value::str_from("__mutsu_cas_var"));
            let name_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::LoadConst(name_idx));
            for arg in &args[1..] {
                self.compile_expr(arg);
            }
            let arity = args.len() as u32; // var_name + remaining args
            self.code.emit(OpCode::CallFunc {
                name_idx: call_name_idx,
                arity,
                arg_sources_idx: None,
            });
            return;
        }
        // Rewrite undefine($var) -> $var = Any (assign type object to the variable)
        if name == "undefine" && args.len() == 1 {
            // Handle `undefine temp $var` — parsed as undefine(temp($var))
            // Emit: LetSave (temp), then $var = Any
            if let Expr::Call {
                name: inner_name,
                args: inner_args,
            } = &args[0]
                && inner_name.resolve() == "temp"
                && inner_args.len() == 1
            {
                let inner_var = match &inner_args[0] {
                    Expr::Var(n) => Some(n.clone()),
                    Expr::ArrayVar(n) => Some(format!("@{}", n)),
                    Expr::HashVar(n) => Some(format!("%{}", n)),
                    _ => None,
                };
                if let Some(vname) = inner_var {
                    // Compile: temp $var (LetSave)
                    let let_stmt = Stmt::Let {
                        name: vname.clone(),
                        index: None,
                        value: None,
                        is_temp: true,
                    };
                    self.compile_stmt(&let_stmt);
                    // Then undefine the variable (assign Any type object)
                    if vname.starts_with('@') || vname.starts_with('%') {
                        let name_idx = self.code.add_constant(Value::str(vname));
                        self.code.emit(OpCode::UndefineAggregate(name_idx));
                    } else {
                        let any_idx = self
                            .code
                            .add_constant(Value::Package(crate::symbol::Symbol::intern("Any")));
                        self.code.emit(OpCode::LoadConst(any_idx));
                        let name_idx = self.code.add_constant(Value::str(vname));
                        self.code.emit(OpCode::AssignExpr(name_idx));
                    }
                    return;
                }
            }
            let var_name = match &args[0] {
                Expr::Var(n) => Some(n.clone()),
                Expr::ArrayVar(n) => Some(format!("@{}", n)),
                Expr::HashVar(n) => Some(format!("%{}", n)),
                _ => None,
            };
            if matches!(&args[0], Expr::CodeVar(_)) {
                // undefine &sub -- always dies (subs are immutable)
                self.compile_expr(&args[0]);
                let builtin_name = self
                    .code
                    .add_constant(Value::str("__mutsu_undefine_rvalue".to_string()));
                self.code.emit(OpCode::CallFunc {
                    name_idx: builtin_name,
                    arity: 1,
                    arg_sources_idx: None,
                });
            } else if let Some(vname) = var_name {
                // For @/% variables, clear in-place so references see the change.
                // For scalars, assign the type object Any (undefined but not Nil).
                if vname.starts_with('@') || vname.starts_with('%') {
                    let name_idx = self.code.add_constant(Value::str(vname));
                    self.code.emit(OpCode::UndefineAggregate(name_idx));
                } else {
                    let any_idx = self
                        .code
                        .add_constant(Value::Package(crate::symbol::Symbol::intern("Any")));
                    self.code.emit(OpCode::LoadConst(any_idx));
                    let name_idx = self.code.add_constant(Value::str(vname));
                    self.code.emit(OpCode::AssignExpr(name_idx));
                }
            } else if matches!(&args[0], Expr::Index { target, .. } if matches!(**target, Expr::HashVar(_) | Expr::ArrayVar(_) | Expr::Var(_)))
            {
                // undefine %hash<key> or undefine @arr[idx] -> target[index] = Nil
                if let Expr::Index {
                    target,
                    index,
                    is_positional: _,
                } = &args[0]
                {
                    let assign_expr = Expr::IndexAssign {
                        target: target.clone(),
                        index: index.clone(),
                        value: Box::new(Expr::Literal(Value::Nil)),
                        is_positional: true,
                    };
                    self.compile_expr(&assign_expr);
                }
            } else {
                // Evaluate the expression and check at runtime whether the
                // result is mutable.  Immutable values (Bool, Int, etc.)
                // must die with "Cannot modify an immutable <Type>".
                self.compile_expr(&args[0]);
                let builtin_name = self
                    .code
                    .add_constant(Value::str("__mutsu_undefine_rvalue".to_string()));
                self.code.emit(OpCode::CallFunc {
                    name_idx: builtin_name,
                    arity: 1,
                    arg_sources_idx: None,
                });
            }
        }
        // Rewrite shift(@arr)/pop(@arr) -> @arr.shift()/@arr.pop() for mutability
        else if matches!(name.resolve().as_str(), "shift" | "pop")
            && args.len() == 1
            && matches!(args[0], Expr::ArrayVar(_) | Expr::Var(_))
        {
            let method_call = Expr::MethodCall {
                target: Box::new(args[0].clone()),
                name: *name,
                args: Vec::new(),
                modifier: None,
                quoted: false,
            };
            self.compile_expr(&method_call);
        }
        // Rewrite push($obj.attr, val...)/unshift/append/prepend on method call targets.
        // Compile as __mutsu_push_through_accessor($obj, "attr", "push", vals...) so the
        // mutation propagates to all instances sharing the same array container.
        else if args.len() >= 2
            && matches!(
                name.resolve().as_str(),
                "push" | "unshift" | "append" | "prepend"
            )
            && matches!(&args[0], Expr::MethodCall { args: mc_args, .. } if mc_args.is_empty())
        {
            if let Expr::MethodCall {
                target: mc_target,
                name: mc_name,
                ..
            } = &args[0]
            {
                let builtin_name = Symbol::intern("__mutsu_push_through_accessor");
                let mut new_args: Vec<Expr> = vec![
                    (**mc_target).clone(),
                    Expr::Literal(Value::str(mc_name.resolve())),
                    Expr::Literal(Value::str(name.resolve())),
                ];
                for arg in &args[1..] {
                    new_args.push(arg.clone());
                }
                let call = Expr::Call {
                    name: builtin_name,
                    args: new_args,
                };
                self.compile_expr(&call);
            }
        }
        // Rewrite push(@arr, val...)/unshift(@arr, val...)/append/prepend/splice -> @arr.method(val...)
        // splice needs only 1 arg (the array); others need at least 2
        else if !args.is_empty()
            && matches!(
                args[0],
                Expr::ArrayVar(_) | Expr::Var(_) | Expr::Index { .. }
            )
            && (matches!(
                name.resolve().as_str(),
                "push" | "unshift" | "append" | "prepend"
            ) && args.len() >= 2
                || name.resolve().as_str() == "splice")
        {
            // Autovivification: when the first arg is an `Expr::Index` referring
            // to a missing slot (e.g. `push @array[2], ...` / `push %h<key>, ...`),
            // we must create an Array in that slot before the method call.
            // Rewrite as `<slot> = <slot> // []; <slot>.method(args)`.
            if let Expr::Index { .. } = &args[0]
                && matches!(
                    name.resolve().as_str(),
                    "push" | "unshift" | "append" | "prepend"
                )
            {
                let slot = args[0].clone();
                let viv_assign = Expr::IndexAssign {
                    target: match &slot {
                        Expr::Index { target, .. } => target.clone(),
                        _ => unreachable!(),
                    },
                    index: match &slot {
                        Expr::Index { index, .. } => index.clone(),
                        _ => unreachable!(),
                    },
                    value: Box::new(Expr::Binary {
                        left: Box::new(slot.clone()),
                        op: TokenKind::SlashSlash,
                        right: Box::new(Expr::BracketArray(Vec::new(), false)),
                    }),
                    is_positional: match &slot {
                        Expr::Index { is_positional, .. } => *is_positional,
                        _ => true,
                    },
                };
                let method_call = Expr::MethodCall {
                    target: Box::new(slot.clone()),
                    name: *name,
                    args: args[1..].to_vec(),
                    modifier: None,
                    quoted: false,
                };
                // Write the method call result back to the slot so that
                // nested hash mutations (e.g. `push %h<a><b>, 1, 2`)
                // are visible through the parent hash.
                let writeback = Expr::IndexAssign {
                    target: match &slot {
                        Expr::Index { target, .. } => target.clone(),
                        _ => unreachable!(),
                    },
                    index: match &slot {
                        Expr::Index { index, .. } => index.clone(),
                        _ => unreachable!(),
                    },
                    value: Box::new(method_call),
                    is_positional: match &slot {
                        Expr::Index { is_positional, .. } => *is_positional,
                        _ => true,
                    },
                };
                let do_block = Expr::DoBlock {
                    body: vec![Stmt::Expr(viv_assign), Stmt::Expr(writeback)],
                    label: None,
                };
                self.compile_expr(&do_block);
            } else {
                let method_call = Expr::MethodCall {
                    target: Box::new(args[0].clone()),
                    name: *name,
                    args: args[1..].to_vec(),
                    modifier: None,
                    quoted: false,
                };
                self.compile_expr(&method_call);
            }
        }
        // sink: evaluate the expression (including calling blocks), discard result, push Nil
        else if name == "sink" && args.len() == 1 {
            match &args[0] {
                Expr::AnonSub { body, .. } => {
                    // sink { ... } -- execute the block body inline via do block
                    let do_block = Expr::DoBlock {
                        body: body.clone(),
                        label: None,
                    };
                    self.compile_expr(&do_block);
                    // Discard the block result and push Nil
                    self.code.emit(OpCode::Pop);
                    self.code.emit(OpCode::LoadNil);
                }
                _ => {
                    // sink expr -- evaluate, discard, push Nil
                    // SinkPop throws unhandled Failures
                    self.compile_expr(&args[0]);
                    self.code.emit(OpCode::SinkPop);
                    self.code.emit(OpCode::LoadNil);
                }
            }
        }
        // Rewrite indir($path, { ... }) body into a callable block value so
        // call_function("indir", ...) can execute it after switching $*CWD.
        else if name == "indir" && args.len() >= 2 {
            let mut rewritten_args = args.to_vec();
            if let Expr::Block(body) = &rewritten_args[1] {
                rewritten_args[1] = make_anon_sub(body.clone());
            }
            let arity = rewritten_args.len() as u32;
            let arg_sources_idx = self.add_arg_sources_constant(&rewritten_args);
            for arg in &rewritten_args {
                self.compile_call_arg(arg);
            }
            let name_idx = self.code.add_constant(Value::str(name.resolve()));
            self.code.emit(OpCode::CallFunc {
                name_idx,
                arity,
                arg_sources_idx,
            });
        }
        // Rewrite cas($target, $expected, $new)
        else if name == "cas" && args.len() == 3 {
            // For array element CAS: cas(@arr[idx], $expected, $new)
            // Emit __mutsu_cas_array_elem("@arr_name", idx, expected, new)
            // Single-dim array element CAS: cas(@arr[idx], $expected, $new)
            if let Expr::Index {
                target,
                index,
                is_positional: _,
            } = &args[0]
                && let Some(arr_name) = match target.as_ref() {
                    Expr::ArrayVar(n) => Some(format!("@{}", n)),
                    Expr::Var(n) if n.starts_with('@') => Some(n.clone()),
                    _ => None,
                }
            {
                let call_name_idx = self
                    .code
                    .add_constant(Value::str_from("__mutsu_cas_array_elem"));
                let name_idx = self.code.add_constant(Value::str(arr_name));
                self.code.emit(OpCode::LoadConst(name_idx));
                self.compile_expr(index);
                self.compile_expr(&args[1]);
                self.compile_expr(&args[2]);
                self.code.emit(OpCode::CallFunc {
                    name_idx: call_name_idx,
                    arity: 4,
                    arg_sources_idx: None,
                });
            }
            // Multi-dim array element CAS: cas(@arr[d1;d2;...], $expected, $new)
            else if let Expr::MultiDimIndex { target, dimensions } = &args[0]
                && let Some(arr_name) = match target.as_ref() {
                    Expr::ArrayVar(n) => Some(format!("@{}", n)),
                    Expr::Var(n) if n.starts_with('@') => Some(n.clone()),
                    _ => None,
                }
            {
                // Pass dimensions as a list: __mutsu_cas_array_multidim("@arr", [d1,d2,...], expected, new)
                let call_name_idx = self
                    .code
                    .add_constant(Value::str_from("__mutsu_cas_array_multidim"));
                let name_idx = self.code.add_constant(Value::str(arr_name));
                self.code.emit(OpCode::LoadConst(name_idx));
                // Build the dimensions as a list
                for dim in dimensions {
                    self.compile_expr(dim);
                }
                let dim_count = dimensions.len() as u32;
                self.code.emit(OpCode::MakeArray(dim_count));
                self.compile_expr(&args[1]);
                self.compile_expr(&args[2]);
                self.code.emit(OpCode::CallFunc {
                    name_idx: call_name_idx,
                    arity: 4,
                    arg_sources_idx: None,
                });
            } else {
                let assign_stmt = match &args[0] {
                    Expr::Var(target_name) => Some(Stmt::Assign {
                        name: target_name.clone(),
                        expr: args[2].clone(),
                        op: AssignOp::Assign,
                    }),
                    Expr::Index {
                        target,
                        index,
                        is_positional: _,
                    } => Some(Stmt::Expr(Expr::IndexAssign {
                        target: target.clone(),
                        index: index.clone(),
                        value: Box::new(args[2].clone()),
                        is_positional: true,
                    })),
                    _ => None,
                };
                if let Some(assign_stmt) = assign_stmt {
                    let seen_name = format!(
                        "__mutsu_cas_seen_{}",
                        STATE_COUNTER.fetch_add(1, Ordering::Relaxed)
                    );
                    let cas_expr = Expr::DoBlock {
                        body: vec![
                            Stmt::VarDecl {
                                name: seen_name.clone(),
                                expr: args[0].clone(),
                                type_constraint: None,
                                is_state: false,
                                is_our: false,
                                is_dynamic: false,
                                is_export: false,
                                export_tags: Vec::new(),
                                custom_traits: Vec::new(),
                                where_constraint: None,
                            },
                            Stmt::If {
                                cond: Expr::Binary {
                                    left: Box::new(Expr::Var(seen_name.clone())),
                                    op: TokenKind::EqEq,
                                    right: Box::new(args[1].clone()),
                                },
                                then_branch: vec![assign_stmt],
                                else_branch: vec![],
                                binding_var: None,
                            },
                            Stmt::Expr(Expr::Var(seen_name)),
                        ],
                        label: None,
                    };
                    self.compile_expr(&cas_expr);
                } else {
                    let arity = args.len() as u32;
                    let arg_sources_idx = self.add_arg_sources_constant(args);
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    let name_idx = self.code.add_constant(Value::str(name.resolve()));
                    self.code.emit(OpCode::CallFunc {
                        name_idx,
                        arity,
                        arg_sources_idx,
                    });
                }
            }
        }
        // Rewrite cas($var, &fn) to assignment expression:
        // $var = fn($var)
        else if name == "cas" && args.len() == 2 {
            let var_name = Self::atomic_target_name(&args[0]);
            if let Some(vname) = var_name {
                if let Expr::Lambda { param, body, .. } = &args[1]
                    && let [Stmt::Expr(Expr::Binary { left, op, right })] = body.as_slice()
                    && *op == TokenKind::Plus
                {
                    let delta = match (left.as_ref(), right.as_ref()) {
                        (Expr::Var(lhs), rhs) if lhs == param => Some(rhs.clone()),
                        (lhs, Expr::Var(rhs)) if rhs == param => Some(lhs.clone()),
                        _ => None,
                    };
                    if let Some(delta) = delta {
                        let atomic_add = Expr::Call {
                            name: Symbol::intern("__mutsu_atomic_add_var"),
                            args: vec![Expr::Literal(Value::str(vname.clone())), delta],
                        };
                        self.compile_expr(&atomic_add);
                        return;
                    }
                }
                let assign_expr = Expr::AssignExpr {
                    name: vname,
                    expr: Box::new(Expr::CallOn {
                        target: Box::new(args[1].clone()),
                        args: vec![args[0].clone()],
                    }),
                    is_bind: false,
                };
                self.compile_expr(&assign_expr);
            } else if let Expr::Index {
                target,
                index,
                is_positional: _,
            } = &args[0]
                && let Some(hash_name) = match target.as_ref() {
                    Expr::HashVar(n) => Some(format!("%{}", n)),
                    Expr::Var(n) if n.starts_with('%') => Some(n.clone()),
                    _ => None,
                }
            {
                // cas(%hash{key}, &code) -> __mutsu_cas_hash_elem("%hash", key, code)
                let call_name_idx = self
                    .code
                    .add_constant(Value::str_from("__mutsu_cas_hash_elem"));
                let name_idx = self.code.add_constant(Value::str(hash_name));
                self.code.emit(OpCode::LoadConst(name_idx));
                self.compile_expr(index);
                self.compile_expr(&args[1]);
                self.code.emit(OpCode::CallFunc {
                    name_idx: call_name_idx,
                    arity: 3,
                    arg_sources_idx: None,
                });
            } else {
                let arity = args.len() as u32;
                let arg_sources_idx = self.add_arg_sources_constant(args);
                for arg in args {
                    self.compile_expr(arg);
                }
                let name_idx = self.code.add_constant(Value::str(name.resolve()));
                self.code.emit(OpCode::CallFunc {
                    name_idx,
                    arity,
                    arg_sources_idx,
                });
            }
        } else {
            // Rewrite VAR($var)/VAR(@var)/VAR(%var)/VAR(&var) -> $var.VAR, etc.
            if name == "VAR"
                && args.len() == 1
                && matches!(
                    args[0],
                    Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::CodeVar(_)
                )
            {
                let method_call = Expr::MethodCall {
                    target: Box::new(args[0].clone()),
                    name: Symbol::intern("VAR"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                };
                self.compile_expr(&method_call);
                return;
            }
            // Check for capture slip args (|c)
            let has_slip = args.iter().any(|arg| {
                matches!(
                    arg,
                    Expr::Unary {
                        op: TokenKind::Pipe,
                        ..
                    }
                )
            });
            if has_slip {
                // Compile all args in source order, tracking the slip position
                let mut regular_count = 0u32;
                let mut slip_pos: Option<u32> = None;
                let mut stack_idx = 0u32;
                for arg in args {
                    if let Expr::Unary {
                        op: TokenKind::Pipe,
                        expr,
                    } = arg
                    {
                        slip_pos = Some(stack_idx);
                        self.compile_expr(expr);
                        stack_idx += 1;
                    } else {
                        self.compile_call_arg(arg);
                        regular_count += 1;
                        stack_idx += 1;
                    }
                }
                let name_idx = self.code.add_constant(Value::str(name.resolve()));
                self.code.emit(OpCode::CallFuncSlip {
                    name_idx,
                    regular_arity: regular_count,
                    arg_sources_idx: None,
                    slip_pos,
                });
            } else {
                let arity = args.len() as u32;
                let arg_sources_idx = self.add_arg_sources_constant(args);
                for arg in args {
                    self.compile_call_arg(arg);
                }
                let name_idx = self.code.add_constant(Value::str(name.resolve()));
                self.code.emit(OpCode::CallFunc {
                    name_idx,
                    arity,
                    arg_sources_idx,
                });
                // Emit writeback for any Index expressions that were passed
                // as `is rw` arguments (temp variable -> original slot).
                self.emit_index_rw_writebacks();
            }
        }
    }
}
