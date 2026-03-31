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
            && targets.iter().all(|t| matches!(t, Expr::Var(_)))
        {
            // ($a, $b, ...) = expr -- list assignment to existing variables
            // 1. Compile the RHS and flatten to a list
            self.compile_expr(&args[2]);
            // Store RHS in a temp variable for indexing
            let tmp_name = format!(
                "__mutsu_destructure_assign_tmp_{}",
                self.code.constants.len()
            );
            let tmp_idx = self.code.add_constant(Value::str(tmp_name));
            self.code.emit(OpCode::Dup);
            self.code.emit(OpCode::SetGlobal(tmp_idx));
            // For each target variable, index into the RHS and assign
            for (i, target) in targets.iter().enumerate() {
                if let Expr::Var(var_name) = target {
                    self.code.emit(OpCode::GetGlobal(tmp_idx));
                    let idx = self.code.add_constant(Value::Int(i as i64));
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Index);
                    let name_idx = self.code.add_constant(Value::str(var_name.clone()));
                    self.code.emit(OpCode::AssignExpr(name_idx));
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
            self.code.emit(OpCode::IndexAssignExprNamed(var_name_idx));
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
        // Rewrite undefine($var) -> $var = Nil (assign Nil to the variable)
        if name == "undefine" && args.len() == 1 {
            let var_name = match &args[0] {
                Expr::Var(n) => Some(n.clone()),
                Expr::ArrayVar(n) => Some(format!("@{}", n)),
                Expr::HashVar(n) => Some(format!("%{}", n)),
                Expr::CodeVar(n) => Some(format!("&{}", n)),
                _ => None,
            };
            if let Some(vname) = var_name {
                // Push Nil and assign to the variable
                self.code.emit(OpCode::LoadNil);
                let name_idx = self.code.add_constant(Value::str(vname));
                self.code.emit(OpCode::AssignExpr(name_idx));
            } else if matches!(&args[0], Expr::Index { target, .. } if matches!(**target, Expr::HashVar(_) | Expr::ArrayVar(_) | Expr::Var(_)))
            {
                // undefine %hash<key> or undefine @arr[idx] -> target[index] = Nil
                if let Expr::Index { target, index } = &args[0] {
                    let assign_expr = Expr::IndexAssign {
                        target: target.clone(),
                        index: index.clone(),
                        value: Box::new(Expr::Literal(Value::Nil)),
                    };
                    self.compile_expr(&assign_expr);
                }
            } else {
                self.code.emit(OpCode::LoadNil);
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
            let method_call = Expr::MethodCall {
                target: Box::new(args[0].clone()),
                name: *name,
                args: args[1..].to_vec(),
                modifier: None,
                quoted: false,
            };
            self.compile_expr(&method_call);
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
            if let Expr::Index { target, index } = &args[0]
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
                    Expr::Index { target, index } => Some(Stmt::Expr(Expr::IndexAssign {
                        target: target.clone(),
                        index: index.clone(),
                        value: Box::new(args[2].clone()),
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
                };
                self.compile_expr(&assign_expr);
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
            }
        }
    }
}
