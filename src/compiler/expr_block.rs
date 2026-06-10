use super::*;

impl Compiler {
    /// Compile DoStmt expression (do { ... }, do if, do for, etc.).
    pub(super) fn compile_expr_do_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } if Self::do_if_branch_supported(then_branch)
                && Self::do_if_branch_supported(else_branch) =>
            {
                self.compile_do_if_expr(cond, then_branch, else_branch);
            }
            Stmt::Given { topic, body } => {
                self.compile_expr(topic);
                if let Some(source_name) = match topic {
                    Expr::Var(name) => Some(name.clone()),
                    Expr::ArrayVar(name) => Some(format!("@{}", name)),
                    Expr::HashVar(name) => Some(format!("%{}", name)),
                    _ => None,
                } {
                    let name_idx = self.code.add_constant(Value::str(source_name));
                    self.code.emit(OpCode::TagContainerRef(name_idx));
                }
                let given_idx = self.code.emit(OpCode::DoGivenExpr { body_end: 0 });
                self.compile_block_inline(body);
                self.code.patch_body_end(given_idx);
            }
            Stmt::Assign { name, expr, .. } => {
                // do $var = expr -> returns the assigned value
                self.compile_expr(expr);
                // Duplicate the value so we can assign AND return it
                self.code.emit(OpCode::Dup);
                self.emit_set_named_var(name);
            }
            Stmt::VarDecl {
                name,
                expr,
                is_state,
                is_our,
                is_dynamic: ast_is_dynamic,
                type_constraint,
                custom_traits,
                ..
            } => {
                let is_dynamic = *ast_is_dynamic || self.var_is_dynamic(name);
                // my $x = expr in expression context -> declare, assign, return value
                if *is_state {
                    self.compile_expr(expr);
                    let slot = self.alloc_local(name);
                    let ip = self.code.ops.len();
                    let key = format!("__state_{}::{}@{}", self.current_package, name, ip);
                    let key_idx = self.code.add_constant(Value::str(key.clone()));
                    self.code.state_locals.push((slot as usize, key.clone()));
                    self.code.emit(OpCode::StateVarInit(slot, key_idx));
                    self.code.emit(OpCode::GetLocal(slot));
                } else if name.starts_with('@') || name.starts_with('%') {
                    let marker_name = format!(
                        "__do_decl_init_{}",
                        STATE_COUNTER.fetch_add(1, Ordering::Relaxed)
                    );
                    let marker_slot = self.alloc_local(&marker_name);
                    // Container declarations in expression position (`my @a`) should
                    // initialize once per declaration site and then keep using the
                    // current lexical value on repeated evaluation (e.g. in loops).
                    self.code.emit(OpCode::GetLocal(marker_slot));
                    let jump_have_value = self.code.emit(OpCode::JumpIfNotNil(0));
                    self.code.emit(OpCode::Pop);
                    self.compile_expr(expr);
                    let name_idx = self.code.add_constant(Value::str(name.clone()));
                    self.code.emit(OpCode::SetGlobal(name_idx));
                    // Read back the coerced value (SetGlobal coerces list->hash for %)
                    let name_idx2 = self.code.add_constant(Value::str(name.clone()));
                    if name.starts_with('@') {
                        self.code.emit(OpCode::GetArrayVar(name_idx2));
                    } else {
                        self.code.emit(OpCode::GetHashVar(name_idx2));
                    }
                    self.code.emit(OpCode::Dup);
                    self.code.emit(OpCode::SetLocal(marker_slot));
                    let jump_end = self.code.emit(OpCode::Jump(0));
                    self.code.patch_jump(jump_have_value);
                    self.code.emit(OpCode::Pop);
                    let name_idx = self.code.add_constant(Value::str(name.clone()));
                    if name.starts_with('@') {
                        self.code.emit(OpCode::GetArrayVar(name_idx));
                    } else {
                        self.code.emit(OpCode::GetHashVar(name_idx));
                    }
                    self.code.patch_jump(jump_end);
                } else {
                    // For `our` redeclarations with no initializer (expr is Nil),
                    // load the existing package variable value instead of Nil.
                    let is_our_redecl_nil = *is_our && matches!(expr, Expr::Literal(Value::Nil));
                    if is_our_redecl_nil {
                        let qualified = self.qualify_variable_name(name);
                        let idx = self.code.add_constant(Value::str(qualified));
                        self.code.emit(OpCode::GetOurVar(idx));
                    } else {
                        self.compile_expr(expr);
                    }
                    if *is_our {
                        self.code.emit(OpCode::Dup); // for return value
                        self.code.emit(OpCode::Dup); // for SetGlobal
                        self.emit_set_named_var(name);
                        let qualified = self.qualify_variable_name(name);
                        let slot_opt = self.local_map.get(name).copied();
                        if let Some(slot) = slot_opt {
                            self.code
                                .our_locals
                                .push((slot as usize, qualified.clone()));
                        }
                        let idx = self.code.add_constant(Value::str(qualified));
                        self.code.emit(OpCode::SetGlobal(idx));
                    } else {
                        // For native int types, the value may be wrapped during
                        // assignment (e.g., -1 -> 255 for uint8). We need to
                        // return the wrapped value, so emit TypeCheck (which
                        // wraps on the stack) then Dup, then store.
                        let is_native_int = type_constraint
                            .as_ref()
                            .is_some_and(|tc| crate::runtime::native_types::is_native_int_type(tc));
                        if is_native_int {
                            let tc = type_constraint.as_ref().unwrap();
                            // Set type constraint so future assignments also wrap
                            let name_idx2 = self.code.add_constant(Value::str(name.clone()));
                            let tc_idx = self.code.add_constant(Value::str(tc.clone()));
                            self.code.emit(OpCode::SetVarType {
                                name_idx: name_idx2,
                                tc_idx,
                            });
                            // TypeCheck wraps the value on the stack for native types
                            let tc_idx2 = self.code.add_constant(Value::str(tc.clone()));
                            self.code.emit(OpCode::TypeCheck(tc_idx2, None));
                            // Now Dup the wrapped value and store
                            self.code.emit(OpCode::Dup);
                            self.emit_set_named_var(name);
                        } else {
                            // Enforce a scalar type constraint in expression
                            // position too (e.g. a bare `my Str $x := 3` whose
                            // value is the program result). Skip uninitialized
                            // typed decls (`my Str $x` -> Nil).
                            if let Some(tc) = type_constraint
                                && !matches!(expr, Expr::Literal(Value::Nil))
                            {
                                let tc_idx = self.code.add_constant(Value::str(tc.clone()));
                                let display_name = format!("${}", name);
                                let var_name_idx = self.code.add_constant(Value::str(display_name));
                                let is_bind =
                                    custom_traits.iter().any(|(t, _)| t == "__scalar_bind");
                                if is_bind {
                                    self.code
                                        .emit(OpCode::TypeCheckBind(tc_idx, Some(var_name_idx)));
                                } else {
                                    self.code
                                        .emit(OpCode::TypeCheck(tc_idx, Some(var_name_idx)));
                                }
                            }
                            self.code.emit(OpCode::Dup);
                            self.emit_set_named_var(name);
                        }
                    }
                }
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::SetVarDynamic {
                    name_idx,
                    dynamic: is_dynamic,
                });
                // `my $ = expr` (anonymous scalar without type constraint) returns
                // a Scalar container so the caller can store it in an immutable
                // List and later mutate the element via index assignment.
                // Typed anonymous scalars (`my num $`, `my int $`, etc.) must NOT
                // be wrapped -- their value must be used directly for numeric ops.
                if name == "__ANON_STATE__" && type_constraint.is_none() {
                    self.code.emit(OpCode::WrapScalar);
                }
                // Apply custom traits (e.g. `is default(...)`) that were
                // captured by `..` in the original pattern. Without this,
                // chained declarations like `my $b = my $d is default(42) = "foo"`
                // would lose the default trait on the inner variable.
                for (trait_name, trait_arg) in custom_traits {
                    if trait_name.starts_with("__") {
                        continue;
                    }
                    if let Some(arg) = trait_arg {
                        self.compile_expr(arg);
                    }
                    let trait_name_idx = self.code.add_constant(Value::str(trait_name.clone()));
                    self.code.emit(OpCode::ApplyVarTrait {
                        name_idx,
                        trait_name_idx,
                        has_arg: trait_arg.is_some(),
                    });
                }
            }
            Stmt::Expr(inner_expr) => {
                self.compile_expr(inner_expr);
            }
            Stmt::For {
                iterable,
                param,
                param_def,
                params,
                body,
                label,
                mode,
                ..
            } if *mode == crate::ast::ForMode::Lazy => {
                self.compile_lazy_for_expr(
                    iterable,
                    param,
                    param_def.as_ref(),
                    params,
                    body,
                    label,
                );
            }
            Stmt::For {
                iterable,
                param,
                param_def,
                params,
                body,
                label,
                ..
            } => {
                self.compile_do_for_expr(iterable, param, param_def.as_ref(), params, body, label);
            }
            Stmt::While { cond, body, label } => {
                self.compile_do_while_expr(cond, body, label);
            }
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } if !*repeat => {
                self.compile_do_loop_expr(init, cond, step, body, label);
            }
            Stmt::ClassDecl {
                name, name_expr, ..
            } => {
                // Register the class and return the type object
                self.compile_stmt(stmt);
                if let Some(expr) = name_expr {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::IndirectTypeLookup);
                } else {
                    let name_idx = self.code.add_constant(Value::str(name.resolve()));
                    self.code.emit(OpCode::GetBareWord(name_idx));
                }
            }
            Stmt::RoleDecl { name, .. } => {
                // Register the role and return the role type object.
                self.compile_stmt(stmt);
                let name_idx = self.code.add_constant(Value::str(name.resolve()));
                self.code.emit(OpCode::GetBareWord(name_idx));
            }
            Stmt::Package { name, .. } => {
                // Register the package and return the type object.
                self.compile_stmt(stmt);
                let name_idx = self.code.add_constant(Value::str(name.resolve()));
                self.code.emit(OpCode::GetBareWord(name_idx));
            }
            Stmt::EnumDecl { name, .. } if name.resolve().is_empty() => {
                // Anonymous enum: RegisterEnum pushes the Map result
                self.compile_stmt(stmt);
            }
            Stmt::Whenever { supply, .. } => {
                self.compile_stmt(stmt);
                if let Expr::Var(name) = supply {
                    self.compile_expr(&Expr::Var(name.clone()));
                } else {
                    self.code.emit(OpCode::LoadNil);
                }
            }
            Stmt::Block(inner) => {
                self.compile_do_block_expr_scoped(inner, &None);
            }
            Stmt::SyntheticBlock(inner)
                if inner
                    .last()
                    .is_some_and(|s| matches!(s, Stmt::MarkSigillessReadonly(_))) =>
            {
                self.compile_block_inline(inner);
            }
            Stmt::SyntheticBlock(inner) if inner.iter().any(|s| matches!(s, Stmt::MarkBind)) => {
                self.compile_block_inline(inner);
            }
            Stmt::SyntheticBlock(inner) => {
                self.compile_do_block_expr_scoped(inner, &None);
            }
            Stmt::Let {
                name,
                index: _,
                value: _,
                is_temp: _,
                undefine_first: _,
            } => {
                // Compile the temp/let statement, then push the variable as result.
                // This handles `(temp @a)` / `(temp $x = 42)` in expression position.
                self.compile_stmt(stmt);
                // Push the variable value as the expression result.
                // Reuse the same logic as Expr::ArrayVar/HashVar/Var compilation.
                if name.starts_with('@') {
                    let stripped = name.strip_prefix('@').unwrap_or(name);
                    self.compile_expr(&Expr::ArrayVar(stripped.to_string()));
                } else if name.starts_with('%') {
                    let stripped = name.strip_prefix('%').unwrap_or(name);
                    self.compile_expr(&Expr::HashVar(stripped.to_string()));
                } else {
                    self.compile_expr(&Expr::Var(name.clone()));
                }
            }
            _ => {
                self.compile_stmt(stmt);
                self.code.emit(OpCode::LoadNil);
            }
        }
    }

    /// Compile CallOn expression: @($x), %($x), &code(args), value(args).
    pub(super) fn compile_expr_call_on(&mut self, target: &Expr, args: &[Expr]) {
        // @($x) -- array contextualizer: coerce single arg to Array
        if let Expr::ArrayVar(name) = target
            && (name == "__ANON_ARRAY__" || name.starts_with("__ANON_ARRAY_"))
            && args.len() == 1
        {
            self.compile_expr(&args[0]);
            let method_idx = self.code.add_constant(Value::str_from("Array"));
            self.code.emit(OpCode::CallMethod {
                name_idx: method_idx,
                arity: 0,
                modifier_idx: None,
                quoted: false,
                arg_sources_idx: None,
            });
            return;
        }
        // %(args) -- hash contextualizer or hash construction
        if let Expr::HashVar(name) = target
            && name == "__ANON_HASH__"
        {
            if args.len() == 1 {
                self.compile_expr(&args[0]);
                let method_idx = self.code.add_constant(Value::str_from("Hash"));
                self.code.emit(OpCode::CallMethod {
                    name_idx: method_idx,
                    arity: 0,
                    modifier_idx: None,
                    quoted: false,
                    arg_sources_idx: None,
                });
            } else {
                let n = args.len() as u32;
                for arg in args {
                    self.compile_expr(arg);
                }
                self.code.emit(OpCode::MakeHashFromPairs(n));
            }
            return;
        }
        if let Expr::CodeVar(name) = target {
            let arg_sources_idx = self.add_arg_sources_constant(args);
            for arg in args {
                self.compile_expr(arg);
            }
            let name_idx = self.code.add_constant(Value::str(name.clone()));
            self.code.emit(OpCode::CallOnCodeVar {
                name_idx,
                arity: args.len() as u32,
                arg_sources_idx,
            });
        } else {
            self.compile_expr(target);
            let arg_sources_idx = self.add_arg_sources_constant(args);
            for arg in args {
                self.compile_expr(arg);
            }
            self.code.emit(OpCode::CallOnValue {
                arity: args.len() as u32,
                arg_sources_idx,
            });
        }
    }

    /// Compile Block expression -- inline if no placeholders, closure otherwise.
    pub(super) fn compile_expr_block(&mut self, stmts: &[Stmt]) {
        if Self::has_block_placeholders(stmts) {
            let placeholders = crate::ast::collect_placeholders_shallow(stmts);
            let compiled = self.compile_closure_body(&placeholders, &[], stmts);
            let esc = self.escaping_position;
            let cc_idx = self.code.add_closure_code(compiled, esc);
            let idx = self.code.add_stmt(Stmt::Block(stmts.to_vec()));
            self.code.emit(OpCode::MakeBlockClosure(idx, Some(cc_idx)));
        } else {
            self.compile_block_inline(stmts);
        }
    }
}
