use super::*;
use crate::symbol::Symbol;

impl Compiler {
    fn regex_match_returns_multiple(expr: &Expr) -> bool {
        let Expr::Binary { op, right, .. } = expr else {
            return false;
        };
        if !matches!(op, TokenKind::SmartMatch | TokenKind::BangTilde) {
            return false;
        }
        let regex = match right.as_ref() {
            Expr::MatchRegex(v) | Expr::Literal(v) => v,
            _ => return false,
        };
        matches!(
            regex,
            Value::RegexWithAdverbs { global: true, .. }
                | Value::RegexWithAdverbs { overlap: true, .. }
                | Value::RegexWithAdverbs {
                    exhaustive: true,
                    ..
                }
                | Value::RegexWithAdverbs {
                    repeat: Some(_),
                    ..
                }
        )
    }

    /// Returns true if the expression contains a state variable declaration
    /// (recursively). Used to decide whether `StateVarInitGuard` can safely
    /// skip evaluation of a state variable's RHS initializer.
    fn expr_has_state_decl(expr: &Expr) -> bool {
        match expr {
            Expr::DoStmt(stmt) => {
                if let Stmt::VarDecl { is_state: true, .. } = stmt.as_ref() {
                    return true;
                }
                false
            }
            _ => false,
        }
    }

    fn compile_assignment_rhs_for_target(&mut self, name: &str, expr: &Expr) {
        self.compile_expr(expr);
        if !name.starts_with('@')
            && !name.starts_with('%')
            && !name.starts_with('&')
            && Self::regex_match_returns_multiple(expr)
        {
            self.code.emit(OpCode::ScalarizeRegexMatchResult);
        }
    }

    fn compile_condition_expr(&mut self, cond: &Expr) {
        match cond {
            Expr::Literal(Value::Regex(_)) | Expr::Literal(Value::RegexWithAdverbs { .. }) => {
                self.compile_expr(&Expr::MatchRegex(match cond {
                    Expr::Literal(v) => v.clone(),
                    _ => unreachable!(),
                }));
            }
            other => self.compile_expr(other),
        }
    }

    fn extract_test_more_plan_arg(arg: &Option<Expr>) -> Option<&Expr> {
        let expr = arg.as_ref()?;
        if let Expr::Binary {
            left,
            op: TokenKind::FatArrow,
            right,
        } = expr
            && matches!(
                left.as_ref(),
                Expr::Literal(Value::Str(key)) if key.as_str() == "tests"
            )
        {
            return Some(right.as_ref());
        }
        None
    }

    fn compile_test_more_use(&mut self, arg: &Option<Expr>) {
        // `Test::More` is provided by native Test functions.
        let test_name_idx = self.code.add_constant(Value::str_from("Test"));
        self.code.emit(OpCode::UseModule(test_name_idx));
        if let Some(plan_arg) = Self::extract_test_more_plan_arg(arg) {
            self.compile_expr(plan_arg);
            let plan_name_idx = self.code.add_constant(Value::str_from("plan"));
            self.code.emit(OpCode::ExecCall {
                name_idx: plan_name_idx,
                arity: 1,
                arg_sources_idx: None,
            });
        }
    }

    pub(super) fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_condition_expr(expr);
                self.code.emit(OpCode::SinkPop);
            }
            Stmt::Block(stmts) => {
                // Check for placeholder conflicts in blocks
                let placeholders = crate::ast::collect_placeholders(stmts);
                if !placeholders.is_empty()
                    && let Some(err_val) =
                        self.check_placeholder_conflicts(&placeholders, stmts, None)
                {
                    let idx = self.code.add_constant(err_val);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                let saved_dynamic_scope = self.push_dynamic_scope_lexical();
                if Self::has_catch_or_control(stmts) {
                    self.compile_try(stmts, &None);
                    self.code.emit(OpCode::Pop);
                } else if Self::has_block_enter_leave_phasers(stmts) {
                    let idx = self.code.emit(OpCode::BlockScope {
                        pre_end: 0,
                        enter_end: 0,
                        body_end: 0,
                        keep_start: 0,
                        undo_start: 0,
                        post_start: 0,
                        end: 0,
                    });
                    // PRE phasers (forward order, before ENTER)
                    Self::compile_pre_phasers(self, stmts);
                    self.code.patch_block_pre_end(idx);
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
                    let body_stmts: Vec<&Stmt> = stmts
                        .iter()
                        .filter(|s| {
                            !matches!(
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
                        .collect();
                    for (i, s) in body_stmts.iter().enumerate() {
                        let is_last = i == body_stmts.len() - 1;
                        if is_last {
                            self.compile_last_stmt_as_topic(s);
                        } else {
                            self.compile_stmt(s);
                        }
                    }
                    self.code.patch_block_body_end(idx);
                    self.code.patch_block_keep_start(idx);
                    for s in stmts.iter().rev() {
                        if let Stmt::Phaser { kind, body } = s
                            && matches!(kind, PhaserKind::Leave | PhaserKind::Keep)
                        {
                            for inner in body {
                                self.compile_stmt(inner);
                            }
                        }
                    }
                    self.code.patch_block_undo_start(idx);
                    for s in stmts.iter().rev() {
                        if let Stmt::Phaser { kind, body } = s
                            && matches!(kind, PhaserKind::Leave | PhaserKind::Undo)
                        {
                            for inner in body {
                                self.compile_stmt(inner);
                            }
                        }
                    }
                    // POST phasers (reverse order, after LEAVE)
                    self.code.patch_block_post_start(idx);
                    Self::compile_post_phasers(self, stmts);
                    self.code.patch_loop_end(idx);
                } else if Self::has_let_deep(stmts) {
                    // Block contains `let`/`temp` — wrap in LetBlock for save/restore
                    let idx = self.code.emit(OpCode::LetBlock { body_end: 0 });
                    let needs_topic = Self::has_real_let_deep(stmts);
                    for (i, s) in stmts.iter().enumerate() {
                        let is_last = i == stmts.len() - 1;
                        if is_last && needs_topic {
                            // For `let` blocks, set topic from the last statement's
                            // value so we can check success/failure
                            self.compile_last_stmt_as_topic(s);
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
                        pre_end: 0,
                        enter_end: 0,
                        body_end: 0,
                        keep_start: 0,
                        undo_start: 0,
                        post_start: 0,
                        end: 0,
                    });
                    self.code.patch_block_pre_end(idx);
                    self.code.patch_block_enter_end(idx);
                    for s in stmts {
                        self.compile_stmt(s);
                    }
                    self.code.patch_block_body_end(idx);
                    self.code.patch_block_keep_start(idx);
                    self.code.patch_block_undo_start(idx);
                    self.code.patch_block_post_start(idx);
                    self.code.patch_loop_end(idx);
                }
                self.pop_dynamic_scope_lexical(saved_dynamic_scope);
            }
            Stmt::SyntheticBlock(stmts) => {
                // Detect `:=` bind context for `@` variables: the parser wraps
                // `my @a := expr` in a SyntheticBlock containing VarDecl followed
                // by `__mutsu_record_bound_array_len`.  Set bind_vardecl so the
                // VarDecl compilation emits MarkBindContext before SetLocal,
                // making the VM preserve the container type (e.g. List stays List).
                let has_bound_array_len = stmts.iter().any(|s| {
                    matches!(s,
                        Stmt::Expr(Expr::Call { name, .. })
                        if name.resolve() == "__mutsu_record_bound_array_len"
                    )
                });
                // Detect `:=` bind context for scalar variables via MarkBind.
                let has_mark_bind = stmts.iter().any(|s| matches!(s, Stmt::MarkBind));
                // Collect sigilless readonly names so we can clear the flag
                // before the VarDecl assignment (allows re-declaration in loops).
                let sigilless_readonly_names: Vec<String> = stmts
                    .iter()
                    .filter_map(|s| {
                        if let Stmt::MarkSigillessReadonly(name) = s {
                            Some(name.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                for s in stmts {
                    if has_bound_array_len
                        && let Stmt::VarDecl { name, .. } = s
                        && name.starts_with('@')
                    {
                        self.bind_vardecl = true;
                    }
                    if has_mark_bind && matches!(s, Stmt::VarDecl { .. }) {
                        self.bind_vardecl = true;
                    }
                    // Before compiling a VarDecl that will be followed by
                    // MarkSigillessReadonly, clear the old readonly flag so
                    // that re-declaration in a loop iteration succeeds.
                    if let Stmt::VarDecl { name, .. } = s
                        && sigilless_readonly_names.contains(name)
                    {
                        let key = format!("__mutsu_sigilless_readonly::{}", name);
                        let key_idx = self.code.add_constant(Value::str(key));
                        let false_idx = self.code.add_constant(Value::Bool(false));
                        self.code.emit(OpCode::LoadConst(false_idx));
                        self.code.emit(OpCode::SetGlobal(key_idx));
                    }
                    self.compile_stmt(s);
                }
            }
            Stmt::MarkReadonly(name) => {
                let idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::MarkVarReadonly(idx));
            }
            Stmt::MarkBind => {
                // Handled by SyntheticBlock detection; no-op when compiled standalone.
            }
            Stmt::MarkSigillessReadonly(name) => {
                // Set __mutsu_sigilless_readonly::NAME = true in env
                let key = format!("__mutsu_sigilless_readonly::{}", name);
                let key_idx = self.code.add_constant(Value::str(key));
                let true_idx = self.code.add_constant(Value::Bool(true));
                self.code.emit(OpCode::LoadConst(true_idx));
                self.code.emit(OpCode::SetGlobal(key_idx));
            }
            Stmt::Say(exprs) => {
                self.compile_exprs(exprs);
                self.code.emit(OpCode::Say(exprs.len() as u32));
            }
            Stmt::Put(exprs) => {
                self.compile_exprs(exprs);
                self.code.emit(OpCode::Put(exprs.len() as u32));
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
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                where_constraint: _,
            } => {
                // X::Dynamic::Package: dynamic variables cannot have package-like names
                if Self::is_dynamic_package_var(name) {
                    self.emit_dynamic_package_error(name);
                    return;
                }
                // X::Dynamic::Postdeclaration: dynamic variable used before declaration
                if name.starts_with('*') && self.accessed_dynamic_vars.contains(name) {
                    let symbol = Self::dynamic_var_symbol(name);
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("symbol".to_string(), Value::str(symbol));
                    let err =
                        Value::make_instance(Symbol::intern("X::Dynamic::Postdeclaration"), attrs);
                    let idx = self.code.add_constant(err);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                let is_dynamic = *is_dynamic || self.var_is_dynamic(name);
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::SetVarDynamic {
                    name_idx,
                    dynamic: is_dynamic,
                });
                let has_default_trait = custom_traits.iter().any(|(n, _)| n == "default");
                // Register type constraint early (for assignment checking) unless
                // `is default` trait is present — in that case defer until after
                // the trait is applied so the default value can be set first.
                if !has_default_trait && let Some(tc) = type_constraint {
                    let tc_idx = self.code.add_constant(Value::str(tc.clone()));
                    self.code.emit(OpCode::SetVarType { name_idx, tc_idx });
                }
                // Record type constraint for compile-time literal type checks
                if let Some(tc) = type_constraint {
                    self.local_types.insert(name.clone(), tc.clone());
                }
                // For state variables, emit a guard that skips the RHS evaluation
                // when the state is already initialized (avoiding side effects).
                // Skip the guard if the RHS contains nested state declarations
                // (e.g. `state $a = state $b = 42`) since the inner state var
                // needs its StateVarInit to run on every call.
                let state_guard_idx = if *is_state && !Self::expr_has_state_decl(expr) {
                    // Pre-compute the state key early so the guard can reference it.
                    // We use a placeholder IP that will be unique enough.
                    let placeholder_ip = self.code.ops.len();
                    let key = format!(
                        "__state_{}::{}@{}",
                        self.current_package, name, placeholder_ip
                    );
                    let key_idx = self.code.add_constant(Value::str(key.clone()));
                    let guard_idx = self.code.emit(OpCode::StateVarInitGuard(key_idx, 0));
                    Some((guard_idx, key, key_idx))
                } else {
                    None
                };
                if self.bind_vardecl && !name.starts_with('@') && !name.starts_with('%') {
                    // `:=` binding for scalar VarDecl: use compile_call_arg
                    // so WrapVarRef is emitted and the VM can set up aliases.
                    self.bind_vardecl = false;
                    self.compile_call_arg(expr);
                } else {
                    self.compile_assignment_rhs_for_target(name, expr);
                }
                // Skip TypeCheck for hash declarations: the type constraint
                // applies to element values, not to the collection itself.
                // TODO: enforce per-element type constraints at assignment time.
                let is_hash = name.starts_with('%');
                if let Some(tc) = type_constraint
                    && !is_hash
                    && !has_default_trait
                {
                    let tc_idx = self.code.add_constant(Value::str(tc.clone()));
                    // Build the display name for error messages (e.g. "a" -> "$a")
                    let display_name = if name.starts_with('@')
                        || name.starts_with('%')
                        || name.starts_with('&')
                    {
                        name.clone()
                    } else {
                        format!("${}", name)
                    };
                    let var_name_idx = self.code.add_constant(Value::str(display_name));
                    self.code
                        .emit(OpCode::TypeCheck(tc_idx, Some(var_name_idx)));
                }
                let slot = self.alloc_local(name);
                if *is_state {
                    if let Some((guard_idx, key, key_idx)) = state_guard_idx {
                        // Patch the guard jump target to the StateVarInit instruction
                        let state_init_ip = self.code.ops.len();
                        self.code.ops[guard_idx] =
                            OpCode::StateVarInitGuard(key_idx, state_init_ip as u32);
                        self.code.state_locals.push((slot as usize, key.clone()));
                        self.code.emit(OpCode::StateVarInit(slot, key_idx));
                    } else {
                        // No guard (e.g., chained state declarations) — use the
                        // original approach where RHS is always evaluated.
                        let ip = self.code.ops.len();
                        let key = format!("__state_{}::{}@{}", self.current_package, name, ip);
                        let key_idx = self.code.add_constant(Value::str(key.clone()));
                        self.code.state_locals.push((slot as usize, key.clone()));
                        self.code.emit(OpCode::StateVarInit(slot, key_idx));
                    }
                } else {
                    if *is_our {
                        self.code.emit(OpCode::Dup);
                    }
                    if self.bind_vardecl && name.starts_with('@') {
                        self.code.emit(OpCode::MarkBindContext);
                        self.bind_vardecl = false;
                    }
                    self.code.emit(OpCode::SetLocal(slot));
                    if *is_our {
                        let qualified = self.qualify_variable_name(name);
                        let idx = self.code.add_constant(Value::str(qualified));
                        self.code.emit(OpCode::SetGlobal(idx));
                    }
                }
                if *is_export {
                    let tags_idx = if export_tags.is_empty() {
                        None
                    } else {
                        let entries = export_tags
                            .iter()
                            .cloned()
                            .map(Value::str)
                            .collect::<Vec<Value>>();
                        Some(self.code.add_constant(Value::array(entries)))
                    };
                    self.code
                        .emit(OpCode::RegisterVarExport { name_idx, tags_idx });
                }
                for (trait_name, trait_arg) in custom_traits {
                    // Skip internal markers (not real traits)
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
                // Deferred type constraint registration after traits are applied
                if has_default_trait && let Some(tc) = type_constraint {
                    let tc_idx = self.code.add_constant(Value::str(tc.clone()));
                    self.code.emit(OpCode::SetVarType { name_idx, tc_idx });
                }
            }
            Stmt::Assign {
                name,
                expr,
                op: op @ (AssignOp::Assign | AssignOp::Bind),
            } if name != "*PID" => {
                // Handle $CALLER::varname = expr or $CALLER::varname := expr
                if let Some((bare_name, depth)) = Self::parse_caller_prefix(name) {
                    if matches!(op, AssignOp::Bind) {
                        // For := (bind), if the RHS is a variable, set up an alias
                        if let Expr::Var(rhs_name) = expr {
                            let target_idx = self.code.add_constant(Value::str(bare_name));
                            let source_idx = self.code.add_constant(Value::str(rhs_name.clone()));
                            self.code.emit(OpCode::BindCallerVar {
                                target_idx,
                                source_idx,
                                depth: depth as u32,
                            });
                        } else {
                            self.compile_expr(expr);
                            let name_idx = self.code.add_constant(Value::str(bare_name));
                            self.code.emit(OpCode::SetCallerVar {
                                name_idx,
                                depth: depth as u32,
                            });
                        }
                    } else {
                        self.compile_expr(expr);
                        let name_idx = self.code.add_constant(Value::str(bare_name));
                        self.code.emit(OpCode::SetCallerVar {
                            name_idx,
                            depth: depth as u32,
                        });
                    }
                    return;
                }
                if name.starts_with('&')
                    && !name.contains("::")
                    && !self.local_map.contains_key(name.as_str())
                {
                    self.code.emit(OpCode::AssignReadOnly);
                    return;
                }
                // Compile-time check: assigning a numeric literal to a typed
                // numeric variable with a mismatched type (e.g. `my Num $n; $n = 42`)
                // should produce X::Syntax::Number::LiteralType.
                if matches!(op, AssignOp::Assign)
                    && let Some(err) = self.check_literal_type_mismatch(name, expr)
                {
                    let idx = self.code.add_constant(err);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                // Emit readonly check for assignment to potentially readonly params.
                // Skip the check for `:=` (rebinding replaces the container).
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                if !matches!(op, AssignOp::Bind) {
                    self.code.emit(OpCode::CheckReadOnly(name_idx));
                }
                if matches!(op, AssignOp::Bind) {
                    if name.starts_with('@') {
                        self.code.emit(OpCode::MarkBindContext);
                    }
                    self.compile_call_arg(expr);
                } else {
                    self.compile_assignment_rhs_for_target(name, expr);
                }
                self.emit_set_named_var(name);
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
            } => {
                // Check for heredoc scope violations in then/else branches
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
                // Check if the then_branch uses @_ (bare if blocks receive
                // the condition value as @_ in Raku).
                let needs_at_underscore =
                    binding_var.is_none() && Self::body_uses_legacy_args(then_branch);
                if let Some(var_name) = binding_var {
                    // Desugar: if EXPR -> $var { BODY } else { ELSE }
                    // into: { my $var = EXPR; if $var { BODY } else { ELSE } }
                    let bare_name = var_name.trim_start_matches('$').to_string();
                    let desugared_cond = Expr::Var(bare_name.clone());
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
                    self.compile_condition_expr(&desugared_cond);
                } else {
                    self.compile_condition_expr(cond);
                    if needs_at_underscore {
                        // Duplicate condition value: one for JumpIfFalse truthiness
                        // test, one for setting @_ in the then_branch.
                        self.code.emit(OpCode::Dup);
                    }
                }
                let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
                if needs_at_underscore {
                    // Flatten the duplicated condition into @_ (like *@ slurpy).
                    self.code.emit(OpCode::FlattenSlurpy);
                    self.emit_set_named_var("@_");
                }
                if Self::body_mutates_topic(then_branch) {
                    self.compile_stmt(&Stmt::Block(then_branch.clone()));
                } else {
                    self.compile_body_with_implicit_try(then_branch);
                }
                if else_branch.is_empty() {
                    self.code.patch_jump(jump_else);
                    if needs_at_underscore {
                        // Pop the leftover duplicated condition value on the
                        // false branch (JumpIfFalse consumed only one copy).
                        self.code.emit(OpCode::Pop);
                    }
                } else {
                    let jump_end = self.code.emit(OpCode::Jump(0));
                    self.code.patch_jump(jump_else);
                    if needs_at_underscore {
                        self.code.emit(OpCode::Pop);
                    }
                    if else_branch.len() == 1 && matches!(else_branch[0], Stmt::If { .. }) {
                        self.compile_stmt(&else_branch[0]);
                    } else if Self::body_mutates_topic(else_branch) {
                        self.compile_stmt(&Stmt::Block(else_branch.clone()));
                    } else {
                        self.compile_body_with_implicit_try(else_branch);
                    }
                    self.code.patch_jump(jump_end);
                }
            }
            Stmt::While { cond, body, label } => {
                let (pre_stmts, loop_body, post_stmts) =
                    self.expand_loop_phasers(body, label.as_deref());
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                let loop_idx = self.code.emit(OpCode::WhileLoop {
                    cond_end: 0,
                    body_end: 0,
                    label: label.clone(),
                    collect: false,
                    isolate_topic: Self::body_mutates_topic(&loop_body),
                });
                self.compile_condition_expr(cond);
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
                mode,
                rw_block,
            } => {
                let (pre_stmts, mut loop_body, post_stmts) =
                    self.expand_loop_phasers(body, label.as_deref());
                let restore_topic = param.is_none() && params.is_empty() && body.len() == 1;
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                // When there's a single named param (-> $k), store its name as a constant
                // so the VM can bind $k directly without overriding $_
                let param_idx = param
                    .as_ref()
                    .map(|p| self.code.add_constant(Value::str(p.clone())));
                let bind_stmts =
                    Self::build_for_bind_stmts(param, param_def.as_ref(), param_idx, params);
                if !bind_stmts.is_empty() {
                    let mut merged = bind_stmts;
                    merged.extend(loop_body);
                    loop_body = merged;
                }
                // Determine if this for-loop has rw params (via `<->` or `is rw` trait)
                let has_rw = *rw_block
                    || (**param_def)
                        .as_ref()
                        .is_some_and(|def| def.traits.iter().any(|t| t == "rw"));
                // `is copy` also makes the param writable (but without writeback)
                let has_copy = (**param_def)
                    .as_ref()
                    .is_some_and(|def| def.traits.iter().any(|t| t == "copy"));
                let arity = if !params.is_empty() {
                    params.len() as u32
                } else {
                    1
                };
                let normalized_iterable = Self::normalize_for_iterable(iterable);
                self.compile_expr(&normalized_iterable);
                if let Some(source_name) = Self::for_iterable_source_name(iterable) {
                    let source_idx = self.code.add_constant(Value::str(source_name));
                    self.code.emit(OpCode::TagContainerRef(source_idx));
                }
                // If the for-loop parameter name already has a local slot
                // (e.g. from a prior `my $i` in an enclosing scope), we must
                // tell the VM so it can keep the local in sync with the env
                // on each iteration and on redo.
                let param_local = param
                    .as_ref()
                    .and_then(|p| self.local_map.get(p.as_str()).copied());
                let rw_param_names = if has_rw && !params.is_empty() {
                    params.clone()
                } else {
                    Vec::new()
                };
                let kv_mode = has_rw && Self::for_iterable_is_kv(iterable);
                let source_var_names = Self::for_iterable_var_names(iterable);
                let loop_idx = self.code.emit(OpCode::ForLoop {
                    param_idx,
                    param_local,
                    body_end: 0,
                    label: label.clone(),
                    arity,
                    collect: false,
                    restore_topic,
                    threaded: *mode != crate::ast::ForMode::Normal,
                    // is_rw: param is writable (don't mark readonly)
                    is_rw: has_rw || has_copy,
                    // do_writeback: actually write back modifications to source container
                    do_writeback: has_rw && !has_copy,
                    rw_param_names,
                    kv_mode,
                    source_var_names,
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
                let (pre_stmts, loop_body, post_stmts) =
                    self.expand_loop_phasers(body, label.as_deref());
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
                    collect: false,
                });
                // Compile condition (or push True if none)
                if let Some(cond_expr) = cond {
                    self.compile_condition_expr(cond_expr);
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
                // Check for invocant colon syntax: foo($obj:) → $obj.foo()
                if let Some(CallArg::Invocant(_)) = args.first() {
                    let invocant_expr = match &args[0] {
                        CallArg::Invocant(e) => e.clone(),
                        _ => unreachable!(),
                    };
                    let method_args: Vec<Expr> = args[1..]
                        .iter()
                        .filter_map(|arg| match arg {
                            CallArg::Positional(e) => Some(e.clone()),
                            _ => None,
                        })
                        .collect();
                    let method_call = Expr::MethodCall {
                        target: Box::new(invocant_expr),
                        name: *name,
                        args: method_args,
                        modifier: None,
                        quoted: false,
                    };
                    self.compile_expr(&method_call);
                    self.code.emit(OpCode::Pop);
                    return;
                }

                let name_str = name.resolve();
                let rewritten_args = Self::rewrite_stmt_call_args(&name_str, args);
                let positional_only = rewritten_args
                    .iter()
                    .all(|arg| matches!(arg, CallArg::Positional(_)));

                // Normalize mutating/structural call statements through Expr::Call
                // so they reuse call rewrites and method-based mutation paths.
                if positional_only && Self::is_normalized_stmt_call_name(&name_str) {
                    let expr_args: Vec<Expr> = rewritten_args
                        .iter()
                        .filter_map(|arg| match arg {
                            CallArg::Positional(expr) => Some(expr.clone()),
                            _ => None,
                        })
                        .collect();
                    let call_expr = Expr::Call {
                        name: *name,
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
                    let positional_exprs: Vec<Expr> = rewritten_args
                        .iter()
                        .filter_map(|arg| match arg {
                            CallArg::Positional(expr) => Some(expr.clone()),
                            _ => None,
                        })
                        .collect();
                    let arg_sources_idx = self.add_arg_sources_constant(&positional_exprs);
                    for arg in &rewritten_args {
                        if let CallArg::Positional(expr) = arg {
                            self.compile_call_arg(expr);
                        }
                    }
                    let name_idx = self.code.add_constant(Value::str(name.resolve()));
                    self.code.emit(OpCode::ExecCall {
                        name_idx,
                        arity,
                        arg_sources_idx,
                    });
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
                                self.compile_call_arg(expr);
                                regular_count += 1;
                            }
                            CallArg::Named {
                                name: n,
                                value: Some(expr),
                            } => {
                                self.compile_expr(&Expr::Literal(Value::str(n.clone())));
                                self.compile_expr(expr);
                                self.code.emit(OpCode::MakePair);
                                regular_count += 1;
                            }
                            CallArg::Named {
                                name: n,
                                value: None,
                            } => {
                                self.compile_expr(&Expr::Literal(Value::str(n.clone())));
                                self.compile_expr(&Expr::Literal(Value::Bool(true)));
                                self.code.emit(OpCode::MakePair);
                                regular_count += 1;
                            }
                            CallArg::Slip(_) | CallArg::Invocant(_) => {} // handled below
                        }
                    }
                    // Compile the slip expression (last one wins if multiple)
                    for arg in &rewritten_args {
                        if let CallArg::Slip(expr) = arg {
                            self.compile_expr(expr);
                        }
                    }
                    let name_idx = self.code.add_constant(Value::str(name.resolve()));
                    self.code.emit(OpCode::ExecCallSlip {
                        name_idx,
                        regular_arity: regular_count,
                        arg_sources_idx: None,
                    });
                    return;
                }

                // Statement-level call with named args: compile values and encode
                // named args as Pair(name => value), then dispatch without stmt_pool.
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
                self.code.emit(OpCode::ExecCallPairs {
                    name_idx,
                    arity: rewritten_args.len() as u32,
                });
            }
            // Loop control
            Stmt::Goto(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Goto);
            }
            Stmt::Label { name, stmt } => {
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::Label(name_idx));
                self.compile_stmt(stmt);
            }
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
                if self.is_routine {
                    self.code.emit(OpCode::Return);
                } else {
                    self.code.emit(OpCode::ReturnFromNonRoutine);
                }
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
            Stmt::ReactDone => {
                self.code.emit(OpCode::ReactDone);
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
                if let Some(source_name) = match topic {
                    Expr::Var(name) => Some(name.clone()),
                    Expr::ArrayVar(name) => Some(format!("@{}", name)),
                    Expr::HashVar(name) => Some(format!("%{}", name)),
                    _ => None,
                } {
                    let name_idx = self.code.add_constant(Value::str(source_name));
                    self.code.emit(OpCode::TagContainerRef(name_idx));
                }
                let given_idx = self.code.emit(OpCode::Given { body_end: 0 });
                for (i, s) in body.iter().enumerate() {
                    let is_last = i == body.len() - 1;
                    if is_last {
                        if let Stmt::Expr(expr) = s {
                            self.compile_expr(expr);
                            if let Expr::Var(name) = expr {
                                let name_idx = self.code.add_constant(Value::str(name.clone()));
                                self.code.emit(OpCode::TagContainerRef(name_idx));
                            }
                        } else {
                            self.compile_stmt(s);
                        }
                    } else {
                        self.compile_stmt(s);
                    }
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
                            if let Expr::Var(name) = expr {
                                let name_idx = self.code.add_constant(Value::str(name.clone()));
                                self.code.emit(OpCode::TagContainerRef(name_idx));
                            }
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
                            if let Expr::Var(name) = expr {
                                let name_idx = self.code.add_constant(Value::str(name.clone()));
                                self.code.emit(OpCode::TagContainerRef(name_idx));
                            }
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
                let (pre_stmts, loop_body, post_stmts) =
                    self.expand_loop_phasers(body, label.as_deref());
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
                    self.compile_condition_expr(cond_expr);
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
            // HasDecl outside class context is an error
            Stmt::HasDecl {
                name,
                sigil,
                is_public,
                ..
            } => {
                let twigil = if *is_public { "." } else { "!" };
                let bare = name.resolve();
                let bare = bare.trim_start_matches(['.', '!']);
                let sigil_ch = if *sigil == '!' || *sigil == '.' {
                    '$'
                } else {
                    *sigil
                };
                let full_name = format!("{}{}{}", sigil_ch, twigil, bare);
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("name".to_string(), Value::str(full_name));
                let err = Value::make_instance(Symbol::intern("X::Attribute::NoPackage"), attrs);
                let idx = self.code.add_constant(err);
                self.code.emit(OpCode::LoadConst(idx));
                self.code.emit(OpCode::Die);
            }
            // DoesDecl/TrustsDecl outside class context are no-ops
            Stmt::DoesDecl { .. } | Stmt::TrustsDecl { .. } => {}

            // --- Take (gather/take) ---
            Stmt::Take(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Take);
            }

            // --- React: event loop scope ---
            Stmt::React { body } => {
                let idx = self.code.emit(OpCode::ReactScope { body_end: 0 });
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_body_end(idx);
            }

            // --- Package scope ---
            Stmt::Package {
                name,
                body,
                is_unit,
            } => {
                let qualified_name = self.qualify_package_name(&name.resolve());
                // Detect stub body: `module Foo { ... }` — body is a stub operator
                let is_stub_body = body.len() == 1
                    && matches!(&body[0], Stmt::Expr(Expr::Call { name: fn_name, .. })
                        if fn_name.resolve() == "__mutsu_stub_die"
                            || fn_name.resolve() == "__mutsu_stub_warn");
                if *is_unit {
                    // unit module/package — set package for the rest of the scope
                    self.current_package = qualified_name.clone();
                    // Register the package name so it's accessible as a value
                    let name_idx = self.code.add_constant(Value::str(qualified_name.clone()));
                    self.code.emit(OpCode::RegisterPackage { name_idx });
                } else if is_stub_body {
                    // Stub package — register name but don't execute the body
                    let name_idx = self.code.add_constant(Value::str(qualified_name.clone()));
                    self.code.emit(OpCode::RegisterPackage { name_idx });
                    self.code.emit(OpCode::RegisterPackageStub { name_idx });
                } else {
                    let name_idx = self.code.add_constant(Value::str(qualified_name.clone()));
                    // Non-unit package declarations also produce a type object value.
                    self.code.emit(OpCode::RegisterPackage { name_idx });
                    // Clear any previous stub status for this package
                    self.code.emit(OpCode::ClearPackageStub { name_idx });
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

            // --- Phaser (BEGIN/CHECK/INIT) ---
            // These are extracted before compilation by extract_check_init_phasers()
            // and run in the correct order. If one remains (e.g. inside a sub body),
            // compile it inline as a fallback.
            Stmt::Phaser {
                kind: PhaserKind::Begin | PhaserKind::Check | PhaserKind::Init,
                body,
            } => {
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
                name_expr,
                params,
                param_defs,
                return_type,
                signature_alternates,
                body,
                multi,
                is_rw,
                is_raw,
                ..
            } => {
                // Validate placeholder conflicts for subs with implicit params
                if param_defs.is_empty()
                    && !params.is_empty()
                    && let Some(err_val) =
                        self.check_placeholder_conflicts(params, body, Some("sub"))
                {
                    let idx = self.code.add_constant(err_val);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterSub(idx));
                if name_expr.is_some() {
                    // Runtime-resolved sub names cannot be keyed reliably in compiled_fns.
                    return;
                }
                // Also compile the body to bytecode for VM-native dispatch
                let state_group = if *multi && !signature_alternates.is_empty() {
                    Some(format!(
                        "{}::{}",
                        name,
                        crate::ast::function_body_fingerprint(params, param_defs, body)
                    ))
                } else {
                    None
                };
                let name_str = name.resolve();
                self.compile_sub_body(
                    &name_str,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    body,
                    *multi,
                    state_group.as_deref(),
                    *is_rw,
                    *is_raw,
                );
                for (alt_params, alt_param_defs) in signature_alternates {
                    self.compile_sub_body(
                        &name_str,
                        alt_params,
                        alt_param_defs,
                        return_type.as_ref(),
                        body,
                        *multi,
                        state_group.as_deref(),
                        *is_rw,
                        *is_raw,
                    );
                }
            }
            Stmt::MethodDecl {
                name,
                name_expr,
                params,
                param_defs,
                body,
                multi,
                is_rw,
                return_type,
                ..
            } => {
                // Top-level/package method declarations should still produce callable
                // code objects (&name), so lower them through sub registration.
                let lowered = Stmt::SubDecl {
                    name: *name,
                    name_expr: name_expr.clone(),
                    params: params.clone(),
                    param_defs: param_defs.clone(),
                    return_type: return_type.clone(),
                    associativity: None,
                    signature_alternates: Vec::new(),
                    body: body.clone(),
                    multi: *multi,
                    is_rw: *is_rw,
                    is_raw: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    is_test_assertion: false,
                    supersede: false,
                    custom_traits: vec!["__mutsu_method_decl".to_string()],
                };
                let idx = self.code.add_stmt(lowered);
                self.code.emit(OpCode::RegisterSub(idx));
                if name_expr.is_none() {
                    self.compile_sub_body(
                        &name.resolve(),
                        params,
                        param_defs,
                        return_type.as_ref(),
                        body,
                        *multi,
                        None,
                        *is_rw,
                        false,
                    );
                }
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
            Stmt::Use { module, arg } if module == "dynamic-scope" => {
                self.apply_dynamic_scope_pragma(arg.as_ref());
            }
            Stmt::Use { module, arg } if module == "newline" => {
                if let Some(expr) = arg {
                    self.compile_expr(expr);
                    let name_idx = self
                        .code
                        .add_constant(Value::str_from("__mutsu_set_newline"));
                    self.code.emit(OpCode::ExecCall {
                        name_idx,
                        arity: 1,
                        arg_sources_idx: None,
                    });
                }
            }
            Stmt::Use { module, arg } if module == "variables" => {
                // `use variables :D/:U/:_` pragma — emit a SetVariablesPragma opcode
                if let Some(arg_expr) = arg {
                    self.compile_expr(arg_expr);
                } else {
                    let nil_idx = self.code.add_constant(Value::Nil);
                    self.code.emit(OpCode::LoadConst(nil_idx));
                }
                let name_idx = self.code.add_constant(Value::str("variables".to_string()));
                self.code.emit(OpCode::SetPragma(name_idx));
            }
            Stmt::Use { module, arg } if module == "attributes" => {
                // `use attributes :D/:U/:_` pragma — emit a SetPragma opcode
                if let Some(arg_expr) = arg {
                    self.compile_expr(arg_expr);
                } else {
                    let nil_idx = self.code.add_constant(Value::Nil);
                    self.code.emit(OpCode::LoadConst(nil_idx));
                }
                let name_idx = self.code.add_constant(Value::str("attributes".to_string()));
                self.code.emit(OpCode::SetPragma(name_idx));
            }
            Stmt::Use { module, .. }
                if module == "v6"
                    || module == "customtrait"
                    || module == "isms"
                    || module == "nqp"
                    || module == "soft" => {}
            Stmt::Use { module, .. } if module == "MONKEY-TYPING" || module == "MONKEY" => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                self.code.emit(OpCode::UseModule(name_idx));
            }
            Stmt::Use { module, arg } if module == "Test::More" => {
                self.compile_test_more_use(arg);
            }
            Stmt::Use { module, .. } if module == "Test" || module.starts_with("Test::") => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                self.code.emit(OpCode::UseModule(name_idx));
            }
            Stmt::Use { module, .. } => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                self.code.emit(OpCode::UseModule(name_idx));
            }
            Stmt::Import { module, tags } => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                let tags_idx = if tags.is_empty() {
                    None
                } else {
                    let entries = tags.iter().cloned().map(Value::str).collect::<Vec<Value>>();
                    Some(self.code.add_constant(Value::array(entries)))
                };
                self.code.emit(OpCode::ImportModule { name_idx, tags_idx });
            }
            Stmt::No { module } => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                self.code.emit(OpCode::NoModule(name_idx));
            }
            Stmt::Need { module } => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
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
            Stmt::AugmentClass { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::AugmentClass(idx));
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
                    .map(|p| self.code.add_constant(Value::str(p.clone())));
                let target_var_idx = if let Expr::Var(name) = supply {
                    Some(self.code.add_constant(Value::str(name.clone())))
                } else {
                    None
                };
                self.code.emit(OpCode::WheneverScope {
                    body_idx,
                    param_idx,
                    target_var_idx,
                });
            }
            Stmt::Let {
                name,
                index,
                value,
                is_temp,
            } => {
                // Emit LetSave: saves current value of the variable
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                let has_index = index.is_some();
                if let Some(idx_expr) = index {
                    self.compile_expr(idx_expr);
                }
                self.code.emit(OpCode::LetSave {
                    name_idx,
                    index_mode: has_index,
                    is_temp: *is_temp,
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
                let name_idx = self.code.add_constant(Value::str(var_name.clone()));
                self.code.emit(OpCode::LetSave {
                    name_idx,
                    index_mode: false,
                    is_temp: true,
                });
                let assign_expr = Expr::Call {
                    name: Symbol::intern("__mutsu_assign_method_lvalue"),
                    args: vec![
                        Expr::Var(var_name.clone()),
                        Expr::Literal(Value::str(method_name.clone())),
                        Expr::ArrayLiteral(method_args.clone()),
                        value.clone(),
                        Expr::Literal(Value::str(var_name.clone())),
                    ],
                };
                self.compile_expr(&assign_expr);
                self.code.emit(OpCode::Pop);
            }
        }
    }

    /// Compile the last statement of a `let` block so its result sets the topic.
    /// This allows `exec_let_block_op` to check the topic for success/failure.
    pub(super) fn compile_last_stmt_as_topic(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::SetTopic);
            }
            _ => {
                // For non-expression statements (calls, assignments, etc.),
                // compile normally. Any completed statement counts as success.
                self.compile_stmt(stmt);
                self.compile_expr(&Expr::Literal(Value::Bool(true)));
                self.code.emit(OpCode::SetTopic);
            }
        }
    }

    /// Compile PRE phasers in forward source order.
    /// Each PRE body is compiled, followed by a CheckPhaser { is_pre: true }.
    pub(super) fn compile_pre_phasers(compiler: &mut Compiler, stmts: &[Stmt]) {
        for s in stmts {
            if let Stmt::Phaser {
                kind: PhaserKind::Pre,
                body,
            } = s
            {
                // Compile the PRE body as a block expression that produces a value
                for (i, inner) in body.iter().enumerate() {
                    if i == body.len() - 1 {
                        // Last statement: compile as expression to leave value on stack
                        match inner {
                            Stmt::Expr(expr) => compiler.compile_expr(expr),
                            _ => {
                                compiler.compile_stmt(inner);
                                compiler.compile_expr(&Expr::Literal(Value::Bool(true)));
                            }
                        }
                    } else {
                        compiler.compile_stmt(inner);
                    }
                }
                compiler.code.emit(OpCode::CheckPhaser { is_pre: true });
            }
        }
    }

    /// Compile POST phasers in reverse source order.
    /// Each POST body is compiled, followed by a CheckPhaser { is_pre: false }.
    pub(super) fn compile_post_phasers(compiler: &mut Compiler, stmts: &[Stmt]) {
        for s in stmts.iter().rev() {
            if let Stmt::Phaser {
                kind: PhaserKind::Post,
                body,
            } = s
            {
                for (i, inner) in body.iter().enumerate() {
                    if i == body.len() - 1 {
                        match inner {
                            Stmt::Expr(expr) => compiler.compile_expr(expr),
                            _ => {
                                compiler.compile_stmt(inner);
                                compiler.compile_expr(&Expr::Literal(Value::Bool(true)));
                            }
                        }
                    } else {
                        compiler.compile_stmt(inner);
                    }
                }
                compiler.code.emit(OpCode::CheckPhaser { is_pre: false });
            }
        }
    }
}
