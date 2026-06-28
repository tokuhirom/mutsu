use super::*;
use crate::symbol::Symbol;

impl Compiler {
    /// Pre-qualify a class/role declaration's name with the compiler's
    /// `current_package` when compiling inside a `unit module`/`unit class`/
    /// `unit role` body. Bare names (no `::`) are rewritten to
    /// `Pkg::Name`. Names that already contain `::` or are top-level
    /// (current_package == "GLOBAL") are returned unchanged.
    fn qualify_decl_name(&self, stmt: &Stmt) -> Stmt {
        if !self.in_unit_package
            || self.current_package == "GLOBAL"
            || self.current_package.contains("::&")
        {
            return stmt.clone();
        }
        let bare = match stmt {
            Stmt::ClassDecl { name, .. } | Stmt::RoleDecl { name, .. } => name.resolve(),
            _ => return stmt.clone(),
        };
        if bare.contains("::") {
            return stmt.clone();
        }
        let qualified = format!("{}::{}", self.current_package, bare);
        let qualified_sym = Symbol::intern(&qualified);
        let pkg = self.current_package.clone();
        let qualify_parent = |p: &String| -> String {
            if p.contains("::") || p.is_empty() {
                return p.clone();
            }
            // Well-known builtin types should not be package-qualified.
            // "Grammar" inside a module should stay "Grammar", not become
            // "MyModule::Grammar".
            if matches!(
                p.as_str(),
                "Any"
                    | "Cool"
                    | "Mu"
                    | "Grammar"
                    | "Match"
                    | "Int"
                    | "Str"
                    | "Num"
                    | "Rat"
                    | "Bool"
                    | "IO"
                    | "Exception"
                    | "Stash"
                    | "Array"
                    | "Hash"
                    | "List"
                    | "Map"
                    | "Set"
                    | "Bag"
                    | "Mix"
                    | "Range"
                    | "Pair"
                    | "Regex"
                    | "FatRat"
                    | "Complex"
                    | "Callable"
                    | "Numeric"
                    | "Real"
                    | "Stringy"
                    | "Positional"
                    | "Associative"
                    | "Proc"
                    | "Supply"
                    | "Supplier"
                    | "Date"
                    | "DateTime"
                    | "Capture"
                    | "Parameter"
                    | "Signature"
            ) {
                return p.clone();
            }
            format!("{}::{}", pkg, p)
        };
        match stmt {
            Stmt::ClassDecl {
                name_expr,
                parents,
                class_is_rw,
                is_hidden,
                is_lexical,
                hidden_parents,
                does_parents,
                repr,
                body,
                language_version,
                custom_traits,
                is_unit,
                ..
            } => {
                let new_parents: Vec<String> = parents.iter().map(&qualify_parent).collect();
                let new_does: Vec<String> = does_parents.iter().map(&qualify_parent).collect();
                let new_hidden: Vec<String> = hidden_parents.iter().map(&qualify_parent).collect();
                Stmt::ClassDecl {
                    name: qualified_sym,
                    name_expr: name_expr.clone(),
                    parents: new_parents,
                    class_is_rw: *class_is_rw,
                    is_hidden: *is_hidden,
                    is_lexical: *is_lexical,
                    hidden_parents: new_hidden,
                    does_parents: new_does,
                    repr: repr.clone(),
                    body: body.clone(),
                    language_version: language_version.clone(),
                    custom_traits: custom_traits.clone(),
                    is_unit: *is_unit,
                }
            }
            Stmt::RoleDecl {
                type_params,
                type_param_defs,
                is_export,
                export_tags,
                body,
                is_rw,
                language_version,
                custom_traits,
                ..
            } => Stmt::RoleDecl {
                name: qualified_sym,
                type_params: type_params.clone(),
                type_param_defs: type_param_defs.clone(),
                is_export: *is_export,
                export_tags: export_tags.clone(),
                body: body.clone(),
                is_rw: *is_rw,
                language_version: language_version.clone(),
                custom_traits: custom_traits.clone(),
            },
            _ => stmt.clone(),
        }
    }

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
            Value::RegexWithAdverbs(a)
                if a.global || a.overlap || a.exhaustive || a.repeat.is_some()
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

    /// Check if a default value expression statically mismatches a type constraint.
    /// Returns `Some(value_repr)` if a mismatch is detected, `None` otherwise.
    fn check_default_type_mismatch(type_constraint: &str, expr: &Expr) -> Option<String> {
        let effective_constraint = type_constraint
            .strip_suffix(":D")
            .or_else(|| type_constraint.strip_suffix(":_"))
            .unwrap_or(type_constraint);
        let value_type = match expr {
            Expr::Literal(Value::Str(s)) => {
                if effective_constraint != "Str"
                    && effective_constraint != "Cool"
                    && effective_constraint != "Any"
                {
                    return Some(s.to_string());
                }
                return None;
            }
            Expr::Literal(Value::Int(_)) => "Int",
            Expr::Literal(Value::Num(_)) => "Num",
            Expr::Literal(Value::Bool(_)) => "Bool",
            Expr::Literal(Value::Nil) => {
                // Nil is invalid for typed variables (Int, Str, etc.)
                // but valid for untyped (Any, Mu) or explicitly Nil-accepting types
                if effective_constraint != "Any"
                    && effective_constraint != "Mu"
                    && !effective_constraint.contains("Nil")
                {
                    return Some("Nil".to_string());
                }
                return None;
            }
            _ => return None, // non-literal, can't check statically
        };
        // Check type hierarchy: Int matches Numeric, Cool, Any, etc.
        let mro: &[&str] = match value_type {
            "Bool" => &["Bool", "Int", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Int" => &["Int", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Num" => &["Num", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Rat" => &["Rat", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Str" => &["Str", "Stringy", "Cool", "Any", "Mu"],
            _ => &[],
        };
        if mro.contains(&effective_constraint) {
            None
        } else {
            Some(match expr {
                Expr::Literal(v) => v.to_string_value(),
                _ => "?".to_string(),
            })
        }
    }

    /// Slice 2a/2b (`docs/scalar-array-sharing.md`): `$scalar = @arr` / `$scalar
    /// = %hash` (and the chained `$scalar = $other`) shares the source container
    /// by reference (raku semantics) rather than snapshotting it. The source
    /// variable name is known at compile time, so flag the upcoming
    /// `SetLocal`/`AssignExpr` with `MarkArrayShareSource` to promote both to a
    /// shared `ContainerRef` cell. For a scalar source the runtime only shares
    /// when it actually holds a container (a plain `$x = $y` stays a copy).
    /// `@`/`%`/`&` targets (copy/bind elsewhere) are skipped.
    fn try_emit_array_share(&mut self, name: &str, expr: &Expr) -> bool {
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            return false;
        }
        let source = match expr {
            Expr::ArrayVar(n) => format!("@{}", n),
            Expr::HashVar(n) => format!("%{}", n),
            // Chained share: `$r = $q` where `$q` may hold a container. The
            // runtime no-ops when `$q` is a plain scalar, so this stays a copy.
            Expr::Var(n) => n.clone(),
            _ => return false,
        };
        self.with_escape(true, |c| c.compile_expr(expr));
        let name_idx = self.code.add_constant(Value::str(source));
        self.code.emit(OpCode::MarkArrayShareSource(name_idx));
        true
    }

    fn compile_assignment_rhs_for_target(&mut self, name: &str, expr: &Expr) {
        if self.try_emit_array_share(name, expr) {
            return;
        }
        // The RHS value is stored into the target, so a closure literal here
        // escapes the creating frame (escape analysis): force a shared cell for
        // the captured-and-mutated locals it closes over.
        self.with_escape(true, |c| c.compile_expr(expr));
        if !name.starts_with('@')
            && !name.starts_with('%')
            && !name.starts_with('&')
            && Self::regex_match_returns_multiple(expr)
        {
            self.code.emit(OpCode::ScalarizeRegexMatchResult);
        }
        // When assigning a `$` scalar variable to an `@` target, itemize
        // the value so it is treated as a single item (not flattened).
        // Sigilless variables (BareWord) are not itemized. A scalar bound
        // (`:=`) to a Positional is NOT a container, so use the var-aware
        // opcode which skips itemization for bound scalars.
        if name.starts_with('@')
            && let Expr::Var(var_name) = expr
        {
            let name_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::ItemizeVar(name_idx));
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
        self.code.emit(OpCode::UseModule {
            name_idx: test_name_idx,
            tags_idx: None,
        });
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

    /// Does `expr` ultimately root at a variable, so an `Index` over it is an
    /// assignable lvalue (e.g. `%h<k>`, `@a[i]`, `%h<a><b>`)? Function-call and
    /// other non-lvalue roots are excluded so we never synthesize a writeback
    /// assignment into a temporary (which would error where Raku is silent).
    fn for_element_container_is_lvalue(expr: &Expr) -> bool {
        match expr {
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::BareWord(_) => true,
            Expr::Index { target, .. } => Self::for_element_container_is_lvalue(target),
            _ => false,
        }
    }

    /// Rewrite `for <ELEM>.values { ... }`, where `<ELEM>` is a var-rooted
    /// `Index` lvalue (`%h<k>` / `@a[i]` / `%h<a><b>`), into:
    ///
    ///   my @tmp = <ELEM>;          # copy the element array into a temp
    ///   for @tmp.values { ... };   # reuse the array-source per-element writeback
    ///   <ELEM> = @tmp;             # write the temp back into the element
    ///
    /// Returns `None` for anything but this exact shape (plain `@a`/`%h` sources
    /// are already handled by `for_iterable_source_name`).
    fn desugar_for_element_source(&mut self, stmt: &Stmt) -> Option<Vec<Stmt>> {
        let Stmt::For { iterable, .. } = stmt else {
            return None;
        };
        let Expr::MethodCall {
            target, name, args, ..
        } = iterable
        else {
            return None;
        };
        if !args.is_empty() || name.resolve() != "values" {
            return None;
        }
        let Expr::Index {
            target: container,
            index,
            is_positional,
        } = target.as_ref()
        else {
            return None;
        };
        if !Self::for_element_container_is_lvalue(container) {
            return None;
        }

        let tmp = format!("__for_elem_src_{}", self.code.constants.len());
        let element = target.as_ref().clone();

        let decl = Stmt::VarDecl {
            name: format!("@{}", tmp),
            expr: element,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: vec![("__has_initializer".to_string(), None)],
            where_constraint: None,
        };

        // The rewritten for-loop iterates `@tmp.values`; cloning the original
        // For and swapping only its iterable preserves params/body/label/mode.
        let mut for_stmt = stmt.clone();
        if let Stmt::For {
            iterable: new_iterable,
            ..
        } = &mut for_stmt
        {
            *new_iterable = Expr::MethodCall {
                target: Box::new(Expr::ArrayVar(tmp.clone())),
                name: crate::symbol::Symbol::intern("values"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            };
        }

        let writeback = Stmt::Expr(Expr::IndexAssign {
            target: container.clone(),
            index: index.clone(),
            value: Box::new(Expr::ArrayVar(tmp)),
            is_positional: *is_positional,
        });

        Some(vec![decl, for_stmt, writeback])
    }

    /// Whether a bare statement expression yields a syntactically fresh rvalue
    /// (a method call / `Foo.new`) whose value may invoke a user-defined `sink`
    /// method in sink context. Bare variables (`$x;`) and function-call returns
    /// (`frob();`, possibly `is rw` → container) are excluded: Raku keeps those
    /// container-wrapped and does not auto-sink them, and mutsu decontainerizes
    /// before `SinkPop` so the two cases are indistinguishable at runtime.
    fn stmt_value_may_user_sink(expr: &Expr) -> bool {
        match expr {
            Expr::MethodCall { .. } => true,
            Expr::DoStmt(inner) => match inner.as_ref() {
                // `do { ... }` carries the value of its last statement.
                Stmt::Expr(e) => Self::stmt_value_may_user_sink(e),
                _ => false,
            },
            _ => false,
        }
    }

    pub(super) fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_condition_expr(expr);
                self.code
                    .emit(OpCode::SinkPop(Self::stmt_value_may_user_sink(expr)));
            }
            // Feed split for an assignment to an already-declared variable:
            // `@x = SOURCE ==> SINK` — the feed operator is at Sequencer precedence
            // (looser than `=`), so it parses as `(@x = SOURCE) ==> SINK`. Relocate
            // the assignment into the feed's textually-left operand and run the feed
            // as a (sink-context) expression statement. (The `my`-declaration form
            // is split in the parser; see decl/my_decl_assign.rs.)
            Stmt::Assign {
                name,
                expr: feed @ Expr::Feed { .. },
                op: AssignOp::Assign,
            } if name != "*PID" => {
                let mut feed = feed.clone();
                {
                    let slot = crate::parser::feed_leftmost_operand_mut(&mut feed);
                    let source = std::mem::replace(slot, Expr::Literal(Value::Nil));
                    *slot = Expr::AssignExpr {
                        name: name.clone(),
                        expr: Box::new(source),
                        is_bind: false,
                    };
                }
                self.compile_condition_expr(&feed);
                self.code
                    .emit(OpCode::SinkPop(Self::stmt_value_may_user_sink(&feed)));
            }
            Stmt::Block(stmts) => {
                // Check for placeholder conflicts in blocks. Use the *shallow*
                // collector: a placeholder belongs to its innermost enclosing
                // block, so placeholders nested inside an inner closure
                // (`{ my $a; { $^a } }`) must NOT be attributed to this block
                // and falsely flagged as redeclaring this block's `my $a`.
                let placeholders = crate::ast::collect_placeholders_shallow(stmts);
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
                    self.compile_phaser_block_scope(stmts, false);
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
                    // Hoist sub declarations: register subs first so forward
                    // references like `&fa` work before the sub is textually
                    // declared (Raku sub hoisting semantics).
                    // Strip non-internal custom traits during hoisting — types/roles
                    // may not be registered yet; traits are applied during the normal pass.
                    for s in stmts.iter() {
                        if let Stmt::SubDecl { .. } = s {
                            let mut hoisted = s.clone();
                            if let Stmt::SubDecl { custom_traits, .. } = &mut hoisted {
                                custom_traits.retain(|(t, _)| {
                                    t.starts_with("__")
                                        || t == "default"
                                        || t.starts_with("DEPRECATED")
                                });
                            }
                            self.compile_stmt(&hoisted);
                        }
                    }
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
                // Collect the bound array variable name for skipping the
                // trailing Var statement that would force the LazyList via SinkPop.
                let mut bound_array_var: Option<String> = None;
                for s in stmts {
                    if has_bound_array_len
                        && let Stmt::VarDecl { name, .. } = s
                        && name.starts_with('@')
                    {
                        self.bind_vardecl = true;
                        bound_array_var = Some(name.clone());
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
                    // Skip the trailing Var(@name) expression for bound array
                    // declarations. Without this, SinkPop would eagerly force a
                    // lazy gather/take list bound via `:=`.
                    if let Some(ref bav) = bound_array_var
                        && let Stmt::Expr(Expr::Var(vname)) = s
                        && vname == bav
                    {
                        // Emit a Nil instead of the Var to avoid forcing.
                        self.code.emit(OpCode::LoadNil);
                        self.code.emit(OpCode::SinkPop(false));
                        continue;
                    }
                    self.compile_stmt(s);
                }
            }
            Stmt::MarkReadonly(name) => {
                let idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::MarkVarReadonly(idx));
            }
            Stmt::MarkBoundContainer(name) => {
                // Record `__mutsu_bound::NAME` = true in env so the whole-var
                // readonly check (`CheckReadOnly`) can tell a `:=`-bound
                // container (writable) apart from a `constant` one (immutable).
                let key = format!("__mutsu_bound::{}", name);
                let key_idx = self.code.add_constant(Value::str(key));
                let true_idx = self.code.add_constant(Value::Bool(true));
                self.code.emit(OpCode::LoadConst(true_idx));
                self.code.emit(OpCode::SetGlobal(key_idx));
            }
            Stmt::MarkBind => {
                // Handled by SyntheticBlock detection; no-op when compiled standalone.
            }
            Stmt::MarkSigillessReadonly(name) => {
                // Track sigilless locals so BareWord compilation can
                // distinguish them from `$`-sigiled variables.
                self.sigilless_locals.insert(name.clone());
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
                where_constraint,
            } => {
                // Record this declaration for an enclosing scope-isolating
                // do-block (string-interpolation `{...}`) so it can revert
                // exactly its own block-local declarations on exit.
                self.record_block_decl(name);
                // An inline `where` constraint on a scalar/sigilless variable
                // (e.g. `my $x where * > 0`, `my Int $n where { $_ %% 2 }`,
                // `my $v where &predicate`) is desugared into an anonymous subset
                // whose base is the declared type (or `Any`) and whose predicate is
                // the `where` expression. Reusing the subset machinery means the
                // existing type-constraint enforcement (TypeCheck on init and on
                // every assignment) checks the predicate for free. Collection
                // variables (`@`/`%`) are left untouched: a `where` there applies
                // to the whole container, which we do not yet model this way.
                let owned_type_constraint: Option<String> = match where_constraint {
                    Some(wc) if !name.starts_with('@') && !name.starts_with('%') => {
                        let anon = format!("__mutsu_anon_subset_{}", self.tmp_counter);
                        self.tmp_counter += 1;
                        let base = type_constraint.clone().unwrap_or_else(|| "Any".to_string());
                        let subset_stmt = Stmt::SubsetDecl {
                            name: Symbol::intern(&anon),
                            base,
                            predicate: Some((**wc).clone()),
                            version: String::new(),
                            is_export: false,
                            export_tags: Vec::new(),
                        };
                        let idx = self.code.add_stmt(subset_stmt);
                        self.code.emit(OpCode::RegisterSubset(idx));
                        Some(anon)
                    }
                    _ => type_constraint.clone(),
                };
                let type_constraint = &owned_type_constraint;
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
                // Track constant declarations so the compiler can avoid itemizing
                // them in `for` loops (constants have no Scalar container).
                let is_constant_decl = custom_traits.iter().any(|(t, _)| t == "__constant");
                // A `constant` that shadows an outer constant of the same name
                // (in an enclosing block or closure) is a fresh lexical binding,
                // not a reassignment of the outer package symbol. It must read
                // from its own local slot only and must NOT clobber the outer
                // constant's shared package store (`SetGlobalRaw`). Detect the
                // shadow *before* inserting this decl into the in-scope set — a
                // same-scope duplicate errors as X::Redeclaration below, so a hit
                // here is always an outer shadow.
                let shadows_outer_constant = is_constant_decl
                    && (self.constant_vars_in_scope.contains(name.as_str())
                        || self.outer_constant_names.contains(name.as_str()));
                if is_constant_decl {
                    // X::Redeclaration on a duplicate same-scope `constant` is only
                    // fired when the *sigil* matches. mutsu's AST strips the `$`
                    // from a scalar constant name, so `constant sym` (sigilless)
                    // and `constant $sym` (scalar) both arrive here as "sym"; firing
                    // on the bare name alone would wrongly reject that legal pair
                    // (see roast S06-operator-overloading/sub.t). Key the
                    // duplicate-detection set by the source sigil so only true
                    // same-sigil redeclarations are caught.
                    let constant_sigil = custom_traits
                        .iter()
                        .find(|(t, _)| t == "__constant_sigil")
                        .and_then(|(_, e)| match e {
                            Some(Expr::Literal(Value::Str(s))) => Some(s.to_string()),
                            _ => None,
                        })
                        .unwrap_or_default();
                    let redecl_key = format!("{}{}", constant_sigil, name);
                    if !self.constant_vars_current_scope.insert(redecl_key) {
                        let sym = name.trim_start_matches(['$', '@', '%', '&']).to_string();
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("symbol".to_string(), Value::str(sym));
                        attrs.insert("what".to_string(), Value::str_from("symbol"));
                        let err = Value::make_instance(Symbol::intern("X::Redeclaration"), attrs);
                        let idx = self.code.add_constant(err);
                        self.code.emit(OpCode::LoadConst(idx));
                        self.code.emit(OpCode::Die);
                        return;
                    }
                    self.constant_vars.insert(name.clone());
                    self.constant_vars_in_scope.insert(name.clone());
                }
                // X::ParametricConstant: typed @/% constants are forbidden
                if is_constant_decl
                    && type_constraint.is_some()
                    && (name.starts_with('@') || name.starts_with('%'))
                {
                    let err = Value::make_instance(
                        Symbol::intern("X::ParametricConstant"),
                        std::collections::HashMap::new(),
                    );
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
                let has_explicit_initializer =
                    custom_traits.iter().any(|(n, _)| n == "__has_initializer");
                let default_trait_expr =
                    custom_traits.iter().find_map(|(trait_name, trait_arg)| {
                        if trait_name == "default" {
                            trait_arg.as_ref()
                        } else {
                            None
                        }
                    });
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
                // For `our` redeclarations with no initializer (expr is Nil),
                // load the existing package variable value instead of
                // resetting to Nil. This makes `our $x = 3; ... our $x`
                // preserve the value 3 in the redeclaration.
                let is_our_redecl_nil = *is_our && matches!(expr, Expr::Literal(Value::Nil));
                // A scalar `:=` bind to a Positional makes the scalar a
                // non-container alias; record it so SetLocal can mark it
                // decontainerized (so `@a = $bound` flattens, not itemizes).
                // The parser tags such binds with the internal `__scalar_bind`
                // trait.
                let scalar_bind_decont = custom_traits.iter().any(|(t, _)| t == "__scalar_bind");
                // Capture whether this is a `:=` bind of an `@`/`%` container var
                // *before* the RHS-compilation branches below consume `bind_vardecl`.
                // Used by the `our` global store to skip the readonly check (the
                // var was marked readonly purely as a bind signal).
                let is_bound_container_vardecl =
                    self.bind_vardecl && (name.starts_with('@') || name.starts_with('%'));
                // A `constant` initializer is evaluated at BEGIN (compile) time,
                // so an uncaught exception while evaluating it surfaces as
                // X::Comp::BeginTime (with the original exception nested). Wrap
                // the RHS evaluation in a CheckPhaser scope so the top-level run
                // loop re-wraps any throw. Placed AFTER the X::Redeclaration /
                // X::ParametricConstant early-returns above so those compile-time
                // errors are not themselves wrapped.
                let constant_init_phaser_start = if is_constant_decl {
                    Some(self.code.emit(OpCode::CheckPhaserStart { end_ip: 0 }))
                } else {
                    None
                };
                if is_our_redecl_nil {
                    let qualified = self.qualify_variable_name(name);
                    let idx = self.code.add_constant(Value::str(qualified));
                    self.code.emit(OpCode::GetOurVar(idx));
                } else if self.bind_vardecl
                    && (!name.starts_with('@') && !name.starts_with('%')
                        || Self::is_simple_var_expr(expr))
                {
                    // `:=` binding for VarDecl: use compile_call_arg so WrapVarRef
                    // is emitted and the VM can set up aliases.  For @/% targets,
                    // only emit WrapVarRef when the RHS is a simple variable.
                    self.bind_vardecl = false;
                    self.scalar_bind_autovivify = true;
                    self.bind_terminal = true;
                    self.compile_call_arg(expr);
                    self.scalar_bind_autovivify = false;
                    self.bind_terminal = false;
                } else if scalar_bind_decont
                    && (matches!(expr, Expr::ArrayVar(_) | Expr::HashVar(_))
                        || matches!(expr, Expr::DoStmt(s) if matches!(s.as_ref(), Stmt::VarDecl { .. })))
                {
                    // A scalar `:=` bind to a *whole* container variable
                    // (`my $ref := @a` / `my $ref := %h`) must alias the same
                    // container, not snapshot it (so `$ref.push` mutates `@a`).
                    // Likewise a bind to an inline declaration (`my $a := my $b`,
                    // which the parser leaves as a bare `VarDecl` with the
                    // `__scalar_bind` trait rather than wrapping it in the
                    // MarkBind SyntheticBlock that `my $a := $b` gets) must alias
                    // the freshly-declared variable's container.
                    // Route through compile_call_arg so WrapVarRef is emitted and
                    // SetLocal's bind path shares one cell. (Scalar binds normally
                    // take the assignment path below, which only Arc-shares — a
                    // COW push would then detach the alias.)
                    self.scalar_bind_autovivify = true;
                    self.bind_terminal = true;
                    self.compile_call_arg(expr);
                    self.scalar_bind_autovivify = false;
                    self.bind_terminal = false;
                } else {
                    let rhs_expr = if has_default_trait
                        && !name.starts_with('@')
                        && !name.starts_with('%')
                        && matches!(expr, Expr::Literal(Value::Nil))
                    {
                        default_trait_expr.unwrap_or(expr)
                    } else {
                        expr
                    };
                    self.compile_assignment_rhs_for_target(name, rhs_expr);
                }
                if let Some(start_idx) = constant_init_phaser_start {
                    self.code.emit(OpCode::CheckPhaserEnd);
                    let end_ip = self.code.ops.len() as u32;
                    if let OpCode::CheckPhaserStart { end_ip: ref mut e } = self.code.ops[start_idx]
                    {
                        *e = end_ip;
                    }
                }
                // `constant @x = ...` should store a List, not an Array.
                // Coerce the value on the stack before storing.
                if name.starts_with('@') && custom_traits.iter().any(|(t, _)| t == "__constant") {
                    self.code.emit(OpCode::CoerceToList);
                }
                // Skip TypeCheck for hash declarations: the type constraint
                // applies to element values, not to the collection itself.
                // TODO: enforce per-element type constraints at assignment time.
                let is_hash = name.starts_with('%');
                let is_native_type = type_constraint.as_ref().is_some_and(|tc| {
                    crate::runtime::native_types::is_native_int_type(tc)
                        || matches!(tc.as_str(), "num" | "num32" | "num64" | "str")
                });
                if let Some(tc) = type_constraint
                    && !is_hash
                    && !has_default_trait
                    && !(has_explicit_initializer
                        && matches!(expr, Expr::Literal(Value::Nil))
                        && !is_native_type)
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
                    // A `:=` bind to a typed scalar reports X::TypeCheck::Binding
                    // on mismatch (e.g. `my Str $x := 3`), not Assignment.
                    if scalar_bind_decont && !name.starts_with('@') && !name.starts_with('%') {
                        self.code
                            .emit(OpCode::TypeCheckBind(tc_idx, Some(var_name_idx)));
                    } else {
                        self.code
                            .emit(OpCode::TypeCheck(tc_idx, Some(var_name_idx)));
                    }
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
                    let is_constant = custom_traits.iter().any(|(t, _)| t == "__constant");
                    // For `our` we need a second copy of the value to store into the
                    // global. Normally we `Dup` the raw initializer up front, but for
                    // a constant the global store (`SetGlobalRaw`) coerces the value
                    // (e.g. calling `.Map` on a `%`-sigil RHS) — coercing the raw
                    // value a second time would invoke that side-effecting coercion
                    // twice. Instead, for constants we re-read the already-coerced
                    // value from the local slot via `GetLocal` after `SetLocal`.
                    if *is_our && !is_constant {
                        self.code.emit(OpCode::Dup);
                    }
                    if self.bind_vardecl && (name.starts_with('@') || name.starts_with('%')) {
                        self.code.emit(OpCode::MarkBindContext);
                        self.bind_vardecl = false;
                    }
                    // Mark constant context so SetLocal uses List coercion for @ and
                    // skips Hash coercion for %, matching Raku's constant semantics.
                    if is_constant && (name.starts_with('@') || name.starts_with('%')) {
                        self.code.emit(OpCode::MarkConstantContext);
                    }
                    if has_explicit_initializer {
                        self.code.emit(OpCode::MarkExplicitInitializerContext);
                    }
                    // Mark this SetLocal as coming from a VarDecl so the VM
                    // can allow overwriting immutable containers (e.g. Blob)
                    // when the local slot is reused across loop iterations.
                    self.code.emit(OpCode::MarkVarDeclContext);
                    // A shaped declaration (`my @a[5] = ...`) keeps its declared
                    // shape; mark it so SetLocal does not strip the shape the way
                    // an unshaped value-copy (`my @u = @shaped`) does.
                    if custom_traits.iter().any(|(t, _)| t == "__shaped_decl") {
                        self.code.emit(OpCode::MarkShapedDeclContext);
                    }
                    // For % variables with QuantHash `is` traits, skip hash coercion
                    // so the trait handler gets the raw array/list value.
                    let has_quant_hash_trait = name.starts_with('%')
                        && custom_traits.iter().any(|(t, _)| {
                            let base = t.split('[').next().unwrap_or(t);
                            matches!(
                                base,
                                "BagHash" | "SetHash" | "MixHash" | "Bag" | "Set" | "Mix"
                            )
                        });
                    if has_quant_hash_trait {
                        self.code.emit(OpCode::MarkBindContext);
                    }
                    if scalar_bind_decont {
                        self.code.emit(OpCode::MarkScalarBindContext);
                    }
                    self.code.emit(OpCode::SetLocal(slot));
                    // A `constant` that shadows an outer constant of the same name
                    // (in an enclosing block or closure) is a fresh lexical binding,
                    // not a reassignment of the outer package symbol. Compile it as
                    // a pure `my` lexical: it lives only in its own local slot and
                    // never touches the shared package store (`our_locals` /
                    // `SetGlobalRaw`). Writing the package store would clobber the
                    // outer constant for sibling scopes, and the writeback merge for
                    // an `our` var declared inside a closure leaks the shadowing
                    // value back to the caller.
                    if *is_our && !shadows_outer_constant {
                        let qualified = self.qualify_variable_name(name);
                        // Track this slot as `our`-scoped so BlockScope restoration
                        // can sync the local from its global after block exit.
                        self.code
                            .our_locals
                            .push((slot as usize, qualified.clone()));
                        let idx = self.code.add_constant(Value::str(qualified));
                        // Constants should not have their values coerced by the
                        // @/% container rules: `constant @x` stores a List,
                        // `constant %x` stores a Map (not Array/Hash).
                        if is_constant {
                            // Re-read the value `SetLocal` already coerced (and
                            // cached in the slot) so `SetGlobalRaw` does not run
                            // the coercion — and its side effects — a second time.
                            self.code.emit(OpCode::GetLocal(slot));
                            self.code.emit(OpCode::SetGlobalRaw(idx));
                        } else {
                            // A `:=` bind of an `our` container var (`our %g := %h`)
                            // marks the var readonly as the bind signal; re-mark the
                            // bind context so the global store skips the readonly
                            // check (the mark is a bind signal, not a real RO).
                            if is_bound_container_vardecl {
                                self.code.emit(OpCode::MarkBindContext);
                            }
                            self.code.emit(OpCode::SetGlobal(idx));
                        }
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
                    // `is default` on native types is not allowed
                    if trait_name == "default"
                        && let Some(tc) = type_constraint
                        && matches!(
                            tc.as_str(),
                            "int"
                                | "num"
                                | "str"
                                | "uint"
                                | "int8"
                                | "int16"
                                | "int32"
                                | "int64"
                                | "uint8"
                                | "uint16"
                                | "uint32"
                                | "uint64"
                                | "num32"
                                | "num64"
                        )
                    {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(format!(
                            "X::Comp::Trait::NotOnNative: is default is not supported on native type {}",
                            tc
                        )));
                        attrs.insert("type".to_string(), Value::str("is".to_string()));
                        attrs.insert("subtype".to_string(), Value::str("default".to_string()));
                        let err = Value::make_instance(
                            Symbol::intern("X::Comp::Trait::NotOnNative"),
                            attrs,
                        );
                        let idx = self.code.add_constant(err);
                        self.code.emit(OpCode::LoadConst(idx));
                        self.code.emit(OpCode::Die);
                        return;
                    }
                    // `is default(expr)` with a type constraint: check that the
                    // default value is compatible with the type at compile time.
                    if trait_name == "default"
                        && let Some(tc) = type_constraint
                        && let Some(arg_expr) = trait_arg
                        && let Some(type_mismatch) = Self::check_default_type_mismatch(tc, arg_expr)
                    {
                        // Construct the expected type name based on sigil:
                        // @-sigil → Array[Type], %-sigil → Hash[Type], $-sigil → Type
                        let expected_type_name = if name.starts_with('@') {
                            format!("Array[{}]", tc)
                        } else if name.starts_with('%') {
                            format!("Hash[{}]", tc)
                        } else {
                            tc.to_string()
                        };
                        let err_msg = format!(
                            "X::Parameter::Default::TypeCheck: Default value '{}' will never bind to a parameter of type {}",
                            type_mismatch, expected_type_name
                        );
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(err_msg));
                        attrs.insert(
                            "expected".to_string(),
                            Value::Package(Symbol::intern(&expected_type_name)),
                        );
                        attrs.insert(
                            "got".to_string(),
                            if type_mismatch == "Nil" {
                                Value::Nil
                            } else {
                                Value::str(type_mismatch.clone())
                            },
                        );
                        let err = Value::make_instance(
                            Symbol::intern("X::Parameter::Default::TypeCheck"),
                            attrs,
                        );
                        let idx = self.code.add_constant(err);
                        self.code.emit(OpCode::LoadConst(idx));
                        self.code.emit(OpCode::Die);
                        return;
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
                // Mark constant variables as readonly so that subsequent
                // assignments are rejected at runtime.
                if is_constant_decl {
                    self.code.emit(OpCode::MarkVarReadonly(name_idx));
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
                // `self` is immutable — reject assignments at compile time
                if name == "self" {
                    self.code.emit(OpCode::AssignReadOnly);
                    return;
                }
                if name.starts_with('&')
                    && !name.contains("::")
                    && !self.local_map.contains_key(name.as_str())
                    && !name.starts_with("&!")
                {
                    self.code.emit(OpCode::AssignReadOnly);
                    return;
                }
                // For &!attr (callable private attributes), strip the &
                // sigil so the env key matches the attribute name (!attr).
                let effective_name = if name.starts_with("&!") {
                    &name[1..]
                } else {
                    name.as_str()
                };
                // Compile-time check: assigning a numeric literal to a typed
                // numeric variable with a mismatched type (e.g. `my Num $n; $n = 42`)
                // should produce X::Syntax::Number::LiteralType.
                if matches!(op, AssignOp::Assign)
                    && let Some(err) = self.check_literal_type_mismatch(effective_name, expr)
                {
                    let idx = self.code.add_constant(err);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                // A genuine `$*x = ...` assignment to a never-declared dynamic var
                // throws X::Dynamic::NotFound. Only the plain Assign form (not `:=`)
                // reaches here; param binding / element auto-viv / `my` decls use
                // other paths and are intentionally exempt.
                if matches!(op, AssignOp::Assign) {
                    self.maybe_emit_dynamic_var_check(effective_name);
                }
                // Emit readonly check for assignment to potentially readonly params.
                // Skip the check for `:=` (rebinding replaces the container).
                let name_idx = self
                    .code
                    .add_constant(Value::str(effective_name.to_string()));
                // The anonymous state scalar (`$`, compiled as `__ANON_STATE__`)
                // can never be readonly via the sigilless-binding mechanism, and
                // SetGlobal performs its own readonly_vars check, so the extra
                // CheckReadOnly op (which allocates a `__mutsu_sigilless_readonly::`
                // lookup key on every assignment) is pure overhead here.
                if !matches!(op, AssignOp::Bind) && effective_name != "__ANON_STATE__" {
                    self.code.emit(OpCode::CheckReadOnly(name_idx));
                }
                if matches!(op, AssignOp::Bind) {
                    let mut scalar_elem_bind = false;
                    if effective_name.starts_with('@') {
                        self.code.emit(OpCode::MarkBindContext);
                    } else if !effective_name.starts_with('%') && !effective_name.starts_with('&') {
                        // A scalar rebind (`$r := ...`) is still a bind: mark it
                        // so SetLocal records the bound-decont marker (it has no
                        // `__scalar_bind` trait like a `my $r := ...` VarDecl
                        // does). Without this the rebind is seen as a plain
                        // assignment and clears the marker, so `$r.VAR.^name`
                        // would wrongly report Scalar and `@a = $r` would itemize.
                        self.code.emit(OpCode::MarkScalarBindContext);
                        // A bound array/hash element (`$x := @a[1]`) must share a
                        // cell with the source slot so a later `$x = v` writes
                        // through to `@a[1]`, exactly like the `my $x := @a[1]`
                        // VarDecl path. Without cell promotion the rebind just
                        // snapshots the element value.
                        scalar_elem_bind = matches!(expr, Expr::Index { .. });
                    }
                    // Signal rebind context for cleanup of old bind pairs.
                    self.code.emit(OpCode::MarkRebindContext);
                    if scalar_elem_bind {
                        self.scalar_bind_autovivify = true;
                        self.bind_terminal = true;
                        self.compile_call_arg(expr);
                        self.scalar_bind_autovivify = false;
                        self.bind_terminal = false;
                    } else {
                        self.compile_call_arg(expr);
                    }
                } else {
                    // Fuse `$x OP= rhs` (parsed as `$x = $x OP rhs`) into an
                    // atomic RMW for plain env-named scalars (Track C). The fused
                    // op leaves the result on the stack; statement context wants
                    // nothing, so discard it.
                    if self.try_compile_fused_compound_assign(effective_name, expr) {
                        self.code.emit(OpCode::Pop);
                        return;
                    }
                    self.compile_assignment_rhs_for_target(effective_name, expr);
                }
                self.emit_set_named_var(effective_name);
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
                } else if Self::branch_declares_block_local(then_branch) {
                    self.compile_block_local_branch(then_branch);
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
                    } else if Self::branch_declares_block_local(else_branch) {
                        self.compile_block_local_branch(else_branch);
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
                // When the loop body contains `$_ := expr` (topic rebind via `:=`), wrap
                // the body in a BlockScope so the rebind is lexically scoped per iteration.
                // BlockScope naturally prevents `$_` from propagating out.
                // Note: plain `$_ =` does NOT trigger wrapping — that would break `with`-block
                // topic restoration which relies on `body_mutates_topic` for isolation.
                let body_rebinds_topic = Self::body_rebinds_topic(&loop_body);
                let loop_idx = self.code.emit(OpCode::WhileLoop {
                    cond_end: 0,
                    body_end: 0,
                    label: label.clone(),
                    collect: false,
                    isolate_topic: false,
                });
                self.compile_condition_expr(cond);
                self.code.patch_while_cond_end(loop_idx);
                if body_rebinds_topic {
                    self.compile_stmt(&Stmt::Block(loop_body.clone()));
                } else {
                    self.compile_body_with_implicit_try(&loop_body);
                }
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
                params_def,
                body,
                label,
                mode,
                rw_block,
                explicit_zero_params,
            } => {
                // Element-source writeback: `for %h<k>.values { $_ *= 2 }` /
                // `for @a[i].values { ... }`. The plain @/%-source writeback only
                // handles named container variables, so an element source (an
                // Index expression) is rewritten to copy the element into a temp
                // array, iterate that (reusing the array-source writeback), then
                // write the temp back into the element after the loop.
                if let Some(desugared) = self.desugar_for_element_source(stmt) {
                    for s in &desugared {
                        self.compile_stmt(s);
                    }
                    return;
                }
                let (pre_stmts, mut loop_body, post_stmts) =
                    self.expand_loop_phasers(body, label.as_deref());
                let non_setline_count = body
                    .iter()
                    .filter(|s| !matches!(s, Stmt::SetLine(_)))
                    .count();
                let restore_topic = param.is_none() && params.is_empty() && non_setline_count == 1;
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                // When there's a single named param (-> $k), store its name as a constant
                // so the VM can bind $k directly without overriding $_
                let param_idx = param
                    .as_ref()
                    .map(|p| self.code.add_constant(Value::str(p.clone())));
                let bind_stmts = Self::build_for_bind_stmts(
                    param,
                    param_def.as_ref(),
                    param_idx,
                    params,
                    params_def,
                );
                // A sigilless raw binding (`-> \v`) aliases the source element
                // directly; in Raku it is writable and modifications propagate
                // back to the source container (`for @a -> \v { v = 99 }` mutates
                // @a, and `for %h.kv -> \k, \v { v = 9 }` / `for %h.values -> \v`
                // write back through the value alias). Treat it like an rw param:
                // don't mark it readonly, and write modifications back.
                let has_sigilless = (**param_def).as_ref().is_some_and(|def| def.sigilless)
                    || params_def.iter().any(|def| def.sigilless);
                // Determine if this for-loop has rw params (via `<->` or `is rw` trait)
                let has_rw = *rw_block
                    || has_sigilless
                    || (**param_def)
                        .as_ref()
                        .is_some_and(|def| def.traits.iter().any(|t| t == "rw"));
                // `is copy` also makes the param writable (but without writeback)
                let has_copy = (**param_def)
                    .as_ref()
                    .is_some_and(|def| def.traits.iter().any(|t| t == "copy"));
                if !bind_stmts.is_empty() {
                    let mut merged = bind_stmts;
                    // After binding multi-param variables, mark them readonly
                    // (unless the block uses `<->` or `is rw`).
                    // Skip @-sigil and %-sigil params: they bind to a mutable
                    // Array/Hash container, so assignments must be allowed.
                    if !has_rw && !has_copy && !params.is_empty() {
                        for p in params {
                            if !p.starts_with('@') && !p.starts_with('%') {
                                merged.push(Stmt::MarkReadonly(p.clone()));
                            }
                        }
                    }
                    merged.extend(loop_body);
                    loop_body = merged;
                }
                let arity = if !params.is_empty() {
                    params.len() as u32
                } else {
                    1
                };
                let normalized_iterable = self.normalize_for_iterable(iterable);
                self.compile_expr(&normalized_iterable);
                if let Some(source_name) = Self::for_iterable_source_name(iterable) {
                    let source_idx = self.code.add_constant(Value::str(source_name));
                    if Self::for_iterable_is_reversed(iterable) {
                        self.code.emit(OpCode::TagContainerRefReversed(source_idx));
                    } else {
                        self.code.emit(OpCode::TagContainerRef(source_idx));
                    }
                }
                // If the for-loop parameter name already has a local slot
                // (e.g. from a prior `my $i` in an enclosing scope), we must
                // tell the VM so it can keep the local in sync with the env
                // on each iteration and on redo.
                let param_local = param
                    .as_ref()
                    .and_then(|p| self.local_map.get(p.as_str()).copied());
                let rw_param_names = if has_rw && !params.is_empty() {
                    params
                        .iter()
                        .map(|p| p.strip_prefix('\\').unwrap_or(p).to_string())
                        .collect()
                } else {
                    Vec::new()
                };
                let kv_mode = has_rw && Self::for_iterable_is_kv(iterable);
                let source_var_names = Self::for_iterable_var_names(iterable);
                // When the block parameter has a type constraint other than Mu
                // or Junction, junction items should be autothreaded (expanded
                // into their eigenstates).
                let autothread_junctions = match param_def.as_ref() {
                    Some(def) => match def.type_constraint.as_deref() {
                        None | Some("Mu") | Some("Junction") => false,
                        Some(_) => true,
                    },
                    // No param_def means default (Mu) — no autothreading
                    None => false,
                };
                let loop_idx = self.code.emit(OpCode::ForLoop {
                    param_idx,
                    param_local,
                    body_end: 0,
                    label: label.clone(),
                    arity,
                    collect: false,
                    restore_topic,
                    threaded: matches!(
                        *mode,
                        crate::ast::ForMode::Race | crate::ast::ForMode::Hyper
                    ),
                    // is_rw: param is writable (don't mark readonly)
                    is_rw: has_rw || has_copy,
                    // do_writeback: actually write back modifications to source container
                    do_writeback: has_rw && !has_copy,
                    rw_param_names,
                    kv_mode,
                    source_var_names,
                    autothread_junctions,
                    explicit_zero_params: *explicit_zero_params,
                    multi_param_names: params
                        .iter()
                        .map(|p| p.strip_prefix('\\').unwrap_or(p).to_string())
                        .collect(),
                    loop_var_wraps_element: Self::for_iterable_wraps_pair(iterable),
                    values_mode: Self::for_iterable_is_values_alias(iterable),
                    single_array_source: Self::for_single_array_source(iterable),
                });
                // Register sigilless for-params (`-> \v`, `-> \k, \v`) as
                // sigilless locals while compiling the body so postfix/prefix
                // `++`/`--` on the bare word (`v--`, `++v`) resolve to an
                // in-place PostDecrement/etc. on the bound env var rather than
                // the `__mutsu_incdec_nomatch` fallback. They are NOT readonly
                // (rw aliases), so we only add them to the set, not mark them.
                let sigilless_param_names: Vec<String> = if has_sigilless {
                    let mut names = Vec::new();
                    let single_sigilless = (**param_def).as_ref().is_some_and(|def| def.sigilless);
                    if let Some(p) = param.as_ref().filter(|_| single_sigilless) {
                        names.push(p.strip_prefix('\\').unwrap_or(p).to_string());
                    }
                    for (p, def) in params.iter().zip(params_def.iter()) {
                        if def.sigilless {
                            names.push(p.strip_prefix('\\').unwrap_or(p).to_string());
                        }
                    }
                    names
                } else {
                    Vec::new()
                };
                let newly_registered: Vec<String> = sigilless_param_names
                    .iter()
                    .filter(|n| self.sigilless_locals.insert((*n).clone()))
                    .cloned()
                    .collect();
                self.compile_body_with_implicit_try(&loop_body);
                for n in &newly_registered {
                    self.sigilless_locals.remove(n);
                }
                self.code.patch_loop_end(loop_idx);
                for s in &post_stmts {
                    self.compile_stmt(s);
                }
                // Restore the single named loop param after the post (LAST)
                // phasers ran. The ForLoop opcode deferred this restore (pushing
                // its saved binding) so the phasers could still see the param at
                // its final value. Emit only when a single non-@/% named param
                // exists, mirroring the VM's save condition so push/pop balance.
                if param
                    .as_ref()
                    .is_some_and(|p| !p.starts_with('@') && !p.starts_with('%'))
                {
                    self.code.emit(OpCode::RestoreForParam);
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
                    // Compile all args in source order, tracking the slip position
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
                    self.code.emit(OpCode::ExecCallSlip {
                        name_idx,
                        regular_arity: regular_count,
                        arg_sources_idx: None,
                        slip_pos,
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
                // A returned closure escapes the routine frame (escape analysis).
                self.with_escape(true, |c| c.compile_expr(expr));
                if self.is_routine {
                    self.code.emit(OpCode::Return);
                } else {
                    self.code
                        .emit(OpCode::ReturnFromNonRoutine(self.lexically_in_routine));
                }
            }
            Stmt::Die(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Die);
            }
            Stmt::Fail(expr) => {
                // A failed value is stored/propagated -> closure escapes.
                self.with_escape(true, |c| c.compile_expr(expr));
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
                self.with_escape(true, |c| c.compile_expr(expr));
                self.code.emit(OpCode::StrCoerce);
                self.emit_set_named_var(name);
            }
            Stmt::Assign { .. } => {
                self.code.emit(OpCode::AssignReadOnly);
            }
            // Given/When/Default
            Stmt::Given { topic, body } => {
                // A pointy `-> $_ is copy` block is desugared by the parser into a
                // `$_ = $_` head (see `pointy_topic_bind`): the topic becomes a
                // fresh, writable copy with NO writeback to the source. Detect that
                // head so the topic is not marked read-only (`given 42 -> $_ is copy`
                // must allow `$_ = ...`) and so a bare-variable topic is not tagged
                // for writeback (`given $x -> $_ is copy { $_ = ... }` leaves `$x`
                // untouched).
                let is_copy_topic = matches!(
                    body.first(),
                    Some(Stmt::Assign { name, op: AssignOp::Assign, expr: Expr::Var(t) })
                        if name == "_" && t == "_"
                );
                // An lvalue container *element* topic (`given %h<k>` /
                // `given @a[i]`) aliases that element rw: both `$_ = ...` and
                // container mutations (`.push`) propagate to the element. Push
                // the element value and tag the (container, index) source so the
                // body's final `$_` is written back. `topic_readonly` is false.
                let element_source = match topic {
                    Expr::Index {
                        target,
                        index,
                        is_positional,
                    } => Self::container_var_name(target)
                        // The element-source writeback optimization looks the
                        // container up by name in the locals store. An instance
                        // attribute (`%!h`, `@!a`, twigil `!`/`.`) lives in the
                        // instance attribute store, not in locals, so the lookup
                        // would read an empty container and bind `$_` to Nil.
                        // Fall through to evaluating the element value directly
                        // (read-only, but correct) for attribute containers.
                        .filter(|c| {
                            let after_sigil = c.strip_prefix(['$', '@', '%']).unwrap_or(c);
                            !after_sigil.starts_with(['!', '.'])
                        })
                        .map(|c| (c, index, *is_positional)),
                    _ => None,
                };
                let topic_readonly;
                if let Some((container, index, is_positional)) = element_source {
                    self.compile_expr(index);
                    let container_idx = self.code.add_constant(Value::str(container));
                    self.code.emit(OpCode::TagElementSource {
                        container_idx,
                        positional: is_positional,
                    });
                    topic_readonly = false;
                } else {
                    self.compile_expr(topic);
                    // `given my $x = EXPR` (a scalar declaration used as the topic)
                    // aliases the freshly-declared `$x` rw, exactly like `given $x`,
                    // so `$_ = ...` / `s///` inside the block write back to `$x`.
                    // The declaration is wrapped in a `DoStmt`; the VarDecl name has
                    // no sigil for scalars (arrays/hashes carry `@`/`%`).
                    let topic_decl_scalar = match topic {
                        Expr::DoStmt(inner) => match inner.as_ref() {
                            Stmt::VarDecl { name, .. }
                                if !name.starts_with('@')
                                    && !name.starts_with('%')
                                    && !name.starts_with('&') =>
                            {
                                Some(name.clone())
                            }
                            _ => None,
                        },
                        _ => None,
                    };
                    // `is copy` makes a detached copy, so suppress the
                    // source-writeback tag even for a bare-variable topic.
                    let source_name = if is_copy_topic {
                        None
                    } else {
                        match topic {
                            Expr::Var(name) => Some(name.clone()),
                            Expr::ArrayVar(name) => Some(format!("@{}", name)),
                            Expr::HashVar(name) => Some(format!("%{}", name)),
                            _ => topic_decl_scalar.clone(),
                        }
                    };
                    if let Some(source_name) = source_name {
                        let name_idx = self.code.add_constant(Value::str(source_name));
                        self.code.emit(OpCode::TagContainerRef(name_idx));
                    }
                    // The topic is read-only unless it is a bare scalar variable
                    // (`given $x` aliases `$x` rw), a scalar declaration topic
                    // (`given my $x = ...`), or an `is copy` writable copy.
                    // `given @a` / `given 42` / `given expr()` are read-only (Raku
                    // errors on `$_ = ...`).
                    topic_readonly = !is_copy_topic
                        && !matches!(topic, Expr::Var(_))
                        && topic_decl_scalar.is_none();
                }
                // A pointy block (`given @a -> @p { ... }`) is desugared by the
                // parser into `@p := $_` at the body head. Record that bound
                // parameter so the topic-source writeback reads its final value
                // (e.g. after `@p.push`) instead of `$_`, propagating the
                // mutation back to the source. `is copy` desugars to `@p = $_`
                // (an Assign, not a Bind), so it is not detected here and does
                // not write back.
                let pointy_param_idx = match body.first() {
                    Some(Stmt::Assign {
                        name,
                        op: AssignOp::Bind,
                        expr: Expr::Var(topic),
                    }) if topic == "_"
                        && !name.starts_with('!')
                        && !name.starts_with('.')
                        && !name.starts_with('&') =>
                    {
                        Some(self.code.add_constant(Value::str(name.clone())))
                    }
                    _ => None,
                };
                let given_idx = self.code.emit(OpCode::Given {
                    body_end: 0,
                    topic_readonly,
                    pointy_param_idx,
                });
                if Self::has_catch_or_control(body) {
                    self.compile_try(body, &None);
                    self.code.emit(OpCode::Pop);
                } else {
                    for (i, s) in body.iter().enumerate() {
                        let is_last = i == body.len() - 1;
                        if is_last {
                            if !self.compile_when_tail_stmt(s) {
                                self.compile_stmt(s);
                            }
                        } else {
                            self.compile_stmt(s);
                        }
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
                        if !self.compile_when_tail_stmt(s) {
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
                if Self::has_catch_or_control(body) {
                    self.compile_try(body, &None);
                    self.code.emit(OpCode::Pop);
                } else {
                    for (i, s) in body.iter().enumerate() {
                        let is_last = i == body.len() - 1;
                        if is_last {
                            if !self.compile_when_tail_stmt(s) {
                                self.compile_stmt(s);
                            }
                        } else {
                            self.compile_stmt(s);
                        }
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
            // HasDecl outside class context.
            Stmt::HasDecl {
                name,
                sigil,
                is_public,
                is_our,
                is_my,
                ..
            } => {
                // `our $.x` / `my $.x` in the mainline is not a fatal error in
                // Raku; it merely warns that generating an accessor method here
                // is useless (there is no package to attach it to). Only the
                // `has $.x` form (no `our`/`my`) is fatal.
                if *is_our || *is_my {
                    let warn_call = Expr::Call {
                        name: Symbol::intern("warn"),
                        args: vec![Expr::Literal(Value::str(
                            "Useless generation of accessor method in mainline".to_string(),
                        ))],
                    };
                    self.compile_expr(&warn_call);
                    self.code.emit(OpCode::Pop);
                    return;
                }
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
                let err = if let Some(pkg_kind) = self.current_package_kind {
                    // Inside a `module`/`package` body: a package cannot hold
                    // attributes — X::Attribute::Package.
                    let kind_str = pkg_kind.as_str();
                    let message = format!(
                        "A {} cannot have attributes, but you tried to declare '{}'",
                        kind_str, full_name
                    );
                    attrs.insert("name".to_string(), Value::str(full_name));
                    attrs.insert("package-kind".to_string(), Value::str(kind_str.to_string()));
                    attrs.insert("message".to_string(), Value::str(message));
                    Value::make_instance(Symbol::intern("X::Attribute::Package"), attrs)
                } else {
                    // Mainline: no enclosing package at all — X::Attribute::NoPackage.
                    let message = format!(
                        "You cannot declare attribute '{}' here; maybe you'd like a class or a role?",
                        full_name
                    );
                    attrs.insert("name".to_string(), Value::str(full_name));
                    attrs.insert("message".to_string(), Value::str(message));
                    Value::make_instance(Symbol::intern("X::Attribute::NoPackage"), attrs)
                };
                let idx = self.code.add_constant(err);
                self.code.emit(OpCode::LoadConst(idx));
                self.code.emit(OpCode::Die);
            }
            // DoesDecl/TrustsDecl outside class context are no-ops
            Stmt::DoesDecl { .. } | Stmt::TrustsDecl { .. } => {}

            // --- Take (gather/take) ---
            Stmt::Take(expr, is_rw) => {
                if *is_rw {
                    // `take-rw <lvalue>`: capture the source container (a shared
                    // `ContainerRef` cell), not a snapshot, so the gathered value
                    // keeps container identity with the original (`=:=`). Compile
                    // the operand exactly like a `:=` bind RHS: `scalar_bind_autovivify`
                    // makes an element subscript (`@a[i][j]`) yield the promoted
                    // cell; `bind_terminal` marks the leaf so a scalar element is
                    // boxed. A leading `// next` guard preserves the cell because
                    // `//` returns its (peeked) left operand unchanged when defined.
                    let saved_av = self.scalar_bind_autovivify;
                    let saved_term = self.bind_terminal;
                    self.scalar_bind_autovivify = true;
                    self.bind_terminal = true;
                    self.compile_expr(expr);
                    self.scalar_bind_autovivify = saved_av;
                    self.bind_terminal = saved_term;
                } else {
                    self.compile_expr(expr);
                }
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
                kind,
                is_unit,
                is_my,
            } => {
                let qualified_name = self.qualify_package_name(&name.resolve());
                // Detect stub body: `module Foo { ... }` — body is a stub operator
                // Filter out SetLine when checking, since the parser now emits
                // line tracking statements in all block bodies.
                let non_setline_body: Vec<_> = body
                    .iter()
                    .filter(|s| !matches!(s, Stmt::SetLine(_)))
                    .collect();
                let is_stub_body = non_setline_body.len() == 1
                    && matches!(non_setline_body[0], Stmt::Expr(Expr::Call { name: fn_name, .. })
                        if fn_name.resolve() == "__mutsu_stub_die"
                            || fn_name.resolve() == "__mutsu_stub_warn");
                if *is_unit {
                    // unit module/package — set package for the rest of the scope
                    self.current_package = qualified_name.clone();
                    self.in_unit_package = true;
                    // A `grammar` body legitimately holds attributes; only
                    // `module`/`package` bodies reject `has`.
                    if !matches!(kind, crate::ast::PackageKind::Grammar) {
                        self.current_package_kind = Some(*kind);
                    }
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
                    if *is_my {
                        self.code.emit(OpCode::RegisterPackageMy { name_idx });
                    } else {
                        self.code.emit(OpCode::RegisterPackage { name_idx });
                    }
                    // Clear any previous stub status for this package
                    self.code.emit(OpCode::ClearPackageStub { name_idx });
                    let pkg_idx = self.code.emit(OpCode::PackageScope {
                        name_idx,
                        body_end: 0,
                    });
                    let saved_package = self.current_package.clone();
                    let saved_in_unit = self.in_unit_package;
                    let saved_package_kind = self.current_package_kind;
                    self.current_package = qualified_name;
                    // Inside a non-unit `module Foo { ... }` block, runtime
                    // PackageScope handles the package context, so we must
                    // not pre-qualify nested class/role decls here.
                    self.in_unit_package = false;
                    // A `grammar` body legitimately holds attributes; only
                    // `module`/`package` bodies reject `has`.
                    self.current_package_kind = if matches!(kind, crate::ast::PackageKind::Grammar)
                    {
                        None
                    } else {
                        Some(*kind)
                    };
                    // Hoist `my sub` declarations so they are visible to earlier
                    // statements in the same package block (a `my sub` is lexically
                    // scoped and compile-time-visible throughout its block). Without
                    // this, a forward reference inside `package P { f(); my sub f {…} }`
                    // failed with "Unknown function". Sub bodies and inline blocks
                    // already hoist; the non-unit package body did not.
                    self.hoist_sub_decls(body, true);
                    for s in body {
                        self.compile_stmt(s);
                    }
                    self.current_package = saved_package;
                    self.in_unit_package = saved_in_unit;
                    self.current_package_kind = saved_package_kind;
                    self.code.patch_body_end(pkg_idx);
                }
            }

            // --- Phaser (BEGIN/CHECK/INIT) ---
            // These are extracted before compilation by extract_check_init_phasers()
            // and run in the correct order. If one remains (e.g. inside a sub body),
            // compile it inline as a fallback.
            Stmt::Phaser {
                kind: PhaserKind::Check,
                body,
            } => {
                // CHECK phasers run at compile time. If an error occurs inside
                // a CHECK phaser, Raku wraps it in X::Comp::BeginTime.
                self.compile_check_phaser(body);
            }
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
            } => {
                // BEGIN runs at compile time; an error thrown inside it is wrapped
                // in X::Comp::BeginTime (same mechanism as CHECK — the
                // CheckPhaserStart/End opcodes raise the `check_phaser_depth`
                // counter, and a throw at depth > 0 is wrapped). The opcodes don't
                // touch the stack, so the body's value/side-effects are preserved.
                self.compile_check_phaser(body);
            }
            Stmt::Phaser {
                kind: PhaserKind::Init | PhaserKind::Enter,
                body,
            } => {
                // INIT runs at run start, ENTER on block entry — neither is a
                // compile-time phaser, so their errors are NOT X::Comp::BeginTime.
                // ENTER at top-level scope compiles inline (in sub/method/closure
                // bodies it is handled by BlockScope and filtered out before
                // reaching this match arm).
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
                let site_id =
                    super::STATE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed) as u64;
                self.code.emit(OpCode::PhaserEnd { idx, site_id });
            }
            Stmt::Phaser {
                kind: PhaserKind::Pre,
                body,
            } => {
                // PRE phaser inline: compile body, check truthiness
                for (i, inner) in body.iter().enumerate() {
                    if i == body.len() - 1 {
                        match inner {
                            Stmt::Expr(expr) => self.compile_expr(expr),
                            _ => {
                                self.compile_stmt(inner);
                                self.compile_expr(&Expr::Literal(Value::Bool(true)));
                            }
                        }
                    } else {
                        self.compile_stmt(inner);
                    }
                }
                let condition_idx = self.phaser_condition_idx(body);
                self.code.emit(OpCode::CheckPhaser {
                    is_pre: true,
                    condition_idx,
                });
            }
            Stmt::Phaser {
                kind: PhaserKind::Post,
                body,
            } => {
                // POST phaser inline: compile body, check truthiness
                for (i, inner) in body.iter().enumerate() {
                    if i == body.len() - 1 {
                        match inner {
                            Stmt::Expr(expr) => self.compile_expr(expr),
                            _ => {
                                self.compile_stmt(inner);
                                self.compile_expr(&Expr::Literal(Value::Bool(true)));
                            }
                        }
                    } else {
                        self.compile_stmt(inner);
                    }
                }
                let condition_idx = self.phaser_condition_idx(body);
                self.code.emit(OpCode::CheckPhaser {
                    is_pre: false,
                    condition_idx,
                });
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
                custom_traits,
                ..
            } => {
                // Reject overriding a reserved special-form operator
                // (`infix:<=>`, `infix:<:=>`, `infix:<::=>`, `infix:<~~>`,
                // `prefix:<|>`) — these are handled directly by the compiler and
                // cannot be user-defined (X::Syntax::Extension::SpecialForm).
                if let Some(err_val) = Self::check_special_form_override(&name.resolve()) {
                    let idx = self.code.add_constant(err_val);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
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
                // Compile-time check: assignment to native-typed read-only
                // params (e.g. `sub foo(int $x) { $x = 42 }`) is an error.
                if let Some(err_val) =
                    Self::check_native_readonly_param_assignment(param_defs, body)
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
                // Extract deprecation info from custom_traits
                let deprecated_info = custom_traits.iter().find_map(|(t, _)| {
                    if t == "DEPRECATED" {
                        Some((
                            "Sub".to_string(),
                            name_str.clone(),
                            self.current_package.clone(),
                            String::new(),
                        ))
                    } else {
                        t.strip_prefix("DEPRECATED:").map(|msg| {
                            (
                                "Sub".to_string(),
                                name_str.clone(),
                                self.current_package.clone(),
                                msg.to_string(),
                            )
                        })
                    }
                });
                self.compile_sub_body_with_deprecation(
                    &name_str,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    body,
                    *multi,
                    state_group.as_deref(),
                    *is_rw,
                    *is_raw,
                    deprecated_info.clone(),
                );
                for (alt_params, alt_param_defs) in signature_alternates {
                    self.compile_sub_body_with_deprecation(
                        &name_str,
                        alt_params,
                        alt_param_defs,
                        return_type.as_ref(),
                        body,
                        *multi,
                        state_group.as_deref(),
                        *is_rw,
                        *is_raw,
                        deprecated_info.clone(),
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
                    precedence_trait: None,
                    signature_alternates: Vec::new(),
                    body: body.clone(),
                    multi: *multi,
                    is_rw: *is_rw,
                    is_raw: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    is_test_assertion: false,
                    supersede: false,
                    custom_traits: vec![("__mutsu_method_decl".to_string(), None)],
                };
                let idx = self.code.add_stmt(lowered);
                self.code.emit(OpCode::RegisterSub(idx));
                if name_expr.is_none() {
                    let mut method_params: Vec<String> = vec![
                        "self".to_string(),
                        "__ANON_STATE__".to_string(),
                        "?CLASS".to_string(),
                        "?ROLE".to_string(),
                    ];
                    method_params.extend(params.iter().cloned());
                    self.compile_sub_body(
                        &name.resolve(),
                        &method_params,
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
            Stmt::Use { module, arg, .. } if module == "lib" && arg.is_some() => {
                if let Some(expr) = arg {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::UseLibPath);
                }
            }
            Stmt::Use { module, arg, .. } if module == "lib" && arg.is_none() => {}
            Stmt::Use { module, arg, .. } if module == "dynamic-scope" => {
                self.apply_dynamic_scope_pragma(arg.as_ref());
            }
            Stmt::Use { module, arg, .. } if module == "newline" => {
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
            Stmt::Use { module, arg, .. } if module == "variables" => {
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
            Stmt::Use { module, arg, .. } if module == "attributes" => {
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
                    || module == "soft"
                    || module == "oo"
                    || module == "class"
                    // `use experimental :pack/:cached/:macros/...` enables
                    // experimental features that mutsu provides unconditionally
                    // (e.g. pack/unpack), so the pragma is a compile-time no-op.
                    || module == "experimental" => {}
            Stmt::Use { module, .. } if module == "MONKEY-TYPING" || module == "MONKEY" => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                self.code.emit(OpCode::UseModule {
                    name_idx,
                    tags_idx: None,
                });
            }
            Stmt::Use { module, arg, .. } if module == "Test::More" => {
                self.compile_test_more_use(arg);
            }
            Stmt::Use { module, tags, .. } if module == "Test" || module.starts_with("Test::") => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                let tags_idx = if tags.is_empty() {
                    None
                } else {
                    let entries = tags.iter().cloned().map(Value::str).collect::<Vec<Value>>();
                    Some(self.code.add_constant(Value::array(entries)))
                };
                self.code.emit(OpCode::UseModule { name_idx, tags_idx });
            }
            // `use if;` — the bare `if` pragma module itself is a no-op; it only
            // provides the `:if(...)` adverb handled below.
            Stmt::Use {
                module,
                condition: None,
                ..
            } if module == "if" => {}
            Stmt::Use {
                module,
                tags,
                condition,
                ..
            } => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                let tags_idx = if tags.is_empty() {
                    None
                } else {
                    let entries = tags.iter().cloned().map(Value::str).collect::<Vec<Value>>();
                    Some(self.code.add_constant(Value::array(entries)))
                };
                // `use Foo:if(EXPR)` (the `if` pragma): load the module only when
                // EXPR is true at runtime, evaluated here so platform-conditional
                // imports (`use Foo:if($*DISTRO.is-win)`) pick the right branch.
                if let Some(cond) = condition {
                    self.compile_expr(cond);
                    let skip = self.code.emit(OpCode::JumpIfFalse(0));
                    self.code.emit(OpCode::UseModule { name_idx, tags_idx });
                    self.code.patch_jump(skip);
                } else {
                    self.code.emit(OpCode::UseModule { name_idx, tags_idx });
                }
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
            Stmt::No { module, .. } => {
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
            Stmt::ClassDecl { body, .. } if self.emit_block_placeholder_die(body) => {}
            Stmt::ClassDecl { .. } => {
                // Pre-qualify the class name when compiling inside a
                // `unit module`/`unit class` body so that the runtime
                // registers it under the correct nested package
                // (e.g. `class D` inside `unit module A::B` → `A::B::D`).
                let stmt = self.qualify_decl_name(stmt);
                let idx = self.code.add_stmt(stmt);
                self.code.emit(OpCode::RegisterClass(idx));
            }
            Stmt::AugmentClass { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::AugmentClass(idx));
            }
            Stmt::RoleDecl { body, .. } if self.emit_block_placeholder_die(body) => {}
            Stmt::RoleDecl { .. } => {
                let stmt = self.qualify_decl_name(stmt);
                let idx = self.code.add_stmt(stmt);
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
                undefine_first,
            } => {
                // If undefine_first is set, assign Nil to the variable before saving.
                // This makes LetSave capture the undefined state, so on scope exit
                // the variable is restored to undefined (and its default value applies).
                if *undefine_first {
                    self.compile_expr(&Expr::Literal(Value::Nil));
                    self.emit_set_named_var(name);
                }
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
                        // For array/hash index assignment: compile as Stmt::Expr(IndexAssign)
                        let is_hash = name.starts_with('%');
                        let target_expr = if let Some(stripped) = name.strip_prefix('@') {
                            Expr::ArrayVar(stripped.to_string())
                        } else if let Some(stripped) = name.strip_prefix('%') {
                            Expr::HashVar(stripped.to_string())
                        } else {
                            Expr::Var(name.to_string())
                        };
                        let assign_expr = Expr::IndexAssign {
                            target: Box::new(target_expr),
                            index: Box::new(index.as_ref().unwrap().as_ref().clone()),
                            value: Box::new(val_expr.as_ref().clone()),
                            is_positional: !is_hash,
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
            Stmt::SetLine(line) => {
                self.last_source_line = Some(*line);
                self.code.emit(OpCode::SetSourceLine(*line));
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

    /// Like [`compile_last_stmt_as_topic`], but leaves the statement's value on
    /// the value stack instead of routing it through the topic. Used for the
    /// final statement of a block compiled in expression context (e.g. a `do`
    /// block), whose value the enclosing `DoBlockExpr` pops off the stack.
    pub(super) fn compile_last_stmt_as_value(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.compile_expr(expr),
            _ => {
                self.compile_stmt(stmt);
                self.compile_expr(&Expr::Literal(Value::Bool(true)));
            }
        }
    }

    /// Compile PRE phasers in forward source order.
    /// Add the source text of a PRE/POST phaser's condition as a constant and
    /// return its index, for the X::Phaser::PrePost `condition`/message. The
    /// condition is the phaser body's final expression (e.g. `0`); block-form
    /// bodies and non-trivial expressions yield `None`.
    fn phaser_condition_idx(&mut self, body: &[Stmt]) -> Option<u32> {
        let last = body.last()?;
        let Stmt::Expr(expr) = last else { return None };
        let src = Self::deparse_phaser_condition(expr)?;
        Some(self.code.add_constant(Value::str(src)))
    }

    /// Best-effort source reconstruction of a phaser condition expression,
    /// covering the common literal/variable forms (e.g. statement-form
    /// `PRE 0`). Non-trivial expressions yield `None`.
    fn deparse_phaser_condition(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Literal(v) => Some(v.to_string_value()),
            Expr::Var(name) => Some(format!("${}", name)),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::BareWord(name) => Some(name.to_string()),
            _ => None,
        }
    }

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
                let condition_idx = compiler.phaser_condition_idx(body);
                compiler.code.emit(OpCode::CheckPhaser {
                    is_pre: true,
                    condition_idx,
                });
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
                let condition_idx = compiler.phaser_condition_idx(body);
                compiler.code.emit(OpCode::CheckPhaser {
                    is_pre: false,
                    condition_idx,
                });
            }
        }
    }
}
