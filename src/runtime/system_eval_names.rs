use super::*;

impl Interpreter {
    fn find_unknown_code_var_in_expr(
        &self,
        expr: &Expr,
        declared: &HashSet<String>,
    ) -> Option<String> {
        match expr {
            Expr::CodeVar(name)
                if (name.starts_with("infix:<")
                    || name.starts_with("prefix:<")
                    || name.starts_with("postfix:<"))
                    && !self.has_function(name)
                    && !self.has_multi_function(name)
                    && !declared.contains(name) =>
            {
                Some(name.clone())
            }
            Expr::Binary { left, right, .. } => self
                .find_unknown_code_var_in_expr(left, declared)
                .or_else(|| self.find_unknown_code_var_in_expr(right, declared)),
            Expr::Unary { expr, .. }
            | Expr::Grouped(expr)
            | Expr::Eager(expr)
            | Expr::Itemize(expr)
            | Expr::DeitemizeForBind(expr)
            | Expr::IndirectTypeLookup(expr)
            | Expr::ZenSlice(expr) => self.find_unknown_code_var_in_expr(expr, declared),
            Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
                self.find_unknown_code_var_in_expr(target, declared)
                    .or_else(|| {
                        args.iter()
                            .find_map(|arg| self.find_unknown_code_var_in_expr(arg, declared))
                    })
            }
            Expr::DynamicMethodCall {
                target,
                name_expr,
                args,
                ..
            }
            | Expr::HyperMethodCallDynamic {
                target,
                name_expr,
                args,
                ..
            } => self
                .find_unknown_code_var_in_expr(target, declared)
                .or_else(|| self.find_unknown_code_var_in_expr(name_expr, declared))
                .or_else(|| {
                    args.iter()
                        .find_map(|arg| self.find_unknown_code_var_in_expr(arg, declared))
                }),
            Expr::Call { args, .. }
            | Expr::CaptureLiteral(args)
            | Expr::ArrayLiteral(args)
            | Expr::BracketArray(args, _) => args
                .iter()
                .find_map(|arg| self.find_unknown_code_var_in_expr(arg, declared)),
            Expr::StringInterpolation(parts) => parts
                .iter()
                .find_map(|part| self.find_unknown_code_var_in_expr(part, declared)),
            _ => None,
        }
    }

    fn find_unknown_code_var_in_stmt(
        &self,
        stmt: &Stmt,
        declared: &HashSet<String>,
    ) -> Option<String> {
        match stmt {
            Stmt::Expr(expr) => self.find_unknown_code_var_in_expr(expr, declared),
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                self.find_unknown_code_var_in_expr(expr, declared)
            }
            Stmt::Say(es) | Stmt::Print(es) | Stmt::Put(es) | Stmt::Note(es) => es
                .iter()
                .find_map(|expr| self.find_unknown_code_var_in_expr(expr, declared)),
            Stmt::Block(body)
            | Stmt::SyntheticBlock(body)
            | Stmt::Catch(body)
            | Stmt::Control(body)
            | Stmt::Default(body)
            | Stmt::React { body }
            | Stmt::Phaser { body, .. } => body
                .iter()
                .find_map(|stmt| self.find_unknown_code_var_in_stmt(stmt, declared)),
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => self
                .find_unknown_code_var_in_expr(cond, declared)
                .or_else(|| {
                    then_branch
                        .iter()
                        .find_map(|stmt| self.find_unknown_code_var_in_stmt(stmt, declared))
                })
                .or_else(|| {
                    else_branch
                        .iter()
                        .find_map(|stmt| self.find_unknown_code_var_in_stmt(stmt, declared))
                }),
            Stmt::For { iterable, body, .. }
            | Stmt::Whenever {
                supply: iterable,
                body,
                ..
            } => self
                .find_unknown_code_var_in_expr(iterable, declared)
                .or_else(|| {
                    body.iter()
                        .find_map(|stmt| self.find_unknown_code_var_in_stmt(stmt, declared))
                }),
            Stmt::While { cond, body, .. } => self
                .find_unknown_code_var_in_expr(cond, declared)
                .or_else(|| {
                    body.iter()
                        .find_map(|stmt| self.find_unknown_code_var_in_stmt(stmt, declared))
                }),
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                ..
            } => init
                .as_deref()
                .and_then(|stmt| self.find_unknown_code_var_in_stmt(stmt, declared))
                .or_else(|| {
                    cond.as_ref()
                        .and_then(|expr| self.find_unknown_code_var_in_expr(expr, declared))
                })
                .or_else(|| {
                    step.as_ref()
                        .and_then(|expr| self.find_unknown_code_var_in_expr(expr, declared))
                })
                .or_else(|| {
                    body.iter()
                        .find_map(|stmt| self.find_unknown_code_var_in_stmt(stmt, declared))
                }),
            Stmt::Given { topic, body } | Stmt::When { cond: topic, body } => self
                .find_unknown_code_var_in_expr(topic, declared)
                .or_else(|| {
                    body.iter()
                        .find_map(|stmt| self.find_unknown_code_var_in_stmt(stmt, declared))
                }),
            _ => None,
        }
    }

    pub(crate) fn check_eval_unknown_code_vars(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let mut declared: HashSet<String> = HashSet::new();
        for stmt in stmts {
            Self::collect_declared_routine_names(stmt, &mut declared);
        }
        for stmt in stmts {
            if let Some(name) = self.find_unknown_code_var_in_stmt(stmt, &declared) {
                let suggestions = self.suggest_routine_names(&name);
                return Err(RuntimeError::undeclared_routine_symbols(
                    &name,
                    format!("Undeclared routine:\n    {} used at line 1", name),
                    suggestions,
                ));
            }
        }
        Ok(())
    }

    pub(crate) fn check_eval_when_undeclared_types(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut local_classes: HashSet<String> = HashSet::new();
        let mut declared: HashSet<String> = HashSet::new();
        for stmt in stmts {
            if let Stmt::ClassDecl { name, .. } = stmt {
                local_classes.insert(name.resolve());
            }
            if let Stmt::RoleDecl { name, .. } = stmt {
                local_classes.insert(name.resolve());
            }
            Self::collect_declared_vars(stmt, &mut declared);
            Self::collect_declared_routine_names(stmt, &mut declared);
        }
        for stmt in stmts {
            if let Some(name) =
                self.find_undeclared_when_type_in_stmt(stmt, &local_classes, &declared)
            {
                let suggestions = self.suggest_type_names(&name);
                let sorrow = RuntimeError::undeclared_type_symbols(
                    &name,
                    format!("Undeclared name:\n    {} used at line 1", name),
                    suggestions,
                )
                .exception
                .map(|v| *v)
                .unwrap_or(Value::Nil);
                let mut group_attrs = std::collections::HashMap::new();
                group_attrs.insert("sorrows".to_string(), Value::array(vec![sorrow]));
                group_attrs.insert("worries".to_string(), Value::array(vec![]));
                group_attrs.insert("panic".to_string(), Value::Nil);
                group_attrs.insert(
                    "message".to_string(),
                    Value::str(format!("Undeclared name:\n    {} used at line 1", name)),
                );
                return Err(RuntimeError::typed("X::Comp::Group", group_attrs));
            }
        }
        Ok(())
    }

    fn find_undeclared_when_type_in_stmt(
        &self,
        stmt: &Stmt,
        local_classes: &HashSet<String>,
        declared: &HashSet<String>,
    ) -> Option<String> {
        match stmt {
            Stmt::Given { body, .. }
            | Stmt::Block(body)
            | Stmt::SyntheticBlock(body)
            | Stmt::Catch(body)
            | Stmt::Control(body)
            | Stmt::Default(body)
            | Stmt::React { body }
            | Stmt::Phaser { body, .. } => body.iter().find_map(|stmt| {
                self.find_undeclared_when_type_in_stmt(stmt, local_classes, declared)
            }),
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => then_branch
                .iter()
                .find_map(|stmt| {
                    self.find_undeclared_when_type_in_stmt(stmt, local_classes, declared)
                })
                .or_else(|| {
                    else_branch.iter().find_map(|stmt| {
                        self.find_undeclared_when_type_in_stmt(stmt, local_classes, declared)
                    })
                }),
            Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::Whenever { body, .. } => {
                body.iter().find_map(|stmt| {
                    self.find_undeclared_when_type_in_stmt(stmt, local_classes, declared)
                })
            }
            Stmt::Loop { init, body, .. } => init
                .as_deref()
                .and_then(|stmt| {
                    self.find_undeclared_when_type_in_stmt(stmt, local_classes, declared)
                })
                .or_else(|| {
                    body.iter().find_map(|stmt| {
                        self.find_undeclared_when_type_in_stmt(stmt, local_classes, declared)
                    })
                }),
            Stmt::When { cond, body } => self
                .find_undeclared_name_in_expr(cond, local_classes, declared)
                .or_else(|| {
                    body.iter().find_map(|stmt| {
                        self.find_undeclared_when_type_in_stmt(stmt, local_classes, declared)
                    })
                }),
            _ => None,
        }
    }

    fn stmt_declares_type_name(stmt: &Stmt) -> Option<String> {
        match stmt {
            Stmt::ClassDecl { name, .. }
            | Stmt::RoleDecl { name, .. }
            | Stmt::EnumDecl { name, .. }
            | Stmt::SubsetDecl { name, .. } => Some(name.resolve()),
            Stmt::Package { name, kind, .. }
                if !matches!(
                    kind,
                    crate::ast::PackageKind::Module | crate::ast::PackageKind::Package
                ) =>
            {
                Some(name.resolve())
            }
            _ => None,
        }
    }

    fn find_undeclared_type_in_type_body(
        &self,
        stmts: &[Stmt],
        visible_types: &HashSet<String>,
    ) -> Option<String> {
        for stmt in stmts {
            match stmt {
                Stmt::MethodDecl { body, .. }
                | Stmt::SubDecl { body, .. }
                | Stmt::TokenDecl { body, .. }
                | Stmt::RuleDecl { body, .. }
                | Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::Default(body)
                | Stmt::Catch(body)
                | Stmt::Control(body)
                | Stmt::React { body }
                | Stmt::Phaser { body, .. } => {
                    if let Some(name) = self.find_undeclared_type_in_type_body(body, visible_types)
                    {
                        return Some(name);
                    }
                }
                Stmt::ClassDecl { name, body, .. } | Stmt::RoleDecl { name, body, .. } => {
                    let mut nested_visible = visible_types.clone();
                    nested_visible.insert(name.resolve());
                    if let Some(name) =
                        self.find_undeclared_type_in_type_body(body, &nested_visible)
                    {
                        return Some(name);
                    }
                }
                Stmt::If {
                    cond,
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if let Some(name) =
                        self.find_undeclared_name_in_expr(cond, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                    if let Some(name) =
                        self.find_undeclared_type_in_type_body(then_branch, visible_types)
                    {
                        return Some(name);
                    }
                    if let Some(name) =
                        self.find_undeclared_type_in_type_body(else_branch, visible_types)
                    {
                        return Some(name);
                    }
                }
                Stmt::For { iterable, body, .. } => {
                    if let Some(name) =
                        self.find_undeclared_name_in_expr(iterable, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                    if let Some(name) = self.find_undeclared_type_in_type_body(body, visible_types)
                    {
                        return Some(name);
                    }
                }
                Stmt::While { cond, body, .. } => {
                    if let Some(name) =
                        self.find_undeclared_name_in_expr(cond, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                    if let Some(name) = self.find_undeclared_type_in_type_body(body, visible_types)
                    {
                        return Some(name);
                    }
                }
                Stmt::Loop {
                    init,
                    cond,
                    step,
                    body,
                    ..
                } => {
                    if let Some(init) = init.as_deref()
                        && let Some(name) =
                            self.find_undeclared_name_in_stmt(init, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                    if let Some(cond) = cond
                        && let Some(name) =
                            self.find_undeclared_name_in_expr(cond, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                    if let Some(step) = step
                        && let Some(name) =
                            self.find_undeclared_name_in_expr(step, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                    if let Some(name) = self.find_undeclared_type_in_type_body(body, visible_types)
                    {
                        return Some(name);
                    }
                }
                Stmt::Whenever { supply, body, .. } => {
                    if let Some(name) =
                        self.find_undeclared_name_in_expr(supply, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                    if let Some(name) = self.find_undeclared_type_in_type_body(body, visible_types)
                    {
                        return Some(name);
                    }
                }
                Stmt::Given { topic, body } => {
                    if let Some(name) =
                        self.find_undeclared_name_in_expr(topic, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                    if let Some(name) = self.find_undeclared_type_in_type_body(body, visible_types)
                    {
                        return Some(name);
                    }
                }
                Stmt::When { cond, body } => {
                    if let Some(name) =
                        self.find_undeclared_name_in_expr(cond, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                    if let Some(name) = self.find_undeclared_type_in_type_body(body, visible_types)
                    {
                        return Some(name);
                    }
                }
                _ => {
                    if let Some(name) =
                        self.find_undeclared_name_in_stmt(stmt, visible_types, &HashSet::new())
                    {
                        return Some(name);
                    }
                }
            }
        }
        None
    }

    pub(crate) fn check_eval_type_body_forward_refs(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut visible_types: HashSet<String> = HashSet::new();
        for stmt in stmts {
            match stmt {
                Stmt::ClassDecl { name, body, .. } | Stmt::RoleDecl { name, body, .. } => {
                    let mut body_visible = visible_types.clone();
                    body_visible.insert(name.resolve());
                    if let Some(missing) =
                        self.find_undeclared_type_in_type_body(body, &body_visible)
                    {
                        let suggestions = self.suggest_type_names(&missing);
                        return Err(RuntimeError::undeclared_type_symbols(
                            &missing,
                            format!("Undeclared name:\n    {} used at line 1", missing),
                            suggestions,
                        ));
                    }
                }
                Stmt::Package { body, .. } => {
                    if let Some(missing) =
                        self.find_undeclared_type_in_type_body(body, &visible_types)
                    {
                        let suggestions = self.suggest_type_names(&missing);
                        return Err(RuntimeError::undeclared_type_symbols(
                            &missing,
                            format!("Undeclared name:\n    {} used at line 1", missing),
                            suggestions,
                        ));
                    }
                }
                _ => {}
            }
            if let Some(name) = Self::stmt_declares_type_name(stmt) {
                visible_types.insert(name);
            }
        }
        Ok(())
    }

    /// Check for undeclared type names in EVAL'd code.
    /// Walks the AST looking for BareWord expressions that start with uppercase
    /// and aren't known types, classes, or packages. This mirrors Raku's
    /// compile-time check for undeclared symbols.
    /// A `BEGIN { ... }` block runs at compile time, so it can only see routines
    /// declared *before* it. Calling a sub that is declared *later* in the same
    /// unit (`BEGIN { ohnoes() }; sub ohnoes() {}`) is X::Undeclared::Symbols at
    /// BEGIN time, even though the sub exists by the end of the unit.
    pub(crate) fn check_eval_begin_forward_calls(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // All sub names declared at this level (forward + backward).
        let mut all_subs: HashSet<String> = HashSet::new();
        for s in stmts {
            if let Stmt::SubDecl { name, .. } = s {
                all_subs.insert(name.resolve());
            }
        }
        if all_subs.is_empty() {
            return Ok(());
        }
        let mut declared_before: HashSet<String> = HashSet::new();
        for s in stmts {
            match s {
                Stmt::SubDecl { name, .. } => {
                    declared_before.insert(name.resolve());
                }
                Stmt::Phaser {
                    kind: PhaserKind::Begin,
                    body,
                } => {
                    let mut calls: HashSet<String> = HashSet::new();
                    Self::collect_call_names_in_stmts(body, &mut calls);
                    // A call to a sub declared only *after* this BEGIN.
                    if let Some(fwd) = calls
                        .iter()
                        .find(|c| all_subs.contains(*c) && !declared_before.contains(*c))
                    {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("symbol".to_string(), Value::str(fwd.clone()));
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!("Undeclared routine:\n    {} used at line 1", fwd)),
                        );
                        return Err(RuntimeError::typed("X::Undeclared::Symbols", attrs));
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Collect the names of bare function calls (`foo(...)`) appearing anywhere
    /// in `stmts`, recursing into nested expressions and block bodies.
    fn collect_call_names_in_stmts(stmts: &[Stmt], out: &mut HashSet<String>) {
        for s in stmts {
            match s {
                Stmt::Expr(e) => Self::collect_call_names_in_expr(e, out),
                Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                    Self::collect_call_names_in_expr(expr, out)
                }
                Stmt::Say(es) | Stmt::Print(es) | Stmt::Put(es) | Stmt::Note(es) => {
                    for e in es {
                        Self::collect_call_names_in_expr(e, out);
                    }
                }
                Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                    Self::collect_call_names_in_stmts(body, out)
                }
                _ => {}
            }
        }
    }

    fn collect_call_names_in_expr(expr: &Expr, out: &mut HashSet<String>) {
        match expr {
            Expr::Call { name, args } => {
                out.insert(name.resolve());
                for a in args {
                    Self::collect_call_names_in_expr(a, out);
                }
            }
            Expr::Binary { left, right, .. } => {
                Self::collect_call_names_in_expr(left, out);
                Self::collect_call_names_in_expr(right, out);
            }
            Expr::Unary { expr, .. } | Expr::Grouped(expr) => {
                Self::collect_call_names_in_expr(expr, out)
            }
            Expr::MethodCall { target, args, .. } => {
                Self::collect_call_names_in_expr(target, out);
                for a in args {
                    Self::collect_call_names_in_expr(a, out);
                }
            }
            Expr::StringInterpolation(parts) => {
                for p in parts {
                    Self::collect_call_names_in_expr(p, out);
                }
            }
            _ => {}
        }
    }

    pub(crate) fn check_eval_undeclared_names(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        self.check_eval_unknown_code_vars(stmts)?;
        self.check_eval_when_undeclared_types(stmts)?;
        self.check_eval_type_body_forward_refs(stmts)?;
        // Collect class/role names and locally-declared variables/subs from
        // this EVAL code. Both are valid references so they should not be
        // flagged as undeclared bareword names.
        let mut local_classes: HashSet<String> = HashSet::new();
        for stmt in stmts {
            if let Stmt::ClassDecl { name, .. } = stmt {
                local_classes.insert(name.resolve());
            }
            if let Stmt::RoleDecl { name, .. } = stmt {
                local_classes.insert(name.resolve());
            }
        }
        let mut declared: HashSet<String> = HashSet::new();
        for stmt in stmts {
            Self::collect_declared_vars(stmt, &mut declared);
        }
        // Normalize collected names: strip leading sigils so bareword lookups
        // (e.g. `foo` after `my &foo := ...`) resolve correctly.
        let bare: Vec<String> = declared
            .iter()
            .filter_map(|n| n.strip_prefix(['$', '@', '%', '&']).map(|s| s.to_string()))
            .collect();
        for n in bare {
            declared.insert(n);
        }
        // Also include any sub/method/grammar/enum names defined at top-level.
        for stmt in stmts {
            Self::collect_declared_routine_names(stmt, &mut declared);
        }
        for stmt in stmts {
            if let Some(name) = self.find_undeclared_name_in_stmt(stmt, &local_classes, &declared) {
                let suggestions = self.suggest_type_names(&name);
                return Err(RuntimeError::undeclared_type_symbols(
                    &name,
                    format!("Undeclared name:\n    {} used at line 1", name),
                    suggestions,
                ));
            }
        }
        Ok(())
    }

    fn collect_declared_routine_names(stmt: &Stmt, out: &mut HashSet<String>) {
        match stmt {
            Stmt::SubDecl { name, .. } | Stmt::MethodDecl { name, .. } => {
                out.insert(name.resolve());
            }
            Stmt::EnumDecl { name, variants, .. } => {
                out.insert(name.resolve());
                for (vname, _) in variants {
                    out.insert(vname.clone());
                }
            }
            Stmt::SyntheticBlock(body) => {
                for s in body {
                    Self::collect_declared_routine_names(s, out);
                }
            }
            _ => {}
        }
    }

    /// Find an undeclared type name (BareWord starting with uppercase) in a statement.
    fn find_undeclared_name_in_stmt(
        &self,
        stmt: &Stmt,
        local_classes: &HashSet<String>,
        declared: &HashSet<String>,
    ) -> Option<String> {
        match stmt {
            Stmt::Expr(expr) => self.find_undeclared_name_in_expr(expr, local_classes, declared),
            // A `constant NAME = <init>` whose initializer references an undeclared
            // bareword (`constant foo = bar`) is X::Undeclared::Symbols. Scoped to
            // `constant` only (custom trait `__constant`) to avoid false positives
            // on ordinary `my $x = ...` initializers that resolve at runtime.
            Stmt::VarDecl {
                expr,
                custom_traits,
                ..
            } if custom_traits.iter().any(|(t, _)| t == "__constant") => {
                self.find_undeclared_name_in_expr(expr, local_classes, declared)
            }
            // `given X { when SomeUndeclaredType {...} }`: a `when` whose condition
            // is an undeclared (bare uppercase) type name is X::Undeclared.
            Stmt::Given { body, .. } => body
                .iter()
                .find_map(|s| self.find_undeclared_name_in_stmt(s, local_classes, declared)),
            Stmt::When { cond, .. } => {
                self.find_undeclared_name_in_expr(cond, local_classes, declared)
            }
            // `enum E (Foo, Bar)` with bare *terms* in the parenthesised body
            // (parsed as a single `__DYNAMIC__` value expression) references
            // undeclared symbols — `<Foo Bar>` is the autoquoting form. Scan the
            // value expressions for undeclared barewords.
            Stmt::EnumDecl { variants, .. } => variants.iter().find_map(|(_, vexpr)| {
                vexpr
                    .as_ref()
                    .and_then(|e| self.find_undeclared_name_in_expr(e, local_classes, declared))
            }),
            // `use Module BareWord` — a bare identifier as a positional import
            // argument is a term reference, not an import symbol (those are
            // strings / `<...>` / `:tags`). An undeclared one is
            // X::Undeclared::Symbols (rakudo: "Undeclared name: ...").
            Stmt::Use { arg: Some(arg), .. } | Stmt::No { arg: Some(arg), .. } => {
                self.find_undeclared_name_in_expr(arg, local_classes, declared)
            }
            _ => None,
        }
    }

    /// Find an undeclared bareword name in an expression.
    /// Checks BareWord nodes that are not known types, classes, functions, or
    /// declared in the current EVAL scope.
    fn find_undeclared_name_in_expr(
        &self,
        expr: &Expr,
        local_classes: &HashSet<String>,
        declared: &HashSet<String>,
    ) -> Option<String> {
        match expr {
            Expr::BareWord(name) => {
                // Skip well-known constants and special names
                if matches!(
                    name.as_str(),
                    "NaN" | "Inf" | "Empty" | "True" | "False" | "Nil" | "Any" | "Mu"
                ) {
                    return None;
                }
                // Skip Raku keywords / special syntactic words that are
                // legitimately parsed as BareWord but should not be treated
                // as undeclared names.
                if matches!(
                    name.as_str(),
                    "self"
                        | "given"
                        | "when"
                        | "default"
                        | "if"
                        | "elsif"
                        | "else"
                        | "unless"
                        | "with"
                        | "without"
                        | "orwith"
                        | "for"
                        | "while"
                        | "until"
                        | "loop"
                        | "repeat"
                        | "do"
                        | "try"
                        | "anon"
                        | "my"
                        | "our"
                        | "has"
                        | "state"
                        | "sub"
                        | "method"
                        | "submethod"
                        | "multi"
                        | "proto"
                        | "only"
                        | "class"
                        | "role"
                        | "grammar"
                        | "token"
                        | "rule"
                        | "regex"
                        | "module"
                        | "package"
                        | "enum"
                        | "subset"
                        | "constant"
                        | "return"
                        | "leave"
                        | "last"
                        | "next"
                        | "redo"
                        | "succeed"
                        | "proceed"
                        | "die"
                        | "fail"
                        | "is"
                        | "does"
                        | "of"
                        | "where"
                        | "but"
                        | "use"
                        | "no"
                        | "need"
                        | "require"
                        | "import"
                        | "lazy"
                        | "eager"
                        | "hyper"
                        | "race"
                        | "sink"
                        | "react"
                        | "supply"
                        | "whenever"
                        | "start"
                        | "gather"
                        | "take"
                        | "quietly"
                        | "now"
                        | "time"
                        | "rand"
                        | "pi"
                        | "e"
                        | "tau"
                        | "i"
                ) {
                    return None;
                }
                // Skip names with :: (package-qualified)
                if name.contains("::") {
                    return None;
                }
                // Skip if it's a known type, class, role, enum, function, or env entry
                if self.has_type(name)
                    || self.has_class(name)
                    || self.has_function(name)
                    || self.has_multi_function(name)
                    || self.env().contains_key(name)
                    // `our`-scoped constants/variables installed in the package
                    // survive in `our_vars` even after their lexical block exits.
                    || self.get_our_var(name).is_some()
                    || local_classes.contains(name.as_str())
                    || declared.contains(name.as_str())
                    || crate::runtime::Interpreter::is_builtin_type(name)
                    || crate::runtime::Interpreter::is_implicit_zero_arg_builtin(name)
                {
                    return None;
                }
                Some(name.clone())
            }
            Expr::Binary { left, right, .. } => self
                .find_undeclared_name_in_expr(left, local_classes, declared)
                .or_else(|| self.find_undeclared_name_in_expr(right, local_classes, declared)),
            Expr::Unary { expr, .. } => {
                self.find_undeclared_name_in_expr(expr, local_classes, declared)
            }
            Expr::Grouped(expr)
            | Expr::Eager(expr)
            | Expr::Itemize(expr)
            | Expr::DeitemizeForBind(expr)
            | Expr::IndirectTypeLookup(expr)
            | Expr::ZenSlice(expr) => {
                self.find_undeclared_name_in_expr(expr, local_classes, declared)
            }
            Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
                self.find_undeclared_name_in_expr(target, local_classes, declared)
                    .or_else(|| {
                        args.iter().find_map(|arg| {
                            self.find_undeclared_name_in_expr(arg, local_classes, declared)
                        })
                    })
            }
            Expr::DynamicMethodCall {
                target,
                name_expr,
                args,
                ..
            }
            | Expr::HyperMethodCallDynamic {
                target,
                name_expr,
                args,
                ..
            } => self
                .find_undeclared_name_in_expr(target, local_classes, declared)
                .or_else(|| self.find_undeclared_name_in_expr(name_expr, local_classes, declared))
                .or_else(|| {
                    args.iter().find_map(|arg| {
                        self.find_undeclared_name_in_expr(arg, local_classes, declared)
                    })
                }),
            Expr::Call { args, .. } | Expr::CaptureLiteral(args) | Expr::ArrayLiteral(args) => args
                .iter()
                .find_map(|arg| self.find_undeclared_name_in_expr(arg, local_classes, declared)),
            Expr::StringInterpolation(parts) => {
                for part in parts {
                    if let Some(name) =
                        self.find_undeclared_name_in_expr(part, local_classes, declared)
                    {
                        return Some(name);
                    }
                }
                None
            }
            _ => None,
        }
    }
}
