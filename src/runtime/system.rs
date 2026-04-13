use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    fn parse_and_eval_with_operators(
        &mut self,
        src: &str,
        op_names: &[String],
        op_assoc: &HashMap<String, String>,
        imported_names: &[String],
    ) -> Result<Value, RuntimeError> {
        match crate::parser::parse_program_with_operators(src, op_names, op_assoc, imported_names) {
            Ok((stmts, _)) => {
                self.check_eval_class_redeclarations(&stmts)?;
                self.check_eval_undeclared_vars(&stmts)?;
                self.check_eval_undeclared_names(&stmts)?;
                // When EVAL is called inside a class body, MethodDecl statements
                // should be added to the enclosing class rather than lowered to subs.
                let stmts = self.inject_eval_methods_into_class(stmts);
                let value = self.eval_block_value(&stmts)?;
                if self.eval_result_is_unresolved_bareword(&stmts, &value) {
                    return Err(RuntimeError::undeclared_symbols("Undeclared name"));
                }
                self.check_unresolved_stubs()?;
                // When the last statement is an assignment, the VM pops the
                // value from the stack, so eval_block_value returns Nil/Any.
                // In Raku, EVAL returns the value of the last expression,
                // which for assignments is the assigned value.
                let value = if matches!(value, Value::Nil)
                    || matches!(&value, Value::Package(name) if name == "Any")
                {
                    if let Some(Stmt::Assign { name, .. }) = stmts.last() {
                        self.env.get(name).cloned().unwrap_or(value)
                    } else {
                        value
                    }
                } else {
                    value
                };
                Ok(value)
            }
            Err(parse_err) => {
                // BEGIN blocks should execute even when a later parse error occurs.
                // Do a partial parse to find any BEGIN phasers and execute them
                // before returning the parse error.
                let (partial_stmts, _) = crate::parser::parse_program_partial_with_operators(
                    src,
                    op_names,
                    op_assoc,
                    imported_names,
                );
                self.execute_begin_phasers(&partial_stmts);
                Err(parse_err)
            }
        }
    }

    /// When EVAL is called inside a class body, method declarations should be
    /// added to the enclosing class rather than lowered to subs. This method
    /// extracts MethodDecl statements and injects them into the current class,
    /// returning the remaining statements for normal evaluation.
    fn inject_eval_methods_into_class(&mut self, stmts: Vec<Stmt>) -> Vec<Stmt> {
        // Only applies when current_package is a class being defined
        let class_name = self.current_package.clone();
        if !self.classes.contains_key(&class_name) {
            return stmts;
        }
        let mut remaining = Vec::new();
        for stmt in stmts {
            if let Stmt::MethodDecl {
                name: method_name,
                name_expr: _,
                param_defs,
                body: method_body,
                multi,
                is_rw,
                is_private,
                is_my,
                return_type,
                is_default_candidate,
                deprecated_message,
                ..
            } = &stmt
            {
                let resolved_method_name = method_name.resolve();
                let effective_param_defs = Self::effective_method_param_defs(param_defs, false);
                let effective_params: Vec<String> = effective_param_defs
                    .iter()
                    .map(|p| p.name.clone())
                    .collect();
                let def = MethodDef {
                    params: effective_params,
                    param_defs: effective_param_defs,
                    body: std::sync::Arc::new(method_body.clone()),
                    is_rw: *is_rw,
                    is_private: *is_private,
                    is_multi: *multi,
                    is_my: *is_my,
                    role_origin: None,
                    original_role: None,
                    return_type: return_type.clone(),
                    compiled_code: None,
                    delegation: None,
                    is_default: *is_default_candidate,
                    deprecated_message: deprecated_message.clone(),
                };
                if let Some(class_def) = self.classes.get_mut(&class_name) {
                    if *multi {
                        class_def
                            .methods
                            .entry(resolved_method_name)
                            .or_default()
                            .push(def);
                    } else {
                        class_def.methods.insert(resolved_method_name, vec![def]);
                    }
                }
            } else {
                remaining.push(stmt);
            }
        }
        remaining
    }

    /// Execute BEGIN phasers found in a list of statements (used for partial
    /// parse results where a later parse error prevents full evaluation).
    fn execute_begin_phasers(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            if let Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
            } = stmt
            {
                let _ = self.eval_block_value(body);
            }
        }
    }

    pub(crate) fn check_eval_class_redeclarations(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut seen_classes: HashMap<String, bool> = HashMap::new();
        for stmt in stmts {
            let (name, body, is_lexical) = match stmt {
                Stmt::ClassDecl {
                    name,
                    body,
                    is_lexical,
                    ..
                } => (name.resolve(), body, *is_lexical),
                _ => continue,
            };
            let body_no_sl: Vec<_> = body
                .iter()
                .filter(|s| !matches!(s, Stmt::SetLine(_)))
                .collect();
            let is_stub = body_no_sl.len() == 1
                && matches!(body_no_sl[0], Stmt::Expr(Expr::Call { name: fn_name, .. })
                    if *fn_name == "__mutsu_stub_die" || *fn_name == "__mutsu_stub_warn");
            // Check against classes already registered in the outer environment.
            // A non-stub, non-lexical class that already exists cannot be redeclared.
            // Lexical classes (`my class`) are allowed to shadow outer names.
            // Only check package-qualified names (containing `::`) because simple
            // names may have been registered by block-scoped class declarations that
            // leaked into the global registry (a known scoping limitation).
            if !is_stub && !is_lexical && name.contains("::") && self.has_class(&name) {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: Redeclaration of symbol '{}'",
                    name
                )));
            }

            match seen_classes.get(&name) {
                None => {
                    seen_classes.insert(name.to_string(), is_stub);
                }
                Some(false) => {
                    return Err(RuntimeError::new(format!(
                        "X::Redeclaration: Redeclaration of symbol '{}'",
                        name
                    )));
                }
                Some(true) if is_stub => {
                    return Err(RuntimeError::new(format!(
                        "X::Redeclaration: Redeclaration of symbol '{}'",
                        name
                    )));
                }
                Some(true) => {
                    seen_classes.insert(name.to_string(), false);
                }
            }
        }
        Ok(())
    }

    /// Check for undeclared variable references in EVAL'd code.
    /// Collects declared variable names from VarDecl/Assign nodes and checks
    /// Var references against them and the outer environment.
    pub(crate) fn check_eval_undeclared_vars(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let mut declared: HashSet<String> = HashSet::new();
        for stmt in stmts {
            Self::check_self_referential_init(stmt)?;
            Self::collect_declared_vars(stmt, &mut declared);
            if let Some(var_name) = self.find_undeclared_var_in_stmt(stmt, &declared) {
                let symbol = format!("${}", var_name);
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("name".to_string(), Value::str(symbol.clone()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("Variable '{}' is not declared.", symbol)),
                );
                return Err(RuntimeError::typed("X::Undeclared", attrs));
            }
        }
        Ok(())
    }

    fn check_self_referential_init(stmt: &Stmt) -> Result<(), RuntimeError> {
        let (name, expr, custom_traits) = match stmt {
            Stmt::VarDecl {
                name,
                expr,
                custom_traits,
                ..
            } => (name, expr, custom_traits),
            Stmt::SyntheticBlock(body) => {
                for s in body {
                    Self::check_self_referential_init(s)?;
                }
                return Ok(());
            }
            _ => return Ok(()),
        };
        let has_init = custom_traits.iter().any(|(n, _)| n == "__has_initializer")
            || !matches!(expr, Expr::Literal(Value::Nil));
        let self_ref_expr = has_init && Self::expr_references_var(expr, name);
        let self_ref_trait = custom_traits.iter().any(|(_, arg)| {
            arg.as_ref()
                .is_some_and(|e| Self::expr_references_var(e, name))
        });
        if self_ref_expr || self_ref_trait {
            let sigil = if name.starts_with('@') || name.starts_with('%') {
                ""
            } else {
                "$"
            };
            let symbol = format!("{}{}", sigil, name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::str(symbol));
            return Err(RuntimeError::typed(
                "X::Syntax::Variable::Initializer",
                attrs,
            ));
        }
        Ok(())
    }

    fn expr_references_var(expr: &Expr, var_name: &str) -> bool {
        let bare = var_name
            .strip_prefix('%')
            .or_else(|| var_name.strip_prefix('@'))
            .or_else(|| var_name.strip_prefix('$'))
            .unwrap_or(var_name);
        match expr {
            Expr::Var(name) => name == var_name || name == bare,
            Expr::HashVar(name) | Expr::ArrayVar(name) => name == bare,
            Expr::Binary { left, right, .. } | Expr::MetaOp { left, right, .. } => {
                Self::expr_references_var(left, var_name)
                    || Self::expr_references_var(right, var_name)
            }
            Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => {
                Self::expr_references_var(expr, var_name)
            }
            Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
                Self::expr_references_var(target, var_name)
                    || args.iter().any(|a| Self::expr_references_var(a, var_name))
            }
            Expr::Index { target, index, .. } => {
                Self::expr_references_var(target, var_name)
                    || Self::expr_references_var(index, var_name)
            }
            Expr::ArrayLiteral(items) => {
                items.iter().any(|i| Self::expr_references_var(i, var_name))
            }
            _ => false,
        }
    }

    fn collect_declared_vars(stmt: &Stmt, out: &mut HashSet<String>) {
        match stmt {
            Stmt::VarDecl { name, expr, .. } => {
                out.insert(name.clone());
                Self::collect_declared_vars_in_expr(expr, out);
            }
            Stmt::Assign { name, .. } => {
                out.insert(name.clone());
            }
            Stmt::SyntheticBlock(body) => {
                for s in body {
                    Self::collect_declared_vars(s, out);
                }
            }
            Stmt::For { param, body, .. } => {
                if let Some(v) = param {
                    out.insert(v.clone());
                }
                for s in body {
                    Self::collect_declared_vars(s, out);
                }
            }
            _ => {}
        }
    }

    fn collect_declared_vars_in_expr(expr: &Expr, out: &mut HashSet<String>) {
        if let Expr::DoStmt(stmt) = expr {
            Self::collect_declared_vars(stmt, out);
        }
    }

    /// Find an undeclared Var reference in a statement.
    /// Returns the variable name (without sigil) if undeclared, None otherwise.
    fn find_undeclared_var_in_stmt(
        &self,
        stmt: &Stmt,
        declared: &HashSet<String>,
    ) -> Option<String> {
        match stmt {
            Stmt::Say(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) | Stmt::Put(exprs) => {
                for expr in exprs {
                    if let Expr::BareWord(name) = expr
                        && !self.has_function(name)
                        && !self.has_class(name)
                    {
                        return Some(name.clone());
                    }
                    if let Some(name) = self.find_undeclared_var_in_expr(expr, declared) {
                        return Some(name);
                    }
                }
                None
            }
            Stmt::Expr(expr) => self.find_undeclared_var_in_expr(expr, declared),
            _ => None,
        }
    }

    /// Find an undeclared Var reference in an expression.
    fn find_undeclared_var_in_expr(
        &self,
        expr: &Expr,
        declared: &HashSet<String>,
    ) -> Option<String> {
        match expr {
            Expr::Var(name) => {
                // Skip special variables, variables with twigils, and capture vars ($0, $1, ...)
                if name == "_"
                    || name == "/"
                    || name == "!"
                    || name.starts_with('*')
                    || name.starts_with('?')
                    || name.starts_with('^')
                    || name.starts_with('~')
                    || name.starts_with('.')
                    || name.starts_with('!')
                    || name.contains("::")
                    || name.starts_with("CALLER")
                    || name.starts_with("DYNAMIC")
                    || name == "__ANON_STATE__"
                    || name.chars().next().is_some_and(|c| c.is_ascii_digit())
                {
                    return None;
                }
                // Check if declared locally
                if declared.contains(name) {
                    return None;
                }
                // Check if in the outer environment (with $ prefix)
                let sigiled = format!("${}", name);
                if self.env.contains_key(&sigiled) || self.env.contains_key(name) {
                    return None;
                }
                // Check if it's a known function or class name
                if self.has_function(name) || self.has_class(name) {
                    return None;
                }
                Some(name.clone())
            }
            _ => None,
        }
    }

    /// Check for undeclared type names in EVAL'd code.
    /// Walks the AST looking for BareWord expressions that start with uppercase
    /// and aren't known types, classes, or packages. This mirrors Raku's
    /// compile-time check for undeclared symbols.
    pub(crate) fn check_eval_undeclared_names(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        // Collect class/role names declared within this EVAL code
        let mut local_classes: HashSet<String> = HashSet::new();
        for stmt in stmts {
            if let Stmt::ClassDecl { name, .. } = stmt {
                local_classes.insert(name.resolve());
            }
            if let Stmt::RoleDecl { name, .. } = stmt {
                local_classes.insert(name.resolve());
            }
        }
        for stmt in stmts {
            if let Some(name) = self.find_undeclared_name_in_stmt(stmt, &local_classes) {
                return Err(RuntimeError::undeclared_symbols(format!(
                    "Undeclared name:\n    {} used at line 1",
                    name
                )));
            }
        }
        Ok(())
    }

    /// Find an undeclared type name (BareWord starting with uppercase) in a statement.
    fn find_undeclared_name_in_stmt(
        &self,
        stmt: &Stmt,
        local_classes: &HashSet<String>,
    ) -> Option<String> {
        match stmt {
            Stmt::Expr(expr) => self.find_undeclared_name_in_expr(expr, local_classes),
            _ => None,
        }
    }

    /// Find an undeclared type name in an expression.
    /// Only checks BareWord nodes that start with uppercase and are not known types.
    fn find_undeclared_name_in_expr(
        &self,
        expr: &Expr,
        local_classes: &HashSet<String>,
    ) -> Option<String> {
        match expr {
            Expr::BareWord(name) => {
                // Only check uppercase-starting names (type name convention)
                if !name.chars().next().is_some_and(|c| c.is_uppercase()) {
                    return None;
                }
                // Skip well-known constants and special names
                if matches!(
                    name.as_str(),
                    "NaN" | "Inf" | "Empty" | "True" | "False" | "Nil" | "Any" | "Mu"
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
                    || local_classes.contains(name.as_str())
                    || crate::vm::VM::is_builtin_type(name)
                {
                    return None;
                }
                Some(name.clone())
            }
            Expr::Binary { left, right, .. } => self
                .find_undeclared_name_in_expr(left, local_classes)
                .or_else(|| self.find_undeclared_name_in_expr(right, local_classes)),
            Expr::Unary { expr, .. } => self.find_undeclared_name_in_expr(expr, local_classes),
            _ => None,
        }
    }

    fn eval_result_is_unresolved_bareword(&self, stmts: &[Stmt], result: &Value) -> bool {
        let [Stmt::Expr(Expr::BareWord(name))] = stmts else {
            return false;
        };
        matches!(result, Value::Str(s) if s.as_str() == name)
            && !self.env().contains_key(name)
            && !self.has_class(name)
            && !self.has_function(name)
            && !self.has_multi_function(name)
            && !matches!(name.as_str(), "NaN" | "Inf" | "Empty")
    }

    /// Collect operator sub names from the current environment for EVAL pre-registration.
    /// Only collects circumfix/postcircumfix operators since they require parser support
    /// to recognize their delimiter syntax. Other operator categories (prefix, postfix,
    /// infix, term) work through runtime dispatch without parser pre-registration.
    pub(crate) fn collect_operator_sub_names(&self) -> Vec<String> {
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        // Include circumfix/postcircumfix operators defined in any loaded
        // module so EVAL parser can still recognize their delimiter syntax.
        for key in self.functions.keys() {
            let key_s = key.resolve();
            let name = if let Some(pos) = key_s.rfind("::") {
                &key_s[pos + 2..]
            } else {
                key_s.as_str()
            };
            if name.starts_with("circumfix:") || name.starts_with("postcircumfix:") {
                seen.insert(name.to_string());
            }
        }
        for key in self.env.keys() {
            if key.starts_with("circumfix:") || key.starts_with("postcircumfix:") {
                seen.insert(key.clone());
            }
        }
        // Also include operators imported via `use Module` at runtime. This
        // captures prefix/infix/postfix operators declared with `is export`
        // in loaded modules, without exposing non-exported subs.
        for name in &self.imported_operator_names {
            seen.insert(name.clone());
        }
        let mut names: Vec<String> = seen.into_iter().collect();
        names.sort();
        names
    }

    pub(crate) fn collect_operator_assoc_map(&self) -> HashMap<String, String> {
        let mut assoc = HashMap::new();
        for (key, value) in &self.operator_assoc {
            let name = if let Some(pos) = key.rfind("::") {
                &key[pos + 2..]
            } else {
                key.as_str()
            };
            if name.starts_with("infix:<") {
                assoc.insert(name.to_string(), value.clone());
            }
        }
        assoc
    }

    pub(crate) fn collect_eval_imported_function_names(&self) -> Vec<String> {
        const TEST_EXPORTS: &[&str] = &[
            "ok",
            "nok",
            "is",
            "isnt",
            "is-deeply",
            "is-approx",
            "cmp-ok",
            "like",
            "unlike",
            "isa-ok",
            "does-ok",
            "can-ok",
            "lives-ok",
            "dies-ok",
            "eval-lives-ok",
            "eval-dies-ok",
            "throws-like",
            "fails-like",
            "pass",
            "flunk",
            "skip",
            "skip-rest",
            "todo",
            "diag",
            "plan",
            "done-testing",
            "bail-out",
            "subtest",
            "use-ok",
            "force_todo",
            "force-todo",
            "tap-ok",
        ];
        TEST_EXPORTS
            .iter()
            .map(|name| (*name).to_string())
            .collect()
    }

    pub(super) fn eval_eval_string(&mut self, code: &str) -> Result<Value, RuntimeError> {
        let routine_snapshot = self.snapshot_routine_registry();
        let roles_snapshot = self.roles.clone();
        let user_declared_roles_snapshot = std::mem::take(&mut self.user_declared_roles);
        let role_candidates_snapshot = self.role_candidates.clone();
        let role_type_params_snapshot = self.role_type_params.clone();
        let role_parents_snapshot = self.role_parents.clone();
        let role_hides_snapshot = self.role_hides.clone();
        let classes_snapshot = self.classes.clone();
        let hidden_classes_snapshot = self.hidden_classes.clone();
        let hidden_defer_parents_snapshot = self.hidden_defer_parents.clone();
        let class_composed_roles_snapshot = self.class_composed_roles.clone();
        let class_role_param_bindings_snapshot = self.class_role_param_bindings.clone();
        let env_snapshot = self.env.clone();
        let saved_topic = self.env.get("_").cloned();
        let trimmed = code.trim();
        if trimmed == "<>" || trimmed == "<STDIN>" {
            return Err(RuntimeError::new(
                "X::Obsolete: The degenerate case <> and old angle forms like <STDIN> are disallowed.",
            ));
        }
        let previous_pod = self.env.get("=pod").cloned();
        let saved_in_eval = self.env.get("__mutsu_in_eval").cloned();
        self.env
            .insert("__mutsu_in_eval".to_string(), Value::Bool(true));
        self.collect_pod_blocks(trimmed);
        // Collect operator sub names so the parser recognizes them in EVAL context
        let op_names = self.collect_operator_sub_names();
        let op_assoc = self.collect_operator_assoc_map();
        let imported_names = self.collect_eval_imported_function_names();
        let bracketed_stmt_inner = unwrap_bracketed_statements(trimmed)
            .filter(|inner| looks_like_bracketed_statement_list(inner));
        // General case: parse and evaluate as Raku code
        let mut result = if let Some(inner) = bracketed_stmt_inner {
            // EVAL q[[ ... ]] can yield one wrapper [] around statement lists.
            self.parse_and_eval_with_operators(inner, &op_names, &op_assoc, &imported_names)
                .or_else(|_| {
                    self.parse_and_eval_with_operators(
                        trimmed,
                        &op_names,
                        &op_assoc,
                        &imported_names,
                    )
                })
        } else {
            self.parse_and_eval_with_operators(trimmed, &op_names, &op_assoc, &imported_names)
        };
        for warning in crate::parser::take_parse_warnings() {
            self.write_warn_to_stderr(&warning);
        }
        // Fallback: parser still rejects forms like `~< foo bar >`.
        // Rewrite to an equivalent parenthesized form and try again.
        if result.is_err()
            && let Some(rewritten) = rewrite_prefixed_angle_list(trimmed)
        {
            result = self.parse_and_eval_with_operators(
                &rewritten,
                &op_names,
                &op_assoc,
                &imported_names,
            );
        }
        // Accept parenthesized statement lists like `(6;)` in EVAL.
        if result.is_err()
            && let Some(inner) = unwrap_parenthesized_statements(trimmed)
        {
            result =
                self.parse_and_eval_with_operators(inner, &op_names, &op_assoc, &imported_names);
        }
        // EVAL q[[ ... ]] sometimes carries one outer statement-list bracket pair.
        if result.is_err()
            && bracketed_stmt_inner.is_none()
            && let Some(inner) = unwrap_bracketed_statements(trimmed)
        {
            result =
                self.parse_and_eval_with_operators(inner, &op_names, &op_assoc, &imported_names);
        }
        // EVAL should accept routine declarations in snippet context.
        // If unit-scope parsing rejects a declaration, retry inside an implicit block.
        if result.is_err()
            && trimmed.contains("sub ")
            && let Some(err) = result.as_ref().err()
            && err.message.contains("X::UnitScope::Invalid")
        {
            let wrapped = format!("{{ {}; }}", trimmed);
            let saved_wrapped_eval = self.env.get("__mutsu_eval_wrapped_decls").cloned();
            self.env
                .insert("__mutsu_eval_wrapped_decls".to_string(), Value::Bool(true));
            result =
                self.parse_and_eval_with_operators(&wrapped, &op_names, &op_assoc, &imported_names);
            if let Some(saved) = saved_wrapped_eval {
                self.env
                    .insert("__mutsu_eval_wrapped_decls".to_string(), saved);
            } else {
                self.env.remove("__mutsu_eval_wrapped_decls");
            }
        }
        if let Some(saved) = previous_pod {
            self.env.insert("=pod".to_string(), saved);
        } else {
            self.env.remove("=pod");
        }
        if let Some(saved) = saved_in_eval {
            self.env.insert("__mutsu_in_eval".to_string(), saved);
        } else {
            self.env.remove("__mutsu_in_eval");
        }
        self.restore_routine_registry_eval(routine_snapshot);
        let current_roles = self.roles.clone();
        let current_role_candidates = self.role_candidates.clone();
        let current_role_type_params = self.role_type_params.clone();
        let current_role_parents = self.role_parents.clone();
        let current_role_hides = self.role_hides.clone();
        let current_classes = self.classes.clone();
        let current_hidden_classes = self.hidden_classes.clone();
        let current_hidden_defer_parents = self.hidden_defer_parents.clone();
        let current_class_composed_roles = self.class_composed_roles.clone();
        let current_class_role_param_bindings = self.class_role_param_bindings.clone();
        let current_env = self.env.clone();
        let current_type_keys: std::collections::HashSet<String> = self
            .roles
            .keys()
            .chain(self.classes.keys())
            .cloned()
            .collect();
        let snapshot_type_keys: std::collections::HashSet<String> = roles_snapshot
            .keys()
            .chain(classes_snapshot.keys())
            .cloned()
            .collect();
        self.roles = roles_snapshot;
        self.user_declared_roles = user_declared_roles_snapshot;
        self.role_candidates = role_candidates_snapshot;
        self.role_type_params = role_type_params_snapshot;
        self.role_parents = role_parents_snapshot;
        self.role_hides = role_hides_snapshot;
        self.classes = classes_snapshot;
        self.hidden_classes = hidden_classes_snapshot;
        self.hidden_defer_parents = hidden_defer_parents_snapshot;
        self.class_composed_roles = class_composed_roles_snapshot;
        self.class_role_param_bindings = class_role_param_bindings_snapshot;
        self.roles.extend(current_roles);
        // Don't extend user_declared_roles: roles declared in EVAL are EVAL-scoped
        // and should not affect the parent's redeclaration detection.
        self.role_candidates.extend(current_role_candidates);
        self.role_type_params.extend(current_role_type_params);
        self.role_parents.extend(current_role_parents);
        self.role_hides.extend(current_role_hides);
        self.classes.extend(current_classes);
        self.hidden_classes.extend(current_hidden_classes);
        self.hidden_defer_parents
            .extend(current_hidden_defer_parents);
        self.class_composed_roles
            .extend(current_class_composed_roles);
        self.class_role_param_bindings
            .extend(current_class_role_param_bindings);
        for key in current_type_keys.union(&snapshot_type_keys) {
            if let Some(value) = current_env.get(key).cloned() {
                self.env.insert(key.clone(), value);
            } else if let Some(value) = env_snapshot.get(key).cloned() {
                self.env.insert(key.clone(), value);
            } else {
                self.env.remove(key);
            }
        }
        // Lexical code variables created inside EVAL must not leak back into the
        // caller's environment, or repeated EVALs of the same helper sub will
        // trip routine redeclaration checks on stale `&name` bindings.
        let callable_keys: std::collections::HashSet<String> = current_env
            .keys()
            .chain(env_snapshot.keys())
            .filter(|key| key.starts_with('&'))
            .cloned()
            .collect();
        for key in callable_keys {
            if let Some(value) = env_snapshot.get(&key).cloned() {
                self.env.insert(key, value);
            } else {
                self.env.remove(&key);
            }
        }
        // Restore $_ so EVAL does not clobber the caller's topic variable
        if let Some(topic) = saved_topic {
            self.env.insert("_".to_string(), topic);
        } else {
            self.env.remove("_");
        }
        result
    }

    /// Build a CallFrame Instance for the given depth.
    /// `callsite_line` is the line where `callframe()` was called (from hidden arg).
    pub(super) fn callframe_value(
        &self,
        depth: usize,
        callsite_line: Option<i64>,
    ) -> Option<Value> {
        let file = self
            .env
            .get("?FILE")
            .map(|v| v.to_string_value())
            .unwrap_or_default();

        if depth == 0 {
            // Current frame: use current env, current file/line, current code
            let line = callsite_line.unwrap_or(0);
            let code = self.current_routine_sub_value();
            let my_hash = self.build_lexical_hash(&self.env, None);
            let mut attrs = HashMap::new();
            attrs.insert("line".to_string(), Value::Int(line));
            attrs.insert("file".to_string(), Value::str(file));
            Self::insert_callframe_code_attrs(&mut attrs, &code);
            attrs.insert("code".to_string(), code);
            attrs.insert("my".to_string(), my_hash);
            attrs.insert("inline".to_string(), Value::Bool(false));
            attrs.insert("__depth".to_string(), Value::Int(0));
            attrs.insert("annotations".to_string(), self.build_annotations(&attrs));
            return Some(Value::make_instance(Symbol::intern("CallFrame"), attrs));
        }

        // depth >= 1: walk up the caller env stack
        let stack_len = self.callframe_stack.len();
        if depth > stack_len {
            return None;
        }
        let entry = &self.callframe_stack[stack_len - depth];
        let code = entry.code.clone().unwrap_or(Value::Nil);
        let my_hash = self.build_lexical_hash(&entry.env, Some(depth));
        let mut attrs = HashMap::new();
        attrs.insert("line".to_string(), Value::Int(entry.line));
        attrs.insert("file".to_string(), Value::str(entry.file.clone()));
        Self::insert_callframe_code_attrs(&mut attrs, &code);
        attrs.insert("code".to_string(), code);
        attrs.insert("my".to_string(), my_hash);
        attrs.insert("inline".to_string(), Value::Bool(false));
        attrs.insert("__depth".to_string(), Value::Int(depth as i64));
        attrs.insert("annotations".to_string(), self.build_annotations(&attrs));
        Some(Value::make_instance(Symbol::intern("CallFrame"), attrs))
    }

    /// Extract subname, package, subtype, and sub attributes from a code value
    /// and insert them into the CallFrame attributes map.
    fn insert_callframe_code_attrs(attrs: &mut HashMap<String, Value>, code: &Value) {
        match code {
            Value::Sub(sd) => {
                let name = sd.name.resolve();
                attrs.insert("subname".to_string(), Value::str(name.to_string()));
                let pkg = sd.package.resolve();
                attrs.insert("package".to_string(), Value::str(pkg.to_string()));
                attrs.insert("subtype".to_string(), Value::str("SubRoutine".to_string()));
                attrs.insert("sub".to_string(), code.clone());
            }
            _ => {
                attrs.insert("subname".to_string(), Value::str(String::new()));
                attrs.insert("package".to_string(), Value::str(String::new()));
                attrs.insert("subtype".to_string(), Value::str(String::new()));
                attrs.insert("sub".to_string(), Value::Nil);
            }
        }
    }

    fn current_routine_sub_value(&self) -> Value {
        // Try to find the current routine as a Sub value from the block stack
        for v in self.block_stack.iter().rev() {
            if matches!(v, Value::Sub(_)) {
                return v.clone();
            }
        }
        // Fallback: look at the most recent callframe stack entry for the code
        if let Some(entry) = self.callframe_stack.last()
            && let Some(ref code) = entry.code
        {
            return code.clone();
        }
        Value::Nil
    }

    fn build_lexical_hash(
        &self,
        env: &HashMap<String, Value>,
        callframe_depth: Option<usize>,
    ) -> Value {
        let mut hash = HashMap::new();
        for (k, v) in env {
            // Skip internal keys and special variables
            if k.starts_with("__") || k.starts_with('?') || k.starts_with('*') || k.starts_with('=')
            {
                continue;
            }
            // Skip type names, enum values, and signal names
            if k.chars().next().is_some_and(|c| c.is_uppercase()) {
                continue;
            }
            // Add sigil prefix based on the env key convention:
            // - Keys starting with '@' or '%' already have sigils (array/hash vars)
            // - Other keys are scalar variables that need '$' prefix
            let sigiled = if k.starts_with('@') || k.starts_with('%') || k.starts_with('&') {
                k.clone()
            } else {
                format!("${}", k)
            };
            hash.insert(sigiled, v.clone());
        }
        // Store callframe depth so assignments can write back to the caller env
        if let Some(depth) = callframe_depth {
            hash.insert("__callframe_depth".to_string(), Value::Int(depth as i64));
        }
        Value::hash(hash)
    }

    fn build_annotations(&self, attrs: &HashMap<String, Value>) -> Value {
        let mut map = HashMap::new();
        if let Some(file) = attrs.get("file") {
            map.insert("file".to_string(), file.clone());
        }
        if let Some(line) = attrs.get("line") {
            map.insert("line".to_string(), line.clone());
        }
        Value::hash(map)
    }

    pub(super) fn seconds_from_value(val: Option<Value>) -> Option<f64> {
        val.and_then(|v| super::to_float_value(&v))
    }

    pub(super) fn duration_from_seconds(secs: Option<f64>) -> Duration {
        let secs = secs.unwrap_or(0.0).max(0.0);
        Duration::from_secs_f64(secs)
    }

    pub(super) fn hostname() -> String {
        env::var("HOSTNAME")
            .ok()
            .or_else(|| env::var("COMPUTERNAME").ok())
            .or_else(|| {
                Command::new("hostname")
                    .output()
                    .ok()
                    .and_then(|output| String::from_utf8(output.stdout).ok())
                    .map(|s| s.trim().to_string())
            })
            .unwrap_or_else(|| "localhost".to_string())
    }

    pub(super) fn resolve_host(name: &str) -> Vec<String> {
        format!("{}:0", name)
            .to_socket_addrs()
            .map(|addrs| {
                addrs
                    .map(|addr| addr.ip().to_string())
                    .filter(|ip| !ip.is_empty())
                    .collect()
            })
            .unwrap_or_default()
    }

    pub(super) fn make_os_name_value(name: String, mut addresses: Vec<String>) -> Value {
        if addresses.is_empty() {
            addresses.push("127.0.0.1".to_string());
        }
        let mut info = HashMap::new();
        info.insert("name".to_string(), Value::str(name.clone()));
        info.insert("addr".to_string(), Value::str(addresses[0].clone()));
        info.insert("aliases".to_string(), Value::array(Vec::new()));
        let addrs_values = addresses.into_iter().map(Value::str).collect();
        info.insert("addrs".to_string(), Value::array(addrs_values));
        Value::hash(info)
    }

    pub(super) fn get_login_name() -> Option<String> {
        env::var("LOGNAME")
            .ok()
            .or_else(|| env::var("USER").ok())
            .or_else(|| env::var("USERNAME").ok())
    }

    pub(super) fn send_signal(pid: i64, signal: i64) -> bool {
        if pid == 0 {
            return false;
        }
        let pid_str = pid.to_string();
        #[cfg(unix)]
        {
            let mut cmd = Command::new("kill");
            cmd.arg(format!("-{}", signal));
            cmd.arg(&pid_str);
            cmd.status().map(|status| status.success()).unwrap_or(false)
        }
        #[cfg(windows)]
        {
            let mut cmd = Command::new("taskkill");
            cmd.args(["/PID", &pid_str, "/F"]);
            cmd.status().map(|status| status.success()).unwrap_or(false)
        }
        #[cfg(not(any(unix, windows)))]
        {
            false
        }
    }
}

fn rewrite_prefixed_angle_list(code: &str) -> Option<String> {
    let (prefix, rest) = if let Some(rest) = code.strip_prefix('~') {
        ('~', rest)
    } else if let Some(rest) = code.strip_prefix('+') {
        ('+', rest)
    } else if let Some(rest) = code.strip_prefix('?') {
        ('?', rest)
    } else {
        return None;
    };
    let inner = rest.trim_start();
    if !inner.starts_with('<') || !inner.ends_with('>') {
        return None;
    }
    Some(format!("{}({})", prefix, inner))
}

fn unwrap_parenthesized_statements(code: &str) -> Option<&str> {
    if !code.starts_with('(') || !code.ends_with(')') {
        return None;
    }
    let mut depth = 0usize;
    for (i, ch) in code.char_indices() {
        if ch == '(' {
            depth += 1;
        } else if ch == ')' {
            if depth == 0 {
                return None;
            }
            depth -= 1;
            if depth == 0 && i + ch.len_utf8() != code.len() {
                return None;
            }
        }
    }
    if depth != 0 {
        return None;
    }
    let inner = &code[1..code.len() - 1];
    // Restrict this fallback to statement-list snippets like `(6;)`.
    // Plain parenthesized expressions should keep normal parse behavior.
    if !inner.contains(';') {
        return None;
    }
    Some(inner)
}

fn unwrap_bracketed_statements(code: &str) -> Option<&str> {
    if !code.starts_with('[') || !code.ends_with(']') {
        return None;
    }
    let mut depth = 0usize;
    for (i, ch) in code.char_indices() {
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            if depth == 0 {
                return None;
            }
            depth -= 1;
            if depth == 0 && i + ch.len_utf8() != code.len() {
                return None;
            }
        }
    }
    if depth != 0 {
        return None;
    }
    Some(&code[1..code.len() - 1])
}

fn looks_like_bracketed_statement_list(inner: &str) -> bool {
    let trimmed = inner.trim_start();
    if !inner.contains(';') {
        return false;
    }
    matches!(
        trimmed.split_whitespace().next(),
        Some(
            "my" | "our"
                | "state"
                | "sub"
                | "multi"
                | "proto"
                | "class"
                | "role"
                | "grammar"
                | "module"
                | "unit"
                | "use"
                | "need"
        )
    )
}
