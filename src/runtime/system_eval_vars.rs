use super::*;

impl Interpreter {
    /// Check for undeclared variable references in EVAL'd code.
    /// Collects declared variable names from VarDecl/Assign nodes and checks
    /// Var references against them and the outer environment.
    /// Placeholder variables ($^x, @_, ...) used directly in the mainline are
    /// X::Placeholder::Mainline. This must take precedence over the undeclared
    /// check (otherwise `@_` would be reported as X::Undeclared).
    pub(crate) fn check_eval_mainline_placeholders(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        if let Some(ph) = crate::ast::collect_unattached_placeholders(stmts)
            .into_iter()
            .next()
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("placeholder".to_string(), Value::str(ph.clone()));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Cannot use placeholder parameter {} outside of a sub or block",
                    ph
                )),
            );
            return Err(RuntimeError::typed("X::Placeholder::Mainline", attrs));
        }
        Ok(())
    }

    pub(crate) fn check_eval_undeclared_vars(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let mut declared: HashSet<String> = HashSet::new();
        for stmt in stmts {
            Self::check_self_referential_init(stmt)?;
            Self::collect_declared_vars(stmt, &mut declared);
            if let Some((sigil, var_name, suggestions)) =
                self.find_undeclared_var_in_stmt(stmt, &declared)
            {
                let symbol = format!("{}{}", sigil, var_name);
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("name".to_string(), Value::str(symbol.clone()));
                attrs.insert("symbol".to_string(), Value::str(symbol.clone()));
                // `post` is the source text following the eject point. For a bare
                // undeclared variable reference, that is the symbol itself.
                attrs.insert("post".to_string(), Value::str(symbol.clone()));
                // `highexpect` is the list of additional things the parser was
                // still expecting at the eject point. For an undeclared variable
                // there is nothing else expected, so it is an empty list.
                attrs.insert("highexpect".to_string(), Value::array(vec![]));
                let mut message = format!("Variable '{}' is not declared.", symbol);
                if suggestions.len() == 1 {
                    message.push_str(&format!(" Did you mean '{}'?", suggestions[0]));
                } else if suggestions.len() > 1 {
                    let quoted: Vec<String> =
                        suggestions.iter().map(|s| format!("'{}'", s)).collect();
                    message.push_str(&format!(
                        " Did you mean any of these: {}?",
                        quoted.join(", ")
                    ));
                }
                attrs.insert(
                    "suggestions".to_string(),
                    Value::array(suggestions.into_iter().map(Value::str).collect()),
                );
                attrs.insert("message".to_string(), Value::str(message));
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

    pub(super) fn collect_declared_vars(stmt: &Stmt, out: &mut HashSet<String>) {
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
    /// Returns (sigil, name, suggestions) if undeclared, None otherwise.
    fn find_undeclared_var_in_stmt(
        &self,
        stmt: &Stmt,
        declared: &HashSet<String>,
    ) -> Option<(&'static str, String, Vec<String>)> {
        match stmt {
            Stmt::Say(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) | Stmt::Put(exprs) => {
                for expr in exprs {
                    if let Expr::BareWord(name) = expr
                        && !self.has_function(name)
                        && !self.has_class(name)
                    {
                        let suggestions =
                            Self::suggest_declared_vars(&format!("${}", name), declared);
                        return Some(("$", name.clone(), suggestions));
                    }
                    if let Some(result) = self.find_undeclared_var_in_expr(expr, declared) {
                        return Some(result);
                    }
                }
                None
            }
            Stmt::Expr(expr) => self.find_undeclared_var_in_expr(expr, declared),
            // Descend into `sub`/`method` bodies so undeclared variables used
            // inside them (e.g. `sub greet($name) { say "$nam" }`) are caught at
            // EVAL/compile time. The body's lexical scope adds the routine's
            // parameters (and, for methods inside a class, the attributes).
            Stmt::SubDecl {
                params,
                param_defs,
                body,
                ..
            } => {
                let mut inner = declared.clone();
                Self::add_routine_locals(params, body, &mut inner);
                Self::add_sub_signature_locals(param_defs, &mut inner);
                self.find_undeclared_var_in_body(body, &inner)
            }
            Stmt::MethodDecl {
                params,
                param_defs,
                body,
                ..
            } => {
                let mut inner = declared.clone();
                Self::add_routine_locals(params, body, &mut inner);
                Self::add_sub_signature_locals(param_defs, &mut inner);
                self.find_undeclared_var_in_body(body, &inner)
            }
            Stmt::ClassDecl { body, .. } | Stmt::RoleDecl { body, .. } => {
                // Attributes become `$!x` / `$.x` (and a bare `$x` only for
                // `has $x` aliases). Methods in the body see them in scope.
                let mut inner = declared.clone();
                Self::add_class_attributes(body, &mut inner);
                // Class-body lexicals (`my @a`) are also in scope for sibling
                // statements/methods; collect them all up front.
                for s in body {
                    Self::collect_declared_vars(s, &mut inner);
                }
                for s in body {
                    if let Some(result) = self.find_undeclared_var_in_stmt(s, &inner) {
                        return Some(result);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Seed a lexical scope with a routine's parameters and any variables its
    /// body declares, so they are not flagged as undeclared.
    fn add_routine_locals(params: &[String], body: &[Stmt], out: &mut HashSet<String>) {
        for p in params {
            out.insert(p.trim_start_matches(['$', '@', '%', '&']).to_string());
        }
        for s in body {
            Self::collect_declared_vars(s, out);
        }
    }

    /// Seed a scope with the names introduced by parameter sub-signatures
    /// (destructuring / renaming), e.g. `@a ($first, @rest)` declares `$first`
    /// and `@rest`. The simplified `params: Vec<String>` only records the outer
    /// parameter name, so these inner names would otherwise be flagged as
    /// undeclared when used in the body.
    fn add_sub_signature_locals(param_defs: &[crate::ast::ParamDef], out: &mut HashSet<String>) {
        for pd in param_defs {
            Self::collect_param_sub_signature(&pd.sub_signature, out);
        }
    }

    fn collect_param_sub_signature(
        sub_sig: &Option<Vec<crate::ast::ParamDef>>,
        out: &mut HashSet<String>,
    ) {
        if let Some(params) = sub_sig {
            for sp in params {
                if !sp.name.is_empty() {
                    out.insert(sp.name.trim_start_matches(['$', '@', '%', '&']).to_string());
                }
                Self::collect_param_sub_signature(&sp.sub_signature, out);
            }
        }
    }

    /// Seed a scope with a class/role body's attributes. `has $.name` makes
    /// `$!name` and `$.name` available (but not a bare `$name`); `has $x`
    /// (alias form) additionally makes `$x` available.
    fn add_class_attributes(body: &[Stmt], out: &mut HashSet<String>) {
        for s in body {
            if let Stmt::HasDecl { name, is_alias, .. } = s {
                let attr = name.resolve();
                // Rakudo's "did you mean" for a bare `$name` that refers to an
                // attribute suggests `$!name` (the private accessor form).
                out.insert(format!("!{}", attr));
                if *is_alias {
                    out.insert(attr);
                }
            }
        }
    }

    /// Scan a routine body for the first undeclared variable reference.
    fn find_undeclared_var_in_body(
        &self,
        body: &[Stmt],
        declared: &HashSet<String>,
    ) -> Option<(&'static str, String, Vec<String>)> {
        for s in body {
            if let Some(result) = self.find_undeclared_var_in_stmt(s, declared) {
                return Some(result);
            }
        }
        None
    }

    /// Find an undeclared Var reference in an expression.
    /// Returns (sigil, name, suggestions) if undeclared, None otherwise.
    fn find_undeclared_var_in_expr(
        &self,
        expr: &Expr,
        declared: &HashSet<String>,
    ) -> Option<(&'static str, String, Vec<String>)> {
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
                    || name.starts_with("__ANON_")
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
                if self.env.contains_key(&sigiled) {
                    return None;
                }
                // A bare-name environment entry only declares the scalar `$name`
                // if it is an actual variable. A class/sub named `Foo` is stored
                // bare (as a type object), but does NOT declare the scalar `$Foo`
                // -- in Raku `$Foo` is its own (undeclared) symbol.
                if self.env.contains_key(name)
                    && !self.registry().classes.contains_key(name)
                    && !self.has_function(name)
                {
                    return None;
                }
                let suggestions = Self::suggest_declared_vars(&sigiled, declared);
                Some(("$", name.clone(), suggestions))
            }
            Expr::ArrayVar(name) => {
                if name.starts_with("__ANON_") {
                    return None;
                }
                let sigiled = format!("@{}", name);
                if declared.contains(name) || declared.contains(&sigiled) {
                    return None;
                }
                if self.env.contains_key(&sigiled) || self.env.contains_key(name) {
                    return None;
                }
                let suggestions = Self::suggest_declared_vars(&sigiled, declared);
                Some(("@", name.clone(), suggestions))
            }
            Expr::HashVar(name) => {
                if name.starts_with("__ANON_") {
                    return None;
                }
                let sigiled = format!("%{}", name);
                if declared.contains(name) || declared.contains(&sigiled) {
                    return None;
                }
                if self.env.contains_key(&sigiled) || self.env.contains_key(name) {
                    return None;
                }
                let suggestions = Self::suggest_declared_vars(&sigiled, declared);
                Some(("%", name.clone(), suggestions))
            }
            // Recurse into string interpolation, indexing, and method calls
            Expr::StringInterpolation(parts) => {
                for part in parts {
                    if let Some(result) = self.find_undeclared_var_in_expr(part, declared) {
                        return Some(result);
                    }
                }
                None
            }
            Expr::Index { target, index, .. } => self
                .find_undeclared_var_in_expr(target, declared)
                .or_else(|| self.find_undeclared_var_in_expr(index, declared)),
            Expr::MethodCall { target, args, .. } => {
                if let Some(result) = self.find_undeclared_var_in_expr(target, declared) {
                    return Some(result);
                }
                for arg in args {
                    if let Some(result) = self.find_undeclared_var_in_expr(arg, declared) {
                        return Some(result);
                    }
                }
                None
            }
            _ => None,
        }
    }
}
