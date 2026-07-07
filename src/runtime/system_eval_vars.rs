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
            || !matches!(expr, Expr::Literal(v) if v.is_nil());
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
            // A `loop (my $x = 1; ...; ...)` initializer declares `$x` in the
            // enclosing scope (visible after the loop). Likewise `if/while my $x
            // = ...` bring the condition's declaration into scope.
            Stmt::Loop { init, cond, .. } => {
                if let Some(init) = init {
                    Self::collect_declared_vars(init, out);
                }
                if let Some(cond) = cond {
                    Self::collect_declared_vars_in_expr(cond, out);
                }
            }
            Stmt::While { cond, .. } => {
                Self::collect_declared_vars_in_expr(cond, out);
            }
            Stmt::If { cond, .. } => {
                Self::collect_declared_vars_in_expr(cond, out);
            }
            _ => {}
        }
    }

    fn collect_declared_vars_in_expr(expr: &Expr, out: &mut HashSet<String>) {
        match expr {
            Expr::DoStmt(stmt) => Self::collect_declared_vars(stmt, out),
            // Recurse through operators/grouping so a declaration embedded in a
            // larger expression (`my $x = 1, my $y = 2`, `not my $x = 1`) is found.
            Expr::Binary { left, right, .. } | Expr::MetaOp { left, right, .. } => {
                Self::collect_declared_vars_in_expr(left, out);
                Self::collect_declared_vars_in_expr(right, out);
            }
            Expr::Unary { expr, .. } | Expr::Grouped(expr) | Expr::Itemize(expr) => {
                Self::collect_declared_vars_in_expr(expr, out);
            }
            _ => {}
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
            // `$ret = <rhs>` — the RHS may reference a variable before its own
            // embedded `my` declaration (`$ret = $foo ~ my $foo` is X::Undeclared,
            // but `$ret = (my $foo) ~ $foo` is fine). Walk the RHS left-to-right so
            // an embedded `my $x` only brings `x` into scope for references that
            // follow it.
            Stmt::Assign { name, expr, .. } => {
                let mut local = declared.clone();
                local.insert(name.clone());
                self.find_undeclared_var_ordered(expr, &mut local)
            }
            // A bare block `{ ... }` is a nested lexical scope. Inside it, an
            // assignment to a variable that was never declared (`{ $var = 42 }`)
            // is X::Undeclared — assignment does not declare in strict mode, and a
            // block can only assign to a name that is already in scope (an outer
            // lexical, in `declared`/env) or block-local (`my`/`state`). This
            // strict assign-target check is confined to blocks; the top-level
            // ordered scan keeps its lenient assign-as-declaration behavior.
            Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                self.check_block_scope_undeclared(body, declared)
            }
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
                if let Some(r) = Self::find_undeclared_var_in_param_wheres(param_defs) {
                    return Some(r);
                }
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
                if let Some(r) = Self::find_undeclared_var_in_param_wheres(param_defs) {
                    return Some(r);
                }
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

    /// Check a bare-block body as a nested lexical scope. Variables declared with
    /// `my`/`state`/etc. inside the block extend `declared` for later statements;
    /// an assignment whose target is not in scope (nor in the outer env) is
    /// X::Undeclared. Assignment itself never declares (strict mode), so the
    /// target is not added to the scope after the check.
    fn check_block_scope_undeclared(
        &self,
        body: &[Stmt],
        declared: &HashSet<String>,
    ) -> Option<(&'static str, String, Vec<String>)> {
        let mut inner = declared.clone();
        for s in body {
            if let Stmt::Assign { name, expr, .. } = s {
                // Check the assignment *target* against the current scope + env.
                let lhs = Self::assign_lhs_expr(name);
                if let Some(r) = self.find_undeclared_var_in_expr(&lhs, &inner) {
                    return Some(r);
                }
                // Then check the RHS, treating the target as in scope for a
                // self-referential `$x = $x + 1`.
                let mut local = inner.clone();
                local.insert(name.clone());
                if let Some(r) = self.find_undeclared_var_ordered(expr, &mut local) {
                    return Some(r);
                }
                // Assignment does not declare: do NOT add the target to `inner`.
            } else {
                if let Some(r) = self.find_undeclared_var_in_stmt(s, &inner) {
                    return Some(r);
                }
                // Bring the statement's real (`my`/`state`/loop) declarations into
                // scope for subsequent statements. `collect_declared_vars` may also
                // pull in nested assign targets, which only makes the check *less*
                // strict (a safe false negative).
                Self::collect_declared_vars(s, &mut inner);
            }
        }
        None
    }

    /// Build the reference-form expression for an assignment target name
    /// (`"var"` → `$var`, `"@a"` → `@a`, `"%h"` → `%h`), so the shared
    /// `find_undeclared_var_in_expr` skip/env logic applies to the LHS.
    fn assign_lhs_expr(name: &str) -> Expr {
        if let Some(base) = name.strip_prefix('@') {
            Expr::ArrayVar(base.to_string())
        } else if let Some(base) = name.strip_prefix('%') {
            Expr::HashVar(base.to_string())
        } else {
            Expr::Var(name.trim_start_matches('$').to_string())
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
    /// A parameter's `where` constraint is checked left-to-right: it may name the
    /// parameter it constrains and any *earlier* parameter, but referencing a
    /// *later* parameter is `X::Undeclared` (that param is not in scope yet). Walk
    /// the signature accumulating bound names, and for each `where` flag a
    /// reference to a name that only becomes a parameter later. Matches
    /// `sub foo($x where { $x == $y }, $y) {}` → X::Undeclared. (subtypes.t 77)
    fn find_undeclared_var_in_param_wheres(
        param_defs: &[crate::ast::ParamDef],
    ) -> Option<(&'static str, String, Vec<String>)> {
        let bare = |n: &str| n.trim_start_matches(['$', '@', '%', '&']).to_string();
        // Names of params that appear *after* index i, for each i.
        let all_names: Vec<String> = param_defs
            .iter()
            .filter(|pd| !pd.name.is_empty())
            .map(|pd| bare(&pd.name))
            .collect();
        let mut seen_upto = HashSet::new();
        for (i, pd) in param_defs
            .iter()
            .filter(|pd| !pd.name.is_empty())
            .enumerate()
        {
            let self_name = bare(&pd.name);
            // The param may reference itself and earlier params in its own `where`.
            seen_upto.insert(self_name.clone());
            if let Some(where_expr) = &pd.where_constraint {
                let mut refs = HashSet::new();
                Self::collect_where_var_refs(where_expr, &mut refs);
                // A reference to a param declared strictly later is undeclared.
                for r in &refs {
                    let is_later_param = all_names.iter().skip(i + 1).any(|later| later == r)
                        && !seen_upto.contains(r);
                    if is_later_param {
                        return Some(("$", r.clone(), Vec::new()));
                    }
                }
            }
        }
        None
    }

    /// Collect the bare names of all `$`/`@`/`%`/`&` variables referenced anywhere
    /// in `expr`, descending through the expression / block forms a `where`
    /// constraint can take. Conservative: unhandled forms are simply not walked
    /// (this only ever *misses* references, never invents them).
    fn collect_where_var_refs(expr: &Expr, out: &mut HashSet<String>) {
        use Expr::*;
        match expr {
            Var(n) | ArrayVar(n) | HashVar(n) | CodeVar(n) => {
                out.insert(n.clone());
            }
            Grouped(e) | ZenSlice(e) | PositionalPair(e) | Eager(e) | Itemize(e) => {
                Self::collect_where_var_refs(e, out)
            }
            Unary { expr, .. } | PostfixOp { expr, .. } => Self::collect_where_var_refs(expr, out),
            Binary { left, right, .. } => {
                Self::collect_where_var_refs(left, out);
                Self::collect_where_var_refs(right, out);
            }
            Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::collect_where_var_refs(cond, out);
                Self::collect_where_var_refs(then_expr, out);
                Self::collect_where_var_refs(else_expr, out);
            }
            Index { target, index, .. } => {
                Self::collect_where_var_refs(target, out);
                Self::collect_where_var_refs(index, out);
            }
            MethodCall { target, args, .. } | CallOn { target, args } => {
                Self::collect_where_var_refs(target, out);
                args.iter()
                    .for_each(|a| Self::collect_where_var_refs(a, out));
            }
            Call { args, .. } => args
                .iter()
                .for_each(|a| Self::collect_where_var_refs(a, out)),
            StringInterpolation(parts) | ArrayLiteral(parts) | CaptureLiteral(parts) => parts
                .iter()
                .for_each(|p| Self::collect_where_var_refs(p, out)),
            Block(stmts) | AnonSub { body: stmts, .. } | Lambda { body: stmts, .. } => {
                Self::collect_where_var_refs_in_stmts(stmts, out)
            }
            _ => {}
        }
    }

    fn collect_where_var_refs_in_stmts(stmts: &[Stmt], out: &mut HashSet<String>) {
        for s in stmts {
            match s {
                Stmt::Expr(e) => Self::collect_where_var_refs(e, out),
                Stmt::Assign { expr, .. } | Stmt::VarDecl { expr, .. } => {
                    Self::collect_where_var_refs(expr, out)
                }
                Stmt::Say(es) | Stmt::Print(es) | Stmt::Put(es) | Stmt::Note(es) => {
                    es.iter().for_each(|e| Self::collect_where_var_refs(e, out))
                }
                _ => {}
            }
        }
    }

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

    /// Walk an expression left-to-right looking for an undeclared variable,
    /// threading the in-scope `declared` set so an embedded `my $x` declaration
    /// only shadows references that appear *after* it in source order. This is
    /// what distinguishes `$foo ~ my $foo` (X::Undeclared — `$foo` used before
    /// its declaration) from `(my $foo) ~ $foo` (legal).
    fn find_undeclared_var_ordered(
        &self,
        expr: &Expr,
        declared: &mut HashSet<String>,
    ) -> Option<(&'static str, String, Vec<String>)> {
        match expr {
            // An embedded declaration: check its initializer against the current
            // scope, then bring the new name in for subsequent references.
            Expr::DoStmt(stmt) => {
                if let Stmt::VarDecl {
                    name, expr: init, ..
                } = stmt.as_ref()
                {
                    if let Some(r) = self.find_undeclared_var_ordered(init, declared) {
                        return Some(r);
                    }
                    declared.insert(name.clone());
                    declared.insert(name.trim_start_matches(['$', '@', '%', '&']).to_string());
                    None
                } else {
                    None
                }
            }
            Expr::Binary { left, right, .. } | Expr::MetaOp { left, right, .. } => self
                .find_undeclared_var_ordered(left, declared)
                .or_else(|| self.find_undeclared_var_ordered(right, declared)),
            Expr::Grouped(inner)
            | Expr::Unary { expr: inner, .. }
            | Expr::PostfixOp { expr: inner, .. }
            | Expr::Itemize(inner) => self.find_undeclared_var_ordered(inner, declared),
            // Leaves (and anything not containing an embedded declaration): defer
            // to the plain checker against the accumulated scope.
            _ => self.find_undeclared_var_in_expr(expr, &*declared),
        }
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
