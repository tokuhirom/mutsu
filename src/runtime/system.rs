use super::*;
use crate::symbol::Symbol;

/// Core built-in type names used as candidates for type-name "Did you mean"
/// suggestions (in addition to user-registered classes).
pub(crate) const CORE_TYPE_NAMES: &[&str] = &[
    "Mu",
    "Any",
    "Cool",
    "Int",
    "Num",
    "Rat",
    "FatRat",
    "Complex",
    "Str",
    "Bool",
    "Array",
    "Hash",
    "List",
    "Map",
    "Set",
    "Bag",
    "Mix",
    "SetHash",
    "BagHash",
    "MixHash",
    "Range",
    "Pair",
    "Seq",
    "Slip",
    "Junction",
    "Regex",
    "Match",
    "Grammar",
    "Exception",
    "Failure",
    "Version",
    "Nil",
    "Block",
    "Code",
    "Routine",
    "Sub",
    "Method",
    "Whatever",
    "WhateverCode",
    "Callable",
    "Numeric",
    "Real",
    "Stringy",
    "Positional",
    "Associative",
    "Iterable",
    "Iterator",
    "Capture",
    "Signature",
    "Parameter",
    "Date",
    "DateTime",
    "Instant",
    "Duration",
    "Buf",
    "Blob",
    "Promise",
    "Supply",
    "Channel",
    "Thread",
    "Proc",
    "IO",
    "Scalar",
];

impl Interpreter {
    fn parse_and_eval_with_operators(
        &mut self,
        src: &str,
        op_names: &[String],
        op_assoc: &HashMap<String, String>,
        imported_names: &[String],
    ) -> Result<Value, RuntimeError> {
        let user_sub_names = self.collect_eval_user_sub_names();
        // Make module search paths visible to the parser so that `use Foo`
        // inside the EVAL'd code can resolve and register Foo's exported sub
        // names (needed for parenless calls like `use Foo; bar`).
        crate::parser::set_parser_lib_paths(self.lib_paths.clone());
        crate::parser::set_parser_program_path(self.program_path.clone());
        let parse_result = crate::parser::parse_program_with_operators_and_user_subs(
            src,
            op_names,
            op_assoc,
            imported_names,
            &user_sub_names,
        );
        crate::parser::clear_parser_lib_paths();
        match parse_result {
            Ok((stmts, _)) => {
                self.check_eval_class_redeclarations(&stmts)?;
                self.check_eval_undeclared_trusts(&stmts)?;
                self.check_eval_undeclared_type_args(&stmts)?;
                self.check_eval_undeclared_vars(&stmts)?;
                self.check_eval_undeclared_names(&stmts)?;
                self.check_eval_param_type_constraints(&stmts)?;
                // When EVAL is called inside a class body, MethodDecl statements
                // should be added to the enclosing class rather than lowered to subs.
                let mut stmts = self.inject_eval_methods_into_class(stmts);
                // Reorder phasers so BEGIN/CHECK run at compile time and INIT
                // runs once before the main body, matching the top-level pipeline.
                // Use the EVAL-specific variant that also lifts BEGIN from
                // closure bodies in the EVAL'd code.
                crate::runtime::phasers::reorder_phasers_for_eval(&mut stmts);
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
    pub(super) fn inject_eval_methods_into_class(&mut self, stmts: Vec<Stmt>) -> Vec<Stmt> {
        // Only applies when current_package is a class being defined
        let class_name = self.current_package();
        if !self.registry().classes.contains_key(&class_name) {
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
                    is_submethod: false,
                };
                if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
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
    pub(super) fn execute_begin_phasers(&mut self, stmts: &[Stmt]) {
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
        // Type-like declarations (class/role/subset/enum) share one symbol
        // namespace; a non-stub declaration cannot be redeclared by another
        // type declaration of the same name (regardless of kind).
        // `bool` value records whether the existing declaration is a forward stub.
        let mut seen_types: HashMap<String, bool> = HashMap::new();
        // Routine declarations (`sub`) share a separate namespace; a routine may
        // only be redeclared if every declaration of that name is `multi`.
        let mut seen_routines: HashMap<String, bool> = HashMap::new();
        // Nested type names declared directly inside each top-level class body,
        // so that `augment class C { class Nested {} }` can detect redeclaring a
        // nested type that the original `class C { class Nested {} }` declared.
        let mut class_nested: HashMap<String, HashSet<String>> = HashMap::new();
        let collect_nested = |body: &[Stmt]| -> HashSet<String> {
            body.iter()
                .filter_map(|s| match s {
                    Stmt::ClassDecl { name, .. }
                    | Stmt::RoleDecl { name, .. }
                    | Stmt::SubsetDecl { name, .. }
                    | Stmt::EnumDecl { name, .. } => Some(name.resolve().to_string()),
                    _ => None,
                })
                .collect()
        };
        // A forward stub body is a single `... { !!! }` / `{ ??? }` placeholder,
        // compiled to a lone `__mutsu_stub_die` / `__mutsu_stub_warn` call.
        let body_is_stub = |body: &[Stmt]| -> bool {
            let body_no_sl: Vec<_> = body
                .iter()
                .filter(|s| !matches!(s, Stmt::SetLine(_)))
                .collect();
            body_no_sl.len() == 1
                && matches!(body_no_sl[0], Stmt::Expr(Expr::Call { name: fn_name, .. })
                    if *fn_name == "__mutsu_stub_die" || *fn_name == "__mutsu_stub_warn")
        };

        let type_redeclaration = |name: &str| -> RuntimeError {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("symbol".to_string(), Value::str(name.to_string()));
            attrs.insert(
                "message".to_string(),
                Value::str(format!("Redeclaration of symbol '{}'", name)),
            );
            RuntimeError::typed("X::Redeclaration", attrs)
        };

        // mutsu currently mis-parses `anon sub foo {}` as a bare `anon` term
        // followed by a normal named SubDecl; an anonymous routine installs no
        // symbol, so skip the SubDecl that immediately follows a bare `anon`.
        // TODO: parse `anon sub`/`anon multi` into a genuinely anonymous routine
        // so this textual heuristic is unnecessary.
        let mut prev_was_anon = false;
        for stmt in stmts {
            // SetLine markers are interleaved between real statements; they must
            // not reset the "previous statement was `anon`" tracking.
            if matches!(stmt, Stmt::SetLine(_)) {
                continue;
            }
            let was_anon = prev_was_anon;
            prev_was_anon = matches!(stmt, Stmt::Expr(Expr::BareWord(w)) if w == "anon");

            // Routine redeclaration (sub a {}; sub a {} / sub a {}; multi sub a {}).
            if let Stmt::SubDecl { name, multi, .. } = stmt {
                if was_anon {
                    continue;
                }
                let name = name.resolve().to_string();
                if name.is_empty() {
                    continue;
                }
                match seen_routines.get(&name) {
                    None => {
                        seen_routines.insert(name, *multi);
                    }
                    Some(prev_all_multi) => {
                        if *prev_all_multi && *multi {
                            // still all-multi, allowed
                        } else {
                            let mut attrs = std::collections::HashMap::new();
                            attrs.insert("symbol".to_string(), Value::str(name.clone()));
                            attrs.insert("what".to_string(), Value::str("routine".to_string()));
                            attrs.insert(
                                "message".to_string(),
                                Value::str(format!(
                                    "Redeclaration of routine '{}'. Did you mean to declare a multi-sub?",
                                    name
                                )),
                            );
                            return Err(RuntimeError::typed("X::Redeclaration", attrs));
                        }
                    }
                }
                continue;
            }

            // `augment class C { class Nested {} }` redeclares a nested type that
            // the original declaration of C already declared.
            if let Stmt::AugmentClass { name, body, .. } = stmt {
                let cname = name.resolve().to_string();
                if let Some(existing) = class_nested.get(&cname) {
                    for nested in collect_nested(body) {
                        if existing.contains(&nested) {
                            return Err(type_redeclaration(&nested));
                        }
                    }
                }
                continue;
            }

            // Type-like declarations: class / role / subset / enum.
            let (name, is_stub) = match stmt {
                Stmt::ClassDecl {
                    name,
                    body,
                    is_lexical,
                    ..
                } => {
                    let name = name.resolve().to_string();
                    class_nested
                        .entry(name.clone())
                        .or_default()
                        .extend(collect_nested(body));
                    let is_stub = body_is_stub(body);
                    // A non-stub, non-lexical class that already exists in the
                    // outer environment cannot be redeclared. Lexical classes
                    // (`my class`) may shadow outer names. Only check
                    // package-qualified names (containing `::`): simple names may
                    // have leaked into the global registry from block-scoped
                    // declarations (a known scoping limitation).
                    if !is_stub && !*is_lexical && name.contains("::") && self.has_class(&name) {
                        return Err(type_redeclaration(&name));
                    }
                    (name, is_stub)
                }
                Stmt::RoleDecl { name, body, .. } => {
                    (name.resolve().to_string(), body_is_stub(body))
                }
                Stmt::SubsetDecl { name, .. } => (name.resolve().to_string(), false),
                Stmt::EnumDecl { name, .. } => (name.resolve().to_string(), false),
                _ => continue,
            };

            if name.is_empty() {
                continue;
            }

            match seen_types.get(&name) {
                None => {
                    seen_types.insert(name, is_stub);
                }
                Some(false) => {
                    return Err(type_redeclaration(&name));
                }
                Some(true) if is_stub => {
                    return Err(type_redeclaration(&name));
                }
                Some(true) => {
                    // A forward stub followed by its real definition: upgrade.
                    seen_types.insert(name, false);
                }
            }
        }
        Ok(())
    }

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

    /// Suggest lexically-declared variable names close to an undeclared
    /// `symbol` (sigiled, e.g. `$Foo`). Candidate names from `declared` are
    /// normalized to sigiled form (scalars are stored without `$`; arrays/hashes
    /// keep their sigil). Matching is sigil-insensitive on the name part so that
    /// e.g. `$barf` suggests `@barf`. Returns suggestions sorted by edit distance.
    fn suggest_declared_vars(symbol: &str, declared: &HashSet<String>) -> Vec<String> {
        use crate::runtime::did_you_mean::levenshtein_distance;
        let sigiled = |name: &str| -> String {
            if name.starts_with(['$', '@', '%', '&']) {
                name.to_string()
            } else {
                format!("${}", name)
            }
        };
        // Compare on the name part (without sigil) so a differing sigil counts
        // as a near match rather than excluding the candidate.
        let strip_sigil = |s: &str| s.trim_start_matches(['$', '@', '%', '&']).to_string();
        let target = strip_sigil(symbol);
        let max_distance = if target.len() <= 3 {
            1
        } else if target.len() <= 6 {
            2
        } else {
            3
        };
        let mut scored: Vec<(usize, String)> = Vec::new();
        for cand in declared {
            let cand_sigiled = sigiled(cand);
            if cand_sigiled == symbol {
                continue;
            }
            // Self is already excluded above by the full-name comparison, so a
            // distance of 0 here means a same-name-different-sigil candidate
            // (e.g. `$barf` -> `@barf`), which is a valid suggestion.
            let dist = levenshtein_distance(&target, &strip_sigil(&cand_sigiled));
            if dist <= max_distance {
                scored.push((dist, cand_sigiled));
            }
        }
        scored.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        scored.into_iter().map(|(_, s)| s).collect()
    }

    /// Suggest close routine names (built-in functions + user-defined subs) for
    /// an undeclared routine `name`. Used for X::Undeclared::Symbols
    /// `.routine_suggestion`.
    pub(crate) fn suggest_routine_names(&self, name: &str) -> Vec<String> {
        let mut candidates: Vec<String> = crate::runtime::builtins::BUILTIN_FUNCTION_NAMES
            .iter()
            .map(|s| s.to_string())
            .collect();
        candidates.extend(self.registry().functions.keys().map(|s| s.resolve()));
        Self::suggest_from_candidates(name, &candidates)
    }

    /// Suggest close type names (registered classes/roles) for an undeclared
    /// type `name`. Used for X::Undeclared::Symbols `.type_suggestion`.
    pub(crate) fn suggest_type_names(&self, name: &str) -> Vec<String> {
        let mut candidates: Vec<String> = self.registry().classes.keys().cloned().collect();
        candidates.extend(CORE_TYPE_NAMES.iter().map(|s| s.to_string()));
        Self::suggest_from_candidates(name, &candidates)
    }

    fn suggest_from_candidates(name: &str, candidates: &[String]) -> Vec<String> {
        use crate::runtime::did_you_mean::levenshtein_distance;
        // Rakudo accepts a candidate whose Levenshtein distance from the typo
        // is at most `chars div 3` (e.g. a 4-char name tolerates 1 edit, a
        // 9-char name 3 edits), with a floor of 1 for names of length >= 3.
        let max_distance =
            (name.chars().count() / 3).max(if name.chars().count() >= 3 { 1 } else { 0 });
        let mut scored: Vec<(usize, String)> = Vec::new();
        let mut seen = HashSet::new();
        for cand in candidates {
            if cand == name || !seen.insert(cand.clone()) {
                continue;
            }
            let dist = levenshtein_distance(name, cand);
            if dist > 0 && dist <= max_distance {
                scored.push((dist, cand.clone()));
            }
        }
        scored.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        scored.into_iter().map(|(_, s)| s).collect()
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
            Stmt::SubDecl { params, body, .. } => {
                let mut inner = declared.clone();
                Self::add_routine_locals(params, body, &mut inner);
                self.find_undeclared_var_in_body(body, &inner)
            }
            Stmt::MethodDecl { params, body, .. } => {
                let mut inner = declared.clone();
                Self::add_routine_locals(params, body, &mut inner);
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

    /// Check for undeclared type names in EVAL'd code.
    /// Walks the AST looking for BareWord expressions that start with uppercase
    /// and aren't known types, classes, or packages. This mirrors Raku's
    /// compile-time check for undeclared symbols.
    pub(crate) fn check_eval_undeclared_names(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
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
        // Include all user-defined operators (infix, prefix, postfix,
        // circumfix, postcircumfix) so the EVAL parser can recognize them.
        for key in self.registry().functions.keys() {
            let key_s = key.resolve();
            let name = if let Some(pos) = key_s.rfind("::") {
                &key_s[pos + 2..]
            } else {
                key_s.as_str()
            };
            if name.starts_with("circumfix:")
                || name.starts_with("postcircumfix:")
                || name.starts_with("infix:")
                || name.starts_with("prefix:")
                || name.starts_with("postfix:")
            {
                seen.insert(name.to_string());
            }
        }
        for key in self.env.keys() {
            if key.starts_with("circumfix:")
                || key.starts_with("postcircumfix:")
                || key.starts_with("infix:")
                || key.starts_with("prefix:")
                || key.starts_with("postfix:")
            {
                seen.insert(key.resolve());
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

    /// Collect user-declared subroutine names from the current runtime so
    /// EVAL'd code can see them as declared at parse time. This allows
    /// constructs like `first.uc` (where `first` is a user sub shadowing
    /// the `first` listop builtin) to parse correctly as `first().uc`.
    pub(crate) fn collect_eval_user_sub_names(&self) -> Vec<String> {
        let mut names: Vec<String> = Vec::new();
        for key in self.registry().functions.keys() {
            let key_s = key.resolve();
            let short = if let Some(pos) = key_s.rfind("::") {
                &key_s[pos + 2..]
            } else {
                key_s.as_str()
            };
            // Skip empty/meta-named entries. Operator subs are handled by
            // collect_operator_sub_names.
            if short.is_empty() || short.contains(':') {
                continue;
            }
            names.push(short.to_string());
        }
        names
    }

    pub(super) fn eval_eval_string(&mut self, code: &str) -> Result<Value, RuntimeError> {
        let routine_snapshot = self.snapshot_routine_registry();
        let roles_snapshot = self.registry().roles.clone();
        let user_declared_roles_snapshot =
            std::mem::take(&mut self.registry_mut().user_declared_roles);
        let role_candidates_snapshot = self.registry().role_candidates.clone();
        let role_type_params_snapshot = self.registry().role_type_params.clone();
        let role_parents_snapshot = self.registry().role_parents.clone();
        let role_hides_snapshot = self.registry().role_hides.clone();
        let classes_snapshot = self.registry().classes.clone();
        let hidden_classes_snapshot = self.registry().hidden_classes.clone();
        let hidden_defer_parents_snapshot = self.registry().hidden_defer_parents.clone();
        let class_composed_roles_snapshot = self.registry().class_composed_roles.clone();
        let class_role_param_bindings_snapshot = self.registry().class_role_param_bindings.clone();
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
        // Record the `&name` code-vars that already exist in the enclosing scope,
        // so a sub declared inside this EVAL may shadow them without being treated
        // as a redeclaration (see registration_sub.rs).
        crate::runtime::registration_sub::push_eval_outer_amp_names(
            self.env
                .keys()
                .map(|k| k.resolve().to_string())
                .filter(|k| k.starts_with('&')),
        );
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
        let current_roles = self.registry().roles.clone();
        let current_role_candidates = self.registry().role_candidates.clone();
        let current_role_type_params = self.registry().role_type_params.clone();
        let current_role_parents = self.registry().role_parents.clone();
        let current_role_hides = self.registry().role_hides.clone();
        let current_classes = self.registry().classes.clone();
        let current_hidden_classes = self.registry().hidden_classes.clone();
        let current_hidden_defer_parents = self.registry().hidden_defer_parents.clone();
        let current_class_composed_roles = self.registry().class_composed_roles.clone();
        let current_class_role_param_bindings = self.registry().class_role_param_bindings.clone();
        let current_env = self.env.clone();
        let current_type_keys: std::collections::HashSet<String> = self
            .registry()
            .roles
            .keys()
            .chain(self.registry().classes.keys())
            .cloned()
            .collect();
        let snapshot_type_keys: std::collections::HashSet<String> = roles_snapshot
            .keys()
            .chain(classes_snapshot.keys())
            .cloned()
            .collect();
        self.registry_mut().roles = roles_snapshot;
        self.registry_mut().user_declared_roles = user_declared_roles_snapshot;
        self.registry_mut().role_candidates = role_candidates_snapshot;
        self.registry_mut().role_type_params = role_type_params_snapshot;
        self.registry_mut().role_parents = role_parents_snapshot;
        self.registry_mut().role_hides = role_hides_snapshot;
        self.registry_mut().classes = classes_snapshot;
        self.registry_mut().hidden_classes = hidden_classes_snapshot;
        self.registry_mut().hidden_defer_parents = hidden_defer_parents_snapshot;
        self.registry_mut().class_composed_roles = class_composed_roles_snapshot;
        self.registry_mut().class_role_param_bindings = class_role_param_bindings_snapshot;
        self.registry_mut().roles.extend(current_roles);
        // Don't extend user_declared_roles: roles declared in EVAL are EVAL-scoped
        // and should not affect the parent's redeclaration detection.
        self.registry_mut()
            .role_candidates
            .extend(current_role_candidates);
        self.registry_mut()
            .role_type_params
            .extend(current_role_type_params);
        self.registry_mut()
            .role_parents
            .extend(current_role_parents);
        self.registry_mut().role_hides.extend(current_role_hides);
        self.registry_mut().classes.extend(current_classes);
        self.registry_mut()
            .hidden_classes
            .extend(current_hidden_classes);
        self.registry_mut()
            .hidden_defer_parents
            .extend(current_hidden_defer_parents);
        self.registry_mut()
            .class_composed_roles
            .extend(current_class_composed_roles);
        self.registry_mut()
            .class_role_param_bindings
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
            .filter(|key| key.starts_with("&"))
            .map(|key| key.resolve())
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
        crate::runtime::registration_sub::pop_eval_outer_amp_names();
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

    fn build_lexical_hash(&self, env: &Env, callframe_depth: Option<usize>) -> Value {
        let mut hash = HashMap::new();
        for (k, v) in env.iter() {
            // Skip internal keys and special variables
            if k.starts_with("__") || k.starts_with("?") || k.starts_with("*") || k.starts_with("=")
            {
                continue;
            }
            // Skip type names, enum values, and signal names
            k.with_str(|s| {
                if s.chars().next().is_some_and(|c| c.is_uppercase()) {
                    return;
                }
                // Add sigil prefix based on the env key convention:
                // - Keys starting with '@' or '%' already have sigils (array/hash vars)
                // - Other keys are scalar variables that need '$' prefix
                let sigiled = if s.starts_with('@') || s.starts_with('%') || s.starts_with('&') {
                    s.to_string()
                } else {
                    format!("${}", s)
                };
                hash.insert(sigiled, v.clone());
            });
        }
        // Store callframe depth so assignments can write back to the caller env
        if let Some(depth) = callframe_depth {
            hash.insert("__callframe_depth".to_string(), Value::Int(depth as i64));
        }
        Value::hash(hash)
    }

    pub(crate) fn build_annotations(&self, attrs: &HashMap<String, Value>) -> Value {
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
