use super::*;

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
    pub(super) fn parse_and_eval_with_operators(
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
                self.check_eval_begin_forward_calls(&stmts)?;
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

    /// Walk bare blocks collecting `our sub` names (package-scoped): two
    /// `our sub foo` declarations install the same package symbol, so a duplicate
    /// across sibling blocks (or block vs mainline) is X::Redeclaration. `my sub`
    /// is lexical and does not conflict across blocks, so it is ignored here.
    pub(super) fn find_our_routine_redeclaration(
        stmts: &[Stmt],
        seen: &mut HashSet<String>,
    ) -> Option<RuntimeError> {
        for stmt in stmts {
            match stmt {
                Stmt::SubDecl {
                    name,
                    multi: false,
                    custom_traits,
                    ..
                } if custom_traits.iter().any(|(t, _)| t == "__our_scoped") => {
                    let n = name.resolve().to_string();
                    if !n.is_empty() && !seen.insert(n.clone()) {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("symbol".to_string(), Value::str(n.clone()));
                        attrs.insert("what".to_string(), Value::str("routine".to_string()));
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!("Redeclaration of routine '{}'", n)),
                        );
                        return Some(RuntimeError::typed("X::Redeclaration", attrs));
                    }
                }
                Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                    if let Some(e) = Self::find_our_routine_redeclaration(body, seen) {
                        return Some(e);
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Suggest lexically-declared variable names close to an undeclared
    /// `symbol` (sigiled, e.g. `$Foo`). Candidate names from `declared` are
    /// normalized to sigiled form (scalars are stored without `$`; arrays/hashes
    /// keep their sigil). Matching is sigil-insensitive on the name part so that
    /// e.g. `$barf` suggests `@barf`. Returns suggestions sorted by edit distance.
    pub(super) fn suggest_declared_vars(symbol: &str, declared: &HashSet<String>) -> Vec<String> {
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

    pub(crate) fn suggest_from_candidates(name: &str, candidates: &[String]) -> Vec<String> {
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
}
