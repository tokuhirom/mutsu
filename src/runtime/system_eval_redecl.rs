use super::*;

impl Interpreter {
    pub(crate) fn check_eval_class_redeclarations(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // `our sub` is package-scoped: a duplicate across sibling blocks is
        // X::Redeclaration even though each block is its own lexical scope.
        if let Some(e) = Self::find_our_routine_redeclaration(stmts, &mut HashSet::new()) {
            return Err(e);
        }
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
        // The `repr` of each class at its first declaration. A later declaration
        // (e.g. upgrading a `{ ... }` stub) may not introduce a different repr —
        // that is X::TooLateForREPR ("must be set at initial declaration").
        let mut seen_class_repr: HashMap<String, Option<String>> = HashMap::new();
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

        // Within one class/role/grammar body, two `my`/`our`-scoped methods (or
        // tokens) of the same name are an X::Redeclaration (a plain `method`/`token`
        // redeclared is the separate X::Method::Duplicate, not handled here).
        // Mirrors Rakudo.
        let dup_scoped_method = |body: &[Stmt]| -> Option<RuntimeError> {
            let mut seen_my: HashSet<String> = HashSet::new();
            let mut seen_our: HashSet<String> = HashSet::new();
            for s in body {
                let (name, is_my, is_our, what) = match s {
                    Stmt::MethodDecl {
                        name,
                        is_my,
                        is_our,
                        ..
                    } => (name, *is_my, *is_our, "method"),
                    Stmt::TokenDecl {
                        name,
                        is_my,
                        is_our,
                        ..
                    } => (name, *is_my, *is_our, "regex"),
                    _ => continue,
                };
                let n = name.resolve().to_string();
                if n.is_empty() {
                    continue;
                }
                let dup = (is_my && !seen_my.insert(n.clone()))
                    || (is_our && !seen_our.insert(n.clone()));
                if dup {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("symbol".to_string(), Value::str(n.clone()));
                    attrs.insert("what".to_string(), Value::str(what.to_string()));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!("Redeclaration of {} '{}'", what, n)),
                    );
                    return Some(RuntimeError::typed("X::Redeclaration", attrs));
                }
            }
            None
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
                    repr,
                    ..
                } => {
                    if let Some(err) = dup_scoped_method(body) {
                        return Err(err);
                    }
                    let name = name.resolve().to_string();
                    // X::TooLateForREPR: the repr must be fixed at the initial
                    // declaration. If an earlier declaration of this class had a
                    // different (typically absent) repr, a later one cannot set it.
                    match seen_class_repr.get(&name) {
                        Some(prev) if repr.is_some() && prev != repr => {
                            let mut attrs = std::collections::HashMap::new();
                            attrs.insert("type".to_string(), Value::str(name.clone()));
                            attrs.insert(
                                "message".to_string(),
                                Value::str(format!(
                                    "Cannot change REPR of {} now (must be set at initial declaration)",
                                    name
                                )),
                            );
                            return Err(RuntimeError::typed("X::TooLateForREPR", attrs));
                        }
                        Some(_) => {}
                        None => {
                            seen_class_repr.insert(name.clone(), repr.clone());
                        }
                    }
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
                    if let Some(err) = dup_scoped_method(body) {
                        return Err(err);
                    }
                    (name.resolve().to_string(), body_is_stub(body))
                }
                Stmt::SubsetDecl { name, .. } => (name.resolve().to_string(), false),
                Stmt::EnumDecl { name, variants, .. } => {
                    // Enum value names share the type/term namespace, so a later
                    // class/role/enum (or another value) of the same name is a
                    // redeclaration. Register each value name as a non-stub
                    // symbol. (`__DYNAMIC__` is the placeholder for an unresolved
                    // parenthesised body, handled by the undeclared-names check.)
                    for (vname, _) in variants {
                        if vname.is_empty() || vname == "__DYNAMIC__" {
                            continue;
                        }
                        if matches!(seen_types.get(vname), Some(false)) {
                            return Err(type_redeclaration(vname));
                        }
                        seen_types.insert(vname.clone(), false);
                    }
                    (name.resolve().to_string(), false)
                }
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
}
