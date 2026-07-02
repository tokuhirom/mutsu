use super::*;

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

impl Interpreter {
    pub(super) fn eval_result_is_unresolved_bareword(
        &self,
        stmts: &[Stmt],
        result: &Value,
    ) -> bool {
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
        let class_direct_composed_roles_snapshot =
            self.registry().class_direct_composed_roles.clone();
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
        let current_class_direct_composed_roles =
            self.registry().class_direct_composed_roles.clone();
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
        self.registry_mut().class_direct_composed_roles = class_direct_composed_roles_snapshot;
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
            .class_direct_composed_roles
            .extend(current_class_direct_composed_roles);
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
}
