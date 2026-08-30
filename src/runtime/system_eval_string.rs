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
        matches!(result.view(), ValueView::Str(s) if s.as_str() == name)
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
        // The `Test` module's own exports; the single copy lives in
        // runtime::test_functions so the parser, EVAL and `exec_call` agree.
        crate::runtime::TEST_MODULE_EXPORTS
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
        // Also collect lexical `&name` code-variables (e.g. `my &b2 := ...`).
        // These are subs held in the environment rather than the registry, so
        // EVAL'd code that calls them in listop form (`b2 Num`) needs their bare
        // names known at parse time to parse as a call rather than two terms.
        for key in self.env.keys() {
            let key_s = key.resolve();
            if let Some(bare) = key_s.strip_prefix('&') {
                if bare.is_empty() || bare.contains(':') {
                    continue;
                }
                names.push(bare.to_string());
            }
        }
        names
    }

    /// Collect the type names (classes, roles, enums, subsets) the calling unit
    /// has declared, so an EVAL'd snippet parses them as declared types. mutsu
    /// registers user types in the runtime registry, not in the parser's scope
    /// stack, and the nested parse starts from an empty scope stack — without
    /// this seed every outer type looks undeclared to it, which the `when`
    /// gobbled-block check would report as a syntax error on valid code.
    pub(crate) fn collect_eval_user_type_names(&self) -> Vec<String> {
        let registry = self.registry();
        registry
            .classes
            .keys()
            .chain(registry.roles.keys())
            .chain(registry.enum_types.keys())
            .chain(registry.subsets.keys())
            .cloned()
            .collect()
    }

    /// Collect the sigilless *value* term names (`constant Foo = 1`, `my \\x`)
    /// the calling unit has declared, so an EVAL'd snippet parses them as
    /// declared terms. The type-name twin above reads the class/role/enum
    /// registry; constants have no such registry, but every one of them leaves a
    /// `__mutsu_constant_var::<name>` marker in the environment when it is
    /// declared, which is exactly the set the parser wants back.
    ///
    /// Sigiled constants (`constant $x = 1`) are skipped: they are read through
    /// their sigil, never as a bareword term, so they are not term symbols.
    pub(crate) fn collect_eval_user_value_term_names(&self) -> Vec<String> {
        const MARKER: &str = "__mutsu_constant_var::";
        self.env
            .keys()
            .filter_map(|key| {
                let name = key.resolve();
                let bare = name.strip_prefix(MARKER)?;
                let first = bare.chars().next()?;
                (!matches!(first, '$' | '@' | '%' | '&') && !bare.contains(':'))
                    .then(|| bare.to_string())
            })
            .collect()
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
        // ADR-0019 F4c-9b: `classes_snapshot` no longer carries method rows
        // (there is no `ClassDef::methods` field left), so snapshot the
        // canonical table's rows for every class here too -- before the
        // EVAL runs and can mutate/clear them -- for the resurrected-class
        // repair below to restore from.
        let method_rows_snapshot: HashMap<String, Vec<(crate::symbol::Symbol, Vec<MethodDef>)>> =
            classes_snapshot
                .keys()
                .map(|name| {
                    let owner = crate::symbol::Symbol::intern(name);
                    (
                        name.clone(),
                        self.registry().user_method_rows_for_owner(owner),
                    )
                })
                .collect();
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
            return Err(RuntimeError::obsolete(
                trimmed,
                "lines() to read input, ('') to represent a null string or () to \
                 represent an empty list",
            ));
        }
        let previous_pod = self.env.get("=pod").cloned();
        let saved_in_eval = self.env.get("__mutsu_in_eval").cloned();
        // A pragma the EVAL'd unit turns on is scoped to that unit. `use fatal`
        // is the one that bites: mutsu keeps it as an interpreter-wide flag, so
        // without this the *caller* kept throwing on every later soft Failure
        // long after the EVAL returned. (`throws-like 'use fatal; ...'` is a
        // common assertion shape, so one of them poisoned the rest of the file.)
        let saved_fatal_mode = self.fatal_mode;
        // ... and the EVAL'd unit does not INHERIT one either. `fatal` is
        // lexical to a compilation unit and EVAL compiles a fresh one, so a
        // caller's `use fatal` — or `try`'s implicit one — must not fatalize the
        // snippet. Measured against rakudo: `use fatal; try EVAL 'my $x =
        // Failure.new; 1'` answers 1, and so does the same without `use fatal`;
        // only `EVAL 'use fatal; my $x = Failure.new; 1'`, where the snippet
        // turns it on itself, dies. (The `use fatal; try { EVAL q["bar"[5]] }`
        // -> X::OutOfRange case that once argued for inheriting it proves
        // nothing: an out-of-range subscript throws there with or without
        // `fatal`.)
        self.fatal_mode = false;
        // Unlike `fatal` (a runtime dynamic-scope check the EVAL'd unit
        // legitimately inherits from its caller -- `raku -e 'use
        // MONKEY-SEE-NO-EVAL; use fatal; try { EVAL q["bar"[5]] }; say
        // $!.^name'` prints X::OutOfRange, so the caller's `fatal` IS live
        // inside the EVAL), `MONKEY-TYPING` gates a COMPILE-TIME check
        // (`augment class Foo {}` is only legal syntax when it's active) and
        // EVAL is a fresh compilation unit for that check: an outer `use
        // MONKEY-TYPING;` does NOT make an `augment` inside a *separately
        // EVAL'd* string legal (verified against `raku -e 'use MONKEY-TYPING;
        // try { EVAL q[class C { method f {} }; augment class C { method f
        // {} }] }; say $!.^name'` -> X::Syntax::Augment::WithoutMonkeyTyping,
        // not the method-clash error an inherited pragma would reach).
        // `roast/S12-class/augment-supersede.t` exercises exactly this shape.
        let saved_monkey_typing = self.monkey_typing;
        self.monkey_typing = false;
        // CALLER:: from the EVAL'd unit's mainline must not resolve directly in
        // the scope that invoked EVAL (see push_eval_caller_frames for the
        // frame layout raku exposes).
        self.push_eval_caller_frames();
        // Record the `&name` code-vars that already exist in the enclosing scope,
        // so a sub declared inside this EVAL may shadow them without being treated
        // as a redeclaration (see registration_sub.rs).
        //
        // `visible_keys_where`, not `keys()`: the redeclaration check this feeds
        // resolves the name with `env.get`, which walks the parent chain and the
        // base tier, while `keys()` exposes only the innermost tier's overlay.
        // A name reachable through the chain was therefore *found* by the check
        // but *absent* from the shadow snapshot, so an EVAL'd `sub f` collided
        // with an outer `f` instead of shadowing it — which is what
        // `roast/S04-statements/given.t` does, EVALing a fresh `sub test-given`
        // per subtest while an earlier subtest's `my sub test-given` is still
        // reachable.
        crate::runtime::registration_sub::push_eval_outer_amp_names(
            self.env
                .visible_keys_where(|k| k.starts_with('&'))
                .into_iter(),
        );
        // ...and the registry counterpart, which is what the redeclaration
        // checks against `functions` consult. Interned `Symbol`s, so no strings
        // are allocated to take it.
        crate::runtime::registration_sub::push_eval_outer_routine_keys(
            self.registry().functions.keys().copied(),
        );
        self.env.insert("__mutsu_in_eval".to_string(), Value::TRUE);
        // A `:key<>` colonpair (empty angle brackets) in the EVAL'd source's Pod
        // is a fatal compile error in Raku; short-circuit before evaluating.
        let pod_err = self.collect_pod_blocks(trimmed).err();
        let pod_failed = pod_err.is_some();
        // Collect operator sub names so the parser recognizes them in EVAL context
        let op_names = self.collect_operator_sub_names();
        let op_assoc = self.collect_operator_assoc_map();
        let imported_names = self.collect_eval_imported_function_names();
        let bracketed_stmt_inner = unwrap_bracketed_statements(trimmed)
            .filter(|inner| looks_like_bracketed_statement_list(inner));
        // General case: parse and evaluate as Raku code
        let mut result = if let Some(err) = pod_err {
            Err(err)
        } else if let Some(inner) = bracketed_stmt_inner {
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
        self.emit_parse_warnings(crate::parser::take_parse_warnings());
        // Fallback: parser still rejects forms like `~< foo bar >`.
        // Rewrite to an equivalent parenthesized form and try again.
        if !pod_failed
            && result.is_err()
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
        if !pod_failed
            && result.is_err()
            && let Some(inner) = unwrap_parenthesized_statements(trimmed)
        {
            result =
                self.parse_and_eval_with_operators(inner, &op_names, &op_assoc, &imported_names);
        }
        // EVAL q[[ ... ]] sometimes carries one outer statement-list bracket pair.
        if !pod_failed
            && result.is_err()
            && bracketed_stmt_inner.is_none()
            && let Some(inner) = unwrap_bracketed_statements(trimmed)
        {
            result =
                self.parse_and_eval_with_operators(inner, &op_names, &op_assoc, &imported_names);
        }
        // EVAL should accept routine declarations in snippet context.
        // If unit-scope parsing rejects a declaration, retry inside an implicit block.
        if !pod_failed
            && result.is_err()
            && trimmed.contains("sub ")
            && let Some(err) = result.as_ref().err()
            && err.message.contains("X::UnitScope::Invalid")
        {
            let wrapped = format!("{{ {}; }}", trimmed);
            let saved_wrapped_eval = self.env.get("__mutsu_eval_wrapped_decls").cloned();
            self.env
                .insert("__mutsu_eval_wrapped_decls".to_string(), Value::TRUE);
            result =
                self.parse_and_eval_with_operators(&wrapped, &op_names, &op_assoc, &imported_names);
            if let Some(saved) = saved_wrapped_eval {
                self.env
                    .insert("__mutsu_eval_wrapped_decls".to_string(), saved);
            } else {
                self.env.remove("__mutsu_eval_wrapped_decls");
            }
        }
        self.pop_eval_caller_frames();
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
        self.fatal_mode = saved_fatal_mode;
        self.monkey_typing = saved_monkey_typing;
        self.restore_routine_registry_eval(routine_snapshot);
        let current_env = self.env.clone();
        let eval_role_names: std::collections::HashSet<String> = current_env
            .keys()
            .filter_map(|key| key.strip_prefix_str("__mutsu_eval_role::"))
            .collect();
        // An EVAL may return a role type object (`my $r = EVAL 'unit role R;'`).
        // That object remains usable by its caller, so preserve exactly that
        // role's registry entries while isolating every other EVAL-local role.
        let returned_role_names: std::collections::HashSet<String> = result
            .as_ref()
            .ok()
            .and_then(|value| match value.view() {
                crate::value::ValueView::Package(name)
                    if eval_role_names.contains(&name.resolve()) =>
                {
                    Some(name.resolve().to_string())
                }
                _ => None,
            })
            .into_iter()
            .collect();
        let current_roles = self.registry().roles.clone();
        let current_role_candidates = self.registry().role_candidates.clone();
        let current_role_type_params = self.registry().role_type_params.clone();
        let current_role_parents = self.registry().role_parents.clone();
        let current_role_hides = self.registry().role_hides.clone();
        let is_eval_role_artifact = |name: &str| {
            eval_role_names.iter().any(|role| {
                !returned_role_names.contains(role)
                    && (name == role
                        || name
                            .strip_prefix(role)
                            .is_some_and(|suffix| suffix.starts_with('[')))
            })
        };
        let mut current_classes = self.registry().classes.clone();
        current_classes.retain(|name, _| !is_eval_role_artifact(name));
        let current_hidden_classes = self.registry().hidden_classes.clone();
        let current_hidden_defer_parents = self.registry().hidden_defer_parents.clone();
        let mut current_class_composed_roles = self.registry().class_composed_roles.clone();
        current_class_composed_roles.retain(|name, roles| {
            !is_eval_role_artifact(name) && !roles.iter().any(|role| eval_role_names.contains(role))
        });
        let mut current_class_direct_composed_roles =
            self.registry().class_direct_composed_roles.clone();
        current_class_direct_composed_roles.retain(|name, roles| {
            !is_eval_role_artifact(name) && !roles.iter().any(|role| eval_role_names.contains(role))
        });
        let mut current_class_role_param_bindings =
            self.registry().class_role_param_bindings.clone();
        current_class_role_param_bindings.retain(|name, _| !is_eval_role_artifact(name));
        let snapshot_type_keys: std::collections::HashSet<String> = roles_snapshot
            .keys()
            .chain(classes_snapshot.keys())
            .cloned()
            .collect();
        // ADR-0019 F4c-8(b): the classes whose canonical method table
        // actually needs a registry re-derive after the `classes = snapshot;
        // classes.extend(current)` merge below -- computed now, before that
        // merge consumes `classes_snapshot`. `extend` makes `current` win
        // for every key `current` has, so the merge's only real effect is
        // *resurrecting* keys the EVAL removed (`withdraw_role_pun`,
        // `__MUTSU_UNREGISTER_CLASS__`, ...); every other class's
        // `method_entries` rows are already correct, kept in sync live by
        // whatever ran during the EVAL itself.
        let resurrected_classes: Vec<String> = classes_snapshot
            .keys()
            .filter(|name| !current_classes.contains_key(*name))
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
        // Roles declared in EVAL, including `my role`, are lexical to that
        // compilation unit. Preserve only a role type object returned directly
        // from EVAL; every other EVAL-local role must not overwrite a same-named
        // role the caller declares later.
        for name in &returned_role_names {
            if let Some(role) = current_roles.get(name) {
                self.registry_mut().roles.insert(name.clone(), role.clone());
            }
            if let Some(candidates) = current_role_candidates.get(name) {
                self.registry_mut()
                    .role_candidates
                    .insert(name.clone(), candidates.clone());
            }
            if let Some(params) = current_role_type_params.get(name) {
                self.registry_mut()
                    .role_type_params
                    .insert(name.clone(), params.clone());
            }
            if let Some(parents) = current_role_parents.get(name) {
                self.registry_mut()
                    .role_parents
                    .insert(name.clone(), parents.clone());
            }
            if let Some(hides) = current_role_hides.get(name) {
                self.registry_mut()
                    .role_hides
                    .insert(name.clone(), hides.clone());
            }
        }
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
        // ADR-0019 F4c-8(b): this used to loop over every class in the
        // registry (O(all classes) x O(total table rows), to repair at most
        // a handful of owners); scoping to `resurrected_classes` (computed
        // above, before the merge) is O(resurrected) instead. Every other
        // class's `method_entries` rows are already correct and untouched
        // by the snapshot/extend dance.
        for class_name in resurrected_classes {
            let owner = crate::symbol::Symbol::intern(&class_name);
            let rows = method_rows_snapshot
                .get(&class_name)
                .cloned()
                .unwrap_or_default();
            self.registry_mut().restore_user_method_rows(owner, rows);
            self.registry_mut().sync_accessor_entries(owner);
        }
        for key in &snapshot_type_keys {
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
        self.env
            .retain(|key, _| !key.starts_with("__mutsu_eval_role::"));
        // Restore $_ so EVAL does not clobber the caller's topic variable
        if let Some(topic) = saved_topic {
            self.env.insert("_".to_string(), topic);
        } else {
            self.env.remove("_");
        }
        crate::runtime::registration_sub::pop_eval_outer_amp_names();
        crate::runtime::registration_sub::pop_eval_outer_routine_keys();
        // An EVAL parse failure is exposed as `$!`, whose string form is what
        // Test.rakumod's `eval-lives-ok` prints after `# Error:`.  The raw
        // parser diagnostic is useful to the CLI renderer, but starts with an
        // internal "Confused. parse error ..." wrapper that is neither useful
        // nor compatible with Raku's EVAL diagnostic.  Keep the structured
        // parse code and location intact while giving the caught exception the
        // EVAL-facing prefix Raku uses.
        if let Err(err) = &mut result
            && err.code().is_some_and(|code| code.is_parse())
            && err.message.starts_with("Confused. parse error")
            && !err.message.starts_with("Unable to parse")
        {
            err.message = format!("Unable to parse expression; {}", err.message);
        }
        result
    }
}
