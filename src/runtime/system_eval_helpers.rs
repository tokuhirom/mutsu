use super::*;


pub(super) fn rewrite_prefixed_angle_list(code: &str) -> Option<String> {
    let (prefix, rest) = if let Some(rest) = code.strip_prefix('~') {
        ('~', rest)
    } else if let Some(rest) = code.strip_prefix('+') {
        ('+', rest)
    } else {
        let rest = code.strip_prefix('?')?;
        ('?', rest)
    };
    let inner = rest.trim_start();
    if !inner.starts_with('<') || !inner.ends_with('>') {
        return None;
    }
    Some(format!("{}({})", prefix, inner))
}

pub(super) fn unwrap_parenthesized_statements(code: &str) -> Option<&str> {
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

pub(super) fn unwrap_bracketed_statements(code: &str) -> Option<&str> {
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

pub(super) fn looks_like_bracketed_statement_list(inner: &str) -> bool {
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
    pub(in crate::runtime) fn eval_result_is_unresolved_bareword(
        &self,
        stmts: &[Stmt],
        result: &Value,
    ) -> bool {
        let [Stmt::Expr(Expr::BareWord(name))] = stmts else {
            return false;
        };
        matches!(result.view(), ValueView::Str(s) if s.as_str() == name)
            && !self.env().contains_key(name)
            && self.term_binding(name).is_none()
            && !self.has_class(name)
            && !self.has_function(name)
            && !self.has_multi_function_unindexed(name)
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
            let name = crate::qualified::last_segment(*key).as_str();
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
        for name in self.module.imported_operator_names.iter() {
            seen.insert(name.clone());
        }
        let mut names: Vec<String> = seen.into_iter().collect();
        names.sort();
        names
    }

    pub(crate) fn collect_operator_assoc_map(&self) -> HashMap<String, String> {
        let mut assoc = HashMap::new();
        for (key, value) in self.dispatch.operator_assoc.iter() {
            let name = crate::qualified::last_segment(crate::qualified::known_symbol(key)).as_str();
            if name.starts_with("infix:<") {
                assoc.insert(name.to_string(), value.clone());
            }
        }
        assoc
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

    /// The type names the calling scope declares, for the RakuAST conversion of
    /// an EVAL string: the registry's types, plus the lexical ones (`my class
    /// P`), which live in the environment as the package they are stored under.
    // Cost: O(r + e + m + x*d), r = registry types, e = environment entries,
    // m = loaded modules, x = export namespaces, d = EVAL ancestry (at most 64).
    pub(crate) fn eval_caller_type_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self
            .collect_eval_user_type_names()
            .into_iter()
            .map(|n| n.split('\u{0}').next().unwrap_or(&n).to_string())
            .collect();
        // The running package's own types (and its enclosing packages') by
        // their short names: a module routine's `EVAL 'Bar'` for the module's
        // `class Bar`, which the frame env does not hold.
        let pkg = self.current_package_sym();
        if !crate::qualified::is_global_package(pkg) {
            let own: Vec<Symbol> = crate::qualified::package_ancestors(pkg)
                .filter(|p| !crate::qualified::is_global_package(*p))
                .collect();
            let short: Vec<String> = names
                .iter()
                .filter_map(|name| {
                    let sym = Symbol::intern(name);
                    crate::qualified::package_parent(sym)
                        .filter(|parent| own.contains(parent))
                        .map(|_| crate::qualified::unqualified_part(sym).resolve().to_string())
                })
                .collect();
            names.extend(short);
        }
        names.extend(self.env.iter().filter_map(|(key, value)| {
            let name = key.resolve();
            matches!(value.view(), ValueView::Package(_))
                .then(|| name.split('\u{0}').next().unwrap_or(&name).to_string())
        }));
        // A loaded module's package is global, whatever frame the `EVAL` runs
        // in (`require Test; ... eval-lives-ok 'Test'` from inside `Test`).
        names.extend(self.module.loaded_modules.iter().cloned());
        // Generated EXPORT stashes are visible namespace objects without being
        // user package declarations. Preserve their classification in EVAL.
        names.extend(
            self.module.package_export_namespaces.iter().map(|name| name.resolve()),
        );
        let unit = self.executing_unit_sym_for_module_load();
        names.extend(self.module.unit_export_namespaces.iter().filter_map(|(owner, name)| {
            if self.unit_chain_contains_unit(unit, *owner)
                || self.unit_chain_contains_unit(self.current_unit, *owner)
            {
                Some(name.resolve())
            } else {
                None
            }
        }));
        names
    }

    /// The enum values the calling scope declares, bare and qualified by their
    /// enum's name, for the RakuAST conversion of an EVAL string.
    // Cost: O(v), v = enum values in the registry.
    pub(crate) fn eval_caller_enum_value_names(&self) -> Vec<String> {
        let mut names = self.collect_eval_user_value_term_names();
        for (owner, variants) in &self.registry().enum_types {
            for (variant, _) in variants {
                names.push(variant.clone());
                names.push(
                    crate::qualified::qualified(Symbol::intern(owner), Symbol::intern(variant))
                        .resolve()
                        .to_string(),
                );
            }
        }
        names
    }

    /// Collect the sigilless *value* term names (`constant Foo = 1`, `my \\x`)
    /// the calling unit has declared, so an EVAL'd snippet parses them as
    /// declared terms. The type-name twin above reads the class/role/enum
    /// registry; constants have no such registry, but every one of them leaves a
    /// `__mutsu_constant_var::<name>` marker in the environment when it is
    /// declared, which is exactly the set the parser wants back. A module's
    /// top-level constants keep that marker in a package-keyed table instead
    /// (`runtime::toplevel_markers`); those visible from the running
    /// code count too, unless an env marker hides one for this scope.
    ///
    /// Sigiled constants (`constant $x = 1`) are skipped: they are read through
    /// their sigil, never as a bareword term, so they are not term symbols.
    pub(crate) fn collect_eval_user_value_term_names(&self) -> Vec<String> {
        const MARKER: &str = "__mutsu_constant_var::";
        fn term_name(stored: &str) -> Option<String> {
            // A sigil-less constant's marker names its term key (#9962).
            let bare = crate::runtime::term_names::term_spelling(stored).unwrap_or(stored);
            let first = bare.chars().next()?;
            (!matches!(first, '$' | '@' | '%' | '&') && !bare.contains(':'))
                .then(|| bare.to_string())
        }
        // Every enclosing scope's markers count, not only the current frame's
        // tier: a block whose frame does not flatten its outer scope still sees
        // the outer `constant b` (`constant b = 1; { EVAL 'b' }`).
        let mut names: Vec<String> = self
            .env
            .visible_keys_where(|name| name.starts_with(MARKER))
            .into_iter()
            .filter_map(|name| {
                let stored = name.strip_prefix(MARKER)?;
                // A `False` marker hides a table marker for this scope.
                if !self
                    .env
                    .get_sym(Symbol::intern(&name))
                    .is_some_and(Value::truthy)
                {
                    return None;
                }
                term_name(stored)
            })
            .collect();
        names.extend(
            self.visible_toplevel_constant_marker_names()
                .into_iter()
                .filter(|stored| self.constant_marker_visible(stored))
                .filter_map(term_name),
        );
        // A constant is `our`-scoped too: one declared in a block that has
        // already ended (`{ constant c = 1 }; EVAL 'c'`) is still a GLOBAL
        // member. `our_vars` keeps a GLOBAL sigilless symbol under its `\name`
        // spelling; a named package's members carry no such mark, so only
        // code running in GLOBAL gets them.
        if crate::qualified::is_global_package(Symbol::intern(&self.current_package())) {
            names.extend(
                self.our_vars_iter()
                    .filter_map(|(key, _)| key.strip_prefix('\\'))
                    .filter_map(term_name),
            );
        }
        names.extend(crate::parser::imported_value_term_names());
        // Runtime import aliases survive nested parses and cold module loads,
        // unlike the parser's current import table. Their display spelling
        // distinguishes sigilless terms from ordinary exported variables.
        names.extend(self.module.imported_env_aliases.iter().filter_map(|(key, spelling)| {
            let value = self.env.get_sym(*key)?;
            if matches!(value.view(), ValueView::Package(_)) {
                return None;
            }
            term_name(&spelling.resolve())
        }));
        names.sort_unstable();
        names.dedup();
        names
    }

}
