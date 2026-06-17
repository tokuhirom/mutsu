use super::*;
use crate::symbol::Symbol;

/// Snapshot of the compiler's lexical-scope-sensitive state, saved on block
/// entry and restored on block exit.
pub(super) struct LexicalScopeSnapshot {
    dynamic_scope_all: bool,
    dynamic_scope_names: Option<std::collections::HashSet<String>>,
    constant_vars_in_scope: std::collections::HashSet<String>,
    constant_vars_current_scope: std::collections::HashSet<String>,
}

impl Compiler {
    fn normalize_dynamic_scope_name(name: &str) -> String {
        name.trim_start_matches(['$', '@', '%', '&']).to_string()
    }

    pub(super) fn push_dynamic_scope_lexical(&mut self) -> LexicalScopeSnapshot {
        // `std::mem::take` resets the current-scope constant set: the entered
        // block starts with no constants of its own, so an inner `constant X`
        // may legitimately shadow an outer one without being a redeclaration.
        LexicalScopeSnapshot {
            dynamic_scope_all: self.dynamic_scope_all,
            dynamic_scope_names: self.dynamic_scope_names.clone(),
            constant_vars_in_scope: self.constant_vars_in_scope.clone(),
            constant_vars_current_scope: std::mem::take(&mut self.constant_vars_current_scope),
        }
    }

    pub(super) fn pop_dynamic_scope_lexical(&mut self, saved: LexicalScopeSnapshot) {
        self.dynamic_scope_all = saved.dynamic_scope_all;
        self.dynamic_scope_names = saved.dynamic_scope_names;
        // Constants declared inside the exiting block are `our`-scoped: they stay
        // installed in the package, but their lexical local slot is no longer
        // valid, so drop them from the in-scope set. Subsequent bare-word access
        // then resolves them via GetBareWord (package/global lookup).
        self.constant_vars_in_scope = saved.constant_vars_in_scope;
        self.constant_vars_current_scope = saved.constant_vars_current_scope;
    }

    pub(super) fn apply_dynamic_scope_pragma(&mut self, arg: Option<&Expr>) {
        match arg {
            None => {
                self.dynamic_scope_all = true;
                self.dynamic_scope_names = None;
            }
            Some(Expr::ArrayLiteral(items)) => {
                let mut names = std::collections::HashSet::new();
                for item in items {
                    if let Expr::Literal(Value::Str(s)) = item {
                        names.insert(Self::normalize_dynamic_scope_name(s));
                    }
                }
                self.dynamic_scope_all = false;
                self.dynamic_scope_names = Some(names);
            }
            Some(Expr::Literal(Value::Str(s))) => {
                let mut names = std::collections::HashSet::new();
                names.insert(Self::normalize_dynamic_scope_name(s));
                self.dynamic_scope_all = false;
                self.dynamic_scope_names = Some(names);
            }
            Some(_) => {
                self.dynamic_scope_all = false;
                self.dynamic_scope_names = Some(std::collections::HashSet::new());
            }
        }
    }

    /// Check if a variable name is a dynamic variable with a package-like name (contains ::).
    pub(super) fn is_dynamic_package_var(name: &str) -> bool {
        let stripped = name.trim_start_matches(['$', '@', '%', '&']);
        if let Some(after_star) = stripped.strip_prefix('*') {
            after_star.contains("::")
        } else {
            false
        }
    }

    /// Emit X::Dynamic::Package error for a dynamic variable with :: in name.
    pub(super) fn emit_dynamic_package_error(&mut self, name: &str) {
        let symbol = Self::dynamic_var_symbol(name);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("symbol".to_string(), Value::str(symbol));
        let err = Value::make_instance(Symbol::intern("X::Dynamic::Package"), attrs);
        let idx = self.code.add_constant(err);
        self.code.emit(OpCode::LoadConst(idx));
        self.code.emit(OpCode::Die);
    }

    /// If `name` is a `sub`/`multi sub` declaration of a reserved special-form
    /// operator (one handled directly by the compiler grammar and not
    /// user-overridable), return an X::Syntax::Extension::SpecialForm error value
    /// carrying `category` and `opname`. Returns None for any normal operator
    /// (e.g. `infix:<+>`) or non-operator sub name.
    ///
    /// The reserved set matches Rakudo: `infix:<=>` (assignment), `infix:<:=>`
    /// and `infix:<::=>` (bind), `infix:<~~>` (smartmatch), `prefix:<|>` (flatten).
    pub(super) fn check_special_form_override(name: &str) -> Option<Value> {
        // Split `<category>:<...op...>` into the category and the delimited op.
        let (category, rest) = name.split_once(':')?;
        if !matches!(
            category,
            "prefix" | "infix" | "postfix" | "circumfix" | "postcircumfix"
        ) {
            return None;
        }
        // Strip the angle/French-quote delimiters around the operator name.
        let opname = rest
            .strip_prefix('<')
            .and_then(|s| s.strip_suffix('>'))
            .or_else(|| {
                rest.strip_prefix('\u{ab}')
                    .and_then(|s| s.strip_suffix('\u{bb}'))
            })?
            .trim();
        let reserved = match category {
            "infix" => matches!(opname, "=" | ":=" | "::=" | "~~"),
            "prefix" => opname == "|",
            _ => false,
        };
        if !reserved {
            return None;
        }
        let msg = format!(
            "Cannot override {} operator '{}', as it is a special form handled directly by the compiler",
            category, opname
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("category".to_string(), Value::str(category.to_string()));
        attrs.insert("opname".to_string(), Value::str(opname.to_string()));
        attrs.insert("message".to_string(), Value::str(msg));
        Some(Value::make_instance(
            Symbol::intern("X::Syntax::Extension::SpecialForm"),
            attrs,
        ))
    }

    /// Reconstruct the full symbol name (with sigil) from the internal name.
    pub(super) fn dynamic_var_symbol(name: &str) -> String {
        // If name starts with a sigil (@, %, &), it already has the sigil
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            name.to_string()
        } else if name.starts_with('*') {
            // $* variable — sigil $ was stripped
            format!("${}", name)
        } else {
            format!("${}", name)
        }
    }

    pub(super) fn var_is_dynamic(&self, name: &str) -> bool {
        if self.dynamic_scope_all {
            return true;
        }
        let Some(names) = &self.dynamic_scope_names else {
            return false;
        };
        names.contains(&Self::normalize_dynamic_scope_name(name))
    }

    /// Parse CALLER:: prefix(es) from a variable name.
    /// Returns (bare_name, depth) where depth is the number of CALLER:: levels.
    /// E.g. "CALLER::a" -> ("a", 1), "CALLER::CALLER::a" -> ("a", 2).
    pub(crate) fn parse_caller_prefix(name: &str) -> Option<(String, usize)> {
        let mut remaining = name;
        let mut depth = 0;
        while let Some(rest) = remaining.strip_prefix("CALLER::") {
            depth += 1;
            remaining = rest;
        }
        if depth > 0 {
            Some((remaining.to_string(), depth))
        } else {
            None
        }
    }

    /// Parse `OUTER::` / `OUTER::OUTER::` prefix from a variable name.
    /// Returns (bare_name, depth) where depth is the number of OUTER:: prefixes.
    pub(crate) fn parse_outer_prefix(name: &str) -> Option<(String, usize)> {
        let mut remaining = name;
        let mut depth = 0;
        while let Some(rest) = remaining.strip_prefix("OUTER::") {
            depth += 1;
            remaining = rest;
        }
        if depth > 0 {
            Some((remaining.to_string(), depth))
        } else {
            None
        }
    }
}
