use super::*;
use crate::symbol::Symbol;

impl Compiler {
    fn normalize_dynamic_scope_name(name: &str) -> String {
        name.trim_start_matches(['$', '@', '%', '&']).to_string()
    }

    pub(super) fn push_dynamic_scope_lexical(
        &mut self,
    ) -> (bool, Option<std::collections::HashSet<String>>) {
        (self.dynamic_scope_all, self.dynamic_scope_names.clone())
    }

    pub(super) fn pop_dynamic_scope_lexical(
        &mut self,
        saved: (bool, Option<std::collections::HashSet<String>>),
    ) {
        self.dynamic_scope_all = saved.0;
        self.dynamic_scope_names = saved.1;
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
}
