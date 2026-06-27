//! Symbolic stash member lookup and package/indirect-type-name resolution.
use super::*;

impl Interpreter {
    fn stash_symbol_key_from_env_tail(rest: &str) -> String {
        if rest.starts_with('$')
            || rest.starts_with('@')
            || rest.starts_with('%')
            || rest.starts_with('&')
        {
            return rest.to_string();
        }
        if rest.contains("::") || rest.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
            return rest.to_string();
        }
        format!("${rest}")
    }

    fn stash_member_tail<'a>(key: &'a str, package: &str) -> Option<&'a str> {
        let package = package.trim_end_matches("::");
        if package == "GLOBAL" {
            return Some(key);
        }
        let direct = format!("{package}::");
        if let Some(rest) = key.strip_prefix(&direct) {
            return Some(rest);
        }
        let needle = format!("::{package}::");
        if let Some(idx) = key.rfind(&needle) {
            let start = idx + needle.len();
            return Some(&key[start..]);
        }
        None
    }

    fn make_stash_instance(package: &str, symbols: HashMap<String, Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(package.to_string()));
        attrs.insert("symbols".to_string(), Value::hash(symbols));
        Value::make_instance(Symbol::intern("Stash"), attrs)
    }

    fn package_export_tag_parts(package: &str) -> Option<(&str, &str)> {
        let (module, rest) = package.split_once("::EXPORT::")?;
        if module.is_empty() || rest.is_empty() || rest.contains("::") {
            return None;
        }
        Some((module, rest))
    }

    fn package_export_module(package: &str) -> Option<&str> {
        package.strip_suffix("::EXPORT")
    }

    fn qualify_stash_name(package: &str, symbol: &str) -> String {
        let package = package.trim_end_matches("::");
        if package.is_empty() || package == "GLOBAL" {
            symbol.to_string()
        } else {
            format!("{package}::{symbol}")
        }
    }

    fn normalize_stash_package(package: &str) -> String {
        let trimmed = package.trim_end_matches("::");
        if let Some(inner) = trimmed
            .strip_prefix("GLOBAL[")
            .and_then(|s| s.strip_suffix(']'))
        {
            inner.to_string()
        } else {
            trimmed.to_string()
        }
    }

    fn has_package_members(&self, package: &str) -> bool {
        let prefix = format!("{package}::");
        self.env.keys().any(|k| k.starts_with(&prefix))
            || self
                .registry()
                .functions
                .keys()
                .any(|k| k.resolve().starts_with(&prefix))
            || self
                .registry()
                .classes
                .keys()
                .any(|k| k.starts_with(&prefix))
            || self.exported_subs.contains_key(package)
            || self.exported_vars.contains_key(package)
    }

    fn stash_lookup_symbol(stash: &Value, key: &str) -> Option<Value> {
        let Value::Instance {
            class_name,
            attributes,
            ..
        } = stash
        else {
            return None;
        };
        if class_name != "Stash" {
            return None;
        }
        let map = attributes.as_map();
        let Value::Hash(symbols) = map.get("symbols")? else {
            return None;
        };
        if let Some(value) = symbols.get(key) {
            return Some(value.clone());
        }
        if !key.starts_with('$')
            && !key.starts_with('@')
            && !key.starts_with('%')
            && !key.starts_with('&')
        {
            let scalar = format!("${key}");
            if let Some(value) = symbols.get(&scalar) {
                return Some(value.clone());
            }
        }
        None
    }

    fn no_such_symbol_failure(name: &str) -> Value {
        let mut ex_attrs = HashMap::new();
        ex_attrs.insert(
            "message".to_string(),
            Value::str(format!("No such symbol '{name}'")),
        );
        // A failed symbolic lookup (`::('NoSuchName')`, `::('')`) is
        // X::NoSuchSymbol in Raku, not a bare X::AdHoc. `symbol` carries the name.
        ex_attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        let exception = Value::make_instance(Symbol::intern("X::NoSuchSymbol"), ex_attrs);
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::Bool(false));
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    pub(crate) fn resolve_indirect_type_name(&self, name: &str) -> Value {
        if name.is_empty() {
            return Self::no_such_symbol_failure(name);
        }
        // Symbols loaded via `$*REPO.need(...)` stay invisible to `::('Name')`
        // until they are merged into GLOBAL with `merge-symbols`.
        if self.cur_repo.pending_global_symbols.contains(name) {
            return Self::no_such_symbol_failure(name);
        }
        // Pseudo-package names like MY, CORE, OUTER, CALLER, etc. should
        // resolve to Package values so that .WHO can produce the stash.
        if Self::is_pseudo_package_name(name) {
            return Value::Package(Symbol::intern(name));
        }
        if let Some(code_name) = name.strip_prefix('&') {
            let val = self.resolve_code_var(code_name);
            // When the code variable is not found via ::('&name'), return a
            // Failure (like Raku's X::NoSuchSymbol) so that attempting to use
            // the result throws an exception.
            if matches!(val, Value::Nil) {
                return Self::no_such_symbol_failure(name);
            }
            return val;
        }
        // Scalars are stored without the `$` sigil in the env; strip it for lookup.
        if let Some(bare) = name.strip_prefix('$')
            && let Some(value) = self.env.get(bare)
            && !matches!(value, Value::Nil)
        {
            return value.clone();
        }
        if let Some(value) = self.env.get(name)
            && !matches!(value, Value::Nil)
            // Skip `my`-scoped package items for indirect type lookup (::())
            // since they should not be visible outside their declaring scope.
            && !self.is_my_scoped_package_item(name)
        {
            return value.clone();
        }
        // Fallback: check persistent `our`-scoped variables (constants, `our` decls)
        // which may have been removed from the lexical env by block-scope restoration.
        if let Some(bare) = name.strip_prefix('$')
            && let Some(value) = self.our_vars.get(bare)
            && !matches!(value, Value::Nil)
        {
            return value.clone();
        }
        if let Some(value) = self.our_vars.get(name)
            && !matches!(value, Value::Nil)
        {
            return value.clone();
        }
        // Look up well-known numerical constants
        match name {
            "e" | "\u{1D452}" => return Value::Num(std::f64::consts::E),
            "pi" => return Value::Num(std::f64::consts::PI),
            "tau" | "\u{03C4}" => return Value::Num(std::f64::consts::TAU),
            _ => {}
        }
        if !self.method_class_stack.is_empty() && self.loaded_modules.contains(name) {
            return Value::Package(Symbol::intern(name));
        }

        // Check if the full compound name (e.g. "IO::Path") is a known type
        // before splitting on "::".
        if name.contains("::")
            && (crate::runtime::utils::is_known_compound_type(name)
                || self.has_class(name)
                || self.is_role(name))
        {
            return Value::Package(Symbol::intern(name));
        }

        let mut parts = name.split("::").filter(|part| !part.is_empty());
        let Some(first) = parts.next() else {
            return Self::no_such_symbol_failure(name);
        };

        let mut current = if let Some(value) = self.env.get(first)
            && !matches!(value, Value::Nil)
        {
            value.clone()
        } else if crate::runtime::utils::is_known_type_constraint(first)
            || (name.contains("::")
                && (self.has_package_members(first)
                    || self.has_class(first)
                    || self.is_role(first)))
        {
            Value::Package(Symbol::intern(first))
        } else {
            return Self::no_such_symbol_failure(name);
        };

        for part in parts {
            current = match &current {
                Value::Package(package) => {
                    let stash = self.package_stash_value(&package.resolve());
                    if let Some(value) = Self::stash_lookup_symbol(&stash, part) {
                        value
                    } else {
                        return Self::no_such_symbol_failure(name);
                    }
                }
                Value::Instance { class_name, .. } if class_name == "Stash" => {
                    if let Some(value) = Self::stash_lookup_symbol(&current, part) {
                        value
                    } else {
                        return Self::no_such_symbol_failure(name);
                    }
                }
                _ => return Self::no_such_symbol_failure(name),
            };
        }

        current
    }

    pub(crate) fn package_stash_value(&self, package: &str) -> Value {
        let package_name = Self::normalize_stash_package(package);

        // PROCESS:: pseudo-package: exposes process-level dynamic variables
        // like $*PROGRAM, $*PID, %*ENV, @*ARGS, etc.
        // PROCESS::<$PROGRAM> looks up key "$PROGRAM" in the stash.
        if package_name == "PROCESS" {
            let mut symbols: HashMap<String, Value> = HashMap::new();
            for (key, val) in self.env.iter() {
                let key_s = key.resolve();
                // Dynamic variables stored as "*NAME" map to "$NAME" in the stash
                if let Some(name) = key_s.strip_prefix('*')
                    && !name.contains("::")
                {
                    symbols.insert(format!("${name}"), val.clone());
                }
                // Hash dynamic variables stored as "%*NAME" map to "%NAME" in the stash
                if let Some(name) = key_s.strip_prefix("%*")
                    && !name.contains("::")
                {
                    symbols.insert(format!("%{name}"), val.clone());
                }
                // Array dynamic variables stored as "@*NAME" map to "@NAME" in the stash
                if let Some(name) = key_s.strip_prefix("@*")
                    && !name.contains("::")
                {
                    symbols.insert(format!("@{name}"), val.clone());
                }
            }
            return Self::make_stash_instance(package, symbols);
        }

        if let Some((module, tag)) = Self::package_export_tag_parts(package) {
            let mut symbols: HashMap<String, Value> = HashMap::new();
            if let Some(subs) = self.exported_subs.get(module) {
                for (name, tags) in subs {
                    if tag != "ALL" && !tags.contains(tag) {
                        continue;
                    }
                    let fq = format!("{module}::{name}");
                    symbols.insert(format!("&{name}"), self.resolve_code_var(&fq));
                }
            }
            if let Some(vars) = self.exported_vars.get(module) {
                for (name, tags) in vars {
                    if tag != "ALL" && !tags.contains(tag) {
                        continue;
                    }
                    let fq = format!("{module}::{name}");
                    let val = self
                        .env
                        .get(&fq)
                        .cloned()
                        .or_else(|| self.env.get(name).cloned())
                        .unwrap_or(Value::Nil);
                    symbols.insert(name.clone(), val);
                }
            }
            return Self::make_stash_instance(package, symbols);
        }

        if let Some(module) = Self::package_export_module(&package_name) {
            let mut tags = std::collections::BTreeSet::new();
            if let Some(subs) = self.exported_subs.get(module) {
                for tagset in subs.values() {
                    tags.extend(tagset.iter().cloned());
                }
            }
            if let Some(vars) = self.exported_vars.get(module) {
                for tagset in vars.values() {
                    tags.extend(tagset.iter().cloned());
                }
            }
            let mut symbols: HashMap<String, Value> = HashMap::new();
            for tag in tags {
                symbols.insert(
                    tag.clone(),
                    Value::Package(Symbol::intern(&Self::qualify_stash_name(
                        &package_name,
                        &tag,
                    ))),
                );
            }
            return Self::make_stash_instance(package, symbols);
        }

        let mut symbols: HashMap<String, Value> = HashMap::new();

        for (key, val) in self.env.iter() {
            let key_s = key.resolve();
            if key_s.starts_with("__mutsu_callable_id::") {
                continue;
            }
            if (package_name == "MY" || package_name == "GLOBAL")
                && self.should_hide_from_my_global_stash(&key_s)
            {
                continue;
            }
            // Skip env entries hidden from package stash lookups (transitive deps)
            if package_name != "MY"
                && package_name != "GLOBAL"
                && self.package_stash_hidden.contains(&key_s)
            {
                continue;
            }
            // Skip my-scoped items (they should not appear in the package stash)
            if self.is_my_scoped_package_item(&key_s) {
                continue;
            }
            if let Some(rest) = Self::stash_member_tail(&key_s, &package_name) {
                let stash_key = Self::stash_symbol_key_from_env_tail(rest);
                symbols.insert(stash_key, val.clone());
            }
        }

        for (key, def) in &self.registry().functions {
            let key_s = key.resolve();
            let Some(rest) = Self::stash_member_tail(&key_s, &package_name) else {
                continue;
            };
            let base = rest.split('/').next().unwrap_or(rest);
            if base.is_empty() || base.contains("::") || base.contains(':') {
                continue;
            }
            // Skip my-scoped subs (they should not appear in the package stash)
            let fq_base = format!("{}::{}", package_name, base);
            if self.is_my_scoped_package_item(&fq_base) {
                continue;
            }
            symbols
                .entry(format!("&{base}"))
                .or_insert_with(|| Value::Routine {
                    package: def.package,
                    name: def.name,
                    is_regex: false,
                });
        }

        for class_name in self.registry().classes.keys() {
            let class_short = class_name
                .rsplit_once("::")
                .map(|(_, short)| short)
                .unwrap_or(class_name.as_str());
            if (package_name == "MY" || package_name == "GLOBAL")
                && (self.need_hidden_classes.contains(class_name)
                    || self.need_hidden_classes.contains(class_short))
            {
                continue;
            }
            // Skip classes hidden from package stash lookups (transitive deps)
            if package_name != "MY"
                && package_name != "GLOBAL"
                && self.package_stash_hidden.contains(class_name)
            {
                continue;
            }
            // Skip my-scoped classes (they should not appear in the package stash)
            if self.is_my_scoped_package_item(class_name) {
                continue;
            }
            let Some(rest) = Self::stash_member_tail(class_name, &package_name) else {
                continue;
            };
            if rest.is_empty() {
                continue;
            }
            if let Some((head, _)) = rest.split_once("::") {
                symbols.entry(head.to_string()).or_insert_with(|| {
                    Value::Package(Symbol::intern(&Self::qualify_stash_name(
                        &package_name,
                        head,
                    )))
                });
                continue;
            }
            symbols.entry(rest.to_string()).or_insert_with(|| {
                Value::Package(Symbol::intern(&Self::qualify_stash_name(
                    &package_name,
                    rest,
                )))
            });
        }
        for role_name in self.registry().roles.keys() {
            // Skip roles hidden from package stash lookups (transitive deps)
            if package_name != "MY"
                && package_name != "GLOBAL"
                && self.package_stash_hidden.contains(role_name)
            {
                continue;
            }
            let Some(rest) = Self::stash_member_tail(role_name, &package_name) else {
                continue;
            };
            if rest.is_empty() {
                continue;
            }
            if let Some((head, _)) = rest.split_once("::") {
                symbols.entry(head.to_string()).or_insert_with(|| {
                    Value::Package(Symbol::intern(&Self::qualify_stash_name(
                        &package_name,
                        head,
                    )))
                });
                continue;
            }
            symbols.entry(rest.to_string()).or_insert_with(|| {
                Value::Package(Symbol::intern(&Self::qualify_stash_name(
                    &package_name,
                    rest,
                )))
            });
        }

        if self.exported_subs.contains_key(&package_name)
            || self.exported_vars.contains_key(&package_name)
        {
            symbols.entry("EXPORT".to_string()).or_insert_with(|| {
                Value::Package(Symbol::intern(&Self::qualify_stash_name(
                    &package_name,
                    "EXPORT",
                )))
            });
        }

        Self::make_stash_instance(package, symbols)
    }
}
