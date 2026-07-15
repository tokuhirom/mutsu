use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) const LAZY_GATHER_TAKE_LIMIT_SIGNAL: &str =
        "__mutsu_lazy_gather_take_limit_reached__";

    pub(crate) fn is_stub_method_body(body: &[Stmt]) -> bool {
        let filtered: Vec<_> = body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        filtered.len() == 1
            && matches!(
                filtered[0],
                Stmt::Expr(Expr::Call { name, .. })
                    if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn"
            )
    }

    pub(super) fn resolve_function(&self, name: &str) -> Option<Arc<FunctionDef>> {
        if name.contains("::") {
            // Try direct lookup first
            if let Some(def) = self
                .registry()
                .functions
                .get(&Symbol::intern(name))
                .cloned()
            {
                return Some(def);
            }
            // If not found, try qualifying with the current package prefix
            // when the prefix package is visible in the current scope.
            if self.current_package() != "GLOBAL" {
                let prefix_visible = if let Some((pkg_prefix, _)) = name.rsplit_once("::") {
                    self.env.get(pkg_prefix).is_some()
                        || self
                            .env
                            .get(&format!("{}::{}", self.current_package(), pkg_prefix))
                            .is_some()
                } else {
                    false
                };
                if prefix_visible {
                    let qualified = format!("{}::{}", self.current_package(), name);
                    if let Some(def) = self
                        .registry()
                        .functions
                        .get(&Symbol::intern(&qualified))
                        .cloned()
                    {
                        return Some(def);
                    }
                }
            }
            return None;
        }
        let local = format!("{}::{}", self.current_package(), name);
        self.registry()
            .functions
            .get(&Symbol::intern(&local))
            .cloned()
            .or_else(|| {
                self.registry()
                    .functions
                    .get(&Symbol::intern(&format!("GLOBAL::{}", name)))
                    .cloned()
            })
    }

    pub(super) fn insert_token_def(&mut self, name: &str, def: FunctionDef, multi: bool) {
        let key = Symbol::intern(&format!("{}::{}", self.current_package(), name));
        let def = std::sync::Arc::new(def);
        if multi {
            self.registry_mut()
                .token_defs
                .entry(key)
                .or_default()
                .push(def);
        } else {
            self.registry_mut().token_defs.insert(key, vec![def]);
        }
        // Regex parses may fold token bodies in (parse_combined_class); a new /
        // redefined token must invalidate those cached parses.
        crate::runtime::regex_parse::TOKEN_DEFS_GEN
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }

    /// Collect token defs for a given scope (exact + :sym<> variants).
    /// Candidate identity of a token def: the def's own name when it is a
    /// proto candidate of `token_name` (`statement:sym<expr>`), else the bare
    /// `token_name`.
    pub(crate) fn token_def_identity(def_name: &str, token_name: &str) -> String {
        if def_name
            .strip_prefix(token_name)
            .is_some_and(|rest| rest.starts_with(":sym"))
        {
            def_name.to_string()
        } else {
            token_name.to_string()
        }
    }

    /// Collect token defs for `name` in `scope`, deduplicating by candidate
    /// identity (the bare `name` for an exact def, `name:sym<X>` for a proto
    /// candidate) across successive calls via `seen`. Walking an MRO with this
    /// merges proto candidates from every class — a derived grammar that adds
    /// `rule statement:sym<repeat>` keeps the base grammar's candidates — while
    /// a same-identity redefinition in a more-derived class still overrides its
    /// ancestor's (first hit wins).
    pub(crate) fn collect_token_defs_for_scope_dedup(
        &self,
        scope: &str,
        name: &str,
        defs: &mut Vec<std::sync::Arc<FunctionDef>>,
        seen: &mut std::collections::HashSet<String>,
    ) {
        let exact_key = format!("{scope}::{name}");
        if !seen.contains(name)
            && let Some(exact) = self.registry().token_defs.get(&Symbol::intern(&exact_key))
        {
            defs.extend(exact.clone());
            seen.insert(name.to_string());
        }
        let scope_prefix_len = scope.len() + 2;
        let sym_prefix_angle = format!("{scope}::{name}:sym<");
        let sym_prefix_french = format!("{scope}::{name}:sym\u{ab}");
        let mut sym_keys: Vec<String> = self
            .registry()
            .token_defs
            .keys()
            .map(|key| key.resolve())
            .filter(|key| key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french))
            .collect();
        sym_keys.sort();
        for key in &sym_keys {
            let identity = &key[scope_prefix_len..];
            if seen.contains(identity) {
                continue;
            }
            if let Some(sym_defs) = self.registry().token_defs.get(&Symbol::intern(key)) {
                defs.extend(sym_defs.clone());
                seen.insert(identity.to_string());
            }
        }
    }

    pub(crate) fn collect_token_defs_for_scope(
        &self,
        scope: &str,
        name: &str,
        defs: &mut Vec<std::sync::Arc<FunctionDef>>,
    ) {
        let exact_key = format!("{scope}::{name}");
        if let Some(exact) = self.registry().token_defs.get(&Symbol::intern(&exact_key)) {
            defs.extend(exact.clone());
        }
        let sym_prefix_angle = format!("{scope}::{name}:sym<");
        let sym_prefix_french = format!("{scope}::{name}:sym\u{ab}");
        let mut sym_keys: Vec<String> = self
            .registry()
            .token_defs
            .keys()
            .map(|key| key.resolve())
            .filter(|key| key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french))
            .collect();
        sym_keys.sort();
        for key in &sym_keys {
            if let Some(sym_defs) = self.registry().token_defs.get(&Symbol::intern(key)) {
                defs.extend(sym_defs.clone());
            }
        }
    }

    /// Get parent class names without requiring &mut self (no MRO caching).
    pub(crate) fn class_parents_readonly(&self, class_name: &str) -> Vec<String> {
        if let Some(class_def) = self.registry().classes.get(class_name) {
            if !class_def.mro.is_empty() {
                return class_def.mro.iter().map(|s| s.resolve()).collect();
            }
            return class_def.parents.clone();
        }
        vec![]
    }

    /// Walk MRO (read-only) collecting ancestor names.
    pub(crate) fn mro_readonly(&self, class_name: &str) -> Vec<String> {
        // Fast path: when the registry already holds the precomputed C3 MRO it
        // *is* the full ancestor list in order (its first element is the class
        // itself). The BFS below would only re-derive exactly that — and worse,
        // it calls `class_parents_readonly` per node, each of which clones the
        // whole `class_def.mro`, so the walk is O(N^2) string clones for an
        // N-deep hierarchy. `mro_readonly` runs on hot paths (constructor
        // BUILD/TWEAK/smiley probes, dispatch), so return the cached MRO with a
        // single clone instead. Equivalence: the BFS seeds `result` with
        // `[class_name]`, the first pop expands `class_parents_readonly(class_name)
        // == class_def.mro`, and every later node is already visited, so the
        // result is exactly `class_def.mro`.
        if let Some(class_def) = self.registry().classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return class_def.mro.iter().map(|s| s.resolve()).collect();
        }
        // Fallback: built-in/unregistered classes, or a registered class whose
        // MRO has not been computed yet (parents-only walk).
        let mut result = vec![class_name.to_string()];
        let mut visited = std::collections::HashSet::new();
        visited.insert(class_name.to_string());
        let mut queue = vec![class_name.to_string()];
        while let Some(current) = queue.pop() {
            for parent in self.class_parents_readonly(&current) {
                if visited.insert(parent.clone()) {
                    result.push(parent.clone());
                    queue.push(parent);
                }
            }
        }
        result
    }

    pub(crate) fn resolve_token_defs(
        &self,
        name: &str,
    ) -> Option<Vec<std::sync::Arc<FunctionDef>>> {
        if name.contains("::") {
            let mut defs = Vec::new();
            if let Some(exact) = self.registry().token_defs.get(&Symbol::intern(name)) {
                defs.extend(exact.clone());
            }
            let sym_prefix_angle = format!("{name}:sym<");
            let sym_prefix_french = format!("{name}:sym\u{ab}");
            let mut sym_keys: Vec<String> = self
                .registry()
                .token_defs
                .keys()
                .map(|key| key.resolve())
                .filter(|key| {
                    key.starts_with(&sym_prefix_angle) || key.starts_with(&sym_prefix_french)
                })
                .collect();
            sym_keys.sort();
            for key in &sym_keys {
                if let Some(sym_defs) = self.registry().token_defs.get(&Symbol::intern(key)) {
                    defs.extend(sym_defs.clone());
                }
            }
            // Walk the MRO of the package part, merging proto candidates from
            // every ancestor (dedup by candidate identity, derived-first).
            if let Some(pos) = name.rfind("::") {
                let pkg = &name[..pos];
                let token_name = &name[pos + 2..];
                let mut seen: std::collections::HashSet<String> = defs
                    .iter()
                    .map(|d| Self::token_def_identity(&d.name.resolve(), token_name))
                    .collect();
                for ancestor in self.mro_readonly(pkg) {
                    if ancestor == pkg {
                        continue; // already checked
                    }
                    self.collect_token_defs_for_scope_dedup(
                        &ancestor, token_name, &mut defs, &mut seen,
                    );
                }
            }
            return if defs.is_empty() { None } else { Some(defs) };
        }
        let mut defs = Vec::new();
        // Check current package and its MRO, merging proto candidates.
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        let scopes_to_check = self.mro_readonly(&self.current_package());
        for scope in &scopes_to_check {
            self.collect_token_defs_for_scope_dedup(scope, name, &mut defs, &mut seen);
        }
        // Also check GLOBAL
        if defs.is_empty() {
            self.collect_token_defs_for_scope("GLOBAL", name, &mut defs);
        }
        if defs.is_empty() { None } else { Some(defs) }
    }

    pub(crate) fn has_proto_token(&self, name: &str) -> bool {
        if name.contains("::") {
            if self.registry().proto_tokens.contains(name) {
                return true;
            }
            // Walk MRO for qualified names
            if let Some(pos) = name.rfind("::") {
                let pkg = &name[..pos];
                let token_name = &name[pos + 2..];
                for ancestor in self.mro_readonly(pkg) {
                    if ancestor == pkg {
                        continue;
                    }
                    if self
                        .registry()
                        .proto_tokens
                        .contains(&format!("{ancestor}::{token_name}"))
                    {
                        return true;
                    }
                }
            }
            return false;
        }
        // Check current package MRO
        for scope in self.mro_readonly(&self.current_package()) {
            if self
                .registry()
                .proto_tokens
                .contains(&format!("{scope}::{name}"))
            {
                return true;
            }
        }
        self.registry()
            .proto_tokens
            .contains(&format!("GLOBAL::{}", name))
    }
}
