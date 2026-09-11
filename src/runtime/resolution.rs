use super::*;
use crate::symbol::Symbol;

/// Monotonic counter stamped onto every `FunctionDef` at registration time to
/// record declaration order. Two consumers rely on it:
///
/// - Rakudo breaks an equal-length Longest-Token-Match tie between proto
///   `token`/`rule` candidates by declaration order, so the resolver sorts
///   sym-variant candidates by this value instead of alphabetically.
/// - Rakudo breaks an equal-*narrowness* multi-dispatch tie by declaration
///   order too (`multi f(:$a)` before `multi f(Str :$a)` wins `f(a => "x")`),
///   so `sort_candidates_by_specificity` uses it as its final key.
///
/// The counter is global rather than per-`(package, name)`: only the relative
/// order *within* one candidate set matters, and every candidate of a set is
/// registered by the same top-to-bottom pass over its unit.
static NEXT_DECL_ORDER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

thread_local! {
    /// Memo for [`Interpreter::proto_variant_keys_sorted`]: prefix -> the
    /// `TOKEN_DEFS_GEN` the answer was computed under, and the answer.
    ///
    /// The scan it memoizes is O(all registered token keys) and runs once per
    /// `<subrule>` resolution, for a handful of distinct prefixes — 8.5% of a
    /// YAML-parse profile went into re-deriving the same few lists
    /// ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)). Entries carry
    /// the generation they were built under, the same invalidation discipline
    /// as `PARSED_TOKEN_CANDIDATES` and `REGEX_PARSE_CACHE` — anything that
    /// (re)defines, restores or wholesale-replaces `token_defs` bumps it.
    #[allow(clippy::type_complexity)]
    static PROTO_VARIANT_KEYS: std::cell::RefCell<
        rustc_hash::FxHashMap<String, (u64, std::sync::Arc<Vec<Symbol>>)>,
    > = std::cell::RefCell::new(rustc_hash::FxHashMap::default());
}

/// Take the next declaration-order stamp. Called from every `FunctionDef`
/// construction site in the registration paths.
pub(crate) fn next_decl_order() -> u64 {
    NEXT_DECL_ORDER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

/// True when `rest` (the portion of a `token_defs` key AFTER the proto's
/// fully-qualified name) begins a proto-regex variant adverb. Three spellings
/// register a candidate under the proto:
///
/// | spelling | binds `<sym>`? |
/// | --- | --- |
/// | `:sym<int>` / `:sym«int»` | yes |
/// | `:<int>` / `:«int»` (shorthand) | no |
/// | `:int` (bare identifier adverb) | no |
///
/// A resolver that recognized only `:sym<` dropped every `token element:<int>
/// {...}` candidate (YAMLish's `Schema::JSON`); one that additionally missed
/// the bare form dropped every `token gap:spacer {...}` candidate
/// (`Config::TOML`'s grammar, which names all of its alternatives that way).
///
/// `::` is a package separator, never an adverb, so it must not count.
pub(crate) fn is_proto_variant_suffix(rest: &str) -> bool {
    let Some(after) = rest.strip_prefix(':') else {
        return false;
    };
    after.starts_with('<')
        || after.starts_with('\u{ab}')
        || after.starts_with(|c: char| c.is_alphabetic() || c == '_')
}

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

    pub(crate) fn resolve_function(&self, name: &str) -> Option<Arc<FunctionDef>> {
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
        // A compunit-private top-level routine of the unit currently executing
        // wins over any package entry: it is a lexical of that compunit, and
        // the shared registry may well hold an unrelated same-named routine
        // belonging to the scope that loaded it. See
        // `runtime/unit_private_routines.rs`.
        if let Some(def) = self.unit_private_routine(name) {
            return Some(def);
        }
        let cur_pkg = self.current_package();
        // Innermost package first, then each enclosing one, ending at GLOBAL.
        for pkg in self.bare_name_packages() {
            let key = Symbol::intern(&format!("{}::{}", pkg, name));
            if let Some(def) = self.registry().functions.get(&key).cloned() {
                // A prelude splice is registered under `GLOBAL::` for every
                // package to reach, but is lexical to the compunits it was
                // spliced into — see `prelude_visible_here`.
                if !self.prelude_visible_here(key) {
                    continue;
                }
                return Some(def);
            }
        }
        // A module's tagged export (`sub foo is export(:tag)`) that the importing
        // program did not request is hidden by renaming `GLOBAL::foo` to
        // `MOD::foo` (see the hide step in `runtime_module.rs`). But MOD's own
        // routines — a grammar action method running under `MOD::Grammar`, or an
        // ordinary method of `MOD::SomeClass` — still refer to `foo` by its bare
        // name and relied on the now-removed GLOBAL entry. Restore visibility by
        // walking up the enclosing namespace of the code that is running: the
        // current package (set to the grammar's package during action dispatch)
        // and the invocant's class (ordinary method bodies run under GLOBAL, so
        // `self`'s class is the only module signal available). For each ancestor
        // namespace that is a loaded module owning an export of this name,
        // resolve `MOD::foo`. This fires only for genuinely-hidden owned exports,
        // so an unrelated `Foo::bar` in a sibling package block is never
        // spuriously resolved.
        if !self.module_owned_exports.is_empty() {
            if let Some(def) = self.resolve_hidden_owned_export(&cur_pkg, name) {
                return Some(def);
            }
            if let Some(ValueView::Instance { class_name, .. }) =
                self.env.get("self").map(Value::view)
            {
                let cn = class_name.resolve();
                if let Some(def) = self.resolve_hidden_owned_export(&cn, name) {
                    return Some(def);
                }
            }
        }
        None
    }

    /// Walk up the namespace segments of `context` looking for a loaded module
    /// that owns an export named `name`, and return the hidden `MOD::name`
    /// routine if one is registered. See `resolve_function`.
    fn resolve_hidden_owned_export(&self, context: &str, name: &str) -> Option<Arc<FunctionDef>> {
        let mut probe = context;
        loop {
            if self
                .module_owned_exports
                .get(probe)
                .is_some_and(|owned| owned.contains_key(name))
                && let Some(def) = self
                    .registry()
                    .functions
                    .get(&Symbol::intern(&format!("{}::{}", probe, name)))
                    .cloned()
            {
                return Some(def);
            }
            match probe.rsplit_once("::") {
                Some((outer, _)) => probe = outer,
                None => return None,
            }
        }
    }

    pub(super) fn insert_token_def(&mut self, name: &str, mut def: FunctionDef, multi: bool) {
        let key = Symbol::intern(&format!("{}::{}", self.current_package(), name));
        // Stamp declaration order: grammar bodies register their `token`s
        // top-to-bottom, so a monotonic counter captures declaration order,
        // which is Rakudo's tie-break for an equal-length LTM tie between
        // proto candidates (`token pp:sym<**>` before `token pp:sym<m>`).
        def.decl_order = next_decl_order();
        crate::runtime::regex::regex_dynparams::note_token_def_params(&def.param_defs);
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

    /// Put `token_defs` back to a snapshot taken before a `:my token …` inside
    /// a regex could add to it, invalidating every generation-keyed memo built
    /// while the extra definition was visible (`PROTO_VARIANT_KEYS`,
    /// `PARSED_TOKEN_CANDIDATES`, `REGEX_PARSE_CACHE`, the call-graph tables).
    /// The declaration itself bumped the generation on the way in; the removal
    /// has to bump it on the way out for the same reason.
    pub(crate) fn restore_token_defs(&mut self, saved: crate::runtime::registry::TokenDefsMap) {
        self.registry_mut().token_defs = saved;
        crate::runtime::regex_parse::TOKEN_DEFS_GEN
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }

    /// The lowest declaration order among the candidates registered under one
    /// `token_defs` key — Rakudo's LTM tie-break. Takes the key as a `Symbol`
    /// because that is how `token_defs` is keyed, so a scan never re-interns a
    /// key it just read out of the map.
    pub(crate) fn token_key_decl_order_sym(&self, key: Symbol) -> u64 {
        self.registry()
            .token_defs
            .get(&key)
            .and_then(|defs| defs.iter().map(|d| d.decl_order).min())
            .unwrap_or(u64::MAX)
    }

    /// Every `token_defs` key that spells a `:sym<…>` proto variant of
    /// `prefix`, sorted by declaration order (Rakudo's LTM tie-break) and
    /// falling back to alphabetical order for keys that share a declaration
    /// order or are unregistered, so the result stays deterministic.
    ///
    /// This walks the whole `token_defs` key set, and it is walked once per
    /// `<subrule>` resolution, so it must not allocate per key: a callgrind
    /// profile of a YAML parse had the five hand-rolled copies of this scan
    /// materializing a `String` for every key (`Symbol::resolve`) just to test
    /// a prefix and throw it away — 4.6M allocations, ~15% of the whole
    /// program ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
    /// `Symbol::as_str` hands out the interned `&'static str` instead, and the
    /// key symbols come back as symbols so no call site re-interns them.
    pub(crate) fn proto_variant_keys_sorted(&self, prefix: &str) -> std::sync::Arc<Vec<Symbol>> {
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        if let Some(hit) = PROTO_VARIANT_KEYS.with(|c| {
            c.borrow()
                .get(prefix)
                .filter(|(entry_gen, _)| *entry_gen == generation)
                .map(|(_, keys)| keys.clone())
        }) {
            return hit;
        }
        let keys = std::sync::Arc::new(self.proto_variant_keys_sorted_uncached(prefix));
        PROTO_VARIANT_KEYS.with(|c| {
            c.borrow_mut()
                .insert(prefix.to_owned(), (generation, keys.clone()));
        });
        keys
    }

    fn proto_variant_keys_sorted_uncached(&self, prefix: &str) -> Vec<Symbol> {
        let mut keys: Vec<Symbol> = self
            .registry()
            .token_defs
            .keys()
            .copied()
            .filter(|key| {
                key.as_str()
                    .strip_prefix(prefix)
                    .is_some_and(is_proto_variant_suffix)
            })
            .collect();
        keys.sort_by(|a, b| {
            self.token_key_decl_order_sym(*a)
                .cmp(&self.token_key_decl_order_sym(*b))
                .then_with(|| a.as_str().cmp(b.as_str()))
        });
        keys
    }

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
        for &key in self.proto_variant_keys_sorted(&exact_key).iter() {
            let identity = &key.as_str()[scope_prefix_len..];
            if seen.contains(identity) {
                continue;
            }
            if let Some(sym_defs) = self.registry().token_defs.get(&key) {
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
        for &key in self.proto_variant_keys_sorted(&exact_key).iter() {
            if let Some(sym_defs) = self.registry().token_defs.get(&key) {
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
            for &key in self.proto_variant_keys_sorted(name).iter() {
                if let Some(sym_defs) = self.registry().token_defs.get(&key) {
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
