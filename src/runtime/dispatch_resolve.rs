use super::*;

/// The *base name* a registry function key stands for: the key minus its
/// package prefix and its arity/type suffix. `"Pkg::foo/2:Int,Str"` → `"foo"`,
/// `"GLOBAL::infix:</>/2"` → `"infix:</>"`, `"foo"` → `"foo"`.
///
/// The arity suffix is the RIGHTMOST `/` immediately followed by an ASCII
/// digit (`/2`, `/3:Int`, `/1__m…`); an operator name's own `/` (as in
/// `infix:</>`) is never digit-followed, so it survives. The same extraction
/// is applied to both registry keys and query names, so any exotic spelling
/// degrades to a consistent (never wrong) bucket.
fn function_key_base_name(key: &str) -> &str {
    let bytes = key.as_bytes();
    let mut end = key.len();
    let mut i = key.len();
    while i > 1 {
        i -= 1;
        if bytes[i - 1] == b'/' && bytes[i].is_ascii_digit() {
            end = i - 1;
            break;
        }
    }
    let head = &key[..end];
    match head.rfind("::") {
        Some(p) => &head[p + 2..],
        None => head,
    }
}

impl Interpreter {
    /// Whether ANY registered function key carries `name`'s base name — a
    /// cheap negative gate for [`Self::resolve_function_with_types`]. When
    /// this is `false`, no lookup pattern in the resolver (qualified, typed,
    /// arity-keyed, flexible-arity, package-searched) can possibly match, so
    /// the resolver returns `None` without walking the registry. Memoized per
    /// name in `fn_base_name_cache`, invalidated by `fn_resolve_gen` (bumped
    /// on every function registration/removal).
    pub(crate) fn fn_base_name_registered(&mut self, name: &str) -> bool {
        if self.fn_base_name_cache_gen != self.fn_resolve_gen {
            self.fn_base_name_cache.clear();
            self.fn_base_name_cache_gen = self.fn_resolve_gen;
        }
        let base = function_key_base_name(name);
        let base_sym = Symbol::intern(base);
        if let Some(&cached) = self.fn_base_name_cache.get(&base_sym) {
            // Debug-only staleness audit: recompute and compare, so a
            // functions-map mutation that missed its `fn_resolve_gen` bump
            // fails CI's debug `prove t/` with a located panic instead of
            // surfacing as a silent wrong "Unknown function" in release.
            #[cfg(debug_assertions)]
            {
                let fresh = self
                    .registry()
                    .functions
                    .keys()
                    .any(|k| function_key_base_name(&k.resolve()) == base);
                assert_eq!(
                    fresh, cached,
                    "stale fn_base_name_cache entry for {name:?} (base {base:?}): \
                     a registry functions-map mutation missed its fn_resolve_gen \
                     bump — see fn_base_name_registered in dispatch_resolve.rs"
                );
            }
            return cached;
        }
        let found = self
            .registry()
            .functions
            .keys()
            .any(|k| function_key_base_name(&k.resolve()) == base);
        self.fn_base_name_cache.insert(base_sym, found);
        found
    }

    pub(super) fn sort_candidates_by_specificity(
        &self,
        candidates: &mut [(String, Arc<FunctionDef>)],
    ) {
        candidates.sort_by(|a, b| {
            let a_rank = self.candidate_specificity_rank(&a.1);
            let b_rank = self.candidate_specificity_rank(&b.1);
            b_rank.cmp(&a_rank).then(a.0.cmp(&b.0))
        });
    }

    pub(super) fn resolve_function_with_alias(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<Arc<FunctionDef>> {
        self.clear_pending_dispatch_error();
        if let Some(def) = self.resolve_function_with_types(name, arg_values) {
            return Some(def);
        }
        if self.pending_dispatch_error.is_some() {
            return None;
        }
        if name.contains(':') || name.contains("::") {
            return None;
        }
        for alias in [format!("prefix:<{name}>"), format!("postfix:<{name}>")] {
            if let Some(def) = self.resolve_function_with_types(&alias, arg_values) {
                return Some(def);
            }
        }
        None
    }

    /// Candidates for a package-qualified `name` that can absorb a call whose
    /// positional count differs from their declared one — i.e. those with an
    /// optional / defaulted / slurpy positional parameter — gathered across ALL
    /// registered arities, sorted most-specific first.
    ///
    /// Multi candidates are registered under `Pkg::name/<arity>…` keys built
    /// from the *declared* parameter count, so a call that omits a defaulted
    /// trailing parameter never matches the exact-arity keys. The bare-name
    /// path has long had this fallback; the qualified path did not, so
    /// `NativeLibs::cannon-name('foo')` could not reach
    /// `multi cannon-name(Str $l, Version $v = Version)` even though the
    /// identical bare call resolved fine.
    pub(super) fn qualified_flexible_arity_candidates(
        &self,
        name: &str,
    ) -> Vec<(String, Arc<FunctionDef>)> {
        let prefix = format!("{}/", name);
        let mut candidates: Vec<(String, Arc<FunctionDef>)> =
            self.registry()
                .functions
                .iter()
                .filter(|(k, def)| {
                    k.resolve().starts_with(&prefix)
                        && def.param_defs.iter().any(|p| {
                            !p.named && (p.optional_marker || p.default.is_some() || p.slurpy)
                        })
                })
                .map(|(k, def)| (k.resolve(), def.clone()))
                .collect();
        self.sort_candidates_by_specificity(&mut candidates);
        candidates
    }

    pub(super) fn resolve_function_with_arity(
        &self,
        name: &str,
        arity: usize,
    ) -> Option<Arc<FunctionDef>> {
        if name.contains("::") {
            let multi_key = format!("{}/{}", name, arity);
            if let Some(def) = self.registry().functions.get(&Symbol::intern(&multi_key)) {
                return Some(def.clone());
            }
            return self
                .registry()
                .functions
                .get(&Symbol::intern(name))
                .cloned();
        }
        // Try multi-dispatch with arity first, innermost package outwards.
        for pkg in self.bare_name_packages() {
            let multi_key = format!("{}::{}/{}", pkg, name, arity);
            if let Some(def) = self.registry().functions.get(&Symbol::intern(&multi_key)) {
                return Some(def.clone());
            }
        }
        // Fall back to regular lookup
        self.resolve_function(name)
    }

    pub(crate) fn resolve_function_with_types(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<Arc<FunctionDef>> {
        crate::vm::vm_stats::record_function_full_resolve(name);
        // Arity counts only positional args, excluding named args (Pair values)
        let arity = arg_values
            .iter()
            .filter(|v| !v.is_string_pair_value())
            .count();
        // The proto's signature gates the whole dispatch: `proto bar {*}`
        // declares an empty signature, so any call with positional arguments
        // can never reach a candidate (rakudo rejects it at compile time with
        // "Calling bar(Str) will never work with signature of the proto ()").
        // The name set is maintained by proto registration and cleared when a
        // plain `sub` supersedes the proto; the registry lookup re-verifies.
        if arity > 0
            && !self.empty_sig_proto_names.is_empty()
            && self.empty_sig_proto_names.contains(&Symbol::intern(name))
            && let Some(proto) = self.resolve_proto_function(name)
            && proto.empty_sig
        {
            let type_names: Vec<String> = arg_values
                .iter()
                .filter(|v| !v.is_string_pair_value())
                .map(crate::value::types::what_type_name)
                .collect();
            let msg = format!(
                "Calling {}({}) will never work with signature of the proto ()",
                name,
                type_names.join(", ")
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            attrs.insert("objname".to_string(), Value::str(name.to_string()));
            attrs.insert("signature".to_string(), Value::str("()".to_string()));
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::TypeCheck::Argument"),
                attrs,
            )));
            self.set_pending_dispatch_error(err);
            return None;
        }
        // Negative gate: if no registry key carries this base name at all, no
        // candidate scan below can match — skip the whole walk. This is the
        // common case for interpreter-native builtins (`make`, `prefix:<~>`,
        // …) that are dispatched *after* a failed user-function resolution.
        if !self.fn_base_name_registered(name) {
            return None;
        }
        if name.contains("::") {
            // Block access to my-scoped (non-our) package items. Checked before
            // the arity-keyed candidate scan below, not only on the exact-name
            // hit: a `multi sub` is
            // registered under `Pkg::name/arity` keys, so the exact-name lookup
            // misses and the scan would hand back the very routine this gate
            // exists to hide (`MScope::multi-lex(1)` answered where raku says
            // "Could not find symbol '&multi-lex' in 'MScope'").
            if self.qualified_name_hidden_here(name) {
                return None;
            }
            if let Some(def) = self
                .registry()
                .functions
                .get(&Symbol::intern(name))
                .cloned()
            {
                return Some(def);
            }
            let prefix = format!("{}/{arity}:", name);
            let untyped_key = format!("{}/{}", name, arity);
            let untyped_key_sym = Symbol::intern(&untyped_key);
            let untyped_m_prefix = format!("{}__m", untyped_key);
            let mut candidates: Vec<(String, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(key, _)| {
                    let ks = key.resolve();
                    ks.starts_with(&prefix)
                        || **key == untyped_key_sym
                        || ks.starts_with(&untyped_m_prefix)
                })
                .map(|(key, def)| (key.resolve(), def.clone()))
                .collect();
            self.sort_candidates_by_specificity(&mut candidates);
            if let Some(def) = self.choose_best_matching_candidate(name, arg_values, candidates) {
                return Some(def);
            }
            // Capture-subsignature candidates (`multi foo(|c(...))`) are registered
            // at arity 0 because the capture consumes all arguments; the real
            // dispatch parameters live in the subsignature.  Such candidates are
            // not found by the arity-keyed lookup above, so collect them
            // separately (across all arities under `name/`) and dispatch on them.
            let subsig_prefix = format!("{}/", name);
            let mut subsig_candidates: Vec<(String, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(key, def)| {
                    key.resolve().starts_with(&subsig_prefix)
                        && def.param_defs.iter().any(|p| p.is_capture_subsignature())
                })
                .map(|(key, def)| (key.resolve(), def.clone()))
                .collect();
            if !subsig_candidates.is_empty() {
                self.sort_candidates_by_specificity(&mut subsig_candidates);
                if let Some(def) =
                    self.choose_best_matching_candidate(name, arg_values, subsig_candidates)
                {
                    return Some(def);
                }
            }
            // A candidate whose declared arity differs from the call's because a
            // trailing parameter is optional/defaulted/slurpy.
            let flexible = self.qualified_flexible_arity_candidates(name);
            if !flexible.is_empty()
                && let Some(def) = self.choose_best_matching_candidate(name, arg_values, flexible)
            {
                return Some(def);
            }
            // Visibility was decided by the gate at the top of this branch.
            if let Some(def) = self
                .registry()
                .functions
                .get(&Symbol::intern(name))
                .cloned()
            {
                return Some(def);
            }
            // Try qualifying with the current package prefix when the
            // prefix package is visible in the current scope (i.e., exists
            // as a Package value in env).  This handles calls like
            // `Our::Package::pkg()` inside `PackageTest` where the nested
            // package was registered as `PackageTest::Our::Package`.
            if self.current_package() != "GLOBAL" {
                // Check if the prefix package (everything before the last `::`)
                // is visible in env as a Package type object.
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
                    let q_prefix = format!("{qualified}/{arity}:");
                    let q_untyped_key = format!("{qualified}/{}", arity);
                    let q_untyped_key_sym = Symbol::intern(&q_untyped_key);
                    let q_untyped_m_prefix = format!("{}__m", q_untyped_key);
                    let mut q_candidates: Vec<(String, Arc<FunctionDef>)> = self
                        .registry()
                        .functions
                        .iter()
                        .filter(|(key, _)| {
                            let ks = key.resolve();
                            ks.starts_with(&q_prefix)
                                || **key == q_untyped_key_sym
                                || ks.starts_with(&q_untyped_m_prefix)
                        })
                        .map(|(key, def)| (key.resolve(), def.clone()))
                        .collect();
                    self.sort_candidates_by_specificity(&mut q_candidates);
                    if let Some(def) =
                        self.choose_best_matching_candidate(&qualified, arg_values, q_candidates)
                    {
                        return Some(def);
                    }
                }
            }
            return None;
        }
        // Bare name: search the current package, then each enclosing package,
        // then GLOBAL (see `bare_name_packages`).
        let search_pkgs = self.bare_name_packages();
        for pkg in &search_pkgs {
            if let Some(def) = self
                .registry()
                .functions
                .get(&Symbol::intern(&format!("{}::{}", pkg, name)))
                .cloned()
            {
                return Some(def);
            }
        }
        let typed_prefixes: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/{}:", pkg, name, arity))
            .collect();
        let generic_keys: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/{}", pkg, name, arity))
            .collect();
        let mut found_multi_candidates = false;
        let mut candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(key, _)| {
                let ks = key.resolve();
                typed_prefixes.iter().any(|p| ks.starts_with(p))
            })
            .map(|(key, def)| (key.resolve(), def.clone()))
            .collect();
        for key in &generic_keys {
            let key_sym = Symbol::intern(key);
            let m_prefix = format!("{}__m", key);
            let more: Vec<(String, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(k, _)| **k == key_sym || k.resolve().starts_with(&m_prefix))
                .map(|(k, def)| (k.resolve(), def.clone()))
                .collect();
            if !more.is_empty() {
                found_multi_candidates = true;
            }
            candidates.extend(more);
        }
        self.sort_candidates_by_specificity(&mut candidates);
        if let Some(def) = self.choose_best_matching_candidate(name, arg_values, candidates) {
            return Some(def);
        }
        // Try optional/default candidates with different arities.
        // These can match calls with fewer positional arguments.
        let optional_prefixes: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/", pkg, name))
            .collect();
        let mut optional_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                optional_prefixes
                    .iter()
                    .any(|prefix| ks.starts_with(prefix))
                    && def
                        .param_defs
                        .iter()
                        .any(|p| !p.named && (p.optional_marker || p.default.is_some()))
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        if !optional_candidates.is_empty() {
            found_multi_candidates = true;
        }
        optional_candidates.sort_by(|a, b| {
            let a_has_where = a.1.param_defs.iter().any(|p| p.where_constraint.is_some());
            let b_has_where = b.1.param_defs.iter().any(|p| p.where_constraint.is_some());
            let a_has_subsig = a.1.param_defs.iter().any(|p| p.sub_signature.is_some());
            let b_has_subsig = b.1.param_defs.iter().any(|p| p.sub_signature.is_some());
            b_has_where
                .cmp(&a_has_where)
                .then(b_has_subsig.cmp(&a_has_subsig))
                .then(a.0.cmp(&b.0))
        });
        if let Some(def) =
            self.choose_best_matching_candidate(name, arg_values, optional_candidates)
        {
            return Some(def);
        }
        // Try slurpy candidates with different arities (slurpy params accept
        // variable number of args, so the registered arity may differ from call arity).
        let slurpy_prefixes = &optional_prefixes;
        let mut slurpy_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                slurpy_prefixes.iter().any(|prefix| ks.starts_with(prefix))
                    && def
                        .param_defs
                        .iter()
                        .any(|p| p.slurpy || p.is_capture_subsignature())
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        if !slurpy_candidates.is_empty() {
            found_multi_candidates = true;
        }
        slurpy_candidates.sort_by(|a, b| a.0.cmp(&b.0));
        if let Some(def) = self.choose_best_matching_candidate(name, arg_values, slurpy_candidates)
        {
            return Some(def);
        }
        // Try candidates from other arities (e.g., optional/default positional params).
        // This allows calls with fewer args to match signatures like `$x = ...`.
        let any_arity_prefixes = &optional_prefixes;
        let mut any_arity_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, _)| {
                let ks = k.resolve();
                any_arity_prefixes
                    .iter()
                    .any(|prefix| ks.starts_with(prefix))
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        if !any_arity_candidates.is_empty() {
            found_multi_candidates = true;
        }
        self.sort_candidates_by_specificity(&mut any_arity_candidates);
        if let Some(def) =
            self.choose_best_matching_candidate(name, arg_values, any_arity_candidates)
        {
            return Some(def);
        }
        // Fall back to arity-only if no proto declared and no multi candidates were found.
        // When multi candidates exist but none matched (e.g., sub-signature arity mismatch),
        // falling back would bypass the sub-signature check.
        if self.has_proto(name) || found_multi_candidates {
            None
        } else {
            self.resolve_function_with_arity(name, arity)
        }
    }

    /// Collect all matching multi dispatch candidates for a function call,
    /// sorted by specificity (most specific first). Used by callsame/nextcallee.
    pub(crate) fn resolve_all_matching_candidates(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Vec<FunctionDef> {
        let arity = arg_values.len();
        let mut all_matches = Vec::new();

        let search_pkgs = self.bare_name_packages();

        // Collect from typed candidates
        let typed_prefixes: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/{}:", pkg, name, arity))
            .collect();
        for prefix_base in typed_prefixes {
            let candidates: Vec<FunctionDef> = self
                .registry()
                .functions
                .iter()
                .filter(|(key, _)| key.resolve().starts_with(&prefix_base))
                .map(|(_, def)| (**def).clone())
                .collect();
            for def in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    all_matches.push(def);
                }
            }
        }

        // Collect from generic (untyped) candidates
        let generic_keys: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/{}", pkg, name, arity))
            .collect();
        for key in &generic_keys {
            let key_sym = Symbol::intern(key);
            let m_prefix = format!("{}__m", key);
            let mut candidates: Vec<(String, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(k, _)| **k == key_sym || k.resolve().starts_with(&m_prefix))
                .map(|(k, def)| (k.resolve(), def.clone()))
                .collect();
            candidates.sort_by(|a, b| {
                let a_has_subsig = a.1.param_defs.iter().any(|p| p.sub_signature.is_some());
                let b_has_subsig = b.1.param_defs.iter().any(|p| p.sub_signature.is_some());
                b_has_subsig.cmp(&a_has_subsig).then(a.0.cmp(&b.0))
            });
            for (_, def) in candidates {
                if self.args_match_param_types(arg_values, &def.param_defs) {
                    let fp = crate::ast::function_body_fingerprint(
                        &def.params,
                        &def.param_defs,
                        &def.body,
                    );
                    if !all_matches.iter().any(|m: &FunctionDef| {
                        crate::ast::function_body_fingerprint(&m.params, &m.param_defs, &m.body)
                            == fp
                    }) {
                        all_matches.push((*def).clone());
                    }
                }
            }
        }

        // Collect from slurpy candidates
        let slurpy_prefixes: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/", pkg, name))
            .collect();
        let mut slurpy_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                slurpy_prefixes.iter().any(|prefix| ks.starts_with(prefix))
                    && def
                        .param_defs
                        .iter()
                        .any(|p| p.slurpy || p.is_capture_subsignature())
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        slurpy_candidates.sort_by(|a, b| a.0.cmp(&b.0));
        for (_, def) in slurpy_candidates {
            if self.args_match_param_types(arg_values, &def.param_defs) {
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if !all_matches.iter().any(|m: &FunctionDef| {
                    crate::ast::function_body_fingerprint(&m.params, &m.param_defs, &m.body) == fp
                }) {
                    all_matches.push((*def).clone());
                }
            }
        }

        all_matches
    }
}
