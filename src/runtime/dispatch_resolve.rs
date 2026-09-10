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
    // A hand-rolled reverse scan for `::`: `str::rfind` builds a two-way
    // searcher per call, which is most of what this function cost on the
    // per-dispatch candidate walk that calls it once per registry key.
    let hb = head.as_bytes();
    let mut j = hb.len();
    while j >= 2 {
        if hb[j - 1] == b':' && hb[j - 2] == b':' {
            return &head[j..];
        }
        j -= 1;
    }
    head
}

impl Interpreter {
    /// The package chain a bare-name *multi-candidate* search may draw from,
    /// innermost first.
    ///
    /// [`Self::bare_name_packages`] lists every package enclosing the running
    /// code, and the candidate gathers used to pool every one of them into a
    /// single ranking. That is right only while no enclosing scope declares its
    /// own `proto`: raku gives a `proto` a **fresh** candidate list, and a
    /// `multi` declared without one in scope extends the innermost visible
    /// proto's list instead. Pooling across a package that has its own proto
    /// merged two independent families, so
    ///
    /// ```raku
    /// module M { proto foo($){*}; multi foo(Int $x){"in-mod"}; our sub go(){ foo(5) } }
    /// proto foo($){*}; multi foo(Int $x){"mainline"};
    /// M::go();   # raku: in-mod   mutsu: Ambiguous call to foo(Int)
    /// ```
    ///
    /// answered "Ambiguous call ... (Int $x), (Int $x)" — the same signature
    /// twice, from the two packages. Truncating the walk after the innermost
    /// package that declares its own proto restores the shadow while leaving
    /// the no-proto case merging exactly as before.
    fn candidate_search_packages(&self, name: &str) -> Vec<String> {
        let pkgs = self.bare_name_packages();
        let registry = self.registry();
        let mut out = Vec::with_capacity(pkgs.len());
        for pkg in pkgs {
            let owns_proto = registry
                .proto_functions
                .contains_key(&Symbol::intern(&format!("{}::{}", pkg, name)));
            out.push(pkg);
            if owns_proto {
                break;
            }
        }
        out
    }

    /// Whether ANY registered function key carries `name`'s base name — a
    /// cheap negative gate for [`Self::resolve_function_with_types`]. When
    /// this is `false`, no lookup pattern in the resolver (qualified, typed,
    /// arity-keyed, flexible-arity, package-searched) can possibly match, so
    /// the resolver returns `None` without walking the registry. Answered from
    /// the [`Self::fn_keys_for_base`] index, invalidated by `fn_resolve_gen`
    /// (bumped on every function registration/removal).
    pub(crate) fn fn_base_name_registered(&mut self, name: &str) -> bool {
        let keys = self.fn_keys_for_base(name);
        // Debug-only staleness audit, placed HERE rather than inside
        // `fn_keys_for_base`: the resolver asks this negative gate exactly once
        // per resolution but reaches the index several times, so auditing at
        // the gate keeps the debug `prove t/` cost at the one full scan it
        // already paid before the index existed, while checking strictly more
        // (the whole key list, not just whether the base is present). A
        // functions-map mutation that missed its `fn_resolve_gen` bump then
        // fails CI with a located panic instead of surfacing as a silent wrong
        // "Unknown function" — or a silently missing multi candidate.
        #[cfg(debug_assertions)]
        {
            let base = function_key_base_name(name);
            let fresh = self.collect_fn_keys_for_base(base);
            let mut a: Vec<&str> = fresh.iter().map(|k| k.as_str()).collect();
            let mut b: Vec<&str> = keys.iter().map(|k| k.as_str()).collect();
            a.sort_unstable();
            b.sort_unstable();
            assert_eq!(
                a, b,
                "stale fn_keys_by_base entry for {name:?} (base {base:?}): \
                 a registry functions-map mutation missed its fn_resolve_gen \
                 bump — see fn_keys_for_base in dispatch_resolve.rs"
            );
        }
        // A compunit-private top-level routine has been moved OUT of the
        // functions map (`runtime/unit_private_routines.rs`), so the key index
        // cannot see it; the gate must not veto a resolution that would find
        // one. The check is a hash lookup guarded by an emptiness test, and the
        // set only ever holds names some loaded compunit kept private.
        !keys.is_empty() || self.is_unit_scoped_routine_name(name)
    }

    /// Every registry function key whose BASE name is `name`'s base name.
    ///
    /// The order is the functions map's own iteration order, captured once per
    /// generation — so a gather sees a *stable* candidate order within a
    /// generation rather than a fresh hash order each call. Ranking still
    /// happens in `sort_candidates_by_specificity`, whose tie-breakers
    /// (declaration stamp, then key string) are what actually decide.
    ///
    /// This is the index every name-keyed candidate gather runs on. The
    /// resolver's lookup patterns — the exact key `Pkg::name`, the arity keys
    /// `Pkg::name/<arity>`, the typed keys `Pkg::name/<arity>:<types>`, and the
    /// `__m<n>` multi suffixes — all reduce to the same base name under
    /// [`function_key_base_name`], so a gather can iterate this handful of keys
    /// instead of the whole functions map. An empty slice is also the negative
    /// gate [`Self::fn_base_name_registered`] answers with.
    ///
    /// Filled lazily per base name and dropped wholesale when `fn_resolve_gen`
    /// moves, which every function registration/removal bumps.
    pub(crate) fn fn_keys_for_base(&mut self, name: &str) -> std::sync::Arc<[Symbol]> {
        if self.fn_keys_by_base_gen != self.fn_resolve_gen {
            self.fn_keys_by_base.clear();
            self.fn_keys_by_base_gen = self.fn_resolve_gen;
        }
        let base = function_key_base_name(name);
        let base_sym = Symbol::intern(base);
        if let Some(cached) = self.fn_keys_by_base.get(&base_sym) {
            // Staleness is audited once per resolution in
            // `fn_base_name_registered`, not here — see the note there.
            return cached.clone();
        }
        let keys = self.collect_fn_keys_for_base(base);
        self.fn_keys_by_base.insert(base_sym, keys.clone());
        keys
    }

    fn collect_fn_keys_for_base(&self, base: &str) -> std::sync::Arc<[Symbol]> {
        self.registry()
            .functions
            .keys()
            .filter(|k| function_key_base_name(k.as_str()) == base)
            .copied()
            .collect()
    }

    pub(super) fn sort_candidates_by_specificity(
        &self,
        candidates: &mut [(String, Arc<FunctionDef>)],
    ) {
        candidates.sort_by(|a, b| {
            let a_rank = self.candidate_specificity_rank(&a.1);
            let b_rank = self.candidate_specificity_rank(&b.1);
            // Equal narrowness: Rakudo picks the candidate declared first, so
            // the registration stamp decides. The registry key string is only
            // the last resort (defs built outside a registration path share
            // stamp 0), and keeps the order deterministic.
            b_rank
                .cmp(&a_rank)
                .then(a.1.decl_order.cmp(&b.1.decl_order))
                .then(a.0.cmp(&b.0))
        });
    }

    pub(super) fn resolve_function_with_alias(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<Arc<FunctionDef>> {
        self.clear_pending_dispatch_error();
        // Consult the sound multi-resolution cache (`func_multi_resolve_cache`)
        // rather than resolving from scratch: for a type+arity-deterministic
        // name it answers what `resolve_function_with_types` would, without the
        // per-call candidate gather + match + rank + dedup. Un-keyable
        // arguments, value-dependent candidates and ambiguity all resolve fresh
        // inside it, so this is behaviour-preserving.
        if let Some(def) = self.resolve_function_multi_cached(name, arg_values) {
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
        &mut self,
        name: &str,
    ) -> Vec<(String, Arc<FunctionDef>)> {
        let prefix = format!("{}/", name);
        let base_keys = self.fn_keys_for_base(name);
        let registry = self.registry();
        let mut candidates: Vec<(String, Arc<FunctionDef>)> = base_keys
            .iter()
            .filter_map(|k| registry.functions.get(k).map(|def| (k.as_str(), def)))
            .filter(|(ks, def)| {
                ks.starts_with(&prefix)
                    && def.param_defs.iter().any(|p| {
                        !p.named && (p.optional_marker || p.default.is_some() || p.is_variadic())
                    })
            })
            .map(|(ks, def)| (ks.to_string(), def.clone()))
            .collect();
        drop(registry);
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
            let base_keys = self.fn_keys_for_base(name);
            let mut candidates: Vec<(String, Arc<FunctionDef>)> = {
                let registry = self.registry();
                base_keys
                    .iter()
                    .filter_map(|key| registry.functions.get(key).map(|def| (key, def)))
                    .filter(|(key, _)| {
                        let ks = key.as_str();
                        ks.starts_with(&prefix)
                            || **key == untyped_key_sym
                            || ks.starts_with(&untyped_m_prefix)
                    })
                    .map(|(key, def)| (key.resolve(), def.clone()))
                    .collect()
            };
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
            let mut subsig_candidates: Vec<(String, Arc<FunctionDef>)> = {
                let registry = self.registry();
                base_keys
                    .iter()
                    .filter_map(|key| registry.functions.get(key).map(|def| (key, def)))
                    .filter(|(key, def)| {
                        key.as_str().starts_with(&subsig_prefix)
                            && def.param_defs.iter().any(|p| p.is_capture_subsignature())
                    })
                    .map(|(key, def)| (key.resolve(), def.clone()))
                    .collect()
            };
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
                    let q_base_keys = self.fn_keys_for_base(&qualified);
                    let mut q_candidates: Vec<(String, Arc<FunctionDef>)> = {
                        let registry = self.registry();
                        q_base_keys
                            .iter()
                            .filter_map(|key| registry.functions.get(key).map(|def| (key, def)))
                            .filter(|(key, _)| {
                                let ks = key.as_str();
                                ks.starts_with(&q_prefix)
                                    || **key == q_untyped_key_sym
                                    || ks.starts_with(&q_untyped_m_prefix)
                            })
                            .map(|(key, def)| (key.resolve(), def.clone()))
                            .collect()
                    };
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
        // A compunit-private top-level routine of the unit currently executing
        // wins over every package entry: it is a lexical of that compunit, and
        // the shared registry may hold an unrelated same-named routine
        // belonging to the scope that loaded it (`runtime/unit_private_routines.rs`).
        if let Some(def) = self.unit_private_routine(name) {
            return Some(def);
        }
        // Bare name: search the current package, then each enclosing package,
        // then GLOBAL (see `bare_name_packages`), stopping at the innermost one
        // that declares its own `proto` for this name (see
        // `candidate_search_packages`).
        let search_pkgs = self.candidate_search_packages(name);
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
        let base_keys = self.fn_keys_for_base(name);
        let mut candidates: Vec<(String, Arc<FunctionDef>)> = {
            let registry = self.registry();
            base_keys
                .iter()
                .filter_map(|key| registry.functions.get(key).map(|def| (key, def)))
                .filter(|(key, _)| {
                    let ks = key.as_str();
                    typed_prefixes.iter().any(|p| ks.starts_with(p))
                })
                .map(|(key, def)| (key.resolve(), def.clone()))
                .collect()
        };
        for key in &generic_keys {
            let key_sym = Symbol::intern(key);
            let m_prefix = format!("{}__m", key);
            let more: Vec<(String, Arc<FunctionDef>)> = {
                let registry = self.registry();
                base_keys
                    .iter()
                    .filter_map(|k| registry.functions.get(k).map(|def| (k, def)))
                    .filter(|(k, _)| **k == key_sym || k.as_str().starts_with(&m_prefix))
                    .map(|(k, def)| (k.resolve(), def.clone()))
                    .collect()
            };
            if !more.is_empty() {
                found_multi_candidates = true;
            }
            candidates.extend(more);
        }
        self.sort_candidates_by_specificity(&mut candidates);
        let exact_candidate_consumes_optional = candidates.iter().any(|(_, def)| {
            def.param_defs
                .iter()
                .any(|p| !p.named && (p.optional_marker || p.default.is_some()))
        });
        // An exact-arity candidate with no optional positional parameter is
        // already narrower than every default-arity fallback. Preserve that
        // fast path; otherwise `multi f(Int $x)` would lose to
        // `multi f(Int $x, Int $y = 7)` for `f(1)`. When an exact candidate
        // does consume an optional argument, however, it must compete with
        // longer signatures whose required parameters may describe the call
        // more precisely (the `is-approx` tolerance overloads).
        if !exact_candidate_consumes_optional
            && let Some(def) =
                self.choose_best_matching_candidate(name, arg_values, candidates.clone())
        {
            return Some(def);
        }
        // Include optional/default candidates with different registered arities
        // before choosing a winner. A candidate such as `(Numeric, Numeric,
        // Numeric, $desc = '')` is applicable to a three-argument call even
        // though its registration arity is four, and must compete with the
        // exact-arity `(Numeric, Numeric, $desc = '')` candidate rather than
        // being considered only after that wider candidate has already won.
        let optional_prefixes: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/", pkg, name))
            .collect();
        let optional_candidates: Vec<(String, Arc<FunctionDef>)> = self
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
        candidates.extend(optional_candidates);
        self.sort_candidates_by_specificity(&mut candidates);
        if let Some(def) = self.choose_best_matching_candidate(name, arg_values, candidates) {
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
                        .any(|p| p.is_variadic() || p.is_capture_subsignature())
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

        let search_pkgs = self.candidate_search_packages(name);

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
                if self.args_match_multi_candidate(arg_values, &def.param_defs) {
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
                if self.args_match_multi_candidate(arg_values, &def.param_defs) {
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
                        .any(|p| p.is_variadic() || p.is_capture_subsignature())
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        slurpy_candidates.sort_by(|a, b| a.0.cmp(&b.0));
        for (_, def) in slurpy_candidates {
            if self.args_match_multi_candidate(arg_values, &def.param_defs) {
                let fp = def.body_fingerprint();
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

#[cfg(test)]
mod base_name_tests {
    use super::function_key_base_name;

    /// The hand-rolled `::` scan must agree with the `rfind` it replaced on
    /// every key shape the registry produces: bare, package-qualified, with
    /// and without the `/arity` suffix, and degenerate short keys.
    #[test]
    fn base_name_strips_package_and_arity() {
        assert_eq!(function_key_base_name("foo"), "foo");
        assert_eq!(function_key_base_name("GLOBAL::foo"), "foo");
        assert_eq!(function_key_base_name("A::B::foo"), "foo");
        assert_eq!(function_key_base_name("A::B::foo/2"), "foo");
        assert_eq!(function_key_base_name("foo/10"), "foo");
        assert_eq!(function_key_base_name("A::B::infix:<+>/2"), "infix:<+>");
        assert_eq!(function_key_base_name("::foo"), "foo");
        assert_eq!(function_key_base_name(":"), ":");
        assert_eq!(function_key_base_name("::"), "");
        assert_eq!(function_key_base_name(""), "");
        assert_eq!(function_key_base_name("a"), "a");
    }
}
