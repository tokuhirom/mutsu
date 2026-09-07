//! Full multi-candidate gathering for a function name: the list `callsame` /
//! `nextsame` / `callwith` redispatch from, and the input to the
//! value-dependency analysis behind `func_multi_resolve_cache`.

use super::*;

impl Interpreter {
    /// Collect ALL multi dispatch candidates for a function name, regardless of
    /// arity or type matching.  Used to build the full candidate list for
    /// callwith(), which may re-dispatch with different arguments.
    ///
    /// Walks every registry function key. Prefer
    /// [`Self::resolve_all_multi_candidates_indexed`] wherever `&mut self` is
    /// available — this `&self` spelling exists only for the reflective
    /// `resolve_code_var` path, which cannot build the index.
    pub(crate) fn resolve_all_multi_candidates(&self, name: &str) -> Vec<Arc<FunctionDef>> {
        self.multi_candidates_over(name, None)
    }

    /// Same answer as [`Self::resolve_all_multi_candidates`], gathered from the
    /// per-generation base-name key index instead of a full walk of the registry
    /// functions map.
    ///
    /// Every key the prefix filter can match (`<pkg>::<name>/<arity>…`) reduces
    /// to `name`'s own base name under `function_key_base_name`, so
    /// [`Self::fn_keys_for_base`] is a superset of the matches and the filtered
    /// result is identical — the same reduction the resolver's own candidate
    /// gathers already run on. The walk is O(registry) *per enclosing package*
    /// and `push_multi_dispatch_frame` runs it on every dispatch that sets up a
    /// `callsame`/`nextsame` chain, which is once per call for any routine
    /// reached through the OTF-cached named path: it measured 8.5% of the
    /// per-assertion cost of the vendored upstream `Test` module
    /// (`todo/perf/listop-call-bypasses-every-compiled-call-cache.md`).
    ///
    /// The index is also captured once per `fn_resolve_gen`, so the candidate
    /// order it feeds the specificity sort is stable within a generation rather
    /// than freshly hash-ordered per call.
    pub(crate) fn resolve_all_multi_candidates_indexed(
        &mut self,
        name: &str,
    ) -> Vec<Arc<FunctionDef>> {
        let keys = self.fn_keys_for_base(name);
        self.multi_candidates_over(name, Some(&keys))
    }

    /// [`Self::resolve_all_multi_candidates_indexed`] behind the per-generation
    /// memo (`multi_dispatch_candidates_memo`): the same list, shared, for a repeat call
    /// of the same name from the same package context under an unchanged
    /// registry.
    pub(crate) fn resolve_all_multi_candidates_cached(
        &mut self,
        name: &str,
    ) -> crate::runtime::MultiCandidateList {
        let generation = (self.fn_resolve_gen, self.registry().proto_generation());
        if self.multi_dispatch_candidates_memo_gen != generation {
            self.multi_dispatch_candidates_memo.clear();
            self.multi_dispatch_candidates_memo_gen = generation;
        }
        let key = (
            Symbol::intern(name),
            self.current_package_sym(),
            self.routine_stack()
                .last()
                .and_then(|frame| frame.lexical_package),
        );
        if let Some(cached) = self.multi_dispatch_candidates_memo.get(&key) {
            return cached.clone();
        }
        let candidates = Arc::new(self.resolve_all_multi_candidates_indexed(name));
        self.multi_dispatch_candidates_memo
            .insert(key, candidates.clone());
        candidates
    }

    /// Shared body of the two candidate gathers above: `keys`, when given, is the
    /// candidate key set to filter; `None` means walk the whole functions map.
    fn multi_candidates_over(&self, name: &str, keys: Option<&[Symbol]>) -> Vec<Arc<FunctionDef>> {
        let mut all: Vec<(String, Arc<FunctionDef>)> = Vec::new();
        let mut packages = self.bare_name_packages();
        // An imported proto is registered under the importing lexical scope,
        // while its multi candidates remain in the defining module. Include
        // that owner so a first-class `&name` can materialize the same
        // candidates an ordinary `name(...)` call reaches. This matters for
        // real Test's `proto sub skip(|)`: `.&skip` must invoke Test::skip,
        // not the core list builtin of the same name.
        if let Some(proto) = self.resolve_proto_function(name) {
            let owner = proto.package.resolve();
            if !packages.iter().any(|pkg| pkg == &owner) {
                packages.insert(0, owner);
            }
        }
        let prefixes: Vec<String> = packages
            .iter()
            .map(|pkg| format!("{}::{}/", pkg, name))
            .collect();
        let mut seen_fps = Vec::new();
        for prefix in &prefixes {
            // `as_str` (a `&'static str` out of the interner) rather than
            // `resolve()`: the filter runs over every candidate key on every
            // multi call (`push_multi_dispatch_frame`), and `resolve()` copied
            // each one into a fresh `String` just to test a prefix.
            let registry = self.registry();
            let candidates: Vec<(String, Arc<FunctionDef>)> = match keys {
                Some(keys) => keys
                    .iter()
                    .filter(|k| k.as_str().starts_with(prefix.as_str()))
                    .filter_map(|k| {
                        registry
                            .functions
                            .get(k)
                            .map(|def| (k.resolve(), def.clone()))
                    })
                    .collect(),
                None => registry
                    .functions
                    .iter()
                    .filter(|(k, _)| k.as_str().starts_with(prefix.as_str()))
                    .map(|(k, def)| (k.resolve(), def.clone()))
                    .collect(),
            };
            drop(registry);
            for (key, def) in candidates {
                let fp = def.body_fingerprint();
                if !seen_fps.contains(&fp) {
                    seen_fps.push(fp);
                    all.push((key, def));
                }
            }
        }
        // Sort by dispatch specificity (most specific first). The callsame /
        // nextsame consumers (`builtins_dispatch_next.rs`) pick the FIRST
        // arg-matching candidate in this list's order, so it must reflect the
        // real dispatch order rather than arbitrary HashMap iteration order.
        // Without this, when several candidates match the same args — e.g.
        // overlapping `where` constraints plus a generic fallback
        // (`multi foo(Int $ where * > 0)`, `multi foo(Int $ where * < 10)`,
        // `multi foo($)`) — a broader candidate appearing earlier in HashMap
        // order is wrongly chosen before a narrower one, dropping the narrower
        // candidate from the nextsame chain (hash-seed-dependent flake in
        // S12-methods/defer-next.t `nextsame + multi + where`). Mirrors the
        // deterministic winner resolution PR-4 added to `push_multi_dispatch_frame`.
        self.sort_candidates_by_specificity(&mut all);
        all.into_iter().map(|(_, def)| def).collect()
    }
}

#[cfg(test)]
mod indexed_gather_equivalence_tests {
    use super::*;

    /// The indexed gather narrows the key set it filters from "every registry
    /// function key" to "the keys whose base name is this name's". That is only
    /// sound while both sides reduce a key the same way, so pin the two spellings
    /// against each other over the shapes whose key strings are awkward: a plain
    /// multi (arity + typed keys for one name), a proto'd multi, an operator name
    /// carrying its own `/` (`infix:</>`), a non-multi sub, and a name that is not
    /// registered at all.
    #[test]
    fn indexed_gather_answers_what_the_full_walk_answers() {
        let mut i = Interpreter::new();
        i.run(
            r#"
            multi sub f(Int $x) { 1 }
            multi sub f(Str $x) { 2 }
            multi sub f($x, $y) { 3 }
            proto sub g($) {*}
            multi sub g(Int $x) { 1 }
            multi sub g(Str $x) { 2 }
            multi sub infix:<//////>(Int $a, Int $b) { $a + $b }
            multi sub infix:<//////>(Str $a, Str $b) { $a ~ $b }
            sub plain($x) { $x }
            "#,
        )
        .expect("setup program runs");

        for name in ["f", "g", "infix:<//////>", "plain", "never-declared"] {
            let full: Vec<u64> = i
                .resolve_all_multi_candidates(name)
                .iter()
                .map(|d| d.body_fingerprint())
                .collect();
            let indexed: Vec<u64> = i
                .resolve_all_multi_candidates_indexed(name)
                .iter()
                .map(|d| d.body_fingerprint())
                .collect();
            assert_eq!(
                full, indexed,
                "indexed gather disagrees with the full registry walk for {name:?}"
            );
        }

        // The multi names really do have candidates, so the assertions above are
        // not comparing two empty lists.
        assert_eq!(i.resolve_all_multi_candidates_indexed("f").len(), 3);
        assert_eq!(i.resolve_all_multi_candidates_indexed("g").len(), 2);
        assert_eq!(
            i.resolve_all_multi_candidates_indexed("infix:<//////>")
                .len(),
            2
        );

        // The memoized gather answers the same list, and a later registration
        // (which moves `fn_resolve_gen`) refreshes it rather than serving the
        // stale one.
        assert_eq!(i.resolve_all_multi_candidates_cached("f").len(), 3);
        assert_eq!(i.resolve_all_multi_candidates_cached("f").len(), 3);
        i.run("multi sub f(Num $x, Num $y, Num $z) { 4 }")
            .expect("a fourth candidate registers");
        assert_eq!(i.resolve_all_multi_candidates_cached("f").len(), 4);
        assert_eq!(i.resolve_all_multi_candidates_indexed("f").len(), 4);
    }
}
