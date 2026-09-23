//! Memo for the argument-independent tail of
//! [`Interpreter::resolve_function_with_types`]: a bare name that resolves to
//! a plain (non-`multi`) routine.
//!
//! When no `multi` candidate is in scope for a bare name, the resolver answers
//! with the compunit-private routine of the running unit, or else with the
//! first package in the search chain whose exact `Pkg::name` key is
//! registered. Neither step looks at the arguments. What they do read is the
//! functions map (named by `fn_resolve_gen`), the proto table (whose own
//! generation is part of the key), the current package and the innermost
//! frame's lexical package (both part of the key), and the running compunit
//! (`current_unit`, part of the key).
//!
//! Both [`Interpreter::func_multi_dispatch_type_cacheable`] and the VM's
//! `fn_resolve_cache` withhold themselves from exactly these names -- the
//! first because a plain sub has no arity-keyed candidate to gather, the
//! second for a compunit-private name -- so before this memo a routine that
//! could not take a light call path paid the whole walk up to three times per
//! call ([#9081](https://github.com/tokuhirom/mutsu/issues/9081)): once in
//! `find_compiled_function_memo`, once in `user_function_matches_call`, and
//! once more to fetch the def it was going to compile.
//!
//! ## What is (and is not) recorded
//!
//! - A compunit-private routine is recorded only when it came from
//!   `current_unit` itself. [`Interpreter::unit_private_routine`] falls back
//!   to the unit of the executing *frame* when `current_unit` has no such
//!   routine; that answer depends on the frame, which the key does not carry,
//!   so it is never recorded.
//! - A package-searched plain routine is recorded only when the name is not
//!   compunit-private anywhere. Otherwise the same key could be asked again
//!   from a frame whose unit *does* hold a private routine of that name, and
//!   the frame-dependent fallback above would have to win.
//! - Negative answers are never recorded; the multi walk after the plain
//!   lookup needs the arguments.
//!
//! Every write that could change an answer moves one of the key's
//! components: registering, removing or secluding a routine writes the
//! functions map (new `fn_resolve_gen`), a `proto` bumps the proto
//! generation, and `invalidate_fn_resolution` -- the one announcement that can
//! leave the map's version where it was (`wrap`) -- empties the memo outright.

use super::*;

/// `(name, current unit, current package, innermost lexical package, proto
/// generation)`, tagged per entry with `fn_resolve_gen`.
pub(crate) type PlainFnResolveKey = (Symbol, Symbol, Symbol, Option<Symbol>, u64);

impl Interpreter {
    /// The key this call would be memoized under, or `None` when the name can
    /// never be answered from the memo.
    ///
    /// A qualified name takes its own branch of the resolver, and a name with
    /// an empty-signature `proto` is rejected by an arity check that must keep
    /// running on every call, so neither is memoized.
    pub(crate) fn plain_fn_resolve_key(&self, name: &str) -> Option<PlainFnResolveKey> {
        if name.contains("::") {
            return None;
        }
        // `lookup`, not `intern`: every name that was ever registered is
        // interned, so a name that is not has nothing to answer.
        let name_sym = Symbol::lookup(name)?;
        if !self.empty_sig_proto_names.is_empty() && self.empty_sig_proto_names.contains(&name_sym)
        {
            return None;
        }
        Some((
            name_sym,
            self.current_unit,
            self.current_package_sym(),
            self.routine_stack()
                .last()
                .and_then(|frame| frame.lexical_package),
            self.registry().proto_generation(),
        ))
    }

    pub(crate) fn plain_fn_resolve_memo_get(
        &self,
        key: &PlainFnResolveKey,
    ) -> Option<Arc<FunctionDef>> {
        self.plain_fn_resolve_memo
            .get(self.fn_resolve_gen, key)
            .cloned()
    }

    pub(crate) fn plain_fn_resolve_memo_insert(
        &mut self,
        key: PlainFnResolveKey,
        def: &Arc<FunctionDef>,
    ) {
        let generation = self.fn_resolve_gen;
        self.plain_fn_resolve_memo
            .insert(generation, key, def.clone());
    }

    /// The compunit-private routine `name` of `current_unit` (or of an `EVAL`
    /// parent of it) -- the frame-independent first half of
    /// [`Self::unit_private_routine`].
    pub(crate) fn current_unit_private_routine(&self, name: &str) -> Option<Arc<FunctionDef>> {
        if self.unit_private_names.is_empty() {
            return None;
        }
        let name_sym = Symbol::lookup(name)?;
        if !self.unit_private_names.contains(&name_sym) {
            return None;
        }
        self.unit_private_routine_from(self.current_unit, name_sym)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn resolve(i: &mut Interpreter, name: &str) -> Option<Arc<FunctionDef>> {
        i.resolve_function_with_types(name, &[Value::int(1)])
    }

    /// A second resolution of a plain sub is answered from the memo, and a
    /// redefinition (a write to the functions map) is not served stale.
    #[test]
    fn plain_sub_resolution_is_memoized_and_follows_redefinition() {
        let mut i = Interpreter::new();
        i.run("sub plain(Int:D $x) { 1 }\n")
            .expect("setup program runs");
        let first = resolve(&mut i, "plain").expect("plain resolves");
        let key = i.plain_fn_resolve_key("plain").expect("keyable name");
        let memo = i.plain_fn_resolve_memo_get(&key).expect("memoized");
        assert!(Arc::ptr_eq(&first, &memo));

        i.run("sub plain(Int:D $x) { 2 }\n")
            .expect("redefinition runs");
        let second = resolve(&mut i, "plain").expect("plain still resolves");
        assert_ne!(
            first.body_fingerprint(),
            second.body_fingerprint(),
            "the redefinition is what resolves now"
        );
    }

    /// A name with `multi` candidates never enters the memo: its winner
    /// depends on the arguments.
    #[test]
    fn multi_names_are_not_memoized() {
        let mut i = Interpreter::new();
        i.run("multi sub m(Int $x) { 1 }\nmulti sub m(Str $x) { 2 }\n")
            .expect("setup program runs");
        let _ = resolve(&mut i, "m");
        let key = i.plain_fn_resolve_key("m").expect("keyable name");
        assert!(i.plain_fn_resolve_memo_get(&key).is_none());
    }
}
