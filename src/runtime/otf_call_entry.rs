//! The entry of `ResolutionCaches::otf_call_cache`, and the unit key that
//! lets the cache answer a compunit-private routine.
//!
//! A package-less top-level `sub` of a loaded module is lexical to that
//! module's compilation unit (`unit_private_routines.rs`), so its bare name
//! resolves differently depending on which unit is running. The name caches
//! were keyed by name and callsite package only, so such a name skipped every
//! one of them and re-ran the whole resolution walk -- `has_declared_function`,
//! `user_function_matches_call`, the builtin-shadowing checks and the
//! `Pkg::name` key probes -- on every call: ~15k instructions before the
//! callee's frame was even pushed. Monad's helpers (`some-type`, `absent`,
//! `parameterized`) are exactly such subs, called several times per monadic
//! operation (#17331).
//!
//! An entry now also records the unit a compunit-private winner came from.
//! It is written under `Some(unit)` only when the resolver found the routine
//! in `current_unit`'s own private table, and read only by a call from that
//! same `current_unit`, so a hit answers what the resolver's
//! `current_unit_private_routine` step would have answered. Every other
//! reason a name skips the name caches -- a `&name` parameter shadowing it, a
//! `sub EXPORT` override, a routine shadowing `MAIN`'s, a module-provided
//! routine whose visibility rests on an import scope, an import of the name
//! into an open unit scope -- keeps it off this cache as well.

use super::*;

/// `(name, callsite package, unit)`. `unit` is `None` for an ordinary name
/// and `Some(unit)` for a compunit-private routine of `unit`. The package is
/// in the key, not only checked against the entry: a module's helper called
/// alternately from two packages' methods (Monad's `parameterized` from
/// `Monad::Maybe` and from a subtype role) would otherwise evict its own
/// entry on every call.
pub(crate) type OtfCallKey = (Symbol, Symbol, Option<Symbol>);

/// The defining package a hit runs the body under, and the body.
pub(crate) struct OtfCallEntry {
    pub(crate) def_pkg: Symbol,
    pub(crate) cf: Arc<CompiledFunction>,
}

impl Interpreter {
    /// The unit key a call to `name` from `code` may use the OTF call cache
    /// under, when the name is skipped by the name caches only because it is
    /// a compunit-private routine. `None` keeps the call off the cache.
    // Cost: O(s + p), s = open import scopes, p = packages a bare name is
    // searched in (both only while an import scope is open); O(1) otherwise.
    pub(crate) fn otf_unit_private_key(&self, code: &CompiledCode, name: Symbol) -> Option<Symbol> {
        let module = &self.module;
        if module.unit_private_names.is_empty()
            || !module.unit_private_names.contains(&name)
            || self.lexicals.amp_param_shadowed_names.contains(&name)
            || module.export_amp_override_names.contains(&name)
            || module
                .module_visibility
                .module_routine_providers
                .contains_key(&name)
            || self.is_main_shadowing_routine(name)
            || code.routine_shadowed_calls.contains(&name)
            || self.imported_in_open_unit_scope(name)
        {
            return None;
        }
        Some(self.current_unit)
    }

    /// The unit key an OTF cache entry for `def`, just resolved by its bare
    /// name, is written under: `None` for an ordinary name, `Some(unit)` when
    /// `def` is `current_unit`'s own compunit-private routine. A routine the
    /// resolver reached any other way (the frame-dependent fallback of the
    /// private lookup) gets no entry at all: `Err(())`.
    // Cost: O(e), e = nesting depth of `EVAL` units (bounded at 64).
    pub(crate) fn otf_entry_unit_for(&self, def: &FunctionDef) -> Result<Option<Symbol>, ()> {
        let name = def.name;
        if self.module.unit_private_names.is_empty() || !self.module.unit_private_names.contains(&name)
        {
            return Ok(None);
        }
        match self.unit_private_routine_from(self.current_unit, name) {
            Some(found) if std::ptr::eq(Arc::as_ptr(&found), def) => Ok(Some(self.current_unit)),
            Some(_) => Err(()),
            // Not a private routine of this unit: the name's other resolutions
            // never read the cache, so an ordinary entry is harmless.
            None => Ok(None),
        }
    }
}
