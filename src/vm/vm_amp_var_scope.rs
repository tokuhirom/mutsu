//! `&name` scoping: telling a routine's own `&name` from a caller's
//! same-named binding that is merely visible in the by-name env (#10638).

use super::*;

impl Interpreter {
    /// What `&name` names for the reading `code` when the env's `&name` entry
    /// is a binding `code` merely inherited from a caller in another
    /// compunit: `code`'s import of it, or the registered routine.
    ///
    /// A routine runs with its caller's by-name env visible, so a caller's
    /// `my &tab-up = -> |c { $obj.tab-up(|c) }` (EVAL'd template code) was
    /// what `&tab-up(|c)` read in `method tab-up(|c)` of a module that imports
    /// `sub tab-up` -- the method then called itself forever (#10638). An
    /// imported alias beats a caller's same-named env entry, as it does for
    /// other imported names (`module_imported_lexical`). The env entry is
    /// kept when this code captured `&name` (`free_var_syms`), binds it in
    /// its own slot (a `&name` parameter), or the binding was declared in
    /// this code's own compunit (a role's `&name` type parameter lives only in
    /// env, so a same-unit binding cannot be told apart from a lexical one).
    ///
    /// Cost: O(1) when env has no `&name` entry (one hashed probe); otherwise
    /// as `resolve_code_var` (a set probe, a local-slot probe and an
    /// import-table probe first).
    fn imported_amp_over_inherited(&self, code: &CompiledCode, name: &str) -> Option<Value> {
        if name.contains(":<") || name.starts_with(['!', '?', '*', '.']) {
            return None;
        }
        let val =
            crate::runtime::dispatch_key::with_amp_name(name, |amp| self.env().get(amp).cloned())?;
        if !Self::env_callable_is_lexical_override(&val, name)
            || Self::callable_declared_in_unit_of(&val, code)
        {
            return None;
        }
        let name_sym = Symbol::intern(name);
        if crate::qualified::is_qualified(name_sym)
            || self.export_amp_override_names.contains(&name_sym)
        {
            return None;
        }
        let inherited = crate::runtime::dispatch_key::with_amp_name(name, |amp| {
            let captured = Symbol::lookup(amp).is_some_and(|sym| code.free_var_syms.contains(&sym));
            !captured && self.find_local_slot(code, amp).is_none()
        });
        if !inherited {
            return None;
        }
        if let Some(imported) = crate::runtime::dispatch_key::with_amp_name(name, |amp| {
            self.module_imported_lexical(amp).cloned()
        }) {
            return Some(imported.into_deref());
        }
        Some(self.resolve_code_var_unshadowed(name)).filter(|v| !v.is_nil())
    }

    /// `&name`'s value for `code`: [`Self::imported_amp_over_inherited`]
    /// when it applies, else the usual by-name resolution.
    ///
    /// Cost: as `resolve_code_var`, plus [`Self::imported_amp_over_inherited`].
    pub(super) fn resolve_amp_var_for(&mut self, code: &CompiledCode, name: &str) -> Value {
        if let Some(imported) = self.imported_amp_over_inherited(code, name) {
            return imported;
        }
        loan_env!(self, resolve_code_var(name))
    }

    /// The binding a routine's free `&name` closes over when it lives in a
    /// declaration-scoped store rather than in the running frame: a mainline
    /// or block sub's captured unit-lexical cell (ADR-0024, #10483), or a
    /// class/package body's own `my &name` (`package_lexicals`). Consulted
    /// before the by-name env, where a CALLER's same-named `my &name` sits
    /// -- reading that one is dynamic scoping (URI::Template's `uri-encode`
    /// called from a method whose `my &enc` shadowed the class body's).
    /// Scalars resolve through the same two stores ahead of env (`GetGlobal`).
    ///
    /// A `&name` slot of the running `code` itself (a parameter, or a
    /// `my &name` of this frame) shadows both stores, so none is consulted
    /// then.
    ///
    /// Cost: O(1), three hashed probes (the two stores answer immediately
    /// when empty).
    pub(super) fn declared_scope_amp_var_for(
        &self,
        code: &CompiledCode,
        name: &str,
    ) -> Option<Value> {
        if crate::qualified::is_qualified(Symbol::intern(name))
            || name.starts_with(['!', '?', '*', '.'])
        {
            return None;
        }
        crate::runtime::dispatch_key::with_amp_name(name, |amp| {
            if self.find_local_slot(code, amp).is_some() {
                return None;
            }
            self.unit_scope_lexical(amp)
                .or_else(|| self.package_scope_lexical(amp))
        })
        .map(Value::into_deref)
        .filter(|v| !v.is_nil())
    }
}
