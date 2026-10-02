//! `&name` scoping: telling a routine's own `&name` from a caller's
//! same-named binding that is merely visible in the by-name env (#10638).

use super::*;

impl Interpreter {
    /// Whether the env's `&name` entry is a binding this `code` merely
    /// inherited from its caller, shadowing a package sub of the same name.
    ///
    /// A routine runs with its caller's by-name env visible, so a caller's
    /// `my &tab-up = -> |c { $obj.tab-up(|c) }` was what `&tab-up(|c)` in
    /// `method tab-up(|c)` read -- the method then called itself forever
    /// (#10638). `&name` is lexical: unless this code captured `&name` (it is
    /// in `free_var_syms`) or it is a `sub EXPORT`-installed import of another
    /// unit, the package sub wins. The same rule a bare `name(...)` call
    /// applies (`exec_call_func_op`'s `lexical_override`).
    ///
    /// Cost: O(1) when env has no `&name` entry (one hashed probe); otherwise
    /// O(1) amortized: a local-slot probe plus registry probes (the
    /// proto/multi probes are memoized).
    fn inherited_amp_shadow(&mut self, code: &CompiledCode, name: &str) -> bool {
        let name_sym = Symbol::intern(name);
        if crate::qualified::is_qualified(name_sym)
            || name.contains(":<")
            || name.starts_with(['!', '?', '*', '.'])
        {
            return false;
        }
        let Some(val) =
            crate::runtime::dispatch_key::with_amp_name(name, |amp| self.env().get(amp).cloned())
        else {
            return false;
        };
        if !Self::env_callable_is_lexical_override(&val, name) {
            return false;
        }
        let captured = crate::runtime::dispatch_key::with_amp_name(name, |amp| {
            Symbol::lookup(amp).is_some_and(|sym| code.free_var_syms.contains(&sym))
        });
        if captured {
            return false;
        }
        // This frame's own `&name` slot (a `&g` parameter) is its lexical.
        if self.find_local_slot(code, &format!("&{name}")).is_some() {
            return false;
        }
        !(self.export_amp_override_names.contains(&name_sym)
            && !Self::callable_declared_in_unit_of(&val, code))
    }

    /// `&name`'s value for `code`, skipping an inherited caller binding
    /// ([`Self::inherited_amp_shadow`]) when the name resolves to something
    /// else in scope: the reading compunit's import of it (which beats a
    /// caller's same-named env entry, as for any imported alias) or a
    /// registered routine. With no such alternative the env entry still
    /// answers, as before.
    ///
    /// Cost: as `resolve_code_var`, plus [`Self::inherited_amp_shadow`].
    pub(super) fn resolve_amp_var_for(&mut self, code: &CompiledCode, name: &str) -> Value {
        if self.inherited_amp_shadow(code, name) {
            if let Some(imported) = crate::runtime::dispatch_key::with_amp_name(name, |amp| {
                self.module_imported_lexical(amp).cloned()
            }) {
                return imported.into_deref();
            }
            let val = loan_env!(self, resolve_code_var_unshadowed(name));
            if !val.is_nil() {
                return val;
            }
        }
        loan_env!(self, resolve_code_var(name))
    }
}
