//! Which `&name` reads have no lexical `&name` binding in scope (#10997).
//!
//! A routine runs with its caller's by-name env visible, so the env lookup
//! behind `GetCodeVar` / `CallOnCodeVar` cannot tell "my enclosing scope's
//! `my &g`" from "my caller's `my &g`". The compiler can: it knows every scope
//! enclosing the read site. When none of them binds `&g`, the read names the
//! routine declared as `g`, and the VM must not substitute a caller's binding
//! for it (`Interpreter::imported_amp_over_inherited`).

use super::Compiler;
use crate::symbol::Symbol;

impl Compiler {
    /// Record `name` in `CompiledCode::unscoped_amp_reads` when this `&name`
    /// read site provably sees no lexical `&name`: no `&name` local in this
    /// frame (including a closed sibling block's -- `local_map` is monotonic,
    /// which only makes the test more conservative), none in an active or
    /// enclosing scope, none among the `&`-lexicals active in the enclosing
    /// compilations (`outer_code_var_names` -- a method body's only view of
    /// them, since it does not inherit `enclosing_scopes`), no class-body
    /// static `&name`, and no role `&name` type parameter.
    ///
    /// `enclosing_local_names` is deliberately not consulted: it is built from
    /// the enclosing frames' monotonic `local_map`, so it still lists a `my &g`
    /// of an already-closed sibling block -- exactly the caller-side binding
    /// this check exists to see past.
    ///
    /// Recorded only when `lexical_scope_known`: a body compiled out of its
    /// context does not see its enclosing scopes, so "no binding seen" proves
    /// nothing there.
    ///
    /// Cost: O(s), s = scopes in the chain (one hashed probe per scope).
    pub(super) fn note_unscoped_amp_read(&mut self, name: &str) {
        if !self.lexical_scope_known
            || !name.starts_with(|c: char| c.is_alphabetic() || c == '_')
            || name.contains(':')
        {
            return;
        }
        let key = format!("&{name}");
        let bound = self.local_map.contains_key(&key)
            || self.amp_binding_in_active_scope(name)
            || self.code.outer_code_var_names.contains(&key)
            || self.class_body_static_code_vars.contains(&key)
            || self
                .role_param_scope
                .as_ref()
                .is_some_and(|frame| frame.contains_key(&key));
        if bound {
            return;
        }
        let sym = Symbol::intern(name);
        if !self.code.unscoped_amp_reads.contains(&sym) {
            self.code.unscoped_amp_reads.push(sym);
        }
    }

    /// Seed the `&`-lexicals visible at an `EVAL` call site, so a routine or
    /// closure the EVAL'd text declares records a read of one (`&g()`, a bare
    /// `g()`) as a capture, exactly as it would written inline (#11154). The
    /// EVAL compiler is fresh and cannot see the caller's scopes; the caller
    /// passes the names from its env instead.
    ///
    /// Cost: O(n), n = `names`.
    pub(crate) fn seed_outer_code_var_names(&mut self, names: impl IntoIterator<Item = String>) {
        self.code.outer_code_var_names.extend(names);
    }
}
