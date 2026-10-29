//! The inline cache of a by-name `@!x` / `%!x` store (`SetGlobal`).
//!
//! An `@`/`%` attribute has no local slot, so `%!children = %ch` reaches the VM
//! as `SetGlobal("%!children")` and the general resolution
//! ([`Interpreter::with_self_attr`]) finds the attribute by name on every
//! execution. The store's name is a constant of the chunk, so the chunk's
//! attribute cache ([`super::vm_attr_site`]) keeps one word per constant after
//! the local-slot words ([`CompiledCode::global_attr_site`]): the same
//! `(layout, slot)` answer the general resolution gives, replayed under the
//! same guards.

use super::*;

impl Interpreter {
    /// The value of `self`'s `@`/`%` attribute `name`, read through the cache
    /// word `site` of `code`; the general read (which fills the word) on a miss.
    // Cost: O(1) on a hit (one atomic load, one slot read); the general read's
    // cost on a miss.
    pub(super) fn read_self_attr_cell_at(
        &self,
        code: &CompiledCode,
        site: usize,
        name: &str,
    ) -> Option<Value> {
        let self_val = self.attr_access_self(code);
        if let Some(sv) = &self_val
            && let Some(v) = self.read_attr_site_cached(code, site, sv)
        {
            return Some(v);
        }
        let (bare, is_private) = Self::attr_twigil_base(name)?;
        let sigil = crate::value::attr_twigil_sigil(name).unwrap_or('$');
        self.read_attr_cell_by_key(
            self_val,
            Some((code, site)),
            crate::symbol::Symbol::intern(bare),
            is_private,
            sigil,
        )
    }

    /// [`Self::write_self_attr_cell`] through the cache word `site` of `code`.
    // Cost: O(1) on a hit (one atomic load, one slot store); the general
    // write's cost on a miss.
    pub(super) fn write_self_attr_cell_at(
        &self,
        code: &CompiledCode,
        site: usize,
        name: &str,
        val: Value,
    ) {
        let self_val = self.attr_access_self(code);
        let val = match &self_val {
            Some(sv) => match self.write_attr_site_cached(code, site, sv, val) {
                Ok(()) => return,
                Err(val) => val,
            },
            None => val,
        };
        let Some((bare, is_private)) = Self::attr_twigil_base(name) else {
            return;
        };
        let sigil = crate::value::attr_twigil_sigil(name).unwrap_or('$');
        self.write_attr_cell_by_key(
            self_val,
            Some((code, site)),
            crate::symbol::Symbol::intern(bare),
            is_private,
            sigil,
            val,
        );
    }
}
