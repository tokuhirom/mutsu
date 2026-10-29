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

/// What one `@!x = ...` / `%!x = ...` store knows about its target attribute,
/// resolved once so the helpers the store runs (the tied-container probe, the
/// current-container read of the coercion, the key and element type checks)
/// do not each resolve the attribute again.
pub(super) struct GlobalAttrSite<'a> {
    pub(super) code: &'a CompiledCode,
    /// The chunk's cache word of this store ([`CompiledCode::global_attr_site`]).
    pub(super) site: usize,
    /// The attribute cell's value before the store, when `prefetched`.
    cell: Option<Value>,
    /// The attribute's declared type constraint, when `prefetched`.
    pub(super) type_constraint: Option<String>,
    prefetched: bool,
    /// Whether the frame has a `self` (a prefetched site only).
    has_self: bool,
}

impl<'a> GlobalAttrSite<'a> {
    /// Whether `type_constraint` was read (a plain `=` store).
    // Cost: O(1).
    pub(super) fn has_prefetched_type(&self) -> bool {
        self.prefetched
    }

    /// Whether the frame lacks a `self`, when the site was prefetched (`None`
    /// for a site that did not look).
    // Cost: O(1).
    pub(super) fn lacks_self(&self) -> Option<bool> {
        self.prefetched.then_some(!self.has_self)
    }

    /// The site of a store that only needs the cache word (a bind).
    // Cost: O(1).
    pub(super) fn bare(code: &'a CompiledCode, site: usize) -> Self {
        GlobalAttrSite {
            code,
            site,
            cell: None,
            type_constraint: None,
            prefetched: false,
            has_self: false,
        }
    }
}

impl Interpreter {
    /// The site of a plain `=` store to the attribute `name`: the cell's
    /// current value and the declared type, read once.
    // Cost: O(1) on a cache hit, plus the memoized constraint lookup.
    pub(super) fn prefetch_global_attr_site<'a>(
        &self,
        code: &'a CompiledCode,
        site: usize,
        name: &str,
        name_sym: crate::symbol::Symbol,
    ) -> GlobalAttrSite<'a> {
        let self_val = self.attr_access_self(code);
        let cell = match &self_val {
            Some(sv) => self
                .read_attr_site_cached(code, site, sv)
                .or_else(|| self.read_self_attr_cell_at(code, site, name)),
            None => self.read_self_attr_cell_at(code, site, name),
        };
        let type_constraint = match &self_val {
            Some(sv) => self.self_attr_type_constraint_of(sv, name, name_sym),
            None => None,
        };
        GlobalAttrSite {
            code,
            site,
            cell,
            type_constraint,
            prefetched: true,
            has_self: self_val.is_some(),
        }
    }

    /// The prefetched cell value, if the site prefetched one that exists.
    // Cost: O(1).
    pub(super) fn global_attr_site_prefetched_cell(
        &self,
        at: &GlobalAttrSite<'_>,
    ) -> Option<Value> {
        if at.prefetched { at.cell.clone() } else { None }
    }

    /// The attribute cell's value: the prefetched one, else a fresh read.
    // Cost: O(1) when prefetched, as `read_self_attr_cell_at` otherwise.
    pub(super) fn global_attr_site_cell(
        &self,
        at: &GlobalAttrSite<'_>,
        name: &str,
    ) -> Option<Value> {
        if at.prefetched {
            at.cell.clone()
        } else {
            self.read_self_attr_cell_at(at.code, at.site, name)
        }
    }

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
