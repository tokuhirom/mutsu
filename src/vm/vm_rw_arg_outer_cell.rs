//! The container an `is rw` / `:=` argument binds to when the variable is a
//! free variable owned by an *outer store* rather than a frame slot.
//!
//! Three stores outlive the scope that declared their lexicals and are the
//! authoritative home of a free-variable read from inside the routines that
//! close over them — `GetGlobal` consults each BEFORE `env`:
//!
//! * `escaped_our_lexical_cells` — a bare block's `my` captured by an `our sub`
//!   (`escaping_our_read` / `escaping_our_write_cell`);
//! * `package_lexicals` — a `module M { my $x; ... }` block's `my`
//!   (`package_scope_lexical`);
//! * `unit_lexicals` — a loaded `unit module`'s file-scope `my`
//!   (`unit_lexical_slot`).
//!
//! `WrapVarRef` used to tag such an argument with the value it read and leave
//! the container to a by-name write-back into the calling routine's env — a
//! copy no later read consults, so `sub set($f is rw) { $f = ... }; our sub go
//! { set($x) }` silently lost the write (or, for a typed variable whose read
//! produced a bare type object, died with "expects a writable container").
//! Handing the callee the store's own cell makes the write land where every
//! read looks (#10372).

use super::*;

impl Interpreter {
    /// The store-owned `ContainerRef` cell behind the free variable `sym`, or
    /// `None` when no outer store owns it (an ordinary env / closure lexical,
    /// which `exec_wrap_var_ref_op`'s other paths handle). Probes in
    /// `GetGlobal`'s order so the cell is the one a read of the name resolves.
    ///
    /// A `package_lexicals` entry recorded as a plain value (its block never
    /// boxed it, because no nested routine *assigns* it — a call argument is
    /// not a syntactic write) is promoted to a cell in place: every reader
    /// resolves the name through that entry first and derefs a cell, so the
    /// promotion is invisible except that the callee's write now sticks.
    // Cost: O(d + p), d = package nesting depth of the running routine (the
    // unit-lexical chain walk), p = escaping-`our` names (both stores are
    // empty — a pair of `is_empty` checks — for ordinary programs).
    pub(super) fn outer_store_lexical_cell(
        &mut self,
        code: &CompiledCode,
        sym: crate::symbol::Symbol,
    ) -> Option<Value> {
        let name = sym.as_str();
        if !self.escaping_our_lexical_names.is_empty()
            && let Some(cell) = self.escaping_our_write_cell(code, name)
        {
            return Some(cell);
        }
        if let Some(stored) = self.package_scope_lexical(name) {
            if stored.is_container_ref() {
                return Some(stored);
            }
            return self.promote_package_lexical_to_cell(sym, stored);
        }
        // Only a module routine's compunit lexical: a mainline sub's captured
        // cells (ADR-0024's `UNIT<mainline>` bucket) and a routine-nested
        // sub's aliases already reach `WrapVarRef` through their own capture
        // paths, which keep a same-named shadow in the caller apart.
        if crate::qualified::is_global_package(self.current_package_sym()) {
            return None;
        }
        self.unit_lexical_slot(name)
            .filter(|v| v.is_container_ref())
            .cloned()
    }

    /// Replace the current package's plain `package_lexicals` entry `name`
    /// with a `ContainerRef` cell holding `stored`, and return the cell. Only
    /// a bare scalar name is promoted: the qualified spelling
    /// `package_scope_lexical` also accepts comes from a body compiled under
    /// the plain package name, which does not pass rw arguments by `WrapVarRef`
    /// slot sentinel, and an `@`/`%` binds its container value directly.
    pub(crate) fn promote_package_lexical_to_cell(
        &mut self,
        sym: crate::symbol::Symbol,
        stored: Value,
    ) -> Option<Value> {
        let name = sym.as_str();
        if name.starts_with(['@', '%', '&']) || crate::qualified::is_qualified(sym) {
            return None;
        }
        let container = stored.into_container_ref();
        self.register_container_cell_constraint_for_name(&container, name);
        let pkg = self.current_package();
        self.package_lexicals_cow_mut()
            .entry(pkg)
            .or_default()
            .insert(name.to_string(), container.clone());
        Some(container)
    }
}
