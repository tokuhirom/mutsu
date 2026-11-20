//! A private aggregate attribute (`@!x` / `%!x`) addressed through a local
//! slot (ADR-18356 D2).
//!
//! A scalar `$!x` has always had a slot: its reads and writes go through
//! `GetLocal`/`SetLocal` and reach `self`'s attribute cell through the
//! chunk's `(layout, slot)` site cache (ADR-0121 D3). The aggregate forms
//! were compiled by name (`GetArrayVar("@!x")`, `IndexAssignExprNamed`,
//! `CallMethodMut` with no slot), so every access paid a by-name env probe, a
//! snapshot of the cell and a mirror back into it. Giving them a slot lets the
//! slot forms of the container ops (`ArrayPush`, `IndexAssignLocal`,
//! `CallMethodMut`, `IndexElemAutoviv`) apply. The VM refreshes the slot from
//! the cell before each such op and writes the cell back only when the op
//! rebound the slot (`vm_attr_aggregate_slot`). The slot is therefore a
//! reference to the container the instance holds, never a copy that needs
//! reconciling.

use super::Compiler;

impl Compiler {
    /// Whether `name` is a private aggregate attribute twigil (`@!x`, `%!x`).
    // Cost: O(n), n = the name's length.
    pub(super) fn is_private_aggregate_attr(name: &str) -> bool {
        (name.starts_with("@!") || name.starts_with("%!"))
            && matches!(crate::value::attr_twigil_base(name), Some((_, true)))
    }

    /// The local slot of the private aggregate attribute `name`, allocated on
    /// first use; `None` for any other name.
    ///
    /// The slot is kept out of `local_map`: only the sites that opt in (a
    /// read, `push`, a `CallMethodMut` receiver) address the attribute
    /// through it. Every other op on the name -- the element stores, `++` on
    /// an element, autovivification -- still compiles by name and reaches
    /// the cell through the by-name attribute path.
    // Cost: O(l) on the first use in a chunk, l = its locals; O(n) after.
    pub(super) fn private_aggregate_attr_slot(&mut self, name: &str) -> Option<u32> {
        if !Self::is_private_aggregate_attr(name) {
            return None;
        }
        if let Some(&slot) = self.local_map.get(name) {
            return Some(slot);
        }
        if let Some(slot) = self.code.locals.iter().position(|local| local == name) {
            return Some(slot as u32);
        }
        let slot = self.code.locals.len() as u32;
        self.code.locals.push(name.to_string());
        self.code
            .binding_descs
            .push(crate::binding_desc::BindingDesc::new(
                crate::binding_desc::BindingFlags::new(false, false),
            ));
        Some(slot)
    }
}
