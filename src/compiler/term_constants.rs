//! Compile-time side of the term namespace for sigil-less constants
//! (`runtime::term_names`, #9962).
//!
//! A sigil-less `constant b` owns its local slot under the term key `\b`, so a
//! same-named `$b` (a `my`, a parameter) gets a slot of its own and neither
//! declaration can shadow the other. These helpers are the only places the
//! compiler maps between a constant's spelling and its storage key.

use super::Compiler;

impl Compiler {
    /// The package-store name of an `our` declaration spelled `spelled` whose
    /// lexical storage key is `storage`. A qualified store (`Pkg::b`) is keyed
    /// by the spelling; an unqualified one (a GLOBAL-scope `constant b`) IS the
    /// lexical key, so it stays in the term namespace.
    pub(super) fn qualify_our_storage_name(&self, spelled: &str, storage: &str) -> String {
        let qualified = self.qualify_our_variable_name(spelled);
        if qualified == spelled {
            storage.to_string()
        } else {
            qualified
        }
    }

    /// The local slot of the in-scope sigil-less constant spelled `name`, when
    /// the bareword `name` reads it: the constant is in scope and no sigil-less
    /// binding of the same spelling (`my \b`, a `\b` parameter) is.
    // Cost: O(1) expected.
    pub(super) fn term_constant_slot(&self, name: &str) -> Option<u32> {
        if !self.constant_vars_in_scope.contains(name) || self.sigilless_locals.contains(name) {
            return None;
        }
        self.local_map
            .get(crate::runtime::term_names::term_key(name).as_str())
            .copied()
    }

    /// Whether the bareword `name` names a sigil-less constant visible here —
    /// declared in this unit or an enclosing one — rather than a sigil-less
    /// binding (`my \b`, a `\b` parameter) of the same spelling.
    // Cost: O(1) expected.
    pub(super) fn names_term_constant(&self, name: &str) -> bool {
        (self.constant_vars_in_scope.contains(name) || self.outer_constant_names.contains(name))
            && !self.sigilless_locals.contains(name)
            && !self.enclosing_sigilless.contains(name)
    }

    /// Whether a sigil-less assignment target `name` names no binding this
    /// unit can see — no local, no sigil-less binding here or in an enclosing
    /// scope. Such a write (typically an `EVAL`'d `b = 5`) is compiled against
    /// the term key and resolved by the VM.
    // Cost: O(1) expected.
    pub(super) fn sigilless_target_is_unknown(&self, name: &str) -> bool {
        !name.starts_with(['$', '@', '%', '&', '!', '.', '*', '?', '^'])
            && !crate::runtime::utils::has_double_colon(name)
            && !self.local_map.contains_key(name)
            && !self.sigilless_locals.contains(name)
            && !self.enclosing_sigilless.contains(name)
            && !self.enclosing_local_names.contains(name)
    }

    /// The storage key of the sigil-less term `name` used as an lvalue root
    /// (`x[0] = 1`): its term key when it names a sigil-less constant, its
    /// spelling (a `my \x` / `\x` binding) otherwise.
    // Cost: O(|name|).
    pub(super) fn sigilless_storage_key(&self, name: &str) -> String {
        if self.names_term_constant(name) {
            crate::runtime::term_names::term_key(name)
        } else {
            name.to_string()
        }
    }

    /// [`crate::ast::Expr::lvalue_root`] resolved to a storage key: a
    /// sigil-less root goes through [`Compiler::sigilless_storage_key`].
    // Cost: O(w + |name|), w = wrappers peeled.
    pub(super) fn lvalue_root_key(
        &self,
        target: &crate::ast::Expr,
        peel: crate::ast::LvaluePeel,
    ) -> Option<String> {
        Some(match target.lvalue_root(peel)? {
            crate::ast::LvalueRoot::Key(key) => key,
            crate::ast::LvalueRoot::Sigilless(name) => self.sigilless_storage_key(name),
        })
    }
}
