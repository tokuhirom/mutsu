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

    /// A lexical type declaration (`my class NAME`, `my role NAME`) shadows a
    /// same-named sigil-less constant of an enclosing scope for the rest of
    /// its block (#11517): stop reading or inlining the constant, and have a
    /// bareword `NAME` read the type's lexical binding (see
    /// [`Compiler::lexical_type_shadows`]). Only a name that is a visible
    /// constant is recorded; any other bareword keeps `GetBareWord`'s
    /// resolution order.
    // Cost: O(|name|).
    pub(super) fn shadow_constant_with_lexical_type(&mut self, name: &str) {
        if !(self.constant_vars_in_scope.contains(name) || self.outer_constant_names.contains(name))
        {
            return;
        }
        self.constant_vars_in_scope.remove(name);
        self.forget_constant(name);
        self.lexical_type_shadows.insert(name.to_string());
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

    /// Whether the bareword `name` names a sigilless binding (`my \x`, a `\x`
    /// parameter, a `-> \x` pointy parameter) visible here — in this scope or
    /// an enclosing one. Such a binding lives under its term key (ADR-12441).
    // Cost: O(1) expected.
    pub(super) fn names_sigilless_binding(&self, name: &str) -> bool {
        self.sigilless_locals.contains(name) || self.enclosing_sigilless.contains(name)
    }

    /// The storage key of the target of a `name = …` statement whose source
    /// spelled it without a sigil (`Stmt::Assign { target_is_sigilless }`):
    /// the term key when `name` is a sigilless binding (ADR-12441), an
    /// in-scope constant (#9962), or nothing this unit can see (resolved by
    /// the VM); `None` when the plain key stands.
    // Cost: O(|name|).
    pub(super) fn sigilless_assign_target_key(&self, name: &str) -> Option<String> {
        (self.names_sigilless_binding(name)
            || self.names_term_constant(name)
            || self.sigilless_target_is_unknown(name))
        .then(|| crate::runtime::term_names::term_key(name))
    }

    /// The storage key of the sigil-less term `name` used as an lvalue root
    /// (`x[0] = 1`): its term key when it names a sigilless binding or a
    /// sigil-less constant, its spelling otherwise.
    // Cost: O(|name|).
    pub(super) fn sigilless_storage_key(&self, name: &str) -> String {
        if self.names_sigilless_binding(name) || self.names_term_constant(name) {
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

    /// [`Self::lvalue_root_key`] for the root of a subscript ASSIGNMENT
    /// (`b<a> = 3`): a sigil-less root this unit cannot see at all (an
    /// `EVAL`'d assignment over the caller's `my \b`) is written against its
    /// term key, as a `b = 3` statement is
    /// ([`Self::sigilless_assign_target_key`]) and the VM resolves it. Reads and
    /// method-call roots (`Pair.new`) keep the plain key.
    // Cost: O(w + |name|), w = wrappers peeled.
    pub(super) fn assign_root_key(
        &self,
        target: &crate::ast::Expr,
        peel: crate::ast::LvaluePeel,
    ) -> Option<String> {
        Some(match target.lvalue_root(peel)? {
            crate::ast::LvalueRoot::Key(key) => key,
            crate::ast::LvalueRoot::Sigilless(name) if self.sigilless_target_is_unknown(name) => {
                crate::runtime::term_names::term_key(name)
            }
            crate::ast::LvalueRoot::Sigilless(name) => self.sigilless_storage_key(name),
        })
    }
}
