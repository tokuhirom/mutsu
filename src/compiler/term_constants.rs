//! Compile-time side of the term namespace for sigil-less constants
//! (`runtime::term_names`, #9962).
//!
//! A sigil-less `constant b` owns its local slot under the term key `\b`, so a
//! same-named `$b` (a `my`, a parameter) gets a slot of its own and neither
//! declaration can shadow the other. These helpers are the only places the
//! compiler maps between a constant's spelling and its storage key.

use super::Compiler;
use crate::opcode::OpCode;
use crate::symbol::Symbol;
use crate::value::Value;

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

    /// A scalar declaration `my $name` while a sigilless binding `\name` is
    /// visible: both live under the key `name`, so copy the sigilless value
    /// into a slot under the term key before the scalar overwrites it, and
    /// route bare `name` reads there (#11994).
    // Cost: O(|name|).
    pub(super) fn shadow_sigilless_term(&mut self, name: &str) {
        if name.is_empty()
            || !name.starts_with(|c: char| c.is_alphabetic() || c == '_')
            || crate::qualified::is_qualified(Symbol::intern(name))
            || self.shadowed_sigilless_terms.contains(name)
        {
            return;
        }
        // The sigilless binding is either a local of this frame that already
        // owns a slot, or an enclosing one. A sigilless name with NO slot yet
        // is being declared by this very `VarDecl` (a `-> \x` or `my (\a, \b)`
        // binding is a `VarDecl` of the bare name), not shadowed by it.
        let slot = self.local_map.get(name).copied();
        // Only a binding the program wrote itself counts (`sigilless_declared`):
        // the `for`/`with`/`if` pointy parameters of the same spelling are
        // compiler-synthesized `VarDecl`s and register in `sigilless_locals`
        // too, but are rebinding, not shadowing.
        let own = self.sigilless_locals.contains(name);
        if own && (slot.is_none() || !self.sigilless_declared.contains(name))
            || !own && !self.enclosing_sigilless.contains(name)
        {
            return;
        }
        match slot.filter(|_| own) {
            Some(slot) => {
                self.code.emit(OpCode::GetLocal(slot));
            }
            None => {
                self.code.shadowed_sigilless_reads.push(name.to_string());
                let name_idx = self.code.add_constant(Value::str(name.to_string()));
                self.code.emit(OpCode::GetGlobal(name_idx));
            }
        }
        let slot = self.alloc_local(&crate::runtime::term_names::term_key(name));
        self.code.emit(OpCode::SetLocal(slot));
        self.shadowed_sigilless_terms.insert(name.to_string());
        self.sigilless_locals.remove(name);
    }

    /// The read of a bare `name` whose sigilless binding a scalar declaration
    /// shadowed (see [`Compiler::shadow_sigilless_term`]); `false` when
    /// `name` is not such a name.
    // Cost: O(1) expected.
    pub(super) fn emit_shadowed_sigilless_read(&mut self, name: &str) -> bool {
        if !self.shadowed_sigilless_terms.contains(name) {
            return false;
        }
        let key = crate::runtime::term_names::term_key(name);
        if let Some(&slot) = self.local_map.get(key.as_str()) {
            self.code.emit(OpCode::GetLocal(slot));
        } else {
            if !self.code.shadowed_sigilless_reads.contains(&key) {
                self.code.shadowed_sigilless_reads.push(key.clone());
            }
            let idx = self.code.add_constant(Value::str(key));
            self.code.emit(OpCode::GetGlobal(idx));
        }
        true
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
