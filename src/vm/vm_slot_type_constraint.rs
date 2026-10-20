//! A local's type constraint, read from the code that declares it before the
//! by-name record (ADR-13757).
//!
//! A store's type check used to ask only for the record under the variable's
//! name (`__mutsu_type::x`). When the running frame had none, that walk went
//! on into the frames that called it and found a caller's unrelated
//! same-named variable. A slot this code declares without a constraint
//! (`DeclaredConstraint::Untyped`) has no record to find: the store skips the
//! probe. A slot this code declares otherwise (`BindingDesc::declared_here`)
//! reads its record in the running frame only: a typed declaration of this
//! code wrote it there. Only a slot the code does not declare (a free
//! variable, an aliasing parameter) asks by name through the whole lookup.

use crate::binding_desc::DeclaredConstraint;
use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{Value, VarRefDeclared};

impl Interpreter {
    /// The constraint a store into `code`'s slot `idx` (named `name`)
    /// checks against.
    // Cost: O(1) for a slot the code declares untyped; else
    // `slot_type_constraint_value`'s probe.
    pub(super) fn slot_type_constraint(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        name: &str,
        name_sym: Option<Symbol>,
    ) -> Option<String> {
        match self
            .slot_type_constraint_value(code, idx, name, name_sym)?
            .view()
        {
            crate::value::ValueView::Str(tc) => Some(tc.as_str().to_owned()),
            _ => None,
        }
    }

    /// [`Self::slot_type_constraint`] as the env's own `Str` value
    /// (`var_type_constraint_value_for`).
    // Cost: O(1) for a slot the code declares untyped; else the record
    // probe, O(d), d = the running frame's tiers for a slot the code declares
    // and the env tiers walked for any other.
    pub(super) fn slot_type_constraint_value(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        name: &str,
        name_sym: Option<Symbol>,
    ) -> Option<Value> {
        if code.declared_constraint(idx) == DeclaredConstraint::Untyped {
            return None;
        }
        if code.declares_slot(idx) {
            let sym = name_sym.unwrap_or_else(|| Symbol::intern(name));
            return loan_env!(self, own_var_type_constraint_value_sym(sym));
        }
        loan_env!(self, var_type_constraint_value_for(name, name_sym))
    }

    /// The object-hash key type a store into `code`'s slot `idx` (named
    /// `name`) checks against: a slot the code declares reads only the
    /// running frame's record (and the unit's own lane), any other slot asks
    /// by name through the whole lookup.
    // Cost: O(1) expected for a slot the code declares; else
    // `var_hash_key_constraint_sym`'s probe, O(d), d = the env tiers walked.
    pub(super) fn slot_hash_key_constraint(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        name: &str,
        name_sym: Option<Symbol>,
    ) -> Option<String> {
        let sym = name_sym.unwrap_or_else(|| Symbol::intern(name));
        if code.declares_slot(idx) {
            return self.own_var_hash_key_constraint_sym(sym);
        }
        loan_env!(self, var_hash_key_constraint_sym(name, sym))
    }

    /// The declared type of the variable a `WrapVarRef` in `code` tags, for
    /// the `VarRef` to carry: resolved here, in the variable's own frame, so
    /// the binder and the multi dispatcher never probe for it by name from
    /// the callee's frame. `slot` is the site's local slot, `u32::MAX` for a
    /// variable that is not a local of `code`.
    // Cost: O(1) for a slot the code declares untyped or a name no typed
    // declaration has used; else the record probe, O(d), d = the running
    // frame's tiers for a slot the code declares, the env tiers walked else.
    pub(super) fn wrap_var_ref_declared(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_sym: Symbol,
    ) -> VarRefDeclared {
        if slot != u32::MAX
            && code.declared_constraint(slot as usize) == DeclaredConstraint::Untyped
        {
            return VarRefDeclared::Resolved(None);
        }
        if slot != u32::MAX && code.declares_slot(slot as usize) {
            return VarRefDeclared::Resolved(loan_env!(
                self,
                own_var_type_constraint_value_sym(name_sym)
            ));
        }
        VarRefDeclared::Resolved(loan_env!(self, var_type_constraint_value_sym(name_sym)))
    }

    /// The declared type of the variable the `VarRef` argument `varref`
    /// (named `name_sym`) names: what its `WrapVarRef` site resolved, or the
    /// by-name record for a `VarRef` built without one.
    // Cost: O(1) for a resolved `VarRef`; else the record probe, O(d).
    pub(crate) fn varref_type_constraint_value(
        &self,
        varref: &Value,
        name_sym: Symbol,
    ) -> Option<Value> {
        match varref.varref_declared() {
            Some(VarRefDeclared::Resolved(declared)) => declared,
            _ => self.var_type_constraint_value_sym(name_sym),
        }
    }

    /// [`Self::varref_type_constraint_value`] as an owned `String`.
    // Cost: as `varref_type_constraint_value`, plus a copy of the name.
    pub(crate) fn varref_type_constraint(
        &self,
        varref: &Value,
        name_sym: Symbol,
    ) -> Option<String> {
        match self.varref_type_constraint_value(varref, name_sym)?.view() {
            crate::value::ValueView::Str(tc) => Some(tc.as_str().to_owned()),
            _ => None,
        }
    }

    /// The constraint a store into the binding `name` checks against, for a
    /// method-table row that writes a receiver's binding (`Any.push`
    /// autovivifying `my Int $x`, #13823): read off the slot `code`
    /// declares when the call came through the VM (ADR-13757), else by name.
    // Cost: as `slot_type_constraint`, plus a slot lookup by name, O(l),
    // l = `code`'s locals.
    pub(crate) fn binding_store_constraint(
        &mut self,
        code: Option<&CompiledCode>,
        name: &str,
    ) -> Option<String> {
        if let Some(code) = code
            && let Some(idx) = self.find_local_slot(code, name)
        {
            return self.slot_type_constraint(code, idx, name, None);
        }
        self.var_type_constraint(name)
    }
}
