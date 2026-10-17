//! A local's type constraint, read from the code that declares it before the
//! by-name record (ADR-13757).
//!
//! A store's type check used to ask only for the record under the variable's
//! name (`__mutsu_type::x`). When the running frame had none, that walk went
//! on into the frames that called it and found a caller's unrelated
//! same-named variable. A slot this code declares without a constraint
//! (`DeclaredConstraint::Untyped`) has no record to find: the store skips the
//! probe. Every other slot keeps the by-name record, which a typed
//! declaration of this code wrote into its own frame.

use crate::binding_desc::DeclaredConstraint;
use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::Value;

impl Interpreter {
    /// The constraint a store into `code`'s slot `idx` (named `name`)
    /// checks against.
    // Cost: O(1) for a slot the code declares untyped; else
    // `var_type_constraint_for`'s env probe, O(d), d = env tiers walked.
    pub(super) fn slot_type_constraint(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        name: &str,
        name_sym: Option<Symbol>,
    ) -> Option<String> {
        if code.declared_constraint(idx) == DeclaredConstraint::Untyped {
            return None;
        }
        loan_env!(self, var_type_constraint_for(name, name_sym))
    }

    /// [`Self::slot_type_constraint`] as the env's own `Str` value
    /// (`var_type_constraint_value_for`).
    // Cost: as `slot_type_constraint`.
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
        loan_env!(self, var_type_constraint_value_for(name, name_sym))
    }
}
