//! The light call paths' check for a `UInt` constraint
//! ([`crate::opcode::FastParamType::UInt`], #10110).
//!
//! `UInt` is core's `subset UInt of Int where { not .DEFINITE or $_ >= 0 }`.
//! For the values it is almost always given -- a plain `Int`, a big integer or
//! a `Bool` -- the answer is a sign test. Every other shape (a type object, an
//! allomorph, an enum value, an `Int` subclass) goes to the full type matcher,
//! so a light call answers exactly what the general binder answers.

use super::*;

impl Interpreter {
    /// The `UInt` answer for a value whose shape alone decides it, or `None`.
    // Cost: O(1).
    #[inline]
    pub(super) fn uint_inline_answer(val: &Value) -> Option<bool> {
        match val.view() {
            ValueView::Int(i) => Some(i >= 0),
            ValueView::BigInt(b) => Some(b.sign() != num_bigint::Sign::Minus),
            // `False` is 0 and `True` is 1: both are non-negative `Int`s.
            ValueView::Bool(_) => Some(true),
            _ => None,
        }
    }

    /// Does `val` satisfy a `UInt` parameter or return constraint?
    // Cost: O(1) for an `Int`/`BigInt`/`Bool`; otherwise O(m) in
    // `type_matches_value`, m = the value's type MRO.
    pub(crate) fn light_uint_check(&mut self, val: &Value) -> bool {
        let val = val.descalarize();
        match Self::uint_inline_answer(val) {
            Some(answer) => answer,
            None => self.type_matches_value("UInt", val),
        }
    }

    /// [`Self::fast_type_check_tagged`] for a bind site that has the
    /// interpreter at hand, so a `UInt` shape the tagged form cannot decide
    /// inline is decided by the type matcher instead of being refused.
    // Cost: O(1), or `light_uint_check`'s for a `UInt` constraint.
    #[inline]
    pub(crate) fn light_param_type_check(
        &mut self,
        val: &Value,
        kind: crate::opcode::FastParamType,
        name_sym: Symbol,
    ) -> bool {
        if kind == crate::opcode::FastParamType::UInt {
            self.light_uint_check(val)
        } else {
            Self::fast_type_check_tagged(val, kind, name_sym)
                || self.light_instance_of_builtin(val, name_sym.as_str())
        }
    }

    /// An object of a user class that inherits a builtin
    /// (`class TextString { also is Str }`) is no builtin value shape, so the
    /// shape-based fast checks refuse it; its MRO decides.
    // Cost: O(m) in `type_matches_value`, m = the value's type MRO; O(1) for any
    // other shape.
    fn light_instance_of_builtin(&mut self, val: &Value, type_name: &str) -> bool {
        matches!(val.view(), ValueView::Instance { .. }) && self.type_matches_value(type_name, val)
    }

    /// [`Self::light_return_type_check_tagged`] for the light return check:
    /// `Nil` and a `Failure` pass any return type, and a `UInt` return is
    /// checked like a `UInt` parameter.
    // Cost: O(1), or `light_uint_check`'s for a `UInt` return type.
    #[inline]
    pub(super) fn light_return_check(
        &mut self,
        val: &Value,
        kind: crate::opcode::FastParamType,
        name_sym: Symbol,
    ) -> bool {
        if kind != crate::opcode::FastParamType::UInt {
            return Self::light_return_type_check_tagged(val, kind, name_sym);
        }
        if val.is_nil() {
            return true;
        }
        if let ValueView::Instance { class_name, .. } = val.view()
            && class_name.as_str() == "Failure"
        {
            return true;
        }
        self.light_uint_check(val)
    }

    /// [`Self::fast_type_check`] for a by-name bind site with the interpreter
    /// at hand; see [`Self::light_param_type_check`].
    // Cost: O(1), or `light_uint_check`'s for a `UInt` constraint.
    pub(super) fn light_type_check_by_name(&mut self, val: &Value, type_name: &str) -> bool {
        if type_name == "UInt" {
            self.light_uint_check(val)
        } else {
            Self::fast_type_check(val, type_name) || self.light_instance_of_builtin(val, type_name)
        }
    }

    /// The binding error a light bind raises for an argument that failed
    /// parameter `pd`'s constraint `tc`: the general binder's
    /// `X::TypeCheck::Binding::Parameter`, worded as a *constraint* failure
    /// when an `Int` failed `UInt`'s `where` (rakudo: "Constraint type check
    /// failed in binding to parameter '$x'; expected UInt but got Int (-1)").
    // Cost: O(m) in `type_matches_value`, m = the value's type MRO.
    pub(super) fn light_param_type_failure(
        &mut self,
        pd: &crate::ast::ParamDef,
        tc: &str,
        val: &Value,
    ) -> RuntimeError {
        if tc == "UInt" && self.type_matches_value("Int", val) {
            return self.subset_constraint_binding_error(pd, &pd.name, tc, tc, val);
        }
        self.typecheck_binding_parameter_failure(
            &crate::runtime::types::param_display_name(pd),
            tc,
            val,
        )
        .with_parameter_object(pd, Some(&*self))
    }
}
