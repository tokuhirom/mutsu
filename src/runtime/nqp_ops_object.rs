//! `nqp::findmethod` / `nqp::tryfindmethod` (#11499): method lookup by name
//! through the same resolver `.^find_method` uses (`classhow_find_method`), so
//! the two cannot disagree. The other object-model ops compile to Raku method
//! calls (`compiler/nqp_object_forms.rs`).

use crate::runtime::{Interpreter, RuntimeError};
use crate::value::{Value, ValueView};

impl Interpreter {
    /// Run `findmethod` / `tryfindmethod`; `None` for any other name.
    pub(crate) fn call_nqp_op_object(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let try_only = match op {
            "findmethod" => false,
            "tryfindmethod" => true,
            "rebless" => return Some(self.nqp_rebless(args)),
            "setparameterizer" => return Some(self.nqp_set_parameterizer(args.to_vec())),
            "parameterizetype" => return Some(self.nqp_parameterize_type(args.to_vec())),
            "typeparameterized" | "typeparameters" | "typeparameterat" => {
                return Some(self.nqp_type_parameter_op(op, args));
            }
            _ => return None,
        };
        let obj = args.first().cloned().unwrap_or(Value::NIL);
        let name = args.get(1).map(Value::to_string_value).unwrap_or_default();
        // nqp::findmethod($obj, $name) — the method object, dying when there
        // is none; nqp::tryfindmethod answers null (Nil) instead.
        // Cost: as `.^find_method`: O(1) on a method-cache hit, O(d) on a
        // miss, d = the MRO's depth.
        Some(match self.classhow_find_method(&obj, &name) {
            Some(method) => Ok(method),
            None if try_only => Ok(Value::NIL),
            None => Err(RuntimeError::new(format!(
                "Cannot find method '{name}' on object of type {}",
                crate::runtime::utils::value_type_name(&obj)
            ))),
        })
    }

    /// `typeparameterized` / `typeparameters` / `typeparameterat`: what the
    /// parameterizer was given when it made the type.
    // Cost: O(1).
    fn nqp_type_parameter_op(&mut self, op: &str, args: &[Value]) -> Result<Value, RuntimeError> {
        let ty = args.first().cloned().unwrap_or(Value::NIL);
        let from = self.type_parameterization(&ty);
        if op == "typeparameterized" {
            // The root, or null for a type that is not a parameterization.
            return Ok(from.map_or(Value::NIL, |(root, _)| root));
        }
        let Some((_, params)) = from else {
            return Err(RuntimeError::new("This type is not parameterized"));
        };
        if op == "typeparameters" {
            return Ok(params);
        }
        let idx = args.get(1).map_or(0, crate::value::numeric_coerce::to_int);
        let item = match params.view() {
            ValueView::Array(items, _) => usize::try_from(idx).ok().and_then(|i| items.get(i).cloned()),
            _ => None,
        };
        Ok(item.unwrap_or(Value::NIL))
    }

    /// `nqp::rebless($obj, $type)`: give `$obj` the type `$type`, which must be
    /// a mixin type (`$obj.^mixin(R)`'s type object), as Rakudo requires. The
    /// roles of `$type` are composed onto `$obj` the way `.^mixin` / `does`
    /// compose them on a real object (`eval_does_values_mutating`), so every
    /// alias of the object sees them. Answers `$obj`.
    // Cost: O(r * c), r = roles of the type, c = the cost of composing one role.
    fn nqp_rebless(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let operand = |i: usize| {
            crate::runtime::types::unwrap_varref_value(args.get(i).cloned().unwrap_or(Value::NIL))
                .deref_container()
        };
        let (obj, ty) = (operand(0), operand(1));
        let name_of = |v: &Value| match v.view() {
            ValueView::Package(name) => name.resolve().to_string(),
            ValueView::Instance { class_name, .. } => class_name.resolve().to_string(),
            _ => crate::runtime::utils::value_type_name(v).to_string(),
        };
        let ValueView::Mixin(_, mixins) = ty.view() else {
            // Rebless to the object's own type changes nothing.
            if matches!(ty.view(), ValueView::Package(_)) && name_of(&ty) == name_of(&obj) {
                return Ok(obj);
            }
            return Err(RuntimeError::new(format!(
                "New type {} for {} is not a mixin type",
                name_of(&ty),
                name_of(&obj)
            )));
        };
        // `.^roles` order is last applied first; compose in application order.
        let mut result = obj;
        for name in crate::value::types::mixin_roles_applied_last_first(mixins, "")
            .iter()
            .rev()
        {
            let role = Value::package(crate::symbol::Symbol::intern(name));
            if !self.is_role_application(&role) {
                continue;
            }
            result = self.eval_does_values_mutating(result, role)?;
        }
        Ok(result)
    }
}
