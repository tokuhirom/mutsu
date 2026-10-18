//! `__mutsu_custom_store($value)`: whether `.=` on a method-call lvalue
//! stores through the value's own `STORE` (ADR-11276 §9.174).
//!
//! The parser desugars `$obj.attr .= meth` (and `X.=meth` on a non-lvalue) to
//! `$cur.STORE($cur.meth)` when the value is a custom container, and to an
//! assignment through the accessor otherwise. A custom container is a `Proxy`
//! or a value whose user class or composed role declares `STORE`. The built-in
//! containers declare `STORE` too (Rakudo's `Array.^can('STORE')` is true), but
//! assigning to a `$` attribute that holds one rebinds it: `self.x .= Hash`
//! stores the `Hash`, it does not pour its pairs into the old `Array`. So the
//! question is the user's `STORE`, not `.^can`.

use super::*;

impl Interpreter {
    // Cost: O(m), m = methods of the value's class and roles probed for `STORE`.
    pub(super) fn builtin_custom_store(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(value) = args.first() else {
            return Ok(Value::FALSE);
        };
        let custom = match value.view() {
            _ if value.is_proxy_value() => true,
            ValueView::Mixin(..) => self.mixin_composes_method(value, "STORE"),
            ValueView::Instance { class_name, .. } => {
                self.has_user_method_including_role(&class_name.resolve(), "STORE")
            }
            ValueView::Package(name) => {
                self.has_user_method_including_role(&name.resolve(), "STORE")
            }
            _ => false,
        };
        Ok(Value::truth(custom))
    }
}
