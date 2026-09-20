//! `__mutsu_trait_mod_does_apply` and `__mutsu_attribute_set_default` — the
//! native primitives behind the `trait_mod:<does>` and `trait_mod:<is>
//! (Attribute, :$default!)` CORE.setting preludes
//! (`runtime::run::TRAIT_MOD_DOES_PRELUDE`, `TRAIT_MOD_IS_DEFAULT_PRELUDE`,
//! injected via `runtime::run_prelude::inject_trait_mod_does_prelude`,
//! `inject_trait_mod_is_default_prelude`).
//!
//! `trait_mod:<does>` is Raku's callable form of the `does` mixin operator —
//! real Rakudo declares three overloads (verified against `raku`):
//!
//! ```text
//! multi sub trait_mod:<does>(Mu:U $doee, Mu:U $role)
//! multi sub trait_mod:<does>(Attribute:D $a, Mu:U $role)
//! multi sub trait_mod:<does>(Variable:D $v, Mu:U $role)
//! ```
//!
//! Dists like `Hash::Restricted` and `Injector` call the `Variable:D` overload
//! from inside a custom `trait_mod:<is>` handler to mix a role into a
//! *declared variable's* value at `is`-trait time (`my %h is restricted = ...`).
//! That overload is the one this primitive gives real behavior to: the other
//! two exist mainly so the three-candidate multi genuinely collides with a
//! user-declared candidate of the same name the way real Raku's does (see the
//! prelude's own doc comment) — nothing in the corpus that motivated this file
//! calls them, so they fall back to the plain `does` mixin with no
//! variable-reflection step.

use super::*;

impl Interpreter {
    /// `__mutsu_trait_mod_does_apply($doee, $role)` — the Rust half of every
    /// `trait_mod:<does>` prelude candidate. `None` for any other function
    /// name, so the caller falls through to its remaining dispatch (matching
    /// the shape of `try_cglobal_fetch`/`try_nativecast` in
    /// `vm_call_func_ops`).
    pub(super) fn try_trait_mod_does_apply(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != "__mutsu_trait_mod_does_apply" {
            return None;
        }
        if args.len() != 2 {
            return Some(Err(RuntimeError::new(format!(
                "__mutsu_trait_mod_does_apply expects 2 arguments, got {}",
                args.len()
            ))));
        }
        Some(self.apply_trait_mod_does(args[0].clone(), args[1].clone()))
    }

    /// `__mutsu_attribute_set_default($attr, $default)` — the Rust half of
    /// the `trait_mod:<is>(Attribute:D $attr, :$default!)` prelude candidate
    /// (`runtime::run::TRAIT_MOD_IS_DEFAULT_PRELUDE`). `None` for any other
    /// function name, matching the shape of `try_trait_mod_does_apply` above.
    ///
    /// Relays `$default` through `trait_mod_default_writeback` rather than
    /// mutating `$attr` itself: `apply_class_body_attribute_traits` drains it
    /// right after dispatching an attribute's own custom traits and folds it
    /// into that attribute's compiled default. A call reached with no
    /// attribute-trait dispatch in progress (a bare `trait_mod:<is>($attr,
    /// :default($v))` outside a `has` handler) leaves a value nobody reads —
    /// a harmless no-op, matching how real Rakudo's own default machinery is
    /// meaningless there too.
    pub(super) fn try_trait_mod_set_default(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != "__mutsu_attribute_set_default" {
            return None;
        }
        if args.len() != 2 {
            return Some(Err(RuntimeError::new(format!(
                "__mutsu_attribute_set_default expects 2 arguments, got {}",
                args.len()
            ))));
        }
        self.trait_mod_default_writeback = Some(args[1].clone());
        Some(Ok(Value::NIL))
    }

    /// Mix `role` into `doee`, the same way the `does` operator does
    /// (`vm_does_values`), with one extra step for the `Variable:D` overload:
    /// when `doee` is a `.VAR` reflection object (carries `__mutsu_var_target`,
    /// see `Interpreter::var_target_from_meta_value`), the mixin is applied to
    /// the CURRENT live value of the variable it reflects — read fresh from
    /// `env` rather than from whatever snapshot `doee` itself carries — and the
    /// result is written straight back into that same `env` slot. That write
    /// is what makes a same-handler re-read (`v.var` again, e.g.
    /// `Hash::Restricted`'s `v.var.WHAT.^set_name(...)` right after) already
    /// see the mixed value.
    ///
    /// Reaching the ORIGINAL CALLER's variable (several frames further up, at
    /// the `my %h is restricted = ...` declaration site) needs one more step
    /// this function cannot perform itself: it has no access to that frame's
    /// compiled local slot. So it reuses the existing `trait_mod_writeback_key`/
    /// `trait_mod_writeback_value` relay (`runtime::mod`) — previously armed
    /// only around a Routine's `trait_mod:<is>` dispatch
    /// (`registration_sub.rs`) — which `vm_var_trait_ops::exec_apply_var_trait_op`
    /// now also arms around the Variable-trait dispatch, and drains after the
    /// call returns using its own `code`/slot context to perform the real
    /// local-slot write.
    fn apply_trait_mod_does(&mut self, doee: Value, role: Value) -> Result<Value, RuntimeError> {
        if let Some(var_name) = Self::var_target_from_meta_value(&doee) {
            let current = self.env().get(&var_name).cloned().unwrap_or(Value::NIL);
            let mixed = self.vm_does_values(current, role)?;
            self.set_env_with_main_alias(&var_name, mixed.clone());
            if self.trait_mod_writeback_key.is_some() {
                self.trait_mod_writeback_value = Some(mixed.clone());
                // See `trait_mod_attr_writeback_value`'s doc comment.
                if Self::how_target_from_value(&mixed).is_none() {
                    self.trait_mod_attr_writeback_value = Some(mixed.clone());
                }
            }
            return Ok(mixed);
        }
        let mixed = self.vm_does_values(doee, role)?;
        if self.trait_mod_writeback_key.is_some() && matches!(mixed.view(), ValueView::Mixin(..)) {
            self.trait_mod_writeback_value = Some(mixed.clone());
            if Self::how_target_from_value(&mixed).is_none() {
                self.trait_mod_attr_writeback_value = Some(mixed.clone());
            }
        }
        Ok(mixed)
    }
}
