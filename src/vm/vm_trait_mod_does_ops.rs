//! `__mutsu_trait_mod_does_apply` — the native primitive behind the
//! `trait_mod:<does>` CORE.setting prelude (`runtime::run::TRAIT_MOD_DOES_PRELUDE`,
//! injected via `runtime::run_prelude::inject_trait_mod_does_prelude`).
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
            }
            return Ok(mixed);
        }
        let mixed = self.vm_does_values(doee, role)?;
        if self.trait_mod_writeback_key.is_some() && matches!(mixed.view(), ValueView::Mixin(..)) {
            self.trait_mod_writeback_value = Some(mixed.clone());
        }
        Ok(mixed)
    }
}
