//! Whether a user `ACCEPTS` takes part in `$topic ~~ $matcher`.
//!
//! A class that declares `multi method ACCEPTS(...)` candidates (and no
//! `proto`/`only`) ADDS them to the ones it inherits from `Any`/`Mu`, as for
//! any multi method in Rakudo. So a topic none of the user candidates binds is
//! matched by the core candidates — `self === topic` for a defined matcher —
//! instead of dying with "Cannot resolve caller". Tinky's `Transition` has
//! `ACCEPTS(State:D)` and `ACCEPTS(Object:D)` only, and a suite greps a list
//! of transitions with another transition.

use super::*;

impl Interpreter {
    /// Should `$matcher.ACCEPTS($topic)` run the user method rather than the
    /// core smartmatch? True when a user candidate binds the topic, or when a
    /// non-multi `ACCEPTS` owns the name outright (its binding failure is the
    /// error Rakudo reports).
    // Cost: O(m * c), m = MRO length of `class_name`, c = ACCEPTS candidates.
    pub(crate) fn user_accepts_applies(
        &mut self,
        class_name: &str,
        matcher: &Value,
        topic: &Value,
    ) -> Result<bool, RuntimeError> {
        self.user_method_applies(class_name, "ACCEPTS", matcher, std::slice::from_ref(topic))
    }

    /// [`Self::user_accepts_applies`] for any method: should
    /// `invocant.<method>(|args)` run the user method of `class_name` rather
    /// than the built-in one it overrides? A user `multi` adds candidates to
    /// the inherited ones, so it wins only when one of them binds.
    // Cost: O(m * c), m = MRO length of `class_name`, c = `method` candidates.
    pub(crate) fn user_method_applies(
        &mut self,
        class_name: &str,
        method: &str,
        invocant: &Value,
        args: &[Value],
    ) -> Result<bool, RuntimeError> {
        if self
            .resolve_method_with_owner_invocant(class_name, method, args, invocant)?
            .is_some()
        {
            return Ok(true);
        }
        let mro = self.class_mro(class_name);
        let only_multis = mro.iter().all(|owner| {
            self.registry()
                .user_method_overloads(owner.as_str(), method)
                .is_none_or(|defs| defs.iter().all(|def| def.is_multi))
        });
        Ok(!only_multis)
    }
}
