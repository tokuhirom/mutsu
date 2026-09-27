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
    ) -> bool {
        if self
            .resolve_method_with_owner_invocant(
                class_name,
                "ACCEPTS",
                std::slice::from_ref(topic),
                matcher,
            )
            .is_some()
        {
            return true;
        }
        let mro = self.class_mro(class_name);
        let only_multis = mro.iter().all(|owner| {
            self.registry()
                .user_method_overloads(owner.as_str(), "ACCEPTS")
                .is_none_or(|defs| defs.iter().all(|def| def.is_multi))
        });
        !only_multis
    }
}
