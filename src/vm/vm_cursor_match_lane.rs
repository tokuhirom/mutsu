//! The method-table lane for a grammar cursor's `Match` (#17385).
//!
//! A regex match has the `Match` dispatch shape only when it is plain
//! (`MatchNode::is_plain`): a grammar cursor reports the grammar's own class,
//! which may declare any `Match` method itself, so it has no shape and every
//! method call on it (`$<x>.ast` in each action body, `.from`, `.Str`) ran
//! the whole `CallMethodMut` probe chain: ~16k instructions for a `.ast`
//! against ~1k on a plain match.
//!
//! # The guard
//!
//! The lane takes every guard of the shaped lane (it runs after them), and:
//!
//! - the receiver is a lazy `Match` whose cursor class is a grammar;
//! - that grammar's MRO declares no user method of the called name
//!   (`user_shadows_builtin`, the probe the full path asks, which looks the
//!   name up under the cursor's class) and user code has not `augment`ed
//!   `Match` with it (`native_lever_a_user_override_sym`).
//!
//! Then the receiver is answered as a `Match`: the shaped lane looks up the
//! `Match` row exactly as it does for a plain match.
//!
//! # The memo
//!
//! Both checks depend only on the cursor's class, the method and the
//! registry, so the verdict is remembered per `(class, method)` for one
//! registry write generation in the `caches` subsystem (per interpreter, so
//! per thread: no lock).
//!
//! In debug builds the shaped lane's answer is cross-checked against the full
//! path (`check_method_site_lane`), which covers this lane too.

use super::*;
use crate::builtins::method_table::Receiver;
use crate::value::DispatchShape;

impl Interpreter {
    /// The `Match` receiver for the cursor at `self.stack[base]` when its
    /// grammar leaves `method` to `Match`, or `None` to run the full path.
    // Cost: O(1) on a memo hit (one hash probe); a miss adds the user-method
    // probe over the grammar's MRO.
    pub(super) fn cursor_match_lane_receiver(
        &mut self,
        method: Symbol,
        base: usize,
    ) -> Option<Receiver> {
        let target = &self.stack[base];
        if !target.is_lazy_match_value() {
            return None;
        }
        let class = Symbol::intern(target.match_dispatch_class());
        let generation = self.registry_write_generation();
        let key = (class, method);
        if let Some(&(memo_gen, ok)) = self.caches.cursor_match_site_lane.get(&key)
            && memo_gen == generation
        {
            return ok.then_some(Receiver::instance(DispatchShape::Match));
        }
        let target = target.clone();
        // A probe: a dying resolution declines the lane; the generic call
        // path resolves the method again and reports it.
        let ok = !self
            .user_shadows_builtin(&target, method, &[], false)
            .unwrap_or(true)
            && !self.native_lever_a_user_override_sym(&target, method);
        if self.dispatch.native_base_bypass.is_none() {
            self.caches
                .cursor_match_site_lane
                .insert(key, (generation, ok));
        }
        ok.then_some(Receiver::instance(DispatchShape::Match))
    }
}
