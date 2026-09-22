//! The `CallMethodMut` "plain user method" lane (#8880).
//!
//! A method call on a named receiver walks a long chain of speculative probes
//! before anything dispatches: `try_proto_method_body`, the exception-message
//! delegate, `try_fast_accessor_read` (which itself runs
//! `attribute_accessor_owner`, `cstruct_class_name` and
//! `resolve_user_method_or_accessor`), `try_env_pure_mut_dispatch`, and then, a
//! function later, the twelve `IO::Handle`/`IO::Path` probes plus
//! `is_native_method` / `has_user_method`. Each one asks, by name, whether this
//! receiver might be some *other* kind of thing, and for an ordinary
//! `class C { method m() {...} }` every one of them answers "no" on every call.
//! Measured on the issue's own repro (`$o.m()`, 100 000 iterations) that prefix
//! is **~5,100 of the 19,336 instructions** a method call costs.
//!
//! The prefix cannot be made cheap probe by probe -- the chain *is* the design,
//! and most of the probes already carry a memo of their own. So this is a cache
//! **in front of** the chain rather than a sixteenth one inside it: one
//! `(receiver class, method name)` set that says "the whole prefix has been
//! observed inert for this pair; go straight to the user-method dispatch".
//!
//! # Why the memo cannot certify something that did not happen
//!
//! Every probe in the prefix *returns* when it claims a call. So a dispatch that
//! reaches the user-method tail
//! ([`Interpreter::compiled_mut_resolved_dispatch`]) has, by construction,
//! been declined by every probe in between -- there is no path to the tail that
//! skips one. Reaching the tail is therefore the proof, and the memo is written
//! exactly there ([`Interpreter::note_plain_method_lane_reached`]) rather than
//! from a hand-audited list of conditions that would rot the next time a probe
//! is added.
//!
//! # What the key has to hold constant
//!
//! The memo replays a verdict, so everything the prefix reads must be either in
//! the key or pinned by the gate:
//!
//! * **the method name** and **the receiver's class** are the key;
//! * **`args.is_empty()`**, **no `.^`/`.!`/`."…"` modifier**, **not quoted** and
//!   **no accessor-ref marker** are required by the gate on both the install and
//!   the replay, so no probe's argument-shaped or call-shaped early-out can
//!   differ between them;
//! * **the registry** (methods, roles, wraps, MRO, attributes) is pinned by
//!   `Registry::method_generation` -- the same latch the sibling method caches
//!   use, cleared in `refresh_method_caches_for_generation`;
//! * **per-instance state** is ruled out by refusing to install for the three
//!   class families whose probes read the *instance* rather than the class: an
//!   `IO::Handle`/`IO::Path` in the MRO (`try_native_io_handle_method` keys on
//!   the live `handle` attribute), a CStruct class (fields live in native
//!   memory, not the attribute map), and any class the program did not declare
//!   itself.
//!
//! A stale entry can only ever cost a *skipped probe*, never a wrong dispatch:
//! the lane runs the identical `resolve_method_cached` -> wrap-chain ->
//! `call_compiled_method` tail the full path ends in, and falls back to the
//! whole path unchanged when that tail declines.

use super::*;

impl Interpreter {
    /// The `(class, method)` key this dispatch is eligible to replay or install,
    /// or `None` when the call shape is outside the lane.
    ///
    /// Deliberately cheap and allocation-free: it runs on every `CallMethodMut`,
    /// including the ones that will miss.
    pub(super) fn plain_method_lane_key(
        target: &Value,
        args: &[Value],
        modifier: Option<&str>,
        quoted: bool,
        want_ref: bool,
        method_sym: crate::symbol::Symbol,
    ) -> Option<(crate::symbol::Symbol, crate::symbol::Symbol)> {
        if !args.is_empty() || modifier.is_some() || quoted || want_ref {
            return None;
        }
        match target.view() {
            ValueView::Instance { class_name, .. } => Some((class_name, method_sym)),
            _ => None,
        }
    }

    /// Whether the prefix has already been proven inert for this pair.
    pub(super) fn plain_method_lane_hit(
        &mut self,
        key: (crate::symbol::Symbol, crate::symbol::Symbol),
    ) -> bool {
        self.refresh_method_caches_for_generation();
        self.plain_method_lane.contains(&key)
    }

    /// Dispatch a lane hit: everything the skipped stretch does that is *not* a
    /// probe, then the same user-method dispatch the full path ends in.
    ///
    /// Two non-probe effects live inside the skipped stretch and are reproduced
    /// here. `flatten_scoped_env` collapses a transient scoped overlay env,
    /// because a compiled method body may capture or iterate the caller's
    /// lexicals and would otherwise see a truncated view. `method_dispatch_pure
    /// = false` is the Slice 6.3 seed: assume the dispatch dirties the caller
    /// env unless a proven-pure path clears it.
    pub(super) fn run_plain_method_lane(
        &mut self,
        code: &CompiledCode,
        target_name: &str,
        target: Value,
        method: &str,
        method_sym: crate::symbol::Symbol,
    ) -> Result<(), RuntimeError> {
        self.flatten_scoped_env();
        self.method_dispatch_pure = false;
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
        self.plain_method_lane_active = true;
        let call_result = self.dispatch_compiled_method_mut_with_raw_invocant(
            code,
            target_name,
            target,
            method,
            method_sym,
            Vec::new(),
        );
        // The flag is consumed by `try_compiled_method_mut_or_interpret_sym`;
        // clear it unconditionally so an error path that never reached it (the
        // native-stack guard, a panic-free early return) cannot leak the lane
        // into the next dispatch.
        self.plain_method_lane_active = false;
        if let Err(e) = &call_result
            && Self::is_method_not_found_error(e)
        {
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "notfound");
        }
        self.stack.push(call_result?);
        Ok(())
    }

    /// Record that the prefix was inert for this pair, if the pair is the one
    /// the current dispatch's gate nominated.
    ///
    /// The candidate is cleared by every `CallMethodMut` gate, so a nested
    /// dispatch run from inside a probe cannot leave its own key behind for an
    /// outer call to install; and the equality check means an outer call can
    /// only ever install the key it was nominated with.
    pub(super) fn note_plain_method_lane_reached(
        &mut self,
        class_sym: crate::symbol::Symbol,
        method_sym: crate::symbol::Symbol,
    ) {
        if self.plain_method_lane_candidate != Some((class_sym, method_sym)) {
            return;
        }
        self.plain_method_lane_candidate = None;
        if !self.plain_method_lane_class_eligible(class_sym) {
            return;
        }
        self.refresh_method_caches_for_generation();
        self.plain_method_lane.insert((class_sym, method_sym));
    }

    /// The three class families whose prefix probes read the *instance* (or a
    /// registry the method generation does not cover) rather than the class, and
    /// so cannot have a class-keyed verdict replayed for them.
    ///
    /// Runs at most once per `(class, method)` pair -- only on the install side,
    /// never on the gate -- so the `resolve()`/MRO walk here is not on any hot
    /// path.
    fn plain_method_lane_class_eligible(&mut self, class_sym: crate::symbol::Symbol) -> bool {
        let class_name = class_sym.resolve();
        if !self.user_declared_classes.contains(class_name.as_str()) {
            return false;
        }
        if self.cstruct_class_name(&class_name).is_some() {
            return false;
        }
        !self
            .class_mro(&class_name)
            .iter()
            .any(|c| c == "IO::Handle" || c == "IO::Path")
    }
}
