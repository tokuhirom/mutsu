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
//! * **the method name**, **the receiver's class** and **the arguments' type
//!   keys** ([`Interpreter::multi_arg_type_keys`]: runtime type plus
//!   definedness per argument) are the key. A call with arguments is admitted
//!   only when every argument has a type key, so a `Junction`, a named `Pair`,
//!   a container or a mixin -- the shapes the probes that read arguments
//!   (autothreading, the named-argument intercepts) decide on -- never enters
//!   the lane. Every other probe in the skipped stretch reads at most the
//!   argument *count* for a user-class receiver, which the key fixes (#10111);
//! * **no `.^`/`.!`/`."…"` modifier**, **not quoted** and **no accessor-ref
//!   marker** are required by the gate on both the install and the replay, so
//!   no probe's call-shaped early-out can differ between them;
//! * **the native-method cascade** (`try_native_method`), the one probe in the
//!   stretch whose decline depends on argument *values*, is consulted only
//!   when the class has no user method of the name. A call with arguments
//!   installs only when the class has one, so that probe was never asked;
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
    /// The key this dispatch is eligible to replay or install, or `None` when
    /// the call shape is outside the lane.
    ///
    /// Runs on every `CallMethodMut` on an instance receiver, including the
    /// ones that will miss; allocation-free for a call without arguments.
    // Cost: O(a), a = arguments (one type-key intern each).
    pub(super) fn plain_method_lane_key(
        &mut self,
        target: &Value,
        args: &[Value],
        modifier: Option<&str>,
        quoted: bool,
        want_ref: bool,
        method_sym: crate::symbol::Symbol,
    ) -> Option<crate::runtime::PlainMethodLaneKey> {
        // A private call (`.!`) is admitted: its method symbol is the
        // `!`-prefixed name, so it can never share a key with a public call,
        // and its replay re-runs the private dispatch arm that installed it
        // (`try_private_compiled_mut_dispatch`), including that arm's
        // calling-context permission check.
        if !matches!(modifier, None | Some("!")) || quoted || want_ref {
            return None;
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        let arg_keys = if args.is_empty() {
            Vec::new()
        } else {
            self.multi_arg_type_keys(args)?
        };
        Some((class_name, method_sym, arg_keys))
    }

    /// Whether the prefix has already been proven inert for this key.
    pub(super) fn plain_method_lane_hit(
        &mut self,
        key: &crate::runtime::PlainMethodLaneKey,
    ) -> bool {
        self.refresh_method_caches_for_generation();
        self.caches.plain_method_lane.contains(key)
    }

    /// Dispatch a lane hit: everything the skipped stretch does that is *not* a
    /// probe, then the same user-method dispatch the full path ends in.
    ///
    /// One non-probe effect of the skipped stretch is reproduced here:
    /// `method_dispatch_pure = false` is the Slice 6.3 seed (assume the
    /// dispatch dirties the caller env unless a proven-pure path clears it).
    ///
    /// The stretch's other effect, `flatten_scoped_env`, is deliberately NOT
    /// reproduced (#9494). It guards consumers that capture or iterate the
    /// whole lexical view, and a lane hit reaches none of them before the
    /// callee runs: the lane goes straight to `compiled_mut_resolved_dispatch`,
    /// whose user-method arm runs a compiled body exactly the way a sub call
    /// runs one -- a fresh `Env::scoped_child` over the caller's (possibly
    /// scoped) env, with closure captures flattening at their own capture site
    /// and the return merge reading the callee's overlay / frame-write log. Sub
    /// calls have never flattened at the call. The tail's non-user fallbacks
    /// (the native forks and the interpreter) are not covered by that argument,
    /// so the tail flattens before reaching them. Flattening here cost a
    /// whole-scope map clone per call and, worse, left the CALLER's frame flat
    /// for the rest of its life, turning its own return merge scope-sized
    /// (#7630, #7563).
    pub(super) fn run_plain_method_lane(
        &mut self,
        code: &CompiledCode,
        target_name: &str,
        target: Value,
        method: &str,
        method_sym: crate::symbol::Symbol,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        self.method_dispatch_pure = false;
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
        self.caches.plain_method_lane_active = true;
        let call_result = self.dispatch_compiled_method_mut_with_raw_invocant(
            code,
            target_name,
            target,
            method,
            method_sym,
            args,
        );
        // The flag is consumed by `try_compiled_method_mut_or_interpret_sym`;
        // clear it unconditionally so an error path that never reached it (the
        // native-stack guard, a panic-free early return) cannot leak the lane
        // into the next dispatch.
        self.caches.plain_method_lane_active = false;
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
        let Some(key) = self
            .caches
            .plain_method_lane_candidate
            .take_if(|key| key.0 == class_sym && key.1 == method_sym)
        else {
            return;
        };
        if !self.plain_method_lane_class_eligible(class_sym) {
            return;
        }
        // With arguments, the native-method cascade must not have been asked:
        // its decline reads argument values, which the key does not hold. It
        // is skipped exactly when the class has a user method of this name
        // (`skip_native` in `exec_call_method_mut_op_impl`). No native method
        // is spelled with a leading `!`, so a private call's cascade declines
        // on the name alone.
        if !key.2.is_empty()
            && !method_sym.as_str().starts_with('!')
            && !self.grammar_has_user_method_memo(class_sym, method_sym)
        {
            return;
        }
        self.refresh_method_caches_for_generation();
        self.caches.plain_method_lane.insert(key);
    }

    /// The three class families whose prefix probes read the *instance* (or a
    /// registry the method generation does not cover) rather than the class, and
    /// so cannot have a class-keyed verdict replayed for them.
    ///
    /// Runs at most once per `(class, method)` pair -- only on the install side,
    /// never on the gate -- so the `resolve()`/MRO walk here is not on any hot
    /// path.
    pub(super) fn plain_method_lane_class_eligible(
        &mut self,
        class_sym: crate::symbol::Symbol,
    ) -> bool {
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
