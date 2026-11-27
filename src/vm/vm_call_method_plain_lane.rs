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
//!   definedness per argument) are the key. A named argument is keyed by its
//!   name and its value's type key
//!   ([`Interpreter::multi_arg_type_keys_into_with_named`]). A call with
//!   arguments is admitted only when every argument has a type key, so a
//!   `Junction`, a pair-valued named argument, a container or a mixin -- the
//!   shapes the probes that read arguments (autothreading, the
//!   named-argument intercepts) decide on -- never enters the lane. Every
//!   other probe in the skipped stretch reads at most the argument *count*
//!   for a user-class receiver, which the key fixes (#10111);
//! * **no `.^`/`."…"` modifier** and **not quoted** are required by the gate
//!   on both the install and the replay, and **the accessor-ref marker** (a
//!   call asked for a container) is part of the key, so no probe's
//!   call-shaped early-out can differ between them;
//! * **the native-method cascade** (`try_native_method`), the one probe in the
//!   stretch whose decline depends on argument *values*, is consulted only
//!   when the class has no user method of the name. A call with arguments
//!   installs only when the class has one, so that probe was never asked;
//! * **the registry** (methods, roles, wraps, MRO, attributes) is pinned by
//!   `Registry::method_generation` -- the same latch the sibling method caches
//!   use, cleared in `refresh_method_caches_for_generation`;
//! * **per-instance state** is ruled out by refusing to install for the three
//!   class families whose probes read the *instance* rather than the class: an
//!   `IO::Handle`/`IO::Path` in the MRO (the former IO::Handle fast paths keyed on
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
        if !matches!(modifier, None | Some("!"))
            || Self::quoting_matters(quoted, method_sym.as_str())
        {
            return None;
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        let mut arg_keys = std::mem::take(&mut self.caches.plain_method_lane_scratch);
        if !args.is_empty() && !self.plain_lane_arg_keys_into(args, &mut arg_keys) {
            arg_keys.clear();
            self.caches.plain_method_lane_scratch = arg_keys;
            return None;
        }
        // A call asked for a container (the right side of `:=`, an `is raw`
        // routine's last statement) keys apart from a plain one: the probes
        // that read the request (the accessor reads, the wrapped-accessor
        // terminal) are then pinned by the key like everything else. The
        // dispatch tail never sees the request (it is consumed here), so a
        // user method's plan is the same either way (#18267).
        if want_ref {
            arg_keys.push(super::vm_method_call_plan::want_ref_marker());
        }
        Some((class_name, method_sym, arg_keys))
    }

    /// Whether a quoted spelling (`$obj."name"()`, or a run-time name) can
    /// dispatch differently from the bare one. Only for a MOP pseudo-method
    /// name, which a quoted call sends to a user method of that name instead of
    /// the reflection macro (`user_shadows_builtin`,
    /// `skip_pseudo_method_native`); for every other name the two spellings
    /// run the same stretch, so a quoted call shares the plain-method lane
    /// (#17811: `self."$name"()` in SBOM::CycloneDX's `Map`).
    // Cost: O(1).
    pub(super) fn quoting_matters(quoted: bool, method: &str) -> bool {
        quoted
            && matches!(
                method,
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
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
        method_sym: crate::symbol::Symbol,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        self.dispatch.method_dispatch_pure = false;
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
        self.caches.plain_method_lane_active = true;
        let call_result = self.dispatch_compiled_method_mut_with_raw_invocant(
            code,
            target_name,
            target,
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

    /// The early form of the lane for a method call on an ordinary object: a
    /// hit skips the whole stretch of opcode-level receiver probes before the
    /// lane's own gate, not only the prefix after it (~2k of the ~15k
    /// instructions of `$o.m()`, ~3k of a one-argument call).
    ///
    /// The late gate sits after that stretch because several of its probes read
    /// the *instance* rather than the class. Hoisting the gate is sound only
    /// for a call none of them can claim, so this refuses by method name every
    /// probe in the stretch that reads per-instance state, and relies on the
    /// memo for the rest -- those key on the receiver's kind, the method name
    /// or the argument type keys, all fixed by the key (a `Seq`, a lazy list,
    /// a gather, a shaped array or a `Slip` is never an `Instance`):
    ///
    /// * `message`: the thrown-message attribute of an exception instance;
    /// * `value`: the weight pair of a QuantHash instance;
    /// * the renderers (`renders_receiver_elements`): the nested-`Proxy` scan
    ///   over the instance's attributes, and `raku`'s itemization test;
    /// * the push family: `vivify_bound_entry_variable` reads the variable's
    ///   binding by name;
    /// * `subbuf-rw`, `freeze`, `flat`, `Slip`: kind-specific interceptions.
    ///
    /// **A probe added to that stretch that reads the instance must add its
    /// method name here**, or a warm lane would skip it for the instances it
    /// would have claimed.
    ///
    /// The stretch also *transforms* the arguments before the late gate keys
    /// them. A call whose arguments it would change is refused, so the key
    /// computed here from the arguments as they stand is the late gate's key:
    /// a `|` spread at the call site, a `VarRef`, a value `hllize_for_binding`
    /// rewrites, and the `AT-KEY` family (`coerce_assoc_key_arg` stringifies an
    /// object key through user code). The two non-probe effects it has on
    /// arguments that pass are reproduced on a hit: the call's argument sources
    /// are published, and a deferred `.map`/`.grep` argument is reified.
    ///
    /// Also refused: any program with a user `^find_method` or a wrapped
    /// method (their probes sit in the stretch and are not covered by the
    /// memo's generation).
    // Cost: O(a), a = arguments (a type key each) -- one set probe.
    pub(super) fn try_plain_method_early_lane(
        &mut self,
        code: &CompiledCode,
        target_name: &str,
        method: &str,
        method_sym: crate::symbol::Symbol,
        arity: usize,
        arg_sources_idx: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        if self.caches.plain_method_lane.is_empty()
            || Self::early_lane_method_excluded(method)
            || (arity > 0
                && matches!(
                    method,
                    "AT-KEY" | "EXISTS-KEY" | "DELETE-KEY" | "ASSIGN-KEY" | "BIND-KEY"
                ))
            || crate::runtime::find_method_intercept::any_user_find_method()
            || self.registry().method_name_ever_wrapped(method)
        {
            return None;
        }
        let target_idx = self.stack.len().checked_sub(arity + 1)?;
        let ValueView::Instance { class_name, .. } = self.stack[target_idx].view() else {
            return None;
        };
        // A call asked for a container (the right side of `:=`) keys apart
        // exactly as at the late gate (`plain_method_lane_key`), so a hit
        // replays the outcome recorded for such a call; the request is
        // consumed by the hit, as the full path consumes it.
        let want_ref = self.accessor_ref_pending;
        if arity == 0 {
            self.refresh_method_caches_for_generation();
            let mut arg_keys = std::mem::take(&mut self.caches.plain_method_lane_scratch);
            arg_keys.clear();
            if want_ref {
                arg_keys.push(super::vm_method_call_plan::want_ref_marker());
            }
            let key = (class_name, method_sym, arg_keys);
            let hit = self.caches.plain_method_lane.contains(&key);
            let plan = if hit {
                self.method_call_plan_for_hit(&key)
            } else {
                None
            };
            let mut arg_keys = key.2;
            arg_keys.clear();
            self.caches.plain_method_lane_scratch = arg_keys;
            if !hit {
                return None;
            }
            self.accessor_ref_pending = false;
            let target = self.stack.pop()?;
            // What the stretch leaves behind for a call without arguments.
            self.caches.ctor_lane_candidate = None;
            self.caches.plain_method_lane_candidate = None;
            self.pending_call_arg_source_slots.clear();
            self.set_pending_call_arg_sources(None);
            if let Some(plan) = plan {
                return Some(self.run_method_call_plan(
                    code,
                    target_name,
                    target,
                    method_sym,
                    &plan,
                    Vec::new(),
                ));
            }
            return Some(self.run_plain_method_lane(
                code,
                target_name,
                target,
                method_sym,
                Vec::new(),
            ));
        }
        if self.stack[target_idx + 1..]
            .iter()
            .any(|a| matches!(a.view(), ValueView::VarRef { .. }) || a.needs_hllize_for_binding())
        {
            return None;
        }
        // The stretch's own decode: `true` means a `|` spread at the call site.
        let decoded = self.decode_arg_sources_shared(code, arg_sources_idx);
        if decoded.as_ref().is_some_and(|(_, spread)| *spread) {
            return None;
        }
        let args = self.stack.split_off(target_idx + 1);
        self.refresh_method_caches_for_generation();
        let mut arg_keys = std::mem::take(&mut self.caches.plain_method_lane_scratch);
        let mut plan = None;
        let hit = self.plain_lane_arg_keys_into(&args, &mut arg_keys) && {
            if want_ref {
                arg_keys.push(super::vm_method_call_plan::want_ref_marker());
            }
            let key = (class_name, method_sym, arg_keys);
            let hit = self.caches.plain_method_lane.contains(&key);
            if hit {
                plan = self.method_call_plan_for_hit(&key);
            }
            arg_keys = key.2;
            hit
        };
        arg_keys.clear();
        self.caches.plain_method_lane_scratch = arg_keys;
        if !hit {
            self.stack.extend(args);
            return None;
        }
        self.accessor_ref_pending = false;
        let target = self.stack.pop()?;
        // What the stretch leaves behind for a call with these arguments.
        self.caches.ctor_lane_candidate = None;
        self.caches.plain_method_lane_candidate = None;
        self.set_pending_call_arg_sources_shared(decoded.map(|(names, _)| names));
        if let Err(e) = self.reify_map_grep_seq_method_args(&target, method_sym, &args) {
            return Some(Err(e));
        }
        if let Some(plan) = plan {
            return Some(self.run_method_call_plan(
                code,
                target_name,
                target,
                method_sym,
                &plan,
                args,
            ));
        }
        Some(self.run_plain_method_lane(code, target_name, target, method_sym, args))
    }

    /// Method names the stretch handles for an instance receiver by reading
    /// the instance, which [`Self::try_plain_method_early_lane`] must not skip.
    fn early_lane_method_excluded(method: &str) -> bool {
        Self::renders_receiver_elements(method)
            || matches!(
                method,
                "message"
                    | "value"
                    | "push"
                    | "append"
                    | "unshift"
                    | "prepend"
                    | "subbuf-rw"
                    | "freeze"
                    | "flat"
                    | "Slip"
            )
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
        if !self.plain_method_lane_class_eligible(class_sym)
            && !self.plain_method_lane_mixin_eligible(class_sym)
        {
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
        // The tail this note runs in records its plan for the new key too
        // (ADR-15737 D1, `install_method_call_plan`, right after this).
        self.want_method_call_plan(&key);
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
        if !self
            .types
            .user_declared_classes
            .contains(class_name.as_str())
        {
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

    /// A mixin type the runtime synthesized for `$obj does R` over a built-in
    /// base whose stretch probes are audited to read only the class and the
    /// method name, never the instance (#18267 stage 4).
    ///
    /// A mixin over a user-declared class is already
    /// [`Self::plain_method_lane_class_eligible`] (it stays user-declared).
    /// One over a built-in base is not, because a built-in class's probes
    /// may read the instance. `Attribute` is the one base admitted: its only
    /// probes in the stretch are the meta-row pair
    /// (`prepare_attribute_set_value_arg`, `try_attribute_meta_row`), which
    /// decide on the receiver's class, its MRO, the method name and whether a
    /// user method of that name exists -- all fixed by the key and the
    /// method generation. This is the receiver of AttrX::Mooish's attribute
    /// methods (`Attribute+{AttrX::Mooish::Attribute}`).
    ///
    /// The mixin type's name is a registered class, stable for the
    /// composition (the same `(base, roles)` always names the same class), so
    /// it keys the lane exactly as a declared class does.
    // Cost: O(r) once per `(class, method)` pair on the install side, r = the
    // recipe's roles (the recipe is cloned out of the shared table).
    pub(super) fn plain_method_lane_mixin_eligible(
        &mut self,
        class_sym: crate::symbol::Symbol,
    ) -> bool {
        const AUDITED_BASES: [&str; 1] = ["Attribute"];
        let class_name = class_sym.as_str();
        if !self.types.published_mixins.may_hold(class_name) {
            return false;
        }
        let Some(recipe) = self.types.published_mixins.lookup(class_name) else {
            return false;
        };
        AUDITED_BASES.contains(&recipe.base.as_str())
            && self.registry().classes.contains_key(class_name)
            && self.cstruct_class_name(class_name).is_none()
    }
}
