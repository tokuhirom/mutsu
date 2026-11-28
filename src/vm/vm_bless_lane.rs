//! The bless lane: the `bless` twin of the default-constructor lane
//! (`vm_ctor_lane`, ADR-0121 D3), for #19399.
//!
//! A user constructor is almost always `method new(*%_) { self.bless(|%_) }`
//! or a variation of it. Its `self.bless(...)` compiles to `CallMethodMut`,
//! which walks the argument stretch at the top of the opcode (decode, spread,
//! hllize, gather, the lazy-`Seq` passes, the receiver rewrites) and then the
//! long chain of speculative probes before the dispatch tail reaches the
//! native `bless` (`try_native_bless`). For a plain user class every probe
//! answers "no" on every call: on `bench-ctor` that walk cost about as much as
//! `bless` itself.
//!
//! # Why the memo cannot certify something that did not happen
//!
//! As for the ctor lane: every probe *returns* when it claims a call, so a
//! dispatch that reaches the native `bless` has been declined by every probe
//! in between. Reaching it is the proof, and the memo is written exactly there
//! ([`Interpreter::take_bless_lane_nomination`], [`Interpreter::install_bless_lane`]), for the class the dispatch's
//! own gate nominated (`ctor_lane_candidate` with method `bless`, cleared by
//! every `CallMethodMut` and site gate).
//!
//! # What the key has to hold constant
//!
//! * **the receiver** is the key: a type object, never an instance (an
//!   instance receiver's probes read its attributes);
//! * **the call shape** -- method `bless`, no `.^`/`.!` modifier, not quoted,
//!   no accessor-ref marker, and every argument after the spread a
//!   string-keyed `Pair` whose value is not a `Junction` -- is required on both
//!   the install and the replay;
//! * **the registry** (methods, wraps, MRO) is pinned by the method
//!   generation: the lane is cleared with the other method caches;
//! * **the class shape** is pinned by the identity of the `NativeCtorPlan` the
//!   install saw, which every class-shape mutation drops.
//!
//! # The early form, and the spread
//!
//! The replay runs at the top of `CallMethodMut`, before the argument
//! stretch, like the ctor lane's early form (`vm_ctor_early_lane`), and with
//! the same argument for skipping it: a call whose arguments the stretch
//! would change is refused (a `VarRef`, a value `hllize_for_binding`
//! rewrites, a gather or consumed `Seq`), the stretch's two non-probe effects
//! are reproduced (the argument sources are published, a deferred
//! `.map`/`.grep` argument is reified), and a type-object receiver is one
//! neither receiver rewrite touches. Unlike the ctor lane's early form it
//! takes a `|` spread at the call site -- `self.bless(|%_)` is the shape this
//! lane exists for -- by running the stretch's own spread
//! (`spread_call_args_by_syntax_shared`, a pure function of the arguments and
//! the call site) on a copy, and checking the spread arguments against the
//! key. A refusal leaves the stack as it was and the whole chain runs.

use super::*;

impl Interpreter {
    /// The class a `CallMethodMut` dispatch is eligible to install into the
    /// bless lane, or `None` when the call shape is outside it. The shape check
    /// is [`Self::ctor_lane_key`]'s, for method `bless`.
    // Cost: O(a), a = arguments; O(1) for any method other than `bless`.
    pub(super) fn bless_lane_key(
        target: &Value,
        args: &[Value],
        modifier: Option<&str>,
        quoted: bool,
        want_ref: bool,
        method_sym: Symbol,
    ) -> Option<Symbol> {
        if method_sym != crate::symbol::wk::bless_method()
            || modifier.is_some()
            || quoted
            || want_ref
        {
            return None;
        }
        let ValueView::Package(class_sym) = target.view() else {
            return None;
        };
        Self::bless_lane_args_ok(args).then_some(class_sym)
    }

    /// Every argument a string-keyed `Pair` whose value is not a `Junction`.
    // Cost: O(a), a = arguments.
    fn bless_lane_args_ok(args: &[Value]) -> bool {
        args.iter().all(|a| {
            a.is_string_pair_value()
                && matches!(a.view(), ValueView::Pair(_, v) if !v.is_junction_value())
        })
    }

    /// Take this dispatch's bless-lane nomination for `class_sym`: whether the
    /// gate nominated exactly this class for `self.bless(...)` on its type
    /// object. Asked where the chain reaches the native `bless`, before it
    /// runs: a BUILD/TWEAK body makes calls of its own, whose gates clear the
    /// candidate, and they are not part of the chain the memo proves.
    // Cost: O(1).
    pub(crate) fn take_bless_lane_nomination(&mut self, target: &Value, class_sym: Symbol) -> bool {
        if self.caches.ctor_lane_candidate != Some((class_sym, crate::symbol::wk::bless_method()))
            || !matches!(target.view(), ValueView::Package(_))
        {
            return false;
        }
        self.caches.ctor_lane_candidate = None;
        true
    }

    /// Record that the whole chain was inert for `self.bless(...)` on
    /// `class_sym`, for a dispatch that held the nomination
    /// ([`Self::take_bless_lane_nomination`]) and whose `bless` succeeded.
    // Cost: one plan fetch and the eligibility checks, once per class and
    // generation.
    pub(crate) fn install_bless_lane(&mut self, class_sym: Symbol) {
        let plan = self.native_ctor_plan(class_sym);
        if plan.has_custom_bless || !self.plain_method_lane_class_eligible(class_sym) {
            return;
        }
        self.caches.bless_lane.insert(class_sym, plan);
    }

    /// Whether the bless lane holds `class_sym` for the current registry
    /// generation and class shape.
    // Cost: O(1), two hash probes.
    fn bless_lane_hit(&mut self, class_sym: Symbol) -> bool {
        if self.caches.bless_lane.is_empty() {
            return false;
        }
        self.refresh_method_caches_for_generation();
        match (
            self.caches.bless_lane.get(&class_sym),
            self.caches.native_ctor_plan_cache.get(&class_sym),
        ) {
            (Some(installed), Some(current)) => std::sync::Arc::ptr_eq(installed, current),
            _ => false,
        }
    }

    /// Answer `self.bless(named...)` at the top of `CallMethodMut` when the
    /// bless lane holds the receiver's class (see the module doc). `None`,
    /// with the stack untouched, on a miss.
    // Cost: O(a), a = arguments (the spread and the key check); the
    // construction itself is `dispatch_bless`'s.
    pub(super) fn try_bless_early_lane(
        &mut self,
        code: &CompiledCode,
        method_sym: Symbol,
        arity: usize,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        if self.caches.bless_lane.is_empty()
            || method_sym != crate::symbol::wk::bless_method()
            || modifier_idx.is_some()
            || quoted
            || self.accessor_ref_pending
            || crate::runtime::find_method_intercept::any_user_find_method()
            || self.registry().method_name_ever_wrapped("bless")
        {
            return None;
        }
        let target_idx = self.stack.len().checked_sub(arity + 1)?;
        let ValueView::Package(class_sym) = self.stack[target_idx].view() else {
            return None;
        };
        if !self.bless_lane_hit(class_sym) {
            return None;
        }
        if self.stack[target_idx + 1..]
            .iter()
            .any(|a| matches!(a.view(), ValueView::VarRef { .. }) || a.needs_hllize_for_binding())
        {
            return None;
        }
        // The stretch's own decode and spread (`true`: a `|` at the call site).
        let decoded = self.decode_arg_sources_shared(code, arg_sources_idx);
        let raw = self.stack[target_idx + 1..].to_vec();
        let (args, sources) = if decoded.as_ref().is_some_and(|(_, spread)| *spread) {
            Self::spread_call_args_by_syntax_shared(code, raw, arg_sources_idx, decoded)
        } else {
            (raw, decoded.map(|(names, _)| names))
        };
        if !Self::bless_lane_args_ok(&args)
            || args.iter().any(Value::needs_hllize_for_binding)
            || Self::has_builtin_seq_read(&args)
        {
            return None;
        }
        // An attribute default or a BUILD/TWEAK body may read the caller's
        // lexicals: a transient scoped overlay env is collapsed first, as the
        // full path does before its dispatch tail (`try_ctor_lane`).
        let installed = self.caches.bless_lane.get(&class_sym)?;
        let runs_submethods = installed.has_build || installed.has_tweak;
        let flatten = runs_submethods || !installed.evaluates_no_decl_expr(self);
        // Committed: the call's own arguments leave the stack.
        let raw_args = self.stack.split_off(target_idx + 1);
        let target = self.stack.pop()?;
        // What the stretch leaves behind for a call with these arguments.
        self.caches.ctor_lane_candidate = None;
        self.caches.plain_method_lane_candidate = None;
        self.set_pending_call_arg_sources_shared(sources);
        if args.iter().any(Self::ctor_arg_may_hold_lazy)
            && let Err(e) = self.reify_map_grep_seq_method_args(&target, method_sym, &args)
        {
            return Some(Err(e));
        }
        if flatten {
            self.flatten_scoped_env();
        }
        let Some(result) = loan_env!(self, try_native_bless(&target, &args)) else {
            // The native `bless` declined (the plan identity says it cannot):
            // the whole chain runs on the call as it arrived.
            self.stack.push(target);
            self.stack.extend(raw_args);
            return None;
        };
        // As the chain's tail leaves it (`try_compiled_method_mut_or_interpret_sym`).
        self.dispatch.method_dispatch_pure = !runs_submethods;
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "bless-lane");
        Some(result.map(|value| self.stack.push(value)))
    }
}
