//! ADR-0121 D3: the default-constructor lane of `CallMethodMut`.
//!
//! `P.new(x => 1, y => 2)` compiles to `CallMethodMut`, which walks the same
//! long chain of speculative probes as any other method call (proto bodies,
//! exception delegates, lazy lists, junction autothreading, the storage
//! delegates, the native method table, ...) before
//! `try_compiled_method_mut_or_interpret_sym` reaches the native default
//! constructor. For a plain user class every probe answers "no" on every
//! call, and walking them cost more than building the instance did.
//!
//! This is a cache **in front of** the chain, the constructor twin of the
//! plain-method lane (`vm_call_method_plain_lane`): a set of classes whose
//! `.new` has been observed to walk the whole chain and land in the native
//! default constructor, so the next call goes straight there.
//!
//! # Why the memo cannot certify something that did not happen
//!
//! Every probe *returns* when it claims a call, so a dispatch that reaches the
//! native default constructor has been declined by every probe in between.
//! Reaching it is the proof, and the memo is written exactly there
//! ([`Interpreter::note_ctor_lane_reached`]), not from a hand-audited list of
//! conditions.
//!
//! # What the key has to hold constant
//!
//! * **the receiver** is the key: a type object, never an instance;
//! * **the call shape** -- method `new`, no `.^`/`.!` modifier, not quoted, no
//!   accessor-ref marker, and every argument a string-keyed `Pair` whose value
//!   is not a `Junction` -- is required on both the install and the replay, so
//!   no probe's argument-shaped early-out (autothreading, positional-argument
//!   handling, the `push`-family and `AT-KEY`-family arms) can differ between
//!   them;
//! * **the registry** (methods, wraps, MRO) is pinned by
//!   `Registry::method_generation`: the lane is cleared with the other method
//!   caches in `refresh_method_caches_for_generation`;
//! * **the class shape** is pinned by the identity of the `NativeCtorPlan` the
//!   install saw. That plan is dropped at every class-shape mutation site (the
//!   MOP mutators clear it without a generation bump), so a replay whose plan
//!   is not the very same `Arc` misses instead of trusting an old verdict;
//! * **user code** is ruled out on the install side: a class with a `BUILD` or
//!   `TWEAK` anywhere in its MRO, a CUnion, a class the program did not declare,
//!   and a class with a builtin base never enter.
//!
//! A miss, or a replay the constructor declines, falls through to the whole
//! chain unchanged.

use super::*;

impl Interpreter {
    /// The class this dispatch is eligible to replay or install, or `None`
    /// when the call shape is outside the lane.
    ///
    /// Cheap and allocation-free: it runs on every `CallMethodMut`, including
    /// the ones that will miss.
    // Cost: O(a), a = number of arguments (the shape check), and O(1) for any
    // method other than `new`.
    pub(super) fn ctor_lane_key(
        target: &Value,
        args: &[Value],
        modifier: Option<&str>,
        quoted: bool,
        want_ref: bool,
        method_sym: crate::symbol::Symbol,
    ) -> Option<crate::symbol::Symbol> {
        if method_sym != crate::symbol::wk::new_method() || modifier.is_some() || quoted || want_ref
        {
            return None;
        }
        let ValueView::Package(class_sym) = target.view() else {
            return None;
        };
        let named_only = args.iter().all(|a| {
            a.is_string_pair_value()
                && matches!(a.view(), ValueView::Pair(_, v) if !v.is_junction_value())
        });
        named_only.then_some(class_sym)
    }

    /// Answer a lane hit: the native default constructor, run exactly as the
    /// full chain's tail runs it. `None` on a miss, or when the constructor
    /// declines, so the caller walks the whole chain.
    // Cost: O(1) to decide the hit; the construction itself is
    // `try_native_default_construct`'s.
    pub(super) fn try_ctor_lane(
        &mut self,
        class_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if self.ctor_lane.is_empty() {
            return None;
        }
        self.refresh_method_caches_for_generation();
        let installed = self.ctor_lane.get(&class_sym)?;
        let current = self.native_ctor_plan_cache.get(&class_sym)?;
        if !std::sync::Arc::ptr_eq(installed, current) {
            return None;
        }
        // The one non-probe effect of the skipped stretch the constructor can
        // observe: an attribute default expression may read the caller's
        // lexicals, so a transient scoped overlay env is collapsed first, as
        // the full path does before its dispatch tail.
        self.flatten_scoped_env();
        let result = loan_env!(self, try_native_default_construct(class_sym, args))?;
        // No BUILD/TWEAK (checked on install), so the construction cannot
        // write the caller's env.
        self.method_dispatch_pure = true;
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "ctor-lane");
        Some(result)
    }

    /// Record that the whole chain was inert for `class_sym`, if it is the
    /// class the current dispatch's gate nominated.
    ///
    /// The candidate is cleared by every `CallMethodMut` gate, so a nested
    /// dispatch run from inside a probe cannot leave its own key behind for an
    /// outer call to install; and the equality check means an outer call can
    /// only ever install the class it was nominated with.
    // Cost: O(1) when the candidate does not match; otherwise one plan fetch
    // and the eligibility checks, once per class and generation.
    pub(crate) fn note_ctor_lane_reached(&mut self, class_sym: crate::symbol::Symbol) {
        if self.ctor_lane_candidate != Some(class_sym) {
            return;
        }
        self.ctor_lane_candidate = None;
        let plan = self.native_ctor_plan(class_sym);
        if !plan.eligible
            || plan.is_cunion
            || plan.has_build
            || plan.has_tweak
            || !plan.attrs_fully_known
            || !self.plain_method_lane_class_eligible(class_sym)
        {
            return;
        }
        self.ctor_lane.insert(class_sym, plan);
    }
}
