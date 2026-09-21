//! The Associative half of the element-store fast lane: `%h{$k} = $v`,
//! consulted *before* [`Interpreter::exec_index_assign_expr_named_op`]'s shared
//! preamble rather than at the bottom of its dispatch chain.
//!
//! `try_fast_hash_element_assign` — the lane this module fronts — is the oldest
//! of the element-store fast paths, and it has sat at the bottom of that chain
//! since it was written. That is the exact position the Positional lane left in
//! [#8151](https://github.com/tokuhirom/mutsu/issues/8151), where running it
//! first (rather than making it cheaper) took a plain `@a[$i] = $v` from 3,097
//! instructions to 1,210: the preamble's probes all ask about shapes the lane
//! has already refused, so a store the lane can serve was paying for a `Range`
//! receiver probe, a deferred-vivification-token probe, the ADR-0039
//! unit-lexical cell seed/restore, a `Pair`-subscript aggregate store, a
//! `Seq`/`LazyList` element-cell resolve and a `Proxy` destination resolve, on
//! the way down to a lane that then declined all of them again.
//!
//! The Associative lane never got that treatment. Measured the same way
//! (differenced `callgrind` runs of the store loop against the identical loop
//! without it, `--profile profiling`, `MUTSU_JIT=off`), `%h{$k} = $v` costs
//! **3,642 instructions** where the Positional twin costs **1,115** — and
//! `benchmarks/bench-index-store.raku` spends more total instructions in its
//! 200,000 hash stores than in its 500,000 array stores because of it
//! ([#8069](https://github.com/tokuhirom/mutsu/issues/8069)).
//!
//! Same deliberate shape as the Positional wrapper: this lane never *errors*.
//! Any condition it is not certain about makes it return `None`, it touches
//! nothing — not the stack, not env, not a local slot — unless the lane it
//! calls commits, and the full path downstream runs exactly as it did before.

use super::*;

impl Interpreter {
    /// [`Self::try_fast_hash_element_assign`], consulted before the element
    /// store's shared preamble.
    ///
    /// Each guard below stands in for one preamble step that would otherwise
    /// have established the same fact on the lane's behalf; see the module
    /// comment for why the ordering rather than the lane is the cost. The
    /// Positional wrapper ([`Self::try_fast_array_element_assign_early`]) is the
    /// line-by-line twin of this one, and the two deliberately keep the same
    /// guard order so a future preamble step is obviously missing from both or
    /// from neither.
    pub(crate) fn try_fast_hash_element_assign_early(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        // Associative subscripts only. `%h[0]` is not this lane's shape, and
        // the Positional wrapper owns `@a[$i]`.
        if is_positional {
            return None;
        }
        // Slice 2b's `=`-element share is captured in the preamble and consumed
        // by the lane's caller; the early call site is above that capture, so
        // the only safe answer while one is pending is to decline (and, above
        // all, NOT to clear the flag).
        if self.element_share_pending {
            return None;
        }
        // The name-keyed cross-thread lane (`try_shared_hash_element_assign`)
        // is skipped by running here, and it owns the store whenever a thread
        // shares this env. Its own gate is this exact flag.
        if self.shared_vars_active {
            return None;
        }
        // Once a second VM mutator thread exists the store is routed by the
        // name-keyed cross-thread lanes or excluded by ADR-0068's
        // `ContainerStructGuard`, and this lane reproduces neither. Same
        // stand-down, and the same reasoning, as the Positional lane's.
        if crate::value::container_lock::multi_mutator_threads_live() {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        // The preamble's `Whatever` refusal ("Cannot assign to *, as the order
        // of keys is non-deterministic"), its `Pair`-subscript aggregate store
        // and its `Seq`/`Proxy` destination resolve all need a subscript this
        // lane would reject anyway; settle the shape up front so the guards
        // below are only paid for a store the lane can serve.
        if !matches!(
            self.stack[stack_len - 1].view(),
            ValueView::Str(_) | ValueView::Int(_)
        ) {
            return None;
        }
        // `itemize_for_element_store` (the preamble's ADR-0040 rvalue hook) and
        // `fetch_proxy_for_store` are both the IDENTITY on a plain scalar
        // rvalue, which is what lets this call site skip them. The allow-list
        // is the Positional lane's, for the same reason it is an allow-list
        // there: an aggregate rvalue itemizes, and a `Proxy` rvalue must FETCH.
        if !matches!(
            self.stack[stack_len - 2].view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::BigRat(..)
                | ValueView::Complex(..)
                | ValueView::Enum { .. }
                | ValueView::Instance { .. }
                | ValueView::Version { .. }
        ) {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        // Only a real `%` hash; the lane itself re-checks, and every guard
        // below is keyed on the name.
        if !var_name.as_bytes().starts_with(b"%") {
            return None;
        }
        // ADR-0039 slice 1: a compunit's own file-scope `%` is stored in the
        // `unit_lexicals` cell, and the preamble seeds env from it around the
        // store. The lane reads env directly, so it must not run for a name the
        // seed would have redirected. Opens with its own `is_empty` gate.
        if self.unit_lexical_container_cell(var_name).is_some() {
            return None;
        }
        // A variable still holding a deferred vivification token is resolved by
        // `try_deferred_token_index_assign`, which finds its slot BY NAME. The
        // lane's own `strong_count`/`resolve_local_slot` bookkeeping uses the
        // compiler-baked `target_slot`, which does not address this frame when
        // it is out of range; close that gap explicitly, as the Positional
        // wrapper does.
        if target_slot.is_some_and(|slot| (slot as usize) >= self.locals.len())
            && self.find_local_slot(code, var_name).is_some()
        {
            return None;
        }
        // The preamble resolves its store target as `locals[target_slot]` FIRST
        // and only then env, and the shapes it handles off that target -- a
        // `Pair` whose value takes a whole-container store, a `Seq`/`LazyList`
        // decided per element cell, a `Proxy` element mediating its own store,
        // a `Range` receiver that refuses associative indexing -- are all
        // reached through that slot. The lane reads env instead, so a local
        // slot holding anything but this hash (or nothing at all) is a shape it
        // has not reasoned about. `Nil` is an untouched/absent slot, which is
        // what the lane's own `strong_count == 1` case already means.
        if let Some(slot) = self.resolve_local_slot(code, target_slot, var_name)
            && !matches!(
                self.locals[slot].view(),
                ValueView::Hash(_) | ValueView::Nil
            )
        {
            return None;
        }
        // `env_root_descended_mut_tracked` -- the write chokepoint the full
        // store funnels through -- resolves a name in a strict precedence
        // order: a captured unit lexical, then the running routine's own
        // package `our %h` mirror, then env. The bare env key belongs to
        // whatever scope *loaded* the module, so for a module routine's own
        // `our %h` it holds the loading script's same-named hash. The lane
        // commits straight into env, so it must not run for a name either
        // higher-precedence root claims -- the Positional lane carries the same
        // two probes for the same reason, after `t/modules/
        // our-container-bare-name-resolution.t` caught it writing the wrong
        // array. Both probes open with their own emptiness gate.
        //
        // These are guards on the EARLY call only: declining here leaves the
        // preamble and the lane's original call site exactly as they were, so
        // no store changes destination because of them.
        if self.unit_lexical_slot(var_name).is_some()
            || self.our_package_container_key(var_name).is_some()
        {
            return None;
        }
        self.try_fast_hash_element_assign(code, name_idx, is_positional, target_slot)
    }
}
