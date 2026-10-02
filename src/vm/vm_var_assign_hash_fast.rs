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

/// What [`Interpreter::plain_hash_lane_target`] found under a `%` name.
pub(crate) enum PlainHashTarget {
    /// env holds nothing under the name.
    Absent,
    /// env holds a plain hash; `local_slot` is the one slot sharing its node.
    Hash { local_slot: Option<usize> },
}

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
        if !self.fast_hash_lane_root_is_env(code, name_idx, target_slot) {
            return None;
        }
        self.try_fast_hash_element_assign(code, name_idx, is_positional, target_slot)
    }

    /// The name-resolution guards both early hash lanes share -- the
    /// element store above and [`Self::try_fast_hash_element_incdec`]: `true`
    /// when `%name` is an ordinary lexical whose env entry is the container a
    /// store must reach, with no cross-thread routing, compunit cell, `our`
    /// package mirror or foreign local-slot shape in the way.
    ///
    /// Every probe is a pure read, so the order relative to a lane's own
    /// stack-shape checks does not matter; each opens with its own cheap gate.
    // Cost: O(1) expected (a few emptiness gates and hash probes).
    pub(crate) fn fast_hash_lane_root_is_env(
        &self,
        code: &CompiledCode,
        name_idx: u32,
        target_slot: Option<u32>,
    ) -> bool {
        // The name-keyed cross-thread lane (`try_shared_hash_element_assign`)
        // is skipped by running here, and it owns the store whenever a thread
        // shares this env. Its own gate is this exact flag.
        if self.shared_vars_active {
            return false;
        }
        // Once a second VM mutator thread exists the store is routed by the
        // name-keyed cross-thread lanes or excluded by ADR-0068's
        // `ContainerStructGuard`, and this lane reproduces neither. Same
        // stand-down, and the same reasoning, as the Positional lane's.
        if crate::value::container_lock::multi_mutator_threads_live() {
            return false;
        }
        let var_name = Self::const_str(code, name_idx);
        // Only a real `%` hash; the lane itself re-checks, and every guard
        // below is keyed on the name.
        if !var_name.as_bytes().starts_with(b"%") {
            return false;
        }
        // ADR-0039 slice 1: a compunit's own file-scope `%` is stored in the
        // `unit_lexicals` cell, and the preamble seeds env from it around the
        // store. The lane reads env directly, so it must not run for a name the
        // seed would have redirected. Opens with its own `is_empty` gate.
        if self.unit_lexical_container_cell(var_name).is_some() {
            return false;
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
            return false;
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
            return false;
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
            return false;
        }
        true
    }

    /// The env-level guards both hash fast lanes share -- the element store
    /// ([`Self::try_fast_hash_element_assign`]) and the element read-modify-write
    /// ([`Self::try_fast_hash_element_incdec`]): `%name` must be a plain,
    /// untyped, writable hash with no `:=`-bound element and no `is default`,
    /// held by env and at most one local slot.
    ///
    /// Returns `None` to decline, `Some(Absent)` when env holds no entry under
    /// the name at all (the store lane autovivifies it), and `Some(Hash)` with
    /// the local slot that shares env's node, if any.
    // Cost: O(1) expected (a fixed number of env probes).
    pub(crate) fn plain_hash_lane_target(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_slot: Option<u32>,
    ) -> Option<PlainHashTarget> {
        // Reject if there are any local bind pairs (`:=` bindings in scope)
        if !self.local_bind_pairs.is_empty() {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        // Only handle %-sigiled hash variables
        if !var_name.starts_with('%') {
            return None;
        }
        let var_sym = code.const_sym(name_idx);
        // Check that no type constraints, key constraints, or defaults exist.
        // ADR-0042 slice 1: reads the target hash's own embedded metadata
        // (see the `try_shared_hash_element_assign` comment for why
        // `container_type_metadata` rather than `element_constraint_for`) —
        // the `has_type_meta()` check further below is a second,
        // container-only belt-and-suspenders check on the SAME embedded
        // metadata, kept for its extra strong-count/local-slot bookkeeping.
        //
        // Scoped tightly in its own block: `current` clones the hash's Arc,
        // and the `strong_count` check a few lines below (the "does an
        // external binding exist" heuristic) counts EVERY live Arc clone —
        // including this temporary one, if it were still alive. An
        // unscoped `let current = ...` here made every hash-element
        // assignment whose value's rvalue-itemization is observed by
        // surrounding code (`my @z = (%a<x> = ...)`) see `strong_count == 3`
        // instead of 2, permanently falling off the fast path and losing its
        // itemization (`t/hash-key-single-itemize.t`).
        {
            let current = self.env().get_sym(var_sym).cloned().unwrap_or(Value::NIL);
            if self.container_type_metadata(&current).is_some()
                || self.var_default(var_name).is_some()
                || self.is_readonly_sym(var_sym)
            {
                return None;
            }
        }
        // Reject if any bound indices exist for this variable
        // (e.g. `%h<a> := $foo` makes element writes propagate to $foo).
        // Gated like the twin above: no bound element, no probe.
        if crate::env::elem_index_meta_possible() {
            let bound_key = crate::meta_ns::MetaNs::BoundIndex.key_for_str(var_name);
            if self.env().contains_key_sym(bound_key) {
                return None;
            }
        }
        // Check that the variable exists in env as a plain Hash
        // and that it has no container type metadata
        let env = self.env();
        match env.get_sym(var_sym).map(Value::view) {
            Some(ValueView::Hash(hash_arc)) => {
                let strong_count = crate::gc::Gc::strong_count_of(&hash_arc);
                // Reject if the hash Arc has more than 2 refs (e.g. HashEntryRef binding)
                // strong_count == 1: only env holds it (no local slot)
                // strong_count == 2: env + locals hold it (common case in for loops)
                // strong_count > 2: external binding exists, fall through to slow path
                if strong_count > 2 {
                    return None;
                }
                let local_slot = if strong_count == 2 {
                    // The extra ref should be from locals — verify.
                    //
                    // ADR-0039 slice 2: through the compiler-baked
                    // `target_slot`, never a by-name search. `find_local_slot`
                    // is a `position` over `code.locals`, so with a same-named
                    // shadow (`code.locals == ["%h", "%h"]`) it answered the
                    // OUTER binding's slot — which this path then nil'd and
                    // re-seeded, corrupting a variable the store never touched.
                    Some(self.resolve_local_slot(code, target_slot, var_name)?)
                } else {
                    None
                };
                // Reject if there's container type metadata
                if hash_arc.has_type_meta()
                    || loan_env!(self, var_type_constraint(var_name)).is_some()
                {
                    return None;
                }
                Some(PlainHashTarget::Hash { local_slot })
            }
            None => Some(PlainHashTarget::Absent),
            _ => None,
        }
    }

    /// The commit both hash fast lanes end in: store `val` under `key` in the
    /// env hash [`Self::plain_hash_lane_target`] approved, keeping the one
    /// local slot that shares its node pointing at the mutated node.
    // Cost: O(1) expected (one hash insert; the node is unshared first, so
    // `make_mut` never copies the map).
    pub(crate) fn commit_plain_hash_insert(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_slot: Option<u32>,
        local_slot: Option<usize>,
        key: String,
        val: Value,
    ) {
        let var_name = Self::const_str(code, name_idx);
        let var_sym = code.const_sym(name_idx);
        // When locals and env share the same Arc (strong_count == 2),
        // drop the local ref first so Arc::make_mut can mutate in-place
        // instead of cloning the entire HashMap (O(n) → O(1) per insert).
        if let Some(slot) = local_slot {
            self.locals[slot] = Value::NIL;
        }
        if let Some(entry) = self.env_mut().get_mut_sym(var_sym) {
            entry.with_hash_mut(|hash| {
                Value::hash_insert_through(&mut crate::gc::Gc::make_mut(hash).map, key, val);
            });
        }
        // Restore the local slot to point to the (now mutated) env Arc
        if let Some(slot) = local_slot
            && let Some(env_val) = self.env().get_sym(var_sym).cloned()
        {
            self.locals[slot] = env_val;
        }
        // strong_count==1 divergence repair: a re-entrant call evaluated
        // as the RHS (e.g. a `proto {*}` redispatch) can swap `self.env`
        // out from under the block's local slot via
        // `restore_env_preserving_existing`, leaving the slot pointing at
        // a stale, detached Arc while env holds the live one (strong_count
        // drops to 1). The insert above mutated only env, so a local slot
        // that still exists is — by definition of strong_count==1 — a
        // diverged copy. Mirror the live env value back to it to keep the
        // dual store coherent, so a later `state`-var persist (which reads
        // env first, then `sync_env_from_locals` flushes the slot) does not
        // clobber the value with the stale slot. No-op for a genuine
        // env-only hash (e.g. `%*ENV`) that has no local slot, and the
        // default build's blanket reconcile makes it redundant (byte-
        // identical) — it only matters on the single-store path.
        if local_slot.is_none()
            && let Some(slot) = self.resolve_local_slot(code, target_slot, var_name)
            && let Some(env_val) = self.env().get_sym(var_sym).cloned()
        {
            self.locals[slot] = env_val;
        }
    }

    /// `%h{$k}++`, `%h{$k}--`, `++%h{$k}` and `--%h{$k}` on a plain hash whose
    /// element is an `Int` (or absent, which counts from 0) -- the
    /// word-frequency idiom.
    ///
    /// [`Self::exec_inc_dec_index_op`] answers this from a generic chain that
    /// classifies the target for every container shape it supports (QuantHash,
    /// object hash, `is Array` instance, Buf, Capture, `AT-KEY` override,
    /// captured cell, typed and defaulted elements), re-resolving the name for
    /// each probe: about 6,800 instructions per increment, against ~1,800 for
    /// the plain store `%h{$k} = $v`. This lane takes the store lane's guards
    /// ([`Self::fast_hash_lane_root_is_env`], [`Self::plain_hash_lane_target`])
    /// -- which already exclude every one of those shapes -- and does the
    /// integer step itself. It never errors: a non-`Int` element, an overflow
    /// to `BigInt`, or any guard it is unsure about returns `None` with nothing
    /// touched, and the generic op runs exactly as before.
    // Cost: O(1) expected (one key stringification, a fixed number of env
    // probes and one hash insert).
    pub(crate) fn try_fast_hash_element_incdec(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
        increment: bool,
        return_new: bool,
    ) -> Option<Result<(), RuntimeError>> {
        // A plain `%ident` lexical. An attribute (`%!h`/`%.h`) is routed
        // through the cell snapshot/mirror the dispatch arm wraps around the
        // generic op, a dynamic `%*h` or `%_` resolves elsewhere, and a
        // package-qualified `%Foo::h` persists through `our_vars`.
        let name = Self::const_str(code, name_idx).as_bytes();
        if name.len() < 2 || name[0] != b'%' || !name[1].is_ascii_alphabetic() {
            return None;
        }
        if crate::qualified::is_package_hash(code.const_sym(name_idx)) {
            return None;
        }
        // A single scalar key. A `Whatever`/`WhateverCode`, an aggregate (a
        // slice, or an itemized list that is one `.WHICH` key) and a type
        // object all need the generic op's resolution.
        let key = match self.stack.last().map(Value::view) {
            Some(ValueView::Str(s)) => s.as_str().to_owned(),
            Some(ValueView::Int(_)) => self.stack.last()?.to_string_value(),
            _ => return None,
        };
        // Also declines while a thread shares this env, which is the only time
        // the generic op's `Lock.protect` writeback (`writeback_critical_var`)
        // does anything.
        if !self.fast_hash_lane_root_is_env(code, name_idx, slot) {
            return None;
        }
        let PlainHashTarget::Hash { local_slot } =
            self.plain_hash_lane_target(code, name_idx, slot)?
        else {
            return None;
        };
        let old = match self
            .env()
            .get_sym(code.const_sym(name_idx))
            .map(Value::view)
        {
            Some(ValueView::Hash(hash)) => match hash.get(&key).map(Value::view) {
                // No `is default` and no element type (both guarded above), so
                // an absent element counts from `Int` 0, as the generic op does.
                None => 0,
                Some(ValueView::Int(n)) => n,
                _ => return None,
            },
            _ => return None,
        };
        let new = if increment {
            old.checked_add(1)?
        } else {
            old.checked_sub(1)?
        };
        self.stack.pop();
        self.commit_plain_hash_insert(code, name_idx, slot, local_slot, key, Value::int(new));
        self.stack
            .push(Value::int(if return_new { new } else { old }));
        Some(Ok(()))
    }

    /// The four `*IncrementIndex`/`*DecrementIndex` opcodes: the plain-hash
    /// lane first, otherwise the generic op wrapped in the attribute-element
    /// mirroring (`++@!a[0]` and `@!a[0]++` must reach the attribute's cell
    /// alike). The lane only accepts a plain `%ident`, for which the snapshot
    /// and mirror are no-ops, so skipping them there changes nothing.
    // Cost: O(1) for a single index/key.
    pub(crate) fn exec_inc_dec_index_dispatch(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
        increment: bool,
        return_new: bool,
    ) -> Result<(), RuntimeError> {
        if let Some(result) =
            self.try_fast_hash_element_incdec(code, name_idx, slot, increment, return_new)
        {
            return result;
        }
        let pre = self.attr_elem_env_snapshot(code, name_idx);
        self.exec_inc_dec_index_op(code, name_idx, slot, increment, return_new)?;
        self.mirror_attr_elem_env_to_cell(code, name_idx, pre);
        Ok(())
    }
}
