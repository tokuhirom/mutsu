//! Fast path for the plain positional element store `@a[$i] = $v`.
//!
//! The Associative twin of this lane (`try_fast_hash_element_assign`) has
//! existed for a long time; the Positional one did not, so **every** `@a[$i] =
//! $v` -- one of the most common statements in the language -- went through
//! `exec_index_assign_expr_named_op_inner`, which re-derives the target's whole
//! *declaration* by string key on each write: is this a sigilless alias, is it
//! shaped, is it `:=`-bound, does it have a type/key constraint, a default, a
//! readonly flag, a bound index, a `Proxy` element, a lazy tail. Measured on a
//! `--profile profiling` build (#8069), that cost **14,053 instructions, 22
//! `Symbol::intern` calls and 8 heap allocations per store**, against roughly
//! 40 ns of marginal cost in rakudo.
//!
//! None of those questions is a property of *this store*; all of them are
//! properties of *that container*, settled when the binding was declared. Until
//! the resolved-descriptor architecture of #8069 §4.1 replaces the name-keyed
//! lane outright, this module answers them the way the rest of the VM already
//! does for its own hot probes: from the container's *embedded* metadata
//! (`ArrayData::has_type_meta`, `ArrayKind`) and from the monotonic
//! `env::*_possible()` latches, which are false for any program that never
//! declared the feature -- so a plain array store asks no env question at all
//! and reaches a `Vec` slot write.
//!
//! The deliberate shape, mirroring the hash twin: this lane never *errors*. Any
//! condition it is not certain about makes it return `None` and the caller falls
//! through to the full store, which is unchanged and remains the authority on
//! semantics.

use super::*;

impl Interpreter {
    /// Fast path for a simple positional element store: `@a[$i] = $v`.
    ///
    /// Returns `Some(Ok(()))` when it handled the store, `None` when the caller
    /// must fall through to [`Self::exec_index_assign_expr_named_op_inner`].
    /// It never returns `Some(Err(_))`.
    ///
    /// Preconditions (all must hold):
    /// - a positional subscript on an `@`-sigiled name, with no `:=` bindings in
    ///   scope and no pending `=`-element share;
    /// - a plain non-negative `Int` index that is already **in range** (an
    ///   autovivifying store needs the full path's native-fill and hole
    ///   bookkeeping);
    /// - a plain rvalue (not a bind marker, not `Nil`);
    /// - the variable resolves, in env, to a plain mutable `Array`/`ItemArray`
    ///   with no embedded type metadata, no declared shape, no `is default`,
    ///   and not readonly;
    /// - the destination slot is not itself a container (`ContainerRef` /
    ///   `Scalar` / `Proxy` / a varref / a `__mutsu_bound*` marker), which would
    ///   mean the store must write *through* it;
    /// - no per-element index metadata and no shaped-array declaration exists
    ///   anywhere in the program (the two monotonic latches).
    pub(crate) fn try_fast_array_element_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
        share_pending: bool,
    ) -> Option<Result<(), RuntimeError>> {
        if !is_positional || share_pending {
            return None;
        }
        // A `:=` binding in scope can make any name an alias; the slow path owns
        // that resolution.
        if !self.local_bind_pairs.is_empty() {
            return None;
        }
        // Once a second VM mutator thread exists, an element store is no longer
        // just a slot write: it is routed by the name-keyed cross-thread lanes
        // (`__mutsu_atomic_arr::`, `shared_array_elem_set`) or, where those
        // decline, excluded by ADR-0068's `ContainerStructGuard` -- five
        // documented routes whose interaction this lane reproduces none of.
        // Taking the guard alone would buy exclusion but not the lane's
        // accumulate-instead-of-snapshot semantics, so the honest answer is to
        // stand down entirely and let the audited path run. One relaxed atomic
        // load, and the same latch `ContainerStructGuard` itself consults.
        //
        // The cost is real and deliberate: a program that spawns a thread gets
        // no fast store anywhere, for the rest of the process. #8069 §3 (the
        // shared-container half of that issue) is what addresses the concurrent
        // case; this lane is the single-threaded half.
        if crate::value::container_lock::multi_mutator_threads_live() {
            return None;
        }
        // Both latches are false for the overwhelming majority of programs, and
        // each one being true means a *name-keyed* side table may describe this
        // element (`__mutsu_bound_index::`, `__mutsu_elem_share::`,
        // `__mutsu_deleted_index::`, `__mutsu_ro_index::`, or a declared shape).
        // This lane deliberately knows nothing about those: it declines instead.
        if crate::env::elem_index_meta_possible() || crate::env::shaped_array_dims_possible() {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        // Only a real `@` array. A sigilless alias (`my \a = @x`) has no sigil,
        // so this also excludes the `__mutsu_sigilless_alias::` redirect the
        // slow path resolves -- same reasoning as the `%` hash twin.
        if !var_name.as_bytes().starts_with(b"@") {
            return None;
        }
        // `env_root_descended_mut_tracked` -- the write chokepoint the full
        // store funnels through -- resolves a name in a strict precedence
        // order: a captured unit lexical, then the running routine's own
        // package `our @a`/`our %h` mirror, then env. The bare env key belongs
        // to whatever scope *loaded* the module, so for a module routine's own
        // `our @arr` it holds the loading script's same-named array. Reading
        // env directly, as the commit below does, is therefore only correct
        // when neither higher-precedence root claims the name; without this the
        // module's `@arr[0] = $v` wrote the script's array
        // (`t/modules/our-container-bare-name-resolution.t`).
        //
        // Both probes open with their own emptiness gate, so a program with no
        // unit lexicals and no `our` variables pays two `is_empty` checks. The
        // chokepoint's third root needs no probe here: it is keyed on
        // SIGIL-LESS names (an `our $a = [...]` reached as bare `a`), which the
        // `@` requirement above has already excluded.
        if self.unit_lexical_slot(var_name).is_some()
            || self.our_package_container_key(var_name).is_some()
        {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        // Only a plain non-negative Int subscript: a slice, `Whatever`, `Range`,
        // `Junction`, negative or lazy index all mean something else entirely.
        let ValueView::Int(idx_i) = self.stack[stack_len - 1].view() else {
            return None;
        };
        if idx_i < 0 {
            return None;
        }
        let i = idx_i as usize;
        let val_ref = &self.stack[stack_len - 2];
        // A `:=` element bind arrives wrapped in this marker Pair; `Nil` needs
        // the full path's container-default / type-object handling.
        if matches!(val_ref.view(), ValueView::Pair(name, _) if name == "__mutsu_bind_index_value")
            || matches!(val_ref.view(), ValueView::Nil)
        {
            return None;
        }
        let var_sym = code.const_sym(name_idx);
        // `is default(...)` and `:=`-bound-container readonly are both cheap
        // (an empty-map short-circuit and one interned-symbol probe).
        if self.var_default(var_name).is_some() || self.is_readonly_sym(var_sym) {
            return None;
        }
        // The target, read from env. `@` containers are not subject to the
        // (B) per-store scalar-slot seeding the slow path does for `$`-held
        // containers, so env is the right place to look -- but the local slot
        // must be pointing at the SAME backing node, or an in-place write here
        // would be invisible to the half the next read consults. That is
        // verified below, after the kind checks.
        // The `Gc` handle is cloned out so the env borrow ends here: every check
        // below, and the commit, needs `&mut self`. (The clone is what makes the
        // node's `strong_count` unusable as an aliasing signal afterwards -- see
        // the in-place write at the bottom, which does not consult it.)
        let (items, kind) = self.env().get_sym(var_sym).and_then(|v| match v.view() {
            ValueView::Array(items, kind) => Some((items.clone(), kind)),
            _ => None,
        })?;
        // A `List`/`ItemList` is immutable as a container (its element slots
        // cannot be replaced), and `Shaped`/`Lazy` have their own store rules.
        if !matches!(
            kind,
            crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray
        ) {
            return None;
        }
        // A typed array (`my Int @a`, `my int @a`) needs the full path's
        // constraint check and native wrapping. Read from the array's own
        // embedded metadata (ADR-0042), not a name-keyed map.
        if items.has_type_meta() {
            return None;
        }
        // ... and the name-keyed constraint the declaration may still carry.
        // Latched: no typed lexical in the program, no probe.
        if self.var_type_constraint_sym(var_sym).is_some() {
            return None;
        }
        // In-range only: an autovivifying store must resize with the right
        // native fill and materialize the hole set, which is the slow path's job.
        if i >= items.len() {
            return None;
        }
        // The destination slot must be a plain value. Anything container-shaped
        // there means the store writes THROUGH it (a `:=`-bound cell, a `Proxy`,
        // an itemized `Scalar` element, a varref back-reference) and the slow
        // path owns every one of those rules.
        match items.items().get(i).map(Value::view) {
            Some(
                ValueView::ContainerRef(_)
                | ValueView::Scalar(_)
                | ValueView::Proxy { .. }
                | ValueView::VarRef { .. },
            ) => return None,
            Some(ValueView::Pair(name, _)) if name.as_str().starts_with("__mutsu_bound") => {
                return None;
            }
            None => return None,
            Some(_) => {}
        }
        // `@a[0] = @a` stores the array *itself* (a genuinely circular
        // structure, as in rakudo) rather than an itemized copy -- a distinction
        // the slow path makes and this lane does not reproduce.
        if matches!(
            self.stack[stack_len - 2].view(),
            ValueView::Array(source_items, ..) if crate::gc::Gc::ptr_eq(&items, &source_items)
        ) {
            return None;
        }
        // Dual-store coherence: if this frame has a local slot for the name, it
        // must hold the same backing node, so the single in-place write below is
        // seen by both halves. A diverged slot (a stale COW copy) falls through
        // to the slow path, which has the machinery to reconcile it.
        if let Some(slot) = self.resolve_local_slot(code, target_slot, var_name) {
            match self.locals[slot].view() {
                ValueView::Array(local_items, ..)
                    if crate::gc::Gc::ptr_eq(&items, &local_items) => {}
                // An untouched/absent slot is fine: nothing there to diverge.
                ValueView::Nil => {}
                _ => return None,
            }
        }

        // ---- All checks passed; commit. ----
        self.stack.pop();
        let val = self.stack.pop().unwrap();
        // ADR-0040 slice 1: an element is a Scalar container, so an aggregate
        // stored into it itemizes. (`exec_index_assign_expr_named_op_seeded_inner`
        // has already applied `itemize_for_element_store` to the stack value for
        // this exact index shape; `itemize_value` is idempotent over that.)
        let stored = Self::itemize_value(val.clone());
        // Container identity (§3): an element write on a SHARED array mutates
        // through the backing node so every by-value holder of the same
        // container observes it. COW would detach it; copies detach at copy time
        // via `detach_shared_container`. This mirrors the slow path's
        // `use_inplace` choice exactly.
        // (A sole-owner array takes the same route: with `strong_count == 1`
        // `Gc::make_mut` could not clone anyway, and the `ValueView` hands out a
        // `&Gc`, not the `&mut Gc` `make_mut` would need.)
        //
        // SAFETY: audited aliased in-place container write (see
        // `value::aliased_mut`). No borrow into this node is live across the
        // write -- `items` is a `Gc` handle, the `ValueView` it came from is not
        // read again below, and this lane runs on the executing thread only.
        let data: &mut crate::value::ArrayData = unsafe { crate::value::gc_contents_mut(&items) };
        data.items_mut()[i] = stored;
        // The hole set, maintained exactly as `mark_initialized_index` does:
        // materializing it from `None` is what makes the OTHER gap-marker slots
        // read as holes, so this lane must not skip that transition.
        data.initialized
            .get_or_insert_with(std::collections::HashSet::new)
            .insert(i);
        // A single positional index names one scalar slot, so the assignment's
        // rvalue is itemized (`@z = (@a[0] = 1, 2)` has two elements, the first
        // itemized) -- the same rule the slow path's final push applies.
        self.stack.push(Self::itemize_value(val));
        Some(Ok(()))
    }
}
