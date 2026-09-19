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
    /// [`Self::try_fast_array_element_assign`], consulted *before* the element
    /// store's shared preamble rather than after it.
    ///
    /// The lane below deletes the store's own cost, but it used to sit at the
    /// bottom of `exec_index_assign_expr_named_op`'s dispatch chain, so a plain
    /// `@a[$i] = $v` still paid the whole preamble on the way down: a `Range`
    /// receiver probe against both the local slot and env, a deferred
    /// vivification-token probe (which allocated the variable name as a
    /// `String` and scanned `code.locals` by name), the ADR-0039 unit-lexical
    /// cell seed/restore, a lazy-array reify probe, the rvalue itemization
    /// hook, and a `Seq`/`Proxy` destination resolve that clones the target and
    /// the addressed element. Measured on a `--profile profiling` build
    /// (#8069), that residue was **3,097 instructions and one heap allocation**
    /// per store after the lane itself had been reduced to a `Vec` slot write;
    /// running the lane first brings it to 1,210 instructions and none.
    ///
    /// Every one of those probes asks about a shape this lane has *already
    /// refused* -- a `Range`, a token, a unit lexical, a `LazyList`, a `Seq`, a
    /// `Proxy`, an aggregate rvalue. So the honest order is to ask the cheap,
    /// container-answered questions first and only run the preamble for the
    /// stores that actually need it. This function is that reordering: it adds
    /// the handful of guards that the preamble would otherwise have
    /// established, then defers to the same lane.
    ///
    /// It touches nothing -- not the stack, not env, not a local slot -- unless
    /// the lane it calls commits, so declining is free and the full path
    /// downstream runs exactly as it did before.
    pub(crate) fn try_fast_array_element_assign_early(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        if !is_positional {
            return None;
        }
        // Slice 2b's `=`-element share is captured in the preamble and consumed
        // by the lane's caller; the early call site is above that capture, so
        // the only safe answer while one is pending is to decline (and, above
        // all, NOT to clear the flag).
        if self.element_share_pending {
            return None;
        }
        // The two name-keyed cross-thread lanes (`try_shared_hash_element_assign`
        // / `try_shared_array_element_assign`) are skipped by running here, and
        // they own the store whenever a thread shares this env. Their own gate
        // is this exact flag.
        if self.shared_vars_active {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        // The preamble's `Whatever` refusal, the `Seq` element-cell store and
        // the slice paths all need a subscript this lane would reject anyway;
        // check it up front so the guards below are only paid for the shape the
        // lane can actually serve.
        if !matches!(self.stack[stack_len - 1].view(), ValueView::Int(n) if n >= 0) {
            return None;
        }
        // `itemize_for_element_store` (the preamble's ADR-0040 rvalue hook) and
        // `fetch_proxy_for_store` are both the IDENTITY on a plain scalar
        // rvalue, which is what lets this call site skip them. An aggregate
        // rvalue itemizes, can BE the target (`@a[0] = @a` stores a genuinely
        // circular structure), and a `Proxy` rvalue must FETCH -- all three are
        // the preamble's business. Deliberately an allow-list: a variant this
        // lane has not reasoned about falls through to the unchanged full path.
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
        // Checked here as well as in the lane: every guard below is keyed on
        // the name, and only an `@` name can reach the lane at all.
        if !var_name.as_bytes().starts_with(b"@") {
            return None;
        }
        // ADR-0039 slice 1: a compunit's own file-scope `@` is stored in the
        // `unit_lexicals` cell, and the preamble seeds env from it around the
        // store. The lane reads env directly, so it must not run for a name the
        // seed would have redirected. (`unit_lexical_slot`, which the lane
        // already probes, does not see the MAINLINE bucket this cell lookup
        // checks first, so the two are not interchangeable.) Opens with the
        // same `unit_lexicals.is_empty()` gate, so a program with no captured
        // file-scope lexicals pays one `is_empty`.
        if self.unit_lexical_container_cell(var_name).is_some() {
            return None;
        }
        // A variable still holding a deferred vivification token is resolved by
        // `try_deferred_token_index_assign`, which finds its slot BY NAME. The
        // lane's own dual-store coherence check uses the compiler-baked
        // `target_slot` and would catch the token there -- except when the
        // baked slot is out of range for this frame, where the lane skips the
        // check entirely and the by-name search would still find one. Close
        // that gap explicitly; in the common case it is one comparison.
        if target_slot.is_some_and(|slot| (slot as usize) >= self.locals.len())
            && self.find_local_slot(code, var_name).is_some()
        {
            return None;
        }
        self.try_fast_array_element_assign(code, name_idx, is_positional, target_slot, false)
    }

    /// True when a recorded `:=` bind pair names the `@` variable this element
    /// store is about to write, so the store may have to be reconciled with a
    /// second local slot holding a detached copy of the container.
    ///
    /// This replaces the lane's opening refusal, which declined whenever
    /// `local_bind_pairs` was non-empty at all. That field is **frame-global**,
    /// so **one** `my $x := $y` anywhere in a scope pushed every `@a[$i] = $v`
    /// in that scope back onto
    /// [`Interpreter::exec_index_assign_expr_named_op_inner`] -- 23
    /// `Symbol::intern` calls and 8 heap allocations per store, the numbers
    /// [#8069](https://github.com/tokuhirom/mutsu/issues/8069) §2 was filed
    /// with -- including stores into arrays the binding cannot reach.
    /// `benchmarks/bench-threads-serial.raku` is the measured case: its single
    /// `my $sref := $seed` cost that file's element stores 15,858 instructions
    /// each against a `:=`-free copy's 1,214, and made its row the worst ratio
    /// in `bench-history.tsv` with no array binding and no thread in sight
    /// ([#8747](https://github.com/tokuhirom/mutsu/issues/8747)). Asking the
    /// question per name rather than per frame is the same narrowing
    /// [`Interpreter::slot_is_bind_pair_source`] already made for the *scalar*
    /// store, against the same field and the same benchmark; the element store
    /// is the half that was left behind.
    ///
    /// **Why a name test is the right question here.** A pair is a pair of local
    /// *slots*, and the only thing the VM does with one is copy a scalar store's
    /// value from the source slot into the alias slot
    /// (`exec_set_local_op_inner`'s reverse-propagation loop,
    /// `exec_compound_assign_scalar_for`'s forward one). An element store writes
    /// no local slot at all -- it mutates the backing node in place, which every
    /// holder of that node observes -- so a pair can only matter when one of its
    /// two slots IS this array, i.e. when the frame holds a second slot for it
    /// whose value the lane has not checked. Most pairs cannot be that: a
    /// whole-container `:=` (`my @b := @a`) is served by a shared
    /// `ContainerCell` and returns *before* any pair is recorded, which is why
    /// the pairs are overwhelmingly scalar-only. They are not exclusively so --
    /// an `@`/`%`/`&` bind routed through `SetGlobal` (a free-variable `:=`
    /// inside a named sub) skips the cell branch and reaches
    /// [`Interpreter::resolve_pending_alias_binds`], which records the pair
    /// bidirectionally on two `@` slots -- so this asks the question rather than
    /// assuming the answer.
    ///
    /// Matching by NAME, not by slot index, is deliberate: a name can occupy
    /// several `code.locals` slots (a same-named shadow) while the lane's own
    /// `target_slot` resolves to just one of them, and a frame torn down by an
    /// exception can leave a pair whose slot index does not address this frame
    /// at all (the reverse-propagation loop bounds-checks for exactly that).
    /// Both cases resolve to "decline", which is always safe.
    ///
    /// A program with no `:=` binding pays one `is_empty` check on the `Vec` and
    /// reads nothing, exactly as before.
    #[inline]
    fn bind_pair_names_array(&self, code: &CompiledCode, var_name: &str) -> bool {
        if self.local_bind_pairs.is_empty() {
            return false;
        }
        self.local_bind_pairs.iter().any(|&(source, target)| {
            [source, target]
                .iter()
                .any(|&slot| code.locals.get(slot).is_some_and(|n| n == var_name))
        })
    }

    /// Fast path for a simple positional element store: `@a[$i] = $v`.
    ///
    /// Returns `Some(Ok(()))` when it handled the store, `None` when the caller
    /// must fall through to [`Self::exec_index_assign_expr_named_op_inner`].
    /// It never returns `Some(Err(_))`.
    ///
    /// Preconditions (all must hold):
    /// - a positional subscript on an `@`-sigiled name that is not itself named
    ///   by a `:=` bind pair recorded in this frame, and no pending `=`-element
    ///   share;
    /// - a plain non-negative `Int` index that is already **in range** (an
    ///   autovivifying store needs the full path's native-fill and hole
    ///   bookkeeping);
    /// - a plain rvalue (not a bind marker, not `Nil`);
    /// - the variable resolves, in env, to a plain mutable `Array`/`ItemArray`
    ///   with no embedded type metadata, no declared shape, no `is default`,
    ///   and not readonly -- either held directly, or through exactly one
    ///   `:=` `ContainerCell` (`my @alias := @a`), which the lane descends;
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
        // A `:=` bind pair that names THIS array; see
        // [`Interpreter::bind_pair_names_array`] for why the frame-global
        // `is_empty` test this replaces was far wider than the question.
        if self.bind_pair_names_array(code, var_name) {
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
        //
        // A `:=`-bound container (`my @alias := @a`) is reached through a
        // `ContainerCell`, so env hands out a `ContainerRef` rather than the
        // `Array` itself and this match used to end the lane there. That is the
        // ONE thing that declined for the bound shape -- measured, neither the
        // `unit_lexical_container_cell` guard in the early wrapper nor
        // `is_readonly_sym` fires for it -- and it cost 3,823 ns per store
        // against a plain array's 782 ns, where rakudo charges 226 vs 224
        // ([#8307](https://github.com/tokuhirom/mutsu/issues/8307)).
        //
        // Descending exactly one cell is sound for this lane because the write
        // at the bottom mutates the backing node IN PLACE: the cell's inner
        // `Value` keeps pointing at that node, so every alias sharing the cell
        // observes the store, which is precisely what the cell is for. The cell
        // itself is never replaced, so nothing here needs to hold its lock past
        // reading the handle out. `cell` is carried to the coherence check
        // below, which has to compare a different thing for a bound target.
        let (items, kind, cell) = {
            let v = self.env().get_sym(var_sym)?;
            match v.view() {
                ValueView::Array(items, kind) => (items.clone(), kind, None),
                // A holder-local itemization flavour makes a read of this cell
                // yield something other than its contents
                // (`Value::into_deref`), a rule this lane does not reproduce.
                ValueView::ContainerRef(arc) if !v.container_ref_is_itemized() => {
                    let arc = crate::gc::Gc::clone(&arc);
                    // `ok()?`, not `unwrap()`: a poisoned cell is one more thing
                    // this lane is not certain about, and declining is what it
                    // does with those. It also keeps the panic-surface ratchet
                    // (#8186) where it was.
                    let inner = match arc.lock().ok()?.view() {
                        ValueView::Array(items, kind) => Some((items.clone(), kind)),
                        _ => None,
                    };
                    match inner {
                        Some((items, kind)) => (items, kind, Some(arc)),
                        None => return None,
                    }
                }
                _ => return None,
            }
        };
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
        // must name the same container as env did, so the single in-place write
        // below is seen by both halves. A diverged slot (a stale COW copy) falls
        // through to the slow path, which has the machinery to reconcile it.
        //
        // What "the same container" means differs by shape. For a plain array it
        // is the same backing node. For a `:=`-bound one the slot holds the
        // CELL, not the node, and the cell is the one thing every alias of the
        // binding genuinely shares -- so cell identity is what makes the write
        // visible to both halves, and a slot holding a bare `Array` while env
        // holds a cell (or the reverse) is exactly the divergence to refuse.
        if let Some(slot) = self.resolve_local_slot(code, target_slot, var_name) {
            match self.locals[slot].view() {
                ValueView::Array(local_items, ..)
                    if cell.is_none() && crate::gc::Gc::ptr_eq(&items, &local_items) => {}
                ValueView::ContainerRef(local_cell)
                    if cell
                        .as_ref()
                        .is_some_and(|c| crate::gc::Gc::ptr_eq(c, &local_cell)) => {}
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
        let stored = Self::itemize_value_for_element_store(val.clone());
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
        data.mark_initialized(i);
        // A single positional index names one scalar slot, so the assignment's
        // rvalue is itemized (`@z = (@a[0] = 1, 2)` has two elements, the first
        // itemized) -- the same rule the slow path's final push applies.
        self.stack.push(Self::itemize_value(val));
        Some(Ok(()))
    }
}
