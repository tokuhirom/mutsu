//! Fast path for the plain chained element store `@a[$i][$j] = $v`.
//!
//! The single-subscript store has had a fast lane on both axes for a while --
//! `try_fast_array_element_assign` for `@a[$i] = $v`, `try_fast_hash_element_assign`
//! for `%h{$k} = $v` -- and both are consulted before the shared preamble. The
//! *chained* store, which ADR-0068 §4 step 3 treats as its own funnel, never got
//! one: `@a[$i][$j] = $v` goes through `exec_index_assign_expr_nested_op_body` in
//! full, every time.
//!
//! What that costs, measured the way the other two lanes were (differenced
//! `callgrind` runs of the store loop against the identical loop without it,
//! `--profile profiling`, `MUTSU_JIT=off`):
//!
//! | form | Ir per store |
//! | --- | ---: |
//! | `@a[$i] = $v` | 1,115 |
//! | `%h{$k} = $v` | 3,642 |
//! | `@a[$i][$j] = $v` | **12,134** |
//!
//! Eleven times the positional single-subscript store, and enough that
//! `benchmarks/bench-index-store.raku` spends more instructions in its 100,000
//! chained stores than in its 500,000 array stores and 200,000 hash stores put
//! together ([#8069](https://github.com/tokuhirom/mutsu/issues/8069)).
//!
//! The reason is the same one #8069 §2 gives for the single-subscript store: the
//! body re-derives the whole *declaration* of the target on every write, by
//! string key. It copies the variable name out of the constant pool, resolves a
//! type constraint by name, probes env by name for a user-object root, a `Pair`
//! root and a vivification-needed root, scans `code.locals` by name (twice --
//! once to nil the slot, once to restore it), and re-interns the name at each
//! env probe: **seven `Symbol::intern` calls per store**, where the positional
//! single-subscript lane makes none. On top of that it renders BOTH subscripts
//! to a decimal `String` via `to_string_value` and immediately parses each one
//! back with `str::parse::<usize>()`, for indices that arrived on the stack as
//! `Int`.
//!
//! None of those questions is a property of *this store*. They are properties of
//! *those two containers*, and both containers can answer them from their own
//! embedded metadata (`ArrayData::has_type_meta`, `ArrayKind`, `HashData::has_type_meta`)
//! and from the monotonic `env::*_possible()` latches -- exactly as the
//! single-subscript lanes already do.
//!
//! Same deliberate shape as those lanes, and the same contract: **this lane
//! never errors.** Any condition it is not certain about makes it return `None`,
//! it touches nothing -- not the stack, not env, not a local slot -- unless it
//! commits, and `exec_index_assign_expr_nested_op_body` remains the authority on
//! semantics for everything it declines.

use super::*;

/// The destination reached by the FIRST subscript: the intermediate container
/// the second subscript then indexes into.
enum NestedStep {
    Array(crate::gc::Gc<crate::value::ArrayData>, usize),
    Hash(crate::gc::Gc<crate::value::HashData>, String),
}

impl Interpreter {
    /// Fast path for `@a[$i][$j] = $v` and its three associative spellings.
    ///
    /// Returns `Some(Ok(()))` when it handled the store, `None` when the caller
    /// must fall through to [`Self::exec_index_assign_expr_nested_op_body`]. It
    /// never returns `Some(Err(_))`.
    ///
    /// Naming follows the opcode's, which is the reverse of how the source
    /// reads: `inner_*` is the FIRST subscript (the one that indexes the
    /// variable's own container) and `outer_*` is the SECOND (the one that
    /// indexes the container the first subscript reached). Stack, bottom to
    /// top: `[value, outer_idx, inner_idx]`.
    ///
    /// Preconditions (all must hold):
    /// - single-threaded, no shared-var env, no pending `=`-element share, and
    ///   neither element-index-metadata nor shaped-array latch set;
    /// - both subscripts plain: a non-negative `Int` for a positional one, a
    ///   plain `Int`/`Str` for an associative one;
    /// - a plain scalar rvalue from the same allow-list the single-subscript
    ///   lanes use, which is what makes the preamble's `fetch_proxy_for_store`
    ///   and itemization hooks the identity;
    /// - the root is an `@`/`%` name that resolves, in env, to a plain mutable
    ///   `Array`/`ItemArray` or `Hash` with no embedded type metadata, no native
    ///   backing, no declared constraint, no `is default` and not readonly, and
    ///   which no unit lexical, `our` package mirror or `:=` bind pair also
    ///   claims;
    /// - the element the first subscript addresses already exists and is itself
    ///   a plain mutable `Array`/`Hash` with no type metadata -- matching the
    ///   second subscript's bracket kind, so no autovivification and no
    ///   stringify-a-positional-index-into-a-hash-key is needed;
    /// - the slot the second subscript addresses is in range (positional) and
    ///   holds a plain value, not a container that would mediate the store;
    /// - if this frame has a local slot for the root name, it holds the same
    ///   backing node as env (or nothing), so one in-place write is seen by both
    ///   halves of the dual store.
    pub(crate) fn try_fast_nested_element_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        outer_positional: bool,
        inner_positional: bool,
    ) -> Option<Result<(), RuntimeError>> {
        // Slice 2b's `=`-element share is captured in the body and would be
        // silently dropped by committing here; the only safe answer while one is
        // pending is to decline without clearing the flag.
        if self.element_share_pending {
            return None;
        }
        // The name-keyed cross-thread lanes own the store whenever a thread
        // shares this env, and this is their own gate.
        if self.shared_vars_active {
            return None;
        }
        // Once a second VM mutator thread exists, a chained store is ADR-0068 §4
        // step 3's funnel: the body takes a `ContainerStructGuard` keyed on the
        // outermost cell the descent stepped through, and taking that guard here
        // would buy exclusion but not the name-keyed lanes' accumulate-instead-
        // of-snapshot semantics. Stand down entirely, exactly as the
        // single-subscript positional lane does, and let the audited path run.
        if crate::value::container_lock::multi_mutator_threads_live() {
            return None;
        }
        // Each latch being true means a *name-keyed* side table may describe one
        // of the two elements this store walks through (`__mutsu_bound_index::`,
        // `__mutsu_elem_share::`, `__mutsu_deleted_index::`, `__mutsu_ro_index::`,
        // or a declared shape). This lane knows nothing about those.
        if crate::env::elem_index_meta_possible() || crate::env::shaped_array_dims_possible() {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 3 {
            return None;
        }
        // A `Junction`/slice/`Whatever`/`Range` subscript at either level means
        // something else entirely (the body autothreads the second one per key
        // and distributes an RHS across the first), and a `WhateverCode` has to
        // be resolved against the container it indexes.
        if !Self::plain_subscript(&self.stack[stack_len - 1], inner_positional)
            || !Self::plain_subscript(&self.stack[stack_len - 2], outer_positional)
        {
            return None;
        }
        // `itemize_for_element_store` and `fetch_proxy_for_store` are both the
        // IDENTITY on a plain scalar rvalue, which is what lets this lane skip
        // the body's rvalue hooks. Deliberately the same allow-list as the
        // single-subscript lanes: an aggregate rvalue itemizes and can be the
        // target, a `Proxy` rvalue must FETCH, and a `__mutsu_bind_index_value`
        // marker is a `:=` bind installing a container rather than a store.
        if !matches!(
            self.stack[stack_len - 3].view(),
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
        // The root's own kind follows its sigil, and the FIRST subscript must
        // address it the way that kind is addressed. A `%h[0]` / `@a<k>` root
        // step is not a shape this lane has reasoned about.
        let root_is_array = match var_name.as_bytes().first() {
            Some(b'@') => true,
            Some(b'%') => false,
            _ => return None,
        };
        if root_is_array != inner_positional {
            return None;
        }
        // ADR-0039 slice 1 and the `env_root_descended_mut_tracked` precedence
        // order: a captured unit lexical and the running routine's own package
        // `our` mirror both outrank the bare env key this lane reads, and the
        // bare key belongs to whatever scope *loaded* the module. Reading env
        // for a name either of them claims is how the single-subscript lane
        // wrote the loading script's array instead of the module's
        // (`t/modules/our-container-bare-name-resolution.t`). All three probes
        // open with their own emptiness gate.
        if self.unit_lexical_container_cell(var_name).is_some()
            || self.unit_lexical_slot(var_name).is_some()
            || self.our_package_container_key(var_name).is_some()
        {
            return None;
        }
        // A `:=` bind pair naming this root means the frame may hold a second
        // local slot for it whose value this lane has not checked.
        if self.bind_pair_names_container(code, var_name) {
            return None;
        }
        let var_sym = code.const_sym(name_idx);
        // `is default(...)`, a `:=`-bound-container readonly mark and a declared
        // type constraint are all cheap (an empty-map short-circuit and two
        // interned-symbol probes) and all mean the body's machinery is needed:
        // a default decides what a `Nil` store writes, a constraint decides what
        // an autovivified intermediate container is allowed to be.
        if self.var_default(var_name).is_some()
            || self.is_readonly_sym(var_sym)
            || self.var_type_constraint_sym(var_sym).is_some()
        {
            return None;
        }
        // The root container, read from env. The `Gc` handle is cloned out so
        // the env borrow ends here; every check below, and the commit, needs
        // `&mut self`.
        //
        // A `:=`-bound root arrives as a `ContainerRef` rather than the
        // container itself, and a user-object / `Seq` / `LazyList` / `Pair` root
        // has its own descent in the body. All of them decline here.
        enum Root {
            Array(crate::gc::Gc<crate::value::ArrayData>),
            Hash(crate::gc::Gc<crate::value::HashData>),
        }
        let root = {
            let v = self.env().get_sym(var_sym)?;
            match v.view() {
                ValueView::Array(items, kind) if root_is_array => {
                    // A `List`/`ItemList` is immutable as a container and
                    // `Shaped`/`Lazy` have their own store rules.
                    if !matches!(
                        kind,
                        crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray
                    ) {
                        return None;
                    }
                    Root::Array(items.clone())
                }
                ValueView::Hash(map) if !root_is_array => Root::Hash(map.clone()),
                _ => return None,
            }
        };
        // A typed container (`my Int @a`, `my Str %h`) needs the body's
        // constraint check, and a natively-backed array (ADR-0015/ADR-0030)
        // keeps `items` as a seed that only the sync chokepoint may read.
        match &root {
            Root::Array(items) => {
                if items.has_type_meta() || items.has_native_backing() {
                    return None;
                }
            }
            Root::Hash(map) => {
                if map.has_type_meta() {
                    return None;
                }
            }
        }
        // Dual-store coherence: a local slot for this name must hold the SAME
        // backing node as env, so the single in-place write below is seen by
        // both halves. A diverged slot (a stale COW copy, a deferred
        // vivification token, an unrelated shadow) falls through to the body,
        // which has the machinery to reconcile it. `Nil` is an untouched slot,
        // with nothing to diverge.
        if let Some(slot) = self.find_local_slot(code, var_name) {
            let coherent = match (&root, self.locals[slot].view()) {
                (Root::Array(items), ValueView::Array(local, ..)) => {
                    crate::gc::Gc::ptr_eq(items, &local)
                }
                (Root::Hash(map), ValueView::Hash(local)) => crate::gc::Gc::ptr_eq(map, &local),
                (_, ValueView::Nil) => true,
                _ => false,
            };
            if !coherent {
                return None;
            }
        }
        // Step one: the element the FIRST subscript addresses. It must already
        // exist -- an autovivifying descent has to pick the new container's
        // kind, apply the right native fill and materialize the hole set, which
        // is the body's job -- and it must be a plain mutable container of the
        // kind the SECOND subscript addresses. A `ContainerRef` element (a `:=`
        // bound row) is a write *through* a shared cell, a `Pair` element takes
        // a whole-container store into the Pair's value, and a `Buf`/`Blob`
        // `Instance` keeps its elements in an attribute cell: three more of the
        // body's rules this lane does not reproduce.
        let step = {
            let elem = match &root {
                Root::Array(items) => {
                    let i = Self::plain_index(&self.stack[stack_len - 1])?;
                    items.items().get(i)?.clone()
                }
                Root::Hash(map) => {
                    let key = self.stack[stack_len - 1].to_string_value();
                    map.get(&key)?.clone()
                }
            };
            match elem.view() {
                ValueView::Array(items, kind) if outer_positional => {
                    if !matches!(
                        kind,
                        crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray
                    ) || items.has_type_meta()
                        || items.has_native_backing()
                    {
                        return None;
                    }
                    let j = Self::plain_index(&self.stack[stack_len - 2])?;
                    // In range only, for the same reason the first step must
                    // already exist.
                    if j >= items.len() {
                        return None;
                    }
                    // The destination must be a plain value. Anything
                    // container-shaped there means the store writes THROUGH it,
                    // and the body owns every one of those rules.
                    match items.items().get(j).map(Value::view) {
                        Some(
                            ValueView::ContainerRef(_)
                            | ValueView::Scalar(_)
                            | ValueView::Proxy { .. }
                            | ValueView::VarRef { .. },
                        ) => return None,
                        Some(ValueView::Pair(name, _))
                            if name.as_str().starts_with("__mutsu_bound") =>
                        {
                            return None;
                        }
                        None => return None,
                        Some(_) => {}
                    }
                    NestedStep::Array(items.clone(), j)
                }
                ValueView::Hash(map) if !outer_positional => {
                    if map.has_type_meta() {
                        return None;
                    }
                    let key = self.stack[stack_len - 2].to_string_value();
                    // Same destination rule as the positional arm, plus the
                    // `HashEntryRef` alias the hash lane refuses. An ABSENT
                    // entry is fine here: inserting one is what `%h<a><b> = v`
                    // means once `%h<a>` exists, and needs no autovivification.
                    if let Some(existing) = map.get(&key) {
                        match existing.view() {
                            ValueView::HashEntryRef { .. }
                            | ValueView::Scalar(..)
                            | ValueView::ContainerRef(_)
                            | ValueView::Proxy { .. }
                            | ValueView::VarRef { .. } => return None,
                            ValueView::Pair(name, _) if name.starts_with("__mutsu_bound") => {
                                return None;
                            }
                            _ => {}
                        }
                    }
                    NestedStep::Hash(map.clone(), key)
                }
                _ => return None,
            }
        };

        // ---- All checks passed; commit. ----
        self.stack.pop();
        self.stack.pop();
        let val = self.stack.pop().unwrap();
        // ADR-0040 slice 4: a value assigned through a chained subscript lands
        // in an element slot, so it itemizes -- the body's own expression, so
        // the stored value and the pushed rvalue are the same ones it would
        // have produced.
        let stored = val.itemize_for_element_store();
        match step {
            NestedStep::Array(items, j) => {
                // Container identity (§3): an element write on a shared node
                // mutates through it so every by-value holder observes it; COW
                // would detach it. This mirrors the body's `gc_data_mut` choice
                // exactly.
                //
                // SAFETY: audited aliased in-place container write (see
                // `value::aliased_mut`). No borrow into this node is live across
                // the write -- `items` is a `Gc` handle, the `ValueView` it came
                // from is not read again, and this lane runs on the executing
                // thread only, having declined once a second mutator exists.
                let data: &mut crate::value::ArrayData =
                    unsafe { crate::value::gc_contents_mut(&items) };
                // The body reaches this same slot through `autoviv_resize_tracking`,
                // which for an IN-RANGE index does no resize and records the
                // write only into an already-materialized hole set. Calling it
                // rather than reproducing that rule keeps the two in one place;
                // the fill is unused because the index is in range, and this
                // lane has already declined every typed container anyway.
                Self::autoviv_resize_tracking(data, j, Self::native_fill_for_constraint(None))
                    .ok()?;
                Value::assign_element_slot(&mut data.items_mut()[j], stored.clone());
            }
            NestedStep::Hash(map, key) => {
                // SAFETY: as above.
                let data: &mut crate::value::HashData =
                    unsafe { crate::value::gc_contents_mut(&map) };
                Value::hash_insert_through(&mut data.map, key, stored.clone());
            }
        }
        self.stack.push(stored);
        Some(Ok(()))
    }

    /// A subscript this lane can serve: a plain non-negative `Int` for a
    /// positional bracket, a plain `Int`/`Str` for an associative one. Every
    /// other shape -- a slice, a `Junction`, a `Range`, a `Whatever`, a
    /// `WhateverCode` needing resolution against the container it indexes, a
    /// `Seq` index -- means something the body handles.
    #[inline]
    fn plain_subscript(idx: &Value, positional: bool) -> bool {
        if positional {
            matches!(idx.view(), ValueView::Int(n) if n >= 0)
        } else {
            matches!(idx.view(), ValueView::Str(_) | ValueView::Int(_))
        }
    }

    /// The `usize` a positional subscript addresses, given
    /// [`Self::plain_subscript`] has already accepted it.
    #[inline]
    fn plain_index(idx: &Value) -> Option<usize> {
        match idx.view() {
            ValueView::Int(n) if n >= 0 => usize::try_from(n).ok(),
            _ => None,
        }
    }
}
