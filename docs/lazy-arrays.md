# Lazy infinite arrays — `my @a = 1..*` reify-on-demand (design)

> **Status:** IN PROGRESS (2026-06-19). Captures the analysis behind the "lazy
> infinite sequence" track (PLAN.md §C / BLOCKERS "Real lazy infinite
> sequences"). The reify-on-demand machinery already works for **scalar-held**
> and **gather-coroutine** lazy values; the gap is that infinite **ranges and
> `...` sequences assigned to an `@` array are materialized (capped at 100k)**
> at assignment, losing laziness. Closing it is a multi-slice campaign because
> array *mutation* ops assume a materialized backing.

## Progress log

- **L1 — lazy `.gist`/`.Str`/`.raku` (DONE, #3306).** A
  `Value::Array(_, ArrayKind::Lazy)` now renders `[...]` (gist/raku/say) /
  `...` (Str/print/interp) instead of dumping its capped 100k backing. Four
  sites: `dispatch_core_repr` gist arm + both nested `gist_item` closures,
  `dispatch_core_coerce` Str, `runtime::utils::gist_value` (say path),
  `value::display::to_string_value`. **Finding:** Rakudo's lazy-array gist is a
  flat `[...]`/`(...)`, NOT the bounded-prefix `(1 2 3 … )` this doc originally
  assumed — much simpler.
- **L5 — lazy `.elems` → X::Cannot::Lazy (DONE, #3310).** A lazy
  `ArrayKind::Lazy` array's `.elems` now throws (was returning the 100k cap).
  Guard added before `as_list_items` in `dispatch_core_numeric`, reusing
  `range_elems_lazy_failure`. **L5b (DONE):** `.Numeric`/`.Int`/`.Real`/`.end`
  and prefix-`+` on a lazy array now also throw `X::Cannot::Lazy` (action
  `.elems`, matching raku). Shared `runtime::utils::cannot_lazy_failure(action)`
  builds the Failure value; guards in `dispatch_core_coerce` (numeric coercions),
  `dispatch_core_list` (`.end`), and `vm_misc_ops::exec_num_coerce_op` (prefix
  `+`). t/lazy-array-numeric.t. **Still capped:** `.elems` on a *finite* `lazy
  gather` LazyList returns the realized count (raku throws — is-lazy True); the
  gather `.elems` path special-cases `__mutsu_lazylist_from_gather`, left alone.
- **L1b — coroutine `Value::LazyList` gist + dual-rep wart (DONE).** A genuinely
  lazy `Value::LazyList` (gather coroutine with the `lazy` marker, infinite
  sequence/closure/scan spec, lazy map/grep pipeline) now renders raku's
  placeholder under gist/Str/raku/perl/say/print/`~`/interpolation instead of
  forcing (which hung for an infinite gather held in a variable) or returning
  empty. **Dual-rep wart resolved:** the `@`-assign preserve sites tag the
  LazyList with an `ARRAY_CONTEXT_MARKER` env flag, so gist renders `[...]`
  (held in `@a`) vs `(...)` (bare `$s` Seq) and `.WHAT` reports `Array` vs `Seq`.
  Key helpers: `LazyList::is_genuinely_lazy()` (gates on the `lazy` marker for
  the gather family — a *plain* `gather` is `.is-lazy` `False` and must
  materialize, the regression that gated this), `with_array_context()`,
  `in_array_context()`, and the shared `value::lazy_list_placeholder()`. Guards
  added to BOTH method-dispatch paths (`vm_call_method_ops` for direct calls,
  `vm_call_method_mut_ops` for variable-target calls — the latter was the hang,
  since `@a.gist`/`$s.gist` route through CallMethodMut) plus the runtime slow
  path, `runtime::utils::gist_value`, and `value::display::to_string_value`.
  t/lazy-list-gist-context.t. **Known follow-up:** `@a[]` zen-slice
  interpolation of a lazy array still renders empty (the list-flatten interp
  path, separate from `.Str`); no hang. With the dual-rep wart resolved, **L2**
  (preserve infinite Range / closure_seq / lazy_pipe as a reify `LazyList` on
  `@`-assign instead of capping to an Array) is now unblocked.
- **L2a — `my @a = 1..*` stays a reify `LazyList` (DONE).** An infinite *integer*
  range assigned to `@a` is converted to a `SequenceSpec::Arithmetic` `LazyList`
  (array-context tagged) by `runtime::utils::infinite_int_range_to_lazy_array`,
  seeded with the same 100k prefix `coerce_to_array` would have produced — so
  eager reads that consume the cache (`.head`/`.first`/`.map`/`.grep`) behave
  exactly as before, while `@a[N]` past the prefix reifies on demand
  (`@a[200000]` → 200001, was `Nil`). `.is-lazy`/`.gist`/`.WHAT` and the
  X::Cannot::Lazy numeric guards (extended to `LazyList` via `is_lazy_count_source`)
  all stay correct. **Mutation:** end-mutations raku rejects (push/pop/append)
  throw `X::Cannot::Lazy`; front mutations (unshift/prepend/shift/splice,
  `@a[i]=v`, `@a[i]:delete`) reify the cached prefix to a real Array first via
  `reify_lazy_array_slot` (no worse than the pre-L2 capped Array). `@a[i]:exists`
  answers from the index (any non-negative index exists) without forcing.
  Chokepoints: `vm_call_method_mut_ops` (method mutators), `vm_data_ops`
  (ArrayPush opcode), `vm_var_assign_ops` (`@a[i]=v`), `vm_var_delete_ops`
  (`:delete`), `vm_var_exists_ops` (`:exists`). t/lazy-array-reify.t.
  **Scope:** only the integer-range case is converted; `(1...*)`/closure-seq/
  gather arrays are still forced to a capped Array on `@`-assign (converting them
  too would regress S32-array/create.t's partial-reify, which the capped Array
  fakes). True memory-laziness (seed `[1]` instead of the 100k prefix, requires
  `.head`/`.first`/`.map`/`.grep` to extend a `sequence_spec` `LazyList`) is the
  L2b follow-up (planned below).

## L2b — true memory-laziness (DONE 2026-06-20, PR #3319)

**Shipped.** `infinite_int_range_to_lazy_array` now seeds `[start]`, so
`my @a = 1..*` is O(1) memory. Read ops extend the sequence on demand:
- `.head(n)`/`.first` → bounded incremental pull (`force_lazy_list_vm_n` /
  `try_lazy_gather_first`);
- `.map`/`.grep` → lazy pipe (`is_lazy_pipe_source` now true for
  `sequence_spec`/`closure_seq`; the VM force gates + the interpreter force block
  in `methods.rs` + the `@a.map` rw fast path in `methods_mut.rs` all exclude
  infinite specs for map/grep/coercions);
- `.elems`/`.Int`/`.Numeric` → soft `X::Cannot::Lazy` `Failure` (unchanged);
- `.sort`/`.reverse`/… → `X::Cannot::Lazy` hard throw.

The gate is expressed by new `LazyList::{is_from_gather,is_infinite_spec,
needs_vm_lazy_dispatch}`. **The catch (handled):** `force_lazy_list_vm` and
`reify_lazy_array_slot` now *extend* a sequence spec to the 100k cap rather than
reading the O(1) seed cache, so a strict force / front mutation still gets a real
prefix. t/lazy-array-reify.t gained 3 fresh-`1..*` map/grep cases; the reify test
runs in 0.06 CPU (was 1.8) — confirming the O(1) read path. **Still deferred to
step 6:** converting `(1...*)`/closure-seq arrays at `@`-assign (gated on
S32-array/create.t partial-reify surviving). Original plan kept below for
reference.

### Original plan (executed above)

**Goal:** seed `infinite_int_range_to_lazy_array` with `[start]` instead of the
100k prefix, so `my @a = 1..*` is O(1) memory. This requires `.head`/`.first`/
`.map`/`.grep` (and any cache-reading mutation reify) to *extend* a
`sequence_spec`/`closure_seq` `LazyList` on demand instead of reading its (now
tiny) cache.

**Feasibility CONFIRMED — all extension machinery already exists; L2b is mostly
broadening `__mutsu_lazylist_from_gather` gates to `is_genuinely_lazy()`:**
- `force_lazy_list_vm_n(list, n)` (`vm_helpers.rs:604`) already extends EVERY lazy
  kind to `n` elements: cache → `sequence_spec` (`extend_sequence_cache`) →
  `closure_seq` (`extend_closure_sequence`) → `lazy_pipe` (`force_lazy_pipe`) →
  coroutine. This is the universal bounded pull. (Proven: `@a[200000]`→200001.)
- `pull_source_element` (`vm_helpers.rs:1028`) already has a `Value::LazyList`
  arm that calls `force_lazy_list_vm_n(ll, idx+1)` — so a `lazy_pipe` whose
  *source* is a `sequence_spec` LazyList pulls correctly.
- `try_lazy_gather_first` (`vm_native_first.rs:97`) already pulls one element at a
  time via `force_lazy_list_vm_n` and tests the matcher — generic over ANY
  LazyList, only the *call site* is gated on `from_gather`.

**The catch (don't miss):** `reify_lazy_array_slot` (`vm_helpers.rs`) currently
calls `force_lazy_list_vm` (returns the CACHE). With seed `[1]` the cache is
`[1]`, so a front-mutation (`@a.shift`/`@a[2]=99`) would reify to `[1]`, losing
the prefix. **Change it to `force_lazy_list_vm_n(ll, MAX_ARRAY_EXPAND)`** so the
reify materializes a 100k prefix (matching today's reify-to-cap). Same for any
other site that read the cache assuming it held the prefix.

**Concrete edits:**
1. `runtime::utils::infinite_int_range_to_lazy_array` — seed `vec![Value::Int(start)]`.
2. `.head`: broaden the force-block gate in BOTH `vm_call_method_ops.rs` (the
   `lazy_list_needs_forcing` block, ~line 619 on main 2026-06-20) and
   `vm_call_method_mut_ops.rs` (~line 430) from the `__mutsu_lazylist_from_gather`
   marker to `ll.is_genuinely_lazy()`. `gather_head_bound`→`Some(n)`→
   `force_lazy_list_vm_n` already does the right thing. The `None =>`
   unbounded-force arm must throw `cannot_lazy` for an infinite list
   (`sequence_spec`/`closure_seq`/`lazy_pipe`) instead of `force_lazy_list_vm`
   (which returns the small cache → wrong for `.sort`/`.reverse`/...).
3. `.first`: broaden the `.first` gate in BOTH `vm_call_method_ops.rs` (~line 608,
   the block guarding `try_lazy_gather_first`) and `vm_call_method_mut_ops.rs`
   (~line 419) from `from_gather` to `is_genuinely_lazy()`. Handler is already
   generic. (Infinite + unsatisfiable predicate loops forever — raku hangs too;
   acceptable.)  [verify the 4 `__mutsu_lazylist_from_gather` sites with
   `grep -n __mutsu_lazylist_from_gather src/vm/vm_call_method*.rs`.]
4. `.map`/`.grep`: `Interpreter::is_lazy_pipe_source` (`methods_collection.rs:53`)
   — add `Value::LazyList(ll) if ll.sequence_spec.is_some() || ll.closure_seq.is_some()`.
   `force_lazy_pipe`+`pull_source_element` then pull from the sequence on demand.
5. `reify_lazy_array_slot` — switch `force_lazy_list_vm` → `force_lazy_list_vm_n(ll,
   MAX_ARRAY_EXPAND)` (see "the catch" above).
6. Once steps 1–5 land for integer ranges, **also convert `(1...*)`/closure-seq
   arrays** in the `@`-assign LazyList arm (re-add the `is_genuinely_lazy()`
   preserve that L2a reverted) — but ONLY after confirming S32-array/create.t's
   partial-reify still passes (the create.t `1,{rand}…*` closure array does
   `@a[^20]` then unshift/shift/`@b[5]=42`; the reify-prefix fix in step 5 should
   make it pass since reify now materializes a real prefix).

**Validation (expect the L2a 6-wave tail to re-surface — re-run ALL of these):**
`make test` + `t/lazy-array-reify.t` + `t/lazy-array-gist.t` +
`t/lazy-array-numeric.t`, then targeted roast S32-array/create.t,
delete-adverb*.t, S09-subscript/{array,slice}.t, S32-list/{flat,join,grep,map,
first,head-tail}.t, S03-sequence/misc.t, S07-iterators/range-iterator.t. The
high-blast-radius rendering/interp paths (`flat_val`, `builtin_join`, nested
`gist_item`) already handle a genuinely-lazy LazyList regardless of cache size,
so they should NOT regress — but `make test` is the net; let CI's full roast be
the comprehensive one (L2a passed CI with no 7th wave).

**No whitelist payoff** (memory optimization). The slurpy-params.t (6 fails) /
slice.t (9 fails) payoff needs a *different* track (Seq single-pass consumption /
`X::Seq::Consumed`), not lazy arrays.

## Open findings / blockers discovered this session

- **L1b — `Value::LazyList` gist still HANGS for the coroutine case.** L1 fixed
  `ArrayKind::Lazy` *arrays*; but `my @a = lazy gather { … }` stores a preserved
  `Value::LazyList` (mutsu reports its `.WHAT` as `Seq`), and `@a.gist` /
  `@a.map(…)` on it forces → **hangs**. A `sequence_spec` LazyList held in a
  scalar already gists `(...)` without forcing, so the gap is specifically the
  *coroutine* LazyList gist path. **Dual-representation wart:** a lazy
  `Value::LazyList` carries no `[`-vs-`(` context, so it cannot know whether to
  render `[...]` (held in `@a`) or `(...)` (a bare `$s` Seq). This must be
  resolved before **L2** (preserving an infinite Range as a reify `LazyList` at
  `@`-assign) is viable — else `my @a = 1..*; @a.gist` would hang.
- **slurpy-params.t is NOT just a lazy problem.** Its remaining 6 fails
  (70-71, 74-77) need real **Seq single-pass consumption** (`X::Seq::Consumed`
  on second iteration) + Seq pass-through (`+a`→Seq, `+@a`→List). mutsu
  materializes every Seq so a Seq is silently re-iterable; implementing true
  single-pass consumption is a broad-blast-radius feature. (Tests 80-81
  X::Parameter::TypedSlurpy DONE #3308; the slurpy `1..*` hang DONE #3303.)

---

## 0. TL;DR

Raku: `my @a = 1..*` keeps `@a` **lazy** — `@a.is-lazy` is `True`, `@a[200000]`
reifies on demand (`200001`), `@a.gist` shows a bounded prefix without hanging,
and mutation (`@a[2]=99`) reifies a *prefix* while keeping the tail lazy.

mutsu today: `my @a = 1..*` is **capped at 100 000 elements** (`coerce_to_array`
→ `ArrayKind::Lazy` Array of 100k reals). So `@a[200000]` → `Nil` (raku
`200001`), `@a.gist` would dump 100k elements, and passing `1..*` to a slurpy
**hangs** (no cap on the slurpy path → unbounded `flatten_into_slurpy`).

The fix substrate is **already present** and proven:
- A **scalar** holding an infinite range reifies on index: `my $s = 1..*;
  $s[200000]` → `200001` (a `Range` index is just `start + n`).
- A **preserved gather `LazyList`** reifies on `@`-index:
  `my @a = lazy gather { … }; @a[200000]` → `200001`, `@a[5]` → `6`.
- The index op (`vm_var_index_ops.rs:387`) already does a **bounded
  incremental pull** (`force_lazy_list_vm_n`) for `coroutine` / `lazy_pipe` /
  `sequence_spec` / `closure_seq` LazyLists.

So the missing piece is: **make infinite ranges / `...` sequences assigned to
`@a` *stay* a reify-on-demand `LazyList` (instead of being capped into an
Array), and teach the array *mutation* path to reify-on-write.**

---

## 1. Representations in play (know these before touching anything)

| value | how produced | lazy? | `@a[N]` reifies? |
|---|---|---|---|
| `Value::Range(a, i64::MAX)` | `1..*` literal | yes (`*` end = `i64::MAX`) | as a **scalar** yes; assigned to `@a` → capped |
| `Value::Array(_, ArrayKind::Lazy)` | `coerce_to_array(Range(a,MAX))` caps at `MAX_ARRAY_EXPAND` = 100k | **fake** (finite 100k, just *flagged* lazy) | only `< 100k` |
| `Value::LazyList { coroutine }` | `lazy gather {…}` | yes (truly) | **yes** (proven) |
| `Value::LazyList { sequence_spec }` | `1,2,3 … *` (`SequenceSpec::Arithmetic`/`Geometric*`) | yes | yes via `force_lazy_list_vm_n` — **but** assigned to `@a` it is forced/capped |
| `Value::LazyList { closure_seq }` | `1,1,*+* … *` | yes | yes |

**The capping points** (all use the constant `100_000`):
- `src/runtime/utils.rs::coerce_to_array` — `Range`/`RangeExcl*`/`GenericRange`
  arms with `b == i64::MAX` build a capped `ArrayKind::Lazy` Array
  (`MAX_ARRAY_EXPAND`).
- `src/vm/vm_var_assign_ops.rs::assignment_rhs_values` — slice-assign RHS
  expansion (`MAX_ASSIGN_SLICE_EXPAND`).
- `src/vm/vm_var_assign_ops.rs` `@`-assign SetLocal path (~6268): a plain
  `LazyList` (no preserve marker, no coroutine) is **forced**
  (`force_lazy_list_vm`); only `coroutine` lists and lists carrying
  `__mutsu_preserve_lazy_on_array_assign` survive.

---

## 2. Why no small slice is safe on its own

`my @a = 1..*` today produces a **real (capped) Array**, so **mutation works**:
`@a.push(99)`, `@a[2]=99`, `@a.shift` all operate on the materialized backing.
A preserved `LazyList` does **not** support these — `@a.push` on a preserved
gather list throws `No such method 'push'`. So **preserving laziness on assign
without also handling mutation is a regression** (this is the
"regresses S32-array create/delete-adverb" warning recorded in BLOCKERS).

And Raku's mutation is **partial-reify**: `my @a = 1..*; @a[2]=99; say @a[^4]`
→ `(1 2 99 4)` — element 3 is *still* pulled from the lazy source. So
reify-on-mutation cannot simply "force to a finite Array"; it must reify the
**touched prefix** and keep the tail lazy (or, as a first approximation, reify
to the old 100k cap on mutation so behavior is no worse than today).

`.gist`/`.Str` on a true `LazyList` currently **forces → hangs** (even for the
working gather case). So lazy stringification is a *separate* missing piece.

---

## 3. Staged slice plan

Each slice must be validated against **`make test` + the named regression
files** below + targeted lazy probes, with a **release** build for any
heavy/perf-sensitive check (#2746 lesson: lazy/array regressions surface only in
the CI release roast timeout, not in `make test`).

Named regression watch-list: `roast/S32-array/create.t`,
`roast/S32-array/delete-adverbs.t`, `roast/S32-array/*`, `roast/S07-iterators/*`,
`roast/S02-types/array.t`, and any whitelisted file using `1..*` / `…` / `1..Inf`.

- **Slice L1 — lazy `.gist`/`.Str`/`.raku` (no materialize).** Isolated to the
  stringification path. A `LazyList` (or `ArrayKind::Lazy`) renders a **bounded
  prefix** then `…` (Raku: `(1 2 3 4 5 6 7 … )`), pulling only ~N elements via
  `force_lazy_list_vm_n`. Safe (does not change array representation or
  mutation). Building block for slurpy-params.t line 291. Does not whitelist
  alone.
- **Slice L2 — infinite Range / `…` sequence stays a reify `LazyList` on `@`
  assignment.** Convert `Range(a, i64::MAX)` (and `RangeExcl*`/`GenericRange`
  infinite forms) to `LazyList::new_sequence([a], SequenceSpec::Arithmetic{step,
  all_int})` **at the `@`-assign site** (NOT in `coerce_to_array`, which is
  called everywhere and must stay Array-returning). Preserve `sequence_spec`
  LazyLists through assignment (extend the ~6268 branch beyond `coroutine`).
  Verify `@a[200000]` / `@a[^5]` / `for @a {…}` / `.is-lazy`.
- **Slice L3 — reify-on-mutation guard.** Before every `@`-array *mutation*
  (`push`/`pop`/`shift`/`unshift`/`splice`/elem-assign `@a[i]=v`/`:delete`/
  `.=` in place), if the slot holds a lazy `LazyList`, reify the needed prefix
  to a real Array first (first approximation: reify to the old 100k cap so
  behavior ≥ today; ideal: reify the touched prefix and keep a lazy tail). This
  is the **~10 chokepoints** the prior session deferred. Enumerate them from
  `vm_data_ops.rs` (push fast path 499/513/595/601), `vm_var_assign_ops.rs`
  (elem-assign / simple-array-op), `vm_var_delete_ops.rs` (delete/exists),
  and the method handlers in `runtime/` for `pop`/`shift`/`unshift`/`splice`.
  **A missed chokepoint = panic/`No such method` on a lazy list.**
- **Slice L4 — slurpy `*@`/`+@` bound a lazy/infinite source stays lazy.**
  `flatten_into_slurpy` (`runtime/types/signature.rs`) currently expands an
  infinite `Range` unboundedly → **hang**. When the slurpy reduces to a single
  lazy source (infinite Range / lazy Seq / `LazyList`), bind the slurpy `@x` to
  that as a reify `LazyList` (depends on L2 representation). Needs L1 for the
  `.gist on @_ … does not hang` test. Unblocks `S06-signature/slurpy-params.t`
  (tests 58/62 etc.) together with L1/L2.
- **Slice L5 — lazy `.elems` / `.Numeric` / `.Bool` semantics.** `.elems` on an
  infinite lazy list should throw `X::Cannot::Lazy` (Raku) rather than return a
  capped count. Coordinate with `S03-operators/eqv.t` (`X::Cannot::Lazy` on
  same-type lazy iterables) and `S09-subscript/slice.t` (`@a[0..*]`).

**Whitelist payoff once the campaign lands:** `S06-signature/slurpy-params.t`,
`S09-subscript/slice.t`, `S03-operators/eqv.t` (lazy cases),
`S04-declarations/constant.t` is already past laziness (needs the unrelated
`G::c` dispatch), plus the slurpy `1..*` hang in several integration files.

---

## 4. Open questions

1. **Partial-reify vs reify-to-cap on mutation (L3).** Raku keeps the tail
   lazy; the cheap first cut reifies to the old 100k cap. Decide per-slice; the
   cap is "no worse than today" and de-risks L3, with true partial-reify as a
   follow-up.
2. **`ArrayKind::Lazy` Array vs `LazyList`.** Two "lazy" representations
   coexist. L2 should standardize infinite sources on `LazyList` (real reify)
   and retire the fake-lazy capped `ArrayKind::Lazy` Array, or clearly delimit
   where each is used.
3. **`.is-lazy` truth source.** Today it is a heuristic; after L2 it should
   reflect whether the backing is a non-coroutine-exhausted reify `LazyList`.
4. **Perf.** Non-lazy `@a = (1,2,3)` must NOT pay any lazy cost — the L2
   conversion fires only when `b == i64::MAX` (or the RHS is already a reify
   `LazyList`). Confirm with a release timed roast.

## 5. References
- `src/value/mod.rs` — `LazyList`, `SequenceSpec`, `new_sequence`.
- `src/vm/vm_var_index_ops.rs:387` — bounded incremental pull on index (works).
- `src/vm/vm_helpers.rs` — `force_lazy_list_vm` / `force_lazy_list_vm_n` /
  `extend_sequence_cache` / `extend_closure_sequence`.
- `src/runtime/utils.rs::coerce_to_array` — the capping point for ranges.
- `src/runtime/types/signature.rs::flatten_into_slurpy` — the slurpy hang.
- `t/` building blocks to add: `t/lazy-array-index.t`, `t/lazy-gist.t`.
