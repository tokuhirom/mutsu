# The last divergent hole predicate in the multidim adverb family is folded onto `ArrayData::hole_at`

ADR-0049 (`docs/adr/0049-nil-decays-to-the-container-default-at-the-element-store.md`) §1.6 named
"three divergent hole predicates" living alongside the canonical `ArrayData::hole_at`. Two were folded
onto it during that ADR's slice 5; the third -- `multidim_exists_adverb_multi`'s open-coded
`!value.is_nil() && !matches!(Package("Any"))` check in
`src/runtime/builtins_multidim_ops.rs` -- was deliberately deferred, because folding it needed
`(ArrayData, index)` context its shared `multidim_collect_leaves` leaf-collector did not carry (only
the extracted leaf value). That deferral was tracked as
`todo/tickets/multidim-exists-adverb-blind-to-initialized-and-typed-holes.md`. This is now done.

## What was actually wrong

The old predicate had two gaps: it only recognized the literal `"Any"` package name, not a typed
array's own element-type gap marker (an unwritten `Int` slot in a typed array read as "exists"), and it
never consulted `ArrayData::initialized`, so an explicitly-assigned `Any`/type-object value was
indistinguishable from a genuine gap. Both are only reachable through a genuine `;`-separated
`Expr::MultiDimIndex` combined with a `Whatever`/list index on an Array level -- real Rakudo itself
throws `X::NYI` ("`:exists`/`:kv`/`:p` on multi-dimensional slices not yet implemented") for that exact
combination, so there is no raku oracle for the compound query. The bug was verified instead by
comparing mutsu's own multidim-adverb answer against mutsu's own single-coordinate `:exists` for the
identical semantic question -- the latter is raku-verified correct and was already going through
`hole_at` via a completely different code path (`src/vm/vm_var_exists_ops.rs`). Two concrete,
self-inconsistent examples: `my Int @a[2;2]; @a[0;0]=1; @a[0;1]:exists` answered `True` where the
single-coordinate form (via `@a[0]:exists`-style reasoning) required `False`; `my @a[2;2]; @a[0;1] =
Any; @a[0;1]:exists` answered `False` where it required `True`.

Digging further turned up two *more* single-coordinate (`!has_multi_indices`) predicates in the same
file with the identical bug, and this time with a direct `raku`-comparable repro (no `Whatever`
involved at all): `builtin_multidim_subscript_adverb`'s and `builtin_multidim_exists_adverb`'s
non-multi fallback branches computed `exists` from a bare `!value.is_nil()` (the latter also had a
narrower, shaped-array-only `is_any_type_object()` special case). `my Int @a[2;2]; @a[0;0]=1; say
@a[0;1]:exists;` gave `True` in mutsu against `False` in raku; `my @a[2;2]; @a[0;0]=1; @a[0;1]=Any; say
@a[0;1]:exists;` gave `False` against raku's `True`.

## The fix

`multidim_collect_leaves`'s output grew a third tuple element, `is_hole: bool` -- the `hole_at`
verdict for each leaf in its *immediate* parent array (always `false` when the parent is a Hash, since
a missing hash key is already precisely represented by the leaf value being `Value::NIL`). It is
computed at each `Array`-level `Whatever` iteration and at each single-index navigation step (a new
`multidim_index_step` helper mirroring `multidim_index`'s own single-index tail, plus `hole_at`), and
threaded through the recursion including the `ContainerRef`/`Scalar` deref arm. All six original call
sites across `builtins_multidim_ops.rs` were updated to destructure the new triple and use
`!value.is_nil() && !is_hole` in place of every open-coded variant. A companion `multidim_index_with_hole`
function (the single-value counterpart, for the `!has_multi_indices` fallback branches) closed the two
further predicates found along the way -- so there is no second definition of "hole" left in that
file.

Making the read side precise surfaced a write-side gap it depended on: multidim element assignment
(`@a[i;j] = v`) never recorded the write in `ArrayData::initialized` at all, for either the shaped
(`assign_array_multidim`, `src/vm/vm_var_multidim_helpers.rs`) or the autoviv/non-shaped
(`multi_dim_assign_scalar`/`multi_dim_assign_slice`, `src/vm/vm_var_multidim_ops.rs`) path -- so even a
perfectly precise `hole_at` call had no accurate data to consult for an explicitly-assigned `Any` at a
multidim coordinate. Fixed alongside: a fresh autovivified row now starts as
`Value::real_array_unassigned` (an `ArrayData` with an empty `initialized` set, the non-shaped
counterpart to the existing `Value::shaped_array_unassigned`), `ensure_array_size` materializes
`initialized` for an existing array's pre-growth prefix when it grows one that never tracked gaps, and
each multidim leaf write marks its own index. `multidim_delete` had the matching gap on removal (it
never marked `initialized`, relying entirely on the old unconditional `Package("Any")`-is-a-hole
reading) -- fixed the same way, materializing `initialized` before removing (Whatever-deleted-everything
clears the whole set) so a `:delete`d multidim slot is recognized as a hole afterward.

## What stayed out of scope

Two further findings surfaced during verification, neither reachable from the ticket's own scope, and
both filed as new `todo/deep/` tickets rather than folded into this fix:

- A **genuinely 2+-dimensional typed shaped array** (`my Int @a[2;2]`) still fails to recognize its own
  typed gap marker below the outer dimension: each row is a plain `Array`, not carrying the declared
  element `value_type`, because `make_shaped_array_seeded`'s recursive row construction never sets it.
  `@a[0].WHAT` is `(Array)` where it should track `Int`. There is no raku oracle here either (real
  Rakudo throws "Partially dimensioned views of shaped arrays not yet implemented" for `@a[0]` on a
  shaped array) -- filed as
  `todo/deep/typed-shaped-array-rows-lose-element-value-type.md`.
- The **chained double-bracket** form, `@a[i][j] = v` (compiled as a nested `IndexAssign`, not
  `MultiDimIndexAssign`), autovivifies its row through an entirely different code path that this fix
  did not touch, and has the same `initialized`-tracking gap: `my @a; @a[0][1]=5; say @a[0][0]:exists;`
  gives `True` in mutsu against raku's `False` -- this one IS raku-comparable. Filed as
  `todo/deep/chained-index-assign-autoviv-loses-hole-tracking.md`.
- A pre-existing, unrelated bug was also noticed and filed: `@a[i;j]:v` (and `:k`/`:p`) on a multidim
  hole returns `Value::NIL` (`say` prints `Nil`) instead of an empty list `()`, reproducing identically
  before and after this fix on a plain untyped shaped array with no typed/initialized subtlety at all.
  Filed as `todo/tickets/multidim-value-adverb-hole-returns-nil-not-empty-list.md`.

## Verification

A new regression file, `t/multidim-exists-canonical-hole-predicate.t` (28 assertions), pins both the
raku-comparable plain-coordinate cases (shaped and autoviv, `:exists`/`:kv`/`:delete`, both the static
and dynamic `:$delete` adverb forms, and a Hash-multidim control) and the mutsu-only
`Whatever`/list-index cases, cross-checked for self-consistency against the plain-coordinate
assertions in the same file. Manually run against real `raku` (with `use v6;`, not `v6.e.PREVIEW` --
the preview language version routes shaped-array `:kv` through a different, more-`X::NYI` internal
path even for plain coordinates) confirms every raku-comparable assertion (tests 1-12) passes
identically; the `Whatever`/list-index assertions (13+) correctly hit raku's own `X::NYI` for
that combination, matching the file's documented expectation.

`make test` (full local `t/` suite, 3478+ files) passes with no regressions, `cargo clippy -- -D
warnings` and `cargo fmt` are clean, and a targeted roast sweep of every whitelisted `S09-*`,
`S32-array/*`, and `S32-hash/*` file (59 files, on the debug binary) passes.
