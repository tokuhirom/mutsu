# #8507's self-referential `ContainerRef` cell root cause was already fixed by PR #8580

[#8507](https://github.com/tokuhirom/mutsu/issues/8507) had two separable halves. The
crash-safety half (`value_is_defined` deadlocking on a cyclic `ContainerRef` cell) was
fixed in #8520 and is recorded in
[`value-is-defined-cyclic-container-ref-deadlock.md`](value-is-defined-cyclic-container-ref-deadlock.md).
That entry left the root cause — how mutsu's multi-dim slice-swap assignment machinery
could construct a self-referential cell in the first place — open, with the note "this
half of #8507 remains unresolved."

It turned out to already be fixed, by a PR that did not know about #8507 at all. A
second, independent investigation the same day ([#8552](https://github.com/tokuhirom/mutsu/issues/8552))
chased the same `Game::Entities` 0.1.6 `t/sorting.t` hang from a different angle (why does
`.sort: A, B;` never terminate?) and isolated it to exactly the mechanism #8507 had
hypothesized but could not reduce to a minimal repro: a plain (non-`:=`) multi-dimensional
index READ's terminal case (`multi_dim_index_read` in `src/vm/vm_var_multidim_ops.rs`)
returned a `ContainerRef` leaf as-is instead of dereferencing it. A subsequent
swap-via-slice-assignment on the same two leaves
(`@arr[DIM; $i, $j] = @arr[DIM; $j, $i];`, or the chained SPARSE/DENSE form
`Game::Entities`'s own `&swap` helper uses) then wrote each leaked cell *reference* into
the other cell (since assignment through an existing cell writes *through* it),
cross-wiring the two cells into a self-referential/cyclic pair instead of swapping their
contents.

PR [#8580](https://github.com/tokuhirom/mutsu/pull/8580) (commit `dafe38dd`, merged
2026-09-16, closing #8552) fixed it at the root: `multi_dim_index_read`'s terminal case now
dereferences a `ContainerRef` leaf via `Value::into_deref` before returning it, so a plain
read never leaks a cell reference into rvalue position regardless of why the leaf happened
to be cell-promoted. It also stopped the parser's synthetic `__mutsu_list_assign_rhs`
helper (the wrapper a plain `my (...) = EXPR` list-assignment's RHS gets) from hitting the
raw `\target` / `is rw` bind-ref special case, since a plain list-assign read should never
have promoted the source array's leaves to cells at all. The regression test it added,
`t/routines/dispatch/multidim-list-assign-no-alias.t`, pins the exact chained
SPARSE/DENSE two-swap shape #8507's own "Hypothesis" section described
(`$set[SPARSE; $le, $re]` feeding indices for a `$set[DENSE; ...]` swap, then vice versa).

This investigation independently re-derived the same mechanism working from #8507's side
(confirmed the single-dimension slice-read path, `resolve_array_entry` in
`src/vm/vm_var_ops.rs`, already had the equivalent `deref_container()` chokepoint — so the
gap was specific to the multi-dim terminal case, not a broader class of bugs), verified
the fix against the exact repro pattern from #8507 plus a same-index degenerate case
(`$set[dim; $i, $i] = $set[dim; $i, $i]`), and confirmed
`t/routines/dispatch/multidim-list-assign-no-alias.t` passes. #8507 is now closed as
already resolved by #8580; both of its halves (crash-safety and root cause) are on `main`.
