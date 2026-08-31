# Multi-dim `:=` binds defer every level, not just the leaf

`todo/tickets/bound-array-slice-still-vivifies-eagerly.md` listed three call
sites that grew an array at *bind* time so that the promoted element would not
have to be a deferred `HashEntryRef` token. Two of them (the bound-slice arm of
`exec_index_autovivify_lazy_op` and the slice-dimension descent in
`collect_multi_dim_leaf_cells`) were fixed when the bound-slice work landed. The
remaining one — the all-scalar multi-dim descent
(`multi_dim_scalar_autoviv_cell`) — is fixed here, and fixing it exposed two
further gaps in the same subscript.

## What was wrong

Three separate defects, all on `@a[i;j;k]`:

1. **The intermediate levels were created eagerly.** `multi_dim_scalar_autoviv_cell`
   called `ensure_array_child` for every non-terminal dimension, so
   `my @a; my $x := @a[0;0;3]` left `@a` as `[[[],],]` where rakudo leaves it
   `[]`.

2. **The write through the bind was swallowed.** The terminal element had
   already been made lazy, but the caller re-wrapped the deferred token in a
   *fresh* `ContainerRef` (`cell.into_container_ref()`) — a cell with no link
   back to the array. `$x = 5` wrote into that detached cell and `@a` stayed
   empty. Rakudo gives `[[[Any, Any, Any, 5],],]`.

3. **A multi-dim RHS never took the bind route at all.** The parser wrapped
   `my $x := @a[5]` in the `MarkBind` synthetic block that makes the compiler
   emit an aliasing subscript, but only for `Expr::Index`; an
   `Expr::MultiDimIndex` RHS fell through to a plain read, so even a correct
   token would not have reached the binding.

## The fix

- `multi_dim_scalar_autoviv_cell` now checks each intermediate index with a new
  `array_index_is_hole` and, on a hole, returns a single `HashEntryRef` token
  carrying *the whole remaining path* (`deferred_multi_dim_token`) instead of
  walking and creating the levels. The existing deferred-write machinery
  walk-creates every level on the first write, so a bind that is never written
  leaves the source untouched.
- `exec_multi_dim_index_bind_ref` pushes the token as-is; the `into_container_ref`
  wrap is gone.
- `bind_to_index` in the `my ... := ...` parser accepts `Expr::MultiDimIndex`
  alongside `Expr::Index`, so the multi-dim form takes the same `MarkBind` route
  and the compiler emits `MultiDimIndexBindRef`.
- `Value::eqv` grew a `HashEntryRef` arm next to its existing `Scalar` and
  `ContainerRef` arms. A deferred token is a container wrapper, not a value, and
  `compile_call_arg` emits `MultiDimIndexBindRef` for *every* multi-dim call
  argument — so a native callee such as `is-deeply` receives the raw token and
  has no signature binding to decontainerize it. Without the arm,
  `is-deeply @array[0;0;3], Any` compared the token itself and failed
  (`roast/S32-array/multislice-6e.t` tests 241/271/301).

## Coverage

`t/array-slot-ref-deferred-vivification.t` gains eight rows for the multi-dim
form (no growth at bind, hole value on read, walk-creation on the first write,
a second write, and the same over an existing structure). The whole file — all
36 assertions — passes under `raku` as well as mutsu.
`roast/S32-array/multislice-6e.t` and `roast/S32-hash/multislice-6e.t` are the
regression oracles and stay green.
