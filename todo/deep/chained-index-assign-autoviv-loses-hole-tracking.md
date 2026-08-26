# Chained `@a[i][j] = v` autoviv loses `ArrayData::initialized` tracking for the new row, so `:exists` on an unwritten sibling slot is wrong

Found while fixing `todo/tickets/multidim-exists-adverb-blind-to-initialized-and-typed-holes.md`
(see `news/2026-08/multidim-exists-adverb-canonical-hole-predicate.md`). That fix included a write-side
correction: `@a[i;j] = v` (the *semicolon* multidim form) autovivifying a fresh row now seeds the new
row with `ArrayData::initialized = Some(empty set)` (`Value::real_array_unassigned`,
`src/value/value_methods_a.rs`) and marks the written index in `src/vm/vm_var_multidim_ops.rs`'s
`multi_dim_assign_scalar`/`multi_dim_assign_slice` and `src/vm/vm_var_multidim_helpers.rs`'s
`assign_array_multidim`.

That fix only covers the `;`-separated `Expr::MultiDimIndex`/`MultiDimIndexAssign` path. The *chained
double-bracket* form -- `@a[i][j] = v`, compiled as a nested `Expr::IndexAssign { target: Expr::Index {
target: ArrayVar("a"), index: i }, index: j, value: v }` -- goes through a different assignment path
(ordinary single-dimension `IndexAssign` handling with an `Index` sub-expression target, somewhere in
`src/vm/vm_var_assign_index_named.rs` / the nested-target autoviv helpers referenced by
`src/compiler/helpers_ast_utils.rs:281`'s `@a[0][1][2]` comment) and was not touched by that fix. It has
the same bug: the freshly-autovivified row is a plain `Value::real_array` (or equivalent) with
`initialized: None` ("bulk-constructed, no gaps"), so a sibling slot that was never written reads back
as `:exists == True` instead of `False`.

## Repro (raku-comparable, not a "raku doesn't support this" case)

```
my @a; @a[0][1] = 5;
say @a[0][0]:exists;   # raku: False   mutsu: True
```

```
raku -e 'my @a; @a[0][1]=5; say @a[0][0]:exists;'   # False
```

Contrast with the now-fixed semicolon form, which agrees with raku after this ticket's fix:

```
my @a; @a[0;1] = 5;
say @a[0;0]:exists;    # raku: False   mutsu: False (fixed)
```

## Why this is a separate finding

The semicolon-multidim fix touched exactly three write sites
(`multi_dim_assign_scalar`/`multi_dim_assign_slice` in `src/vm/vm_var_multidim_ops.rs`,
`assign_array_multidim` in `src/vm/vm_var_multidim_helpers.rs`) plus `ensure_array_size`'s
array-creation branch. The chained-bracket form is compiled and executed through an entirely different
opcode/handler (ordinary nested `IndexAssign`, not `MultiDimIndexAssign`), so none of those call sites
run for `@a[0][1] = 5`. This needs its own investigation of exactly which function autovivifies the
inner `@a[0]` array for a chained index-assign target, and the same `initialized`-seeding-plus-marking
treatment applied there. Left unscoped/unlocated deliberately -- narrowing this down (and deciding
whether there is a single shared autoviv helper worth fixing once, or several call sites like the
multidim family had) is exactly the next step for whoever picks this up.
