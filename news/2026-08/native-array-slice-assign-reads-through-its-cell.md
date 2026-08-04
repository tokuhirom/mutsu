# A native array's slice assignment reads through its container cell

```raku
sub takes-scalar(Mu $x) { ?$x }
my @a := array[num].new;
@a[0] = 1e0;
takes-scalar(@a);
@a[^3] = 5e0, 6e0, 7e0;   # Type check failed for an element of @a;
                          # expected num but got List
```

Passing an array to a *scalar* parameter boxes the caller's binding into a
`ContainerRef` cell so a write inside the callee can reach it. From then on
`self.env().get("@a")` answers the cell, not the array — and everything
user-visible derefs through it transparently, so `@a.WHAT`, `@a.of`, `@a.raku`
and even single-element assignment all kept working and the array still looked
native from Raku.

What did not deref was the test that decides whether a Range subscript is a
slice. `exec_index_assign_expr_named_op_inner` gates its "expand a numeric Range
into an explicit index list" step on
`matches!(index_target.view(), Some(ValueView::Array(..)))`, which the cell does
not match. With the expansion skipped, `^3` fell through to the ordinary
single-key path and was stringified into the key `"0 1 2"`, so the whole
right-hand list was type-checked against the scalar element type.

The fix is one `Value::deref_container` before the match, applied to both the
view test and the container-metadata lookup beside it.

This is the third time a raw `ValueView` read of an env binding has been wrong
in this spot's neighbourhood (see
`news/2026-08/shared-array-mutation-through-a-container-cell.md`): **a binding
that a call could have boxed must be dereferenced before its variant is
inspected**, because the boxed form silently takes the `_ =>` branch instead of
failing loudly.

Found under the real `Test` module — `nok @arr, "…"` passes the array to `ok`'s
`Mu $cond` — which is why `roast/S09-typed-arrays/native-num.t` ran 31 of its
518 assertions there while passing under the native provider, whose `nok` never
takes the array as a Raku argument. It is 518/518 now.

Pin: `t/native-array-slice-assign-after-scalar-arg.t` (all eight assertions
verified against `raku`).
