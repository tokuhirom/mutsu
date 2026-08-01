# `:exists:delete` is one operation, in either order

`:exists` and `:delete` combine: the elements are removed and the answer says
whether each one had existed. mutsu accepted only the `:delete:exists` order.
Written the other way round it died:

```raku
my @a = 1, 2, 3;
say (@a[0,1]:exists:delete);   # No such method 'DELETE-KEY' for invocant of type 'List'
say (@a[0]:exists:delete);     # No such method 'DELETE-KEY' for invocant of type 'Bool'
```

The two orders were not two spellings of one thing in the parser. `:delete:exists`
was handled where the leading `:delete` was parsed: it looked ahead for an
`:exists`, and set the `delete` flag on the `Expr::Exists` node it got back. The
reverse order had no such site. By the time the trailing `:delete` was reached
the `:exists` had already been folded into an `Expr::Exists`, and the generic
`:delete` handler wrapped *that* — the answer, a `Bool` or a `List` — in a
`DELETE-KEY` method call.

Both orders now go through one `apply_delete_to_exists`, so the flag is set the
same way whichever came first, and the shape rule pinned by
`t/range-index-exists.t` carries over unchanged: a bare index answers a `Bool`,
every slice form answers one `Bool` per index.

## A zen slice deletes what it names

`@a[]:delete` never reached the `Expr::Exists` path at all: the subscript parser
rewrites a zen slice carrying an adverb into a Whatever index, which is why
`@a[]:delete:exists` worked. It does *not* do that rewrite for `:exists`, so
`@a[]:exists:delete` arrived at the compiler as a `ZenSlice`, and the compiler's
delete half only knew how to handle an `Index` — it silently emitted no delete
at all. A zen slice names every element, exactly like the whatever slice, so the
compiler now synthesises the same Whatever index for it.

## A multi-dimensional subscript is a different candidate set

`@a[0;1;2]` does not share `postcircumfix:<[ ]>`'s adverb candidates. Under
6.c/6.d no combination of `:exists` and `:delete` resolves there at all; under
6.e the positive one does, and only `:!exists:delete` is rejected — as an
`X::Adverb` naming the pair:

```
Unsupported combination of adverbs ('!exists', 'delete') passed to
slice on '@a'.
```

mutsu was passing the roast checks for this by accident: the `DELETE-KEY`
wrapper described above died, which is what `dies-ok` wanted, but with an
unrelated message and for the wrong reason — so making the combination *work*
made those checks fail. `apply_delete_to_exists` therefore treats a
`MultiDimIndex` target as its own case: the negated form builds the `X::Adverb`,
and the positive form lowers to `__mutsu_multidim_exists_adverb_dyn` with a
constant-true delete flag — the same call the dynamic `:$delete` already
produced, which is why `@a[0;0;0]:exists:$delete` had always worked while the
literal spelling did not. mutsu does not version-gate this; it follows 6.e.

Pinned by `t/exists-delete-adverb.t` (48 assertions, both orders throughout).
Found while fixing the Range-index `:exists` slice
([news](range-index-exists-is-a-slice.md)).
