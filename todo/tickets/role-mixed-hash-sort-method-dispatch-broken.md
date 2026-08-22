# `.sort` on a role-mixed (`but`) Hash returns the whole hash unsorted, wrapped in a 1-element list

Found by the doc-diff harness (`docs/doc-diff-backlog.md`, `Language/structures.rakudoc:281`).

## Repro

```raku
role Lastable {
  method last() { self.sort.reverse[0] }
}
my %hash-plus := %( 3 => 33, 4 => 44) but Lastable;
say %hash-plus.sort[0]; # OUTPUT: «3 => 33␤»
say %hash-plus.last;    # OUTPUT: «4 => 44␤»
```

- `raku`: `3 => 33` then `4 => 44`.
- `mutsu` (`target/debug/mutsu`): both lines print `{3 => 33, 4 => 44}` (the whole hash,
  gisted, not sorted, not subscripted).

## Isolated minimal repro

`.sort` itself is what's broken specifically (not `.keys`/`.elems`, which both work
fine on the same mixed value):

```raku
role Lastable { }
my %hp := %(3=>33, 4=>44) but Lastable;
say %hp.sort;        # raku: (3 => 33 4 => 44); mutsu: ({3 => 33, 4 => 44})
say %hp.keys.sort;    # works correctly in mutsu: (3 4)
```

`%hp.sort` in mutsu returns a *1-element list containing the whole hash unsorted*
(`({3 => 33, 4 => 44})`) rather than a sorted list of `Pair`s — i.e. `.sort`'s method
dispatch on a `Mixin`-wrapped Hash isn't unwrapping/iterating the underlying hash the
way the plain-Hash `.sort` handler does; a plain `%h.sort` (no mixin) works correctly.

## Root cause hypothesis

This looks like the same "a `but`/`does`-mixed value's role metadata (or, here,
generic dispatch through the `Mixin` wrapper) not surviving a specific method-dispatch
path" shape already tracked by three open tickets:
[list-but-role-loses-positional-binding.md](list-but-role-loses-positional-binding.md),
[hash-default-role-mixin-dropped.md](hash-default-role-mixin-dropped.md),
[role-mixed-value-gist-skipped-in-array.md](role-mixed-value-gist-skipped-in-array.md).
Those tickets note "investigate together and merge into one PR if a single fix site is
found" — this is a fourth concrete symptom of the same family, this time specifically
the `sort` method's slow-path handler apparently not unwrapping a `ValueView::Mixin`
around a Hash before doing its per-element/pair iteration.

## Affected files (starting point)

- The `sort` method handler in `src/runtime/methods.rs` (or wherever `.sort` on a Hash
  is implemented) — needs to unwrap a `Mixin`-wrapped Hash to its underlying Hash
  value before sorting, the same way `.keys`/`.elems` apparently already do.
