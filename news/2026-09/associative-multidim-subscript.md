# An associative multi-dim subscript walks nested Hash keys

`%h{1;2} = 5` used to die with `Invalid index for multi-dim assignment`, even
though the positional spelling (`my @sh[2;2]; @sh[0;0] = 7`) had always worked.
The multi-dim assignment path treated *every* `{a;b}` subscript as a store into
a dimensioned (shaped) container, so an integer key over a Hash hit the "not an
array index, not a string key" arm and errored out.

An Associative has no shape. Its semicolon subscript is a chain of nested keys:
each level stringifies its key and autovivifies a **Hash**, where the positional
spelling autovivifies an Array from an integer-looking index. Nothing at run
time can tell the two apart from the target alone — `%h{1;2}` and `@a[1;2]`
both arrive as "a container plus two Int dimensions" — so the bracket kind is
now carried from the parser, exactly as the single-subscript `Expr::IndexAssign`
has always carried its `is_positional`:

- `Expr::MultiDimIndex` / `Expr::MultiDimIndexAssign` gained an `is_positional`
  field, set at the two parse sites that build them (`[...]` vs `{...}`) and
  threaded through every lvalue rewrite.
- `OpCode::MultiDimIndex`, `MultiDimIndexAssign` and `MultiDimIndexAssignGeneric`
  carry it into the VM, where `multi_dim_assign_{scalar,slice}` take the
  associative branch: `ensure_hash` the level, `hash_key_encode` the key, and
  autovivify each *intermediate* level as an itemized Hash.

## The multislice is a 6.d/6.e split

Making the assignment work exposed the read side, which is version-dependent:

```raku
my %h; %h{1;2} = 5;
say %h{1;2}.raku;     # 6.d: (5,)   6.e: 5
say %h{1;2} + 3;      # 6.d: 4      6.e: 8
```

Under 6.d and earlier an associative multi-dim subscript is a *multislice*: the
lvalue and the rvalue are a `List` with one element per selected leaf, even when
every dimension is a single key. That makes the assignment a **list**
assignment, so `%h{1;2} = [1,2,3]` stores `1` at the leaf where the positional
`@a[0;1] = [1,2,3]` stores the whole array. 6.e dropped the wrapper and gave the
all-scalar-keys form plain single-element semantics.

mutsu had been unconditionally 6.e-shaped here — which is why
`roast/S32-hash/multislice-6e.t` (549 tests, `use v6.e.PREVIEW`) passed while
the default-6.d spelling did not. Both sides are now correct: the VM consults
`current_language_version()` for the multislice, and keeps the nested-Hash walk
and the leaf itemization (`%h{1;2} = [1,2,3]` renders `${"2" => $[1, 2, 3]}`,
matching the chained `%h{1}{2}` spelling) version-independent.

`t/multidim-hash-read.t` had pinned the old 6.e-only read shape as if it were
the default; running it under `raku` shows the same four failures, so its
expectations were updated rather than the behaviour. Pins:
`t/multidim-associative-subscript-assign.t` (6.d),
`t/multidim-associative-subscript-6e.t` (6.e).

## Still open

An *expression* target still drops the write (`%o<inner>{1;2} = 5` leaves `%o`
empty) — `MultiDimIndexAssignGeneric` mutates a throwaway copy of the popped
target. That gap predates this work and loses the positional spelling too; it
is tracked in `todo/tickets/multidim-assign-to-an-expression-target-is-dropped.md`.
