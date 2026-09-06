# A chained `@a[i][j] = v` now tracks the holes it leaves behind

```raku
my @a; @a[0][1] = 5;
say @a[0][0]:exists;   # raku: False   mutsu was: True
```

Writing one element of a freshly autovivified row made every *other* slot of
that row — and every unwritten slot of the outer array — claim to exist. The
`;`-separated multidim spelling had already been fixed
(`news/2026-08/multidim-exists-adverb-canonical-hole-predicate.md`); the
chained-bracket spelling had not, because it is compiled to entirely different
opcodes and never ran any of those sites.

## What was wrong

`ArrayData::initialized` is the canonical hole predicate (ADR-0049 §1.6). `None`
means "bulk-constructed, no gaps"; `Some(set)` means "only these indices were
written". The multidim fix seeds a fresh row with an empty set and marks each
written index.

`@a[i][j] = v` compiles to `IndexAssignExprNested`, and `@a[i][j][k] = v` to
`IndexAssignDeepNested` — neither of which touches `MultiDimIndexAssign`'s
handlers. Every array those two paths autovivified was born with
`initialized: None`, and every `autoviv_resize` they performed grew an array
without recording which slot the growth was *for*. The result was the same bug
the multidim path used to have, on the spelling people actually write.

## The fix

Two things, both small once located:

- `fresh_autoviv_container` and the two inline autoviv sites now build the row
  with `Value::real_array_unassigned` rather than `Value::real_array`, so a
  brand-new row tracks its gaps from birth.
- A new `autoviv_resize_tracking` replaces the bare `autoviv_resize` at the five
  sites on these two paths. It grows the array, and — this is the part that is
  easy to get wrong — **marks the pre-existing prefix as initialized at the
  moment it grows an untracked array**, because the freshly appended tail is
  gaps and the prefix would otherwise be swallowed by the same set. A write that
  does *not* grow the array deliberately leaves an untracked array untracked:
  seeding a set there would declare every other slot a hole.

The ticket asked whether there was "a single shared autoviv helper worth fixing
once, or several call sites like the multidim family had". It is both: one new
helper, applied at five sites across the two-level and the deep-nested walk,
plus the two row-construction points.

## Coverage

`t/chained-index-autoviv-hole-tracking.t` — 14 rows, every expectation measured
against `raku` v2026.07 first. Beyond the headline it pins the outer level, the
three-level deep-nested walk, an array row vivified under a hash key, and the
five shapes that must *not* change: a bulk-constructed array still reports no
gaps, filling a hole in afterwards makes it exist (at both levels), a `.push`
onto a vivified row appends a real element, `:delete` turns a slot back into a
hole, and the stored structure itself is untouched.

## Noticed while measuring, not fixed here

`my @a = 1,2,3; @a[1][0] = 9` dies in raku with `Cannot modify an immutable Int
(2)` — subscripting an existing non-container element is an error, not an
autovivification — and mutsu performs the write. That is an immutable-lvalue row
rather than a hole-tracking one; it belongs with
`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md`.
