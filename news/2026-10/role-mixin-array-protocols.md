# A role mixed into a native array keeps its own element protocols

Making the `Array::Sparse` distribution's test file pass (23 of 23 subtests, 21 of 23 before the last
two fixes) exposed five gaps in how a role mixed into a native container (`my @a is R`, or a punned
`R.new`) behaves:

- A closure a role method made and that runs after the method returned (a lazy `.map`) read the
  construction seed of a private attribute instead of the live role cell.
- The `:v`/`:k`/`:p` slice adverbs on a blessed object or role mixin now read through the object's own
  `AT-POS`/`EXISTS-POS`, not only for objects that define `keys`.
- `.head`, `.tail` and `.first` on a mixin whose role supplies `iterator` iterate that iterator instead
  of treating the mixin as one item.
- `@a[i] := v` calls the role's `BIND-POS`/`BIND-KEY` rather than replacing `@a` with a plain Array.
- `eqv` on two role mixins with a user `.raku` compares the renderings of the mixins themselves, as
  Rakudo's `.WHAT` + `.raku` rule does.

Pinned by `t/oo/role/role-mixin-array-protocols.t`.
