# Anonymous subsets, `CompUnit.sorries`, attribute types for `static`'s analysis

A third batch for `static` 0.0.1 (#15610), measured against rakudo 2026.09:

- the refined type of `my Int $x where * > 0` is named `<anon>` (in `.^name`,
  `.raku` and the type-check message) instead of its internal name;
- `CompUnit.sorries`, `worries` and `has-check-time-problems` list what
  `add-sorry` / `add-worry` queued so far;
- `Var::Attribute.return-type` is the declared type of the attribute it names,
  read from the enclosing class's `has` declaration;
- `Block` and `PointyBlock` answer their closure as `compile-time-value`.

`static` 0.0.1 against rakudo: `t/01`, `t/07`, `t/13`, `t/14` now match; the
remaining files still differ (the largest being `t/08` and `t/09`).
