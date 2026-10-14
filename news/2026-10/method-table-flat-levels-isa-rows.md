# `flat($levels)`, `Int.Str(:superscript)`, `Mu.isa` and `FatRat.base` are method-table rows

Five more one-argument arms of the native method cascade are gone (ADR-11276 §9.104), each replaced by a row of the method
table on the owner Rakudo declares it on:

- `flat($levels, :hammer)` is `Iterable`'s: a negative level, `Inf` or `*` flattens every level, a `Str` level numifies, and a
  `Hash` flattens as its pairs (`{a => 1}.flat(:hammer)` is `(:a(1),)`). `5.flat(1)` and `(1..3).flat(1)` now die, as in
  Rakudo, where `Any` and `Range` declare only `flat()`.
- `Int.Str(:superscript)` and `Int.Str(:subscript)` raise or lower the digits of any `Int`, big ones included, and every other
  `Str` candidate ignores a named argument it does not know (`1e3.Str(:superscript)` is `"1000"`).
- `Mu.isa` answers every built-in receiver from one row.
- `FatRat.base($radix)` and `FatRat.base($radix, $digits)` work (Rakudo declares `base` on `Rational`).
- The `.zprintf` method form is gone: Rakudo has no such method. The `zprintf` routine stays.
