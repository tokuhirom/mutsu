# `Any.AT-POS` / `ASSIGN-POS` / `DELETE-POS` on an undefined scalar

`my $a; $a.AT-POS(0) = 42` died with `cannot assign through .AT-POS on
non-instance`, `$a.ASSIGN-POS(0, 42)` was `No such method`, and
`$a.DELETE-POS(0)` likewise. Rakudo vivifies an `Array` into the variable for
the first two (`$[42]`, the same as `$a[0] = 42`) and answers `Nil` for the
third. The lvalue `AT-POS` path now vivifies like the existing `AT-KEY` one;
`Any.ASSIGN-POS` is a method-table row that vivifies through the same `grow`
helper as `Any.push` and stores through `Array`'s row; `Any.DELETE-POS` is a
row answering `Nil`. Pinned by `t/types/any-at-pos-assign-pos-autovivify.t`;
`roast/S32-basics/xxPOS.t` now gets to its `substr-rw` block (filed
separately).
