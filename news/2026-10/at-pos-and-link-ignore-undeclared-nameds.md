# `AT-POS` and `IO::Path.link` ignore an undeclared named argument

Every Raku method has an implicit `*%_`, so a named argument the method does
not declare cannot change its answer (ADR-0070). Two methods still let one
through (#9905):

- `(1,2,3).AT-POS("a", :zzz)` answered `Nil`, and `[1,2,3].AT-POS(1, :zzz)`
  was read as a two-dimensional index, because the Array `*-POS` code counted
  the `Pair` as one more dimension. It is now stripped first.
- `"/tmp".IO.link(:zzz)` created a hard link named `zzz\tTrue`. The two-path
  `IO::Path` funnel now strips at its entry, and `link` with no name fails with
  Rakudo's `Too few positionals passed; expected 2 arguments but got 1`.

Fixing the first exposed a second difference. The fast 1-argument `AT-POS`
returned `Nil` for any index that was not an integer. It now coerces a `Str`
or `Rat` index, so `(1,2,3).AT-POS("1")` and `(1..5).AT-POS("2")` read an
element as in Rakudo, and a `Str` that is not a number dies.
