# RakuAST: multi, private, rw and raw methods cross the boundary

`.AST` refused every method declared `multi`, private (`method !p`) or with
`is rw` / `is raw` as a "method with traits / private / multi / delegation",
one of the most frequent refusals under `MUTSU_RAKUAST=1` -- a single
`multi method` made a whole class un-round-trippable.

Measured on rakudo 2026.09, `multi method !p() is rw { … }` is one
`RakuAST::Method` with `multiness => "multi"` and `private => True` ahead of
its `name`, and `traits => (Trait::Is(name => Name.from-identifier("rw")),)`
before its `body`. The new `rakuast::routine_traits` renders those fields, and
lowering reads them back into `Stmt::MethodDecl`. The `multiness` reader is now
shared with subs. A `Trait::Is` on a sub is still refused rather than dropped.

The parser keeps `is rw`, `is raw` and the `returns`/`of` trait as separate
flags, not in source order, so a method carrying more than one of them stays
refused, as do `our`/`my` methods, `handles` and user traits.

The round-trip ratchet grows from 1194 to 1226 of 5778 `t/` files. Pinned by
`t/rakuast/rakuast-method-flags.t`, which passes under both mutsu
and raku.
