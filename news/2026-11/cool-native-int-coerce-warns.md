# Native integer coercions of an undefined `Cool` warn

`Cool.int8` and its ten siblings (`int`, `int16` .. `int64`, `uint` .. `uint64`, `byte`) returned zero
without Rakudo's resumable "Use of uninitialized value of type Cool in numeric context" warning. The
`Cool` rows are now interpreter-aware: a type object other than an `Int` raises the warning through
the usual resumable-warning boundary, so a `CONTROL { when CX::Warn { .resume } }` sees it, and the
conversion then runs on the resumed zero. Concrete values and the `Int` type-object refusal are
unchanged (#15892).
