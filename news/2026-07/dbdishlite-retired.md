# Retire the homegrown DBDishLite SQLite layer

`t/lib/DBDishLite.rakumod` was a minimal, homegrown SQLite layer written on top of NativeCall back
when off-the-shelf `DBDish::SQLite` was believed to be blocked by `MoarVM::Guts::REPRs`. It existed
purely so the web-blog battery stack had *some* database story, and it lived in the test-helper
location (`t/lib`) with a single guarantee test, `t/dbdish-lite.t`.

That premise no longer holds. The upstream `DBIish` distribution is bundled as a battery (#5556) and
passes all nine of its test files at raku parity, driving real SQLite through the genuine
`DBDish::SQLite` driver. Keeping a parallel homegrown layer meant two implementations of the same
capability, which the "1 operation = 1 implementation" rule exists to prevent — and the homegrown one
was strictly the weaker of the two.

So `t/lib/DBDishLite.rakumod` and `t/dbdish-lite.t` are deleted, and PLAN.md's battery list and
`t/lib` promotion task now name `DBIish` instead. Only Tubu remains to be promoted out of `t/lib`.
