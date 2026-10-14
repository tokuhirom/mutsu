# `words($limit)`, `lines($limit)` and `chop($n)` are method rows

`Str` and `Cool` now declare `words($limit)`, `lines($limit, :chomp)`, `lines(:chomp, :count)`
and `chop($n)` as rows of the built-in method table, replacing three name-matched arms of the
one-argument dispatch cascade. The count is read the way `.Int` reads it, so `"a b c".words(1.5)`,
`.words("2")` and `.words(<2>)` answer instead of failing to find the method, and a `Pair`, which is
not a `Cool`, no longer answers them (ADR-11276 §9.102).
