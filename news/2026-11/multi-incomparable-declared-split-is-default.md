# A multi split by declared types is ambiguous, and `is default` settles it

`multi method f(Any:D $a, Real:D $b)` and `multi method f(@a, Mu $b)` are each narrower on a different
declared type (`Positional` is narrower than `Any`, `Real:D` than `Mu`), so rakudo calls them
incomparable and `f((1,2,4), 64)` is `X::Multi::Ambiguous` unless one carries `is default`. mutsu
ranked the first one narrower from its typed-parameter count and always picked it. The ranking now
treats a declared-type split as incomparable, `prune_to_maximal` lets exactly one `is default`
survivor win, and a bind-check-free split with none is reported as ambiguous. `PredictiveIterator`
also composes `Iterator` now, as in rakudo (`class C does PredictiveIterator` is an `Iterator`).
