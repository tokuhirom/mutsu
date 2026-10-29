# Multi candidates with nested arity ranges are ranked by type

`multi p($a?, $b?)` against `multi p(Int $c)` for `p(3)` was reported as
`X::Multi::Ambiguous`, because the two differ in both minimum and maximum
positional count and that was taken as rakudo's incomparable arity mismatch.
Rakudo only treats a pair as incomparable when the ranges overlap without one
containing the other (`[2..3]` against `[3..4]`); fixed-arity candidates whose
ranges nest are ordered by their parameter types, so the `Int` candidate wins.
The multi-method path hit it for any `self.set($callable)` against
`(&block)` / `($first?, $second?)`, which is Selkie::UI's `SplitBuilder` TWEAK.
