# Nil no longer matches aggregate-sigil multi candidates

An untyped `@` or `%` parameter carries an implicit `Positional` or
`Associative` constraint. mutsu's multi-dispatch matcher incorrectly treated
`Nil` as satisfying either constraint, so `defined($value)` could select
P5defined's aggregate candidate after `undef($value)` and die while binding it.

The matcher now rejects `Nil` for both aggregate sigils, allowing the ordinary
sigilless item candidate to handle it, matching Rakudo.

Pinned by `t/routines/dispatch/multi-aggregate-rejects-nil.t`.
P5defined 0.0.8 moves from red (0/1 baseline files, 8/18 assertions) to green
(1/1, 18/18).
