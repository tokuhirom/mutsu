# `use Foo:if(...)` evaluates its condition at BEGIN time

This is slice 3 of [ADR-0134](../../docs/adr/0134-begin-time-prologue.md),
and it closes #9919. Under the `if` pragma, the condition of
`use Foo:if(EXPR)` is now evaluated in the unit's BEGIN prologue.

A condition that only a run-time assignment would define, such as
`my $c = True; use Foo:if($c)`, is undefined at BEGIN time. The program now
stops with rakudo's `Did not provide compile-time-value for :if adverb in use
statement`. Before this change, mutsu loaded `Foo` because it read the
run-time value. A condition that a BEGIN declared or stored is honoured.

The wider step, making every `use` and `constant` a BEGIN-time effect, is
tracked in #10336.
