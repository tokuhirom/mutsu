# `use`, `constant` and `use Foo:if(...)` run at BEGIN time

This is slice 3 of [ADR-0134](../../docs/adr/0134-begin-time-prologue.md),
and it closes #9919. Every top-level `use` and `constant` now runs in the
unit's BEGIN prologue, in source order with the unit's BEGINs, before any
run-time statement.

- A module's mainline runs before statements that precede the `use`.
  `say 'run'; use Foo;` prints Foo's output first, as rakudo does.
- A constant sees lexicals in their static state: `my $x = 5; constant K =
  $x;` gives `Any`.
- Under the `if` pragma, `use Foo:if(EXPR)` evaluates `EXPR` at BEGIN time.
  A condition that only a run-time assignment would define
  (`my $c = True; use Foo:if($c)`) is undefined there. The program then
  stops with rakudo's `Did not provide compile-time-value for :if adverb in
  use statement`, instead of loading `Foo` on the strength of the run-time
  value. A condition a BEGIN declared or stored is honoured.
