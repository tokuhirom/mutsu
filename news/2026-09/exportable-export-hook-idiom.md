# The `Exportable` export-hook idiom works, and Color::DirColors loads

`Color::DirColors` (drawn at random from the ecosystem ledger) died at load
with `Unknown name for export: 'stat'`. Its dependency `File::Stat` uses
`Exportable`, whose `sub EXPORT` declares `is export` trait handlers that close
over a hash, re-dispatch to CORE's `trait_mod:<is>(Routine, :$export!)`, and
return an `&EXPORT` closure that becomes the importing module's own EXPORT.
Five separate gaps were in the way:

- A named sub declared inside a routine now behaves like rakudo's
  `capturelex`: when it is called from code that holds none of its bindings
  (an `our` alias, an exported sub, a multi candidate), it sees the latest
  activation of its routine instead of its caller's variables. `our` subs and
  multi candidates now take part in the ADR-0114 per-activation aliasing.
- An `is export` routine declared inside a routine body (here, inside
  `sub EXPORT`) survives the registry restore when the routine returns.
- `trait_mod:<is>(r, :export(...))` can now be called: it dispatches to a CORE
  candidate provided as a source-gated prelude, like the `:default` one.
- `my &k; sub f { &k = ... }` no longer dies with "Cannot modify an immutable
  value". The same holds for `:=`.
- `class C:auth<..>:ver<..> { my sub s {...} ... }`: the declarator adverbs no
  longer end the class body's lexical scope, so its methods and attribute
  defaults can see its `my sub`s. The same applies to roles.

Exercising the module turned up two more gaps. `so => 'socket'` was parsed as
the prefix `so` operator rather than a Pair. `nqp::stat` and `nqp::lstat` now
answer every MoarVM `STAT_*` field, and `File::Stat` reads all of them.
