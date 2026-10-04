# `!!!` dies, and the yada stubs cross the RakuAST boundary

The `__mutsu_stub_die` marker stopped 46 `t/` files at `.AST`. Looking at
it showed a wrong answer underneath. The parser spelled `...` and `!!!` as
the same marker, which *fails*, so `!!!` returned a Failure to its caller:

```raku
sub g { !!! }; my $x = g(); say "alive"   # mutsu: alive; rakudo: dies
```

Rakudo's `!!!` dies on the spot. `...` fails, and `???` warns and carries
on.

- `!!!` now has its own `__mutsu_stub_fatal` marker, whose builtin raises
  X::StubCode instead of failing.
- A stub without a message now carries no argument; the runtime supplies
  "Stub code executed". So `...` and `... "Stub code executed"` stay
  distinct trees.
- The new `ast::stub` module names the three markers. Ten places asked
  "is this body a stub" with their own copy of the name test (`.yada`, role
  requirements, stub classes, forward declarations); all now call
  `ast::stub::is_marker`.

Measured on rakudo 2026.09, the stubs are `RakuAST::Stub::Fail`, `Die` and
`Warn`, each with an `args` list only when a message is written. The
converter renders them, and the lowering turns them back into the marker
calls.

A stub inside a named sub showed one more bug under `MUTSU_RAKUAST=1`. A
body without `SetLine` statements runs on the TRIR path, and that path's
routine boundary let a `fail` escape as an exception instead of returning a
Failure. `run_trir_routine` now does what the untyped entries do. This is
part of #11728; the missing-`$*x` case it reports is a separate TRIR gap and
stays open.
