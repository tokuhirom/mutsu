# Every top-level `use` and `constant` runs at BEGIN time

ADR-0134 slice 3 is finished (#10336). A unit's top-level `use`, `need`,
`import` and `constant` are now BEGIN-time effects in their own right: they
run in the unit's prologue, in source order, ahead of the run-time statements
that precede them, and they see lexicals in their static state.

```raku
say 'run'; use Foo;                        # Foo's mainline says 'loaded'
# before: run / loaded     now (and raku): loaded / run

my $x = 5; constant K = $x; say K.raku     # before: 5   now (and raku): Any
```

Widening the prologue surfaced three gaps, each fixed here:

- Lexical pragmas that mutsu applies as run-time state (`use strict`,
  `no strict`, `use fatal`, ...) stay in their source position. `use lib` and
  `use if` still move, because the loads after them depend on them.
- The mainline now takes its prologue before flattening desugared
  multi-statement declarations, so `my ($a, $b) = f()` ahead of a `use` keeps
  its initializers. An exported class declaration moves into the prologue
  together with its export marker, so a module's `constant Alias = Class`
  sees the class.
- A block's `use` that re-imports a name the unit already imported
  (`use M :t; { use M } t`) removed that name when the block exited. The
  import scope now restores the value it shadowed.
