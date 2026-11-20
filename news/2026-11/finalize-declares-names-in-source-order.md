# A compilation unit's names are declared in source order when it is finalized

The finalization of a compilation unit installed every name the unit declares
anywhere (its constants, sigilless terms, packages and types) before it
lowered the first statement. A routine body written before a declaration saw
the declaration:

```raku
say tau; sub f { tau }; constant tau = 1; say f   # rakudo: 6.283... twice
class A { method m { pi } }; my \pi = 2; say A.m  # rakudo: 3.14159...
```

mutsu printed `1` for the first, and the second died with `Undeclared routine:
pi`. Rakudo resolves a name where it is written, in source order, and hoists
routines only (ADR-15760 §2.3, Amendment 2 §7.2 (c)).

Each top-level statement now declares its names as the finalization reaches
it; only the unit's routines are known up front, because a routine is hoisted
(`foo; sub foo { ... }` still works, and a routine word inside a body before
the routine is still late-bound). This is slice 2b of ADR-15760 Amendment 2;
`t/rakuast/rakuast-finalize-source-order.t` pins the rows against rakudo
(#15760).
