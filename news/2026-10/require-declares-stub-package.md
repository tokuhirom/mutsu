# A literal `require Foo` declares a stub package, even when the load fails

`try require Foo; say ::("Foo").^name` printed `Failure` for a module that does
not exist. Rakudo prints `Foo`: the `require` statement declares its target as
a stub package in the lexical scope it sits in while the program compiles, so
the name resolves from the head of that scope on and stays resolvable when the
load fails.

The compiler now declares that stub on entry to the owning scope
(`src/compiler/require_stubs.rs`), next to the scope's hoisted routines, through
the new `DeclareRequireStub` opcode:

- a lookup before the statement already finds the stub
  (`say ::('Foo').^name; try require Foo;` prints `Foo`);
- the stub is lexical like a `my package`, so a bare block's stub does not
  escape the block;
- it never shadows a name that already resolves, so a module or type that is
  loaded keeps its real binding, and the real load registers over the stub;
- a computed target (`require ::($name)`) and a file path declare nothing;
- the bare name parses as a type from the statement on, so `Foo.^name` works
  after a failed `require Foo`.

This replaces the stub the BEGIN prologue (ADR-0134) used to add for the same
purpose, which only existed in a unit that already had a BEGIN-time effect and
never reached a `require` inside `try` or a nested block.

Not covered yet: the stub of a `require` in a braced `try { ... }`, and its
lexical scoping inside an `if`/loop body, a routine called twice, and `EVAL`.
The existing `my package` and `my class` declarations leak from the same places,
so the gap is the lexical type scoping of those constructs, not `require`
(tracked in #10594).
