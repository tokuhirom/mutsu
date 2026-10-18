# A `whenever` calls the supply block's own sub, declared after it

`supply { my @u = 1, 2; whenever $src { acc() }; sub acc() { @u.push(3) } }`
lost the `push`: the `whenever` body called `acc` by name, and the registry
answered with whatever `acc` was registered last, reading a stale or foreign
`@u` (#13613, found on Log::Timeline's socket output). A `supply` body is
compiled again from its AST when the supply is tapped, and that recompile did
not record the block's routines as bindings for the closures nested in it
(ADR-12529 phase 2). It does now, so the `whenever` closure takes this
activation's `acc` as an upvalue.

Two by-name leaks that the same programs exposed are fixed with it:

- ADR-0024's captured-cell bucket was keyed by the sub's bare name, so a
  routine-nested `sub acc` read the cells of a program-scope or block
  `sub acc`. A bucket now belongs to the compiled bodies that captured it.
- A routine call seeded every local slot from the env by name, including the
  routine's own `my` variables, which then started out holding the caller's
  same-named variable (ADR-12529 §2.3). A plain `my` the routine declares is
  no longer seeded; its declaration initializes it.

Two declarations with identical bodies still share one compiled body and so
one bucket (#13890).
