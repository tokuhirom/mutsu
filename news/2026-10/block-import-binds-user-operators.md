# A block's `use` binds imported user operators lexically

A user infix operator imported by a `use` inside a block is now bound in the
block's lexical import set rather than in the routine registry (ADR-8751
stage 2). A reduction (`[+%+] @a`, `[\+%+] @a`), a hyper (`@a >>+%+<< @b`), a
compound assignment and `&infix:<+%+>` all reach the import, from a closure
made in the block and from a `.map` or `gather` body consumed after it.
Before, they resolved the operator by name and reached the importing
unit's own operator once the block had exited. A routine written outside
the block no longer sees the imported operator either.

Two related fixes:
- **Junction autothreading.** A call to any lexically imported routine
  autothreads a Junction argument again.
- **Recompiled bodies.** A `.map` body that calls an imported routine keeps
  the import when the body is compiled again for the map loop.
