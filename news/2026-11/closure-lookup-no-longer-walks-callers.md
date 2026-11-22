# A closure no longer looks up names in its callers

A closure resolves a name through the scope it was written in: first its own
captures, then the program scope. Until now, a lexically scoped name that
neither of them bound did not end the lookup. It walked on into the frames
that happened to be calling the closure, a dynamic-scope leak that ADR-12529
set out to retire. That fallback is gone: such a lookup now ends at the
closure's frame root (ADR-12529 phase 4).

The walk had been hiding real bugs. Each was fixed first, in its own slice of
#18248:

- a forward-declared code variable;
- a closure in an `@`/`%` declaration's own initializer;
- recursion of a block-scoped sub;
- a sub nested two levels deep reading its outer routine's parameter;
- a signature's `where` clause, which ran in the smartmatching caller's scope
  (#18570).

With all of them fixed, the full test suite and roast pass without the
fallback.
