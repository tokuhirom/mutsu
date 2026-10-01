# Hand-rolled AST walkers are ratcheted down onto the typed visitor

The typed AST visitor (ADR-0137) replaced the four analyses that serialized the
AST to JSON, but about a hundred hand-rolled recursive walkers over `Stmt` /
`Expr` remained, nearly all with a `_ =>` arm that silently skips every variant
they did not list. A variant handled by the visitor and forgotten by a walker
is exactly the double bookkeeping the visitor exists to remove.

`make check-ast-walkers` (part of `make checks`) now counts those walkers per
file against `scripts/ast-walkers-baseline.txt`; the count may only go down.
The first batch is ported: the `whenever`-outside-`supply`/`react` check, the
`&?ROUTINE`-outside-a-routine check for `EVAL`, the CHECK-time
undeclared-routine check, and the sink-warning search for `gather` bodies.
Because they now reach every child, they find what the old walkers missed — a
`whenever` in an operator operand, an undeclared routine called on the right of
`+=` or in a hash-literal value, `&?ROUTINE` in a pair value, a `gather` in an
attribute default — each confirmed against `raku`. The one place `raku` does
not look, a gather inside a signature, is excluded explicitly.
