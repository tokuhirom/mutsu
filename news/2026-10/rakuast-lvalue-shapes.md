# RakuAST keeps the shape of an assignment's left side

`.AST` now records the left side of an assignment the way rakudo 2026.09
does (#14911):

- A prefix (`-$x = 5`), ternary (`$y ?? 1 !! $x = 2`) or named capture
  (`$<c> = $0`) target is the node itself, not a call on the term.
- A parenthesised target keeps its parentheses, once per pair written:
  `($x) = 1`, `(($x)) = 1`, `(c ?? $a !! $b) = 9`, `(-$x) = 5`. As in
  rakudo, that makes the assignment a list assignment.

These targets are now part of the typed assignment node (ADR-10723 Stage 2,
#14948). The statement parser and the ternary build `ParsedExpr::Assign`
for an lvalue it writes through at run time. `rakuast::assign_lower` holds
that writeback, and a hand-built tree lowers through the same function. The
trees evaluate like the source, and rebuilding them from their `.raku` gives
the same tree (`t/rakuast/rakuast-lvalue-shapes.t`).
