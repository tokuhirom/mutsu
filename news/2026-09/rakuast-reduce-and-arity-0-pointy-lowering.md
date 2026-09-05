# RakuAST reduction and arity-0 pointy-block lowering

Two more write-direction gaps closed. Both constructs have been readable for a
while and simply had no lowering, so `EVAL` refused a tree the converter had
just produced.

- **`RakuAST::Term::Reduce`** — `[+] @a` and the triangle form `[\+] @a`.
  mutsu's `Expr::Reduction` keeps the triangle marker inside the operator string
  itself (a leading backslash), which is how the converter reads it back out
  into the `triangle` field, so the lowerer puts it back the same way.
- **A zero-parameter `RakuAST::PointyBlock`** — `-> { … }`. The
  single-parameter form lowered to `Expr::Lambda` and the multi-parameter form
  to `Expr::AnonSubParams`; the arity-0 form was left as an explicit boundary
  even though the parser builds exactly the same `AnonSubParams` node with an
  empty parameter list. It now takes the same path, so the lowered closure keeps
  arity 0 — unlike a bare block, `-> { … }` rejects arguments.

## Coverage

`t/rakuast-eval-reduce-pointy.t` (10 assertions) pins `[+]`, `[*]`, `[~]`, a
reduction over a literal list, the triangle form's running results, an arity-0
pointy block's call and `.arity`, and the neighbouring single-parameter,
two-parameter and bare-block forms. It is a dual-oracle test: it passes verbatim
under both mutsu and raku.

## A parser bug found while writing it

`Q[[+] 1, 2, 3]` does not mean what it says: a bracketing quote whose content
*starts* with the same opening bracket loses that bracket and its match, so
`Q[[1]]` yields `1` rather than `[1]` and `Q[[1] 2]` fails to compile. Anything
before the nested bracket is handled correctly (`Q[x[1]]` is fine), so it is a
leading-nested-delimiter bug in the bracketing-quote scanner, not a nesting bug
in general. It matters here because `Q[...]` is the idiomatic way to hand a
program to `.AST`, and a *silently different* program is worse than an error in
a dual-oracle test. The affected assertions use `Q{...}`, and the bug is filed
as `todo/tickets/q-bracket-leading-nested-delimiter.md`.
