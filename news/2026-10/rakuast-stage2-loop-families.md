# Loops, topicalizers and bare blocks carry their block origins in `.AST`

The second group of construct families moves to ADR-10723 Stage 2, on the same
mechanism as the conditional family. The parser now builds these as typed
`ast::parsed` nodes:

- `while` / `until`, `loop` (C-style or bare) and `repeat ... while` / `until`
- `given` / `when` / `default`
- a bare `{ ... }` statement

Each node records the source range of its `Block` or `PointyBlock`, its
`Blockoid` and its `StatementList`. A `for` loop keeps its `Stmt::For`, which
already holds what the source wrote, together with the positions of its block.

An executing parse lowers the node where it is built. A `.AST` parse keeps it
and renders it with those origins. The ranges match rakudo 2026.09, and a
pointy block's range starts at its `->`.

Lowering a hand-built `Statement::Given`, `::When`, `::Default`,
`::Loop::While`, `::Loop::Until`, `::Loop`, `::Loop::RepeatWhile` or
`::Loop::RepeatUntil` now reads the node into the same typed struct and goes
through the same lowering. The separate `lower_while`, `lower_cstyle_loop` and
inline `given` / `when` / `repeat` arms are gone.

`visit-children` of a `repeat` loop visits the condition before the body, as
rakudo does.

The results are pinned by `t/rakuast/rakuast-origin-block-statements.t`, which
passes on both mutsu and rakudo.
