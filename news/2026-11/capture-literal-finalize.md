# Capture literals are carried through finalization

ADR-15760 slice 2: `Expr::CaptureLiteral` (`\(1, 2)`, `\(:x(1))`) reads back as the same literal
after the RakuAST round trip, so `finalize_unit` keeps it as the parser built it instead of
converting it to a `Term::Capture` node and lowering it back. The identity is pinned by an
additional source in `finalization_is_the_ordinary_tree`.
