# Interpolating heredocs are carried through finalization

An interpolating heredoc (`qq:to/END/`, `:to` with `$`/`{}` interpolation) is kept by
`finalize_unit` as the parser built it (`Expr::HeredocInterpolation`, read as a `qq` string when
the unit compiles) instead of being converted to a quoted-string node and lowered back
(ADR-15760 slice 2). `t/rakuast/rakuast-heredoc-finalize.t` pins the semantics and
`finalization_is_the_ordinary_tree` the tree.
