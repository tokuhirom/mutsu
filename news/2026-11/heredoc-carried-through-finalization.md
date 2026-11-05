# Interpolating heredocs are read as `qq` strings by the finalization

An interpolating heredoc (`qq:to/END/`) of a spelling-keeping parse is expanded by `finalize_unit`
itself (its body is read as a `qq` string with the parser's own `interpolate_heredoc_content`, and
the resulting expression is finalized in turn) instead of being converted to a quoted-string node
and lowered back (ADR-15760 slice 2). A heredoc that closes an enclosing block on its marker line
is still converted. `t/rakuast/rakuast-heredoc-finalize.t` pins the semantics.
