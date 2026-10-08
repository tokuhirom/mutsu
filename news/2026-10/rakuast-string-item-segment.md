# `"a$(EXPR)b"` crosses the RakuAST boundary

An interpolated `$(EXPR)` inside a double-quoted string is `DoStmt(Expr(EXPR))` in mutsu's
parser output, which has no RakuAST counterpart, so any unit containing `"…$(…)…"` was refused
under `MUTSU_RAKUAST=1`. Rakudo 2026.09 renders the segment as a `Contextualizer::Item` over a
`StatementSequence` holding one `Statement::Expression`.

`convert` now renders that node for the segment (`contextualizer::convert_segment`), and `lower`
turns a one-expression `Contextualizer::Item` segment back into the parser's `DoStmt(Expr(..))`
(`contextualizer::lower_segment`), so the interpolation computes what the parsed string does.
Multi-statement and declaration contents (`"$(1; 2)"`, `"$(my $x = 5)"`) are still refused.
