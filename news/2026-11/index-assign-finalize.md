# Element assignments are carried through finalization

ADR-15760 slice 2: `Expr::IndexAssign` (`@a[0] = 5`, `%h<a> = 1`, `@m[0][1] = 7`, chained and slice
forms) reads back as the same expression after the RakuAST round trip, so `finalize_unit` keeps it
as the parser built it. An indexed bind (`@a[0] := $b`, whose value is an `IndexBindValue`) stays on
the conversion: that child has no node outside the assignment, and `.AST` of such a bind is not
supported yet. About 6% of the fallback census (30 of 466 expressions) was this form.
