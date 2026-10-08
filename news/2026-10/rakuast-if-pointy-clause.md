# `if COND -> PARAMS { }` crosses the RakuAST boundary in every parameter form

`if` / `elsif` clauses with a pointy parameter were converted only when the parameter was a
plain scalar (`-> $v`). Sigilless (`-> \r`), `@`/`%`/`&`-sigilled, typed, `$_`, destructuring
and any other signature were refused, which was the "`if EXPR -> $var` topic binding of another
form" first-refusal cause of the round-trip frontend ratchet.

The parser's expansion now leaves a `SourceForm::IfPointy` record (the parameters and body as
written) at the head of the then-branch, as `given EXPR -> PARAM` already does. `convert`
renders the clause as a `PointyBlock` from that record, and `lower` hands a `PointyBlock`'s
signature back to the same parser function (`if_pointy_clause`), so a hand-built `Statement::If`
expands exactly like parsed source. The ratchet grows by the files that were stuck on this cause.
