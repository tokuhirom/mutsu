# EVAL resolves a lowercase constant imported by its own `use`

`EVAL(q[use M; answer])` where `M` exports `my constant answer is export = 5` reported
`Undeclared routine: answer`. The EVAL undeclared-routine check consulted the parser's imported
function table but not its imported value terms, so a bare lowercase constant looked like an
unknown routine. The check now treats an imported value term as explained by the import.
