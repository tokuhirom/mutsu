# Slangs by execution, slice 2: a slang's own `term`, `value` and `number` productions run

A slang whose grammar role adds `token term:sym<answer> { 'answer' <.end-keyword> }` or
`token number:sym<roman> { 'roman:' $<digits>=<[IVXLCDM]>+ }`, with an action that attaches a
RakuAST value, now has those productions run where the parser starts a term (#13370, ADR-15762).
The expression the action attached is lowered and stands for the term (`ParsedExpr::Lowered`,
taken as it is by the unit's finalization). Same host, channel and hook as the `statement-control`
slice; a production inside a block a shim parsed nests.
