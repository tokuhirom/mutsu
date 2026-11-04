# The core bare calls are lowered by the classifier's answer

`callsame`, `nextsame`, `lastcall`, `nextcallee` and `done` written as barewords
were converted to RakuAST and lowered back by the executing finalization.
`bareword::core_bare_call` asks the one classifier (`view_node`) and, when the
word stays a call (no declaration of the unit shadows it), builds the call with
`lower::call_expr`, the function the named-call lowering now shares, without
building the node tree (ADR-15760 slice 2). A shadowing declaration is still
resolved by the conversion. `.AST` and `EVAL($ast)` are unchanged.
