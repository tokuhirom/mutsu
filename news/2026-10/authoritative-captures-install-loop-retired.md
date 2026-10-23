# Retire the `authoritative_captures` call-time install loop

ADR-0055 slice 5 follow-up (#14573). An ablation showed the loop that
re-installed `SubData::authoritative_captures` in `call_compiled_closure_in_unit`
is redundant with the whole plain-lexical capture install above it: deleting it
leaves all of `t/lang`, `t/vm`, `t/routines`, `t/oo` and `t/collections` green.
The `authoritative_free_vars` install loop is not redundant (removing it breaks
`t/routines/closure/closure-free-var-scope.t` and
`roast/integration/99problems-41-to-50.t`), so it stays, and the doc comments
now say which of the two is a hint and which is still a gate.
