# `authoritative_captures` install loop stays: ablation result

ADR-0055 slice 5 follow-up (#14573). An ablation deleted the call-time install loops
for `authoritative_free_vars` and `authoritative_captures` in
`call_compiled_closure_in_unit`. Neither is redundant with the whole plain-lexical
capture install: removing the first breaks
`t/routines/closure/closure-free-var-scope.t` and
`roast/integration/99problems-41-to-50.t`; removing the second is invisible to
`t/lang`, `t/vm`, `t/routines`, `t/oo` and `t/collections` but breaks four
`t/concurrency/supply/` tests (`supply-block-reinstantiated.t` and three more).
Both loops stay, and their comments now record which tests pin them.
