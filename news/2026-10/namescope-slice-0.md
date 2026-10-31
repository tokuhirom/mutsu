# `NameScope` replaces four frontend thread-locals (ADR-15760 slice 0, part 1)

`src/scope.rs` introduces `NameScope`: the names the unit declares, the names an `EVAL` string's or `Str.AST`'s
calling scope declares, the type names for `is NAME` traits, and the operator routines the caller makes visible.
It replaces `CallerNames`, `CallerTraitTypes`, `with_caller_operators` and the `DECLARED_NAMES` thread-local.
`round_trip_eval` now takes a `NameScope` instead of three closures, and `Str.AST` and a lowered `EVAL($ast)` build
one (`frontend::str_ast_scope`, `lower_scope`) and install it with `frontend::with_scope`. No behaviour change.
What remains of slice 0 is threading the scope as a parameter through `convert` and `lower` instead of one
installed value.
