# The code object `.^find_method` returns runs the method's compiled body

`.^find_method` / `.^lookup` (and the Method object's callable payload) built their `Sub` from the
method's AST with no `compiled_code`, so calling it, or `.wrap`-ing an accessor with a found role
method, recompiled the body through the `call_sub_value` tree-walk carrier at every call
(`LoggingHOW.redirect`, `W._rw_wrapper`). `method_def_callable` now carries the method's
`compiled_code` and its nested-sub table, and a method with none uses the empty table, as
`call_sub_value` already does for a closure. `Value::make_sub_compiled_in` takes the package
and `is rw` (#14572).
