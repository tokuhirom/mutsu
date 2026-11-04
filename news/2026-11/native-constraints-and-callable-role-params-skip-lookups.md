# Native `int` / `num` constraints and role parameters that are types skip needless lookups

`type_matches_value` answers a plain `Int` argument against `int` and a `Num` against `num` (the
64-bit native types) before the full walk, like the core ancestors it already short-circuited, and a
mixin's role method call reads the role candidate's parameter list for callable (`&f`) parameters
only when some role parameter value is a callable (a CArray's `TValue` is a type).

Callgrind slope on a profiling build, second run: fill 83.0k -> 82.3k Ir per store, fill + `.Array`
186.0k -> 185.1k Ir per element: small, and stated as such. The profile of what is left is flat:
about 55 `Symbol::intern` calls and 3 parameter binds per store, `call_compiled_method_in_unit`
frame work, and a ~19k Ir preamble in `dispatch_mixin_method_call`. Refs #16215.
