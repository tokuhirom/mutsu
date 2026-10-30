# `.^meta` calls ask the receiver's HOW directly

`Type.^find_method(...)`, `.^lookup`, `.^can`, `.^mro` and every other `.^meta` call except
`.^name` are dispatched as `Type.HOW.meta(Type, ...)`. The first step, getting the HOW, went
through the whole general method-call chain (`call_method_with_values` -> receiver gates ->
`dispatch_how`), which costs ~38k Ir for a macro no user method can intercept.

The four places that do this (`try_compiled_method_mut_or_interpret_sym_inner`,
`try_compiled_method_or_interpret_inner`, the `^meta_method` arm of `call_method_with_values_inner`
and the metamethod binding of a user-declared `method ^foo`) now call `dispatch_how` directly.

Measured with callgrind (profiling build, slope between 300 and 900 iterations, startup
subtracted, both binaries built from the same `main` plus unrelated light-call changes):

| call | before | after |
| --- | ---: | ---: |
| `C.^find_method("foo", :no_fallback(1))` | 74,758 | 46,874 Ir |
| `C.^lookup("foo")` | 57,568 | 39,054 |
| `C.^mro` | 68,252 | 49,990 |
| `C.^can("foo")` | 107,971 | 89,253 |
| `C.^name` (not on this path) | 41,493 | 41,539 |

`Red::Column.new` runs `.^find_method` once per construction (its `TWEAK`), part of #15498.
