# RakuAST: zen slices inside strings keep their subscript

`"@x[]"`, `"@x.[]"` and `"%h{}"` now parse as the same zen slice they are outside a string, so
`.AST` shows rakudo's `ApplyPostfix(Var, Postcircumfix::ArrayIndex/HashIndex)` instead of a
bare variable. Interpolation output is unchanged. Row 3 of #14820 (`"%h<>"` and `"%h[]"` stay
as before).
