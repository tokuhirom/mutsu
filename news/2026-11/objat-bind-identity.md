# Two `.WHICH` results are `===` but not `=:=`

`A.WHICH =:= A.WHICH` is now `False`, as on rakudo: every `.WHICH` call returns
a fresh `ObjAt`, and `=:=` (including `!=:=` and the `[=:=]` reduction)
compares object identity, not the WHICH text. `A.WHICH === A.WHICH` stays
`True`, and one `ObjAt` held in a variable or a binding is still `=:=` itself.
