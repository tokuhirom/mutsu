# `temp @a[0].attr = v` parses and restores

`temp` over a method or attribute lvalue whose invocant is an element
(`temp @a[0].n = 9`, `temp %h<k>.n = 5`, and the compound `temp @a[0].s ~= "b"`)
was a parse error ("Confused. Two terms in a row") (#16165). `Stmt::TempMethodAssign`
now carries an optional invocant expression for that shape; the save and the
restore go through the attribute accessor of the object the element holds, as
`temp $obj.attr` does, so every holder of the object sees the temporary value
and the restore. The RakuAST view and its lowering round-trip the new shape.
Pinned by `t/oo/method/temp-element-invocant-method-assign.t`.
