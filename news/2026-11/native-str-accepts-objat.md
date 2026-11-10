# `my str $k = $x.WHICH` accepts an ObjAt

A native `str` variable now unboxes an `ObjAt`/`ValueObjAt` (a `.WHICH` result)
to a plain string on assignment, as rakudo does. ParaSeq's `unique(:with)`
stored its seen-keys this way and died with "Cannot unbox a type object to str".
