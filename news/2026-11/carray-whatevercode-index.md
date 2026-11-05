# `$carray[*-1]` indexes from the end

The `CArray` subscript with a `Callable` index calls `$block.POSITIONS(array)`,
which mutsu's `Code` did not have, so `$carray[*-1]` died with "No such method
'POSITIONS' for invocant of type 'WhateverCode'". `Code.POSITIONS(list)` now
calls the code with `list.elems`, as rakudo's does, so `*-1`, `*-2` and
`*-3 .. *-1` index a `CArray` from the end (#16488).
