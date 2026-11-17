# `andthen` / `orelse` / `notandthen` call a Whatever-curried right-hand side

`(%h andthen *<label>)` is `%h<label>` on rakudo 2026.11: a Whatever-curried
right-hand side of `andthen`, `orelse` or `notandthen` is called with the left
operand, exactly like a block literal. mutsu used to call it, but stopped
recognising it once the parser started emitting the `WhateverCurry` priming
marker (ADR-0033) instead of the expanded closure, so the operator returned the
WhateverCode itself. `notandthen` never called a block literal at all; it now
shares the RHS logic of the other two (`Int notandthen -> $x { $x.raku }` is
`"Int"`).

This was the remaining failure of App::Moneymoor's `t/65-main-shell.rakutest`,
whose `tab-label` method returned a WhateverCode into a `--> Str` signature.
