# EVAL subscript assignment to a caller's sigilless term

`EVAL('sigless<a> = 3')` over the caller's `my \sigless = {...}` was lost: the EVAL'd unit
cannot see the caller's sigilless binding, so the lvalue root `sigless` of the subscript
assignment was compiled against the plain key. An lvalue root the unit cannot see at all
now takes the term key, exactly as a `sigless = 3` statement already did, and the VM
resolves it against the caller's scope (#15388).
