# A passed block keeps its loop topic's readonly mark

A block called with no argument writes the `$_` of the scope it was created in.
mutsu decided whether that `$_` was readonly from the dynamic readonly set,
which the called routine had cleared on entry. So inside `for 1, 2 { ... }`,
`dies-ok { $_ = 0 }` assigned to the loop's immutable topic instead of dying,
while the same block called inline died (#19067).

A block that writes `$_` now records the topic's readonly state when it is
created, next to the existing record for the free variables it writes. A call
with no argument restores that state for the duration of the call, journaled,
so the caller's own mark comes back on return. A call that passes an argument
rebinds `$_` and marks it as before.
