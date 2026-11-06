# A sigilless parameter forwarded an `@` parameter aliases the original array

`sub forward(@x) { store(@x) }` with `sub store(\v) { v[0] = 99 }` stored into
a fresh array that was dropped on return: the sigilless parameter's alias
named the caller's variable `@x`, which is a local of `forward` and invisible
from `store`, so the element store autovivified an array under that name. An
alias target the running frame cannot see is now ignored and the store goes
to the parameter itself, which shares the caller's array. BinaryHeap's
`heapsort(&cmp, @array)` (an `Iterable \values` parameter two calls below the
`@array` parameter) needs it to sort the caller's array in place.
