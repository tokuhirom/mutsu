# A multi dispatches on the native type of a List item's container

`n16(($b1, $b0))` with `my byte` variables failed to resolve `multi n16(byte $a,
byte $b)`: the List's items are the variables' own cells (with `byte` registered
on them), but an element argument was dereferenced at the call site and the
dispatcher read a declared type only from a `VarRef`. Cells now carry a lock-free
native marker; the applicability filter, the ranking and the multi-dispatch cache
key read the native type of a container argument, and an element argument hands
over a List slot's native cell (ADR-15309 slice 1). An `Array` copy, a sub return
and a literal stay `Int`, as in rakudo. Net::BGP `t/00-conversions.t` now matches
rakudo (52/52).
