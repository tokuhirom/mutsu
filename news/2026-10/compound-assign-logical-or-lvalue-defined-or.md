# `(A || B) //= v` assigns into `B`

`callsame() || %!meths{name} //= do { ... }` (CSS::Box's `can`) parses as
`(callsame() || %!meths{name}) //= ...`: the `||` yields the selected operand's
container, so the assignment writes into the hash element when the left operand is
false. The compound-assignment expansion treated the whole `||` as a value and
died with "Cannot modify an immutable value". `(A || B) //= v`, `(A || B) ||= v`
and `(A // B) //= v` now expand to `A || (B //= v)` and so on: when `A` is the
selected operand its value is already defined (true), so the short-circuit
assignment is a no-op; otherwise the target is `B`. `A` is evaluated once.

CSS::Properties `t/box-measure.t` goes from 9 to 19 ok (rakudo 19) and `t/box.t`
from 8 to 13 (rakudo 19).
