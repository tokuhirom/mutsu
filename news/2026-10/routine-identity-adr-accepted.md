# ADR-11827 accepted: upstream NativeCall's routine-identity shapes verified

`is native is symbol('qsort')` on a sub and an `is native` method (whose `{ * }` body must not
run) both work through the vendored upstream NativeCall and match `raku`. The pinned test is
`t/nativecall/native-routine-identity-shapes.t`; all 154 `t/` files that mention NativeCall pass.
With both shapes verified, ADR-11827 (a routine has identity; `does` composes in place) moves
from Proposed to Accepted.
