# `Array.AT-POS` answers the element container; `BIND-POS` on an `is Array` instance binds the caller's variable

`@a.AT-POS($i)` now answers the element's own container, as Rakudo's `is raw`
`Array.AT-POS` does, and a `callsame` from a user `AT-POS` on an `is Array`
subclass stays assignable on a plain read too (not only inside an assignment).
`self.BIND-POS($pos, $lval)` on an `is Array` instance binds the element to the
caller's `$lval` container instead of a read-only copy of its value. Both were
needed by PDF::COS::Tie::Array: PDF 0.6.17's `t/indobj-stream.t` goes from 13 to
26 of 27 passing tests (#12907).

Also fixed on the way: `$.x = v` with `has $.x` and an explicit `method x is rw`
stores through that method instead of raising "Cannot modify an immutable".
