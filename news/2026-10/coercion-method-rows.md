# The numeric coercions are method-table rows

`.Real`, `.Num`, `.UInt`, `.Int` and `.Numeric` of a receiver whose own type declares none are now rows of the built-in
method table, owned where Rakudo declares them (ADR-11276 §9.92). `Cool.Real` is new. `Cool.Num` and `Cool.UInt` take the
type objects, lazy lists and `Match`es they did not reach before, and `Mu.Int`/`Mu.Numeric` answer type objects, ranges,
buffers, `StrDistance` and stashes. The coercion arms of `methods_0arg/dispatch_core_coerce.rs` that used to answer them are
gone, and the pure arity cascades drop from 167 to 149 quoted-name arms.

Two answers moved to Rakudo's on the way. `(1..Inf).UInt` is now the `X::Numeric::CannotConvert` `Failure`, and
`slip(1, 2).UInt` is `2`; before, these threw `X::AdHoc` and `X::Method::NotFound`.
