# `Str.indent` with a huge count throws instead of aborting

`"abc".indent(99999999999)` used to abort the process on allocation failure. The indent padding is
now built by the shared `str_prim::repeat` primitive, so the call throws rakudo's
`Repeat count (...) cannot be greater than max allowed number of graphemes 4294967295` error, which
`try` can catch.
