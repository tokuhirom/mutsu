# `Str` methods refuse type objects as Rakudo does

`Str.NFC`/`NFD`/`NFKC`/`NFKD`/`uniprops` normalized or classified the type's name and
`Str.encode` encoded it; Rakudo refuses each (`X::Multi::NoMatch`, and `X::AdHoc` "Cannot unbox a
type object" for `encode`). The `Str` unicode rows no longer admit a type object, `NFKC`/`NFKD`/
`uniprops` joined the `Cool` type-object gate, and `encode` refuses a `Cool` type object. An
allomorph type object (`IntStr`, ...) is now `X::Parameter::InvalidConcreteness` for the
`Str:D`-only methods (`flip`, `uc`, `trim`, ...) and `X::Multi::NoMatch` for the `multi`s,
instead of `X::Method::NotFound`. Closes #12849.
