# An allomorph's .Stringy answers the allomorph

`<1>.Stringy` answered a plain `Str`. Rakudo's `Str.Stringy` is `self` and an
allomorph is a `Str`, so `IntStr`/`NumStr`/`RatStr`/`ComplexStr` now come back
unchanged. `Stringy` left the allomorph's own-name list in
`method_table::allomorph` and redirects to the `Str.Stringy` row with the
allomorph itself as invocant. Pinned in `t/types/coercion/allomorph-stringy-identity.t` (#13108).
