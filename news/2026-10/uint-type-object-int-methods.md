# The UInt type object dispatches Int's methods

`UInt.Range` was `X::Method::NotFound` and `UInt.^mro` was `((UInt) (Any) (Mu))`.
In Rakudo `UInt` is `subset UInt of Int where * >= 0`, so its type object
reports `((UInt) (Int) (Cool) (Any) (Mu))` and answers `Int`'s methods. The
builtin type catalog now lists `UInt` with that MRO, and `Int.Range` answers
`0..^Inf` for it (#13178).
