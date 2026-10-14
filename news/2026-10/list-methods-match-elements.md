# List methods over Match elements match Rakudo

`minpairs`/`maxpairs` answer a `List` (and a `Match` reads its positional captures),
`minmax` numifies `Match` endpoints, `chrs` answers an `X::TypeCheck` Failure for
elements it cannot convert, and `rotor` without a cycle spec is `X::Multi::NoMatch`
(#13195).
