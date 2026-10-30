# `for` over an instance with its own lazy `iterator` pulls it per iteration

`for $obj -> $n { ... }` over a bare instance whose class declares `iterator`
now pulls a lazy (`is-lazy`) iterator one iteration at a time, also inside
`gather`, instead of draining it (or, for an `is Range` subclass, iterating the
range the instance carries and ignoring the override). Found with
`Math::Sequences` (`lazy gather for 𝕀 -> $n { ... }`, #15353).
