# Regex `^` after another atom is a start-of-string anchor

`/z ^ abc/` used to die with "Unrecognized regex metacharacter ^". A `^` that follows another
atom now compiles to a new zero-width `StartOfString` atom (matches only at position 0), as in
Rakudo, so `"zabc" ~~ /z ^ abc/` is `Nil` instead of a compile error. This lets
`roast/S05-mass/rx.t` get past its compile step.
