# A Sub/Block in a numeric comparison fails like rakudo's

`2 == { 1 }`, `2 < &sub`, `2 <=> $block`, `$block.Numeric`, `.Int` and `.Real` now raise
`X::Multi::NoMatch` ("Cannot resolve caller Numeric(Block:D: ); ... (Mu:U \v:: *%_)"; the
ordering operators name `Real`) instead of comparing as `False` or reporting a missing
method, and the candidate line reads `\v::` as in rakudo. Found by the raku-doc
differential sweep (`Language/haskell-to-p6.rakudoc`: `given {...} { when 2 { ... } }`).
