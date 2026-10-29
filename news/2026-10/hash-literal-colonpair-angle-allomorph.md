# `:k<4>` inside a hash literal is an allomorph

A colonpair with an angle value written inside a `{ ... }` hash literal body built a
plain `Str` (`{ :k<4> }<k>` was `Str`), while the same pair elsewhere was an `IntStr`.
The hash-literal parser now builds the value through `angle_word_value`, so `<4>` is an
`IntStr` and `<4.1>` a `RatStr`, as in Raku. Pheix's `t/00-november.t` compares
`is-deeply` against such a literal.
