# Regex: `"$0"` inside a pattern reads the regex's own (still Nil) `$/`

Inside a regex, a double-quoted atom's `$0`, `$/` or `$<a>` names the regex's
own `$/`, which is still Nil while the regex matches. In rakudo,
`'abxab' ~~ /(ab)x"$0"/` therefore matches `abx`, warning "Use of Nil in string
context". mutsu took `$0` there as literal text and failed the match. Those
variables are now read at match time as that Nil, so they contribute the empty
string and warn, as in rakudo. An interpolation of the empty string
(`/x $("")/`) also matches the empty string now, instead of raising "Null regex
not allowed". This fixes roast `S05-mass/rx.t` tests 127-134.
