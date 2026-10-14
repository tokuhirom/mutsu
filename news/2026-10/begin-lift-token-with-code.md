# A BEGIN after a token that runs code is lifted

A nested `BEGIN` is lifted into the unit's BEGIN prologue (ADR-0134), but a
`token`/`rule`/`regex` whose pattern ran code or interpolated a variable kept
it in place — and, to preserve source order, every later nested `BEGIN` of the
unit too, so a `BEGIN` in a routine that never ran was silently lost:

```raku
sub f { my $c = 0; my token t { x { $c++ } }; BEGIN { say so "xx" ~~ /<t>+/; say $c } }
sub g { BEGIN say "b" }
say "m";   # raku and now mutsu: True, 2, b, m — mutsu printed only m
```

The token's code blocks, predicates, quantifier blocks, subrule arguments and
interpolated variables are now read from the pattern's static regex tree, and
the lifted block supplies the lexicals they close over the way it does for a
routine (#13096). A token that declares a variable (`:my`) or takes
parameters still keeps its scope's BEGINs in place.
