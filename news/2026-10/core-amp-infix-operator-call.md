# `&CORE::infix:<op>` is the core operator routine

`&CORE::infix:<eqv>($a, $b)` died with "No such method 'CALL-ME' for string 'eqv'" and
`my &f = &CORE::infix:<*>` did not parse: the pseudo-package branch of the `&`
code-variable parser stopped at the category word `infix` and left `:<op>` to
be read as an adverb. It now takes the operator suffix, as `&Pkg::infix:<op>`
already did (#15245).
