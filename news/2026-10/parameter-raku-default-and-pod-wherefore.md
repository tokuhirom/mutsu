# `Parameter.raku` keeps literal defaults; pod declarant WHEREFORE knows multi and accessors

`Parameter.raku` now spells a literal default back (`Str :$b = "asdf"`), and the
`WHEREFORE` of a `#|` declarator on a `multi` routine reports `.multi`, while a
documented `has $.a` attribute reports `has_accessor`. Found via the
Pod::To::Markdown suite: `t/declarator.rakutest` now passes by direct run.
