# A sub refuses a named argument none of its parameters answers to

`sub f(:$a) { }; f(:b(1))` now dies with `Unexpected named argument 'b' passed`,
as in rakudo. The light named-call path (any sub with declared named parameters
and no slurpy hash) bound the parameters it knew and silently dropped the
rest; it now checks every named argument, in-band and out-of-band, against the
parameters' names and aliases. Methods keep taking any name (implicit `*%_`).
