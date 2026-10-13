# Unicode methods on Nil die; property-less unimatch block names follow Rakudo

`Nil.uniprop`, `.uniprops`, `.unival`, `.univals`, `.unimatch` and `.uniname`, and
`uniname(Nil)`, now fail with `X::Multi::NoMatch` (`Cannot resolve caller
uniprop(Nil:U: )`) as in Rakudo, instead of being absorbed by `Nil`'s method
fallback and answering `Nil`. A property-less `unimatch` value names a block with
underscores and not spaces or hyphens (`"A".unimatch("Basic Latin")` is `False`),
and an unassigned codepoint is in `No_Block` for `unimatch`. `chr` of a surrogate
(the third item of #13192) is left open.
