# The `Unicode` type object is now available

`Unicode` was listed as a known type constraint but not as a runtime built-in
type. As a result, a bare `Unicode` term fell through to a `Str`, so even
`Unicode.^name` was wrong and the documented Unicode query methods could not
be called.

The built-in type catalog and bareword resolver now recognize `Unicode` as a
`Unicode -> Any -> Mu` type object. Its `version` class method derives a
`Version` from the Unicode tables exported by `unicode-normalization`, keeping
the reported version coupled to the normalization data mutsu actually uses.
`NFG` returns true because mutsu provides grapheme-aware Unicode handling.

`t/unicode-type-object.t` covers type-object resolution, MRO, the version
representation, and NFG availability.
