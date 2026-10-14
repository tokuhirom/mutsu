# `Signature.Str` is the `Signature<address>` form

Rakudo's `Signature` declares `gist` and `raku` but not `Str`, so `.Str`, `.Stringy`, prefix `~`
and interpolation fall to `Mu.Str` and answer `Signature<address>`. mutsu rendered the source form
because the signature instance carried a `Str` attribute and the value-level stringifier had a
`Signature` special case. Both are gone; `.gist` (`(Int $x)`) and `.raku` (`:(Int $x)`) are
unchanged. Closes #13109.
