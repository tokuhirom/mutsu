# A dispatch-failure message names a lexical class by its declared name

`Cannot resolve caller f(M:D: Str:D); ...` no longer prints the internal
serial of a `my class` (`M<NUL>2`, shown as `M 2`) in its invocant type; the
`X::Multi::NoMatch` and `X::Multi::Ambiguous` messages use the declared name,
as rakudo does (#16621).
