# `.chrs` accepts IntStr elements

`<67 97 109 101 108 105 97>.chrs` answered a `Failure` ("expected Int but got IntStr") in mutsu:
the element check looked only for a plain `Int` or `Str` and the numeric words of a `< >` list
are `IntStr` allomorphs. `IntStr` is an `Int`, so `.chrs` now reads its integer part (`RatStr` and
`NumStr` elements still fail the check). Found by the raku-doc differential (`Type/Cool.rakudoc:1045`).
