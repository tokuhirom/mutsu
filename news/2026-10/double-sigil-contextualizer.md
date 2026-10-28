# `@@x` applies list context to a bound user Positional

`@@buffer` was parsed as the plain variable `@buffer`, so a user class that
does `Positional` and is bound to `@buffer` came back as the object. It is now
`@(@buffer)`: the object's `.list` (an Array stays itself). Found with
annotations' `Buffer` (#15286).
