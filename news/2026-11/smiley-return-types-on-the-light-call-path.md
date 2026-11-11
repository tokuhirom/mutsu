# Smiley return types stay on the light call path

A routine declared with a definiteness smiley on its return type
(`my sub is-alpha(Str $ch --> Bool:D)`) used to leave the light and
positional-light call paths entirely: the return check only knew the bare
fast type names, so `Bool:D` sent every call through the full call path,
with an environment clone and deep copy per call. Such a call cost about
5.5 us where the same routine with `--> Bool` cost 0.6 us.

The routine's registration now splits a `:D` / `:U` smiley off the return
type, classifies the base as before, and the light return check answers the
smiley from the value's shape: `Nil` and a `Failure` pass, a type object
satisfies only `:U`, a concrete value only `:D`. A shape the light check
cannot classify, and every rejection, is answered by the general type
matcher, so the verdict and the `X::TypeCheck::Return` message are exactly
the general path's.

A chained comparison whose middle operand is a plain variable or a literal
(`0x41 <= $ord <= 0x5A`) now reads that operand in place instead of binding
it to a temporary inside a block, which took about 1 us per chain.

Together they take MCP::Server::Tool::Web's `html-to-text` benchmark of the
`t/12-extract.rakutest` fixture's parts from 47.8 s to 39.4 s (release build,
second run); the remaining gap to rakudo is the general interpreter speed
tracked in #15737 (#16897 stays open).
