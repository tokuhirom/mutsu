# A string-pattern `.subst` no longer overwrites the caller's `$/`

`Str.subst` with a string (non-regex) pattern published its match as the caller's
`$/` on one dispatch path, while Rakudo leaves `$/` untouched. A `while $x ~~ m:c/.../`
loop that substituted into a copy of its target therefore had its continuation
position replaced by a match against the *modified* copy, and stopped one
placeholder early. The string-pattern path now publishes `$/` only for a closure
replacement, which reads it while running. Found via the HTTP::Server::Logger
suite (`t/00-use.t` now passes).
