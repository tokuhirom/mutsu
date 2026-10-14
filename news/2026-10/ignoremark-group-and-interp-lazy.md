# Nested `[:m ...]` groups and `:m` interpolations enter their ends lazily

A `[:m ...]` / `(:m ...)` group inside a plain rule used to compute every end of
its body up front over the mark-stripped subject, so a code block in it ran at
ends the match never entered (`regex r { [:m a+ { @log.push: $/.to }] }` logged
`2 1` where Rakudo logs `2`). The group's body now runs as a frame of its own
over the original subject (its markless compile when the subject has marks), as
a rule call does. An interpolation (`$x`, `@alts`, `<$p>`) inside a whole-`:m`
rule is likewise entered as a markless frame instead of declining the markless
compile. `@( ... )` / `<{ ... }>` in a `:m` rule still use the eager path
(#13216).
