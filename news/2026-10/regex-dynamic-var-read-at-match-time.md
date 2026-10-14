# Dynamic variables in a regex are read where the atom matches

`<$*dyn>` and a bare `$*dyn` in a pattern were read when the pattern was parsed: an unbound or still
undefined `$*dyn` in a branch that is never reached (`<?{ $*dyn.defined }> <$*dyn>`) died with
"Variable is not declared" or `INTERPOLATE_ASSERTION`, and a `:my $*dyn` lexical named inside a
lookaround body (`<!before $*dyn>`) was replaced by the caller's value of that name. `<$*dyn>` is now
a match-time assertion, `:my $*dyn` is recorded as a name the pattern declares (twigil included), and
lookaround bodies inherit the declarations of the enclosing pattern. Found with the Rainbow
distribution's comment tokenizer.
