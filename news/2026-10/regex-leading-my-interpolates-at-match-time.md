# A leading `:my` variable interpolates its current value

`/ :my $n; { $n = "x" } $n .* /` never matched: a regex's leading `:my`
declarations are evaluated into the env before the rest of the pattern is
parsed, and that rest no longer knew `$n` was a regex lexical, so the bare
`$n` was spliced in with the value it had when the match started — `Nil`,
which interpolates as a never-matching atom, or the initializer, ignoring
any code block that assigned the variable since (#13004).

The rest of the pattern now carries the hoisted scalar names in the same
declaration block a rule body uses for its parameters (`regex_rule_params`),
so `$n` lowers to the match-time interpolation that reads the variable when
the cursor reaches it, as rakudo does. YAMLish's `root-block` uses this
shape (`:my $new-indent; <?before … { $new-indent = ~$<sp> }> $new-indent`).

A `$name` interpolation of a regex lexical is now also an ordinary atom to
the structural parser: `/ {} :my $q = 'a'; $q+ b /` applies the `+` to the
interpolation, where the parser used to push the atom and skip its
quantifier.
