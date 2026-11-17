# A rule body's `$var` is read at match time, and `% $sep` is a variable

A plain `$var` in a token or rule body is now read when the cursor reaches
it, in the rule's declaring scope (#14510, ADR-12917 direction 2):

```raku
my $x = "a";
grammar G { token TOP { x { $x = "b" } $x } }
say so G.parse("xb");   # True (was False)
```

The body render (`eval_token_def`) used to splice every visible variable's
value into the pattern before the match started. It now splices only the
rule's own parameters (`interpolate_bound_regex_params`). Other variables
stay in the text, and the candidate parse declares them as match-time
lexicals (`parse_regex_for_match`). The declaring scope is installed for the
match window since #17872.

The separator of `%` / `%%` also takes a variable as one atom
(`split_first_atom`). Since plain scalars became match-time atoms,
`[\w+]+ % $sep` had been reading the separator as the `$` end anchor
followed by `sep`, and failed to match.
