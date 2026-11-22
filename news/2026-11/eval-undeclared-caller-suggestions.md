# An undeclared variable in `EVAL` suggests the caller's lexicals

`my $ab; EVAL q[say $ac]` dies with rakudo's
`Variable '$ac' is not declared. Did you mean '$ab'?`. mutsu's EVAL check
offered only names declared inside the EVAL'd code, so the caller's `$ab` was
never suggested (#18747).

The candidates for the suggestion now include the variables the EVAL'd code
can see. These are the visible environment, plus the static scopes around the
`EVAL` call unless `context =>` moved the code to another frame's scope, which
is what `Test`'s string `throws-like` does. Twigil, package-qualified and
internal names are left out.
