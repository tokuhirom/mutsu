# An undeclared variable in `EVAL` suggests the caller's lexicals

`my $ab; EVAL q[say $ac]` dies with rakudo's
`Variable '$ac' is not declared. Did you mean '$ab'?`. mutsu's EVAL check
offered only names declared inside the EVAL'd code, so the caller's `$ab` was
never suggested (#18747).

The candidates for the suggestion now include the lexicals of the scope that
calls `EVAL` (the calling chunk's `eval_scope_names`), which the EVAL'd code
can see. Internal names are left out.
