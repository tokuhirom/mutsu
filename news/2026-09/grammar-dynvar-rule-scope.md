# Grammar rule dynamic variables are scoped to rule invocations

Grammar-rule `:my $*VAR`, `@*VAR`, and `%*VAR` declarations are now installed
when their rule invocation is matched and restored when that invocation ends.
They are no longer eagerly initialized from every rule in the grammar before
matching starts.

This prevents a declaration in a losing proto candidate from leaking into a
sibling candidate or its action, while preserving per-match values and nested
rule shadowing. It fixes #8148 and adds a regression test in
`t/grammar/grammar-dynvar-rule-scope.t`.
