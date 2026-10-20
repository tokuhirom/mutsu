# `Mu.Callable` and Grammar's `locprepost`/`typed_exception`/`SETFAIL` under 6.e

Language revision 6.e adds `Mu.Callable($method)` (the method object
`.^find_method($method)` answers) and Grammar's `locprepost` (the text around
the cursor, `<BOL>`/`<EOL>` when empty), `typed_exception` (a `Failure` of an
`X::Comp` built with the line, position and surrounding text) and `SETFAIL`.
They are `REVISION_E` late rows, so under 6.d `.^can` denies them as in Rakudo.
