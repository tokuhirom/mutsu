# An interpolated variable name continues through an apostrophe

A Raku identifier may contain `'` (like `-`) between letters, and rakudo reads the whole name in a
double-quoted string: `"x $don't y"` interpolates `$don't`, and `"$src's"` is a compile-time
`X::Undeclared` for `$src's`. mutsu's interpolation scanner stopped at the apostrophe, so it read
`$don` (an undeclared-variable error) and silently printed `1's` for `"$src's"` (#18326). The
scanner now continues the name through `'` or `-` when a letter or `_` follows, as the code
parser does; a trailing apostrophe (`"'$src' is"`, `"it's $src'"`) still ends the name.
