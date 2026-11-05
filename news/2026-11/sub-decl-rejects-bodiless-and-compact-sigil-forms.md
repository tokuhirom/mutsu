# `sub f($a, $b);` and `sub f($$$$)` are rejected like rakudo

mutsu accepted two spellings rakudo refuses at parse time: a bodiless `sub add($a, $b);` as a
forward declaration, and the Perl 5 prototype form `sub f($$$$)` for four anonymous parameters.
They now raise `X::UnitScope::Invalid` ("a unit-scoped sub definition is not allowed except on a
MAIN sub") and `X::Syntax::Malformed` ("Malformed parameter"). A forward declaration is spelled
`sub add($a, $b) {...}` and anonymous parameters are comma-separated. `sub MAIN(...);` keeps its
semicolon form. Closes #16210.
