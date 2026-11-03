# `VAR .= name` carries the origins rakudo gives its parts

In `.AST` of `$f .= uc`, `@a .= map: { ... }` and `$f .= substr(1, 2)`, only the
statement and the `ApplyDottyInfix` had a range. The variable, the `.=`
operator (`DottyInfix::CallAssign`), the `Call::Method` from its name to its
end, its `Name` and its `ArgList` (empty right after the name, just inside the
parentheses, or from the first to the last argument) now carry the range
rakudo gives them. `call_origins::dotty_assign` reads them off the source text
and shares its method-call placement with the postfix `.name(...)` form
(`place_call`); a `.=` whose left side is not a plain variable is left
alone. On a 54-file sample of the differential, nodes rakudo has and mutsu
lacks went from 2501 to 2365, with no new mismatches. Part of #15780 (and of
`locate-node`, #14916).
