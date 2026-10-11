# Stringifying a role type object no longer puns the role

`R.Str`, `R.gist`, `R.raku` and `R.perl` on a role type object are answered by `Mu`, as in Rakudo,
instead of punning the role first. A role that still carries a `method m { ... }` stub (such as
`Map::Agnostic`) therefore no longer dies with "Method 'm' must be implemented" when only its type
object is stringified. Found with the `Map::Agnostic` test suite, which now passes 20/20 by direct run.
