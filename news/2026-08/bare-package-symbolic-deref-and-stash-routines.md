# Bare package symbolic dereferences and lexical routines

Bare pseudo-package symbolic dereferences now parse as scope-selecting symbolic
lookups.  Forms such as `MY::("$x")` and `GLOBAL::("$name")` therefore follow
the same runtime lookup model as their sigilled neighbours instead of being
rejected as incomplete qualified identifiers.

The `MY` and `LEXICAL` pseudo-stashes now also expose visible registered
routines under their `&name` keys.  A declaration such as `sub twice($x) { ... }`
can consequently be retrieved and called through `MY::{"&twice"}` while still
respecting the routine's lexical lifetime.
