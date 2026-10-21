# EVAL CHECK-time checks read the pre-run's import scope, not the module source

The type-name checks of an `EVAL`'d unit (`eval_declared_types`) no longer
harvest the source text of a module its pre-run already `use`d: the names come
from the interpreter's own import scope, so what the module declares but does
not export is no longer accepted (#13441). The source harvest remains for a
`use` the pre-run does not cover (one nested in a block, or after a `use lib`
or BEGIN).
