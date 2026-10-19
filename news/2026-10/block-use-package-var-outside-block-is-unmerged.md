# Outside a block's `use M`, `$M::x` is a fresh variable, not the module's

A `use` inside a block merges the module's packages into that block only.
After the block, rakudo resolves `$M::x` through the unit's own `GLOBAL`,
which never merged `M`. The name is a fresh, autovivified `GLOBAL::M::x`: it
reads `(Any)`, assigning it leaves the module's `our $x` alone, and
`%M::h<k> = v` or `@M::l.push(...)` autovivify fresh containers (#13942).

mutsu answered the module's variable everywhere, because the BEGIN-time
preload loads a block's module for the whole unit. Worse, a write from
outside the block changed what the module's own subs saw.

A qualified variable whose package came from a module that is not merged into
the running code is now sent to a separate store. That covers `$M::x`,
`@M::x`, `%M::x` and `$GLOBAL::M::x`. The module counts as merged when a
block merge of it is live, when the running unit merged it at its top level,
or when the running code is the module's own. The redirect lives at the
by-name chokepoints: the env read and write helpers, the package-variable
store accessors, and the container write path behind element stores and
mutating methods. So reads, assignments, increments, element stores and
`push` all agree.
