# A `use`d module's packages show in `MY::`, not in `GLOBAL::`

Rakudo installs the packages a `use`d module declares lexically, in the
importing scope's pad. mutsu kept them in its process-wide package tables, so
the stash views disagreed with rakudo: `GLOBAL::<M>:exists` was `True` after
any `use M`, and a block's `{ use M; MY::<M>:exists }` was `False`.

Now `GLOBAL::` (and `OUR::` at the root) lists only the program's own
packages and routines. It leaves out a loaded module's top-level packages, a
package-less module's routines, imported routine aliases and an `EXPORT`
package only modules exported into. The `MY::` of the scope that ran the
`use`, `need` or `require` lists the merged module's top-level packages, and
a nested block, another routine or the code after the block does not. A
block with a `need` or `require` now counts as importing for `MY::`, as one
with a `use` already did (#13802, ADR-8751).
