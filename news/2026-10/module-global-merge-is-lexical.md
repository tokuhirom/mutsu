# A module's declarations are visible only where it was loaded

Rakudo merges a module's own package-scope declarations into the lexical scope
that ran its `need`/`use`/`require`, not into the whole program. These are its
classes, `our` subs, constants and packages. So a block-scoped `need` exposes
them only inside the block, and an importer never sees what the module itself
`use`d. mutsu published them process-wide, so every one of them stayed
resolvable everywhere once any code anywhere had loaded the module.

ADR-11136 keeps the stores global and adds visibility on top of them. A module
load records which bare names its own body published. A `need`/`use` merges the
module either into the block that ran it, as an env key that ends with the block
and that a closure created there keeps, or into the whole compilation unit for a
top-level statement. Bareword and `::('...')` lookups, bare routine calls, the
`EVAL` undeclared-name check and #7797's qualified-name gate consult the merges.
Instances, method dispatch and the module's own code are unaffected (#11136).
