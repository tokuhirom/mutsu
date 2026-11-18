# Hook modules and a module's own `use`s load at compile time

ADR-17558's second slice brings two more kinds of `use` to compile time.

A module that computes its imports in a `sub EXPORT` hook now loads when the
parser reaches its `use`, like every other module, and the hook runs then —
once, with the `use`'s literal arguments (`use M <a b>`). Its import in
position installs the map that run returned instead of calling the hook a
second time, so a hook that counts or reports its runs does so once per `use`,
as on rakudo.

A module's own `use`s load while the module itself is compiled, so a module's
source can use an operator another module only defines while loading (Moneys'
computed postfixes) from the module it actually loaded, as the importing
program already does since the parse-time probe was retired. What those loads register still belongs to the
module and not to whoever loaded it: classes, aliases and package visibility
stay where they were when the loads ran in the module's prologue. A `use`
cycle between modules is now reported at compile time, as rakudo reports it.
