# `<@Pkg::name>` resolves an imported module's `our constant @name`

A regex array interpolation naming a package-qualified `our constant @name` of a module loaded
with `use` found no array and failed with "Null regex not allowed". The lookup now also consults
the qualified env entry, so e.g. Bitcoin's `<@Base58::alphabet>+` subset works.
