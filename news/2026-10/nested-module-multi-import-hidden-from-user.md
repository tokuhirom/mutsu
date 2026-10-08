# A nested module's imported proto/multi family is hidden from the using scope

A module that `use`s another module for its own purposes no longer leaks that
module's exported `proto`/`multi` family to the scope that `use`s the outer
module through a symbolic `::('&name')` lookup (GH #12161). Rakudo leaves such
a name undeclared, and so does mutsu now.

`scope_unit_multi_families` used to skip every family whose declaring unit also
registered it under a package key (`unit module M`), so those families had no
per-unit import record and were visible everywhere through their `GLOBAL::`
alias. They are scoped to their declaring unit like package-less families, and
an import adds the importing unit. This also restores the last assertion of
`t/modules/block-use-keeps-nested-module-imports.t`, which covers NativeCall's
`&nativecast` and kin the same way.
