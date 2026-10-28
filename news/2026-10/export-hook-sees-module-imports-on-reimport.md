# A module's `sub EXPORT` still sees its own imports when a nested module re-imports it

`NativeLibs`' hook reads `&trait_mod:<is>.candidates` to hand NativeCall's native `is` trait on. On
the second and later import the hook ran against a registry from which the loading module had hidden the
previous importer's `GLOBAL::` aliases, so it saw only CORE's single candidate and died with
`No such method 'dispatcher' for invocant of type 'Any'` (Red 0.2.7, `require ::("Red::Driver::SQLite")`
inside a block that `use`s Red).

A module with a `sub EXPORT` now remembers the `GLOBAL::` candidates its own body added (its imports),
and the re-run of the hook puts them back for its duration, as it already did for the module's own
package-less multis. Red `t/44-error-without-id` and `t/65-patrickb` now match rakudo.
