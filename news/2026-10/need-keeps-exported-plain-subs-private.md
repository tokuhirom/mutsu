# `need` no longer imports a package-less module's exported subs

A `need` loads a module without importing anything. For a module with no
package of its own, mutsu still left every `sub ... is export` at its shared
`GLOBAL::` key, so the loading scope could call it by name (#11080). The
`CompUnit::Repository.need` path was already right, because it suppresses
the export registration altogether.

Under a `need`, the module's exported package-less routines are now moved
into its private table, exactly like its unexported helpers. The exports are
still registered, so the module's `EXPORT::<tag>` stash keeps them, and a later
`use` of the same module re-installs them from those stash aliases. The multi
and proto half of this was #11004.
