# `CompUnit::Handle.export-package`

`$comp.handle.export-package` answers the loaded unit's `EXPORT` stash (`ALL`, `DEFAULT` and each
tag), as Rakudo does (#11002). It is a row of the method table, owned by `CompUnit::Handle` and
reached through its owner, not another arm of the instance slow path. For that,
`CompUnit::Repository::*.need` now loads the module the way the `need` statement does: its
`is export` routines are registered, so the `EXPORT::<tag>` packages are populated, but nothing is
imported into the caller's scope.
