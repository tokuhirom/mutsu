# A `need`-loaded module keeps its own imports, and its stash omits them

A module loaded through `CompUnit::Repository::FileSystem.need` inside a bare
block lost the routines its own `use` statements imported as soon as the block
exited, so a later call into it died with `Unknown function`. The load now
records those routines as the module's own, like a plain `use` does.

A routine that a module imports with a nested `use` is also no longer reported
as a member of the module's package stash, matching Rakudo (`Foo::.keys` lists
only what `Foo` declares). Template::HAML's `render-cached` and
`load-from-cache` depend on both (t/0477, t/0460).
