# `require` no longer re-runs a loaded module that declares only types

`require` decides whether a module it is asked for is still loaded with
`module_still_registered`: a package of the module's own name, or a routine
under it. A file that declares only types named apart from the file passed
neither check, so a `require` of it re-ran its whole body even though a
`use` had already loaded it. rakudo loads a compunit once.

Red's test library is such a file: `TestRed.pm6` declares `model Zub` and
`model Foo`, and `Red::Column.model` resolves a referenced model with
`::($name)`, falling back to `require ::($require)`. Each re-run applied every
`is relationship` trait again, mixing the relationship role into the same
attribute two and three times, so the outermost mixin's relationship block
belonged to another attribute and `Zub.foos.create(...)` died with
"No such method 'zub-id' for invocant of type 'Zub'" (Red t/017-pp).

A loaded module now also counts as registered while any package-scope name
its load published (`module_name_providers`) is still a known package.
