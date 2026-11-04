# `nqp::setwho` replaces a type's stash

`nqp::setwho($type, $stash)` was the last missing op of the object-model family (#11499). It
files `$stash` under the type in `Registry::who_overrides` (a package by name, a `create_type`
type object by its identity), `dispatch_who` answers that very object for `.WHO` and
`nqp::who`, so `$type.WHO === $stash`, and the op answers the type, as Rakudo does. With it
every op of #11499 is implemented; the issue closes with this change.
