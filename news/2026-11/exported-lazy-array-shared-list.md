# An exported lazy `our @A` is one list for the exporter and its importers

`unit module M; our @A is export = 0, 1, -> $a { ... @A[$k] ... } ... *;` gave wrong values
(Math::Sequences A000311/A000594/A000793): the importer's `@A` was a deep copy of the lazy
list, with its own generation cache, so indexing it ran the generator, whose reads of the
module's `@A` started a second, independent generation sharing the generator's `state`
variables. `ShareLazyArrayInit` now flags the initializer as the array's list once, before it
is duplicated for the local slot and the package store, and the local/bind stores keep an
already-flagged list instead of copying it again (`lazy_array_value_shared`), so every store
and the import hold one list.
