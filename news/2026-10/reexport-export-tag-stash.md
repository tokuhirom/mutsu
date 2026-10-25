# Binding `UNIT::EXPORT::{$tag}` to another module's `EXPORT::{$tag}` re-exports that tag

`NativeLibs` re-exports everything `NativeCall` exports with
`CHECK for NativeCall::EXPORT::.keys { UNIT::EXPORT::{$_} := NativeCall::EXPORT::{$_} }`.
mutsu treated the keyed stash assignment as a plain hash store, so a file that only
said `use NativeLibs` (DB::Pg, and through it every Red test file) never saw `size_t`,
`CArray` or `is native`, and printed a "Not an accepted NativeCall type" warning for
each native routine it declared.

The assignment now registers the source module's exported routines and constants under
the loading module's tag (`reexport_module_tag`); a re-exported constant keeps its value
with the owning module. A module whose mainline also binds the same names through its
own `use NativeCall` no longer reads `Nil` for them: the dynamic import's restore loop
used to clear the pad binding before the module's unit lexicals were taken over.
