# A bare file's exports no longer belong to the unit module that `use`d it

A `unit module A` whose body `use`s a bare-file module (NativeCall) recorded that
file's `is export` routines as exports of `A`, so a block that `use`d `A` listed
NativeCall's routines in `MY::`. Export bookkeeping now goes through
`declared_unit_module_here`, which skips a bare file nested in a unit module.
