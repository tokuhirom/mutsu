# A module required on a worker thread is loaded once for the whole program

The 39 tables that record loaded compunits (`ModuleWorld`) are now each a
`WorldCell` shared by every thread, instead of a per-thread copy forked at
`start` (ADR-17216, slice 3). A `require` on a worker records the load in the
one program world, so a later `require` of the same module on the main thread
finds it loaded and no longer runs the module's mainline a second time, which
matches rakudo.

Lookups that used to hand out a reference into a forked table
(`module_scope_lexical`, `module_imported_lexical`, `toplevel_package_symbol`)
now return owned values read through the cell's thread-local snapshot, and no
write guard on a world table is held across a nested access to the same table.
