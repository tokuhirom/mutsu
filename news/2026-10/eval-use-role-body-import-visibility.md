# A role body sees its own module's imports when composed inside an EVAL

A role body runs at composition under the importer's frames, so the compile-time
and run-time lookup of the routines the role's module `use`d read the EVAL's (or
the caller's) import sets instead of the module's own. A module's mainline now reads
its own unit's top-level `use`s, and a routine of another compunit called from EVAL'd
code reads its own unit's sets too (#14481). Native::Packing's `t/00-readme.t` now
passes 2/2 like raku.
