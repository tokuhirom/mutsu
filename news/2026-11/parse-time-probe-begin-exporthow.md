# A package declarator a module merges at BEGIN time reaches its importers

A module can merge another unit's `EXPORTHOW` into its own from a BEGIN phaser,
as CompUnit::Util's `re-exporthow` does. An importer of that module can then
declare types with the merged declarator (`my pokemon pikachu { }`). mutsu
learns a module's declarators from a static scan, which cannot see a merge
done by a call, so the importer died at parse time.

The parse-time export probe (ADR-0124) now also runs a module whose BEGIN
calls a routine of a module it loads, and reads back the declarators its load
recorded (ADR-17557). At run time, `merge-symbols` of a unit's
`export-how-package` into a module's `EXPORTHOW` records that unit's
declarators as the module's.

A declarator a `use` introduces now also ends with the block of the `use`, as
in rakudo. Two imports of one declarator into the same scope still conflict.

CompUnit::Util 0.7.0's `t/02-re-export.rakutest` (10/10) and
`t/05-rexport-everything.rakutest` (4/4) now pass, matching rakudo (#17557,
#15371). This was route A; loading a module when its `use` is parsed, as
rakudo does, is the follow-up (#17558).
