# A module has its own `$=finish` data section

A module's `=finish` text used to be dropped at parse time, so `$=finish` was `Any` inside
it and `for $=finish.lines { ... }` (the data-table idiom used by `Locale::Codes::Country`)
died at load. Each module load now establishes its own `$=finish`, keeps it as a unit
lexical for the module's routines, and gives the importer back its own value afterwards.
