# A class attribute default sees the constants its module imported

`has @.d = SUPV`, where `SUPV` is a `constant ... is export` the module's own top-level
`use` imported, evaluated to the constant's name (`["SUPV"]`) instead of its value when the
class is a `class Name { }` block of a module file. Such a `use` binds the term in the
module's mainline pad, and the load moves it into the compunit's lexicals; a routine reads
it from there, but a class attribute default runs in a block frame of the declaring unit
and only consulted the module tables keyed by the exporting module. `term_binding_sym` now
also reads the running unit's compunit lexicals, the store every by-name variable read
already uses. `MCP::Server` 0.7.1: 22 of 29 test files that died in `MCP::Server.new` now
run, and 24 of the 29 match Rakudo's assertion counts. Fixes #14662.
