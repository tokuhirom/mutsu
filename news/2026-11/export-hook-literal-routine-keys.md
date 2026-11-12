# Routines a `sub EXPORT` hook names by a literal key parse as listops

A module whose `sub EXPORT` returns `Map.new('&config' => -> &block { ... })`
(built directly or by a helper the hook calls, as Red's migration artifacts
do) now makes `config` a known routine to the importer's parse, so
`config { ... }` is a call with a block argument rather than "Two terms in a
row". The parse-time approximation already covered operator and `term:<>`
keys; it now covers plain routine names too.
