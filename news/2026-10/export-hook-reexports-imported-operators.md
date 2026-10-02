# A `sub EXPORT` hook that re-exports an import now teaches the importer's parse its operators

A module whose `sub EXPORT` returns `Map.new(Other::EXPORT::DEFAULT.WHO.pairs)` exports whatever
`Other` exports, but the parse-time module scan only approximated hook exports from the module's own
unit-scope routines. An importer using the re-exported operators (Qwiratry's `↱`, `⮳`, `⮷`) died with
"Two terms in a row". The scan now also records the exports of every module a hook module `use`s or
`need`s and merges them into the hook module's export set. `Qwiratry::Test`'s `t/00-use.rakutest`
passes; the `need` run-time residue is tracked in #10683.

Regression test: `t/modules/import-export/export-hook-reexports-imported-operators.t`.
