# `temp Type.method = value` after a `sub EXPORT` import

A unit that imports through a `sub EXPORT` hook (Red's `use Red`) parses every
bareword as an imported term. Since the executing parse stopped wrapping those
terms in a spelling marker (#18281) and builds the typed
`ParsedExpr::ImportedTerm` node instead, the `temp` statement no longer
recognized such a word as the type invocant of `temp Type.method = value` /
`temp Type.^meta = value`. The statement fell through to a plain expression,
and `temp` ran as a call to an unknown function: Red's
`t/023-metamodel-model` died with `Unknown function: temp` at
`temp Bla.^table = "temp_bla"`.

The `temp` statement now reads the type name from the imported-term node too,
and Red's `t/023-metamodel-model` passes all 66 tests again (nightly sweep
#111 regression, #18377).
