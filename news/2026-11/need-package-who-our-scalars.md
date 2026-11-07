# `Pkg.WHO` of a `need`ed package lists its `our` variables from any unit

`ModA.WHO.keys` taken inside another module's routine omitted the `need`ed package's unexported
`our` scalars (`$v1`), while the unit that `need`ed it saw them: the stash's `our`-store fallback
read through `get_our_var`, which redirects a package the running unit did not merge to the empty
store a qualified `$ModA::x` read uses. A package object's stash belongs to the package, so the
fallback reads the `our` store directly. CustomImporting `t/02-import-rt.rakutest` is 23/23 and
`t/01-import.rakutest` reaches its last container test.
