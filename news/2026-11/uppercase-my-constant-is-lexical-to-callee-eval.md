# A callee's `EVAL` no longer reads its caller's uppercase `my constant`

`sub callee { EVAL q[SECRETC] }` called from `sub outer { my constant SECRETC = 5; callee() }`
returned `5`; `raku` reports it as not visible (the lowercase spelling was already hidden). The
term key of an uppercase constant (`\SECRETC`) was not a lexically scoped name, so a top-level
routine's reflective lookup did not stop at its own scope. Making it one broke a loaded module's
routines, which read the names they imported (`class N-GE is export` of a `use`d module, then
`my CArray[N-GE] $x` in a module sub) through the caller chain; `has_type` now reads an imported
alias through `term_binding`, which answers from the compunit's own imports.
`my subset` and `$?ROLE` still leak (#16099).
