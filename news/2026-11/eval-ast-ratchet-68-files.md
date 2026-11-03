# 68 more t/ files run as `EVAL(slurp($file).AST)`

After the `use`-import work of #15873 and the unit-level checks of ADR-15760
slices 1a/1b, a cold run of `scripts/rakuast-frontend.sh eval-update` over all of
`t/` passes 9070 of 9414 files in the write direction. 68 files joined
`ci/rakuast-ast-eval-passing.txt`. The remaining 344 are a long tail (no single
cluster larger than a handful); the ones that still report `Undeclared routine`
or `Undeclared name` name routines or types that a `use`d fixture module
defines, and are tracked on #15759.
