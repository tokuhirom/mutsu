# The `EVAL($ast)` ratchet grows by 14 files

Re-running the 76 `t/` files that pass under `raku`'s `EVAL(slurp($file).AST)` driver but were not in `ci/rakuast-ast-eval-passing.txt` shows 14 more of them now pass on mutsu too, after the EVAL-unit work of the last PRs (`add-check-code` and `$*CU` inside an EVAL, `MY::` of an EVAL'd unit, dead-branch `our`, list-associative chains): the lookup-resolution family, the `RakuAST` declaration / accessor files, and others. They join the ratchet, so a regression of any of them fails CI. No source change.
