# The `EVAL($ast)` ratchet lists 135 more `t/` files

Re-measuring the 410 `t/` files the ratchet did not list as `EVAL(slurp($file).AST)` found 135 that
already pass (each run twice on a debug build), so `ci/rakuast-ast-eval-passing.txt` pins them: 9408
of 9581 files. The remaining failures are triaged by cause on #16356 (the Pod group of #14880, files
that start with `use v6.e`, tests that read `$*PROGRAM`/`$?FILE` of the driver, and a few open defects).
