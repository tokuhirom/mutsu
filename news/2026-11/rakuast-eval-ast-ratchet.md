# A CI ratchet for the write direction of the RakuAST layer

`MUTSU_RAKUAST=convert` was the only whole-corpus check of lowering (`EVAL($ast)`), and it lowers
inside the parser, not through the public API. `scripts/ast-eval-driver.raku` now runs a `t/` file as
`EVAL(slurp($file).AST)`, `scripts/run-t-ast-eval.sh` wraps it with the usual timeout and
flaky-retry engine, and `scripts/rakuast-frontend.sh eval-check` runs the files pinned in
`ci/rakuast-ast-eval-passing.txt` (7932 of the 8722 files of the convert list, 91%) as a CI step. The
list only grows (`eval-update`).

The 786 files that do not pass are real `EVAL($ast)` defects the convert mode could not see, including
a Rust panic (`an import region's env was replaced wholesale`) when a lowered tree holds a `use`;
they are filed by cause on #15759. Deleting the convert mode (step 2b) waits for that panic to be fixed.
