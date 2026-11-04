# The EVAL($ast) ratchet lists 82 more t/ files

Re-measuring the 393 `t/` files outside `ci/rakuast-ast-eval-passing.txt` as
`EVAL(slurp($file).AST)` found 82 that now pass (the families lowered where the
parser builds them, and earlier fixes); they were stable over two runs and are
added, 9270 of 9581 files are pinned. Of the 311 that do not, 102 read `$?FILE`
and friends and can never join; the other 209 are triaged by cause in a filed
issue, including one debug-build env/slot assertion panic.
