# The RakuAST frontend cause survey reports interpreter crashes

`scripts/rakuast-frontend.sh causes` filed a file the interpreter died on (a
signal exit such as 139) under `DIFF`, and its output suppression left the
shell's "Segmentation fault" line without a file name. A signal exit is now its
own `CRASH` row with the exit status, and the run ends by naming those files, so
a crash seen during a survey (#12499, one segfault in 550 files, not reproduced
in nine passes of the same commit at 12-16 jobs nor in five passes of current
`main`) is isolated by the next run that hits it.
