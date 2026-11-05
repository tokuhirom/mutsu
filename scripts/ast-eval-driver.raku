# Runs the program in the file named by the first argument as
# `EVAL(slurp($file).AST)`: it is parsed to RakuAST nodes and then lowered back
# and executed, the write direction of the RakuAST layer. Used by
# `scripts/run-t-ast-eval.sh` (RakuAST C1, #15759).
use experimental :rakuast;
EVAL(@*ARGS[0].IO.slurp.AST);
