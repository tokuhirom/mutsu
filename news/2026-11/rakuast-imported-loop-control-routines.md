AST EVAL now preserves calls to imported routines named `last`, `next`, or `redo`. Their parsed routine binding survives RakuAST conversion instead of being lowered as core loop control.
