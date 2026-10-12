# RakuAST represents bare `done` calls

RakuAST conversion now recognizes `done` as a core call when it appears without
parentheses. Lowering preserves the compiler's special `done` marker, so
expression-position uses round-trip while retaining supply completion
behavior.

The RakuAST bareword-resolution test checks the node shape, and the supply
completion tests exercise the behavior after lowering.
