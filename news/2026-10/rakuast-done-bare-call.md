# RakuAST represents bare `done` calls

RakuAST conversion now recognizes `done` as a core call when it appears without
parentheses. This lets expression-position uses of `done` round-trip through
RakuAST while retaining the existing supply completion behavior.

The RakuAST bareword-resolution test checks the node shape, and the supply
completion tests exercise the behavior after lowering.
