# RakuAST: indexed bind source forms

Comma-separated indexed slices and semicolon-separated dimensions now retain
their distinct RakuAST `SemiList` structure across conversion and lowering.
Semicolon binds also use the existing deep lvalue operation, fixing leaf
selection and source aliasing in ordinary execution. Focused RakuAST and
ordinary execution tests cover the structures and behavior. Part of S10 of
#7564 (ADR-10723 §6.9).
