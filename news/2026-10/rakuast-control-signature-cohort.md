# RakuAST retains control statement signatures

Typed, sigilless and destructuring `orwith` parameters, pointy `unless`
clauses, `else` signatures and destructuring `while`/`until` bodies now retain
their written form across the RakuAST frontend. Parsing and lowering share
their executable expansions, including the original false condition passed to
an `unless` or `else` parameter. An `orwith` condition is evaluated once and
its saved value also supplies a trailing `else`.

Conditional and while/until node constructors now accept the corresponding
condition and block fields, continuation clauses and labels. Initializer
parentheses retain the assignment semantics of multiple regex matches,
including their use as aggregate pointy arguments.

EVAL now performs its existing static source checks before reporting a refused
RakuAST conversion.
Undeclared names and invalid parameter types therefore retain their typed
compile-time diagnostics even when the invalid source cannot be converted.
Rejected units do not execute their ordinary statements; valid units still
pass through conversion, lowering and validation before execution. Imported
caller terms remain visible across nested parses and cold module loads.
