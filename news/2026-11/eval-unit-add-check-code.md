# `$*CU.add-check-code` works inside an EVAL

An EVAL is a compilation unit of its own. `$*CU` and `add-check-code` inside
its BEGIN now address that unit (the EVAL'd string or the EVAL'd RakuAST tree,
wrapped in a `RakuAST::CompUnit`), and the closures it queued run at the
unit's CHECK phase, after its CHECK phasers and before the body, as for the
main program. Before, the call was silently dropped because the only unit on
record was the already-compiled main program.

This takes 65 more test files through the `EVAL(.AST)` ratchet
(`ci/rakuast-ast-eval-passing.txt`): the 60 files from the last sweep that
already passed, the new test, and the lookup-resolution family, which reads `is-resolved`
from a CHECK-time closure of an EVAL'd tree. Test: `t/lang/eval-add-check-code.t`.
