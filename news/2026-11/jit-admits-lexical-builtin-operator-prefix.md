# A hot loop in a unit with a `use` runs native

In a unit that has a `use`, every built-in operator carries an `OpCode::LexImportBuiltinOp`
prefix (ADR-8751 stage 2), which the JIT's range scan rejected. So a hot `for` body such as
`$t += $k.m(1)` stayed on the interpreter as soon as the file said `use Anything`, while the
same loop in a file without a `use` was compiled. The prefix is now lowered the way
`LexImportCall` is: a shim runs the operator op itself with the lexically bound candidates in
scope (status `SKIP`, the generated code continues after the operator) or reports that no set
binds the operator, and the operator then runs as itself in the native body.

Callgrind slope (profiling build, warm, `use Cro::HTTP2::RequestParser`, 40000 -> 100000
calls of `$k.m(1)`): 15.5K -> 14.6K Ir per call; the same program without the module costs
14.0K. With #16399 the loaded-vs-bare gap of #9170's method-call row went from 5.4K to 0.6K.

Test: `t/vm/scope/jit-loop-with-lexically-imported-operator.t` (hot loops with and without
operator candidates bound by a block-scoped `use`).
