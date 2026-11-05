# The JIT's operator-site shim answers a known-unbound site first

`OpCode::LexImportBuiltinOp` (a built-in operator used in a block whose `use`
may import candidates for it) is compiled to a call of the
`lex_import_builtin_op` shim. For a site that already found its operator
unbound under the current import-set generation, that call went on to run the
whole interpreter handler (`exec_lex_import_builtin_op_site`) inside a panic
boundary just to return "run the op as itself".

The test "no import set binds an operator, or this site remembered the miss for
this generation" is now one inlined method,
`Interpreter::lex_import_site_known_absent`, used by the interpreter handler and
checked first in the shim, before the panic boundary and the handler call.

Measured with `use Cro::HTTP2::RequestParser` loaded and a `$s += %h<a>` loop
(profiling build, loop-only callgrind, slope 3,000 -> 9,000 iterations): 4,537
-> 4,421 Ir/iteration (-2.6%). The handler's ~74 Ir self cost per site
execution is gone from the profile; the shim itself is ~40 Ir.
