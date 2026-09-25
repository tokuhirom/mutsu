# `$q + 1` on an undefined `$q` now warns, naming the variable

`my $q; my $r = $q + 1` was silent. rakudo prints `Use of uninitialized value
$q of type Any in numeric context` and numifies the operand to 0. Because
nothing was raised, a `CONTROL { when CX::Warn { ... } }` had nothing to
catch. CodeUnit's warning test is built on exactly that (#9359). Only prefix
`+`/`-` and the string-context forms warned.

The numeric infix ops `+ - * / % **` share one operand coercion,
`coerce_numeric_bridge_pair_strict`. The comparisons `== != < <= > >=` each
call the non-strict bridge. Both now pass every operand through
`warn_uninitialized_numeric_operand`. It raises the warning as a resumable
`CX::Warn` for any type object other than `Mu`, the concrete numeric types
(which already die with `X::Numeric::Uninitialized`), and classes with their
own `Numeric` method. It then resumes with the type's numeric zero, so
`Complex + 1` is `1+0i` as in rakudo.

Naming the variable needed the source name. By the time the op runs, the
operand is a bare `Any` with no container. So the compiler now records the
operand variable names of each numeric infix op in a sparse side table
(`CompiledCode::numeric_operand_names`, keyed by op index). The interpreter
loop notes which op it is running (`numeric_op_site`). The cold warning path
looks the name up there, so the hot path pays two stores and no lookup. A
readonly parameter is left unnamed, as in rakudo: it has no container.

Two gaps are left, and in both the warning still fires, just without the
name. Element operands (`@a[0] + 1`, `%h<k> * 2`) are not named. JIT-compiled
ops, which bypass the interpreter loop, carry no op index.

The regression test is
`t/types/string/uninitialized-numeric-context-warning.t`.

CodeUnit's own test still fails after this. Its `CONTROL` block also has a
`default` clause, and with one, `.resume` of any op-raised warning returns
`Nil` from the routine. That is a separate bug, #9425.
