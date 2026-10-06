# An enum in a `package` body no longer replaces the unit's value

`EVAL('package P4 { enum Status <S1 S2> }; 42')` answered the enum's `Map` instead of `42`
([#12036](https://github.com/tokuhirom/mutsu/issues/12036)). `RegisterEnum` pushes the declaration's
`Map` (the value of `enum` in expression position) and the statement-sequence compilers pop it when the
statement is not the sequence's value, through `stmt_nets_a_stack_value` (ADR-0052). The body of a
`package` / `module` block compiled its statements with `compile_stmt` alone, so the `Map` parked at the
frame's stack base and won over the real tail value of the enclosing unit; the same held for an `INIT`
or `ENTER` body (`EVAL('INIT { enum E <A B> }; 11')` answered the `Map`).

`compile_stmt_discarding_value` is `compile_stmt` plus the pop `stmt_nets_a_stack_value` asks for, and
the non-unit package body, the package-runtime body (`class`/`role` bodies registered in place) and the
`INIT`/`ENTER` phaser body use it. A `react` body was already correct and is left alone.

Test: `t/modules/eval-package-enum-result.t`. Closes
[#12036](https://github.com/tokuhirom/mutsu/issues/12036).
