# `nqp::repeat_while` / `nqp::repeat_until`, and `Dateish` on `Date` / `DateTime`

The post-test loop forms `nqp::repeat_while(cond, body)` and
`nqp::repeat_until(cond, body)` died with `Unsupported nqp:: op`, which took
down the `are` distribution's whole test file (`lib/are.rakumod:37`).
Issue [#9347](https://github.com/tokuhirom/mutsu/issues/9347).

Like `nqp::while` / `nqp::until`, they are special forms, not value ops: the
body and condition are evaluated repeatedly, so they compile straight to
jumps (`src/compiler/nqp_forms.rs`) rather than to an `NqpOp` call. The loop
is entered at the body, and the condition jumps back to it — a single
backward `JumpIfFalse` for `repeat_until`, a conditional exit plus a `Jump`
for `repeat_while`. The TRIR lowering got the same shape
(`src/trir/compile/nqp.rs`), so a native-int sub using either form still
compiles to TRIR. Both yield Nil, like the existing `while` / `until` forms
(in value context rakudo collects the body values into a `Seq`; none of the
four loop forms models that yet, and ecosystem code uses them sunk).

Pinned by `t/vm/nqp-repeat-loops.t`, which covers the body-runs-once case in
both the bytecode and the TRIR paths.

With the loop in place, `are` got 3 of its 4 assertions. The fourth,
`are(DateTime.now, Date.today)` expecting `Dateish`, failed because mutsu's
built-in role seeds (`src/runtime/runtime_init.rs`) did not record that
`Date` and `DateTime` compose `Dateish`: `.^roles` answered `()` and
`.^mro(:roles)` skipped the role, so `are`'s MRO walk settled on `Any`.
Both are seeded now (pinned by `t/types/temporal/dateish-builtin-roles.t`), and the
`are` ledger record is green (4/4).
