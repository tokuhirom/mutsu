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
`are(DateTime.now, Date.today)` expecting `Dateish`, walks
`.^mro(:roles)`, and mutsu answered `(DateTime) (Any) (Mu)` without the
role. `Date` and `DateTime` now have rows in the built-in type catalog
(`roles: ["Dateish"]`), `.^roles` reads them, and `.^mro(:roles)` falls back
to the catalog's roles when the registry has no composed-role record (which
also fixes `Promise.^mro(:roles)` missing `Awaitable`).

Two places had to be taught about that. The "a class doing `Dateish`
stringifies through its `!formatter`" dispatch rule is for user classes
composing the role, so it now skips the built-ins and their subclasses,
which stringify natively. And the built-in `.are` ranked an instance's own
MRO tail (`Any`, `Mu`) ahead of its shared-role fallbacks; with the catalog
row supplying `DateTime`'s full MRO, that made `(DateTime.now,
Date.today).are` answer `Any`, so the MRO tail now goes after them.

Pinned by `t/types/temporal/dateish-builtin-roles.t`; the `are` ledger record
is green (4/4).
