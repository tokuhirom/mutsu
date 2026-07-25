# `do for` no longer drops the value of a bare call to an imported sub

`do for ^2 { imported_sub() }` collected `Nil` for every iteration while the
parenthesized form `do for ^2 { (imported_sub()) }` collected correctly — silent
wrong data, not an error. Any script that builds a list this way from a module's
exported subs was affected. Reported in
`todo/tickets/do-for-loses-imported-sub-return-value.md`.

## Root cause

The parser lowers a bare call whose name it already knows to be a routine into
`Stmt::Call` (a *statement* call) rather than `Stmt::Expr(Expr::Call { .. })`.
Whether the name is known depends on whether the enclosing `use` has been
processed by the time the statement is parsed — which is why the bug only showed
up for imported subs and never for a locally declared one, and why wrapping the
call in parens (or assigning it to a temp, or interpolating it) hid it: those
shapes parse as an expression.

`compile_stmts_value` — the value-collecting body compiler used by `do for`,
`do if` and `do given` — had no `Stmt::Call` arm in its final-statement match, so
a `Stmt::Call` in value-final position fell through to the default arm, which
compiles the statement for its side effect (emitting `SinkPop`, discarding the
value) and then pushes `Nil` as the body's result.

The fix routes that arm through the existing `compile_tail_stmt_call_value`
helper, the same one `compile_try` already used for a tail `Stmt::Call`. That
helper also handles the named/slip-argument shape (`ExecCallPairs { keep_value:
true }`), so `do for ^2 { namedsub('x', :upper) }` keeps its value too.

## Note for future investigations

The earlier triage of this ticket concluded — from `--dump-bytecode` output —
that all three forms compiled identically and that the divergence had to be
runtime state. That conclusion was wrong, and the reason is worth remembering:
`--dump-bytecode` (and `--dump-ast`) parse the source *without* the interpreter's
module search paths installed, so the `use`d module is never loaded and the
imported name is not known to the parser. The dumped AST/bytecode therefore
differ from what actually executes for any program that `use`s a module. The
executed opcodes have to be read from the run path itself.

Pinned by `t/do-for-imported-sub-value.t` (fixture `t/lib/DoForValueSub.rakumod`),
which checks `do for` / `do for -> $i` / trailing semicolon / multi-statement
body / list assignment / `do if` / `do given` / named args, plus the
parenthesized control form.
