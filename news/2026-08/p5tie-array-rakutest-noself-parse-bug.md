# Fixed a bare `die`/`fail` inside a class-nested `sub` misparsing as X::Syntax::NoSelf

`P5tie`'s `t/array.rakutest` (from the un-triaged `test_die` batch, see
`todo/deep/p5tie-stash-bind-key-protocol.md`) died at parse time with
`Runtime error: X::Syntax::NoSelf`, before any test ran — a separate,
unrelated bug from that ticket's `Stash.BIND-KEY` gap (`t/scalar.rakutest`
and `t/hash.rakutest` fail later, at runtime, on the `BIND-KEY` gap only).

Root cause: `reject_no_self_in_subs` (in
`src/parser/stmt/class/attr_checks.rs`) scans a plain `sub` nested directly
in a `class` body for attribute-twigil references (`$.attr`/`$!attr`), which
require `self` and are illegal in a `sub` (subs have no invocant). Its
detector, `expr_uses_attr_twigil`, matched `Expr::Var(name)` via
`name.starts_with('!')` with no lower bound on the name's length — but a
bare, no-argument `die`/`fail` statement parses to `Expr::Var("!")` (a
reference to `$!`, the current error variable, per `die_stmt` in
`src/parser/stmt/simple/control_stmts.rs`), which also satisfies
`starts_with('!')` even though it has nothing to do with `$!attr`. Every
other twigil-detection site in the compiler (`compiler/mod.rs`,
`expr_unary.rs`, `expr_postfix.rs`, `expr_call.rs`) already guards this same
check with `name.len() > 1`; `attr_checks.rs` was the one holdout.

Minimal repro: `class Foo { sub helper() { die } }` — a bare `die` inside a
plain `sub` (not a `method`) nested in a `class` body, with no attribute
access anywhere in sight, incorrectly raised `X::Syntax::NoSelf`.

Fixed by adding the same `name.len() > 1` guard. Regression-tested in
`t/syntax-noself.t`: a bare `die`/`fail` inside a class-nested plain `sub`
now runs cleanly, while a genuine `$!attr` reference in the same position is
still correctly rejected.
