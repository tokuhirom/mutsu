# `push(@a, 1, |@rest);` works as a statement, not just as an expression

A mutating listop whose argument list contained a slip resolved to nothing at
all — but only in sink context:

```raku
my @a; push(@a, 1, |(2, 3));          # before: Unknown call: push
my @a; my $r = push(@a, 1, |(2, 3));  # always worked
my @a; push(@a, 1, 2);                # always worked
```

## Why

`push`/`unshift`/`append`/`prepend`/`splice` are not real routines in mutsu;
the compiler rewrites `push(@arr, vals)` into the method form `@arr.push(vals)`.
A statement-level call parses to `Stmt::Call` with typed `CallArg`s, and its
compilation gated that rewrite on the argument list being **all
`CallArg::Positional`** — a `CallArg::Slip` made the check fail, so the
statement fell through to the generic `ExecCallPairs` dispatch, which has no
`push` routine to resolve.

The expression form never had the problem: `Expr::Call` spells a slip as
`Expr::Unary { op: Pipe }`, an ordinary argument expression, so the rewrite fired
and produced `CallMethodMut` with a slip side table.

## The fix

For the fixed listop set, a `CallArg::Slip(e)` now round-trips to the
expression form as `Expr::Unary { op: Pipe, expr: e }` — exactly what the
expression parser produces for the value-position spelling — so the statement
takes the same rewrite the expression always did. An imported routine (also
accepted by `is_normalized_stmt_call_name`) keeps the stricter positional-only
condition; only the five listops opt in.

Found in `Config::TOML::Parser::Actions`, which calls `push(@step-taken, $step,
|pwd($root, @rest))` at three sites (`docs/batteries/toml.md`). Pinned by
`t/listop-slip-arg-sink-context.t`.
