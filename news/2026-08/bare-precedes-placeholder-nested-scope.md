# `bare_precedes_placeholder` does not respect nested block scopes

A placeholder declares its parameter under the *plain* name, so a bare `$b`
written **before** the `$^b` that declares it is `X::Undeclared` in rakudo.
mutsu implements that ordering rule with `ast::bare_precedes_placeholder`, which
walks the block's statements in order, flips `ph_seen` on the first statement
containing `Var("^b")`, and reports the first earlier statement that references
a bare `Var("b")`.

The walk has no notion of scope: it descends into whatever `check_bare_var_stmt`
happens to handle, and a `$^b` found in a **nested block's** body counts as
declaring *this* block's `$b`. rakudo says the opposite — the inner block owns
that placeholder, so the outer `$b` is undeclared:

    my $f = { for 1 { $^b }; say $b }; $f(42)   # rakudo: X::Undeclared;  mutsu: prints 1
    my $f = { if 1  { $^b }; say $b }; $f(42)   # rakudo: X::Undeclared;  mutsu: prints 1

(The `for` **statement modifier** form is the opposite case and is correct:
`{ say $^b for 1; say $b }` is legal in both, because a modifier body is not a
block. See `news/2026-08/for-modifier-placeholder-scope.md`, which added the
`is_statement_modifier` flag the `Stmt::For` arm now consults.)

There is a second, orthogonal gap in the same routine: the order is tracked per
*statement*, not per sub-expression, so `{ $b + $^b }` is accepted even though
the bare use precedes the placeholder within the one statement.

## Why this is more than a one-line fix

`check_bare_var_stmt` / `check_bare_var_expr` are a private ad-hoc walker,
separate from the three placeholder collectors in `ast.rs`
(`collect_ph_stmt_shallow`, `collect_unattached_ph_stmt`,
`collect_ph_stmt`). The right fix is to give the ordering check the same scope
discipline those collectors already encode — stop at every construct that opens
a placeholder scope (bare/pointy block, `if`/`while`/`for` **block** bodies,
closures, `do`/`gather`) while still descending through statement headers and
statement-modifier bodies — rather than growing a fourth walker with its own
idea of what a scope is. Doing that properly probably means expressing the
ordering rule *in terms of* the existing collectors instead of beside them.

Affected: `src/ast.rs` (`bare_precedes_placeholder`, `stmt_contains_var_named`,
`stmt_references_bare`, `check_bare_var_stmt`, `check_bare_var_expr`).

Both cases are **false negatives** — mutsu accepts code rakudo rejects — so
nothing miscompiles today; the cost is a missing compile-time diagnosis.
