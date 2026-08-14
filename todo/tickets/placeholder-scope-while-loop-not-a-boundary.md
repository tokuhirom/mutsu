# `while`/`loop`/plain `{}` bodies should be their own placeholder scope, but aren't

Rakudo treats a `while`/`loop`/bare-`{}` body as its own placeholder scope,
just like `if`/`for`/`given` block bodies: a `$^name` used only inside such a
body does not make `$name` the *enclosing* block's parameter. Confirmed
against real `raku`:

```
$ raku -e 'my $g = { while True { $^c; last }; say $c }; $g(5)'
===SORRY!=== Error while compiling -e
Variable '$c' is not declared. Perhaps you forgot a 'sub' if this was
intended to be part of a signature?
```

(Compare: `raku -e 'my $g = { while $i++ < 1 { $^c }; say "done" }; $g(5)'`
compiles fine when `$c` is never referenced afterward — so the `while` body
*does* accept the placeholder, it just does not export it to the enclosing
block, exactly like the `if`/`for`/`given` case already fixed in
`news/2026-08/bare-precedes-placeholder-nested-scope.md`.)

mutsu's `collect_placeholders_shallow` / `collect_ph_stmt_shallow`
(`src/ast.rs`) — which decides which placeholders belong to a block's own
signature — does NOT treat `Stmt::While`, `Stmt::Loop`, `Stmt::React`,
`Stmt::Block`/`SyntheticBlock`/`Default`/`Catch`/`Control`/`RoleDecl`,
`Stmt::Phaser`, `Expr::Try`, `Expr::DoBlock`, `Expr::PhaserExpr`/`Once` as
scope boundaries: it descends into all of them unconditionally, attributing
any `$^name` inside to the enclosing block. `placeholder_order.rs`'s
`check_bare_var_stmt`/`check_bare_var_expr` (added in the nested-scope fix
above) were written to *mirror* that same boundary set exactly, so they
inherit the same gap: a bare `$name` in the enclosing scope with only a
`while`-nested `$^name` is not flagged as undeclared.

## Why this is bigger than the nested-scope fix

Changing `collect_placeholders_shallow`'s boundary decision for `while`/
`loop`/bare-`{}` would change **which placeholders a block's own signature
contains** for any code that currently puts a `$^name` inside such a
construct — not just the ordering/undeclared diagnostic. That is a much
higher-blast-radius change than the diagnostic-only fix in
`bare-precedes-placeholder-nested-scope`: it could change the arity of
existing blocks (a `{ while ... { $^x } }` block that currently compiles with
one parameter would stop taking that parameter from the enclosing signature,
potentially breaking real code that relies on the current, non-conformant
behavior). It needs its own investigation of how `while`/`loop`/bare `{}`
bodies interact with the *existing* signature-building path (not just the
diagnostic), likely touching every call site of `collect_placeholders_shallow`
(`compiler/expr_closure.rs`, `compiler/stmt.rs`), plus re-auditing whether
`collect_placeholders` (the *deep*, non-shallow collector used for the
class/role "unattached placeholder" diagnostic) needs a matching adjustment.

## Severity

Low: like the sibling tickets, this is a missing compile-time diagnostic
(false negative), not a miscompilation.

Affected: `src/ast.rs` (`collect_placeholders_shallow`,
`collect_ph_stmt_shallow`, `collect_ph_expr_shallow`), and transitively
`src/placeholder_order.rs` (`check_bare_var_stmt`, `check_bare_var_expr`,
which intentionally mirror the same boundaries).
