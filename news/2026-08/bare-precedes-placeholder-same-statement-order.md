# `bare_precedes_placeholder` now tracks order within a single expression, not just across statements

`bare_precedes_placeholder` (`src/placeholder_order.rs`) is the check behind
the rule that a placeholder parameter (`$^name`) declares its block's
`$name` under the plain name, so a bare `$name` written *before* the
`$^name` that declares it must be a compile-time error
(`X::Undeclared`/`X::Placeholder::NonPlaceholder`).

The previous implementation tracked ordering per **statement**: for each
statement it independently checked "does this statement contain `$^name`
anywhere" (setting a `ph_seen` flag) and then, if not yet seen, "does this
statement contain a bare `$name` anywhere". Both checks walked the whole
statement's subtree unordered, so within a SINGLE statement the relative
position of the two uses was never actually compared:

```raku
my $f = { $b + $^b }; say $f(3)   # rakudo: X::Undeclared at compile time
                                   # mutsu (before this fix): compiled, printed 6
```

`stmt_contains_var_named` found `$^b` anywhere in the `Expr::Binary` (the
right operand) and set `ph_seen = true` before the bare-use check ran in the
same loop iteration, so the bare `$b` on the left was never reported even
though it lexically precedes `$^b`.

## Fix

Replaced the pair of independent whole-statement containment checks with a
single left-to-right recursive walk (`order_check_stmt`/`order_check_expr`,
paralleling the existing `check_bare_var_stmt`/`check_bare_var_expr`
scope-boundary rules) that threads one `OrderState` (a running "have we
passed the placeholder yet" flag) through the entire statement list in true
AST evaluation order — left-then-right for `Expr::Binary`, target-then-args
for calls, etc. A bare use is now only flagged if the placeholder truly has
not been evaluated yet at that point in the tree, whether that's an earlier
statement or an earlier sub-expression of the same statement.

Also folded in the `Stmt::Assign` target name check (previously only applied
at the top level via a separate `stmt_references_bare` wrapper, so it was
silently skipped for `Assign` statements nested inside e.g. a `while` body)
into the unified per-node walk, so it now applies at every nesting level.

Verified against real `raku`:

```
$ mutsu -e 'my $f = { $b + $^b }; say $f(3)'
X::Undeclared: Variable '$b' is not declared. Did you mean '$^b'?

$ mutsu -e 'my $f = { $^b + $b }; say $f(3)'
6
```

Regression tests added to `t/bare-precedes-placeholder-nested-scope.t`.

See also `news/2026-08/bare-precedes-placeholder-nested-scope.md` for the
sibling scope-boundary fix this ticket was originally split off from.
