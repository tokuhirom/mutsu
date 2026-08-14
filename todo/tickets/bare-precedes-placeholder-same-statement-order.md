# `bare_precedes_placeholder` tracks order per statement, not per sub-expression

`bare_precedes_placeholder` (`src/placeholder_order.rs`) walks a block's
statements in order, flipping `ph_seen` on the first statement containing
`$^name`, and reports the first *earlier* statement that references a bare
`$name`. The order is tracked per **statement**, not per sub-expression, so a
bare use and its placeholder inside the *same* statement are never correctly
ordered against each other:

```raku
my $f = { $b + $^b }; say $f(3)   # rakudo: X::Undeclared (SORRY! at compile time)
                                   # mutsu:  compiles, prints 6
```

Confirmed against real `raku`:

```
$ raku -e 'my $f = { $b + $^b }; say $f(3)'
===SORRY!=== Error while compiling -e
Variable '$b' is not declared. Perhaps you forgot a 'sub' if this was
intended to be part of a signature?
```

Root cause: `bare_precedes_placeholder`'s loop does, for each statement:

```rust
if stmt_contains_var_named(stmt, &ph_name) { ph_seen = true; }
if !ph_seen && stmt_references_bare(stmt, bare_name) { return true; }
```

For a single statement `$b + $^b` (an `Expr::Binary` with `$b` on the left
and `$^b` on the right), `stmt_contains_var_named` finds `$^b` anywhere in
the statement and sets `ph_seen = true` *before* the bare-use check runs (in
the same loop iteration) — so the bare `$b` on the left is never reported
even though it lexically precedes `$^b`.

## Why this needs a different mechanism than the nested-scope fix

The scope-boundary fix in
`news/2026-08/bare-precedes-placeholder-nested-scope.md` (which this ticket
was split off from) reused the existing "does this scope contain X"
containment-check shape (`check_bare_var_stmt`/`check_bare_var_expr`, now
mirroring `collect_ph_stmt_shallow`/`collect_ph_expr_shallow`'s boundaries).
Fixing *this* gap needs a genuinely different mechanism: a left-to-right,
order-sensitive expression walk that tracks "have we passed a `$^name` yet"
as a running flag through the SAME expression tree, returning as soon as a
bare `$name` is found with the flag still false — not a pair of independent
boolean containment checks. `check_bare_var_expr` would need to become an
early-return, order-aware traversal (matching AST child evaluation order,
e.g. left-then-right for `Binary`, target-then-args for calls) rather than an
unordered "does this subtree contain X" predicate.

## Severity

Low: this is a missing compile-time diagnostic (a false negative — mutsu
accepts code rakudo rejects), not a miscompilation. `{ $b + $^b }` already
behaves sensibly at runtime (both are bound to the same call argument), it
just should have been a compile error.

Affected: `src/placeholder_order.rs` (`bare_precedes_placeholder`,
`check_bare_var_expr`).
