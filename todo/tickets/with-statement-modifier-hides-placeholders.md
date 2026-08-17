# A `with` statement modifier hides its statement's own placeholders

Split out of `todo/tickets/digest-dist-blockers.md` (2026-08-17) — found while bundling the
`Digest` dist ([docs/batteries/digest.md](../../docs/batteries/digest.md)), not itself a `Digest`
concern; a general interpreter bug.

## Repro

Re-verified against `raku` and current `main` on 2026-08-17:

```raku
sub w1 { "a=$^a topic=$_" with $^n }; say w1(3, 4)
# raku:  a=3 topic=4
# mutsu: a=True topic=3
```

## Root cause

`with EXPR` as a statement modifier desugars to `Given { is_statement_modifier, body:
[DoStmt(If { cond: $_.defined, ... })] }`. The `given` form of the same shadowing problem is
already fixed. The synthetic `If` node this desugaring produces is opaque to both:

- the placeholder collector, which should see `$^a` and `$_`/`$^n`'s roles inside the modified
  statement as belonging to the enclosing sub `w1`, not to the synthetic `If`; and
- the compiler's placeholder binding, which incorrectly binds `$^a` to the `If`'s own condition
  value (`$^n.defined`, i.e. `True`) instead of leaving it bound to `w1`'s own first placeholder
  argument.

A genuine, literal nested `if` block written by the user inside the modified statement must still
introduce its own scope, so the fix needs a marker that distinguishes "synthetic `If` from a
`with`/`without` statement-modifier desugaring" from "a real `if` the user wrote" — not a blanket
change to how `If` nodes are treated for placeholder purposes.

## Affected files

- Parser/desugaring for `with`/`without` statement modifiers (search for where `given` statement
  modifiers already got their `is_statement_modifier` marker/fix, and do the analogous thing for
  the `with`/`without` desugaring's synthetic `If`).
- Placeholder collection and binding (wherever `$^a`-style placeholders are gathered and bound for
  a routine/block body).
