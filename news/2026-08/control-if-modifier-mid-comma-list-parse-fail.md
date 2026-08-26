# A statement modifier over a comma list, and its comma-list condition

`say (1, 2 if True, 3);` was a hard parse failure in mutsu (*"Confused. expected
statement"*), where `raku` prints `(1 2)`. Found by the doc-diff harness on
`Language/control.rakudoc:263`.

## What raku actually does (measured, v2026.06)

The content of `(...)` is a semilist of **statements**, so the whole thing is one
statement `1, 2` carrying an `if` modifier — and the modifier's condition is
itself a full `EXPR`, which in Raku includes the list-infix comma. So the
condition is the *list* `(True, 3)`, not just `True`. Three probes pin that
reading down:

| expression | raku |
| --- | --- |
| `(1, 2 if True, 3)` | `(1, 2)` |
| `(1, 2 if False, 3)` | `(1, 2)` — the 2-element list `(False, 3)` is truthy |
| `(1, 2 unless False, 3)` | `Empty` — same condition, negated |
| `(1, 2 if True, die("boom"))` | dies — the whole comma list is evaluated |
| `(1 if True, 2, 3)` | `1` |

The same rule applies outside parens: `my $x = 0; $x = 1 if False, 3;` leaves
`$x` at `1` in raku, because the condition is `(False, 3)`.

A call-argument list is a *different* construct and rejects the shape in both
implementations (`say(1, 2 if True, 3)` is a syntax error in raku too), so
nothing there needed to change.

## Root cause — two independent gaps

1. **`src/parser/primary/container/paren.rs`.** `try_inline_modifier` was
   consulted for a single leading item (`(5 if True)` worked) and immediately
   after a comma, but never after a *completed* comma list. So the item loop in
   `paren_expr_inner` reached `2 if True` and bailed out with
   `"',' or ';' in parenthesized list"`.
2. **`src/parser/stmt/modifier.rs`.** The `if` / `unless` / `when` modifier
   conditions were parsed with `expression()`, which stops at the comma. That
   left `, 3` unconsumed — a hard error inside `(...)`, and outside parens a
   bogus *"Useless use of constant integer 3 in sink context"* warning plus the
   wrong truth value.

## Fix

The paren item loop now tries `try_inline_modifier` on the accumulated list
before demanding a separator, and the three conditional modifiers parse their
condition with `parse_comma_or_expr` — the same helper the `given` modifier
already used, which builds an `Expr::ArrayLiteral` for a real list and returns
the bare expression otherwise. `for`'s iterable already built its own comma list,
so it was already correct.

`while` / `until` / `with` / `without` were deliberately left on `expression()`.
Rakudo's grammar gives them a comma-inclusive `EXPR` too, but a comma-list loop
condition is unconditionally truthy (an infinite loop), and `with`/`without`
carry an existing behaviour that folds a trailing comma list into the modified
`Stmt::Call`'s argument list. Neither is exercised by the reported divergence, so
they stay as they were rather than being changed unmeasured.

Pinned by `t/control-constructs-in-expression-position.t`.
