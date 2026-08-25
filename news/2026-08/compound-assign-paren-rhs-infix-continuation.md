# Compound-assignment RHS now continues past a parenthesized inner assignment

`$term += ($sign = -$sign) / $_` — the inner loop of a Rosetta Code
Euler-Mascheroni (Vacca series) program — failed to parse in mutsu with
`Runtime error: Regex not terminated.` (issue #6953), even though `raku` runs
it fine. The minimal repro was even smaller:

```raku
my $a; my $b = 1;
$a += ($b = 2) / 2;   # mutsu: "Regex not terminated."; raku: $a becomes 1
```

## Root cause

Statement-level `$var += RHS` parses its RHS through
`parse_assign_expr_or_comma_no_word_logical` (`src/parser/stmt/assign/sink.rs`),
which tries `try_parse_assign_expr` first so a chained assignment like
`$a += $b = 3` still works. When the RHS text starts with `(` and looks like
a parenthesized assignment, `try_parse_assign_expr` delegates to
`parenthesized_assign_expr` (`src/parser/stmt/assign/paren.rs`), which parses
`($b = 2)` and returns *immediately* after the closing `)` — without checking
whether a tighter infix operator (`/`, `+`, `-`, `*`, ...) follows.

`+=` is item assignment in Raku, which is *looser* than the following `/`, so
`$a += ($b = 2) / 2` must parse as `$a += (($b = 2) / 2)`. Because the
compound-assign RHS parsers treated the paren-assignment shortcut's `Ok`
result as the complete answer, the leftover `/ 2` became a separate
statement. In term position a leading `/` starts a regex literal, which never
terminates — hence the confusing "Regex not terminated" error. For `+`, `-`,
and `*` the leftover text didn't error; it silently parsed as a second,
sink-context statement, producing a *silently wrong* answer instead
(`$a += ($b = 2) + 3` gave `$a == 2` instead of `5`).

The plain (non-compound) `$var = RHS` path never hit this bug: it parses its
RHS with `expression_no_word_logical` directly (not `try_parse_assign_expr`),
so `paren_expr` (the general parenthesized-primary parser) recognizes the
inner assignment, wraps it in `Expr::Grouped`, and the ambient
precedence-climbing call stack (`additive_expr` → `multiplicative_expr` → ...
→ `primary` → `paren_expr`) naturally continues parsing `/ 2` on top of it —
exactly the continuation the compound-assign RHS path was missing.

## Fix

Added `paren_assign_rhs_is_complete` (`src/parser/stmt/assign/try_assign.rs`):
given the input immediately after a parenthesized-assignment's closing `)`,
it reports whether that's a genuine terminator (end of input, `;`, `)`, `}`,
`]`, a comma, `=>`, or a loose word-logical / statement-modifier keyword that
an enclosing layer is responsible for) as opposed to a tighter infix/postfix
operator that still needs to bind to the group.

The compound-assign RHS call sites — `parse_assign_expr_or_comma` and
`parse_assign_expr_or_comma_no_word_logical` (`sink.rs`), and the
`parse_compound_assign_op` arm of `try_parse_assign_expr` itself
(`try_assign.rs`, per the reporter's note that the expression-form compound-
assign path needed the identical fix) — now only accept the parenthesized-
assignment shortcut when `paren_assign_rhs_is_complete` holds. Otherwise they
discard the shortcut result and fall back to the general expression grammar
(`parse_comma_or_expr`/`_no_word_logical`, `expression_no_sequence`), which
reaches the same recognition through `paren_expr` and then correctly
continues the infix/postfix precedence chain — the same mechanism that
already made the plain `=` case work.

## Tests

`t/compound-assign-paren-rhs-infix.t` (22 assertions, cross-checked against
`raku`) covers: the issue's minimal repros (`/`, and the Vacca-series
`($sign = -$sign) / $_` shape), the silently-wrong-answer `+`/`-`/`*`
variants, the negative cases from the issue that must keep working (plain
`=`, no inner paren-assignment, extra grouping parens, parenthesized `(my
$)`/`(my $x)`/`($x)` lvalue targets, and a `my` declaration inside parens),
the Vacca-series inner loop with a small range, and the full motivating
Rosetta Code `gamma` function for `N=10` (`0.574285301882304`, matching
`raku` exactly).
