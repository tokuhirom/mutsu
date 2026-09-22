# A trailing statement modifier now gates a whole comma-separated statement

A statement of the form `EXPR1, EXPR2 if COND;` was parsed as an unconditional
`EXPR1` followed by a separately-gated `EXPR2`, instead of gating the entire
comma-separated list as one unit. Real Raku (confirmed against `raku`) treats
the whole list as the thing the modifier wraps: `$a++, $b++ if False;` runs
neither side, and `$a++, $b++ if True;` runs both.

This surfaced through `HTTP::Server::Async`'s header-parsing loop:

```raku
while $index++ < $data.elems - 4 {
    $index--, last if $data[$index] == $rn[0] && ...;
}
```

With only `last` gated by the condition, the unconditional `$index--` undid
the loop's own postfix `$index++` on every iteration whose condition was
false — `$index` never advanced, and the loop spun forever without ever
finding the `\r\n\r\n` header terminator. Every request to an
`HTTP::Server::Async` server hung indefinitely.

Root cause: `expr_stmt`'s comma-list handling
(`src/parser/stmt/simple_expr_stmt/core.rs`) split a comma-list statement
followed by a modifier into an unconditional prefix (every element but the
last) plus a modifier wrapping only the last element. Fixed by building one
list expression (`Expr::ArrayLiteral`) for the whole comma-separated
statement and letting the modifier wrap that list as a unit, matching how
the no-modifier case already built a single list.

Pinned by `t/vm/scope/comma-list-statement-modifier-scope.t`.

`HTTP::Server::Async` 0.2 moved from `partial` (1/8 baseline files, 12/28
assertions) to `green` (8/8 files, 28/28 assertions) — the fix cleared every
remaining failure in the distribution's own suite.
