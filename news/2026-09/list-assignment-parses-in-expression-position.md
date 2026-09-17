# Parenthesized list assignment now parses in expression position

Part of the `#7988` ecosystem "parse-error-expectation-dump" campaign
(`eco-cluster: 859ab33e`).

A parenthesized multi-element list assignment — `($a, $b, ...) = RHS`, Raku's
"list assignment" operator, distinct from single-scalar "item assignment" —
parsed correctly as the whole statement, but not in expression position: as a
listop's argument (`ok ($a, $b) = foo(), "message"`), or as a later item of a
top-level comma list (`1, ($a, $b) = foo()`). In both of those positions the
`=` was left unconsumed by the expression-level assignment parser
(`src/parser/expr/precedence/logic.rs`), which fell through to a bare `_ =>
(rest, expr)` default for a plain `Expr::ArrayLiteral` left-hand side. The
caller then saw a generic `Confused. expected statement: expected use
statement or import statement or no statement or ...` error with no hint that
assignment was the actual problem — the family of messages this whole
ecosystem cluster is grouped by.

The statement-level assignment parser
(`src/parser/stmt/simple_expr_stmt/core.rs`) already had a general fallback
for exactly this shape, lowering it to the `__mutsu_assign_callable_lvalue`
runtime handler (the same one a `CallOn`-wrapped list-lvalue already used in
expression position). The fix adds the missing `Expr::ArrayLiteral(items)`
arm to the expression-level match, mirroring that existing `CallOn` arm: try
the single-`Whatever`-element shorthand (`list_lvalue_assign_expr`) first,
then fall back to the same builtin call the statement-level path uses. No new
runtime mechanism, no special-casing — this reuses machinery that already
existed for the two positions that did work.

Found via `Geo::Coordinates::UTM`, whose
`t/01_basic_settings.t` uses exactly this shape:

```raku
ok ($zone,$east,$north)=|latlon-to-utm('WGS-84', 57.833055556, -2.788951667), "latlon-to-utm available";
```

Before the fix this aborted the whole file with a `Confused` parse error at
that line; after it, the file parses and runs its three assertions under
mutsu (one of its own assertions — `utm-to-latlon available` — fails for an
unrelated reason, not a parser gap).

Re-measured with `scripts/ecosystem-sweep.py --only Geo::Coordinates::UTM`:
`t/01_basic_settings.t` moves from `regression` (dies at the parse error) to
`partial` (2/3 assertions pass). The distribution's overall status stays
`red` — its other two test files hit unrelated, deeper bugs once parsing gets
further into them (a `Real` numeric-context coercion issue in
`t/02_points.t`, a signature/type-inference mismatch in `t/03_mgrs.t`) — but
those are separate findings, not part of this cluster's parse-gap family.

Pinned by `t/lang/parsing/paren-list-assign-expr-position.t`, verified against
`raku`, covering: a list assignment as a listop argument with a trailing
sibling argument (the `ok (...) = |foo(), "msg"` shape), the value a list
assignment evaluates to when used as an expression, and a list assignment as
a non-first item of a top-level comma list.
