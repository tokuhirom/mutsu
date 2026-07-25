# `.=` in a ternary branch is no longer rejected as "too loose"

`1 ?? $v.=uc !! 9` failed with
`X::Syntax::ConditionalOperator::PrecedenceTooLoose` ("Assignment operators
inside ?? !! are too loose; parenthesize them"). raku accepts it: `.=` is a
*mutating method call* at method-postfix / dotty-infix precedence
(`raku-doc/doc/Language/operators.rakudoc`, the "Method call" and "Dotty infix"
rows), which is far **tighter** than the conditional `?? !!` — unlike `=` and the
compound assignments, which really are looser and do need parentheses.

This was PLAN §B4's top recurring blocker from the dist-compatibility sweep
("Assignment operators inside ?? !! are too loose", 2 dists): the guard was
firing on valid code.

## Root cause

The parser lowers `$v .= uc` to exactly the same node `$v = $v.uc` produces —
`Expr::AssignExpr { name: "v", expr: MethodCall { target: Var("v"), … } }`
(`postfix::dot_assign::wrap_dot_assign`). The two ternary guards
(`precedence/ternary.rs` and `precedence/list_infix_top.rs`) tested only that AST
shape, so they could not tell the tight operator from the loose one and rejected
both.

Both guards now consult `assign_operator_is_tight`, which looks at the branch's
source text: it skips the leading sigil variable and checks whether the operator
that follows is `.=`. Anything else is still reported as too loose, so `=`, `+=`,
`~=`, `//=` and friends keep raising the same exception.

Reading the source is a workaround for the AST ambiguity, not the end state — the
helper carries a `TODO` to record the operator in the AST instead. That was not
done here because `Expr::AssignExpr` has ~170 construction sites and Rust struct
literals must name every field, so adding a discriminant is a large mechanical
change best done on its own.

Measured, not assumed: a temporary probe showed which of the two guards fires for
each shape — `ternary.rs` for the plain expression form and `list_infix_top.rs`
for the list-assignment form (`my @z = 1 ?? $v.=uc !! 9`), so both needed the
same treatment.

Pinned by `t/ternary-dot-assign-branch.t` (10 subtests: then-branch, spaced
dotty-infix, else-branch, list-assignment context, chained `.=`, hash and array
element targets, plus `=` and `+=` still throwing
`X::Syntax::ConditionalOperator::PrecedenceTooLoose`). All 10 identical under
raku.

## Known remaining divergence

`1 ?? $v .= 3 !! 9` (a `.=` with a non-method RHS) is a compile-time error in
raku; mutsu rejects it at runtime with `No such method '3'`. Both reject it, so
this is the general compile-time-vs-runtime detection gap, not specific to the
ternary.
