# `unless COND -> $x { }` accepts a pointy-block parameter

`unless 0 -> $_ { $_.say }` was a compile error ("Missing block"), while the
sibling constructs `if`, `while`, `until` and `with` all accepted the pointy
form. Rakudo prints `0` — the parameter binds the condition's **own** value, not
its negation.

## Root cause

`unless_stmt` in `src/parser/stmt/control/conditionals.rs` parsed the condition
and went straight to `block(rest)`, never calling `parse_if_binding_params` the
way `if_stmt` and the `elsif` arm do — so the `->` was left unconsumed and the
block parse failed.

Simply adding the call would not have been enough: `unless` lowers to
`Stmt::If { cond: !COND }`, and `binding_var` binds the value of *that* `cond`,
which would have bound `True`/`False` instead of the condition's value. So a
pointy `unless` is now lowered as the **else** branch of an un-negated `if` —
`lower_if_chain(vec![IfChainClause { cond, then_branch: vec![] }], Some(ElseClause { … }))`
— which reuses, unchanged, the machinery that already hands `if COND { } else ->
$x { }` the condition value through a generated temp binding. The plain
(non-pointy) `unless` keeps its existing `if !COND` lowering, and the
`X::Syntax::UnlessElse` check for a following `else`/`elsif`/`orwith` is
unaffected.

`until` needed no change: `until $i == 0 -> $x { }` already bound the condition
value (`False` on every iteration that keeps looping), matching raku. Both are
now pinned, along with `if`/`while`/`with`, by `t/signature-binding-gaps.t`.

## Adjacent finding, not fixed here

A `$_` pointy parameter on a conditional leaks into the enclosing scope:
`$_ = 1; if 5 -> $_ { }; say $_` prints `5` in mutsu and `1` in raku. This is
pre-existing and independent of `unless` (it reproduces on `if`, which has
accepted the pointy form for a long time), so it is filed separately as
`todo/tickets/conditional-pointy-topic-param-leaks-scope.md`.
