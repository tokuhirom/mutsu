# Chained comparison gets a genuine AST node, closing ADR-0033's Phase-4 prerequisite

mutsu's parser used to expand a chained comparison (`a < m < b`, `a !before b
before c`, ...) at parse time, either into `(a < m) && (m < b)` with the
middle operand duplicated, or — for an effectful middle — into a `DoBlock`
binding a `__mutsu_chain_cmp_N` temporary. Both shapes leaked into `.AST`:
`Q[1 < 2 < 3].AST` rendered `RakuAST::StatementPrefix::Do` instead of
rakudo's plain left-nested `ApplyInfix(ApplyInfix(1, "<", 2), "<", 3)`, even
for the all-literal case. Runtime semantics were already correct (the middle
was evaluated exactly once, short-circuiting worked), so this was purely a
representation gap — but a load-bearing one, since ADR-0033's "Phase-4
prerequisite" section had worked around it with a dedicated
`TokenKind::ChainAnd` token specifically to keep a synthesized chain
conjunction distinguishable from a user-written `&&` for Whatever-priming
purposes.

## The new node

`Expr::ChainedCompare { operands: Vec<Expr>, ops: Vec<(TokenKind, bool)> }`
is a marker, mirroring `Expr::WhateverCurry`: `ops[i]` (operator, negated)
links `operands[i]` and `operands[i+1]`, and the parser (`chain_cmp.rs`,
`comparison.rs`) builds it directly instead of expanding on the spot. Only an
actual chain (more than one comparison) uses it; a lone comparison stays a
plain `Binary`/`Unary`, matching rakudo's own rendering.

Expansion into the runtime `&&`-conjunction shape moved to the compiler
(`src/chain_compare.rs`, a new crate-root module mirroring
`whatever_curry`'s precedent): the `Expr::ChainedCompare` arm in
`Compiler::compile_expr` calls `chain_compare::expand`, which immediately
compiles the result — the expanded tree never re-enters any AST-level walker,
exactly like `whatever_curry::build_closure`.

One simplification fell out for free: the old code had **two** expansion
algorithms — a duplicated-middle form (chosen whenever any operand contained
a Whatever placeholder, to keep the placeholder visible to the Whatever-curry
walkers, which never descended into a `DoBlock`'s nested `Stmt::VarDecl`) and
a safe single-evaluation temp-variable form (`build_chain_cmp_expr`) used
otherwise. With a first-class node, Whatever substitution now runs on
`operands` directly (before `chain_compare::expand` ever executes), so the
duplicated-middle form's reason for existing is gone. `expand` always uses
the safe form now, which is also a latent correctness fix: the old
`operands.iter().any(contains_whatever)` gate chose the duplicating path
whenever *any* operand contained a Whatever — even one unrelated to the
shared middle — so an effectful non-Whatever middle sitting next to an
unrelated Whatever operand could theoretically have been evaluated twice.
`build_chain_cmp_expr_with_repeated_middle` was deleted entirely.

A `--dump-bytecode` comparison of `my ($a,$b,$c) = (1,2,3); say $a < $b < $c;`
against `main` confirmed the emitted bytecode is otherwise byte-for-byte
identical for a non-Whatever chain — the only difference was the numeric
suffix of the internal `__mutsu_chain_cmp_N` temp-variable name (an
artifact of the global counter now being bumped at compile time instead of
parse time, with no effect on program behavior).

## The walker audit

Per ADR-0033 Phase 1's own record (41 files needed a `WhateverCurry` arm,
with the failure mode being silent — a walker with a `_ => {}` catch-all
simply stops seeing the new node's operands, no compile error), the same
~40-file list was used as the audit proxy for this node. Two placeholder
collectors (`ast.rs`'s `collect_ph_expr` / `collect_ph_expr_shallow`, which
must see `{ $^a < $^b < $^c }`) and about a dozen other cross-cutting passes
(redeclaration shadowing, sink-context warnings, attribute-twigil and
private-access validation, `whenever`-scope detection, phaser lifting/
reordering, undeclared-routine scanning) needed a new arm — each mirroring
its existing `Binary` arm's behavior over the new node's `operands` list.

The audit caught, and CI caught a second time, real bugs in
`contains_whatever`'s `Expr::ChainedCompare` arm — both around the same
question: when should an operand that is *already* a `WhateverCurry` marker
(from an explicit `(* + 1)`, or from `wrap_smartmatch_rhs`'s autoprime of a
compound SmartMatch RHS) compose into the *enclosing* chain's curry, versus
stay an independent, already-scoped closure?

A first attempt tried to answer that precisely, by checking
`is_wrapped_whatevercode` on each operand and special-casing the SmartMatch/
BangTilde RHS role (mirroring the single-`Binary` SmartMatch arm's "only the
left operand counts" rule). A local targeted roast sweep caught the
`roast/S03-operators/relational.t` half of the problem
(`0 == 0 ~~ (* == 0)` was over-curried into one big closure instead of
leaving the already-materialized `(*==0)` alone) — but CI then caught a
second, opposite-direction break the local sweep missed:
`roast/S03-smartmatch/disorganized.t`'s `("foo" ~~ *.chars == 3) ~~ Bool`
regressed, because a `WhateverCurry`-wrapped operand in a *non-final,
non-SmartMatch* chain position (the `~~` in `X ~~ *.chars == 3` is not the
chain's *last* link) was now composing into the outer chain when it should
not have.

Measuring against `main`'s pre-existing behaviour (not just against rakudo)
resolved it: mutsu's old chain expansion ran at *parse time*, and the
`operands.iter().any(contains_whatever)` gate it used never actually saw
through a `WhateverCurry`-wrapped operand at all (`contains_whatever` has no
`Expr::WhateverCurry` arm), regardless of the operand's position or the
adjacent operator. So a chain never composed an already-wrapped operand in
main, even for the compositional `1 < (* + 1) < 10` case that a plain
`Binary` *would* compose (raku itself does compose that one — a real,
narrower divergence, but not one this ticket introduces or need fix). The
final `Expr::ChainedCompare` arm restores that exact gate,
`operands.iter().any(contains_whatever)` with no `is_wrapped_whatevercode`
check at all, fixing both regressions at once and matching `main`'s
behaviour byte for byte. `count_whatever` and both `replace_whatever_*`
still special-case a chain's final operand when its link is `~~`/`!~~` (the
one case that *is* reachable once some other operand's bare `*` triggers the
curry), mirroring the pre-existing single-`Binary` SmartMatch arm.

## Band-aid retirement

`TokenKind::ChainAnd` is deleted (was: a token identical to `AndAnd` at the
bytecode level, existing purely so `is_thunk_barrier` and the Whatever-curry
walkers could distinguish a synthesized chain conjunction from a
user-written `&&`). With `Expr::ChainedCompare` never matching
`is_thunk_barrier`'s `Expr::Binary` arm in the first place, and Whatever
substitution running on the un-expanded node before any `&&` is ever
synthesized, the token has no remaining purpose — the internal expansion
inside `chain_compare::expand` now just uses plain `TokenKind::AndAnd`.
`exprs_structurally_eq` (the `Debug`-string middle-duplication detector in
`whatever_curry/build.rs`) is also deleted, since there is no longer any
duplicated middle to detect.

## RakuAST rendering

`src/rakuast/convert.rs` gained a `convert_chained_compare` helper that folds
`operands`/`ops` left-to-right into nested `ApplyInfix` nodes, matching
rakudo's own shape exactly for the all-literal, effectful-middle, and
multi-link cases (verified byte-for-byte against the system `raku`'s
`.AST.gist` output). A negated link (`!before`) renders using the same
`ApplyPrefix("!", ApplyInfix(...))` shape a standalone negated comparison
already used in mutsu — matching rakudo's own `RakuAST::MetaInfix::Negate`
remains a separate, pre-existing gap (`1 !before 2` already rendered as
`ApplyPrefix` before this change) that this ticket does not close.

## Validation

New dual-oracle `t/chained-compare-ast-node.t` (24 assertions, passing
verbatim under both `target/debug/mutsu` and the system `raku`): `.AST`
rendering for pure-literal, effectful-middle, and 3-link chains; the middle
evaluated exactly once with short-circuiting; negated and mixed chains;
`{ $^a < $^b < $^c }` placeholder collection; `1 < * < 10` WhateverCode
arity/invocation and its distinction from a user-written `1 < * && * < 10`.
The pre-existing pins (`t/whatever-chained-comparison.t`,
`t/whatever-thunky-operators.t`, `t/rakuast-whatever-code.t`) pass unchanged.
`make test` (3477 files, 34034 assertions, including `cargo test --workspace`
at 878 unit tests) is green. A targeted roast sweep covering every
whitelisted `roast/S03-operators/*.t` (70 files), every whitelisted
`roast/S03-smartmatch/*.t` (22 files, including `disorganized.t`),
`roast/S02-types/{whatever,hyperwhatever}.t`, and `roast/S12-subset/
{multi-dispatch,subtypes,type-subset}.t` is green — re-run in full after the
`contains_whatever` fix above. `cargo clippy -- -D warnings` and `cargo fmt`
are clean. CI's full `make roast` (which runs the roast suite this PR's local
sweep did not cover, e.g. `S03-smartmatch/disorganized.t` before it was added
to the local sweep) caught the second regression described above.
