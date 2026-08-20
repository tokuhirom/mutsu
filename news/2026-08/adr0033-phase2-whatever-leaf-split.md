# ADR-0033 Phase 2: `*` leaf classification, `Q[* + 1].AST` now works

`Q[* + 1].AST` used to error with "`.AST` does not yet support this construct:
WhateverCurry(...)" because mutsu's parser had no way to tell Rakudo's converter
that a `*` was a Whatever-priming *argument* (`RakuAST::WhateverCode::Argument`)
rather than a `Whatever` *value* (`RakuAST::Term::Whatever`) — both parsed to
the same `Expr::Whatever` AST node. ADR-0033 Phase 1 (2026-08-19) had already
added an inert `Expr::WhateverArg` sibling variant and deferred WhateverCode
closure construction out of the parser into the compiler; Phase 2 makes that
sibling variant real.

A new post-parse pass, `src/whatever_curry/mark.rs`'s `mark_program`, walks
every freshly-parsed program once and reclassifies each `Expr::Whatever` leaf
to `Expr::WhateverArg` unless its immediate syntactic parent is one of the
*value* positions measured against the system `raku` (a comma operand, a
range/series endpoint, an `xx` operand, an assignment/bind RHS, a call/method
argument, a whole-slice subscript `@a[*]`, a non-currying pseudo-method target
like `*.WHAT`, a bareword pair value, or a bare `*` standing alone). The rule
is deliberately syntactic and scope-independent, not derived from mutsu's
existing "does this subtree curry" predicates — `1 x *` and `* Z 1` are both
`WhateverCode::Argument` in raku even though mutsu plants no priming scope for
either, which a scope-derived classifier would get wrong.

The change is a pure annotation: outside `src/rakuast/`, `Expr::WhateverArg`
and `Expr::Whatever` stay indistinguishable (`crate::parser::is_whatever`
treats them identically, and both compile to the same `LoadConst(Value::WHATEVER)`),
so a misclassified leaf can only produce a wrong `.AST` gist, never a wrong
program result. `src/rakuast/mod.rs` gained `RakuAST::WhateverCode::Argument`
and, as a read-direction bonus, `RakuAST::Term::HyperWhatever` for `**`.

Along the way, three remaining hand-built WhateverCode closures (`* ~~ Type`,
`Type ~~ *`, and their `!~~` negation) were converted to plant the same
`Expr::WhateverCurry` marker as every other Whatever construct, which required
re-checking (and generalizing) the smartmatch-specific arity/substitution
logic that historically only looked at the left operand. That conversion
surfaced and fixed two adjacent RakuAST rendering bugs (`!~~` and `=>` were
rendering as mutsu's internal dispatch strings, `"!~"` and `"FatArrow"`) and
one latent runtime bug: `$_ ~~ *`'s generated closure previously replaced the
*outer* `$_` too, so it ignored the caller's dynamic topic — raku's actual
semantics read the outer `$_` on the left and only prime the right (verified:
`$_ = 10; ($_ ~~ *)(3)` is `False`, i.e. `10 ~~ 3`, in both raku and mutsu now).

Pinned by a new dual-oracle `t/rakuast-whatever-code.t` (68 assertions),
passing verbatim under both `target/debug/mutsu` and the system `raku`. The
full `t/*whatever*.t` suite (35 files), `t/rakuast-*.t` (88 files), and the
relevant roast whitelist entries (`S02-types/{whatever,hyperwhatever}.t`,
`S03-operators/composition.t`, `S12-subset/{multi-dispatch,subtypes,
type-subset}.t`) all stayed green.

Phase 4 — the thunk-barrier priming *correctness* fix (mutsu currently primes
straight through `&&`/`||`/`//`/ternary, so `(1..10).grep(* > 3 && * < 8)`
silently returns the wrong list) — is the next highest-payoff slice; Phase 2's
leaf split is its prerequisite. See
[`docs/adr/0033-whatever-priming-leaf-and-derived-scope.md`](../../docs/adr/0033-whatever-priming-leaf-and-derived-scope.md).
