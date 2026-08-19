# ADR-0033 Phase 1: WhateverCode closure construction moved out of the parser

mutsu used to build the `WhateverCode` closure for a Whatever-curried expression (`* + 1`,
`.grep(* > 3)`, `@a[* - 1]`) eagerly, in the parser, at roughly fifty call sites spread
across thirteen files. This destroyed the pre-curry expression before any later consumer
— the RakuAST converter, `.DEPARSE`, error messages — could see it, and left no single
place owning the "which expression boundary does the `*` get captured within" rule.

Per [ADR-0033](../../docs/adr/0033-whatever-priming-leaf-and-derived-scope.md), Phase 1
defers closure construction to compile time without changing any observable behaviour.
Two new AST nodes were added: `Expr::WhateverArg` (a `*` that participates in priming —
not yet produced anywhere; that is Phase 2's leaf-splitting work) and `Expr::WhateverCurry`
(a marker wrapping a maximal priming scope's un-curried body). Every parser site that used
to call `wrap_whatevercode(&e)` to build the closure on the spot now constructs
`Expr::WhateverCurry(Box::new(e))` instead; the scope-decision predicates
(`should_wrap_whatevercode`, `contains_whatever`) are unchanged. The actual closure
construction — `build_closure` (formerly `wrap_whatevercode`), placeholder substitution,
and arity counting — moved to a new parser-independent module, `src/whatever_curry/`, and
is now invoked from exactly one place: a new `Expr::WhateverCurry` arm in
`Compiler::compile_expr`. The emitted bytecode is unchanged.

The mechanical rewrite surfaced two genuine, pre-existing correctness gaps in code that
pattern-matched the *eagerly built* closure shape to detect "this subtree is already a
WhateverCode" — both fixed in the same PR:

- `crate::ast::collect_ph_expr_shallow` (the placeholder-order collector) didn't hoist a
  `$^name` placeholder out of a nested WhateverCurry into the enclosing block's implicit
  signature, undercounting its arity by one — reproduced via the YAMLish battery's
  `flatten-tags` helper, which silently mis-bound values.
- The expression-context `:=` bind fast path and its `X::Bind::Slice` throw both matched
  only the built closure shape to detect a Whatever-index bind (`@a[*-1] := 42`), so they
  silently fell through to the *valid*-bind fast path instead of throwing.

About a dozen other diagnostic/validation/fast-path sites across the parser, compiler, and
runtime got a defensive `Expr::WhateverCurry` arm too, so they keep seeing (a
structurally-equivalent, pre-substitution form of) what they used to see.

Validated locally: `cargo test --workspace` green (all binaries), the full `t/` TAP suite
(3255 files) green apart from the pre-existing, already-ticketed local
`autoviv-index-guard.t` hang, and the whatever/composition/subset-focused roast files
green. Phases 2-4 (RakuAST `.AST` support for `* + 1`, and the thunk-barrier fix for
`(* > 3 && * < 8).arity`/`.grep(* > 3 && * < 8)`) are separate, not-yet-started follow-up
work — see the ADR's Outcome section.
