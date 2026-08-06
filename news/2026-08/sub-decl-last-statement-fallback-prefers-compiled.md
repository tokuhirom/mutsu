# The sub-decl-as-last-statement Sub-value fallback prefers the plan's compiled routine

Continuing the ADR-0019 C6e-3c `legacy_body` audit: when a `sub` declared
as the last statement of a block returns a Sub value (`my $f = do { sub
foo() {...} }`), and the def cannot be found in the registry under the
plan's static name (a computed-name `sub ::($name)` declaration, or a
just-out-of-scope def), `vm_call_named_inner.rs` used to build the
returned Sub straight from `plan.legacy_body` — always the AST, never
compiled bytecode.

It now tries the plan's own `compiled_routine_keys[0]` against the call's
functions table first (the same lookup pattern `RegisterSub` uses to
decide body-less registration) and only falls back to the AST body if that
key does not resolve.

Measurement found this branch already unreachable through any realistic
Raku program: a full `t/` run (27700+ tests) hit it zero times even before
the fix, and real Rakudo does not accept a runtime-computed sub name in
the first place (`SORRY! Name ::($name) is not compile-time known...`).
Validated with the project's env-gated-widen A/B methodology instead of a
dedicated pin test: an env var forced the registry lookup to miss for
every declaration in the suite, exercising the new fallback path
exclusively, and all 27705 tests still passed.

This was the last outstanding `legacy_body` reader tracked for C6e-3c's
former keep-classes. The field itself has not been deleted yet — that
needs a fresh grep audit of every remaining `.legacy_body` reference
before removal is safe.
