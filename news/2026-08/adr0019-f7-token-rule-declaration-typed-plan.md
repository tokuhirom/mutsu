# ADR-0019 F7 closed: `token`/`rule` declarations register from typed plans

F7's box text was "Delete obsolete declaration payloads and generic statement-pool entries. Remove
old `Register*` compatibility code and assert that migrated sub/class/role declarations retain no
executable source AST." Read literally this was already satisfied by Phase A4
(`RegisterSub`/`RegisterClass`/`RegisterRole` had long been consolidated into
`RegisterDecl(CompiledDeclPlanRef)`), but three earlier boxes (C6d-2, D6, D9) had each explicitly
deferred their own `token`/`rule` carve-out "until the token/rule work lands, then closed together
with F7" — grammar `token`/`rule` declarations were the last `Register*` path anywhere in the
codebase that still read a raw `Stmt` end to end to decide what to register.

The regex body itself was never in scope: ADR-0009 fixed the regex-matching engine as
interpreter-executed by design, not debt to retire. F7's real scope was narrower — the
**declaration-registration shell** around a token/rule (name, params, multi) becoming a typed plan
instead of a bare `Stmt` clone, mirroring C8's own `CompiledProtoDeclPlan` precedent (which kept
`legacy_body: Vec<Stmt>` opaque for the identical reason).

## Slice 1 — top-level declarations

`CompiledTokenDeclPlan` (`name`/`params`/`param_defs`/`multi`/`raw_body`) plus a new
`CompiledDeclPlanRef::Token(u32)` variant replaced the old dedicated `RegisterToken(idx)` opcode
(which indexed `stmt_pool` directly) with `RegisterDecl(idx)`. `is_my`/`is_our` were deliberately
NOT carried onto the plan — the pre-existing path never read them either (the old match arm dropped
them via `..`), verified benign with a `{ my token foo {...} ... }` lexical-scope-leak repro
byte-identical to `raku`.

## Slice 2 — class-body declarations

A class body's own package is fixed and known at class-declaration compile time (unlike a role
body's, whose composing package is not known until composition), so a class-body `token`/`rule`
statement could skip the registration-time `run_block_raw` on-the-fly recompile the same way slice 1
removed it at the top level. `ClassBodyOp` gained a `TokenRule { plan: CompiledTokenDeclPlan }`
variant (the plan-building logic factored out of `add_token_decl_plan` into a shared
`build_token_decl_plan(stmt)` free function, reused by `classify_class_body_stmt`);
`run_class_body` now calls `register_token_decl` straight from the plan's fields instead of falling
into `ClassBodyOp::Other`'s raw-`Stmt` + `run_block_raw` path.

**Role-body `token`/`rule` declarations are a permanent exception, not a deferred slice 3.** They
stay `RoleBodyOp::Deferred`/`DeferredBodyOpKind::TokenRule`, carrying a raw `Stmt`: a role's
composing package genuinely is not known until composition (the same reason `Plain` deferred
statements also keep the raw-`Stmt` fallback, per D8-1/D8-2), so there is no compile-time package to
precompute a plan against — this mirrors `AugmentClass`'s and `RegisterEnum`/`RegisterSubset`'s own
already-accepted permanent carve-outs.

## Verification

Both slices verified with the full local `t/` suite (3197 files, 29769 tests), the full local
grammar/token/rule-named `t/` subset (68 files), every whitelisted grammar/regex roast file
(`S05-*`/`S12-*` subset, 191 files, 7573 tests, including `integration/advent2013-day18.t`) — all
release, all green — `cargo test --lib` (835 tests, including `opcode_stays_small`, confirming
`OpCode` shrank rather than grew), `cargo build`/`clippy -- -D warnings`/`fmt` clean, and hand-built
raku-verified tables (grammar `token`/`rule` bodies, `proto token`/`multi token` variants, a role
token composed into two different grammars, a grammar redeclared inside a loop body) byte-identical
to `raku` including exit codes.

This closes ADR-0019's F7 box. Only the completion gates (G1-G4) remain open on the ADR.
