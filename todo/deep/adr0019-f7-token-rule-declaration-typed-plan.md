# ADR-0019 F7: scoping — grammar `token`/`rule` declarations are the only remaining raw-`Stmt` `Register*` path

## Summary

F7's box text is "Delete obsolete declaration payloads and generic statement-pool entries. Remove
old `Register*` compatibility code and assert that migrated sub/class/role declarations retain no
executable source AST." Read literally, this is already satisfied: `RegisterSub`/`RegisterClass`/
`RegisterRole` were consolidated into `RegisterDecl(CompiledDeclPlanRef)` back in Phase A4, and
`CompiledDeclPlanRef` (`Sub`/`Class`/`Role`/`Proto`/`ProtoToken`) indexes typed plan pools, not
`stmt_pool` — no sub/class/role declaration reads a raw `Stmt` to decide what to register or how.

But three earlier boxes explicitly deferred their own `token`/`rule` carve-out "until the
token/rule work lands, then closed together with F7":

- **C6d-2** (`ordinary routines run compiled bytecode`): grammar token/rule bodies stay
  interpreter-executed — this is ADR-0009's accepted execution model (the regex-matching engine
  stays tree-walked on purpose; it is not debt), not something F7 should change.
- **D6/D9** (class/role `legacy_body` removal): the `TokenRule` arm of `ClassBodyOp`/`RoleBodyOp`
  still carries a raw `raw: Stmt` payload instead of a typed one, deliberately excluded "same rule
  as C6d-2."

So F7's real remaining scope, per that coupling, is the **declaration-registration** shell around a
token/rule (name, params, multi, is_my/is_our) becoming a typed plan — NOT the regex body's
execution model, which stays exactly as ADR-0009 decided (interpreter-walked, never bytecode).
This mirrors C8's own `RegisterProtoToken`/`CompiledProtoDeclPlan` precedent, which kept
`legacy_body: Vec<Stmt>` as an opaque payload for the same reason.

## What still reads a raw `Stmt` for token/rule declarations

Top level (`src/compiler/stmt.rs:3648`):

```rust
Stmt::TokenDecl { .. } | Stmt::RuleDecl { .. } => {
    let idx = self.code.add_stmt(stmt.clone());
    self.code.emit(OpCode::RegisterToken(idx));
}
```

`RegisterToken(idx)` executes via `exec_register_token_op`
(`src/vm/vm_register_sub_ops.rs:1031`), which destructures `code.stmt_pool[idx]` as
`Stmt::TokenDecl { name, params, param_defs, body, multi, .. }` (the `..` silently drops
`is_my`/`is_our` — see "Found while scoping" below) and calls `register_token_decl` — no
precomputed facts, no fingerprinting, no on-demand compile: the raw AST clone IS the registration
payload end to end.

Inside class/role bodies (`ClassBodyOp::TokenRule`/`RoleBodyOp::TokenRule`,
`src/opcode.rs:3120,3159`): same carve-out, `raw: Stmt` is the sole payload, consumed by
`run_class_body`/`walk_role_body`'s own `TokenRule` arm (not traced further in this scoping pass —
D6/D9's own notes call this "the token/rule arms of the body walk," same shape as the top-level
site).

`AugmentClass` (a *different*, permanently-accepted exception per D10's own closing note — "the
`stmt_pool`-fed augment walker's own one-shot construction") is unrelated to this scoping and
should NOT be touched by this work. Likewise `RegisterEnum`/`RegisterSubset` are Phase A's own
explicitly-deferred, non-blocking items ("can adopt the same representation later; they do not
block retiring the three tree-walking paths") — out of scope here too.

## How complex the actual migration looks

Smaller than Phase D's own class/role work. `register_token_decl`
(`src/runtime/registration_sub.rs:1582`) builds a `FunctionDef` straight from the raw
name/params/param_defs/body/multi arguments with `compiled: None` always (a token/rule body is
NEVER compiled to bytecode — consistent with ADR-0009) and calls `insert_token_def`
(`src/runtime/resolution.rs:159`), which just stamps `decl_order` (Rakudo's LTM declaration-order
tie-break) and inserts into `registry_mut().token_defs`, bumping `TOKEN_DEFS_GEN` to invalidate
cached regex parses. No redeclaration check, no `is_my`/`is_our` scoping logic, no attribute
pre-scan, no trait dispatch — this registration path is much flatter than a `SubDecl`'s.

A `CompiledTokenDeclPlan` (mirroring `CompiledProtoDeclPlan`'s shape) would plausibly carry:
`name: Symbol`, `params: Vec<String>`, `param_defs: Vec<ParamDef>`, `multi: bool`,
`is_my: bool`, `is_our: bool`, and `raw_body: Vec<Stmt>` (opaque, mirroring
`CompiledProtoDeclPlan::legacy_body` — the regex body stays interpreter-executed per ADR-0009, so
this is not "no executable source AST" in the same sense C1-D10 achieved for sub/class/role; it is
the same accepted exception token/rule has had all along, just wrapped in a typed struct instead of
a bare `Stmt` clone). `RegisterToken(idx)` would become `RegisterDecl` with a new
`CompiledDeclPlanRef::Token(u32)` variant (or its own opcode, if `OpCode`'s 48-byte size guard
makes growing `CompiledDeclPlanRef` cheaper — check `size_of::<OpCode>()` before choosing).

## Found while scoping — likely a non-issue, worth a quick raku-verified pin before touching this code

`exec_register_token_op`'s destructure uses `..`, silently dropping `Stmt::TokenDecl`'s
`is_my`/`is_our` fields — `register_token_decl`'s own signature doesn't even accept them.
Spot-checked with a `{ my token foo { \d+ } ... }` / lexical-scope-leak repro: `raku` and `mutsu`
produce byte-identical output (both correctly reject the token as unavailable outside its
declaring block — `No such method 'foo' for invocant of type 'Match'`). So this is very likely
inert in practice (token lexical scoping is probably enforced elsewhere, e.g. by regex name
resolution itself, not by this dropped flag) rather than a live bug — but re-verify with a
proper `raku`-compared table (an `our token` at file scope reused from a nested package, a `my
token` shadowing an outer same-named token, multi-file/EVAL cases) before assuming a typed plan
can drop these fields too. If a real gap surfaces, file it separately; it is not this box's own
target.

## Recommended shape for the actual implementation slice

Follow C8's own precedent closely (it is the nearest-shape prior migration): a single, mostly
additive PR that (1) adds `CompiledTokenDeclPlan` + its `CompiledDeclPlanRef`/opcode variant, (2)
lowers `Stmt::TokenDecl`/`RuleDecl` into it at compile time (name/params/param_defs/multi/is_my/
is_our precomputed, `raw_body` kept verbatim), (3) switches `exec_register_token_op` to read the
plan instead of `stmt_pool`, and (4) verifies with the full local `t/` suite plus every
whitelisted grammar/regex roast file (`S05-*`, grammar-adjacent `S12-*`) and a small raku-verified
table covering `token`/`rule`/`multi token`/`proto token` interaction (the existing
`RegisterProtoToken` machinery from C8 already covers the `proto token` marker itself — this slice
is only the concrete candidate token/rule, not its proto). The `ClassBodyOp`/`RoleBodyOp`
`TokenRule` carve-out (inside class/role/grammar bodies) is a separate, second slice — do not
bundle it with the top-level `RegisterToken` slice, matching this box's own "no shared-helper by
pattern-match" discipline used throughout Phase D/E/F.

## Pointers

- ADR-0019 F7 box (`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`, search
  "F7 — Delete obsolete declaration payloads").
- C8's box (same file, search "C8 — Proto declarations register from typed plans") — the nearest
  prior precedent, including its own decision to keep a `legacy_body` field for the same
  ADR-0009 reason.
- `src/compiler/stmt.rs:3648` (top-level `RegisterToken` emission),
  `src/vm/vm_register_sub_ops.rs:1031` (`exec_register_token_op`),
  `src/runtime/registration_sub.rs:1582` (`register_token_decl`),
  `src/runtime/resolution.rs:159` (`insert_token_def`).
- `src/opcode.rs:3120` (`ClassBodyOp`/`RoleBodyOp`'s `TokenRule` variant — the second, separate
  carve-out).
