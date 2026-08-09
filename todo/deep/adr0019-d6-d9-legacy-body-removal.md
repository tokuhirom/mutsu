# ADR-0019 D6/D9 design: replacing the two `legacy_body` fields with typed body plans (and D10's endgame)

Design pass (2026-08-08, no code landed) for D6 ("Remove `CompiledClassDeclPlan::legacy_body`"),
D9 ("Remove `CompiledRoleDeclPlan::legacy_body`"), and the D10 walker deletion they enable. The
ADR mandated a C6d-style survey before touching either field; that survey is done and inventoried
below. (`CompiledProtoDeclPlan::legacy_body` is explicitly a separate later box — out of scope.)

## Reader inventory (grep-complete, 2026-08-08)

The only destructuring reads of either field are the two register ops
(`vm_typedecl_ops.rs:134` class, `:602` role); each passes the body as a bare positional
`&[Stmt]` into its walker. No reader exists outside registration — no introspection, error
message, Pod, or serialization path touches the fields. `Stmt::AugmentClass` reads `stmt_pool`,
not a plan, so D6 does not change augment's body source (the ADR's "preserve augmentation"
clause is about the shared arm helpers D3-2..6 already unified).

**Class side** — `register_class_decl` reads the body at exactly one site,
`run_class_body` (`registration_class_decl.rs:221`); everything else in the orchestrator is
already plan-driven (D1's `is_stub`/`trusts`, snapshot/redeclaration are registry-only). The
walk's arms, scored:

| Arm / reader | Plan coverage today |
|---|---|
| `MethodDecl` | done (D3-1/D3-7 — raw stmt is only a trigger; body AST inside `CompiledMethodDecl` is D3-8, orthogonal) |
| `TrustsDecl` | **redundant** — D1's plan `trusts` already feeds `publish_class_shell`; the walk arm double-inserts. Deletable now |
| `HasDecl` | partial — `CompiledAttrDecl` built at runtime `from_stmt`; closed by D2b-2 (`todo/deep/adr0019-d2-remainder-attr-plan-lowering.md`) |
| `DoesDecl` (`also does R`) | name trivially typeable; execution half is D8; bracket args are D4/D7 |
| `Phaser{Leave}` | none — bodies collected, then `run_block_raw` at exit |
| `VarDecl` code alias (`our &baz ::= &bar`) | none |
| `ProtoDecl{is_method}` | none (class-body method protos bypass C8's sub proto plans; builds a `FunctionDef` with a raw AST body) |
| `SubDecl` tail probe (fq class-sub names) | none (a name scan) |
| `_` → `class_body_other_stmt` | none — the largest reader; see below |
| `persist_class_body_statics` VarDecl re-scan | none — pure syntactic name set |

The `_` arm (`registration_class_body.rs:328-407`) is where plain statements, `use`/`need`,
nested `class`/`role`, `sub` decls, BEGIN/CHECK, EVAL, `my`/`our` lexicals, **and `token`/`rule`
decls** land. Mechanism: `run_block_raw` on a single raw `Stmt` — a fresh throwaway `Compiler`
per statement, **per registration** (`run.rs:726-770`), then re-entrant `run_nested`. The user
code already runs as bytecode; what `legacy_body` buys is only the *deferred per-registration
compile*. The surrounding semantics that must survive any replacement: `current_package`/`?CLASS`
seeding, class pre-inserted in the registry, `defining_class` for BEGIN/EVAL `has`-injection,
BEGIN/EVAL failure swallowing, class-scoped env writeback into the saved env, per-statement
registry re-publish, and the free-var writeback drain (Slice F, `run.rs:749-768` →
`apply_pending_rw_writeback` at `vm_typedecl_ops.rs:522`).

**Role side** — three body reads in `register_role_decl`: `check_role_body_our_scoped_decls`
(syntactic violation scan), `role_body_is_stub` (**the role plan has no D1-style `is_stub`
field** — a straight gap), and `walk_role_body`, whose arms are HasDecl (D2b-2 closes),
DoesDecl (D4/D7), MethodDecl (done), stub-call detection (same missing `is_stub`), and the
catch-all that **clones every other statement into `RoleDef::deferred_body_stmts`** — the one
place a plan body escapes registration into the registry, executed per composition at five
`run_block_raw` consumer sites. Dropping the role field therefore **requires D8 first** (the
deferred-body chunk mechanism, `todo/deep/adr0019-d7-d8-role-plan-encoding.md`), which is why
D9 is sequenced after D8 in the ADR.

## Design: a typed, ordered body plan with per-statement compiled chunks

The endgame shape for both sides is the same: replace `legacy_body: Vec<Stmt>` with an ordered
op list lowered at compile time —

```
CompiledClassDeclPlan::body_plan: Vec<ClassBodyOp>
ClassBodyOp = Attr { name: Symbol }            // joins the name-keyed attr_decls row (D2b-2)
            | Method                            // advances the existing method cursor (D3-1/D3-7)
            | Does { name: Symbol, args: Vec<DeclTraitArg> }   // D4/D7 chunks
            | CodeAlias { alias: Symbol, source: Symbol }
            | ProtoMethod(CompiledProtoDeclPlan-shaped payload)
            | LeavePhaser(CompiledDeclExpr)
            | ClassSub { name: Symbol }         // the SubDecl tail probe fact + Other chunk
            | Other { chunk: CompiledDeclExpr, raw: Option<Stmt> }
```

with `Other.chunk` a statement-shaped `CompiledDeclExpr` (the C5 struct compiles any `&[Stmt]`
— nothing limits it to one expression) compiled once at plan lowering, replacing the
per-registration `run_block_raw` OTF compile. The driver keeps its exact current structure —
same per-op env seeding, BEGIN/EVAL swallowing, writeback, re-publish — only the *source* of
each step changes from raw `Stmt` to typed op. The `TrustsDecl` arm is deleted (redundant);
`persist_class_body_statics`' re-scan becomes a `declared_static_names: Vec<Symbol>` plan field
(D2a pattern); the lowering mirrors `collect_nested_class_has_decls`' append-at-end order for
nested-sub attrs so `Attr` ops fire in the runtime walk's order.

**The token/rule exclusion is carried by `Other.raw`**: per the phase preamble, D6/D9 exclude
token/rule arms until their ADR-0009-scoped slice. A lowered `Other` op whose statement is a
`TokenDecl`/`RuleDecl` keeps the raw `Stmt` beside (or instead of) the chunk and the driver
routes it through today's `run_block_raw` path; every other statement kind drops its raw copy.
The field-drop slice then shrinks `legacy_body` to exactly the token/rule raws — either an
`Option`al rump field or the `Other.raw` payload — so "drop `legacy_body`" and "token/rule
stays" are not in tension, mirroring how C6 scoped `FunctionDef.body` to token defs.

**BEGIN semantics note**: a `BEGIN` phaser inside a class body currently executes during the
body walk at registration time (not at main-pass compile time), and the chunk design preserves
exactly that — the chunk is *compiled* earlier but still *executed* at registration. No
observable timing change; anything more raku-faithful (true compile-time BEGIN) is out of
ADR-0019's scope.

## Slices

- **D6-1 — cheap syntactic plan fields + dead arm.** `declared_static_names` on the class plan
  (consumed by `persist_class_body_statics`); delete the redundant `TrustsDecl` walk arm.
  Role twins in **D9-1**: `is_stub` and the our-scope-violation verdict precomputed on the role
  plan (retiring `role_body_is_stub` + `check_role_body_our_scoped_decls`'s AST scans).
- **D6-2** = D2b-2 (attr plan lowering, shared with the D2 remainder).
- **D6-3 — `body_plan` introduction, additive.** Lower the op list alongside `legacy_body`;
  the driver consumes ops but the field remains for fallback parity, with an env-var instrument
  (C6e-3a's `MUTSU_DROP_LEGACY_BODY` precedent) that forces op-only execution for validation
  sweeps. This is the big slice; expect it to subdivide per arm the way C6d did
  (other-stmt chunks first, then code-alias/proto/leave-phaser).

  **Tentative sub-slice breakdown (2026-08-09, no code landed), mirroring D3-8a-d's
  additive-then-cutover shape:**
  - **D6-3a — skeleton, fully additive.** Define `ClassBodyOp` (the shape already sketched
    above) and the `body_plan: Vec<ClassBodyOp>` field; lower every flattened body statement to
    an op in source order. The already-typed arms (`Attr`/`Method`/`Does`/`ClassSub`) carry only
    a name/marker — their real payload stays in `attr_decls`/`method_decls`/
    `parent_arg_chunks`, which `body_plan` just orders a cursor-advance against, so this is cheap.
    `Other`/`ProtoMethod`/`CodeAlias`/`LeavePhaser` initially carry `chunk: None` and the raw
    `Stmt` only (no compiled chunk yet) — `body_plan.len()` matching the flattened statement
    count is the checkable invariant, pinned by a compiler unit test. Nothing reads the field;
    zero behavior risk, same class as D3-8a/D7-1/D6-1.
  - **D6-3b — compile `Other` chunks.** The largest reader (per the inventory table above) and
    the highest-value target: generalize `compile_decl_expr_inner` (currently wraps one `Expr`
    into a one-statement body) to accept an arbitrary `&Stmt` directly, and populate
    `Other.chunk` for every non-token/rule statement. Still additive — the driver keeps reading
    `legacy_body`.
  - **D6-3c — compile the remaining small arms.** `CodeAlias`/`ProtoMethod`/`LeavePhaser` chunks
    (each far smaller than `Other`; `ProtoMethod` may reuse `CompiledProtoDeclPlan`'s existing
    shape rather than inventing a new one). `body_plan` is now a complete, compiled mirror of
    `legacy_body` with zero consumers.
  - **D6-3d — driver cutover, instrument-gated.** `run_class_body` switches its statement source
    from `legacy_body` to `body_plan`, behind the `MUTSU_DROP_LEGACY_BODY`-style env var
    (C6e-3a precedent) forcing op-only execution for validation sweeps; verify with the full `t/`
    suite + roast whitelist + the bundled-battery gate (`scripts/battery-testsuite.sh`, since
    class bodies with nested `use`/BEGIN/EVAL are load-bearing for several batteries) under the
    forced-instrument env var, then flip the default. Per C6e-3c's precedent this step alone may
    need to subdivide further once the real per-op driver rewiring is in front of the diff.
  - **D6-3e — token/rule carve-out check.** Confirm `Other.raw` is the only thing left populated
    for a `TokenDecl`/`RuleDecl` statement (per the phase preamble's ADR-0009 exclusion) and that
    the driver still routes those through today's `run_block_raw` path unchanged. Likely folds
    into D6-3d rather than needing its own PR — flagged here so it isn't silently dropped.

  These boundaries are a starting estimate, not a commitment — like D3-8's own slice plan, expect
  the real diff to reveal a different natural cut line once D6-3a is in hand.
- **D6-4 — drop the class field** (modulo the token/rule rump) after a forced-instrument run
  of the full `t/` suite and the roast whitelist, per the C6e-3c playbook. D5's verification
  gate (OO::Monitors battery + metamodel roast) rides on D6-3/D6-4.
- **D9-2** = the role `HasDecl` cutover (D2b-2's role half) and **D9-3** the typed `DoesDecl`
  (D7's role-structure ops). **D9-4** = D8's deferred-body chunks replacing
  `deferred_body_stmts`. **D9-5** — drop the role field, same instrument playbook.

## D10 — walker deletion

D10 needs no separate mechanism design: once D6-4/D9-5 land, `run_class_body`/`walk_role_body`
*are* the plan-op executors and the "walkers" left to delete are the residual raw-`Stmt` match
arms and any helper whose only caller was an AST arm. Completion criteria: (1) no
`Stmt::`-matching code remains in `registration_class_*`/`registration_role_*` except the
token/rule routing and the `stmt_pool`-fed augment walker (both explicitly retained); (2) the
metadata helpers that survive (`collect_nested_class_has_decls`' compile-time mirror, prescans)
live compiler-side only; (3) grep proves no runtime `CompiledAttrDecl::from_stmt`/
`CompiledMethodDecl::from_stmt` caller remains outside augment/EVAL fallbacks. D10 is a
cleanup PR, not a campaign — if it grows beyond that, a slice was landed incompletely.

## Dependency order across the remaining Phase D boxes

D2b-2 → D6-1..3 (class body plan) — with D3-8a-d and D4-1/2 landable in parallel —
then D4-3, then D7 (role structure ops) → D8 (deferred-body chunks) → D9 → D6-4/D9-5 field
drops → D5 verification gate → D10 cleanup.
