# ADR-0019: Compile declarations and unify method dispatch entries

- **Status**: Proposed
- **Date**: 2026-08-03
- **Related**: [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md),
  [ANALYSIS.md §1.1, §3.3, §4-1](../../ANALYSIS.md)

## Context

Routine bodies already execute as bytecode, but `RegisterSub`, `RegisterClass`, and
`RegisterRole` still index `CompiledCode::stmt_pool` and hand an AST declaration to the runtime.
The runtime then walks that declaration to build routine, attribute, role-composition, and MOP
state. This duplicates sub registration (an AST body and a separately compiled body), makes the
MOP protocol depend on the legacy registration walker, and keeps declaration semantics outside
the compiler/VM boundary.

Method lookup has the same split at a different layer. Native methods are recognized by several
arity-specific string matches, user candidates enter through `run_instance_method` variants, and
built-in introspection maintains a candidate-name universe plus abstract/slow-path lists. A method
can therefore be callable without being introspectable, or introspectable without sharing the
actual dispatch entry that answers the call. The drift tests detect examples after the fact but do
not make drift impossible.

The two problems meet at user HOWs: declaration installs methods through MOP calls, while method
dispatch and introspection have no single entry representing the installed result.

## Decision

### 1. Declarations compile to immutable plans

The compiler lowers every sub, class, and role declaration to a typed immutable
`CompiledDeclPlan`, stored in a declaration pool on `CompiledCode`. `RegisterDecl(plan_index)` is
the only general declaration-registration opcode. A plan is a linear sequence of semantic
declaration operations, including:

- begin or reopen a package/type;
- install a compiled routine candidate;
- install an attribute descriptor;
- apply inheritance or role composition;
- run a compiled declaration-time expression or body chunk;
- invoke a custom trait or user-HOW operation; and
- publish lexical, package, export, and version aliases.

Runtime-dependent expressions (computed names, defaults, trait arguments, and user HOW calls) are
compiled child chunks referenced by the plan. The VM executes those chunks through its normal
re-entrant bytecode entry. A declaration plan may contain data descriptors, but it must not retain
the source `Stmt` or an executable AST body. `EVAL` follows the same parse -> compile -> bytecode
path and is not an exception.

`RegisterSub`/`RegisterClass`/`RegisterRole` remain temporarily while their individual plans are
migrated, then are removed together with declaration-shaped entries in `stmt_pool`. Enum and subset
registration can adopt the same representation later; they do not block retiring the three
tree-walking paths named in ANALYSIS §1.1. Three more declaration opcodes are outside the three
named paths but inside this decision's end state: `RegisterProtoSub` and `RegisterProtoToken`
still index `stmt_pool` and are migrated by slice C8, while `RegisterToken` carries a *regex*
body whose execution model is ADR-0009's — it is waived here the same way enum/subset are and
adopts a typed plan together with the grammar-token work scoped in C6d-2 and Phase D's token
note.

### 2. One registry owns every type×method entry

The runtime registry owns a canonical table keyed by `(TypeId, Symbol)`. Each value is a
`MethodEntry` containing:

- the method's visibility and ownership metadata;
- accepted arity/signature descriptors;
- one or more ordered user candidates, or a native handler identifier;
- submethod/multi/wrap metadata; and
- the data needed to construct `Method` objects for introspection.

Built-in type IDs and native entries are generated from one static catalog. User declaration plans
insert candidates into that same table. Inheritance/MRO resolution maps a concrete receiver to an
ordered sequence of type IDs and probes the table; it does not select a second dispatch mechanism.
`samewith`/`nextsame` carry a cursor into the resolved candidate sequence rather than re-entering a
separate resolver.

All public entry points (`CallMethod` opcodes, mutation-aware calls, metaobject calls, and the
compatibility carrier used by re-entrant evaluation) call this resolver. Arity-specific native
functions become handlers selected by `MethodEntry`; they are implementation details, not lookup
entry points.

`.^methods`, `.^can`, and the method portion of `.^mro` derive from the same table and MRO walk.
There is no probe-based `METHOD_UNIVERSE` and no per-type method-name list after the migration.
Attributes and parent edges remain type metadata beside the method table rather than pretending to
be methods.

### 3. Cache invalidation is generation-based

Every mutation of type metadata or the method table advances the registry's method generation.
Resolved-call caches are keyed by receiver type, method symbol, call shape, and generation. MOP
mutation, augmentation, role composition, wrapping, and ordinary declarations therefore share one
invalidation rule. Code must not manually clear a collection of partially overlapping method
caches at individual registration sites.

## Migration order

1. Add typed declaration-plan pools and migrate `SubDecl`. This removes the duplicated AST body
   while preserving the existing routine registry as an adapter.
2. Migrate class/role structural registration and compiled declaration-time expression chunks.
3. Add the canonical method table and import native entries from a single static catalog.
4. Make user declaration plans install into the table; route all call entry points through its
   resolver and generation cache.
5. Derive introspection, remove the hand/probe tables and compatibility adapters, then delete the
   old `Register*` opcodes and AST registration walkers.

Each stage must preserve source order, lexical capture cells, redeclaration errors, MOP interception,
role conflict rules, multi dispatch, wraps, and re-entrant `EVAL`. Intermediate stages are allowed
to adapt a compiled plan into an existing registry operation, but must not introduce a new AST
fallback.

## Execution plan and progress

This checklist is the operational source of truth for the migration. **One checkbox is one PR.** A
box is checked only after that PR has merged to `main` with required CI green. Reverted work is
unchecked even if its original PR merged. PRs are sequential branches from the then-current
`main`; this is not a stacked-PR plan.

**Current progress: 35/53 slices merged (C6, C7, C8, D1, D2d, and D7 complete; D2a and D2c-1/2/3
also landed, 2026-08-07; D2b-2, D2c-4, D6-1, D7-1/D9-1, D4-1, D4-2, D4-3, D7-3, and D3-8a landed
2026-08-08; D3-8b, D3-8c, D3-8d, D7-4, D8-1, D8-2, and D8-3 landed 2026-08-09). Phase C is fully checked; the open box is
D2 (attributes and generated accessors), subdivided D2a-D2d — D2a, D2b-2, D2c-1/2/3/4, and D2d are
done; only the optional D2c-5 (A/B env-setup unification, gated on raku-behavior verification of
shape B's `has_class_scoped_subs` gate) remains open in D2. D3 (class methods/submethods as compiled candidates) is open;
D3-1 through D3-7 landed (walker-drift unification plus the compile-time `CompiledMethodDecl`
precompute), D3-8a, D3-8b, D3-8c, and D3-8d also landed (the additive compiler-side half, the
class-walker and role-walker install-by-key cutovers, and the fallback-narrowing survey — which
found and fixed a real closure-nesting gap rather than just a straggler list — of the method-body
main-pass compile; see below), and a 2026-08-08 scoping
pass found D3's literal goal — compiling method *bodies*
through the single main-pass `Compiler` the way `SubDecl` does, instead of a throwaway
per-registration `Compiler::new()` — still fully open and scoped as a future D3-8, whose detailed
design (parity-first bare compile, per-decl `compiled_routine_key` on `CompiledMethodDecl`,
guarded registration install, D3-8a-d slice plan) landed 2026-08-08 as
`todo/deep/adr0019-d3-8-method-body-main-pass-compilation.md`. D4 (class
declaration-time expressions) was also scoped 2026-08-08, no code landed: its "aliases" piece is
closed as already-bytecode-native (a lateral move, not a gain), its "deferred class bodies" piece
folds into D8 rather than needing its own slice, and its "parent expressions" piece is a real
re-parse-per-registration bug but is gated on parser/AST work and constrained by a shared `&str`
resolver API also used for genuinely dynamic type-name concretization — scoped as future
D4-1/D4-2/D4-3. A 2026-08-08 design sweep then produced detailed designs for **every remaining
Phase D box** — the D2 remainder, D4, D5, D6, D7, D8, D9, and D10 — recorded as
`todo/deep/adr0019-d2-remainder-attr-plan-lowering.md`, `adr0019-d4-parent-expr-chunks.md`,
`adr0019-d5-plan-driven-how-ops.md`, `adr0019-d6-d9-legacy-body-removal.md` (includes the
grep-complete `legacy_body` reader inventory and D10), and
`adr0019-d7-d8-role-plan-encoding.md`, with condensed entries in each box below; the notable
re-scope is D5, which shrank to ordering invariants plus a verification gate riding on D6 (the
user-HOW protocol never reads a raw `Stmt`). The recommended cross-box order is D2b-2 →
D6-1..3 (with D3-8a-d and D4-1/2 in parallel) → D4-3 → D7 → D8 → D9 → the two field drops →
D5's gate → D10. D1 found most class structural data already typed-plan-driven
from Phase A3/A4; the two remaining body-scanning reads (stub detection, `Stmt::TrustsDecl`) are
now precomputed at plan lowering as `CompiledClassDeclPlan::is_stub`/`trusts`; see
`news/2026-08/d1-class-structural-plan-fields.md`. D2, unlike D1, found attribute data with no
existing typed-plan coverage at all; D2a took the same "precompute a re-derived body scan" pattern
for the two pure pre-scan facts (class own-attribute names, role own-attribute
names/used-modules/declared-types) — see `news/2026-08/d2a-attribute-prescan-plan-fields.md`.
C8 migrated `RegisterProtoSub`/`RegisterProtoToken` onto `RegisterDecl` and made a non-trivial
proto body compile its `{*}`-rewritten dispatch once, at declaration time, instead of on every
call; see `news/2026-08/c8-proto-declarations-compiled-plans.md`. C7 removed the last sub-shaped
AST-registration adapter: `preregister_top_level_subs` now installs a forward-declared sub through
`register_compiled_sub_decl` with an eagerly OTF-compiled routine instead of leaving `compiled`
unset for the first call to fill in; see
`news/2026-08/c7-forward-declaration-preregistration-compiles-eagerly.md`. C6's last sub-box,
C6e-3c, dropped `CompiledSubDeclPlan::legacy_body` for real once every keep-class was closed; see
`news/2026-08/legacy-body-field-dropped.md`. C6d's only open sub-box remains the
ADR-0009-scoped C6d-2, which does not gate C6 (token defs never come from
`CompiledSubDeclPlan`) — it stays open only for the later `FunctionDef.body` field deletion
(F7).**

The count tallies top-level boxes only; sub-boxes (C6a–C6e, C6d-1..5, E1a/E1b) are that box's
PRs, and a subdivided box is checked when its last sub-box merges. A box that turns out to need
subdivision follows C6's precedent: measure first, then split in place.

The migration is complete only when every required box below is checked and the completion gates
at the end pass. The order within a phase is intentional. A later phase may start when its stated
dependency is complete, but cleanup slices stay last so each intermediate `main` remains usable.

### Phase A — typed declaration entry (complete)

- [x] **A1 — Define the architecture and migration invariants.** Add this ADR and identify the
  declaration, dispatch, cache, introspection, and cleanup boundaries.
- [x] **A2 — Lower sub declarations into typed plans.** Move `SubDecl` registration operands out of
  the generic statement pool while retaining the routine-registry adapter.
- [x] **A3 — Lower class and role declarations into typed plans.** Cover source-order declarations
  and hoisted forward-reference shells.
- [x] **A4 — Consolidate declaration opcodes.** Replace the three declaration entry opcodes with
  `RegisterDecl(CompiledDeclPlanRef)` without growing `OpCode`.

### Phase B — canonical method-table write side (complete)

- [x] **B1 — Introduce canonical built-in type×method entries.** Give static catalog rows an owner,
  method symbol, and stable introspection order.
- [x] **B2 — Move built-in entries into `Registry`.** Seed the `(owner, method)` table without
  probing or invoking handlers during interpreter initialization.
- [x] **B3 — Add ordered user candidates to `MethodEntry`.** Make built-in and user candidates share
  one row without changing dispatch reads yet.
- [x] **B4 — Publish every user-method mutation.** Synchronize class registration, rollback,
  augmentation, role composition, MOP `add_method`, and EVAL restoration into the table.
- [x] **B5 — Add method-generation invalidation.** Advance one generation for table/type mutation
  and invalidate resolver/fast/multi/constructor/private caches from the read boundary.
- [x] **B6 — Move user-method presence and overload reads to `MethodEntry`.** Retain the class mirror
  only as a transitional write/compilation adapter.
- [x] **B7 — Remove the uncompiled class-mirror read fallback.** Compile mutation results before
  publication and make dispatch reads table-only.
- [x] **B8 — Close dynamic-owner writeback gaps.** Publish role pun and runtime mixin types, remove
  withdrawn pun entries, and transfer method entries to every nested test/EVAL VM.

### Phase C — sub declaration plans become bytecode-native

- [x] **C1 — Bind source-order sub plans to compiled routines.** Record stable primary/alternate
  compiled-function keys while keeping hoisted shells keyless.
- [x] **C2 — Preserve plan-to-routine identity across module import.** Remap compiled routine keys
  when a compunit is imported so nested/module declarations retain direct references.
- [x] **C3 — Install routine candidates from compiled references.** Add the temporary
  compiled-`FunctionDef` adapter and stop rediscovering compiled routines from name/signature keys.
- [x] **C4 — Precompute AST-derived routine metadata in the compiler.** Move auto-signature use,
  empty-signature, return-shape validation, stub/proto identity, and redeclaration fingerprint data
  required by registration into `CompiledSubDeclPlan`/`CompiledFunction`.
- [x] **C5 — Move sub custom-trait and computed-name evaluation to child chunks.** Execute those
  expressions through re-entrant bytecode, including EVAL and NativeCall declaration cases.
- [x] **C6 — Remove `CompiledSubDeclPlan::legacy_body`.** Make ordinary, multi, `our`, hoisted,
  exported, operator, and top-level-method declarations register without an executable AST body.
  The blocker is `FunctionDef.body`, which had 58 readers when C6 started. They fall into
  separable groups, each its own PR. The gate is scoped to what the plan field actually feeds: a
  token/rule `FunctionDef` never comes from `CompiledSubDeclPlan` (top level is `RegisterToken`
  → `stmt_pool`; a grammar body's tokens come from the Phase-D class walker), so the box is
  checked when **no sub-plan-derived def or code object retains or reads an AST body** —
  `FunctionDef.body` becomes optional, is empty for plan-derived defs, and survives for
  token/rule defs until the field itself is deleted with C6d-2/F7:
  - [x] **C6a — identity hashes.** Replace per-read `function_body_fingerprint(&def.…)` with a
    memoized `FunctionDef::body_fingerprint()`, retiring the `func_def_fp_cache` side cache.
  - [x] **C6b — OTF-gate body predicates.** Memoize `needs_interpreter` /
    `module_otf_needs_interpreter` (deleted in C6e-2c) / `declares_state` as `RoutineBodyFacts` on the def, behind the
    single reader `Interpreter::routine_body_facts`, and read the existing `is_stub` field on the
    redeclaration path. (`collect_routine_body_local_names` and rw-target extraction return AST
    data rather than facts; they move with C6c/C6d.)
  - [x] **C6c — `Value::make_sub` from a def.** A code object built from a registry routine now
    carries that routine's bytecode in `SubData::compiled_routine` (filled from
    `FunctionDef::compiled` by `Value::make_sub_for_routine`), and the two `Sub` dispatch paths
    that used to compile `data.body` on the fly read it instead. It needed no
    calling-convention change: both compile paths bake `param_local_slots` into the
    `CompiledCode`, and an empty upvalue array falls back to by-name env reads — see
    `news/2026-08/routine-code-objects-are-bytecode-backed.md`, which corrects the earlier
    scoping note.
  - [x] **C6d — interpreter execution sites.** Route `eval_block_value(&def.body)` /
    `run_block(&def.body)` through `def.compiled`. Surveyed by instrumenting all six sites and
    running the whole `t/` suite once (1148 hits — see
    `todo/deep/c6d-interpreter-body-sites-are-mostly-token-bodies.md`), which corrects the
    earlier "widen OTF coverage to every routine" framing: a routine whose body declares a class
    never reaches these sites, and the sites do not tree-walk — `run_block` recompiles the body
    per call, so what C6d removes is a repeated compile. The mismatch to solve is that
    `compile_block_raw` compiles a *block* whose arguments the caller already bound, while
    `def.compiled` binds its own from `param_local_slots`. (The six-site count itself turned
    out to be incomplete: C6d-5 found a seventh live site and an eighth negligible one — the
    grep to trust is for *all* `&def.body` / `&data.body` execution forms, not the two the
    survey instrumented.) Subdivide:
    - [x] **C6d-1 — the ordinary-routine tail** (`calls.rs:call_function_def`,
      `calls.rs:exec_call`): 192 hits over ~37 names. The shape is settled and is neither
      candidate above: these are *callers* reaching the interpreter entry `call_function_def`
      where a compiled entry already exists, so each one is rewired rather than re-compiled. The
      multi-deferral caller (`builtins_dispatch_next`, 102 of the 144
      `call_function_def` hits) landed first — see
      `news/2026-08/multi-deferral-runs-the-compiled-candidate.md`, which also records why the
      general `compile_and_call_function_def` entry cannot be used here (it re-pushes the
      multi-dispatch frame the chain owns, and the chain then defers to the same candidate
      forever). Every remaining caller then followed — `builtins_operators_fallback` (user
      operators), `builtins_operators_infix` (reduce), `builtins_operators_coerce` (hyper),
      `accessors_state`, `main_args` (`MAIN`) — through one shared
      `call_routine_def` entry, measured at -13.7% instructions / -22% wall on a reduce over a
      user multi operator (`news/2026-08/user-operators-run-their-compiled-body.md`, which also
      records that the *debug* build's instruction count inverts that ranking and must not be
      used to judge a dispatch-path change). `Interpreter::call_function_def` is now gone: its
      last gated shape, `multi_candidate_state_forces_interpreter`, was retired by installing
      multi candidates from their plan-compiled routines (see the implementation status below).
      The final piece — `calls.rs:exec_call`'s inlined copy of the retired
      `call_function_def`'s body (48 hits), including its own `run_block(&def.body)` — now
      delegates to `call_routine_def`, so statement-position calls run the routine's
      plan-attached bytecode through the same entry and writeback merge as expression-position
      calls (`news/2026-08/statement-calls-run-the-compiled-body.md`).
    - [ ] **C6d-2 — grammar token/rule bodies** (`dispatch.rs:eval_token_def`,
      `regex_token_resolve.rs`): 956 of the 1148 hits, but those `FunctionDef`s carry a regex
      body, so scope this against ADR-0009's execution model rather than the OTF gate. **This
      sub-box does not gate C6**: token defs are never built from `CompiledSubDeclPlan`, so it
      gates only the later deletion of the `FunctionDef.body` field itself (with F7). It stays
      listed here so the field's last reader class is not forgotten.
    - [x] **C6d-3 — the two sites dead across the whole suite**
      (`dispatch_proto_call.rs:call_proto_dispatch`, `types/roles.rs:run_role_submethod`); the
      latter's `def` is a `MethodDef`, so it is reassigned to Phase D. The former's proto-sub
      arm now delegates the candidate run to `call_routine_def`, keeping only the
      remaining-candidate/multi/samewith frames and the `X::Multi::NoMatch` construction, and
      is pinned for the first time by `t/proto-dispatch-interpreter-path.t`
      (`news/2026-08/proto-star-fallback-runs-compiled-candidate.md`). The `is rw` relay
      through a non-trivial proto body is a pre-existing gap, unchanged by the rewire —
      `todo/tickets/rw-writeback-through-nontrivial-proto-body-is-lost.md`.
    - [x] **C6d-4 — `call_sub_value`'s `eval_block_value(&data.body)`**, which takes a *code
      object's* body rather than a def's: after C6c that was the one path left that still
      executed a routine code object's AST, reached when a `.wrap` chain routes dispatch through
      the interpreter carrier (230 of the site's 9,574 fresh-survey hits; the rest are
      blocks/closures, which carry `compiled_code` and keep the carrier). Now runs
      `SubData::compiled_routine` via `call_compiled_closure`, gated off for scalar-rw/raw
      routines until rw binding is cell-based
      (`todo/tickets/rw-writeback-through-wrap-chain-needs-shared-cells.md` — the
      different-name wrap relay is broken on every path, `main` included). Fixed two general
      bugs along the way: the C6c value-dispatch path's missing rw-param slot flush, and both
      compiled_routine forks reclassifying a value call's binding failure as a compile-flavored
      `X::TypeCheck::Argument` (raku and roast S03-sequence/misc.t demand runtime
      `X::TypeCheck::Binding`) — `news/2026-08/routine-code-object-carrier-runs-bytecode.md`.
    - [x] **C6d-5 — `call_function_fallback`'s def arm**, a seventh `&def.body` execution site
      the original six-site survey missed (an eighth, `call_proxy_callback`, gets 2
      anonymous-block hits and is out of C6d scope): 410 hits across `t/`, dominated by
      multi-candidate names, `def.compiled` attached in essentially every hit. Folded to
      `call_routine_def` behind the same gate the OTF dispatch uses
      (`def_module_single_sig_body_ok_ignoring_state`); a gate-rejected def keeps the
      interpreter arm, which is load-bearing semantics for those shapes (the sigilless-scalar
      EVAL-boundary writeback of `t/sigilless-params.t` is why the gate exists) —
      `news/2026-08/fallback-def-arm-runs-compiled-body.md`.
  - [x] **C6e — redeclaration comparison and eager body facts, then drop the plan field.**
    Replace the two `body_debug_without_setline(&def.body)` comparisons (`registration_sub.rs`)
    with the plan's C4 redeclaration fingerprint, and fill `RoutineBodyFacts` eagerly at plan
    lowering — the C6b cache is lazy and still reads `def.body` on a miss, which a body-less def
    cannot serve. The proto `{*}` rewrite moved to C8: a proto def is built from `stmt_pool` by
    `RegisterProtoSub`, not from the sub plan, so it does not gate this field. Subdivided
    (measure-then-split): C6e-1 landed the identity hash + eager facts (#5952); C6e-2 kills the
    gate-rejected interpreter shapes — C6e-2a (landed, #5953) runs sigilless scalars compiled
    (`news/2026-08/sigilless-params-run-compiled.md`, which also surfaced and fixed the
    take-in-callee lazy-gather suspension bug,
    `news/2026-08/gather-take-in-callee-eager.md`), C6e-2b (landed) lifts the sub-signature
    exclusion (`news/2026-08/subsig-params-run-compiled.md` — parameter shapes no longer gate
    compilation at all), and C6e-2c (landed) lifts the last body exclusion — `start`
    bodies run compiled and the `module_otf_needs_interpreter` predicate is deleted
    outright (`news/2026-08/start-bodies-run-compiled.md` — the historical
    recursive-start param clobber no longer reproduces because the compiled caller-env
    merge excludes callee params); C6e-3 then seeds fingerprints and drops `legacy_body` —
    subdivided: C6e-3a (landed) seeds plan fingerprints (structural + registration
    identity), hardens every body-less code path, and validates the drop end-to-end
    under a `MUTSU_DROP_LEGACY_BODY=1` instrument
    (`news/2026-08/legacy-body-drop-groundwork.md`); C6e-3b (landed) makes the
    safe-class empty body the default at registration and retires the instrument
    (`news/2026-08/safe-class-empty-body-default.md`); C6e-3c dropped the field
    itself (2026-08-07) once the load-bearing classes were unblocked: no
    resolvable plan bytecode (class-walker method-nested subs, fixed by giving
    `MethodDef` its own `compiled_fns` carrier), rw/raw scalars' interpreter
    carrier (shared `ContainerRef` cells), lvalue routines (plan-recorded
    assign-target tail), NativeCall marshalling traits (measured zero live
    readers), runtime-resolved sub/method names (`sub ::($n) {...}`, which
    turned out to just need the early-return compiler skip removed — the
    compiled-routine lookup key is an internal symbol independent of the
    runtime-resolved name), and finally `SubData` (bare blocks/closures)
    gaining the same `compiled_fns` carrier `CompiledFunction`/`MethodDef`
    already had, so a `sub` nested inside a block invoked from a foreign
    compilation unit's compiled code (the `Test::Util` `group-of` shape)
    resolves its own bytecode instead of silently falling back to the
    interpreter. With every keep-class closed, a `MUTSU_FORCE_BODYLESS`
    instrument that unconditionally emptied every plan-derived body was
    validated against the full `t/` suite (27,755 tests) and the entire
    `make roast` whitelist (both green), and the field was then deleted for
    real — `make test`/`make roast` pass with it gone.
    `news/2026-08/legacy-body-field-dropped.md`.
- [x] **C7 — Remove the sub-registration AST adapter.** Delete dead sub-shaped walker branches and
  prove the routine registry never compiles a migrated declaration on demand. The one live adapter
  was `preregister_top_level_subs` (the forward-declaration pass), which built its temporary
  `FunctionDef` from the raw AST body pre-compile and left `compiled: None`, so the first call
  between a forward stub and its real body compiled on demand. It now installs through
  `register_compiled_sub_decl` with an eagerly OTF-compiled routine, which let three functions with
  no other caller — `register_sub_decl`, `register_sub_decl_fp`, `register_sub_decl_as_global` — be
  deleted outright. `news/2026-08/c7-forward-declaration-preregistration-compiles-eagerly.md`.
- [x] **C8 — Proto declarations register from typed plans.** Migrated `RegisterProtoSub` and
  `RegisterProtoToken` off `stmt_pool` onto `RegisterDecl` with two new `CompiledDeclPlanRef`
  variants: `Proto(u32)` indexing a `CompiledProtoDeclPlan` pool, and `ProtoToken(Symbol)` carrying
  its name inline (a `proto token`/`proto rule` LTM marker has no signature, body, or trait to
  lower). A non-trivial proto body's `{*}` is rewritten to `__PROTO_DISPATCH__()` and compiled once
  at declaration time (through the same `compile_sub_body` ordinary subs use); the VM's
  `vm_try_run_nontrivial_proto_body` now runs that bytecode directly instead of rewriting and
  OTF-compiling the AST on every call, keeping the old rewrite-and-OTF-compile path only as a
  defensive fallback for a hand-built `FunctionDef` that never went through plan registration.
  `CompiledProtoDeclPlan` still carries `legacy_body: Vec<Stmt>` — following
  `CompiledRoleDeclPlan`'s own precedent — because the pure-interpreter operator-fallback path
  (`call_proto_function`) and the triviality check
  (`vm_resolve_trivial_proto_candidate`) still need the raw body; dropping it is a later box.
  `news/2026-08/c8-proto-declarations-compiled-plans.md`.

### Phase D — class and role plans become bytecode-native

Two facts about the current walkers shape this phase. First, `register_class_decl` is a single
~2,500-line function (`registration_class_decl.rs`) whose concerns share about eight mutable
locals, and `register_role_decl` (~920 lines, `registration_role.rs`) has the same shape — the
D1–D6 cut lines run *through* those function bodies, not between functions, so D0 exists to make
the later slices independently landable. Second, the class walker also registers grammar
`token`/`rule` declarations, whose bodies are regexes: like C6d-2 they are scoped against
ADR-0009, so D6, D9, and D10 exclude the token/rule arms until that work lands — deleting the
walkers wholesale is not possible before then.

- [x] **D0 — Split the class/role walkers into named phases with no behavior change.** Extract
  `register_class_decl`'s sections (rollback snapshot, parent validation, role composition,
  punning, attribute pre-scan, body walk, custom-HOW install) and `register_role_decl`'s three
  passes into functions with explicit inputs, so D1–D9 replace one function at a time. Landed
  as fifteen files, hosts reduced to orchestrators (229/334 lines), with arm-level `continue`
  semantics made explicit via `ClassBodyFlow::{RunTail,SkipTail}` —
  `news/2026-08/class-role-walkers-split-into-phases.md`. Phase D also inherits
  `types/roles.rs:run_role_submethod` from C6d-3 (a `MethodDef` body-execution site, dead
  across the suite).
- [x] **D1 — Encode class structural operations.** Package open/reopen, parent edges, repr,
  visibility, and lexical/package aliases were already typed-plan-driven from Phase A3/A4
  (`exec_register_class_op` reads them straight off `CompiledClassDeclPlan`). The two remaining
  body-scanning reads — yada-stub detection (duplicated across `check_class_role_redeclaration` and
  the now-deleted `class_body_is_stub`) and the `Stmt::TrustsDecl` scan in `publish_class_shell` —
  are precomputed at plan lowering as `CompiledClassDeclPlan::is_stub`/`trusts`, threaded through
  `ClassDeclModifiers`. `news/2026-08/d1-class-structural-plan-fields.md`.
- [ ] **D2 — Encode attributes and generated accessors.** Compile defaults/constraints as child
  chunks and publish generated methods through the canonical table. Unlike D1, a pre-PR survey
  found attributes have **no** existing typed-plan coverage: `CompiledClassDeclPlan`/
  `CompiledRoleDeclPlan` carried zero attribute fields, registration walked `Stmt::HasDecl` at
  four independent sites, generated accessors were resolved by a special-cased runtime lookup
  (`class_introspection.rs`) rather than `MethodEntry` rows, and defaults were evaluated by raw
  `eval_block_value` at six sites — `CompiledDeclExpr` was not involved anywhere. So, following
  C6/D0's measure-then-split precedent, the box is subdivided:
  - [x] **D2a — Precompute body pre-scan facts.** The two runtime pre-scans that re-derive pure
    syntactic facts from the body on every registration — `run_class_body`'s directly-declared
    attribute-name set (used for `$!attr` validation, combining flattened top-level `has` with
    `has` nested inside a body `sub`) and `walk_role_body`'s combined attribute-name /
    `use`d-module / body-declared-type scan — move to the compiler as
    `CompiledClassDeclPlan::own_attribute_names` and
    `CompiledRoleDeclPlan::{own_attribute_names,body_used_modules,body_declared_types}`, threaded
    through `ClassDeclModifiers` and `register_role_decl` respectively. Registration still walks
    `legacy_body` for the actual `has`-arm dispatch (typing full attribute descriptors is D2b) —
    no behavior change, no new fallback. `news/2026-08/d2a-attribute-prescan-plan-fields.md`.
  - [ ] **D2b — Type full attribute descriptors.** Replace the `ClassAttributeDef` 7-tuple with a
    named struct and a compiler-lowered `CompiledAttrDecl` (mirroring `RuntimeHasDeclSpec`, which
    already covers the mainline/EVAL `has`-outside-class case) covering the full `Stmt::HasDecl`
    surface; make `class_body_has_decl`/`role_body_has_decl`/the augment arm consume it instead of
    re-destructuring the AST, and subsume `RuntimeHasDeclSpec`. **Partly landed 2026-08-07**:
    `CompiledAttrDecl` now exists (`src/opcode.rs`) as a typed mirror of `Stmt::HasDecl`'s full
    18-field surface, built once by `CompiledAttrDecl::from_stmt`; `class_body_has_decl`,
    `role_body_has_decl`, and the `augment class` `has` arm all consume it instead of each
    independently re-destructuring `Stmt::HasDecl` with a different subset of `_`-ignored fields,
    and `RuntimeHasDeclSpec` now wraps `{ decl: CompiledAttrDecl, error: Value }` instead of
    duplicating ten of the same fields — subsuming it as asked. What remains: descriptor
    construction is still runtime-side (`from_stmt` runs once per encountered `Stmt::HasDecl`
    while `class_body_has_decl`/`role_body_has_decl` walk `legacy_body`/`flattened_body` at
    registration time), not compiler-lowered into a `Vec<CompiledAttrDecl>` on
    `CompiledClassDeclPlan`/`CompiledRoleDeclPlan` the way D2a's `own_attribute_names` is. That
    step needs position-correlating the precomputed vec with the registration-time statement walk
    (nested-sub-declared attributes and `SyntheticBlock` flattening make the traversal order
    non-trivial to match) and is naturally forced by D6/D9 dropping `legacy_body` outright, since
    nothing will be left to walk on demand at that point. See
    `news/2026-08/d2b-compiled-attr-decl.md`.
  - [ ] **D2c — Compile defaults/constraints as child chunks.** Replace attribute
    `default`/`is_default`/`where_constraint` `Expr`s (including the `Expr`-valued role
    registry tables `role_attribute_default_exprs`/`role_class_level_attrs`/
    `class_attribute_default_exprs`) with `CompiledDeclExpr` run through `run_decl_expr`,
    collapsing the three near-duplicated env-setup blocks in `attr_build_defaults.rs`,
    `methods_object_default_ctor.rs`, and `methods_object_dispatch_new.rs`. A 2026-08-07
    research pass (`todo/deep/adr0019-d2c-attribute-default-chunks.md`) found the real
    footprint substantially larger than this paragraph implies — ≥15 eval sites across 5
    env-setup shapes, not 3 — and recommended a D2c-1/2/3 split.
    **D2c-1 partly landed 2026-08-07 (`is_default` only)**: `CompiledAttrDecl::is_default`
    is now a `DeclTraitArg` (`Literal`/`Compiled`/`Ast`, reusing the existing enum rather
    than inventing a parallel one) instead of a raw `Expr`. `Compiler::add_class_decl_plan`
    precompiles each own attribute's `is default(...)` argument into a name-keyed
    `Vec<(Symbol, DeclTraitArg)>` (`CompiledClassDeclPlan::is_default_chunks`), threaded
    through `ClassDeclModifiers`/`run_class_body`/`ClassBodyCx` to `class_body_has_decl`,
    which looks its attribute's chunk up **by name** rather than by registration-walk
    position — sidestepping the position-alignment risk the research pass flagged, at the
    cost of a linear per-attribute scan (fine at typical attribute counts). `default` and
    `where_constraint` are NOT migrated (still `Option<Expr>`, still feed
    `ClassAttributeDef`, itself unchanged) — `is_default` was chosen first because it is the
    only one of the three that is read-and-discarded at registration time rather than
    stored for later (construction-time) evaluation, so it did not require also migrating
    `ClassAttributeDef` and the ~15 downstream eval sites in the same PR. Only 2 of the 4
    `CompiledAttrDecl::from_stmt` call sites read `is_default` at all
    (`class_body_has_decl`, `role_body_has_decl`); the other two (the mainline/EVAL
    `has`-outside-class error path, `augment class`) never did. `role_body_has_decl` keeps
    stashing a raw `Expr` into the `role_attribute_default_exprs` registry table (D2c-3
    scope) via a new `DeclTraitArg::as_expr()` escape valve, since no compiled plan exists
    for that path yet. Remaining: `default`/`where_constraint` type-swap plus the ~15 eval
    sites (D2c-2), and the three role registry tables (D2c-3).
    **D2c-2 landed 2026-08-07**: `ClassAttributeDef.default`/`.where_constraint`
    (`src/runtime/mod.rs`) are now `Option<DeclTraitArg>` instead of `Option<Expr>`, and
    every one of the ~15 eval sites the research pass found (`attr_build_defaults.rs`,
    `methods_object_default_ctor.rs`, `methods_object_dispatch_new.rs` ×2,
    `methods_object_attr_constraints.rs` (`check_attribute_where_constraint`,
    `construct_proxy_subclass`), `methods_dispatch_new.rs`'s `dispatch_bless`,
    `types/roles.rs`'s mixin path, `types/role_mixin_class.rs::seed_mixin_role_attributes`,
    `methods_classhow_attribute.rs`'s `.^attributes` introspection, and
    `registration_class_augment.rs`'s CUnion raw-bytes constructor) now dispatches through
    `Interpreter::eval_decl_trait_arg`/`.literal()` instead of its own ad hoc
    `Expr::Literal` pattern match plus a bespoke `eval_block_value(&[Stmt::Expr(...)])`
    call. `DeferredAttrDefault.default` (`attr_build_defaults.rs`) and
    `eval_attr_default_expr` moved in lockstep since a deferred default is a straight
    move of the same field. Deliberately scoped down from the ADR text's aspiration,
    per the research pass's own risk note: this is a **pure mechanism unification, not
    a behavior change or a chunk-precompilation** — every site still constructs its
    `DeclTraitArg` as `Literal`/`Ast` (never `Compiled`), so `CompiledAttrDecl.default`/
    `.where_constraint` stay `Option<Expr>` and the near-duplicated env-setup blocks
    (shapes A/B/C in the research doc) were intentionally left un-collapsed rather than
    risk silently dropping one shape's binding (`methods_object_default_ctor.rs`'s
    shape-B gate on `has_class_scoped_subs` in particular still needs the raku-behavior
    verification the research pass flagged before it can be safely merged into shape A).
    Verified via the full `t/` suite plus every roast `S12-attributes`/`S14-roles`
    whitelisted file, all green with no output changes. Remaining for a later slice:
    precompiling `default`/`where_constraint` chunks the way `is_default_chunks` already
    does (the actual "child chunk" perf win — today's `Ast` variant still recompiles the
    default expression's bytecode on every construction, same as before this slice), and
    D2c-3 (the three role registry tables).
    **D2c-3 landed 2026-08-07**: the three `Expr`-valued role registry tables —
    `role_attribute_default_exprs`, `role_class_level_attrs`, `class_attribute_default_exprs`
    (`registry.rs`) — are now `DeclTraitArg`-valued, matching `ClassAttributeDef`. The write
    side (`registration_role_body.rs`) simplified rather than grew: `role_attribute_default_exprs`
    used to convert `decl.is_default` (already a `DeclTraitArg` since D2c-1) back to a raw
    `Expr` via the `DeclTraitArg::as_expr()` escape valve just to store it — it now stores
    `def_arg.clone()` directly, retiring that escape valve's only caller outside `Ast`-only
    paths. `role_class_level_attrs` still wraps `decl.default` (a `CompiledAttrDecl` field,
    still `Option<Expr>` — unaffected by D2b/D2c-2's scope) in `DeclTraitArg::Ast`, the same
    pattern D2c-2 used at its own `ClassAttributeDef` construction sites.
    `registration_class_compose.rs` (the role→class copy at composition) and all four eval
    sites the migration touched (`runtime_var_meta.rs`'s `class_attribute_default_with_role_fallback`
    and `apply_container_attribute_defaults`, `methods_call_dispatch.rs`'s role
    type-object class-level-attribute read) now call `eval_decl_trait_arg` instead of
    `eval_block_value(&[Stmt::Expr(...)])`. `methods_call_dispatch.rs`'s site is a fourth eval
    site the D2c research pass's original enumeration missed — found only because grepping
    the registry table names surfaces every reader regardless of eval mechanism, unlike a
    field-by-field code search. Same verification as D2c-2 (full `t/`, `S12-attributes`/
    `S14-roles` roast whitelist), all green. With D2c-1 through D2c-3 landed, no
    `ClassAttributeDef`/role-registry attribute-default or `where`-constraint path in the
    interpreter still evaluates through a raw `Expr` + `eval_block_value` call; the only
    remaining piece of the parent D2c box is the actual bytecode precompilation (the
    `Compiled` variant is still unused for `default`/`where_constraint`/the role tables).
  - [x] **D2d — Publish generated accessors through the canonical table.** Give `MethodEntry` an
    accessor arm populated from `ClassDef::attributes` in `sync_user_method_entries`, so
    `has_public_accessor`/`resolve_user_method_or_accessor` (`class_introspection.rs`) and the
    `.^methods`/`.^can`/`.^attributes` synthesis sites become table probes riding the existing
    generation-bump invalidation instead of MRO×attribute-vector scans. Independently landable
    (does not depend on D2b/D2c — `ClassDef::attributes` is already populated by registration).
    **Partly landed 2026-08-07**: `MethodEntry` gained an `accessor: Option<bool>` arm, populated
    in `sync_user_method_entries` alongside the existing `user_candidates` loop (same generation
    bump, no new invalidation hook needed — Phase B's scheme was built write-path-agnostic).
    `has_public_accessor` now probes it (`Registry::accessor_is_public`) per MRO level instead of
    scanning each class's `attributes` vector. See `news/2026-08/d2d-accessor-method-entry.md`.
    **Second slice landed 2026-08-07**: `resolve_user_method_or_accessor`'s per-MRO-level
    `class_def.methods.get(...)` scan (with its own inline `is_private`/`is_my`/`role_origin`
    filtering, duplicating `sync_user_method_entries`' write-side logic) and
    `class_def.attributes.iter().any(...)` scan are now table probes too:
    `Registry::user_method_local_role_presence` (new — returns the two booleans directly instead
    of cloning `Vec<MethodDef>` the way `user_method_overloads` does, since this sits on the
    method/accessor dispatch race) and the already-existing `accessor_is_public`. Only the
    `registry.classes.get(cn)` branch moved; the `registry.roles.get(cn)` branch (a punned role
    used directly as a parent class) is untouched — general roles are not guaranteed to have a
    synced `method_entries` row the way a class always does, so migrating it needs its own
    verification pass. `native_methods.contains(...)` also stays as-is (a separate `HashSet`, out
    of D2d's scope).
    **Closed as-is 2026-08-07, `.^methods`/`.^can`/`.^attributes` synthesis deliberately left
    unmigrated**: those sites (`methods_classhow_method_obj.rs`'s `collect_class_methods`/
    `class_method_table`/`collect_can_methods`, `methods_classhow_attribute.rs`'s
    `collect_attribute_objects`) don't fit the table-probe shape the earlier two migrations used.
    Both migrated call sites were **single-key point lookups** (`(owner, method_name)`) racing on
    a per-dispatch-call hot path, where `MethodEntry`'s `(owner, name)` keying is a direct win. The
    remaining sites instead **enumerate every method/attribute a class declares** to build full
    `Method`/`Attribute` meta-objects (params, body, signature, custom trait state) for
    introspection (`.^methods`, `.^can`, `.^attributes` — not hot dispatch paths). `method_entries`
    has no owner-keyed enumeration index (`builtin_method_names` already pays an O(all methods in
    the program) full-map scan for the same reason, for the builtin side), so migrating would mean
    either adding one, or reading `class_def.methods`/`class_def.attributes` for enumeration while
    only using `method_entries` for name lookups — a split that adds indirection without removing
    any actual duplication: unlike D2b's four independently-drifted `Stmt::HasDecl` destructuring
    sites, `ClassDef::methods`/`ClassDef::attributes` and `MethodEntry.user_candidates`/`.accessor`
    are already a single source of truth kept in lockstep by `sync_user_method_entries` — reading
    one over the other here is a lateral move, not a mechanism unification, so it does not meet the
    ADR's own "gain" bar (see CLAUDE.md's "What gain and risk actually mean"). D2d is done.
  **D2 remainder design pass done 2026-08-08 (no code landed):**
  `todo/deep/adr0019-d2-remainder-attr-plan-lowering.md`. The D2b position-correlation blocker
  is now precisely characterized (the runtime walk *appends* nested-sub `has` decls while the
  compiler collector *interleaves* them — genuinely different orders, plus a latent double-push
  in the collector), so the plan lowering is **name-keyed** (`attr_decls: Vec<(Symbol,
  CompiledAttrDecl)>`, the `is_default_chunks` precedent) with `from_stmt` kept as the guarded
  fallback (slice D2b-2, also a D6/D9 prerequisite). For the D2c remainder, every default/where
  eval site already funnels through `eval_decl_trait_arg`, so flipping to
  `DeclTraitArg::Compiled` needs no eval-site changes — the work is construction-side plus
  retiring the two `as_expr` consumers that would panic on `Compiled` in the same slice
  (D2c-4); the A/B env-setup unification stays optional (D2c-5) behind raku verification of
  shape B's `has_class_scoped_subs` gate.
  **D2b-2 landed 2026-08-08**: `CompiledClassDeclPlan`/`CompiledRoleDeclPlan` both gained
  `attr_decls: Vec<(Symbol, CompiledAttrDecl)>`, replacing the class-only `is_default_chunks`
  field. The class-side collector (`compile_class_attr_decls`/`collect_nested_class_attr_decls`
  in `compiler/decl_plan.rs`) mirrors `class_own_attribute_names`'s proven-correct traversal
  (SyntheticBlock-flattened top level, `has` nested inside a body `sub` surfaced via a
  non-recursing-into-itself second pass) instead of the old `collect_attr_is_default_chunks`
  shape, which fixes the double-push the design pass found: the old code both pushed a
  nested-sub `has ... is default` from the `SubDecl` arm's direct loop AND re-matched it on
  the immediately following recursive call into the same statements. `class_body_has_decl`/
  `role_body_has_decl` now look their current `Stmt::HasDecl` up in `cx.attr_decls` by name
  first, falling back to `CompiledAttrDecl::from_stmt` only on a miss (a class-level
  `our`/`my` attribute, which the collector excludes exactly like `own_attribute_names`
  already does, or a registration path with no compiled plan — `augment class`, role-pun/mixin
  synthesis). The role side gains attribute-descriptor plan lowering for the first time (roles
  never had `is_default_chunks`); its `is default(...)` argument deliberately stays
  `DeclTraitArg::Ast` (no new compile step — `role_body_has_decl` already only stashed the raw
  expression for later composition-time evaluation), so this slice is a pure construction-side
  move on both plans, not a behavior change. Verified via the full `t/` suite (27,942 tests) and
  every whitelisted `S12-attributes`/`S14-roles` roast file (36 files), plus a manual raku-vs-mutsu
  comparison covering a nested-sub `is default`, a role instance-attribute default, and a role
  `my`-scoped class-level attribute — all four values matched exactly.
  **D2c-4 landed 2026-08-08**: `CompiledAttrDecl::default`/`where_constraint` are now
  `Option<DeclTraitArg>` (matching `is_default` and `ClassAttributeDef`'s own D2c-2 field type)
  instead of raw `Option<Expr>`, precompiled to `Literal`/`Compiled` chunks at plan lowering for
  both class and role `attr_decls` (roles gain this for the first time — D2b-2 deliberately left
  it `Ast`-only). `from_stmt` gained an `AttrDeclChunks` override struct (`is_default`/`default`/
  `where_constraint`) replacing the single `is_default_chunk: Option<&DeclTraitArg>` parameter.
  Retired the two `DeclTraitArg::as_expr()` consumers the D2-remainder design doc flagged as
  needing to land in the same slice: the shaped-`@`-attribute pattern match
  (`extract_shape_from_default`) moved to a compile-time-precomputed `CompiledAttrDecl`/
  `ClassAttributeDef::declared_shape: Option<Vec<usize>>` field (the D2a precompute pattern,
  ported to `opcode.rs` as free functions since it had exactly one caller); the `.^attributes.build`
  introspection closure now branches on `DeclTraitArg::Compiled` to build its `SubData` directly
  from the chunk's `compiled_code`/`compiled_fns` instead of calling `.as_expr()` (which panics on
  `Compiled`).
  **Two real regressions surfaced during verification, both architectural gaps the slice exposed
  rather than introduced, both fixed rather than routed around:**
  (1) `run_decl_expr` (the `Compiled`-chunk execution entry) lacked the topic (`$_`) save/restore
  `vm_eval_block_value` already carries for the `Ast` path (#6071) — `has Bool $.b` (no explicit
  default) synthesizes an implicit "unset typed attribute" default `Expr` that, once compiled to
  `Compiled` by this slice, escaped through `run_decl_expr` and clobbered the caller's `$_`
  (`t/decl-time-value-block-keeps-the-topic.t`, caught by `make test`, not the targeted roast
  sweep). Fixed by giving `run_decl_expr` the identical save/restore, factored into a shared
  `run_decl_code` helper both `run_decl_expr` and `vm_eval_block_value` now call.
  (2) The `.^attributes.build` closure's `Compiled`-chunk `SubData` returned `Nil` when actually
  invoked (`roast/S12-introspection/attributes.t`'s `.build().(C, $_)`): `compile_decl_expr`
  produces a standalone "value block" `CompiledCode` (no signature, no `Return`-based call ABI)
  meant only for direct execution via `run_nested` — feeding it to `SubData.compiled_code` made
  the general call path (`vm_call_on_value`) try to invoke it through `call_compiled_closure`,
  which expects the closure/routine calling convention and silently returned `Nil` for this shape.
  A same-shape signal (`body.is_empty()`) looked sufficient but was NOT reliable — an ordinary
  `sub (Int $x) {}` also has an empty body and must still go through `call_compiled_closure` to
  type-check its arguments, so trusting it regressed `t/exception-types.t`'s binding-error tests.
  Fixed properly with a new `SubData::is_decl_expr_thunk: bool` marker (touching ~13
  construction sites, `false` everywhere except this one), and a `vm_call_on_value` arm that
  routes a marked thunk through the new shared `run_decl_code` instead of
  `call_compiled_closure`, ignoring call args exactly as the on-demand-compiled AST-body Sub it
  replaces already did. Re-verified via the full `t/` suite (27,949 tests, two more than D2b-2 —
  an unrelated same-day `main` merge), every whitelisted `S12-attributes`/`S14-roles`/
  `S09-typed-arrays`/`S12-construction`/`S12-meta`/`S06-signature` roast file (102 files) with the
  release binary, and a manual raku-vs-mutsu comparison of a construction-in-a-loop (confirming no
  per-construction recompile) covering class/role `default`/`where`/`is default`.
- [ ] **D3 — Encode class methods and submethods as compiled candidates.** Install ordinary, multi,
  proto, private, rw, wrap, BUILD, and TWEAK metadata without walking `Stmt::MethodDecl`. That
  walk exists in three places, not one — the class walker (~508 lines), the role walker
  (~263 lines), and augmentation (`registration_class_augment.rs`) — plus a fourth reader in
  `registration.rs`; expect this box to subdivide per walker (class/role/augment).
  **Scoping pass done 2026-08-07 (no code beyond the drift fix below).** No `CompiledMethodDecl`
  type exists yet — D3 would invent one from scratch, the way D2b invented `CompiledAttrDecl`. The
  three walkers are `Interpreter::class_body_method_decl`
  (`registration_class_body_method.rs`, ~408 lines, plus a ~209-line param-forms helper file —
  together the ADR's "~508 lines"), `Interpreter::role_body_method_decl`
  (`registration_role_method.rs`, ~274 lines), and `Interpreter::augment_class`'s `MethodDecl` arm
  (`registration_class_augment.rs`, ~105 lines inline). Each hand-builds a `MethodDef`
  (`decl_types.rs`) from its own `let Stmt::MethodDecl { .. } = stmt` destructure — no shared
  constructor. Confirmed drift between the three (the same class of independently-diverged
  destructuring D2b fixed for `Stmt::HasDecl`):
  - `MethodDef.is_my`'s semantics: the class walker deliberately stores `*is_submethod`, not the
    raw parser `is_my` flag, because `my method`/`our method` are filtered out of `class_def.methods`
    before insertion (`is_lexical_only`/`is_our_only` gating) — so by construction every stored
    `MethodDef` only needs `is_my` to mean "is this a submethod" for inheritance filtering. The role
    walker matches this convention. `augment_class` stores the **raw** `*is_my` instead, with no
    equivalent lexical-only gating (it inserts every method unconditionally) — a real inconsistency,
    but hard to demonstrate as user-visible: most read sites OR `is_my` with `is_submethod`
    (`methods_walk.rs`, `class_dispatch.rs`, `ctor_phase_plan.rs`), which masks it; the two sites
    that read `is_my` alone (`resolve_user_method_or_accessor`'s per-MRO-level filter, reached only
    from `methods_mut_method_lvalue.rs`/`methods_instance_ops.rs`'s narrow rw/lvalue accessor-race
    path) could theoretically be affected but no observable repro was found (`.can()` and a direct
    call on an `augment`-declared submethod both correctly rejected inheritance in manual testing) —
    left unfixed pending a real repro rather than guessing at a fix for unconfirmed behavior.
  - `deprecated_message` was dropped (hard-coded `None`) by both the role and augment walkers,
    unlike the class walker. **Fixed 2026-08-07** (`news/2026-08/role-augment-method-deprecated-message-dropped.md`)
    — confirmed against `raku` as a real, user-visible gap (a role-composed or `augment`-declared
    method's `is DEPRECATED(...)` silently produced no deprecation report at all).
  - BUILD/TWEAK `:$!attr`-undeclared-attribute validation, `custom_traits` (native/`trait_mod:<is>`
    dispatch, including `.wrap`-installing traits), and `is_export`/`export_tags` handling all exist
    only at the class walker — absent from both the role walker and `augment_class`.
  - `handles` forwarder synthesis exists at the class and role walkers but not `augment_class`.
  - The class walker's `is_lexical_only`/`is_our_only` gating (excluding `my method`/`our method`
    from `class_def.methods`) has no `augment_class` equivalent — every method is inserted there
    regardless of `is_my`/`is_our`.
  - Duplicate-method detection is privacy-aware at the class and role walkers (compares
    `is_private`) but not at `augment_class` (`all_from_role` only).
  A fourth reader, `registration.rs`'s `validate_private_access_in_stmt`, only recurses into a
  method's `body` for private-call permission checks — it does not build a `MethodDef` and is
  unaffected by the drift above.
  Complications for a compiled plan: `SyntheticBlock` flattening is single-level and consistent
  across all three walkers (not drift). Methods declared inside a nested `sub`
  (`class C { sub f { method inner {...} } }`) have **no** nested-collector equivalent to
  `collect_nested_class_has_decls`/`compile_attr_is_default_chunks`'s attribute handling — such a
  method is simply invisible to registration today, an existing gap a compiled plan must either
  preserve explicitly or fix, not silently change. `name_expr` (computed method names) is evaluated
  identically at all three sites via `eval_block_value` with no compiled-chunk equivalent yet
  (unlike `SubDecl`/`ClassDecl`, which already get a `name_chunk` via `compile_decl_expr`).
  **Recommended first slice (D3-1):** precompile `name_expr` into a `name_chunk` the same way
  `add_sub_decl_plan`/`add_class_decl_plan` already do, since it is read-and-discarded immediately
  (never stored on `MethodDef`) and is the most literally-duplicated-verbatim logic across all
  three sites — a clean, low-risk demonstration slice that doesn't touch `MethodDef`'s shape.
  **D3-1 landed 2026-08-07** for the two walkers with an existing declaration plan:
  `CompiledClassDeclPlan`/`CompiledRoleDeclPlan` gained a `method_name_chunks:
  Vec<Option<CompiledDeclExpr>>`, one entry per top-level `method`/`submethod` statement
  precompiled by `Compiler::compile_method_name_chunks` (mirroring the exact `SyntheticBlock`
  flattening `run_class_body`/`walk_role_body` already perform), and `class_body_method_decl`/
  `role_body_method_decl` read the chunk at that statement's position via a cursor threaded through
  `ClassBodyCx`/`RoleDeclCx` instead of recompiling `name_expr` from a cloned AST node on every
  registration. Position, not name, is the key: unlike D2c-1's attribute `is_default` chunks (keyed
  by the attribute's own unique name), a method's fallback `name: Symbol` is not reliable to key on
  — an indirect declaration with a non-literal expression falls back to a shared placeholder, and
  ordinary `multi` methods legitimately share a literal name. `augment_class`'s `MethodDecl` arm was
  left unmigrated: `augment class` has no declaration plan at all yet (`Stmt::AugmentClass` still
  indexes `stmt_pool` via the legacy `AugmentClass(u32)` opcode, outside the ADR-0019 plan system
  entirely), so giving it a chunk means building that machinery from scratch — separate, larger
  scope than this slice. `news/2026-08/d3-1-method-name-chunks.md`.
  D3-2/D3-3/D3-4 (one per walker, per the ADR's own expectation) should then unify onto a shared
  `CompiledMethodDecl::from_stmt` and fix the drift found above (the `augment_class` `is_lexical_only`
  gating gap and privacy-aware duplicate detection in particular) as part of the unification, the
  way D2b's `CompiledAttrDecl::from_stmt` fixed its four independently-drifted callers by
  construction.
  **D3-2 landed 2026-08-07** (class walker only): `CompiledMethodDecl` now exists (`src/opcode.rs`)
  as a typed mirror of `Stmt::MethodDecl`'s 19 fields (mirroring `CompiledAttrDecl`'s own shape and
  doc comment for D2b), built once by `CompiledMethodDecl::from_stmt`. `class_body_method_decl`
  builds one `decl` at its top and reads every field off it instead of the original 19-binding
  `let Stmt::MethodDecl { .. } = stmt` destructure — a pure mechanical conversion, no behavior
  change (confirmed via the full `t/` suite plus every whitelisted `S12-methods`/`S14-roles`/
  `S12-attributes`/`S12-class`/`S12-construction` roast file, all green). `params: Vec<String>` is
  deliberately dropped from the struct: all three walkers already ignore it (`params: _` at the
  class/role sites, uncaptured by augment's `..`), so mirroring it would carry a field with no
  reader. `role_body_method_decl` and `augment_class`'s `MethodDecl` arm are NOT yet migrated
  (D3-3/D3-4) — this slice does not yet fix the still-open drift documented above (the
  `augment_class` `is_lexical_only` gap and privacy-aware duplicate detection in particular); that
  requires the other two walkers to also build from `CompiledMethodDecl::from_stmt` so the drift
  becomes visible and fixable at one shared construction site, matching D2b's own precedent.
  **D3-3 landed 2026-08-07** (role walker): `role_body_method_decl` now also builds one `decl =
  CompiledMethodDecl::from_stmt(stmt)` and reads every field off it, the same conversion D3-2 did
  for the class walker. Same verification (`t/` suite, same 90-file roast set). This walk never read
  `is_our`/`our_variable_form`/`custom_traits`/`is_export`/`export_tags` before (a role method is
  never `our`-registered as a package sub, and custom traits/exports on a role method go unhandled
  here) — that omission is unchanged, now expressed as unread `CompiledMethodDecl` fields rather
  than `_`-ignored destructure bindings. `augment_class`'s `MethodDecl` arm (D3-4) is the last of the
  three; only once it also builds from `CompiledMethodDecl::from_stmt` does the drift between all
  three become fixable at one shared site.
  **D3-4 landed 2026-08-08** (augment walker, the last of the three): `augment_class`'s `MethodDecl`
  arm also builds a `decl` and reads every field off it. `name_expr` is still evaluated from the raw
  AST here (`self.eval_block_value(&[Stmt::Expr(expr.clone())])`), unlike the class/role walkers'
  D3-1 chunk-cursor lookup — `augment class` has no compiled declaration plan at all
  (`Stmt::AugmentClass` still indexes `stmt_pool`), so there is no `method_name_chunks` to read from.
  This slice preserves, rather than fixes, every drift point the D3 scoping pass found:
  `MethodDef.is_my` is still set from the raw `is_my` flag here (the class/role walkers use
  `is_submethod`), duplicate-method detection is still not privacy-aware
  (`all_from_role` only, no `is_private` comparison), and `is_lexical_only`/`is_our_only` gating,
  `handles` forwarders, custom-trait/`is_export` handling, and BUILD/TWEAK `:$!attr` validation
  remain absent from this walker — now visible as unused `CompiledMethodDecl` fields at this call
  site rather than as absent destructure bindings, which is what makes the drift fixable at all: with
  all three walkers sharing one typed constructor, D3-5 can compare and reconcile the still-open
  fields directly instead of re-deriving each walker's field set from its own AST match arm the way
  the original 2026-08-07 scoping pass had to.
  **D3-5 landed 2026-08-08**: fixed the two `augment_class` drift points confirmed as real,
  user-visible gaps against `raku` (`t/augment-method-lexical-scoping.t`). `MethodDef.is_my` now
  stores `decl.is_submethod` like the class/role walkers, and `is_lexical_only`/`is_our_only`
  gating excludes `my method`/`our method` from the method table — before this fix,
  `augment class Foo { my method secret {...} }` put `secret` in `Foo`'s method table
  (`Foo.can('secret')` wrongly returned it), and `our method` was both directly callable as a
  method and absent as a package sub, the reverse of `raku`'s behavior. Fixing the gating exposed
  that `augment_class` never registered the `our`/`my` function forms the class walker registers
  (`Package::name(invocant)` / lexical `name(invocant)`), so this slice ports that registration
  too — confirmed against `raku` that both forms are expected to resolve. Duplicate-method
  detection is now privacy-aware (`is_private` compared, matching the class/role walkers): a
  public `method foo` and a private `method !foo` of the same name coexist instead of
  `augment_class` wrongly rejecting the second declaration as already-declared. Still open:
  `handles` forwarder synthesis, custom-trait/`is native`-style trait dispatch, `is
  export`/`export_tags`, and BUILD/TWEAK `:$!attr` validation remain absent from `augment_class`
  (present at the class walker, `handles` also at the role walker) — each is a separate,
  independently-scoped gap, not reconciled by this slice.
  **`handles` forwarder synthesis landed as a same-day follow-up (2026-08-08)**: `augment_class`'s
  `MethodDecl` arm now synthesizes `Name`/`Rename` forwarder methods the same way the class/role
  walkers do (`augment class Foo { method inner() handles 'uc' {...} }` previously left
  `Foo.new.uc` dispatching to the built-in `Cool` coercion instead of forwarding to `inner`).
  `Wildcard`/`Regex` specs are wired through the same `class_def.wildcard_handles` list the other
  two walkers populate, but a wildcard handle losing to a same-named built-in `Cool`/`Any` method
  turned out to be a pre-existing bug shared by *all three* walkers (reproduces with a plain
  `class`-declared `handles *`, not just `augment`), so it is out of D3's walker-drift scope and is
  tracked separately as `todo/tickets/wildcard-handles-loses-to-builtin-cool-methods.md`. Custom
  traits, `is export`, and BUILD/TWEAK validation remain open `augment_class` gaps.
  **D3-6 landed 2026-08-08**, closing the last three drift points by porting the class walker's
  logic verbatim: BUILD/TWEAK's `:$!attr` parameters are now validated against a pre-scanned set of
  the class's own attribute names (existing plus any this same augmentation declares), rejecting an
  undeclared one with `X::Attribute::Undeclared` — confirmed against `raku`, which rejects this at
  compile time. `is export` and custom-trait (`trait_mod:<is>`, including `is native`) handling are
  now wired identically to the class walker. Measuring against `raku` while building this slice
  surfaced two further bugs that turned out to be **pre-existing in the class walker itself**, not
  augment-specific drift, so they are out of D3's scope and tracked separately: `is export` on a
  plain (non-operator) method name does nothing on any walker (only the operator-categorical sub-form
  path — `method infix:<as> is export` and friends — actually works;
  `todo/tickets/method-is-export-non-operator-name-does-nothing.md`), and a user `trait_mod:<is>`
  multi typed `(Method $m, ...)` never dispatches because the code-object value built for the
  about-to-be-installed method is a plain `Sub`, not a `Method`, so the type-checked parameter never
  matches (`todo/tickets/method-typed-trait-mod-is-dispatch-never-matches.md`). D3-6 makes
  `augment_class` reach exact parity with the class walker, including that walker's own
  pre-existing limits — it does not fix either underlying bug.
  **D3-7 scoping pass (2026-08-08):** with drift closed, the next natural step is the D2b-style
  precompute — moving `CompiledMethodDecl::from_stmt`'s 19-field destructure from "once per
  registration" (called at runtime by `class_body_method_decl`/`role_body_method_decl`, which
  re-runs for a class/role declared inside a loop or a repeatedly-called sub) to "once, at compile
  time", the way `own_attribute_names`/`is_default_chunks`/`method_name_chunks` already do for
  their own fields. Unlike D2b's own attribute case, this has **no position-correlation blocker**:
  D3-1 already solved exactly that problem for method statements (the `method_name_chunks` cursor,
  built by flattening `SyntheticBlock` the same way the registration walk does), so the identical
  cursor mechanism carries a full `CompiledMethodDecl` instead of just a name chunk.
  A wider scoping investigation (an `Explore` agent survey of `compile_class_methods`/
  `compile_role_methods`, `accessors_resolve.rs`) found a **separate, larger** gap behind the D3
  box's literal text ("install ... compiled candidates ... without walking `Stmt::MethodDecl`"):
  method *bodies* are not compiled by the single main-pass `Compiler` at all (unlike `SubDecl`,
  which gets a pool-keyed `CompiledFunction` via `compiled_routine_keys` — ADR-0019 C1/C3). Instead
  a method body is compiled by a throwaway `Compiler::new()` spun up by
  `compile_method_def_in_place_with_dist` (`accessors_resolve.rs`), triggered once per class/role
  from at least 9 distinct sites (`RegisterClass`/`RegisterRole` VM ops, role mixin composition,
  three `augment class` sites, the method-dispatch-cache miss path, `nextsame` dispatch, BUILD/TWEAK
  constructor-phase planning, `class_dispatch.rs`) and memoized via `compiled_code.is_some()`.
  `Stmt::ClassDecl`/`RoleDecl` bodies are never walked by `compile_stmt` (unlike `module`/`package`,
  which recurse and set `current_package`) — the only main-pass compile that already touches every
  method body is `record_type_body_captures`'s escape analysis, whose result is discarded and whose
  package context is the *enclosing* package, not the class's own name. Migrating this fully would
  mirror C1-C4 for methods and needs its own multi-slice plan: `effective_param_defs`'s
  `::?CLASS`-substitution and auto-`@_`-detection currently run at registration time reading the
  real class name (not always known at the main-pass point a class body is reached — a computed
  class name `class ::($name) {...}` is D3-1's own reason `method_name_chunks` exists); multi-method
  candidates have no signature-keyed pool slot the way multi subs do (`class_body_method_decl`
  pushes a plain `Vec<MethodDef>`); and role method bodies may need per-composition
  re-instantiation depending on how a parametric role's type captures reach compiled bytecode. This
  is left as a scoped-but-unstarted future slice (tentatively D3-8) rather than attempted in one PR.
  **D3-7 landed 2026-08-08**: `CompiledClassDeclPlan`/`CompiledRoleDeclPlan` gained
  `method_decls: Vec<CompiledMethodDecl>`, built once by a new `compile_method_decls` free function
  (mirroring `compile_method_name_chunks`'s exact flatten-and-filter walk, needing no compiler state
  since `CompiledMethodDecl::from_stmt` is pure AST-to-struct). `class_body_method_decl`/
  `role_body_method_decl` now read a clone by position (reusing the existing
  `method_name_chunk_idx` cursor for both vecs) instead of calling `CompiledMethodDecl::from_stmt`
  on the raw statement at every registration; both functions no longer take a `stmt` parameter at
  all, since nothing in the walk reads it anymore. `augment_class` (no compiled plan) and the
  role-pun/mixin synthesis paths keep passing an empty `method_decls` slice, matching
  `method_name_chunks`'s existing D3-1 precedent.
  **D3-8 design pass done 2026-08-08 (no code landed):** the full design is
  `todo/deep/adr0019-d3-8-method-body-main-pass-compilation.md`. Key findings that shaped it: the
  method-body compile is composition- and class-name-independent (`T` is an env variable injected
  at method entry, `::?CLASS`/`::?ROLE` bind dynamically, the `::?CLASS` param substitution only
  rewrites bind-time `param_defs` strings), so one main-pass compile per declaration is sound; the
  scoping pass's "multi methods have no signature-keyed pool slot" complication dissolves by
  carrying a per-declaration `compiled_routine_key: Option<Symbol>` on `CompiledMethodDecl`
  (positionally delivered by the existing D3-1/D3-7 cursor) instead of a keyed pool; role methods
  are today recompiled once per composing class only because the `role_candidates` snapshot is
  cloned before `compile_role_methods` runs, and installing at `role_body_method_decl` time
  collapses that to one compile with no composition changes; and the parity-first rule (replicate
  the throwaway `Compiler::new()` seeding exactly, no lexical-scope inheritance yet) keeps the
  cutover byte-verifiable. Slices: D3-8a (additive compiler side + `MUTSU_VM_STATS`
  `method_body_runtime_compiles` counter), D3-8b/c (class/role walker cutover with a
  params-equality guard that degrades to the runtime fallback), D3-8d (instrument-and-sweep
  proving the fallback only fires for `augment`/`.^add_method`/computed-name shapes).
  `compile_method_def_in_place_with_dist` remains as the narrowed fallback, mirroring
  `otf_compile_function_def`'s role for subs.
  **D3-8a landed 2026-08-08**: the additive compiler-side half — nothing reads its output yet,
  matching how D4-1/D4-2 stayed inert before D4-3's cutover. `Compiler::compile_method_body`
  (new `compiler/helpers_method_body.rs`) replicates `compile_method_def_in_place_with_dist`'s
  bare-parity compile (a fresh `Compiler::new()`, the declaring package, the enclosing
  distribution, `lexically_in_method`) at main-pass time, called from
  `Compiler::add_class_decl_plan`/`add_role_decl_plan` for every statically-named method/submethod
  and stashed on the new `CompiledMethodDecl::compiled_routine_key: Option<Symbol>` (`None` for a
  computed method or class/role name, or for the `__hoisted` forward-reference shell — only the
  source-order declaration plan compiles, mirroring the sub side's hoist/source-order split and
  avoiding a redundant second compile of every class/role method body). `effective_method_param_defs`/
  `auto_signature_uses`/the implicit-slurpy `ParamDef` builders moved out of
  `runtime::registration`/`runtime::methods_signature` into a new shared `method_signature_shared`
  module so the compiler and the three registration call sites (`class_body_method_decl`,
  `role_body_method_decl`, `augment class`) call one implementation instead of drifting copies —
  the same pattern D2b established for `CompiledAttrDecl`. Key shape follows C2:
  `"{package}::{name}!m/{arity}#{fingerprint:x}"`, fingerprinted over the effective
  params/param_defs/body (which also disambiguates same-named multi candidates, so — unlike the
  sub side — no separate signature-keyed lookup is needed). `remap_sub_decl_compiled_routine_keys`
  now also rewrites `class_decl_plans[*].method_decls[*].compiled_routine_key` and the role-plan
  equivalent, so nested-compunit import keeps key identity. `MUTSU_VM_STATS` gained
  `method_body_runtime_compiles`, incremented in `compile_method_def_in_place_with_dist` — the
  D3-8b/c/d exit-criterion baseline. Verification: **V1** (a `param_defs` type-constraint string,
  including the registration-only `::?CLASS` substitution, does not affect emitted bytecode) and
  **V2** (`is_hidden` is a literal `is hidden` class trait, no computed path) confirmed by reading
  the parser plus a targeted byte-parity test; **V3** (`resolve_package_distribution` vs. the
  compiler's `current_distribution`) reasoned sound for the D3-8a-scoped case (a class/role
  declared in its own compilation unit, the only case this box keys) — both derivations trace back
  to the same per-compunit distribution value, `$?DISTRIBUTION`'s `LoadConst` bake-in gives a
  direct byte-level probe, and no divergence was found; a cross-module scenario was not
  filesystem-tested and is noted as residual risk for D3-8b/c to re-check. **V4** (byte-parity): a
  `#[cfg(test)]` suite in `compiler/helpers_method_body.rs`
  (`d3_8a_byte_parity_tests`) compiles a corpus of method shapes (plain method, submethod with
  attribute binds, typed param, multi method, `is hidden` class, auto-`@_`-detected signature-less
  method, method with a nested `sub`, role method, role method's auto-`@_` NON-insertion, the
  `::?CLASS` substitution case, `$?DISTRIBUTION`) both ways — main-pass `compile_method_body` and
  an actual `Interpreter::run` — and asserts the two `CompiledCode`s are `Debug`-identical (after
  normalizing the process-global closure-ordinal/Symbol-intern-id noise both compiles pick up from
  unrelated background compilation). All pass.
  **D3-8b landed 2026-08-09**: the class-walker install-by-key cutover (design decision 4).
  `class_body_method_decl` (`runtime/registration_class_body_method.rs`) now looks up
  `decl.compiled_routine_key` in the ambient `CompiledFns` pool and, when it resolves AND the
  resolved `CompiledFunction`'s `params`/`param_defs` match what this registration walk just
  computed for the same declaration, installs `MethodDef::compiled_code`/`compiled_fns` directly
  from it instead of leaving them `None` for the bulk `compile_class_methods` pass to fill in later
  via the registration-time throwaway compile. The comparison snapshots `effective_param_defs`
  *before* the `::?CLASS` substitution (which `compile_method_body` never performs — design
  decision 3) so the two sides line up; `ParamDef` has no `PartialEq` (it embeds `Expr`, which
  doesn't either, and deriving it project-wide is a separate, much larger change), so the guard
  compares `Debug`-formatted strings instead — exact, not a heuristic, since both sides are derived
  from the same cloned `decl.param_defs`/`decl.body` within one process, unlike the D3-8a test's
  two-separate-compiles comparison which has to normalize cross-run Symbol/closure-ordinal drift.
  The ambient `CompiledFns` table reaches `class_body_method_decl` through a new plumbing path:
  `exec_register_class_op` gained a `compiled_fns: &CompiledFns` parameter (mirroring
  `exec_register_sub_op`, threaded from `exec_register_decl_op`, which already had it),
  `ClassDeclModifiers`/`ClassBodyCx` gained a `compiled_fns` field, and the two non-VM-op call
  sites of `register_class_decl` (role-pun synthesis in `registration_class_augment.rs`, mixin-type
  synthesis in `types/role_mixin_class.rs`) pass `&CompiledFns::default()` — harmless, since both
  call with an empty body, so `method_decls` is empty and the lookup is never reached. Verified
  with a `MUTSU_VM_STATS=1` stress repro (`class C { method m($x) { $x + 1 } }` redeclared and
  instantiated 50 times in a loop): `method_body_runtime_compiles` dropped from 50 (baseline, one
  throwaway compile per loop iteration) to 0. `make test` and a full `make roast` both green, no
  regressions. **One real regression was caught by the full `make roast` run and fixed in the same
  slice**: `roast/S12-introspection/walk.t`'s `$?PACKAGE.^name` returned a mangled name (e.g.
  `"GLOBAL::&::C2"` instead of `"C2"`) for a class declared inside a closure body (`subtest "..."
  => { my class C2 { ... } }`). Root cause: D3-8a's `qualified_class_decl_name` (the compile-time
  predictor of a class's registration-time qualified name, used both to key the compiled body and
  to seed `$?PACKAGE`'s baked-in `LoadConst`) did not account for the compiler's synthetic
  STATE-SCOPE pseudo-package (`current_package` containing `"::&"`, used purely for `state`
  variable key uniqueness inside a sub/closure body) — a case `qualify_package_name`/
  `qualify_variable_name` already special-cased elsewhere in the same file. Inside a state scope,
  `current_package` does NOT track the runtime's real `current_package()` the way ordinary
  package-scope bracketing does, so the "compile-time mirrors registration-time" assumption breaks
  silently — undetectable by the params-equality guard, since a wrong package name is baked
  directly into the body's bytecode, not carried as a parameter. The params-equality guard alone
  cannot catch a wrong *package* prediction; fixed by extending the bail-out itself: both
  `add_class_decl_plan` and `add_role_decl_plan` (`compiler/decl_plan.rs`) now skip main-pass
  method-body compilation entirely (`compiled_routine_key` stays `None`, same as the
  computed-name/hoisted-shell cases) whenever the declaration is nested inside such a state scope,
  falling back to the unaffected registration-time throwaway compile. Fixed on the role side too
  (`qualified_role_decl_name`) even though not yet observable (D3-8c doesn't install from it yet),
  to avoid reintroducing the identical bug there.
  **D3-8c landed 2026-08-09**: the role-walker install-by-key cutover, the same design-decision-4
  guard as D3-8b applied to `role_body_method_decl`
  (`runtime/registration_role_method.rs`). Simpler than the class side: `role_body_method_decl`
  never performs a `::?CLASS`-style param-type substitution, so `effective_param_defs` computed
  here IS exactly what `compile_method_body` computed at plan-lowering time (`is_hidden: false`,
  no auto-`@_` detection, per `add_role_decl_plan`'s existing comment) — no separate
  pre-substitution snapshot is needed, unlike D3-8b. The ambient `CompiledFns` pool reaches
  `role_body_method_decl` the same way: `exec_register_role_op` gained a
  `compiled_fns: &CompiledFns` parameter (threaded from `exec_register_decl_op`, which already had
  it), `register_role_decl`/`RoleDeclCx` gained a `compiled_fns` field. `register_role_decl` has
  only the one call site (the VM op), so no `CompiledFns::default()` plumbing was needed anywhere
  else. Because the install happens inside `register_role_decl` itself — before the
  `role_candidates` snapshot used by composition is cloned — the per-composing-class recompile
  disappears for free (design decision 6), verified with a `MUTSU_VM_STATS=1` repro (a role with
  two methods composed into 3 classes directly plus 5 more inside a loop):
  `method_body_runtime_compiles` dropped from 18 (baseline) to 0. The D3-8a byte-parity unit tests
  (including the two role-specific fixtures) and the full `t/` suite (2974 files, 28019 tests) both
  stayed green, and all 121 whitelisted `roast/S12-*`/`S14-*` files passed on a release build.
  **D3-8d landed 2026-08-09**: the fallback-narrowing survey — but instead of finding only the
  enumerated dynamic shapes, a `MUTSU_VM_STATS=1` sweep over all of `t/` and the whitelisted
  `S12`/`S14` roast files found a real, general gap: `qualified_class_decl_name`/
  `qualified_role_decl_name` predicted the wrong base package for **any** class/role declared
  inside **any** closure (a `sub`, a bare block, `if`/`for`, a block passed to `subtest`, ...) —
  not just the narrow "declared inside a `subtest` block" case D3-8b's regression fix addressed —
  because D3-8b's fix bailed out of main-pass compilation entirely whenever `current_package`
  carried the synthetic STATE-SCOPE pseudo-package marker (`::&`, assigned unconditionally to
  every closure/sub body for `state`-variable key uniqueness), rather than resolving the real
  package. Fixed by using `self.enclosing_package` (already captured and correctly propagated
  through nested closures for `$?PACKAGE`) as the base package whenever `current_package` is
  state-scope-mangled, and dropping the now-unnecessary `in_state_scope` bail-out from
  `add_class_decl_plan`/`add_role_decl_plan`. Verified with the D3-8a byte-parity tests, the full
  `t/` suite, all 121 whitelisted roast files, and the `walk.t` regression pin (all green); a
  before/after `MUTSU_VM_STATS=1` sweep over the 121 whitelisted files measured the effect
  directly: summed `method_body_runtime_compiles` dropped from 494 to 330 (33%), with 6 files
  (`walk.t` among them, 29 → 0) reaching zero entirely. The remaining hits are a second, distinct,
  already-documented cost — `subtest NAME => { ... }`, called as an ordinary function (not the
  dedicated `Stmt::Subtest` statement form), recompiles its block from AST on every call
  (`eval_block_value`, EVAL-like), re-triggering the hoisted-shell's by-design
  always-runtime-fallback on every invocation of the common `plan N; class C {...}` idiom — real,
  but out of D3-8's scope; recorded as `todo/tickets/subtest-recompiles-block-from-ast-every-call.md`
  rather than re-opened as a D3-8 straggler.
- [ ] **D4 — Compile class declaration-time expressions.** Cover computed names, traits, parent
  expressions, aliases, and deferred class bodies through re-entrant bytecode chunks. (Computed
  names and custom-trait arguments already landed with C5; parents, aliases, and deferred bodies
  remain.)
  **Scoping pass done 2026-08-08 (no code landed).** The box's four remaining named pieces turn
  out to be three very different problems, none shaped like a D3-1-style cheap first slice:
  - **Aliases (export/version/auth/api adverbs) are already effectively bytecode-native and need
    no plan-field migration.** `:ver<1.0>`/`:auth(...)`/`:api(...)` parse into a real `Expr`
    (`parse_declarator_traits`, `class_decl.rs:23-58`) but never enter
    `CompiledClassDeclPlan` — the parser instead synthesizes a sibling
    `Stmt::Expr(Expr::Call{name: "__MUTSU_SET_META__", ...})` statement
    (`meta_setter_stmt`, `class_decl.rs:113-122`) wrapped with the `ClassDecl` in a `Stmt::Block`.
    That is a plain call the main-pass compiler already compiles like any other statement — no
    re-parse, no `legacy_body` walk, no runtime AST tree-walk. Folding it into a formal plan field
    would only change representation, not behavior or performance: per the D2d precedent ("a
    lateral move... does not meet the ADR's own gain bar"), this does not clear the bar for its
    own slice and is closed as-is. (Lexical/package aliases were already noted done under D1.)
  - **Parent expressions are a real gap, but not the one the ADR text implies, and the fix is
    constrained by a shared runtime API.** `is Parent[Args]`/`does Role[Args]`/`hides
    Parent[Args]` bracket content is captured as *raw balanced-bracket source text* and
    concatenated onto the parent name string (`parse_optional_bracket_suffix`, `class_decl.rs:
    60-79`, call sites at 416/459/474/487) — there is no `Expr` upstream for `compile_decl_expr`
    to compile at all, unlike every other C5/D3-1 case. At registration,
    `Interpreter::resolve_role_candidate` (`registration_role.rs:134`) splits the bracket text back
    out and `eval_role_arg_values` (`registration_role.rs:18-74`) literally
    `parse_dispatch::parse_source(substring)` + `eval_block_value` — a full lexer/parser
    invocation *per argument, per registration* (every loop iteration, every re-run of a
    `for`/`while`-declared class, every EVAL). However, `resolve_role_candidate` is a `&str`-keyed
    API also used for genuinely dynamic type-name concretization with no source `Expr` at all
    (`methods_qualified.rs:291`, building `Foo[Int]`-shaped names at runtime from pieces) — that
    call site cannot be migrated away from string re-parsing regardless, so the string-based path
    must stay as a general mechanism. Only the 3 call sites that originate from an actual parsed
    declaration (class header `registration_class_compose.rs:88`, role-body `does`
    `registration_role_body.rs:212,268`, `augment class` role puns
    `registration_class_augment.rs:985`) could skip the round-trip by threading a precomputed
    `Vec<Expr>` alongside the existing string, and `eval_role_arg_values`'s several text-based
    heuristics (`should_treat_role_arg_as_type_expr`, the `::T`-prefix rejection, the bare
    block-literal paren-wrap) would need re-deriving from `Expr` shape instead of trimmed text —
    real behavioral-parity risk across a heavily-exercised area (every parametric role
    instantiation in `t/`, roast `S14-roles`, and the bundled-battery suite). Scope as its own
    D4-1 (parser: capture bracket args as `Vec<Expr>` alongside the existing string, additive, no
    behavior change) / D4-2 (compiler: `CompiledDeclExpr` chunks per argument) / D4-3 (registration
    cutover for the 3 eligible call sites) sub-boxes for a future session; not started.
  - **Deferred class bodies is not class-specific at all — it's `RoleDef::deferred_body_stmts`
    (`decl_types.rs:64-67`), produced during role registration
    (`walk_role_body`/`registration_role_decl.rs:240-251`) and consumed once per composition via
    interpreter tree-walk (`run_composed_role_deferred_body`,
    `registration_class_compose_body.rs:64-277`, called during *class* registration —
    `registration_class_compose.rs:328` — which is presumably why the checklist text files it
    under D4 rather than D8). This is the same data and the same execution entry points D8
    ("Compile role declaration-time bodies and traits... run parameterized-role and composed
    ancestor bodies as bytecode child chunks with correct once-per-composition behavior") already
    names. D4's "deferred class bodies" and D8 are the same piece of work described from two
    angles (D4 = consumption site during class composition, D8 = production site during role
    declaration) and should not be separately planned — D4 needs no distinct deferred-body slice;
    treat D8 as the box that closes this piece for both.
  A same-day, unrelated finding surfaced while reading the parent-expression call sites: `also
  does Role[Args];` *inside a class body* silently drops the bracket arguments entirely (the
  parser's `also_trait_stmt` `does` arm, `class_decl.rs:598-608`, never calls
  `parse_optional_bracket_suffix` the way every sibling `is`/`does`/`hides` arm does), and even if
  it captured them, `class_body_does_decl` (`registration_class_body_does.rs`) looks the role up by
  bare name in `registry().roles` directly rather than through `resolve_role_candidate`, and is
  missing the entire role-attribute-carryover machinery (`role_class_level_attrs`,
  `role_attribute_default_exprs`, `role_attribute_is_types`, `role_attribute_types`,
  `role_attribute_smileys`, type-parameter substitution into methods) that
  `compose_role_into_class` performs for the header form — confirmed against `raku`
  (`role R[::T]{...}; class Foo { also does R[Int]; }` returns the wrong type). This is a plain
  correctness bug, independent of ADR-0019's declaration-plan migration either way; filed as
  `todo/tickets/also-does-role-bracket-args-dropped-in-class-body.md` rather than fixed here, since
  a correct fix means porting a ~200-line carryover block, not a one-line parser change.
  **D4 design pass done 2026-08-08 (no code landed):**
  `todo/deep/adr0019-d4-parent-expr-chunks.md` details the D4-1/2/3 sub-boxes: D4-1 (parser
  captures bracket args as parsed `Vec<Expr>` alongside the unchanged concatenated string —
  parse failure keeps the string path, so nothing currently accepted is rejected; also found
  three concat sites beyond the original four, including the role-side synthetic `DoesDecl`),
  D4-2 (class-plan `parent_arg_chunks` via `compile_decl_trait_arg`; the role-body site's
  carriage deliberately joins D7's `parent_ops` instead of a throwaway field), D4-3
  (`resolve_role_candidate` gains `pre_args: Option<&[Value]>` — only the
  `eval_role_arg_values` call swaps, everything downstream already operates on values; the
  cutover is gated on a raku case table because the string path's heuristics sometimes produce
  type objects where naive evaluation would not). The `Expr` path also fixes
  `split_balanced_comma_list`'s quote-blindness (`R["a,b"]` mis-splits today) for free.
  **D4-1 landed 2026-08-08**: `Stmt::ClassDecl` gained `parent_args: Vec<(String, Vec<Expr>)>`
  and `Stmt::DoesDecl` gained `args: Option<Vec<Expr>>`, populated by a new
  `parse_bracket_arg_exprs` helper at every bracket-suffix call site that builds a genuine
  `is`/`does`/`hides` parent string (class body, `unit class`, `unit role`, grammar `is`/`does`)
  — nine sites in total once `grammar_module.rs`'s two and `package_decl.rs`'s unit-role `does`
  were included alongside the seven the design doc enumerated. `augment`'s bracket sites and
  `also does`/body-level `does R;` stay string-only (no bracket parsing happens there today).
  `compiler/stmt.rs`'s `qualify_decl_name` (the `unit class`/`unit module` package-qualification
  pass) re-keys `parent_args` through the same `qualify_parent` closure it already applies to
  `parents`/`does_parents`/`hidden_parents`, so a lookup by the (now qualified) parent string
  still hits. No consumer reads either field yet (D4-2/D4-3/D7-3).
  **D4-2 landed 2026-08-08**: `CompiledClassDeclPlan` gained `parent_arg_chunks: Vec<(String,
  Vec<DeclTraitArg>)>`, lowered from `parent_args` with the existing `compile_decl_trait_arg`
  helper (literal short-circuit + `Compiled` chunks, the C5 mechanism) and keyed the same way
  after `qualify_decl_name` re-keys `parent_args` first. Covers only the class-header site per
  the design doc; the role-body `DoesDecl` site's carriage still joins D7. No consumer reads it
  outside a new compiler unit test yet (D4-3).
  **D4-3 landed 2026-08-08**: `resolve_role_candidate` gained a `resolve_role_candidate_with_args`
  sibling taking `pre_args: Option<&[Value]>`; when set, it replaces the `eval_role_arg_values`
  re-parse with the already-evaluated values, everything downstream (arity filter, trial bind,
  specificity sort) unchanged. `compose_class_parent_roles` evaluates each parent's
  `parent_arg_chunks` (looked up by the plan's *original*, pre-remap parent string — position-
  aligned with `parents` through the `lexical_env_remap_name`/`qualify_sibling_parent_name`/
  Grammar-self-parent-drop chain via a zipped `Vec<Option<&[DeclTraitArg]>>`, since none of those
  remaps filter except the Grammar case, which the zip drops in lockstep) and passes the result;
  the four string-only callers (`registration_role_body.rs` ×2, `registration_class_augment.rs`,
  `methods_qualified.rs`) are unaffected. Verified against an 8-case `raku` table (literal, type
  name, nested parameterization, enum value, comma-containing string, block literal) — all match
  byte-for-byte, and the `Expr` path incidentally fixes a real `R["a,b"]`-comma-in-string parse
  failure the old string path had (per the D4 design doc's prediction). A `make roast`
  regression surfaced during verification (`S14-roles/parameterized-type.t`) traced to an
  unrelated, real parser bug the D4-3 cutover exposed rather than caused: `parse_optional_bracket_suffix`
  returned an owned `String` copy of the bracket content, and two sibling `does R[X] does R[Y]`
  clauses on one class header each allocate their own short-lived copy — when the first is freed
  at loop-iteration end, the second can land at the same heap address, aliasing the pointer-keyed
  expression parse memo (`(ptr, len)`, no content check) and returning the first clause's cached
  `Expr` for the second's distinct bracket content. Fixed at the root: `parse_optional_bracket_suffix`
  now returns a slice of the persistent source buffer (never freed mid-parse) instead of an owned
  copy — the memo can no longer alias it, by construction. Pinned by a Rust unit test
  (`parse_class_decl_two_does_clauses_capture_distinct_bracket_exprs`) and a `t/` integration test
  (`role-double-parametric-args-distinct.t`). A second, independent, pre-existing bug surfaced
  during root-causing (present on `main` before D4-3 too: composing the same parametric role
  twice, multi dispatch always picks one candidate regardless of the call's argument type) is
  out of scope and filed as `todo/tickets/same-role-composed-twice-multi-dispatch-picks-one-candidate.md`.
- [ ] **D5 — Drive user HOW operations from plan ops.** Execute `new_type`, `add_method`, trait
  interception, and `compose` without entering `register_class_decl`'s AST walker.
  **Design pass done 2026-08-08 (no code landed) — the box shrinks:**
  `todo/deep/adr0019-d5-plan-driven-how-ops.md`. The survey found the user-HOW protocol
  (`new_type`/`add_method`/`compose`) runs entirely *after* native registration and reads the
  finished registry, never a raw `Stmt` — `add_method` re-enumerates registry `MethodDef`s,
  `add_attribute` is never called by mutsu, trait-interception inputs are all plan-resident
  (class traits on the plan, method traits/bodies on `CompiledMethodDecl`, attribute traits
  closed by D2b-2). D5 is therefore not a migration: D5-1 codifies the ordering invariants
  D6's cutover must keep (shell → registry-authoritative interleaved body writes → HOW
  instantiate → new_type → add_method → trait dispatch → compose; HOW installs keyed on the
  resolved storage name), and D5-2 is a verification gate (OO::Monitors battery + metamodel
  roast) riding on D6's slices rather than a separate code PR.
- [ ] **D6 — Remove `CompiledClassDeclPlan::legacy_body`.** Preserve augmentation, rollback,
  redeclaration errors, language revisions, nested types, and EVAL behavior. Excludes the
  token/rule arms (see the phase preamble). Start with the C6d-style instrumentation survey —
  C6's one box became nine PRs, and this field has the same shape.
  **Survey + design pass done 2026-08-08 (no code landed):**
  `todo/deep/adr0019-d6-d9-legacy-body-removal.md` holds the grep-complete reader inventory
  (the only destructuring reads are the two register ops; no reader exists outside
  registration; augment reads `stmt_pool`, unaffected) and the design: a typed ordered
  `body_plan: Vec<ClassBodyOp>` lowered at compile time, whose `Other` arm carries a
  per-statement `CompiledDeclExpr` chunk replacing `class_body_other_stmt`'s
  per-registration `run_block_raw` OTF compile — the driver keeps its exact env-seeding /
  BEGIN-swallowing / writeback / re-publish structure, only the statement source changes.
  The token/rule exclusion is carried as an `Other.raw` rump (the C6 `FunctionDef.body`
  precedent). Notable freebies found: the `TrustsDecl` walk arm is redundant with D1's plan
  field (deletable now), and `persist_class_body_statics`' body re-scan becomes a
  `declared_static_names` plan fact. Slices D6-1 (cheap facts + dead arm), D6-2 (= D2b-2),
  D6-3 (`body_plan`, instrument-gated, expected to subdivide per arm like C6d), D6-4 (field
  drop via the C6e-3c forced-instrument playbook).
  **D6-1 landed 2026-08-08**: `CompiledClassDeclPlan` gained
  `declared_static_names: Vec<Symbol>`, computed at plan lowering by a new
  `class_declared_static_names` free function mirroring
  `persist_class_body_statics`'s inline `declared_statics` scan (a top-level,
  unflattened `Stmt::VarDecl` that is neither `our` nor `dynamic`).
  `persist_class_body_statics` now takes this precomputed slice instead of
  re-deriving it from the raw body on every registration. The redundant
  `Stmt::TrustsDecl` walk arm in `run_class_body` is deleted — `publish_class_shell`
  already inserts the same `class_trusts` entry from D1's `trusts` plan field
  before the body walk starts, and the compiler already compiles a bare
  `TrustsDecl` statement to a no-op (`compiler/stmt.rs`), so the statement
  now safely falls through to the catch-all `class_body_other_stmt` arm with
  no behavior change. D9-1 (the role-side twin: `is_stub` + our-scope-violation
  plan facts) is not part of this slice.
  **D6-3a landed 2026-08-09**: `CompiledClassDeclPlan` gained
  `body_plan: Vec<ClassBodyOp>`, a new typed enum (`Attr`/`Method`/`Does`/
  `ClassSub`/`CodeAlias`/`ProtoMethod`/`LeavePhaser`/`Other`) computed at plan
  lowering by a new `class_body_plan` free function that mirrors
  `run_class_body`'s own dispatch loop exactly: the same `SyntheticBlock`-
  flattened top level, classified the same way the runtime `match` does, with
  nested-sub `has` declarations appended at the end (same order as
  `own_attribute_names`). The already-typed arms (`Attr`/`Does`/`ClassSub`
  carry a name, `Method` is a bare marker) advance the existing
  `attr_decls`/`method_decls`/`parent_arg_chunks` cursors rather than
  duplicating their payload; `CodeAlias`/`ProtoMethod`/`LeavePhaser`/`Other`
  carry `chunk: None` plus the raw statement, to be precompiled by D6-3b/c.
  Purely additive — no non-test consumer reads the field yet (`#[allow(dead_code)]`
  on the field/enum, following the `current_pos`/`is_rw` precedent, since
  `cargo clippy -- -D warnings` lints the non-test target where a
  `#[cfg(test)]`-only reader does not silence the lint). Pinned by a new
  compiler unit test (`class_declarations_precompute_body_plan`) asserting
  `body_plan.len()` against an independently re-derived flattened-statement
  count and the typed-op sequence for one of each kind. D6-3b (compiling
  `Other` chunks) is next.
  **D6-3b landed 2026-08-09**: every `Other` op's raw statement is now
  compiled into its own standalone `CompiledDeclExpr` chunk, via a
  generalization of `compile_decl_expr_inner`'s child-`Compiler` setup
  (`Compiler::new_decl_chunk_compiler`) into a new
  `Compiler::compile_decl_stmt_chunk(&Stmt)` sibling that compiles a whole
  statement instead of one wrapped `Expr` — the ADR's own suggested
  generalization. `ClassSub` gets a chunk the same way (it runs through the
  identical `class_body_other_stmt` path at registration, per the original
  design sketch's "the SubDecl tail probe fact + Other chunk" comment) via
  a new `Compiler::compile_class_body_plan` pass that fills in both arms'
  `chunk` after `crate::opcode::class_body_plan`'s pure AST classification.
  `token`/`rule` statements are explicitly excluded (checked by a new
  `class_declarations_body_plan_excludes_token_rule_chunks` test) — they
  keep `chunk: None`, per the phase preamble's ADR-0009 carve-out; D6-3e
  will confirm the driver still routes them through `run_block_raw`
  unchanged once D6-3d cuts over. `CodeAlias`/`ProtoMethod`/`LeavePhaser`
  remain `chunk: None` (D6-3c). Still additive — nothing outside the
  compiler unit tests reads a compiled `Other`/`ClassSub` chunk yet.
  Verified via the full `t/` suite (28,019 tests) and the `S12-class`/
  `S12-construction`/`S14-roles` roast files (only the pre-existing,
  non-whitelisted `S12-class/open_closed.t` failure, unrelated). D6-3c
  (compiling the remaining small arms) is next.
  **D6-3c landed 2026-08-09**: `CodeAlias`/`ProtoMethod`/`LeavePhaser` now
  compile their raw statement into a chunk the same way
  (`Compiler::compile_class_body_plan`'s match widened to all five
  raw-statement-carrying arms) — each still executes its raw statement
  wholesale at registration (`class_body_code_alias`'s trailing
  `run_block_raw`, `class_body_proto_method_decl`'s `FunctionDef.body`
  clone, `run_class_body_leave_phasers`'s per-phaser `run_block_raw`), so a
  single-statement chunk mirrors each exactly; no arm needed a richer typed
  payload for this purely-additive slice (the "`ProtoMethod` may reuse
  `CompiledProtoDeclPlan`'s shape" idea floated in the design doc turned
  out unnecessary). `body_plan` is now a complete, compiled mirror of
  `legacy_body` with zero consumers — matching the slice-plan's own
  description of D6-3c's end state. Verified via the full `t/` suite
  (28,023 tests, two new: `t/cro-client-nested-param-shadow.t` and
  `t/react-whenever-broken-promise.t` landed on `main` from unrelated PRs
  in between) and the `S12-class`/`S12-construction`/`S14-roles`/
  `S05-grammar` (proto/protoregex) roast files (only the same pre-existing
  `open_closed.t` failure). D6-3d (driver cutover, instrument-gated) is
  next.
  **D6-3d landed 2026-08-09**: `run_class_body` now consumes `body_plan` in
  parallel with the flattened `legacy_body` statement list (zipped, same
  order/length by construction — `class_body_plan` mirrors the runtime
  walk's own flatten+nested-has-append exactly), and its three small-arm
  helpers (`class_body_other_stmt`/`ClassSub`, `class_body_code_alias`,
  `run_class_body_leave_phasers`) run a statement's precompiled chunk via a
  new `Interpreter::run_compiled_block_raw` (the `run_block_raw` post-compile
  half — `run_nested` plus the `free_var_writes` writeback drain — factored
  out so both the on-the-fly and precompiled paths share it) instead of the
  registration-time `run_block_raw` OTF compile, gated behind
  `MUTSU_DROP_LEGACY_CLASS_BODY=1` (the `MUTSU_DROP_LEGACY_BODY`/C6e-3a
  precedent) — unset by default, so this slice ships with zero behavior
  change; the instrument exists to validate the chunk path before a future
  slice flips the default. `ProtoMethod`'s chunk stays unused:
  `class_body_proto_method_decl` never executed the raw statement (it only
  clones `proto_body`/`param_defs` off the AST into a `FunctionDef`), so
  there is nothing for its chunk to replace yet.

  Wiring the instrument surfaced two real, previously-invisible bugs in the
  "purely additive" D6-3a-c chunks (invisible because nothing consumed them
  until now):
  1. **`LeavePhaser`'s chunk compiled to a silent no-op.** D6-3c compiled
     the chunk from the *wrapping* `Stmt::Phaser{kind: Leave, ..}`
     statement, but `compiler/stmt.rs`'s `Stmt::Phaser { .. } => {}`
     catch-all arm compiles an un-lowered `PhaserKind::Leave` to nothing
     (LEAVE is normally driven by the enclosing `BlockScope` registering a
     callback, not direct statement compilation) — while
     `run_class_body_leave_phasers` actually runs the phaser's *inner*
     `body`. Fixed by generalizing the single-statement chunk compile into
     `Compiler::compile_decl_stmts_chunk_in_package(stmts: &[Stmt], ..)` and
     calling it with the phaser's own inner body for the `LeavePhaser` op.
     Pinned by `class_declarations_leave_phaser_chunk_compiles_inner_body`.
  2. **Every D6-3b/c chunk qualified bare variable/sub names against the
     wrong package.** `Compiler::qualify_variable_name`/`qualify_package_name`
     bake package qualification in at COMPILE time from `self.current_package`
     — but `compile_class_body_plan`'s child compiler inherited the *outer*
     (enclosing) compiler's ambient package, not the class's own name, unlike
     `compile_method_body`'s explicit
     `method_compiler.set_current_package(package_name.to_string())`. A
     top-level `no strict; class Foo { $foo = 42; }` therefore wrote a bare
     unqualified global instead of `Foo::foo` under the forced instrument,
     diverging from `run_block_raw`'s registration-time compile (which
     qualifies against the interpreter's `current_package()`, already `Foo`
     by the time the body walk runs) — caught by the pre-existing
     `t/strict-use-and-eval.t`. Fixed by threading `package_name: Option<&str>`
     (the same value `compile_method_body_keys` already receives — `None` for
     a computed class name/hoisted shell, in which case every op keeps
     `chunk: None` and falls back to `run_block_raw`, mirroring the
     method-body precedent) into `compile_class_body_plan` and having
     `compile_decl_stmts_chunk_in_package` override `current_package` on the
     child compiler. Pinned by
     `class_declarations_other_chunk_qualifies_against_declaring_class`.

  Verified with `MUTSU_DROP_LEGACY_CLASS_BODY=1` forced: the full `t/` suite
  (28,023 tests), the `S12-class`/`S12-construction`/`S14-roles`/`S05-grammar`
  roast files (1,042 tests, same pre-existing `open_closed.t` failure as
  unforced), and `scripts/battery-testsuite.sh` (158/164 files pass, 2
  excluded — byte-identical PASS/FAIL output to the unforced baseline). D6-3e
  (token/rule carve-out check) is expected to fold into a later default-flip
  slice rather than needing its own PR, per the design doc's own note.
- [x] **D7 — Encode role structure and composition.** Put role parameters, attributes, methods,
  parent roles, conflicts, hides, and pun metadata into immutable plan operations.
  **Design pass done 2026-08-08 (no code landed):**
  `todo/deep/adr0019-d7-d8-role-plan-encoding.md`. The role plan gains the class side's
  missing twins (`is_stub`, our-scope violation — the role plan has no D1-style fields at
  all), name-keyed `attr_decls`, typed `parent_ops` (replacing the
  `__mutsu_role_hides__`/`__mutsu_role_hidden__` string-marker encoding and carrying D4's
  arg chunks for the role-body `does` site), and a `body_plan` op walk. Deliberately narrower
  than the box text sounds: candidate selection, trial binding, specificity, conflict and
  required-method detection, and pun materialization read the *registry*, not the AST, and
  stay runtime — the declaration's own structure becomes plan data; the composition algebra
  over it does not move. Slices D7-1..4 in the doc.
  **D7-1 (= D9-1) landed 2026-08-08**: `CompiledRoleDeclPlan` gained `is_stub: bool` and
  `our_scope_violation: Option<&'static str>`, computed at plan lowering by new
  `role_body_is_stub`/`role_body_our_scope_violation` free functions mirroring
  `Interpreter::role_body_is_stub`/`check_role_body_our_scoped_decls`'s scans verbatim
  (including the role side's looser `.any()` stub check, unlike the class side's
  single-statement `is_stub_routine_body` — an existing divergence, not changed here).
  `register_role_decl` now takes both precomputed facts as parameters instead of re-walking
  the raw body on every registration; the two now-dead `Interpreter` methods are deleted (no
  other callers). D7-2 (= D2b-2's role half, name-keyed `attr_decls`) was already delivered by
  D2b-2 landing on both `CompiledClassDeclPlan` and `CompiledRoleDeclPlan` at once.
  **D7-3 landed 2026-08-08**: `CompiledRoleDeclPlan` gained `parent_ops: Vec<RoleParentOp>`
  (`{ name, hides, hidden, args: Option<Vec<DeclTraitArg>> }`), one op per `DoesDecl` statement
  in the (`SyntheticBlock`-flattened) role body, computed by a new
  `Compiler::compile_role_parent_ops` mirroring `compile_role_attr_decls`'s flatten so the two
  sides' traversal order agrees. `role_body_does_decl` now reads its op by position via a
  cursor (the same style D3-1's `method_name_chunk_idx` uses) instead of string-matching the
  `__mutsu_role_hidden__`/`__mutsu_role_hides__<name>` marker names the parser still encodes on
  the raw `Stmt::DoesDecl` — the marker classification moved from the runtime hot path to the
  one-time compiler precompute. The `args` chunk (D4-1's parsed bracket expressions, compiled
  the same way D4-2 compiled the class-header site) also feeds `resolve_role_candidate_with_args`
  for the role-body `does` site's own parametric-candidate resolution (the piece D4-2 deliberately
  left for D7), reusing the exact `should_treat_role_arg_as_type_expr` coercion-type bail-out
  D4-3's own regression fix established. Deliberately left as the old string path: the earlier
  `concretized_parent` lookup in the same function (a second, independent `resolve_role_candidate`
  call already re-evaluating the same bracket text) — wiring `pre_args` there too would collapse
  today's double-evaluation of a side-effecting bracket argument into a single evaluation, a
  real behavior change out of this slice's scope. Verified via the full `t/` suite (27,992
  tests) and every whitelisted `S14-roles`/`S12-coercion` roast file, plus `t/mro-role-hides.t`
  (hides/hidden-specific coverage), all green.
  **D7-4 landed 2026-08-09**: `CompiledRoleDeclPlan` gained `body_plan: Vec<RoleBodyOp>`, a
  new typed enum (`Attr`/`Method`/`Parent`/`Deferred`) computed at plan lowering by a new
  `role_body_plan` free function mirroring `walk_role_body`'s own dispatch loop: a single-level
  `SyntheticBlock` flatten (unlike the class side, a role body has no nested-sub `has`
  collection — `walk_role_body`'s own comment confirms roles have none), classified the same way
  the runtime match does. Deliberately narrower than `ClassBodyOp`: a role body has no
  `ClassSub`/`CodeAlias`/`ProtoMethod`/`LeavePhaser` arms (those class-only statement kinds, plus
  the `__mutsu_stub_die`/`__mutsu_stub_warn` stub marker and `SetLine` markers, all fall through
  to `Deferred`), and `Deferred` carries no compiled chunk — deferred-statement chunk compilation
  is D8's job (`RoleDef::deferred_body`'s own `DeferredBodyOp`, a separate type per the D7/D8
  design doc). `Deferred`'s `raw: Stmt` field is boxed (`Box<Stmt>`): unlike `ClassBodyOp`, whose
  every non-tiny variant also carries a same-size `Stmt` (keeping the largest/second-largest size
  gap small), `RoleBodyOp`'s `Attr`/`Method`/`Parent` are all marker-sized, so an unboxed `Stmt`
  tripped `clippy::large_enum_variant`. Purely additive — no non-test consumer reads the field yet
  (`#[allow(dead_code)]` on the field/enum, the D6-3a precedent), pinned by a new compiler unit
  test (`role_declarations_precompute_body_plan`) asserting `body_plan.len()` against an
  independently re-derived flattened-statement count and the typed op sequence (including the
  role-header `does` clause's synthetic `DoesDecl`, prepended to the body ahead of a body-level
  `does`, both classifying as `Parent`). Verified via the full `t/` suite (28,037 tests). This
  closes D7 — all of D7-1..4 have now landed.
- [ ] **D8 — Compile role declaration-time bodies and traits.** Run parameterized-role and composed
  ancestor bodies as bytecode child chunks with correct once-per-composition behavior. (Custom-trait
  arguments already landed with C5; the bodies remain.)
  **Design pass done 2026-08-08 (no code landed), same doc as D7.** Unit of compilation is
  **one chunk per deferred statement** (not per body): the five consumer sites' per-statement
  package routing (type decls → role package, token/rule → composing class package), the
  lexical-persistence scan (becomes a precomputed `declared_vars`), and the
  `X::Role::Instantiation` wrapping all operate at statement granularity, so consumers keep
  their exact env dance and swap only `run_block_raw(stmt)` for `run_decl_expr(chunk)`. The
  frozen-plan concern (nested declarations' plans lowered once per role rather than per
  composition) resolves to a verification item, not a blocker — registration is
  per-execution and composition-dependent names resolve through the env; a failing case keeps
  a raw fallback. Once-per-composition semantics are *preserved*, not changed (the current
  role-global `pun:`/`mixin:` memos and the unguarded class path are recorded; any
  raku-conformance divergence gets its own ticket). Includes the `run_role_submethod` rider
  (the C6d-3 leftover goes bytecode after D3-8). Slices D8-1..4 in the doc.
  **D8-1 landed 2026-08-09**: `CompiledRoleDeclPlan` gained `deferred_body_ops:
  Vec<DeferredBodyOp>` (`{ kind: TypeDecl | TokenRule | Plain, chunk: Option<CompiledDeclExpr>,
  declared_vars: Vec<Symbol>, raw: Stmt }`), one op per `RoleBodyOp::Deferred` entry in D7-4's
  `body_plan` — reusing D7-4's already-classified raw statements as input instead of re-deriving
  them from `legacy_body`. `kind` mirrors `run_composed_role_deferred_body`'s own
  `is_type_decl`/`is_regex_decl` classification; `declared_vars` replaces its `VarDecl` re-scan
  (a non-`our`/non-`dynamic` `VarDecl`'s own name, empty otherwise). `chunk` compiles against the
  role's own qualified package for `TypeDecl`/`Plain` — a reasonable but not yet
  raku-cross-verified default per the design doc's "frozen plan" item, since a `Plain`
  statement's true package at composition time is whatever was ambient at the call site, not
  necessarily the role's own; `TokenRule` (the composing class's package, unknown until
  composition — the same ADR-0009 carve-out D6/D9 apply to class-body token/rule statements)
  keeps `chunk: None`. `register_role_decl` copies the ops onto a new `RoleDef::deferred_body`
  field (`#[allow(dead_code)]`); `deferred_body_stmts` remains the sole execution path — nothing
  reads `deferred_body` back yet. Pinned by a new compiler unit test
  (`role_declarations_precompute_deferred_body`) asserting the op count against `body_plan`'s
  own `Deferred` count and the `TypeDecl`/`TokenRule`/`declared_vars` classification for a
  `my $y = 1`/`token t { a }` pair. Verified via the full `t/` suite (28,037 tests). D8-2
  (consumer cutover behind the design doc's V1/V2 raku case tables) is next.
  **D8-2 landed 2026-08-09**: every consumer of a role's deferred body —
  `run_role_body_for_composition` (pun, `does`, runtime mixin) and
  `run_composed_role_deferred_body` (parametric composition) — now runs each op's
  precompiled `chunk` (`run_compiled_block_raw`, matching `run_block_raw`'s exact
  writeback/topic semantics — not `run_decl_expr`, which restores the topic per
  statement instead of once for the whole body) instead of re-compiling the raw
  statement on every composition; `deferred_body_stmts` is now write-only, kept only
  until D8-4 drops it. V1 (a nested class referencing a role type parameter, composed
  at two different type arguments) is covered by the existing
  `t/generics-nominalizable-class.t`; V2 (once-per-composition side-effect timing
  across five composition shapes) was checked against the pre-D8-2 baseline, not raku
  directly — mutsu's existing divergences from raku there are pre-existing and out of
  scope. Three real bugs surfaced during verification, all fixed: (1) only a
  `TypeDecl` op (a nested `class`/`role`, package-independent because every consumer
  explicitly overrides `current_package` for it) gets a compiled chunk — a `Plain`
  statement's true package is ambient at the composition call site, not knowable at
  role-declaration compile time, caught by `t/generics-nominalizable-class.t`'s `my
  package G { class A is Array[T] {} }`; (2) a role's `__hoisted` forward-reference
  shell is NOT a throwaway stub the way a class's is (`rust-gdb` confirmed it and the
  "real" declaration share the same compiled plan, full body included), so gating
  `deferred_body_ops` on `is_hoisted_shell` left it permanently empty for every
  top-level role with a deferred statement — caught by
  `t/indirect-declarator-names.t`'s role-body `my constant` naming an indirect method;
  (3) `RoleBodyOp::Deferred`'s catch-all (D7-4) also matches `SetLine`/stub markers
  that `walk_role_body`'s runtime dispatch never defers, so a method-only role body
  produced a non-empty `deferred_body_ops` from markers alone, spuriously entering
  composition and corrupting a `&f`-typed role parameter via a stray
  `bind_type_capture` call — caught by `t/role-double-parametric-args-distinct.t`.
  Verified via the full `t/` suite (28,037 tests), every whitelisted
  `S06-signature`/`S12-*`/`S14-*` roast file (release binary), and the bundled-library
  gate (`scripts/battery-testsuite.sh`, 158/164, `OO::Monitors` green).
  `news/2026-08/d8-2-role-deferred-body-consumer-cutover.md`. D8-3 (the
  `run_role_submethod` rider) and D8-4 (dropping `deferred_body_stmts`) remain.
  **D8-3 landed 2026-08-09**: `run_role_submethod` (the BUILD/TWEAK submethod runner
  `call_role_build_submethods` uses after `$value does Role` / `$value but Role` composes
  a role onto a non-`Instance` value — an `Int`/`Str`/etc., not a class construction,
  which stays on its own already-compiled path) now runs the submethod's precompiled
  `MethodDef::compiled_code` via `run_compiled_block_raw` instead of re-parsing/
  re-compiling `def.body` through the AST-walking `eval_block_value` carrier on every
  composition, falling back to `eval_block_value` only when a method has no compiled
  chunk (e.g. installed via a meta-programming hook). No behavior change: `$!attr`
  reads/writes inside the body were already resolved through env keys
  (`self`/`"!attr_name"`, seeded/read back by `run_role_submethod` itself, not through
  an instance attribute cell — `self` here is a `Mixin` over a non-`Instance`, so the
  compiled `GetLocal`/`SetLocal` ops' `self_instance_attrs` cell lookup no-ops both ways
  and execution falls through to the ordinary local-slot read/write, which `run_nested`
  bridges to/from `env` at entry/exit exactly as `eval_block_value` did) — verified via
  a raku-checked repro (scalar-attribute BUILD/TWEAK, BUILD-before-TWEAK ordering,
  captured-outer-lexical writeback, the non-mutating `but` form) pinned by
  `t/role-submethod-runtime-does-compiled.t`, the full `t/` suite (28,037 tests), and
  every whitelisted `S06-signature`/`S12-*`/`S14-*` roast file (release binary).
  Verification also surfaced two pre-existing, unrelated bugs in this same composition
  path — confirmed identical before and after this slice's change, so not regressions —
  filed as `todo/tickets/role-submethod-array-hash-attr-key-mismatch.md` (an `@!attr`/
  `%!attr` write inside such a submethod silently no-ops: only the scalar-shaped env key
  is seeded/read back) and `todo/tickets/role-submethod-runtime-does-parameterized-value.md`
  (a parameterized role's own type/value parameter is invisible inside its BUILD/TWEAK
  when composed this way). D8-4 (dropping `deferred_body_stmts`) remains, closing D8.
- [ ] **D9 — Remove `CompiledRoleDeclPlan::legacy_body`.** Preserve role puns, runtime mixins,
  conflicts, BUILD/TWEAK, custom HOWs, and EVAL. Same rule as D6: survey first, token/rule arms
  excluded.
  **Survey + design pass done 2026-08-08 (no code landed), same doc as D6.** The role body's
  structural difference from the class side: its non-declaration statements *escape
  registration* into `RoleDef::deferred_body_stmts` and run per composition — so D9 is
  sequenced after D8 by necessity, exactly as the D4 scoping pass concluded. Slices D9-1
  (role `is_stub` + our-scope plan facts, = D7-1), D9-2 (= D2b-2 role half), D9-3 (= D7-3),
  D9-4 (= D8 chunks), D9-5 (field drop, forced-instrument playbook). **D9-1 landed
  2026-08-08 — see D7-1 above (same slice). D9-3 landed 2026-08-08 — see D7-3 above (same
  slice): `parent_ops` covers the role side's typed `DoesDecl` encoding D9-3 asked for.**
- [ ] **D10 — Delete class/role AST registration walkers.** Keep only VM plan execution plus
  metadata helpers that do not inspect executable AST declarations. The token/rule arms of the
  body walk stay until their ADR-0009-scoped slice lands; D10 deletes everything else.
  **Design note 2026-08-08 (in the D6/D9 doc):** D10 needs no separate mechanism — after
  D6-4/D9-5 the walkers *are* the plan-op executors; D10 is a cleanup PR deleting residual
  raw-`Stmt` match arms and orphaned helpers, with grep-based completion criteria (no
  `Stmt::`-matching registration code outside token/rule routing and the `stmt_pool`-fed
  augment walker; no runtime `from_stmt` callers outside augment/EVAL fallbacks). If it grows
  beyond a cleanup PR, an earlier slice landed incompletely. The doc also fixes the
  cross-box dependency order: D2b-2 → D6-1..3 (D3-8a-d and D4-1/2 parallel) → D4-3 → D7 →
  D8 → D9 → field drops → D5 gate → D10.

### Phase E — one dispatch resolver and native handler table

Phase E depends on B and may proceed alongside C/D only where it does not touch their adapters.
The receiver-identity slice comes first because the reverted handler-ID attempt showed that
`value_type_name()` is not a dispatch owner: type objects appeared as `Package`, user Array
subclasses as `Any`, and representation aliases such as `Map` need explicit handling.

The resolver must cover every entry, and the inventory is larger than the opcode list: six
opcodes (`CallMethod`, `CallMethodMut`, `CallMethodDynamic`, `CallMethodDynamicMut`,
`HyperMethodCall`, `HyperMethodCallDynamic`), the non-opcode entries
(`vm_call_method_with_values`/`vm_call_method_mut_with_values`, the `vm_run_instance_method`
carrier, the two JIT shims, and the three `vm_call_helpers` fallback/temp-target entries), and
the `ArrayPush` fast-path opcode that currently bypasses method dispatch entirely. E5–E7 divide
that inventory; each also rewrites a function larger than any slice merged so far
(`exec_call_method_op` ~1.3k lines, `exec_call_method_mut_op` ~1.7k, the interpreter's
`call_method_with_values` ~3.8k), so expect them to subdivide from a measurement, as C6 did.

- [ ] **E1 — Introduce stable `TypeId` and receiver-owner resolution.** Resolve concrete values,
  type objects, user classes, builtin subclasses, role mixins, and representation aliases to an
  ordered TypeId MRO without initialization probes or per-call string scans. The owner decision
  lives at ~20 sites in 7 files today — including 14 per-MOP-entry fallbacks in
  `methods_classhow_dispatch.rs` and the alias logic baked into `value_type_name` itself — so
  this lands in two steps:
  - [ ] **E1a — shadow mode.** Compute the TypeId-based owner beside the string-based one and
    compare under a `MUTSU_VM_STATS`-gated counter; land with zero behavior change and drive the
    mismatch count to zero on `make test` plus targeted roast.
  - [ ] **E1b — switch.** Make the TypeId owner authoritative and delete the per-site string
    scans; the MOP fallback sites may follow as their own PR if the diff warrants it.
- [ ] **E2 — Give every native entry an exact handler ID.** Generate static type×method handler rows
  for pure arity handlers and stateful/special handlers; pin type-object, subclass, Map/Seq,
  Failure, and Rat-style cases that broke the reverted attempt.
- [ ] **E3 — Add the generation-keyed resolved-call cache.** Key by receiver TypeId, method symbol,
  call shape, and method generation; cache the ordered candidate sequence, not a second resolver.
- [ ] **E4 — Resolve native and user candidates in one MRO walk.** Preserve user shadowing,
  visibility, invocant definedness, arity/signature ordering, and native fallback in one result.
- [ ] **E5 — Route ordinary VM method calls through the resolver.** Cover zero/n-arg and named-call
  opcodes while retaining mutation/writeback semantics at the caller boundary.
- [ ] **E6 — Route mutation-aware and container calls through the resolver.** Cover celled,
  lvalue/rw, Proxy, index/attribute writeback, and mutable aggregate entry points.
- [ ] **E7 — Route metaobject, qualified, and re-entrant calls through the resolver.** Cover HOW,
  `.^lookup`/`.^can`, qualified/private dispatch, EVAL carriers, and method objects.
- [ ] **E8 — Model multi/proto/submethod ordering in the candidate sequence.** Remove parallel
  multi and submethod resolver entry points without changing tie-breaking or role conflicts.
- [ ] **E9 — Add resolver cursors for `samewith`/`nextsame`/`callsame`/`nextwith`.** Continue within
  the resolved sequence instead of re-entering name-based resolution.
- [ ] **E10 — Move wrap/unwrap mutation into canonical entries.** Bump the generation and remove
  wrap-specific cache-clearing paths.
- [ ] **E11 — Retire arity-specific lookup entry points.** Keep native arity functions only as
  handler implementations selected by `MethodEntry`.

### Phase F — derive introspection and remove compatibility state

- [ ] **F1 — Build `Method` objects from canonical entries.** Store ownership, visibility,
  signature, multi/submethod, wrap, and native metadata needed by introspection.
- [ ] **F2 — Derive `.^methods`, `.^can`, and method MRO views from the resolver/table.** Use the
  same TypeId MRO and visibility rules as calls.
- [ ] **F3 — Delete the per-type method-name lists and the test-only `METHOD_UNIVERSE`.** B1/B2
  already removed `METHOD_UNIVERSE` and runtime probing from the runtime path (both are
  `#[cfg(test)]`-only now); the live work is the fourteen per-type `&[&str]` name slices
  (~350 slots in `builtin_type_methods.rs`) that still feed `builtin_method_entries`. This is the
  explicit retirement of ANALYSIS §4-1's hand tables; retain only the generated native entry
  catalog that dispatch itself consumes.
- [ ] **F4 — Remove `ClassDef::methods` as a dispatch/registration mirror.** Leave type structure
  metadata beside the canonical method table and update snapshots/rollback to copy one source.
- [ ] **F5 — Remove superseded method caches and manual invalidation.** Keep only the
  generation-keyed resolved-call cache plus data caches that type mutation cannot invalidate.
  The inventory this box retires: ~72 manual clear sites across 12 files (the 32 in
  `vm_module_ops.rs` are four copies of one block and are a trivial first PR), the `String`-keyed
  `private_zeroarg_method_cache` with nine hand-clear sites of its own, and the *second*
  generation scheme `fn_resolve_cache_gen` that drives block-scope-exit clears in
  `accessors_misc.rs`. `native_ctor_plan_cache` is not "unrelated": it is cleared in lockstep
  with `fast_method_cache` at every one of those sites and must adopt the same generation.
- [ ] **F6 — Delete compatibility call carriers and dead resolver modules.** Remove the
  `run_instance_method` family — three live functions plus two resolved-path helpers in
  `class_dispatch.rs` and the `vm_run_instance_method` carrier, ~700 lines with ~40 references —
  and the name/arity lookup facades once no caller remains. Also delete the eight stale doc
  comments that reference the already-removed `run_instance_method_resolved`.
- [ ] **F7 — Delete obsolete declaration payloads and generic statement-pool entries.** Remove old
  `Register*` compatibility code and assert that migrated sub/class/role declarations retain no
  executable source AST.

### Completion gates

- [ ] **G1 — Full compatibility gate.** `make test`, `make roast`, GC stress, JIT stress, WASM, and
  bundled-library suites pass with no new quarantine.
- [ ] **G2 — Architectural guard tests.** Tests fail if a migrated declaration enters
  `stmt_pool`, retains `legacy_body`, dispatch bypasses `MethodEntry`, or introspection reads a hand
  name table.
- [ ] **G3 — Performance gate.** Benchmarks show no regression from initialization probing,
  per-call owner scans, registry locking, or repeated string interning; cache-hit dispatch remains
  generation-checked O(1).
- [ ] **G4 — Close the ADR and ANALYSIS items.** Mark ADR-0019 Accepted/Implemented and update
  ANALYSIS §1.1, §3.3, and §4-1 only after G1–G3 and all required slices above are complete.

## Rejected alternatives

### Keep declaration ASTs but rename the registration functions

This moves code without changing the execution model. The compiler would still emit a pointer into
an AST pool, and the MOP would still grow inside an AST walker.

### Build an introspection-only catalog

This is another hand table. Even if generated by probing native dispatch, it cannot represent
slow-path handlers or user/MOP mutations and therefore cannot make dispatch and introspection agree
by construction.

### Preserve separate native and user resolvers behind one facade

A facade reduces call-site count but leaves two sources of truth, two cache-invalidation schemes,
and no canonical candidate sequence for `samewith`/`nextsame` or HOW interception.

### Encode every declaration detail directly in `OpCode`

Large payloads would violate the opcode-size guard and make the hot dispatch enum carry cold
metadata. A compact pool index plus immutable typed plans keeps bytecode explicit without bloating
each instruction.

## Consequences

- Declaration registration becomes inspectable bytecode data and executable child chunks, with no
  executable AST retained for migrated declarations.
- Native and user methods gain one lookup, cache, MOP interception, and introspection surface.
- The migration touches broad compatibility-sensitive behavior; sequential one-slice PRs from
  current `main` keep each ordered layer independently testable without stacked PRs.
- Native handlers may remain split across source modules for maintainability. Their *registration*
  and lookup entry is singular even when their implementation is not.

## Implementation status

Stage 1 is in progress. `RegisterSub` now indexes a typed `CompiledSubDeclPlan` pool rather than
`CompiledCode::stmt_pool`; all normal, hoisted, nested-`our`, and top-level-method lowering sites use
the plan, and the VM no longer pattern-matches `Stmt::SubDecl`. The plan no longer carries an
AST body at all — `legacy_body` was removed in C6e-3c (2026-08-07); see
`news/2026-08/legacy-body-field-dropped.md`. Source-order sub plans now also carry the
stable keys of their primary and alternate compiled routines. Child compilation-unit imports now
retain their compiled-function tables and remap colliding keys together with every declaration-plan
reference. Registration now attaches the plan-selected compiled bodies to its temporary
`FunctionDef` candidates, and the common call helper uses those bodies directly instead of
rediscovering them by name and signature. Registration metadata for implicit `@_`/`%_`, empty
signatures, return-shape validation, stub identity, and redeclaration identity is now derived once
while lowering `CompiledSubDeclPlan`; normalized named/default signatures can therefore retain
their compiled bodies without a registration-time AST scan.

A declaration's own expressions — a computed name (`sub ::($name)`) and each custom trait's
argument (`is native(LIB)`, `is symbol('sym')`) — are no longer `Expr`s handed to the runtime and
compiled on demand at every registration. The compiler lowers each one to a `CompiledDeclExpr`
child chunk stored in the plan, and registration runs it through the VM's re-entrant bytecode
entry (`run_decl_expr`). A constant argument is recorded as a `DeclTraitArg::Literal` and needs no
chunk at all. The remaining `DeclTraitArg::Ast` variant is not a new fallback: it carries the
declaration kinds whose registration still walks a source declaration — the prelude's
forward-declaration pass and the class/role *method* walkers — and disappears with phase D.

Removing `CompiledSubDeclPlan::legacy_body` turned out to be gated on
`FunctionDef.body`, which had 58 readers, so C6 is subdivided above. The first of those has
landed: a routine's structural identity is now a memoized `FunctionDef::body_fingerprint()`
rather than a hash recomputed per read over a Debug rendering of the whole body AST. That
removed eight direct body reads and let the `func_def_fp_cache` side cache — which existed only
to hide that cost on the multi-redispatch path — be deleted outright.

A code object built from a registry routine is no longer AST-only either. `SubData` carries a
`compiled_routine: Option<Arc<CompiledFunction>>` naming the routine it *is*, filled from
`FunctionDef::compiled` by `Value::make_sub_for_routine` at the sites that build a `Sub` from a
def (`&foo`, `.candidates`, `.cando`, `nextcallee`, the operator fallback, the `block_stack`
entry `callframe().code` reads). `vm_call_on_value` and the native map-block entry prefer that
bytecode over compiling `data.body` on the fly, so no dispatch path executes an AST body for
these code objects. The three near-identical `SubData` literals behind `make_sub`,
`make_sub_with_id`, and `make_sub_owning` collapsed into one `new_code_object` core in the
process.

Every caller of the interpreter routine entry `call_function_def` now invokes the routine's
bytecode through one shared `call_routine_def`: the multi-deferral chain first, then the user
`prefix:`/`postfix:` operators, the reduce and hyper steps over a user `infix:`, `reduce` given
the operator as a routine value, and the selected `MAIN` candidate. What that removes is a
per-call *compile* of the routine's AST body, not a tree walk. `call_function_def` itself has been
deleted, and the inlined copy of its body that `exec_call` (the statement-position call entry)
carried is gone with it: that site now delegates to `call_routine_def` too, which closed C6d-1.

Its last gated shape was `multi_candidate_state_forces_interpreter`, which existed because a
multi candidate never received plan-compiled bytecode. Registration no longer discovers the
plan's compiled routines by re-deriving a registry key after the fact; the declaration hands
each candidate the routine its plan names, positionally — `compiled_routine_keys[0]` is the
primary signature and the rest follow `signature_alternates` in declaration order, which is the
same order registration installs them in. A multi candidate cannot be identified by its registry
key (candidates are keyed `/arity:types` with a `__m{N}` tiebreak), so the ownership is decided
where it is known, at the declaration, and `FunctionDef::compiled` is filled before the
candidate is inserted. The hoist pass, which registers each declaration a second time and whose
plan never sees a compiled body, now receives the same keys — for a single sub the source-order
install replaced the bytecode-less hoisted one, but a multi candidate is *appended*, so the
hoisted candidate survived and answered calls with no bytecode.

That exposed a latent collision: a multi's compiled routine was keyed by its positional
signature alone, so `multi f(:x($))` and `multi f(:y($))` were both `Pkg::f/0` and the second
body silently replaced the first. Dispatch tolerated it because `vm_call_resolve` re-checks the
body fingerprint and falls back to an on-the-fly compile, but installing bytecode by plan key
would have given one candidate the other's body. A colliding candidate now takes the
fingerprinted key shape that probe already tries next. Distinct keys in turn let resolution hand
the *named*-argument light-call path a per-candidate body for the first time, exposing that it
lacked the multi guard its positional twin has always carried — its name-keyed cache reused the
first call's candidate, and it pushes neither the multi-dispatch frame nor the samewith context
`callsame` needs. Both halves are pinned by `t/multi-named-only-candidates.t`.

`RegisterClass` and `RegisterRole` now likewise index typed class/role declaration-plan pools for
both source-order declarations and hoisted forward-reference shells. The VM no longer discovers
these declaration kinds by inspecting the generic statement pool, and their computed names and
custom-trait arguments run as compiled child chunks alongside the sub ones. Their `legacy_body`
adapters remain until structural registration and the remaining declaration-time expressions
(parents, aliases, deferred bodies) become plan operations and compiled child chunks.

The three declaration entry opcodes have been consolidated into `RegisterDecl`, whose compact
operand selects a tagged `CompiledDeclPlanRef`. Sub/class/role plans retain their typed cold-data
pools, while VM opcode dispatch and declaration-sensitive compiler analysis now share one bytecode
entry.

The dispatch layer now has a canonical `BuiltinMethodEntry` keyed by owner type and method name.
Built-in introspection derives its name list from those entries. The
runtime `Registry` owns those entries in a `(owner, method)` map, seeds them from static data at
construction without invoking native handlers, and serves built-in introspection from that map.
Replacing the transitional arity-specific dispatch functions with static native handler IDs and
routing dispatch reads through the shared entry remain open.

`MethodEntry` now carries both an optional built-in descriptor and ordered user candidates, so a
user override and its built-in fallback share one `(owner, method)` row. Class declaration,
rollback, augmentation, role composition, MOP `add_method`, partial registration, and `EVAL`
restoration synchronize user candidates into the table. User-method presence and overload
dispatch now read `MethodEntry`; `ClassDef::methods` remains only as the transitional
registration/on-demand-compilation write mirror.

Every built-in seed and user-candidate synchronization advances the registry's monotonic method
generation. Resolver and fast-dispatch entry points compare it with the interpreter's observed
generation and invalidate the resolve, fast, multi, constructor, and private-method caches as one
unit when it changes. This removes correctness dependence on registration sites remembering every
individual cache before the read side switches to `MethodEntry`.

User-method presence checks and overload lookup now read every candidate from
`MethodEntry.user_candidates`, including uncompiled EVAL/MOP candidates. The on-demand compiler
updates `ClassDef::methods` and immediately republishes the compiled candidates through the table,
so dispatch no longer falls back to the class method mirror when reading overloads.
