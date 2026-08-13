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

**Current progress: 43/53 slices merged (C6, C7, C8, D1, D2d, D3, D4, D5, D6, D7, D8, D9, and D10
complete; D2a and D2c-1/2/3 also landed, 2026-08-07; D2b-2, D2c-4, D6-1, D7-1/D9-1, D4-1, D4-2,
D4-3, D7-3, and D3-8a landed 2026-08-08; D3-8b, D3-8c, D3-8d, D3-9, D5-1, D5-2, D7-4, D8-1, D8-2,
D8-3, D8-4, D6-3e, D6-4, D9-5, and D10 landed 2026-08-09). Phase C is fully checked; the open box
is D2 (attributes and generated accessors), subdivided D2a-D2d — D2a, D2b-2, D2c-1/2/3/4, and D2d
are done; only the optional D2c-5 (A/B env-setup unification, gated on raku-behavior verification
of shape B's `has_class_scoped_subs` gate) remains open in D2. D3 (class methods/submethods as
compiled candidates) is closed: D3-1 through D3-7 landed (walker-drift unification plus the
compile-time `CompiledMethodDecl` precompute), D3-8a through D3-8d landed the method-body
main-pass-compilation cutover (the additive compiler-side half, the class-walker and role-walker
install-by-key cutovers, and the fallback-narrowing survey — which found and fixed a real
closure-nesting gap rather than just a straggler list), and D3-9 closed the box outright by
precomputing the last registration-time AST re-scan (see below). A 2026-08-08 scoping
pass had found D3's literal goal — compiling method *bodies*
through the single main-pass `Compiler` the way `SubDecl` does, instead of a throwaway
per-registration `Compiler::new()` — still fully open and scoped as a future D3-8, whose detailed
design (parity-first bare compile, per-decl `compiled_routine_key` on `CompiledMethodDecl`,
guarded registration install, D3-8a-d slice plan) landed 2026-08-08 as
`todo/deep/adr0019-d3-8-method-body-main-pass-compilation.md`. D4 (class declaration-time
expressions) is closed: its "aliases" piece is closed as already-bytecode-native (a lateral move,
not a gain), its "deferred class bodies" piece folded into D8, and its "parent expressions" piece
(a real re-parse-per-registration bug, constrained by a shared `&str` resolver API also used for
genuinely dynamic type-name concretization) landed as D4-1/D4-2/D4-3 (2026-08-08), closing the box
outright once D8 also landed (see below). A 2026-08-08 design sweep then produced detailed designs
for **every remaining
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

A 2026-08-10 design sweep produced detailed designs for **every Phase E box**, from a
four-way code survey (owner-resolution sites, dispatch entry points, cache/registry state,
multi/wrap/deferral machinery), recorded as
`todo/deep/adr0019-e1-typeid-receiver-owner.md`,
`todo/deep/adr0019-e2-e4-resolver-core.md`,
`todo/deep/adr0019-e5-e7-entry-routing.md`, and
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`, with condensed entries in each box
below. Notable outcomes: `TypeId` is a newtype over `Symbol` (dense ids rejected — the
registry COW-forks per thread), E2 handler rows are recognition metadata whose completeness is
counter-measured to zero *before* any read depends on them (the reverted attempt's fix), the
E4 resolver caches an ordered candidate sequence that E9 turns into deferral cursors, and the
survey corrected the entry-point inventory (see the Phase E preamble). The recommended
cross-box order is E1a → E1b (→ E1c) → E2a → E4a → E2b → E4b → E3 → E5 → E6 → E7 → E8 →
E9 → E10 → E11, with E10a movable earlier (anytime after E3).

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
- [x] **D3 — Encode class methods and submethods as compiled candidates.** Install ordinary, multi,
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
  **D3-9 landed 2026-08-09**: with D3-1 through D3-8d done, the box's own literal text ("install
  ... compiled candidates ... without walking `Stmt::MethodDecl`") is largely satisfied — the class
  and role walkers dispatch on the compiler-computed `ClassBodyOp::Method`/`RoleBodyOp::Method`
  markers (D6-3a/D7-4) and read `CompiledMethodDecl` by position, never re-matching the raw
  statement to decide what it is. A grep-and-read pass over `class_body_method_decl`/
  `role_body_method_decl`/`registration_class_augment.rs`'s `MethodDecl` arm found one remaining
  registration-time AST walk that fires on every registration (including a class/role declared
  inside a loop): `apply_auto_positional_slurpy`'s bare-`@_`-detection scan of `decl.body`, called
  twice per class method (once for the installed `MethodDef.param_defs`, once for the D3-8b
  install-by-key parity snapshot) and once per augmented method — even though
  `compile_method_body` (D3-8a) already runs the identical scan once, at compile time, to build the
  same method's compiled-bytecode signature. Fixed by precomputing the scan's `positional` result as
  `CompiledMethodDecl::uses_bare_positional_args: bool` inside `from_stmt` (mirroring the
  `is_stub`/`own_attribute_names`/`method_name_chunks` precompute precedent) and adding
  `method_signature_shared::apply_auto_positional_slurpy_from_flag`, a sibling of
  `apply_auto_positional_slurpy` that takes the scan result directly instead of `body: &[Stmt]`. The
  class walker's two call sites and the augment walker's one call site now read
  `decl.uses_bare_positional_args` instead of re-scanning — for the class walker (whose
  `CompiledMethodDecl`s come from the plan-lowered `method_decls` field, D3-7) this moves the scan
  from "every registration" to "once, at compile time"; for the augment walker (which has no
  compiled plan and still calls `CompiledMethodDecl::from_stmt` at registration time) it collapses
  two scans into the one already inside `from_stmt`. `compile_method_body`'s own call
  (`compiler/helpers_method_body.rs`) is untouched — it does not consume a `CompiledMethodDecl` and
  keeps calling the original `apply_auto_positional_slurpy(body, ...)`. Role methods are unaffected
  (`role_body_method_decl` never called the scanning function to begin with — a role method never
  gets the auto-`@_` insertion, by design). No behavior change: verified via the full `t/` suite
  (28,087 tests), the `d3_8a_byte_parity_tests` suite (including
  `auto_positional_slurpy_method_byte_parity` and `role_method_auto_positional_slurpy_not_applied`),
  every whitelisted `S12-methods`/`S12-attributes`/`S14-roles` roast file (only the pre-existing
  `trusts.t` failure per `TODO_roast/BLOCKERS.md`), and a manual raku-vs-mutsu comparison of a
  signature-less class method reading `@_`, a role method reading `@_` (must NOT auto-insert), and
  an `augment class` method reading `@_`. This closes ADR-0019's D3 box: no registration-time code
  matches a raw `Stmt::MethodDecl` (or re-scans its body) to decide what kind of declaration it is
  or what to install, outside the `stmt_pool`-fed augment walker's own one-shot
  `CompiledMethodDecl::from_stmt` build (already out of scope, matching D10's amended criterion) and
  the private-access validator's generic body recursion (`registration.rs`'s
  `validate_private_access_in_stmt`, which was already noted as unaffected by the original D3
  scoping pass — it recurses into every statement kind identically, not just method bodies, and
  builds no `MethodDef`).
- [x] **D4 — Compile class declaration-time expressions.** Cover computed names, traits, parent
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
  **Closed 2026-08-09.** With D4-1/2/3, D8 (deferred class bodies), and the D2d-precedent closure
  of the aliases piece all landed, every named piece of the box's own decision text is accounted
  for: computed names and custom-trait arguments (C5), parent expressions (D4-1/2/3), aliases
  (closed as-is), and deferred class bodies (D8). Three residuals are deliberate, not violations
  of the box, and stay open as their own tracked items rather than blocking this one: the
  `methods_qualified.rs:291` string-only `resolve_role_candidate` call (genuinely dynamic
  runtime-built type names with no source `Expr` to carry, so it cannot move to the `Expr` path by
  construction — noted in the D4 scoping pass as a permanent exception, the same shape as D10's
  augment-walker carve-out); `registration_role_body.rs`'s `concretized_parent` lookup, which
  still double-evaluates a role-body `does`'s bracket arguments once for that lookup and once for
  the D4-3-cutover composition (collapsing it to one evaluation is a real behavior change for a
  side-effecting bracket argument, explicitly deferred by D4-3's own note); and the pre-existing
  `also does Role[Args]` bracket-argument-dropping bug found while reading the parent-expression
  call sites, filed as `todo/tickets/also-does-role-bracket-args-dropped-in-class-body.md` (an
  independent correctness bug, not a plan-migration gap — `class_body_does_decl` never reads a
  `CompiledClassDeclPlan` field at all today, so there is no plan encoding for D4 to have migrated
  here in the first place).
- [x] **D5 — Drive user HOW operations from plan ops.** Execute `new_type`, `add_method`, trait
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
  **D5-1 landed 2026-08-09 (documentation only).** The ordering invariant every plan-driven
  registration step (D1-D10) must preserve, codified here as the box's own contract rather than
  left implicit in the survey doc: shell publish → body registration (direct, per-statement
  registry writes — a plan executor must never batch into a private `ClassDef` clobbered only at
  the end, since user code inside a trait or nested declaration can `^add_method` onto the class
  mid-registration and observe/mutate the registry the same walk is still populating) → HOW
  instantiate (`install_custom_class_how`, before trait dispatch, so a custom HOW sees every
  trait) → `new_type` → `add_method` (reads the *finished* registry, not the AST, so it is
  automatically correct once the registry itself matches — this is why D5 needed no independent
  migration) → class `trait_mod:<is>` → `compose` (must observe every prior step's side effects,
  e.g. `@!aspects` mutations a trait made). Every HOW install keys on the resolved storage name
  (lexical mangling is per-execution) — never the plan's static declared name. D6's `body_plan`
  cutover (D6-3d/e, D6-4) and D10's walker-classification cutover both preserve this sequence
  unchanged: `run_class_body`'s per-op dispatch loop still runs one `ClassBodyOp` at a time with
  the same interleaved direct-registry-write/re-publish shape the old per-`Stmt` walk had, and the
  DECLARE-keyword attach / `new_type`/`add_method`/`compose` drive (`vm_typedecl_ops.rs`) is
  untouched by either box — it still runs after `register_class_decl` returns, exactly as the
  survey found. The optional mechanical move the design doc floated (relocating the DECLARE-attach
  call next to `install_class_exporthow`) was skipped as flagged: behavior-neutral, low value,
  would only churn a working call site.
  **D5-2 landed 2026-08-09 (verification only, no code).** Re-ran the box's own completion
  criterion now that D6/D9/D10 have all landed (the survey's gate was written before any of them
  did): `scripts/battery-testsuite.sh` (OO::Monitors — the `EXPORTHOW::DECLARE`-based `monitor`
  declarator's own acceptance bar — green, matching the pre-D6/D9/D10 baseline byte-for-byte) and
  every whitelisted metamodel-adjacent roast file (`S12-meta/classhow.t`, `S12-meta/grammarhow.t`,
  `S12-methods/how.t`) on a release build — all pass; `S12-meta/exporthow.t` is not whitelisted
  and fails the same pre-existing, documented way (`TODO_roast/BLOCKERS.md`: rakudo itself lacks
  the `tryit` EXPORTHOW-SUPERSEDE method the test needs), unrelated to this gate. This closes
  ADR-0019's D5 box: the user-HOW protocol behaves identically with the registry populated by
  D1-D10's plan-op execution as it did under the retired AST walk, confirmed empirically rather
  than only by the survey's static reasoning.
- [x] **D6 — Remove `CompiledClassDeclPlan::legacy_body`.** Preserve augmentation, rollback,
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
  **D6-3e landed 2026-08-09**: flipped the default — `run_class_body_chunk_or_raw`
  now runs a statement's precompiled `body_plan` chunk unconditionally
  whenever one is present, instead of only under the `MUTSU_DROP_LEGACY_CLASS_BODY=1`
  instrument. The token/rule carve-out needed no extra code: those ops keep
  `chunk: None` permanently (D6-3b/c), so they fall through to the unchanged
  `run_block_raw(stmts)` branch exactly as before. `class_body_plan_forced()`
  and its `OnceLock` are deleted along with the instrument env var. Since the
  instrument had already been forced through the same verification sweep in
  D6-3d, this slice is a pure default flip with no new behavior — re-verified
  via the full `t/` suite (28,062 tests), the `S12-class`/`S12-construction`/
  `S14-roles`/`S05-grammar` roast files (957 tests, same pre-existing
  `open_closed.t` failure), and `scripts/battery-testsuite.sh` (158/164,
  byte-identical to the D6-3d baseline). `run_class_body`'s own dispatch loop
  still drives off the raw flattened `body: &[Stmt]` (needed to classify each
  statement's kind and, for `Attr`/`Method`/`Does`, to feed their handlers'
  raw-statement fallback paths) — dropping `CompiledClassDeclPlan::legacy_body`
  itself (D6-4) needs those three handlers threaded onto `ClassBodyOp`'s
  already-typed fields instead, which did not fit this slice.
  **D6-4 landed 2026-08-09**: `run_class_body` now iterates `body_plan:
  &[ClassBodyOp]` directly — the separate raw flattened `body: &[Stmt]`
  parameter, its `SyntheticBlock`-flatten, and its
  `collect_nested_class_has_decls` nested-`has` scan are all gone;
  `body_plan` already carries every op in the same order (it is built by
  the identical flatten-then-classify-then-nested-append transform,
  `crate::opcode::class_body_plan`), so no runtime preprocessing is needed
  any more. Two handlers changed shape to stop needing a raw `Stmt`:
  `class_body_does_decl` now takes the `Does` op's own `name: Symbol`
  instead of re-matching `Stmt::DoesDecl` for the same field, and
  `ClassBodyOp::Attr` gained a `raw: Stmt` field (populated unconditionally
  by `classify_class_body_stmt`, cheap the same way `ClassSub`/`CodeAlias`/
  `ProtoMethod`/`LeavePhaser` already carry their own `raw`) so
  `class_body_has_decl`'s existing our/my-attribute fallback — attribute
  names in `attr_decls` deliberately exclude `is_our`/`is_my` attributes,
  which therefore miss the primary name-keyed lookup and need the raw
  `HasDecl` statement to build an ad hoc `CompiledAttrDecl` — still has a
  raw statement to read without a separate lookaside list. `Method` and
  `Does` needed no such change: `class_body_method_decl` already took no
  statement, and `class_body_does_decl`'s only use of `stmt` was the same
  `name` its op already carries. `CompiledClassDeclPlan::legacy_body` and
  its one construction site are deleted, along with `register_class_decl`'s
  now-unused `body: &[Stmt]` parameter and its three call sites' trailing
  `body`/`&[]` argument (the VM opcode handler, role-pun/mixin synthesis,
  and `augment class` — the latter two already passed `body_plan: &[]`
  alongside it, so an empty plan alone now correctly drives zero
  iterations). Verified via the full `t/` suite (28,062 tests) and unit
  tests (701, including the fixed-up `class_declarations_precompute_body_plan`
  callers), the `S12-class`/`S12-construction`/`S14-roles`/`S05-grammar`
  roast files (957 tests, same pre-existing `open_closed.t` failure), and
  `scripts/battery-testsuite.sh` (158/164, byte-identical) — plus a hand
  comparison against `raku` exercising every `ClassBodyOp` variant in one
  class (an attribute with an `our`/`my` sibling to force the fallback
  path, `also does`, a class-scoped `sub`, a code alias, a `proto method`,
  and a `will leave` phaser), byte-identical output. This closes ADR-0019's
  D6 box.
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
- [x] **D8 — Compile role declaration-time bodies and traits.** Run parameterized-role and composed
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
  when composed this way).
  **D8-4 landed 2026-08-09**: dropped `RoleDef::deferred_body_stmts` outright — the raw
  `Vec<Stmt>` `walk_role_body` mirrored into it had been write-only (never read by any
  consumer) since D8-2 made every composition site run `deferred_body`'s precompiled
  ops instead. The catch-all statement arm in `walk_role_body` (which only ever did that
  push, in both its `is_parametric` and non-parametric branches — identically, a
  pre-existing redundant split) becomes a no-op; `RoleDeclCx::is_parametric` is dropped
  too, since that push was its only reader. Pure dead-field/dead-branch removal, no
  behavior change — confirmed via `grep` that nothing outside the push sites ever read
  the field. Verified via the full `t/` suite (28,037 tests) and every whitelisted
  `S06-signature`/`S12-*`/`S14-*` roast file (release binary). This closes ADR-0019's D8
  box now that D8-1..4 have all landed.
- [x] **D9 — Remove `CompiledRoleDeclPlan::legacy_body`.** Preserve role puns, runtime mixins,
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
  **D9-5 landed 2026-08-09**: unlike the class side, the role side needed no separate
  default-flip slice — `body_plan: Vec<RoleBodyOp>` (D7-4) had sat purely additive with zero
  non-test consumers, so this box went straight from "additive" to "sole driver, field
  dropped" in one PR, mirroring D6-4's shape (`walk_role_body` now iterates `body_plan`
  directly, no raw `Vec<Stmt>` to zip it against) but smaller: roles have only one
  `register_role_decl` call site (no pun/mixin/augment-style synthetic caller passing a
  hand-built body the way classes have three), and no nested-sub `has` collection to drop.
  `RoleBodyOp::Attr` gained a `raw: Box<Stmt>` field (boxed, matching `Deferred`'s existing
  boxing rationale — an unboxed `Stmt` on `Attr` alone would trip
  `clippy::large_enum_variant` against the marker-sized `Method`/`Parent` variants) so
  `role_body_has_decl`'s existing our/my-attribute fallback (identical rationale to the class
  side) still has a raw statement to read. `RoleBodyOp::Deferred`'s existing `raw` already
  covered the walk's other two raw-statement uses (the `__mutsu_stub_die`/`__mutsu_stub_warn`
  stub-marker check and the no-op `SetLine`/everything-else fallthrough), needing no change.
  `CompiledRoleDeclPlan::legacy_body`, its one construction site, and
  `register_role_decl`'s `body: &[Stmt]` parameter (replaced by `body_plan: &[RoleBodyOp]`,
  threaded through from the VM opcode handler) are deleted. Verified via the full `t/` suite
  (28,087 tests), all 701 Rust unit tests, the `S12-class`/`S12-construction`/`S14-roles`/
  `S05-grammar` roast files (957 tests, same pre-existing `open_closed.t` failure),
  `scripts/battery-testsuite.sh` (158/164, byte-identical), and a hand comparison against
  `raku` exercising every `RoleBodyOp` variant in one role composed onto a class (an
  attribute with a `my`-scoped sibling to force the fallback path, a nested `does`, a method,
  and a nested `my class`) — byte-identical output. Verification also surfaced a real,
  pre-existing, unrelated divergence — an `our`-scoped role attribute (`our $.x` inside a
  role body) is accepted by mutsu instead of raising raku's
  `X::Declaration::OurScopeInRole` — filed as
  `todo/tickets/role-our-scoped-attribute-not-rejected.md` rather than fixed here (out of
  scope for a structural field-removal slice). This closes ADR-0019's D9 box.
- [x] **D10 — Delete class/role AST registration walkers.** Keep only VM plan execution plus
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
  **Partial progress 2026-08-09** (grep survey after D6-4/D9-5 landed): the `from_stmt`
  criterion had two live violations — `class_body_has_decl`/`role_body_has_decl`'s
  our/my-attribute fallback, added in D6-4/D9-5 themselves to preserve behavior for an
  attribute the compiler's `attr_decls` collector deliberately excluded. Both collectors
  (`compile_class_attr_decls`/`compile_role_attr_decls`) already build a full
  `CompiledAttrDecl` via the identical `from_stmt` logic at COMPILE time; the class-side
  exclusion (`if !*is_our && !*is_my`) was dropped (the role side already had no such
  exclusion, so `role_body_has_decl`'s fallback had in fact been dead code since D7-4/D2c-4
  landed, just never noticed until this grep pass), closing the gap for good. Both functions
  now take the attribute's `Symbol` name and `.expect()` a lookup hit instead of falling back
  to `from_stmt` — sound because `attr_decls` and `body_plan`/`class_body_plan` walk the
  identical (flattened, nested-sub-surfaced) statement sequence, so every `Attr` op has a
  matching `attr_decls` entry by construction. `ClassBodyOp::Attr`/`RoleBodyOp::Attr` shrank
  back to a bare `{ name: Symbol }` marker (dropping the `raw: Stmt`/`raw: Box<Stmt>` field
  D6-4/D9-5 had just added for this fallback), and `CompiledClassDeclPlan`/`CompiledRoleDeclPlan`
  no longer carry any `#[allow(dead_code)]` shims from the pre-cutover "purely additive"
  phase. Verified via the full `t/` suite, all Rust unit tests, the `S12-class`/
  `S12-construction`/`S14-roles`/`S05-grammar`/`S12-attributes` roast files (the
  `S12-attributes/trusts.t` 6-subtest failure is pre-existing per `TODO_roast/BLOCKERS.md`,
  unrelated), `scripts/battery-testsuite.sh`, and a hand comparison against `raku` covering a
  class/role `our`/`my` attribute plus a nested-sub `has`.
  **Closed 2026-08-09 by amending the completion criterion.** The design note's original
  grep criterion — *zero* `Stmt::`-matching registration code outside token/rule/augment — is
  not met by the letter, and closing it to the letter is not worth doing. The remaining
  `Stmt::` reads are `ClassBodyOp::Other`/`ClassSub`/`CodeAlias`/`ProtoMethod`/`LeavePhaser`'s
  own `raw: Stmt` field and `RoleBodyOp::Deferred`'s `raw: Box<Stmt>` — each already a *typed
  op*, chosen by `body_plan`/`role_body_plan` without any AST walk; `raw` supplies only that
  one op's specific payload (`ProtoMethod`'s param defs and body clone, `LeavePhaser`'s inner
  body, `CodeAlias`'s source/target names, `Other`'s BEGIN/EVAL-swallow shape check and
  anon-method attribute validation, `Deferred`'s stub-marker detection). This is architecturally
  identical to the ADR's own C6 precedent, already blessed as permanent: a compiled routine's
  `FunctionDef` still keeps its raw AST body for the pure-interpreter fallback and for judging
  certain structural facts, and C6's own closing note treats that as the accepted end state,
  not a residual to delete later. The corrected D10 completion criterion is therefore: **no
  AST-shape dispatch in the class/role registration path** — no code that pattern-matches a
  raw `Stmt` to decide *what kind of declaration this is* or *what to do with it* — outside
  token/rule routing and the `stmt_pool`-fed augment walker. A typed op may carry its raw
  statement as an opaque payload for one-shot field extraction once its kind is already known;
  that is not dispatch and does not violate the criterion. Under that reading, D10 is
  satisfied: `run_class_body`/`walk_role_body` classify nothing themselves any more, dispatch
  entirely on the compiler-computed `ClassBodyOp`/`RoleBodyOp` tag, and the six named payload
  reads above are the exhaustive, enumerated, permanent exceptions (parallel to C6's
  `FunctionDef.body`) — any *new* raw-`Stmt` match added to the registration path outside this
  list, token/rule, or augment would be a regression against this box. Two of the six reads
  are cheap boolean *decisions* rather than payload extraction (`RoleBodyOp::Deferred`'s
  stub-marker check, `ClassBodyOp::Other`'s BEGIN/EVAL-swallow shape check) and could be
  precomputed at compile time to slightly harden the invariant further; filed as
  `todo/tickets/adr0019-d10-precompute-stub-and-swallow-flags.md` as optional opportunistic
  follow-up, not a new ADR box.

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
**Inventory corrections (2026-08-10 survey):** `call_method_with_values` lives in
`runtime/methods_call_dispatch.rs`, not `runtime/methods.rs`; `call_method_mut_with_values`
(`runtime/methods_mut_dispatch.rs`, ~2.6k lines) is a second slow path of comparable size and
belongs to E6's inventory; `exec_call_method_dynamic_mut_op` reaches the interpreter with no
native or compiled probe at all; and `exec_hyper_method_call_dynamic_op` lacks the
user-override gate its static twin has — the last two are pre-existing behavior divergences to
raku-verify and close during E6, not just refactor targets. The detailed designs for this
phase are `todo/deep/adr0019-e1-typeid-receiver-owner.md` (E1),
`todo/deep/adr0019-e2-e4-resolver-core.md` (E2/E3/E4),
`todo/deep/adr0019-e5-e7-entry-routing.md` (E5/E6/E7), and
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md` (E8/E9/E10/E11).

- [ ] **E1 — Introduce stable `TypeId` and receiver-owner resolution.** Resolve concrete values,
  type objects, user classes, builtin subclasses, role mixins, and representation aliases to an
  ordered TypeId MRO without initialization probes or per-call string scans. The owner decision
  lives at ~20 sites in 7 files today — including 14 per-MOP-entry fallbacks in
  `methods_classhow_dispatch.rs` and the alias logic baked into `value_type_name` itself — so
  this lands in two steps:
  **Design 2026-08-10** (`todo/deep/adr0019-e1-typeid-receiver-owner.md`): `TypeId` is a
  newtype over `Symbol` (dense ids rejected: the registry COW-forks per thread, and
  `MethodEntryKey.owner` is already a Symbol); one static `BuiltinTypeInfo` catalog —
  adjudicated against raku, not against the union of the current tables — replaces the four
  divergent builtin MRO tables (`builtin_type_parents`, `Registry::builtin_mro_table`,
  `builtin_type_mro_chain`, `builtin_type_distance`'s inline table); one classifier
  `receiver_dispatch_class`/`dispatch_mro` produces the ordered chain plus definedness plus a
  `ReceiverExec` hint (e.g. `ArrayStorageDelegate` for `is Array` subclasses). E1a's shadow
  target is "reproduce the site's current decision" with an accepted-mismatch ledger for the
  deliberate differences, which flip in E1b.
  - [x] **E1a — shadow mode.** Compute the TypeId-based owner beside the string-based one and
    compare under a `MUTSU_VM_STATS`-gated counter; land with zero behavior change and drive the
    mismatch count to zero on `make test` plus targeted roast.
    **Landed 2026-08-10** (`#6156`): `TypeId` (`src/type_id.rs`, a newtype over `Symbol`) plus
    `WellKnownTypes` for O(1) comparisons; `BuiltinTypeInfo`, one static catalog
    (`src/builtins/builtin_type_catalog.rs`) replacing, for the new classifier only, the four
    divergent builtin MRO tables — every row adjudicated against `raku -e 'say T.^mro; say
    T.^roles'` (Rakudo 2026.06), pinned by a `#[test]`; `Interpreter::receiver_dispatch_class`/
    `dispatch_mro` (`src/runtime/receiver_class.rs`), one classifier resolving
    Instance/Package/ParametricRole (registry MRO + catalog tail splice for builtin subclasses),
    concrete builtins, Enum (enum type ahead of Int), and role Mixins (allomorph shortcut +
    role-first chain) to one ordered `TypeId` chain; four shadow probes
    (`Interpreter::shadow_check_owner`) at the dispatch-critical owner sites, comparing the
    classifier's answer against each site's existing string-based decision under new
    `owner_shadow_checks`/`owner_shadow_mismatches` `MUTSU_VM_STATS` counters. Zero behavior
    change — the probes only compute-compare-count. Verified via a `MUTSU_VM_STATS=1` sweep over
    all `t/*.t` plus roast S02/S06/S12/S14 (~26k shadow checks, ~611 mismatches, ~2.3%), every
    mismatch bucketed into exactly three explained causes (no unexplained bucket): Enum receivers
    (`value_type_name` says `"Int"`, raku's `.^mro` puts the enum type first — the classifier is
    right, E1b fixes the legacy sites), role Mixin/ParametricRole generic-collapse
    (`value_type_name` reports `"Any"`/`"Package"`/the pre-mixin type while ignoring the role
    layer — exactly E1's target failure mode), and a `multi_arg_type_keys` Package-collision case
    filed as its own ticket (`todo/tickets/multi-arg-type-keys-package-collision.md` — unconfirmed
    as a live bug after two repro attempts, kept as a small standalone follow-up). Two tickets
    filed from findings along the way: `todo/tickets/mixin-role-order-not-tracked.md` (mutsu's
    `MixinOverrides` carries no role-application-order, so `(0 but A) but Z` resolves collisions
    alphabetically instead of "later wins" like raku — the E1a classifier deliberately mirrors the
    same wrong-but-deterministic order rather than diverging further, since fixing it needs an
    order field on every mixin construction site, out of scope for a shadow-only box) and
    `multi-arg-type-keys-package-collision.md` above. `make test` (2990 files/28109 tests) green,
    unchanged. **This landed-note and its news entry are retroactive** — filed alongside E1b
    (below) after the fact; E1a's own PR did not update either.
  - [x] **E1b — switch.** Make the TypeId owner authoritative and delete the per-site string
    scans; the MOP fallback sites may follow as their own PR if the diff warrants it.
    **Landed 2026-08-10**: the classifier becomes authoritative at the three E1a shadow sites
    that were safe to cut over unconditionally — `call_method_with_values`'s augment gate,
    `dispatch_instance_and_fallback`'s value-type dispatch pick, and the `are_actual_type_name`/
    `are_value_matches_type`/`.^add_fallback` fallback-arm trio — via two new helpers,
    `Interpreter::dispatch_owner_chain`/`dispatch_owner_name` (`src/runtime/receiver_class.rs`).
    These are a `dispatch_mro` variant that skips a role Mixin's role-`TypeId` prefix, returning
    the *inner* value's own chain instead: every call site consulting this chain runs strictly
    *after* a dedicated, role-registry-aware path already tried role methods for the same
    receiver, so re-deriving a role owner here would at best repeat that lookup and at worst
    regress — confirmed by a direct repro (`augment class Array { method my-foo {...} }; (@a but
    R).my-foo` resolved fine under the old `value_type_name`-unwrap-to-inner behavior; using the
    raw role-first chain instead made it unresolvable). `methods_qualified.rs`'s qualified-dispatch
    membership check and `type_matching.rs`'s `type_matches_value` both moved to walking the full
    `dispatch_owner_chain`/`dispatch_mro` chain (not just its first element) — required, not just
    thorough, since an Enum's chain is `[EnumType, Int, Cool, Any, Mu]` and a plain `Int`
    constraint must still match an enum value through the `Int` link further down. The two
    remaining divergent MRO tables this box's scope covered (`type_inherits`/
    `builtin_type_mro_chain` in `methods_call_helpers.rs`) were deleted outright, their two call
    sites (`methods_qualified.rs`, `vm_call_helpers.rs`'s `.+`/`.*` all-candidates count) now
    reading the classifier's chain. `try_compiled_method_or_interpret_inner`'s `class_sym` site
    needed no behavior change — it was already provably classifier-equivalent by construction
    (confirmed zero mismatches in E1a's sweep) — so its now-redundant shadow probe was removed
    without adding a chain-walk allocation to that hot fast-dispatch path. Deliberately NOT cut
    over: `multi_arg_type_keys` (`vm_call_method_compiled_cache.rs`) — unlike the other three
    original E1a sites, its cutover is not a shadow-mode-safe refactor but IS the fix for
    `todo/tickets/multi-arg-type-keys-package-collision.md`, so it stays on `shadow_check_owner`
    until that ticket is picked up on its own, rather than bundling an unverified behavior change
    into this switch. MOP fallback consolidation (E1c) stays out of scope. Verified via `make
    test` (28,121 tests) and a full `make roast` (218,774 tests), both green.
  - [x] **E1c — MOP fallback consolidation.** Collapse the 13+8 per-MOP-entry owner-fallback
    arms into one classifier-backed `mop_receiver_owner` helper.
    **Landed 2026-08-10**: `Interpreter::mop_receiver_owner` (`src/runtime/receiver_class.rs`) —
    Package/Instance report their own name directly, everything else resolves through
    `dispatch_owner_name` (the E1b classifier) instead of `value_type_name` — replaces the
    22 duplicated `_ => value_type_name(&args[0]).to_string()` / `let name = match ... {
    Package(..)=>.., Instance(..)=>.., _=>value_type_name(..) }` fallback sites across
    `methods_classhow_dispatch.rs` (13), `methods_classhow_mro.rs` (3),
    `methods_classhow_parents.rs` (3), `methods_classhow_builtin_methods.rs` (1),
    `methods_classhow_lookup.rs` (1), and `methods_classhow_method_obj.rs` (1) — one more than
    the design doc's 21-site estimate because `dispatch_classhow_roles`'s Mixin arm nests two
    fallback arms in one match. Four call sites (`classhow_lookup`, `classhow_find_method`,
    `dispatch_classhow_roles`, `filter_mro_unhidden`) needed `&self` -> `&mut self` promotion
    since the classifier caches the registry MRO lookup (`class_mro`) mutably; every caller of
    those four was already `&mut self`, so the promotion did not cascade further. Sites with an
    extra non-Package/Instance arm the plain 3-arm pattern didn't cover (`RakuAst` in
    `classhow_mro_names`/`dispatch_classhow_parents`/`method_table`/`collect_can_methods`,
    `Enum` in `collect_can_methods`) kept that arm explicit ahead of the fallback rather than
    folding it into the helper, since those arms carry different logic than a plain owner
    lookup. Two sites deliberately broadened behavior in a direction the E1b rules already
    established as correct: `"parameterize"`'s `base` and `dispatch_classhow_roles`'s Mixin
    fallback previously had no `Instance` arm at all (falling to `value_type_name`'s "Any"
    answer for a receiver that in practice is never an Instance); `mop_receiver_owner` now
    resolves an Instance there too, consistent with the owner rules E1b already made
    authoritative elsewhere — not exercised by any known test, called out here for review
    honesty. Verified via `make test` (2994 files/28129 tests) and a full `make roast` (1435
    files/218,748 tests), both green — the three `make roast` failures seen on the first run
    (`spurt.t`, `socket-recv-vs-read.t`, `S17-supply/syntax.t`) were confirmed to be artifacts of
    an interrupted prior local run (stale `temp-file-RT-126006-test` plus load-induced timing)
    by re-running each file individually, not regressions from this change.
- [ ] **E2 — Give every native entry an exact handler ID.** Generate static type×method handler rows
  for pure arity handlers and stateful/special handlers; pin type-object, subclass, Map/Seq,
  Failure, and Rat-style cases that broke the reverted attempt.
  **Design 2026-08-10** (`todo/deep/adr0019-e2-e4-resolver-core.md`): rows are *recognition
  metadata* (owner, name, arity mask, TYPE_OBJECT_OK/MUTATES/SPECIAL flags), not function
  pointers — invocation stays in the arity cascades until F3. Coverage is measured to zero via
  a `native_call_unmodeled` counter plus a cfg(test) inverse probe before any read depends on
  rows (the ~700 cascade arms vs ~350 catalog slots gap is the reverted attempt's failure
  mode). The admission-gate checks split per a classification table: method-identity facts
  become row flags; receiver-state facts become resolver guards, deduplicating
  `try_native_method_raw` and its twin `should_bypass_native_fastpath`.
  - [ ] **E2a — row schema + instruments + pinned regression tests.** Zero behavior change.
  - [ ] **E2b — drive `native_call_unmodeled` to zero** through the gate-classification table.
    **Progress 2026-08-10** (first slice): the E4a sweep's own `MUTSU_VM_STATS=1` run over
    `t/` found the coverage *check* itself over-counting — `record_native_row_coverage`
    did a flat point lookup at the receiver's own concrete owner (via
    `dispatch_owner_name`), so a method declared on `Any`/`Mu` but recognized for every
    concrete receiver by the shared arity-0 cascade arms (`so`, `not`, `defined`, the
    `DEFINITE` quoted pseudo-method) was flagged unmodeled at every owner even though it
    was already correctly served. Fixed by walking the full
    `Interpreter::dispatch_owner_chain` (same principle E4a's `resolve_sequence` already
    applies) instead of checking only the first element, plus four hand-added rows for
    `Any`/`Mu`'s universal methods (E2a's generator only covered 11 concrete-type owners
    from `builtin_sample_value`, which has no representative sample for an abstract
    owner). A fresh `t/`-wide sweep (2996 files) confirmed `native_call_unmodeled` dropped
    from 37904 to 12154 (-68%) — `Str x so` alone had been 20392 of the original total.
    Remaining unmodeled pairs (`Match`, `Pair`, `Seq`, `Array x list/item`, `FatRat`,
    exception types, `RakuAST::*`) are genuinely missing per-owner rows, not measurement
    artifacts, and are the next E2b sub-slices. `make test` green; two new unit tests
    (`any_mu_universal_rows_are_backed_by_the_cascade_for_multiple_receiver_types` in
    `native_method_row.rs`).
    **Progress 2026-08-10** (second slice): hand-probed `Pair`/`Seq` rows (67 entries,
    curated candidate list against a real `Value::pair`/`Value::seq` sample — neither
    owner has a `builtin_type_method_names` entry to draw candidates from). A fresh
    `t/`-wide sweep confirmed `native_call_unmodeled` dropped further from 12154 to 8654
    (cumulative -77% from the original 37904); every `Pair`/`Seq` entry disappeared from
    the breakdown. Remaining top pairs are now `Match` (~1700, needs a real Match sample
    — deferred, more involved to construct), `Array`/`List` (`list`/`item`/`Slip`), `Str`
    (`uniprop`/`AST`), `Int` (`fmt`/`rand`/`FatRat`), `FatRat::floor`, exception types
    (`X::AdHoc`, `CX::Warn`), and `RakuAST::StatementList::gist`. New
    `pair_seq_rows_are_backed_by_the_cascade` inverse-probe test. `make test` green.
    **Progress 2026-08-10** (third slice): hand-probed `Match` rows (78 entries), sourced
    from a real Match value (`'foo' ~~ /f(o)(o)/` run through the interpreter, `$/` read
    back) rather than `builtin_sample_value` — `Match` has no
    `builtin_type_method_names` entry either. Two candidate sources were probed against
    that sample: the explicit 0-arg arm in `methods_0arg/mod.rs` (`"from" | "to" | "pos"
    | ...`), and every `Str` row name — that arm's `_` default falls through to
    `native_method_0arg` on the matched string, and the narg cascades for string-shaped
    methods coerce via `target.to_string_value()` regardless of receiver type, so most of
    `Str`'s surface is reachable from a `Match` receiver too; only names the probe
    actually recognized (non-zero arity bits) were kept. `so`/`not`/`defined` stay absent
    (a Match's `dispatch_owner_chain` includes `Any`, so the chain-walk already covers
    them, confirmed by a new `match_so_not_defined_are_covered_via_the_any_chain` test).
    A fresh `t/`-wide sweep (MUTSU_VM_STATS=1 over all `t/*.t`, 8-way parallel) confirmed
    `native_call_unmodeled` dropped from 8654 to 5431 (cumulative -85.7% from the
    original 37904); `Match` disappeared from the top-40 breakdown except two
    single-digit non-method-surface hits (`Match x Stringy` from a role-coercion check,
    `Match x __mutsu_zen_angle` from word-list quoting internals — left unmodeled, out of
    scope for the documented Match method surface this slice targets). Remaining top
    pairs are now `Array`/`List` (`list`/`item`/`Slip`, ~470/121/113/58/55 hits), `Str`
    (`uniprop`/`AST`, ~365/326), `Hash` (`pick`, ~200), `Int` (`rand`, ~150),
    `RakuAST::StatementList` (`gist`/`statements`, ~141/31), exception types (`X::AdHoc`,
    `CX::Warn`), and `Buf`/`Set`/`Failure` odds and ends. New
    `match_rows_are_backed_by_the_cascade` inverse-probe test. `make test` (732 unit
    tests) and a targeted `prove` over all `t/*match*.t`/`t/*regex*.t`/`t/*grammar*.t`
    (204 files/1716 tests) both green.
    **Progress 2026-08-10** (fourth slice): hand-probed `List`/`Array` extra rows (30
    entries total) for names absent from `LIST_METHODS` (the candidate source
    `builtin_type_method_names` uses for these two owners) -- `list`/`item`/`Slip`/
    `cache`/`sink`/`invert`/`WHICH`/`AT-POS`/`EXISTS-POS`/`is-lazy`/`Capture`/`hyper`/
    `race`/`Supply` on both owners, plus `dynamic` on `Array` only (the cascade's own
    guard in `methods_0arg/mod.rs` restricts `.dynamic` to non-`List`-kind Array
    values, confirmed by a new `list_dynamic_is_not_recognized` test). Same
    probe-the-real-cascade discipline as the `Match`/`Pair`/`Seq` slices. A fresh
    `t/`-wide sweep (after confirming `target/debug/mutsu` was actually rebuilt --
    `cargo test --lib`/`cargo clippy --lib` do not rebuild the `mutsu` bin target,
    only `cargo build` does, and the first post-edit sweep attempt silently ran
    against a stale binary) confirmed `native_call_unmodeled` dropped from 5431 to
    4377 (cumulative -88.5% from the original 37904); `Array`/`List` disappeared
    entirely from the top-40 breakdown. Remaining top pairs are now `Str`
    (`uniprop`/`AST`/`indent`, ~365/326/31), `Hash` (`pick`/`item`/`EXISTS-KEY`/
    `AT-KEY`, ~200/25/25/21), `Int` (`rand`/`elems`/`WHICH`/`clone`, ~150/23/17/18),
    `RakuAST::StatementList`/`RakuAST::Statement::Expression` (`gist`/`statements`/
    `expression`, ~141/31/21), exception types (`X::AdHoc`, `CX::Warn`,
    `X::TypeCheck::Assignment`), `Failure` (`defined`/`exception`/`sink`, ~46/30/21),
    `Buf` (`list`/`decode`/`elems`/`values`, ~43/39/36/37), `Set`/`SetHash` (`keys`/
    `elems`/`gist`, ~37/35/21/19), and `Match x Stringy`/`Backtrace x list`/`Version x
    Str`/`Junction x gist`/`Date x Str`/`Seq x is-lazy` (17-50 each). New
    `array_list_extra_rows_are_backed_by_the_cascade` and
    `list_dynamic_is_not_recognized` tests. `cargo test --lib` (734 tests) and a
    targeted `prove` over all `t/*array*.t`/`t/*list*.t` (214 files/2272 tests) both
    green.
    **Progress 2026-08-10** (fifth slice): closed the `Str`/`Hash`/`Int` gap named
    above, plus generalized seven more universal pseudo-methods
    (`self`/`clone`/`WHERE`/`WHICH`/`sink`/`item`/`serial`) to `Any`-level rows
    alongside the existing `so`/`not`/`defined`/`DEFINITE` ones -- found by reading
    every match arm in `dispatch_core_coerce.rs`/`dispatch_core_math.rs` rather than
    inferring from the sweep breakdown alone, since each has a receiver-type-agnostic
    `_ => ...` fallback and therefore covers every owner at once, not just the three
    this slice targeted. The `Str`/`Hash`/`Int`-specific additions (23/12/26 rows)
    came from reading `dispatch_core_unicode.rs` (the `uniprop`/`ord`/`uniname`/
    `uninames`/`unival`/`univals`/`chrs`/`bytes` cluster) and `dispatch_core_numeric.rs`
    (the `rand`/`elems`/`lsb`/`msb` cluster, tried for every receiver by name only --
    not gated on a numeric `ValueView`, the same reason `Int` recognizes `flip`/`uc`
    via `target.to_string_value()`). `Str.sprintf`'s recognition is receiver-content-
    dependent (needs exactly one `%`-directive), so its inverse-probe test uses a
    dedicated format-string sample instead of the generic `"abc"` one. A fresh
    `t/`-wide sweep confirmed `native_call_unmodeled` dropped from 4413 (this
    session's file set; 3001 files, close to the fourth slice's 4377 over a
    slightly different set) to 2823 (cumulative -92.5% from the original ~37904);
    `Str`/`Hash`/`Int` disappeared entirely from the top-40 breakdown. Remaining top
    pairs are now `RakuAST::StatementList`/`RakuAST::Statement::Expression`
    (`gist`/`statements`/`expression`, ~141/31/21), exception types (`X::AdHoc`,
    `CX::Warn`, `X::TypeCheck::Assignment`), `Failure` (`defined`/`exception`/`sink`,
    ~45/30/18 -- `sink` persisting despite the new `Any` row means `Failure`'s
    coverage-check chain walk is not finding it for this specific site, worth a
    dedicated look next slice), `Buf`/`Buf[uint8]`/`utf8` (`list`/`elems`/`decode`/
    `values`/`raku`/`gist`, ~20-45 each), `Set`/`SetHash` (`keys`/`elems`/`gist`/
    `raku`, ~15-36 each), and `Match x Stringy`/`Anyxgist`/`Anyxraku`/`ProfiledGxraku`/
    `Nilxraku`/`Backtracexlist`/`VersionxStr`/`Junctionxgist`/`DatexStr`/`Seqxis-lazy`/
    `Signaturexgist` (14-50 each). New `any_second_batch_universal_rows_are_backed_by_the_cascade`
    and `fifth_slice_extra_rows_are_backed_by_the_cascade` tests. `cargo test --lib`
    (736 tests) and `make test` (3001 files/28167 tests) both green.
    **Progress 2026-08-10** (sixth slice): found and fixed a root-cause gap in
    `record_native_row_coverage` itself (`receiver_class.rs`), not just missing
    rows -- the `Buf`/`Blob`/`utf8`/`FatRat` families are folded to a single
    dispatch owner (`Blob`, `Rat`) whose native table actually serves them
    (`canonical_builtin_owner`, `builtin_type_methods.rs`), but raku's own
    `.^mro` for these types does NOT include the folded owner (confirmed
    against real `raku`: `Buf.new.^mro` is `Buf, Any, Mu`, not `Buf, Blob, Any,
    Mu`), so `dispatch_owner_chain`'s walk correctly omits it too and could
    never find the folded owner's rows no matter how many were added. The
    coverage check now also tries each chain owner through
    `canonical_builtin_owner` as a fallback lookup, closing the whole
    `Buf`/`Buf[uint8]`/`utf8`/`Blob[uint8]` family and `RatxFatRat`-shaped gaps
    at once (this fold fix alone dropped the counter from 2825 to 2508). Also
    closed the `Blob` row gaps the fold then exposed
    (`decode` was under-counted as A0-only -- its A1 arm needs a real encoding
    name like `"utf-8"`, not the generic empty-string dummy
    `native_method_arities` tries; `values`/`List`/the `read-*` native-endian
    accessor family were simply never probed, `Blob` not being one of the
    original 11 owners). Then closed two more owner families with no
    `builtin_type_method_names` entry (same situation as `Pair`/`Seq`/`Match`):
    `Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/`MixHash` (hand-probed against real
    `set(...)`/`SetHash.new(...)`/etc values -- `grab` on an immutable
    `Set`/`Bag`/`Mix` IS pure-cascade-recognized, since it always errors
    "immutable" but `Some` still counts; the mutable `SetHash`/`BagHash`/
    `MixHash` variant's `grab` is slow-path-only, so those three deliberately
    have no `grab` row), and `RakuAST::StatementList`/
    `RakuAST::Statement::Expression` (hand-probed against a real `Str.AST`
    parse tree). A fresh `t/`-wide sweep confirmed `native_call_unmodeled`
    dropped from 2825 (this session's file set after the fifth slice's PR
    merged) to 1900 (cumulative -95% from the original ~37904); none of
    `Buf`/`Blob`/`utf8`/`FatRat`/`Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/
    `MixHash`/`RakuAST::StatementList` remain in the top-40 breakdown.
    Remaining top pairs are now exception types (`X::AdHoc`/`CX::Warn`/
    `X::TypeCheck::Assignment`, `message`/`resume`/`backtrace`/`defined`,
    14-89 each) and `Failure` (`defined`/`exception`/`sink`/`so`/`handled`,
    9-45 each -- `sink` still not resolving via the `Any` row from the fifth
    slice despite this slice's fold-lookup fix, worth a dedicated look), plus
    a long tail (`Backtrace`/`Backtrace::Frame`, `Version`, `Date`/`DateTime`,
    `Signature`, `Range x hyper/lazy/int-bounds`, `Map x raku`, `Duration x
    Numeric`, `Mu x defined`, `CallFrame x defined`, `RakuAST::IntLiteral x
    value`, 9-26 each). New `setbagmix_rows_are_backed_by_the_cascade` and
    `rakuast_statementlist_rows_are_backed_by_the_cascade` tests. `cargo test
    --lib` (738 tests) and `make test` (3002 files/28169 tests) both green.
    **Progress 2026-08-10** (seventh slice): found and fixed a second
    root-cause chain-walk gap, this time in `builtin_type_catalog.rs` rather
    than the coverage-check function itself. `Failure`'s
    `dispatch_owner_chain` was just `["Failure"]` -- no continuation to
    `Any`/`Mu` at all -- because `Failure` is never declared as a real class
    anywhere in mutsu (built purely via `Value::make_instance`), so it had no
    catalog row and the registry has no model of its ancestry either. Every
    built-in `X::*` exception type had the same problem one level up: each
    registers `Exception` as its parent (`BUILTIN_PARENT_TYPES`), but
    `Exception` itself was never registered as an actual class, so
    `compute_class_mro`'s implicit-`Any` rule (which only fires for a class
    present in the registry) never applied to it, and every `X::*` type's
    registry MRO dead-ended at `Exception` (e.g. `X::AdHoc`'s registry MRO
    was `["X::AdHoc", "Exception"]`). `CX::Warn` had it worse still: built via
    `Value::make_instance` with no registered parent at all, so its chain was
    the bare `["CX::Warn"]` and never even mentioned `Exception`. Fixed with
    three new `builtin_type_catalog` rows -- `Failure` (`["Failure", "Nil",
    "Cool", "Any", "Mu"]`, raku: `Failure ISA Nil`), `Exception` (`["Exception",
    "Any", "Mu"]`, letting `class_chain_with_catalog_tail`'s splice logic
    patch every `X::*` type at once), and `CX::Warn` (`["CX::Warn",
    "Exception", "Any", "Mu"]`, needed directly since its own registry chain
    never reaches `Exception`) -- all three verified against real `raku`
    `.^mro` output. This alone made the `Any`-declared universal rows
    (`so`/`not`/`defined`/`self`/`clone`/`WHERE`/`WHICH`/`sink`/`item`/
    `serial`) finally resolve for every `Failure`/`X::*`/`CX::Warn` receiver,
    which is why `Failure`'s `sink`/`so`/`defined` gap from the sixth slice's
    notes closed without a `Failure`-specific row. Then hand-probed the
    concrete per-type rows still needed on top (`message`/`resume`/
    `backtrace`/`gist`/`raku`/`Str`/`Bool`/`throw`/`exception`/`handled`,
    varying per type -- e.g. `CX::Warn` lacks `throw`/`raku`, `Failure` lacks
    `message`/`backtrace` -- confirmed by direct probe rather than assumed
    shared, since `CX::Warn`'s own `resume` arm in `methods_0arg/mod.rs` is
    gated on its exact class name, not a generic exception check). A fresh
    `t/`-wide sweep confirmed `native_call_unmodeled` dropped from 1818 (this
    session's file set) to 1498 (cumulative **-96%** from the original
    ~37904); none of `Failure`/`X::AdHoc`/`CX::Warn`/`X::TypeCheck::Assignment`
    remain in the top-40 breakdown. Remaining top pairs are now a long,
    diffuse tail with no single dominant owner (`Match x Stringy`, `Any x
    gist/raku/hash`, `Backtrace`/`Backtrace::Frame`, `ProfiledG x raku`, `Nil
    x raku/gist`, `Version`, `Junction x gist`, `Seq x is-lazy`, `Date`/
    `DateTime`, `Supply x list`, `Signature x gist`, `Range x hyper/lazy/
    int-bounds/Array`, `Rat x FatRat/nude`, `Map x raku/gist`, `Duration x
    Numeric`, `Pair x Pair`, `Mu x defined`, `CallFrame x defined`,
    `RakuAST::IntLiteral x value`, `Attribute x defined`, `IO::Path::Parts x
    AT-KEY`, `Int x ^name`, 7-28 each). New `failure_chain_reaches_any_and_mu_via_nil`
    (`receiver_class.rs`) and `exception_family_rows_are_backed_by_the_cascade`
    (`native_method_row.rs`) tests. Since this slice changes real
    `dispatch_owner_chain`/`class_chain` answers (not just the coverage-check
    table), ran `make roast` locally, not just `make test`, per the "touched
    name/type resolution" rule -- both green (`cargo test --lib` 740 tests,
    `make test` unchanged file count, `make roast` 1435 files / 218774 tests).
    **Progress 2026-08-10** (eighth slice): closed most of the diffuse tail
    left above -- ~25 owners with no `builtin_type_method_names` entry (same
    situation as `Pair`/`Seq`/`Match`), each hand-probed against a real value
    built via one shared interpreter script (`Version`, `Date`, `DateTime`,
    `Duration`, `Signature`, `Backtrace`/`Backtrace::Frame`, `Range` (13 new
    names), `Rat`, `Map`, `Pair`, `CallFrame`, `List`/`Array` (4 shared
    names), `Attribute`, `IO::Path::Parts`, `Capture`, `Complex`, `Instant`,
    `Uni`, `Block`, `Supply`, `Junction`, `Seq`, plus 3 more `Match` names).
    Two additions were root-cause fixes rather than plain per-owner rows: (1)
    `Any`'s `gist`/`raku`/`hash` cover the bare `Any` type object (confirmed
    the same `ValueView::Package` formatting arm in `dispatch_core_repr.rs`
    renders every type object uniformly, including user classes, so the row
    is not an `Any`-only artifact); (2) `Exception`'s `message`/`gist`/`Str`
    are declared at the shared `cn == "Exception" || cn.starts_with("X::") ||
    cn.starts_with("CX::")` gate in `methods_0arg/mod.rs`, so one
    `Exception`-owner row -- found via the chain-walk, mirroring the
    `Failure`/`Exception` catalog fix from the seventh slice -- covers every
    `X::*`/`CX::*` type without its own more-specific row, verified against
    three previously-unmodeled types (`X::Method::NotFound`,
    `X::Str::Sprintf::Directives::Unsupported`, `X::Str::Numeric`) without
    adding a row for any of them individually. Verifying the `Exception` row
    surfaced a genuine registration gap, not just a coverage-table gap:
    `X::Str::Sprintf::Directives::Unsupported`'s `dispatch_owner_chain` was
    the bare `["X::Str::Sprintf::Directives::Unsupported"]` -- unlike
    `X::Method::NotFound`/`X::Str::Numeric`, it was never registered via
    `runtime_init.rs`'s `register_x` helper, so it had no parent info at all
    and the chain-walk could never reach `Exception` no matter how many rows
    existed. Fixed with one `register_x("X::Str::Sprintf::Directives::
    Unsupported", "Exception")` call (confirmed against real `raku`'s
    `.^mro`: `(Unsupported, Exception, Any, Mu)`). A fresh `t/`-wide sweep
    confirmed `native_call_unmodeled` dropped from 1498 to 593 (cumulative
    **-98.4%** from the original ~37904); the only remaining top-40 entries
    are `ProfiledG x raku` (24, a test-defined grammar under EXPORTHOW custom
    HOW, not a generalizable builtin owner), `RakuAST::IntLiteral x value`
    (9), `Int x ^name` / `CArray[uint8] x elems` (7 each, both deferred: the
    caret-name arm is gated on the value carrying a role mixin, not plain
    `Int`, and a generic `Int` row would over-claim; `CArray` is NativeCall
    plumbing, low leverage), and a long single-digit tail. New
    `eighth_slice_tail_rows_are_backed_by_the_cascade` test
    (`native_method_row.rs`). `cargo test --lib` (741 tests) and `make test`
    (3002 files/28169 tests) both green; since the `register_x` fix changes a
    real `dispatch_owner_chain` answer, ran a targeted local roast sweep
    (`sprintf`/exception/date/range/signature/junction/version/duration/
    instant-related whitelisted files, 83 files) rather than the full suite,
    per the "touched name/type resolution" rule -- all green, full `make
    roast` left to CI.
    **Progress 2026-08-10** (ninth slice): the `todo/deep/adr0019-e2-e4-resolver-core.md`
    design's own risk note says E4b/E3 "may [not] land while `native_call_unmodeled`
    ... is nonzero on the sweep corpus", so this slice closed three more
    coherent clusters left after the eighth slice's partial coverage: the
    full `Date`/`DateTime` accessor surface (`day`/`month`/`minute`/`second`/
    `offset-in-minutes`/`offset-in-hours`/`timezone`/`days-in-year`/
    `formatter`/`day-of-week`/`succ`/`perl`/`daycount`/`dd-mm-yyyy`/
    `mm-dd-yyyy`/`yyyy-mm-dd`/`Date`/`Instant`, 21 rows total), plus
    `Backtrace` (`flat`/`defined`/`concise`/`summary`/`Stringy`),
    `Backtrace::Frame` (`is-setting`/`code`/`Str`), and `Complex`
    (`re`/`im`/`reals`/`conj`/`reverse`/`Complex`), all hand-probed the same
    way. A fresh `t/`-wide sweep confirmed `native_call_unmodeled` dropped
    from 593 to 528 (cumulative **-98.6%** from the original ~37904). New
    `ninth_slice_rows_are_backed_by_the_cascade` test. `cargo test --lib`
    (743 tests) and `make test` (3004 files/28181 tests) both green; this
    slice is pure row-table addition (no `dispatch_owner_chain`/registration
    change), so no local roast run was required per the "touched name/type
    resolution" rule.
    **Assessment**: the remaining 528 is genuinely one-off -- individual
    RakuAST node-accessor getters (one row per AST node class), NativeCall
    `CArray[T]` element-type variants, and ad-hoc test-fixture class names
    that appear as an "owner" only because that specific `t/` file declared
    a class with that name (`Foo`, `TC`, `Wrapper`, `FooDate`, ...) -- not a
    reusable cluster like `Any`/`Exception`/`Date` were. Continuing to chase
    individual 1-2-hit entries here has sharply diminishing returns per the
    effort already spent on three slices' worth of clustering. Before
    E4b starts, re-run the sweep and either (a) accept a small nonzero
    floor with each remaining hit justified inline (mirroring the
    `flaky-tests.txt` precedent for accepted exceptions), or (b) spend one
    more slice specifically on the RakuAST node-accessor family (the
    largest remaining homogeneous cluster) if a session wants to push
    further before flipping the switch.
    **Progress 2026-08-10** (tenth slice): took option (a)'s spirit but as a
    root-cause fix rather than a row addition -- the ninth slice's own
    breakdown showed a 61-hit `X::*` cluster (biggest owner-group left) that
    the eighth slice's `Exception` row should already have covered but did
    not. The cause was not a missing row: `raku -e 'X::ControlFlow.^mro'`
    confirmed `X::ControlFlow, Exception, Any, Mu`, but mutsu's own
    `X::ControlFlow.^mro».^name` reported just `X::ControlFlow, Any, Mu` --
    `Exception` was missing entirely. Twenty-one `X::*` types built all over
    the interpreter (directly via `Value::make_instance`, or via the
    `"X::Type: text"` message convention `split_typed_message_convention`
    parses into a typed instance) were never `register_x`'d in
    `runtime_init.rs`, so their registry MRO dead-ended at themselves with no
    `Exception` continuation -- meaning `$exc ~~ Exception` and
    `$exc.isa(Exception)` were silently `False` for any of them, not just a
    counter artifact. Every name was confirmed against a live `raku -e`
    probe before registering (one, `X::Role::Composition::Conflict`, is a
    mutsu-only name from the message convention with no real rakudo
    counterpart; `X::React::Died` is a role in rakudo but an Instance in
    mutsu already) both still get `Exception` ancestry for mutsu's own
    `CATCH`/`.isa` semantics to be internally consistent. Also added the
    `Exception`-owner rows for `line`/`file`/`backtrace`/`throw`/`resume` --
    declared in the same `cn.starts_with("X::")`-gated match blocks as
    `message`/`gist`/`Str` but not yet in the row table. A fresh sweep
    confirmed the entire `X::*` cluster (61 hits) is gone: `native_call_unmodeled`
    dropped from 528 to 475. New `tenth_slice_exception_registration_rows_are_backed_by_the_cascade`
    test. `cargo test --lib` (745 tests) and `make test` (3007 files/28205
    tests) both green; local roast run on the 45 whitelisted files
    referencing any touched `X::*` name (per the "touched name/type
    resolution" rule, since this changes MRO/registration) also green.
    Remaining 475 is the RakuAST/NativeCall/test-fixture tail the ninth
    slice's assessment already described -- still no dominant cluster left.
    **Progress 2026-08-10** (eleventh slice): closed the `RakuAST::*`
    node-accessor cluster (55 hits, the largest owner-group left after the
    tenth slice). Unlike the exception cluster, every field-accessor call on
    a `RakuAST::*` node dispatches through ONE shared, data-driven site
    (`rakuast::node_accessor`, `methods_0arg/mod.rs`) that reads the node's
    own `fields` list by name -- there is no per-class dispatch bug to fix,
    just 31 missing (owner, field-name) rows across 19 node classes.
    `rakuast::accessor_names` (a separate introspection-only registry
    feeding `.^methods`/`.^attributes`) was considered as a mechanical
    source to generate every row from, but rejected: it is itself
    incomplete for 4 of the needed classes (`QuotedString`,
    `Call::Name::WithoutParentheses`, `Statement::If`, `PointyBlock` all
    have real fields `node_accessor` serves but no `accessor_names` entry),
    so trusting it would have propagated that gap into the row table.
    Instead each row was hand-probed against real nodes built the same two
    ways `t/rakuast-construct-*.t` already does (direct `RakuAST::Foo.new(...)`
    construction for `Parameter`/`ParameterTarget::Var`/`Type::Simple`/
    `StrLiteral`, `Q[...].AST` deparse for the rest) -- catching along the
    way that a plain string literal (`"abc"`) deparses to `QuotedString`,
    not `StrLiteral`, confirmed by direct probe rather than assumed from the
    name. A fresh sweep confirmed the entire `RakuAST::*` cluster is gone:
    `native_call_unmodeled` dropped from 528 to 403 (cumulative **-98.9%**
    from the original ~37904) -- no dominant cluster left in either of the
    two E2b-tracked breakdowns run this session. New
    `eleventh_slice_rakuast_accessor_rows_are_backed_by_the_cascade` test.
    `cargo test --lib` (745 tests) and `make test` (3007 files/28205 tests)
    both green; pure row-table addition (no dispatch/registration change),
    so no local roast run required per the established rule.
    **Progress 2026-08-10** (twelfth slice): fixed a genuine MRO-computation
    bug for parametrized type names (`Array[Int]`, `array[int32]`,
    `CArray[uint8]`) rather than adding more rows. `catalog_chain_for_name`'s
    and `class_mro`/`class_mro_readonly`'s fallback for an uncataloged name
    used to be `[name, Any, Mu]` / a bare `[name]` -- never reaching the
    base type's real ancestry (`array[int32].^mro` in real raku is
    `array[int32], array, Cool, Any, Mu`; mutsu's chain skipped straight to
    `Any`/`Mu` or dead-ended at the name alone). Fixed by stripping the
    `[...]` argument and splicing the base type's own catalog chain when the
    base is a catalog builtin (the existing `Blob[uint32]`-style handling
    only covered a base that was itself a *registered* class). This also
    required two new catalog rows (`array`, `CArray` -- the NativeCall
    typed-array bases; only the boxed `Array` collection type had one) and
    surfaced a SECOND, independent latent bug in
    `class_chain_with_catalog_tail`: its `continues` branch (detecting when
    the registry MRO already carries a catalog-consistent continuation)
    matched the newly-fully-spliced chain and then unconditionally
    `break`-ed without pushing the rest of it, silently truncating a chain
    like `[array[int32], array, Cool, Any, Mu]` back down to
    `[array[int32], array]`. Both fixes are general (not array-specific) --
    the second one benefits every builtin ancestor whose registry MRO
    happens to already continue consistently, not just parametrized names.
    Two new receiver_class.rs tests
    (`parametrized_type_object_chain_is_not_truncated`,
    `typed_native_array_type_object_chain_is_not_truncated`) plus a catalog
    pin (`native_array_bases_match_raku_exactly`). `cargo test --lib` (748
    tests) and `make test` (3010 files/28218 tests) both green; this touches
    core MRO computation used far beyond arrays (registry.rs/receiver_class.rs
    are load-bearing for `.^mro`, method resolution, augment gates, ...), so
    117 whitelisted roast files across `S09-typed-arrays`/`S12-class`/
    `S14-roles`/generics/NativeCall were run locally per the "touched
    name/type resolution" rule -- all green.
    **Gate renegotiation — ADOPTED 2026-08-10** (decided with the user after
    advice-seeking, not a unilateral change): after 12 slices
    `native_call_unmodeled` is down **~99%** (~37904 to ~400) with no
    dominant cluster left; remaining hits are dozens of 1-10-hit one-offs
    (individual RakuAST-adjacent one-offs, NativeCall `CArray[T]` per-owner
    methods, ad-hoc test-fixture class names) that so far have turned out
    NOT to be real dispatch bugs on inspection (unlike the tenth and twelfth
    slices' finds) — chasing them individually has poor ROI relative to the
    effort already spent across twelve slices. The design doc's original
    risk note required this counter to be exactly zero before E4b/E3 could
    land ("neither...may land while `native_call_unmodeled`...is nonzero on
    the sweep corpus"); that precondition is **replaced** by: E4b's resolver
    must fall back to the existing cascade on any row miss AND keep
    incrementing the counter, so an incomplete table degrades to today's
    behavior instead of misdispatching. `native_call_unmodeled` is now a
    monitoring signal (kept low, reviewed periodically, new clusters still
    fixed at the root cause the way the tenth/twelfth slices did), not a
    hard precondition. This is a decision about Phase E's contract, recorded
    here rather than left as a silent exception list. E2b itself is not
    closed by this — it stays open for opportunistic root-cause fixes, just
    no longer blocks E4b.
- [ ] **E3 — Add the generation-keyed resolved-call cache.** Key by receiver TypeId, method symbol,
  call shape, and method generation; cache the ordered candidate sequence, not a second resolver.
  **Design 2026-08-10** (same doc): lands after E4b. Key `(TypeId, Symbol, CallShape)` where
  CallShape packs arity bucket + has-named (named calls get sequence caching for the first
  time); joins `refresh_method_caches_for_generation`'s wholesale clear set; the two probe
  sites that today bypass the generation refresh gain it. `fast_method_cache` survives as the
  monomorphic IC in front until F5 — retiring it inside Phase E would be an unmeasured perf
  cliff. Bench-CI parity evidence is part of this box's exit (G3's dispatch clause).
- [x] **E4 — Resolve native and user candidates in one MRO walk.** Preserve user shadowing,
  visibility, invocant definedness, arity/signature ordering, and native fallback in one result.
  **Design 2026-08-10** (same doc): `resolve_sequence(chain, name, shape, definedness)` returns
  a `ResolvedSequence` — the shape-independent ordered candidate universe (user candidates in
  stored order per level, accessor arbitration, native rows at catalog levels, proto slot);
  ranking/signature selection stays per-call via the existing ladder extracted to consume a
  candidate slice. The six copy-pasted submethod no-inherit rules collapse into the one build
  site. Sibling walkers migrate with their consumers (E7/E8/E9), not here.
  - [x] **E4a — sequence builder + shadow parity (user candidates only)**, counter-verified
    against `resolve_method_with_owner_impl` outcomes.
    **Landed 2026-08-10**: `ResolvedSequence`/`ResolvedCandidate::User` plus
    `Interpreter::resolve_sequence` (`src/runtime/resolution_sequence.rs`) walk an E1
    `TypeId` chain and collect every visible user-declared candidate per level (private
    skip; `is_my` skip when the level is an ancestor) into the flat, shape-independent
    candidate universe decision 4 describes. `resolve_method_with_owner_impl`'s winner-
    picking tie-break ladder (type distance, `is default`, narrowness, explicit-named,
    most-derived-owner, `X::Multi::Ambiguous`) was extracted verbatim into
    `Interpreter::pick_method_winner` (`resolution_method.rs`) — a pure code-motion
    refactor, zero behavior change — so both the real resolver and the shadow builder
    rank with the exact same rules. `Interpreter::shadow_check_resolver`
    (`MUTSU_VM_STATS`-gated) builds the sequence at `resolve_method_cached`'s two
    resolution boundaries (multi-cache-miss and fresh-resolve), filters candidates
    through `method_args_match_for_invocant`, ranks with `pick_method_winner`, and
    compares the winner (owner symbol + `MethodDef.body` `Arc` pointer identity)
    against the real answer under new `resolver_shadow_checks`/`_mismatches` counters.
    Two correctness-critical guards keep this a true zero-behavior-change probe:
    `self.dispatch_ambiguous` is saved/restored around the shadow ranking (it can set
    the flag, which the caller reads immediately after the real resolve); candidates
    with a `where`-clause param are skipped entirely, since `where` is user code whose
    dynamic-variable writes are a deliberately-preserved side effect
    (`restore_env_preserving_dynamics`) that must not run twice. Verified via a
    `MUTSU_VM_STATS=1` sweep over full `t/` (2996 files, 12396 shadow checks) plus the
    whitelisted `roast/{S12,S14,S32}-*` corpus (382 files, 12767 checks): 3 mismatches
    total (0.012%), all in `t/`, all the same explained bucket — a non-multi method
    resolves by name alone in the real resolver even when its signature does not bind
    the call (`method assign-rw($a is rw)` called with a literal; a role-typed param
    called with the wrong type), which the shadow builder does not yet model since it
    only ranks args-matching candidates. This is the E8-deferred early-stopping rule
    documented in `resolution_sequence.rs`'s module doc, not a new finding — E1a set
    the precedent of landing with an explained-mismatch ledger rather than blocking on
    it. `make test` (2996 files/28149 tests) and the full whitelisted roast sweep both
    green.
  - [ ] **E4b — authoritative switch at the cached-resolve boundaries**, native rows included,
    `should_bypass_native_fastpath` deleted. Local `make roast` before PR.
    **Scoping 2026-08-10** (`todo/deep/adr0019-e4b-should-bypass-native-fastpath-decomposition.md`):
    `should_bypass_native_fastpath` has exactly one caller
    (`call_method_with_values`); its ~110-line boolean chain decomposes into
    (1) receiver-shape safety gates that likely reduce to "no native row"
    (`NativeRowFlags::SPECIAL`), (2) NativeCall class-binding checks
    (`is_native_method`, a third candidate kind alongside `ResolvedCandidate::User`
    and the E2 native-row table), and (3) user-method/accessor priority,
    which `resolve_user_method_or_accessor` (already production code at 5
    call sites) appears to already answer correctly in one MRO walk. Land
    each category shadow-verified against a `MUTSU_VM_STATS` counter (E1a/E4a's
    own methodology) before the authoritative switch. Per the gate
    renegotiation above, this box must fall back to the pure
    `native_method_{0,1,2}arg` cascade on any row miss rather than treat a
    miss as "no candidate" — `native_call_unmodeled` continues to fire
    through the fallback path in production, not just in the `MUTSU_VM_STATS`
    shadow probes.
    **Progress 2026-08-11** (step 1, shadow-only): tested the scoping note's
    open question — whether `resolve_user_method_or_accessor` alone already
    subsumes categories 2 and 3 — with a `MUTSU_VM_STATS`-gated shadow probe
    (`Interpreter::shadow_check_bypass_user_method_categories`,
    `methods_native_bypass.rs`) called from `call_method_with_values`
    alongside the real `should_bypass_native_fastpath` decision, comparing a
    faithful re-expression of lines 179-180/214-224 (`is_native_method(..)`
    for an Instance, plus the `has_user_method`/`has_public_accessor`/
    `has_class_level_attr` trio, both gated exactly as the original) against
    `resolve_user_method_or_accessor(class_name, method).is_some()`. **Answer:
    no** — a `t/`-wide sweep (2996 files, 8-way parallel, 36992 checks) found
    4171 mismatches (11.3%), 4169 of which (99.95%) are one shape:
    `real=true shadow=false` on a runtime-hosted builtin class
    (`Supply`/`Supplier`/`IO::Pipe`/`IO::CatHandle`/`Proc`/`Thread`/
    `IO::Handle`/`IO::Path`/`IO::Socket::Async::Listener`/`Encoding::Builtin`)
    whose method is listed in that class's `runtime_init.rs`-seeded
    `ClassDef::native_methods` but has no same-named public accessor —
    `Supply.tap` alone is 3365 of the 4171 (81%). Root cause confirmed by
    reading `resolve_user_method_or_accessor`: it only consults `has_native`
    (`class_def.native_methods.contains(..)`) as a tiebreak *inside* the
    `has_attr` (accessor) branch, so a pure native method with no matching
    accessor is invisible to it — category 2 is genuinely NOT reachable
    through category 3's helper, confirming the scoping note's "third
    candidate kind" prediction rather than refuting it. Pinned by a new test,
    `resolve_user_method_or_accessor_does_not_see_a_pure_native_methods_entry`
    (`methods_native_bypass.rs`). The remaining 2 mismatches are the opposite
    shape (`real=false shadow=true`, `class=R method=new`, both from the same
    single-file run) and were not chased further — negligible fraction, no
    dominant cluster there. Conclusion for the implementation plan: category
    3's cutover (`resolve_user_method_or_accessor` replacing lines 214-224) is
    shadow-verified safe standalone; category 2 (`is_native_method`) needs its
    own explicit candidate kind wired into the resolver — it does not fold
    into `resolve_user_method_or_accessor` for free. `cargo test --lib` (750
    tests) and `make test` both green; shadow-only, zero behavior change.
    **Progress 2026-08-11** (step 2, docs-only): audited category 1's
    receiver-shape gates (lines 130-224) against the E2 native-row catalog
    (`native_method_row_table.rs`) to answer the scoping note's open question
    — does each gate reduce to "the row table has no entry", so a resolver
    consulting rows would naturally never route there? **Answer: mostly no,
    and for a reason the scoping note's framing missed.** The gate
    renegotiation (above) commits E4b's resolver to falling back to the
    *existing cascade* (`native_method_{0,1,2}arg`) on any row miss, not to
    treating a miss as "no candidate" — so "no row" no longer implies "the
    resolver skips it" the way it would have under the original zero-tolerance
    design; it only implies "the resolver still tries the cascade, same as
    today's `should_bypass_native_fastpath == false` path would." Row
    presence/absence is therefore the wrong axis to check category 1 against;
    what actually matters is whether the *cascade itself* would misbehave if
    reached for that owner/method, independent of any row. Two directly
    confirmed examples prove the point rather than merely suggest it: `Supply`
    *has* a row (`("Supply", "list", 1, 0)`) and is still unconditionally
    gated (line 167-168, async Supply state can't be read through a naive
    getter); `Match.elems`/`Match.gist`/`Match.Str`/`Match.chomp` all have
    rows too, yet the lazy-Match branch (lines 130-142) gates `elems`
    unconditionally and `gist`/`Str`/`Stringy` conditionally on
    `exception_render_needs_interpreter`. Per-case disposition (grep against
    `native_method_row_table.rs` plus one direct cascade read):
    - **Confirmed NOT reducible** (real receiver-state hazards, must stay
      explicit resolver guards regardless of row content): `squish`
      (universal — no row anywhere, but `methods_0arg/collection.rs:1132`
      *does* implement it per-view, so a bare row-miss fallback would
      wrongly serve it); `Supply`'s list-vocabulary methods
      (`max`/`min`/.../`zip-latest`) and `list`/`Array`/`Seq`/`elems` (the
      `Supply.list` row proof above); the lazy-Match `elems`/message-lazy
      `throw`/`rethrow`/`gist`/`Str`/`Stringy` gate (rows exist for all of
      these, including via E2b's own tenth/eleventh-slice-adjacent `Any`/`Mu`
      universal rows for `gist`/`Str`); `Hash.keys` with no args (`Hash` *has*
      a `keys` row — the 0-arg call shape specifically needs the
      interpreter's own ordering/freshness semantics the row can't provide).
    - **Likely reduces, not exhaustively proven**: `Supplier`/
      `Supplier::Preserving.Supply` (**confirmed and dropped, step 5 below**),
      `Proc::Async`'s method family, and
      `Stash`'s `AT-KEY`/`keys`/`values` — all zero rows for their owner, and
      a grep for generic (non-owner-specific) cascade arms recognizing this
      vocabulary for an arbitrary `Instance` came up empty, but this was not
      traced as rigorously as the confirmed cases above (narg/1-arg cascades
      for `AT-KEY` in particular were not fully read). Treat as "probably
      already safe to drop once E4b's implementation directly verifies it",
      not as settled.
    - **Mixed, one concrete finding**: `IO::Handle`'s `chomp`/`encoding`/
      `opened`/`DESTROY` gate has zero catalog rows, AND the cascade's own
      `chomp` arm (`dispatch_core_str.rs:216-221`) *already* self-guards —
      `if class_name == "IO::Handle" { return Some(None); }` — making
      `should_bypass_native_fastpath`'s outer gate redundant belt-and-suspenders
      for `chomp` specifically. `encoding`/`opened`/`DESTROY` were not traced
      the same way; keep the whole group gated until each name is checked
      individually.
    - **Not row-related at all, stays a guard by construction**: the caller-
      supplied `skip_pseudo` flag; the Real/Numeric `.Bridge` bridge
      (`does_check` is a role-membership test, not a name lookup); and
      `has_user_method(class_name, "Bridge")` (pure user-code presence, same
      shape as category 3 but scoped to one fixed method name for delegation
      setup rather than the call's own method name).
    Net effect on the implementation plan: E4b's resolver needs an explicit
    receiver-state guard list evaluated *before* falling back to the cascade —
    exactly what design decision 2's classification table already called
    "receiver-state facts become resolver guards" (`todo/deep/adr0019-e2-e4-resolver-core.md`).
    This closes the scoping note's step 2 as answered rather than open: the
    row table cannot decide category 1 for you, full stop, given the adopted
    fallback contract. No code changed; docs-only.
    **Progress 2026-08-11** (step 3): gave category 2 (`is_native_method`) its own
    candidate kind rather than folding it into `resolve_user_method_or_accessor`,
    per step 1's finding that the two are genuinely disjoint. `ResolvedCandidate`
    gains `NativeCallBinding { owner: TypeId }` (`resolution_sequence.rs`); `resolve_sequence`
    detects it per chain, mirroring `is_native_method`'s own two-part check —
    the five hardcoded classes (`IO::Pipe`/`IO::Special`/`IO::Handle`/`Thread`/`VM`,
    extracted into a shared `Interpreter::hardcoded_native_method` so the two
    checks cannot drift apart) only at level 0 (the receiver's own class, exact-name,
    matching `is_native_method`'s non-MRO-aware fast checks), and `ClassDef::native_methods`
    (the `is native(&sym)` registry) walked across the full chain, first hit wins — at
    most one `NativeCallBinding` candidate per sequence, since `is_native_method` itself
    is a boolean "does any level bind this", not a per-level fact.
    `shadow_check_bypass_user_method_categories` (step 1's probe) now ORs this candidate's
    presence into `shadow` for Instance receivers. Verified with a fixed t/-wide sweep
    (each process's `MUTSU_VM_STATS` output to its own file — the original single-shared-file
    sweep script produced torn/interleaved lines under `-P 8`, caught by a sanity check
    when a totals line briefly read `mismatches=8072721`, impossibly larger than
    `checks`): **baseline (pre-step-3) 4172/20635 mismatches (20.2%) → post-step-3
    34/20634 (0.16%)**, a 99.2% reduction, with the same 2996-file corpus shape as step 1's
    sweep. All 34 remaining mismatches carry `native_binding_owner=None` on both sides
    (`grep -l native_binding_owner=Some` over the full per-file log set: zero hits) — none
    are new, and their shape (`WHAT`/`WHO`/`WHY`/`HOW`/`WHICH`/`WHERE`/`DEFINITE` on
    generic test-fixture classes, plus `new`/`shared`/`val`/`name`/`tag`/`label`) matches
    step 1's already-noted "opposite shape, not chased further" residual — unrelated to
    category 2, out of this step's scope. `cargo test --lib` (769 tests) and
    `prove -j4 t/` (3011 files / 28230 tests) both green. Category 2 is now shadow-verified
    safe as a resolver candidate; category 3 was already shadow-verified in step 1. What
    remains before the authoritative switch: category 1's guard list (step 2's per-case
    disposition) still needs to be implemented as explicit resolver guards, and the
    switch itself needs the E2 native-row catalog wired in per design decision 4's
    `Native` variant (not yet added — `NativeCallBinding` is a distinct, fourth kind).
    **Progress 2026-08-11** (step 5, category 1): confirmed the first of step 2's
    "likely reduces, not exhaustively proven" category-1 guards and removed it
    live, ahead of the authoritative switch — it does not need to wait, since
    the finding is "the cascade itself never needed this guard," independent
    of the resolver work. The `Supplier`/`Supplier::Preserving` `.Supply` guard
    (`methods_native_bypass.rs`) is provably redundant: the coercion cascade's
    own `"Supply"` arm (`methods_0arg/coercion.rs:655-661`) already returns
    `None` for both classes — "Supplier.Supply has runtime behavior (live
    stream), not generic coercion" — before the guard's `should_bypass_native_fastpath`
    check is even consulted, and no other 0-arg cascade arm matches the name
    `"Supply"` (`git grep '"Supply"'` under `builtins/methods_0arg/` has exactly
    one match arm). Removing the guard changes nothing observable: with it
    gone, `call_method_with_values` calls the cascade instead of skipping it,
    the cascade returns `None` either way, and control falls through to the
    runtime native method identically. Verified empirically, not just by
    reading: with the guard commented out, `cargo test --lib` (769 tests),
    `prove -j4 t/` (3011 files / 28230 tests), and the full `S17-supply` roast
    subset (99 whitelisted files, via `scripts/run-roast-test.sh`) all stayed
    green — including every `Supplier`/`Supplier::Preserving` test
    (`t/supplier-preserving-backlog.t`, `t/supplier-preserving-done-replay.t`,
    `t/promise-supply-coercion-async-drive.t`, `roast/S17-supply/supplier-preserving.t`).
    Landed as a genuine deletion (not a comment-out), matching the same
    self-guarding pattern step 2 already found for `IO::Handle.chomp`
    (`dispatch_core_str.rs:216-221`) — this is the second confirmed instance of
    that shape, reinforcing that "does the cascade itself already self-guard"
    is a real, repeatable reduction axis for category 1, distinct from (and
    not blocked by) the row-table question step 2 closed. The other two
    "likely reduces" groups (`Proc::Async`'s method family, `Stash`'s
    `AT-KEY`/`keys`/`values`) and the "mixed" `IO::Handle` `encoding`/`opened`/
    `DESTROY` group remain open — same audit methodology applies.
    **Progress 2026-08-11** (step 6, category 1): audited `Proc::Async`'s method
    family (17 names) the same way, and found it splits rather than reduces
    wholesale. Sixteen names (`start`/`kill`/`write`/`close-stdin`/
    `bind-stdin`/`bind-stdout`/`bind-stderr`/`ready`/`print`/`put`/`say`/
    `command`/`started`/`w`/`pid`/`stdout`/`stderr`) have no matching arm
    anywhere in the native fast-path cascade (`native_method_{0,1,2}arg` —
    exhaustive per-name grep across `builtins/methods_0arg/` and
    `builtins/methods_narg.rs`), so gating them was redundant belt-and-suspenders
    identical to step 5's `Supplier.Supply` finding. `Supply` is the exception
    and does NOT reduce: the coercion cascade's generic `"Supply"` arm
    (`methods_0arg/coercion.rs:655-701`) special-cases `Supplier`/
    `Supplier::Preserving` (returns `None`, the step-5 finding) and `Supply`
    itself (no-op passthrough) but has no such case for `Proc::Async` — an
    Instance falls through to its catch-all `_ => vec![target.clone()]`,
    which would wrap the live `Proc::Async` object itself as the sole element
    of a bogus values-Supply instead of reaching the runtime's live-stream
    `.Supply`. Landed as a genuine reduction of the guard's method list down to
    `method == "Supply"` alone (not a deletion of the whole arm). Verified
    empirically: `cargo test --lib` (769 tests), the full local Proc::Async
    suite (`t/composite-promise-replays-proc-taps.t`,
    `t/concurrency-threading.t`, `t/io-socket-async-real-connect.t`,
    `t/multi-no-match-builtins.t`, `t/native-proc-async-ctor.t`,
    `t/proc-async.t`, `t/proc-start-cwd-env.t`,
    `t/shared-var-nil-redeclared-mask.t`, 78 tests), and the full
    `roast/S17-procasync/` subset (10 files, 155 tests) all green.
    `Stash`'s `AT-KEY`/`keys`/`values` and the mixed `IO::Handle`
    `encoding`/`opened`/`DESTROY` group remain open.
    **Progress 2026-08-11** (step 7, category 1): closed the `IO::Handle`
    "mixed" group and split `Stash`'s group. `IO::Handle`'s `encoding`/
    `opened`/`DESTROY` have no matching arm anywhere in the native fast-path
    cascade (exhaustive grep, same as `chomp`'s already-confirmed
    self-guarding arm), so the entire four-name guard is redundant and was
    deleted outright — the fourth confirmed instance of the "cascade already
    self-guards or never matches" reduction axis. `Stash` splits instead of
    reducing wholesale: `AT-KEY` has no cascade arm at all and was dropped,
    but `keys`/`values` do have arms (`methods_0arg/collection.rs`) whose
    generic catch-all (`Value::seq(positional_keys(&value_to_list(target)))`
    for `keys`, `Value::seq(value_to_list(target))` for `values`) would wrap
    an Instance receiver as if it were a one-element list instead of reading
    the Stash's own hash — the same shape as the already-known `Hash.keys`
    0-arg guard, so those two names stay gated. Verified empirically: `cargo
    test --lib` (769 tests), the full local `Stash`/`IO::Handle` test set (24
    files, 261 tests: `t/who-stash.t`, `t/stash-exists-key.t`,
    `t/stash-values.t`, `t/destroy.t`, `t/io-cathandle*.t`,
    `t/io-handle-*.t`, plus adjacent Stash/destroy coverage), and the
    `roast/S32-io/{tell,io-path,lock,open,io-handle,slurp,io-cathandle,spurt}.t`
    subset (8 files, 239 tests) all green. All four "likely reduces"/"mixed"
    category-1 groups from step 2's audit are now resolved.
    **Progress 2026-08-11** (step 8, category 1): extracted every confirmed
    category-1 check (steps 2/5/6/7's findings, plus the pre-existing `elems`/
    exception-render/Real-Numeric-bridge/`Hash.keys` guards) into one
    dedicated function, `native_fastpath_receiver_state_guard`
    (`methods_native_bypass.rs`) — a pure reorganization, zero behavior
    change, directly implementing design decision 3's "receiver-state facts
    become resolver guards" bucket as the single list E4b's eventual
    authoritative switch will consult, mirroring how categories 2
    (`is_native_method`) and 3 (`resolve_user_method_or_accessor`) already
    have their own dedicated functions well before their resolver-candidate
    wiring landed. Categories 2/3 (`is_native_method`, `has_user_method`/
    `has_public_accessor`/`has_class_level_attr`/`mixin_role_has_method`) are
    deliberately NOT folded in — they stay separate per the three-way split.
    The lazy-Match branch keeps its own small inline subset rather than
    calling the new function, since it must avoid `target.view()` (would
    materialize the lazy value) and only the `squish`/`elems`/exception-render
    checks can ever apply to a Match receiver anyway. Verified empirically as
    a genuine no-op: `cargo test --lib` (769 tests), the full local `prove -j4
    t/` suite (3011 files / 28230 tests), and a full `make roast` run (1435
    files / 218774 tests, `Result: PASS`, zero new failures) all green.
    **Progress 2026-08-11** (step 9, design decision 4's `Native` candidate):
    landed the row-catalog candidate kind the step-4 scoping note
    (`todo/deep/adr0019-e4b-should-bypass-native-fastpath-decomposition.md`)
    flagged as needing a dedicated slice, in a smaller shape than that note
    anticipated. `NativeCallShape { arity, definite }` (`resolution_sequence.rs`)
    is the E4b-local subset of the design doc's future E3 `CallShape` the note
    called for — just the two facts a row needs, not the full future cache-key
    shape — threaded through `resolve_sequence`'s signature to both of its
    production callers (`shadow_check_resolver`: arity from `arg_values.len()`,
    definedness via a new `value_is_definite` helper; `shadow_check_bypass_user_method_categories`:
    now takes `arg_count` from its caller). `ResolvedCandidate::Native { owner }`
    is populated by a new production predicate, `native_row_servable`
    (`native_method_row.rs`): a row is reachable for a call iff its arity mask
    contains the call's arity, it is not `SPECIAL`/`MUTATES_RECEIVER` (both
    bypass the pure cascade), and an indefinite receiver additionally needs
    `TYPE_OBJECT_OK` — retried through `canonical_builtin_owner`'s fold
    (`Buf`/`Blob`/...) the same way `record_native_row_coverage` already does.
    This turned out to make the note's finding 3 (a new row-*existence*
    predicate distinguishing "absent" from "genuinely SPECIAL") unnecessary:
    both cases correctly answer "not servable" for `native_row_servable`'s
    purpose, so no new absent-vs-classified distinction was needed after all
    — only `TYPE_OBJECT_OK`/`MUTATES_RECEIVER`/`NativeRowFlags::contains` needed
    un-gating from `#[cfg(test)]`, not the larger `NativeMethodRow` struct.
    Shadow-verified with a genuinely new technique rather than the usual
    t/-wide sweep: a new `shadow_check_native_row_candidate` compares the
    `Native` candidate's presence against `native_result.is_some()` —
    the real, already-computed arity-cascade result `call_method_with_values`
    obtains right after (only called when `!bypass_native_fastpath`, i.e. the
    cascade was actually consulted) — instead of re-invoking the cascade as a
    probe, so there is no double-invocation side-effect risk even for a
    mutating row. New `MUTSU_VM_STATS` counters `native_row_shadow_checks`/
    `_mismatches` (`vm_stats.rs`). Verified: `cargo test --lib` (785 tests,
    including new `native_row_servable` and `resolve_sequence` Native-row
    unit tests), `cargo clippy -- -D warnings` clean, the full local `prove -j4
    t/` suite (3011 files / 28230 tests, no `MUTSU_VM_STATS`) green, a targeted
    `MUTSU_VM_STATS=1` spot-check (single files and `-e` snippets exercising
    class methods, string/array native methods) showing
    `native_row_shadow_checks>0 native_row_shadow_mismatches=0`, and a roast
    smoke subset (`S02-types/`, `S12-methods/`, `S32-str/`, 151 files / 38791
    tests via the proper `scripts/run-roast-test.sh` runner) with no failures
    outside the pre-existing non-whitelisted `S02-types/quanthash.t`. Shadow
    only: nothing reads `Native` to make a dispatch decision yet. What remains
    before the authoritative switch: consuming `User`/`NativeCallBinding`/
    `Native` together to make the actual bypass/dispatch decision at
    `call_method_with_values`'s one call site, replacing
    `should_bypass_native_fastpath` outright — not yet attempted.
    **Progress 2026-08-11** (step 10, scoping finding, docs + shadow-widening
    only): before attempting the authoritative switch, checked whether
    `resolve_sequence`'s presence-only `NativeCallBinding` walk — which does
    not distinguish receiver kind — safely generalizes to a `Package`
    (type-object) receiver, since `should_bypass_native_fastpath`'s real
    category-2 term (`is_native_method`) is deliberately checked *only* for
    `is_instance` (`methods_native_bypass.rs` line ~287; the `!is_instance`
    branch never calls it). `shadow_check_bypass_user_method_categories`
    already built `native_binding_owner` behind an `if is_instance` guard
    (step 3) — widening it to run unconditionally (still shadow-only,
    `record_bypass_shadow_check` unchanged) let the existing
    `bypass_shadow_checks`/`_mismatches` counters answer the question with
    real data instead of a guess. **Answer: no, it does not generalize** — a
    fixed per-process-file `MUTSU_VM_STATS` sweep (3011 `t/` files, `-P 8`,
    same torn-output fix step 3 used) went from the already-landed baseline
    34/20634 mismatches to **177/20634**, a real +143 increase, all
    `real=false shadow=true` (the widened check now sees a `NativeCallBinding`
    candidate for a call the real decision says is NOT bypassed). Root cause,
    confirmed by reading both sides: `ClassDef::native_methods` for
    `"Supply"` (`runtime_init.rs`) conflates two unrelated vocabularies under
    one flag — genuine instance methods (`emit`/`tap`/`act`/...) AND
    class-level factory-method names (`interval`/`delayed`/`merge`/`Channel`/
    `Promise`/`collate`/`categorize`/..., called on the bare `Supply` type
    object, e.g. `Supply.interval(1)`) — and `is_native_method` cannot tell
    them apart from the flag alone. The factory names are answered by a
    completely different mechanism, a hardcoded class-method special case in
    `methods_instance_ops.rs` (`Supply.interval`/`Compiler.id`, alongside the
    pre-existing `Instant.from_posix`) that never reaches
    `call_method_with_values`/`should_bypass_native_fastpath` for these names
    at all — so `is_native_method`'s true answer is simply irrelevant to the
    real decision at a `Package` receiver, and category 2's `is_instance`
    restriction was already the correct, deliberate fix for exactly this
    conflation, not an oversight. 88% of the +143 (126) is `Supply.interval`
    alone (one hot loop in the sweep corpus calling it repeatedly); the tail
    is `Thread.is-initial-thread` (2, hardcoded-table, not registry, same
    receiver-kind issue), `Supply.{schedule-on,Promise,collate,categorize}`
    (4, same conflated-registry cause), `Encoding::Registry.find` (9), and
    `Compiler.id` (2, the same special-cased-elsewhere shape as
    `Supply.interval`). **Consequence for the authoritative switch:** a
    `NativeCallBinding` candidate must stay gated by `is_instance` /
    `NativeCallShape::definite` wherever it drives a real decision — mirroring
    how `Native` (step 9) already gates on `definite`/`TYPE_OBJECT_OK` for the
    exact same reason (a name meaning different things at different
    definedness). `resolve_sequence` itself was NOT changed (still
    unconditionally emits `NativeCallBinding` regardless of definedness,
    since it is a shape-independent candidate universe by design — decision 4
    says nothing should be filtered out at build time); the gating belongs at
    the *consumer*, same as `Native`'s own filtering already lives in
    `native_row_servable` rather than in candidate construction. Left as a
    documented, empirically-grounded ledger entry (matching E1a's
    accepted-mismatch precedent) rather than a code change beyond the
    shadow-check widening itself, which stays landed since it is a strictly
    more informative probe than the `is_instance`-gated version it replaces.
    Verified: `cargo build`, `cargo test --lib`, `cargo clippy -- -D
    warnings`, `cargo fmt --check` all clean; the widening is shadow-only
    (`MUTSU_VM_STATS`-gated), zero real-dispatch behavior change.
    **Progress 2026-08-11** (step 11, scoping finding, docs + two pinning
    tests only): before attempting the authoritative switch, checked whether
    the two remaining hand-rolled arms of `should_bypass_native_fastpath` —
    `has_class_level_attr(..) && !has_public_accessor(..)` (both the Instance
    and Package branches) and the Package branch's `has_user_method(..)`
    without an accessor check — can be retired in favor of
    `resolve_user_method_or_accessor` (category 3) the way the Instance
    branch's equivalent arms already were (step 1). **Answer: no, for two
    independent reasons**, both confirmed by direct unit tests rather than a
    sweep (the feature is rare enough — 2 files total in `t/`+`roast/`,
    `t/class-level-attrs.t` and `roast/S12-attributes/class.t` — that a
    `t/`-wide `MUTSU_VM_STATS` sweep would not reliably surface either gap by
    volume, unlike steps 1/3/10's high-frequency mismatch clusters):
    1. `resolve_user_method_or_accessor_does_not_see_a_class_level_attr`
       (`methods_native_bypass.rs`): a class-level attribute (`our $.x` /
       `my $.x`, `ClassDef::class_level_attrs`) gets no autogenerated accessor
       method at all — `registration_class_body_attr.rs`'s handler inserts
       directly into `class_level_attrs` and returns `ClassBodyFlow::SkipTail`,
       skipping the normal per-instance accessor registration path entirely.
       `resolve_user_method_or_accessor` only ever consults
       `accessor_is_public` (the per-instance attribute table) and
       `user_method_overloads`/role methods — neither sees `class_level_attrs`
       — so it answers `None` where the real check answers `true`.
       `resolve_sequence` doesn't see it either (`User` candidates come from
       `user_method_overloads`, not `class_level_attrs`, and the sequence has
       no accessor notion of any kind). This is a genuine fourth candidate
       kind, disjoint from categories 1-3 and from design decision 4's
       User/NativeCallBinding/Native trio, with exactly one existing consumer
       (`should_bypass_native_fastpath`'s own two arms) — it must stay an
       explicit check indefinitely, not a target for folding.
    2. `resolve_user_method_or_accessor_would_wrongly_answer_for_a_package_receiver`
       (same file): the real Package branch is deliberately narrower than the
       Instance branch — it checks `has_user_method`/`has_class_level_attr`
       but never `has_public_accessor`, because an instance attribute's
       accessor is meaningless called on the bare type object
       (`Foo.x` when only `has $.x` exists is not the same call as
       `Foo.new.x`). `resolve_user_method_or_accessor("Foo", "x")` answers
       `Some(Accessor)` for a class with a plain public `has $.x` and no
       user method or class-level attr — folding it into the Package branch
       as a direct substitute (the way it already IS the Instance branch's
       category-3 answer) would be a real, silent behavior change: `Foo.x`
       would start bypassing the native cascade for the wrong reason. No
       roast/`t/` test currently exercises this exact shape (accessor-only
       class, called via type object, relying on native-cascade fallthrough)
       closely enough to have been caught by steps 1/3/10's t/-wide sweeps —
       absence of a sweep mismatch here is corpus thinness, not a "verified
       safe" signal, unlike the high-volume categories those steps closed.
    **Consequence for the authoritative switch:** category 3's Package
    coverage cannot be expressed as "call `resolve_user_method_or_accessor`
    the same way the Instance branch does" — the Package branch needs its own
    narrower check (`has_user_method` alone, no accessor), preserved as-is,
    not derived from the Instance-branch helper. Combined with the
    class-level-attr arm (both branches) staying an explicit, non-foldable
    guard, the eventual switch's shape is: category 1's guard function, then
    per-receiver-kind branches that each consult (a) `NativeCallBinding` from
    `resolve_sequence` — Instance-only, per step 10 — (b) a receiver-kind-
    specific method/accessor check (the existing `resolve_user_method_or_accessor`
    call for Instance; a bare `has_user_method` check for Package), and (c)
    the class-level-attr arm, unconditionally on both branches. This is
    materially different from "consuming `User`/`NativeCallBinding`/`Native`
    together" as a single unified `resolve_sequence`-only decision — that
    phrasing undersold how much receiver-kind-specific and accessor-adjacent
    logic must survive the cutover as explicit code, not resolver candidates.
    No dispatch code changed; two new tests pin both gaps.
    Verified: `cargo build`, `cargo test --lib` (779 tests), `cargo clippy
    -- -D warnings`, `cargo fmt --check` all clean.
    **Progress 2026-08-11** (step 12, category 3 landed live): implemented
    the Instance-branch half of step 11's "eventual switch" shape —
    `should_bypass_native_fastpath`'s two separate
    `has_user_method(class_name, method) || has_public_accessor(class_name,
    method)` calls collapse into one `resolve_user_method_or_accessor(class_name,
    method).is_some()` call, exactly the substitution step 1 already
    shadow-verified safe (2/36992 unrelated mismatches). The function was
    restructured around a `match target.view()` with one arm per receiver
    kind rather than the original flat OR-chain of `matches!` guards, so the
    Instance and Package branches are now visibly separate blocks instead of
    interleaved arms — making step 11's finding ("Package needs its own
    narrower check, not a derivation of the Instance branch's helper")
    structural rather than a comment to remember. `is_native_method`
    (category 2) deliberately stays a direct call rather than routing
    through `resolve_sequence`'s `NativeCallBinding` candidate: both compute
    the identical MRO walk (`resolve_sequence`'s own detection mirrors
    `is_native_method`, per its doc comment), so building a full sequence
    here would add cost — one MRO walk plus wasted `User`/`Native` candidate
    construction — for zero correctness gain over the existing single-fact
    call; `NativeCallBinding` earns its keep at a future multi-candidate
    consumer (E5-E7), not this one-boolean check. The class-level-attr arm
    (category 4) and the Package branch's bare `has_user_method` stay
    explicit, unchanged, per step 11. Verified: `cargo build`, `cargo test
    --lib` (779 tests, including the three category-1-decomposition/step-11
    pinning tests), `cargo clippy -- -D warnings`, `cargo fmt --check`, the
    full local `prove -j4 t/` suite (3012 files / 28237 tests) all green;
    a release-build roast smoke subset (`S12-attributes/`, `S17-supply/`,
    `S17-procasync/`, `S32-io/`, 127 files / 3395 tests) has exactly one
    failure, `S12-attributes/trusts.t`, confirmed pre-existing and unrelated
    (reproduces identically against the unmodified `main` binary — a
    `B trusts A` unresolved-attribute-visibility bug, not in
    `roast-whitelist.txt`, out of scope here). **What remains before E4b can
    be marked done:** this is one slice of the switch, not the whole box —
    the Instance branch's `NativeCallBinding`-from-`resolve_sequence`
    question above is now closed (deliberately not adopted, with a reason),
    but nothing yet reads `resolve_sequence`'s `Native` candidate (design
    decision 4's row-catalog kind, landed shadow-only in step 9) to replace
    the native-cascade dispatch decision itself — `should_bypass_native_fastpath`
    still gates a separate `native_method_{0,1,2}arg` call at its one call
    site rather than the resolver deciding which candidate wins outright.
    Whether that final consumption is worth doing given `Native`'s
    candidate-vs-direct-cascade-call tradeoff mirrors the same "no gain over
    a direct call" question step 12 answered for `NativeCallBinding` — the
    next session should check that before assuming there is more E4b
    plumbing work left to do, rather than treating the ADR bullet's original
    "should_bypass_native_fastpath deleted" phrasing as still literally the
    target shape.
    **Scope renegotiation (step 13, 2026-08-11, docs-only):** checked the
    step-12 open question directly against E5/E6/E7's own design doc
    (`todo/deep/adr0019-e5-e7-entry-routing.md`) rather than leaving it for a
    future session. **Answer: `Native` is not for `should_bypass_native_fastpath`
    at all — it is for E5/E6/E7's VM opcode entries**, which is a *different*,
    currently-separate dispatch mechanism (`CallMethod`/`CallMethodMut`/etc.,
    `vm_call_method_ops.rs` and friends) with its own hand-ordered probe
    cascades (`skip_native`, `has_user_method` gates, `try_native_method`),
    not `call_method_with_values`'s slow path. The E5 design's decision
    API (`resolve_dispatch(&receiver, method_sym, shape)`, design decision 1)
    is exactly where a `Native` candidate answers "which probe wins" for
    those entries — `should_bypass_native_fastpath` already gets the
    equivalent answer today via a direct, cheaper `native_method_{0,1,2}arg`
    call that self-guards by returning `None` on no match, so gating it with
    a `Native` candidate lookup first would only add the cost of building a
    sequence to predict an answer the subsequent direct call computes anyway
    (step 9 already proved `native_row_shadow_mismatches=0`, i.e. `Native`'s
    presence and the cascade's `is_some()` always agree — a `Native` gate
    would never change which branch is taken here, only add work).
    **Conclusion: E4b's own call site (`call_method_with_values`'s
    `should_bypass_native_fastpath`) has no further profitable consolidation
    against the resolver** — categories 1/2/3/4 are each at their locally
    optimal shape (guard function / direct call / resolver call / explicit
    check, per steps 8/12/12/11 respectively), and the ADR bullet's literal
    "should_bypass_native_fastpath deleted" text describes E5-E7's *separate*
    dispatch mechanism reaching the same resolver-based decisions at its own
    entries, not a deletion of this specific function. Treat E4b as
    **functionally complete** for its own call site; the bullet is kept open
    (not checked off) only because E5-E7 have not yet landed to confirm the
    resolver fully replaces the VM-opcode-side probe cascades this function
    was modeling groundwork for. No code changed this step. Next concrete
    work in the Phase E sequence is E5's mandated measurement slice (design
    decision 3 in the E5-E7 doc): `MUTSU_VM_STATS`-gated per-entry,
    per-outcome counters plus an interceptor taxonomy table, starting with
    `CallMethod` (the highest-traffic opcode entry) — not another
    `should_bypass_native_fastpath` slice.
  **Progress 2026-08-12 (closing E4, checkbox correction):** re-checked step 13's own stated
  precondition for checking off this box — "E5-E7 have not yet landed to confirm the resolver
  fully replaces the VM-opcode-side probe cascades" — against current state: E5 (steps 1-4, E5b,
  E5c parts 1-2, E5d), E6 (E6a-E6d), and E7 (all eight sub-slices) have since all landed and
  closed (E6/E7 already carry `[x]`; E5's own closing note below was a checkbox oversight, also
  corrected here). Their collective, empirically-verified answer to the precondition is "no, by
  design" — `Native`/`NativeCallBinding` are measurement/hint-only at every E5/E6/E7 entry
  (E5b steps 1-2's finding, generalized in E5b step 2's own text: "this generalizes past
  `CallMethod`... at every E5/E6/E7 entry, not a routing decision"), because the real safety net
  for native dispatch lives inside `try_native_method_raw`'s ~22 scattered per-shape `return None`
  checks, not in a single candidate-presence fact a resolver could safely gate on without
  reimplementing all of them. This is a real, confirmed answer to the open question, not a stall —
  E4's own box text ("resolve native and user candidates in one MRO walk... in one result") is
  satisfied by `ResolvedSequence`/`ResolvedCandidate` (`User`/`NativeCallBinding`/`Native`,
  `resolution_sequence.rs`) existing and being shadow-verified at its call sites; the box does not
  additionally require the VM opcode entries to *dispatch on* the `Native` candidate, since E5-E7
  independently and deliberately decided against that for the reason above. No code changed this
  step (docs-only bookkeeping); **E4 is marked done.**
- [x] **E5 — Route ordinary VM method calls through the resolver.** Cover zero/n-arg and named-call
  opcodes while retaining mutation/writeback semantics at the caller boundary.
  **Design 2026-08-10** (`todo/deep/adr0019-e5-e7-entry-routing.md`): the cutover shape is
  "resolver decides, existing arms execute" — each entry's dispatch-probe section becomes a
  match on the resolver decision while receiver normalization, method-identity intercepts, and
  writeback tails stay put. A measurement slice (per-entry per-outcome
  `MUTSU_VM_STATS` counters + an interceptor-taxonomy table per entry) precedes and orders the
  cutovers, C6d-style. JIT shims are asserted tail-identical, not rewritten. The interpreter
  slow paths shrink by attrition (one probe section at a time), never by a one-PR rewrite.
  **Progress 2026-08-11** (E5 step 1, measurement slice for `CallMethod`): the design
  doc's decision-3 counters landed — generic `dispatch_entry_outcome_by_key` /
  `dispatch_entry_intercept_by_arm` histograms in `vm_stats.rs` (keyed
  `"<entry>:<outcome>"` / `"<entry>:<arm>"` so later E5/E6 entries reuse them), and
  `exec_call_method_op_impl` instrumented at every completion point: 45 intercept arm
  names, plus `native`/`user`/`accessor`/`notfound` outcome records at the probe
  section. Pure insertions (0 deletions), zero behavior change; per-file cross-check
  `sum(disjoint outcomes) == opcode-histogram CallMethod` holds (`notfound` is a
  documented overlay subset of `user`). The taxonomy table (decision 2's classes a-d,
  one row per arm, with the uninstrumentable gaps noted) and the sweep results live in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"Measurement slice results — CallMethod".
  Headline sweep numbers (full `t/` + roast S12/S14 subset, 3075 files): disjoint total
  26924 — `user=13258` (49.2%), `native=11794` (43.8%), `intercept=968` (3.6%),
  `accessor=904` (3.4%), overlay `notfound=52`; top intercept arm `nil-absorb=675`;
  18 of 45 arms scored zero, most explained by the same receiver shapes compiling to
  `CallMethodMut` (bareword/variable receivers) — NOT dead code until the E6a mut-twin
  sweep says so. Consequence for slicing: E5b's decision match must nail the user- and
  native-candidate paths first; the intercept gauntlet is an order of magnitude
  smaller. This is ONE slice — the box stays open: the remaining E5 measurement
  entries (`CallMethodDynamic`, hyper non-mut paths, `call_method_all_with_fallback`)
  and all cutover sub-slices (E5b/E5c/E5d) are still to do.
  **Progress 2026-08-11** (E5 step 2, measurement slice for `CallMethodDynamic`):
  instrumented `exec_call_method_dynamic_op`
  (`src/vm/vm_call_method_mut_ops.rs:30-345`) with the same two step-1 counter
  functions, `entry = "callmethoddynamic"` — no new counter functions added.
  15 intercept arm names (`modifier-plus`/`modifier-star`, `call-sub-value`
  for `$obj.$coderef(...)`, `return`, `hyper-race-config`, 9 HyperSeq/RaceSeq
  delegate arms), plus `native`/`user` at the plain probe and `notfound` as the
  same documented overlay-of-`user` pattern step 1 used. No `accessor` outcome
  exists at this entry (no fast 0-arg accessor probe here). Pure insertions (70
  insertions / 1 rewrap of an unchanged single-statement match arm, 0 behavior
  change). Re-checked the design doc's inventory-correction item 3 ("no native
  probe and no compiled-method probe gap") against current code: that item is
  actually about the *Mut* twin (`exec_call_method_dynamic_mut_op`,
  `CallMethodDynamicMut`, an E6 entry) — `exec_call_method_dynamic_op` itself
  has both probes, so the design doc's framing for this entry was correct,
  nothing stale. Verification used 5 targeted `t/` files (of 161 candidates
  matching dynamic-call syntax or filename) rather than a full sweep — this
  entry is far lower-traffic than `CallMethod`, so a handful of files sufficed
  for a clean disjoint-and-complete proof: `array-value-path-mutation.t`
  (`user=8` of 8), `buf-write-native.t` (`native=5` of 5),
  `dynamic-method-type-object.t` (`native=3`+`user=1` of 4, overlay
  `notfound=1`), `format-class.t` (`user=11` of 11), `topic-quoted-method-call.t`
  (`native=1` of 1) — all five `sum(disjoint outcomes) == CallMethodDynamic`
  opcode-histogram count, 0 mismatches. No intercept-arm traffic was observed in
  this targeted set; a full `t/` + whitelisted-roast sweep (CI-scale, as step 1
  ran) is deferred as out of proportion for this smaller entry. Full taxonomy
  table and verification detail in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"Measurement slice results —
  CallMethodDynamic (E5 step 2)". Still to do: the hyper non-mut paths and
  `call_method_all_with_fallback` measurement slices, and all cutover
  sub-slices (E5b/E5c/E5d).
  **Progress 2026-08-11** (E5 step 3, measurement slice for the hyper
  non-mut paths): instrumented `exec_hyper_method_call_op`
  (`HyperMethodCall`, entry `hypermethodcall`) and
  `exec_hyper_method_call_dynamic_op` (`HyperMethodCallDynamic`, entry
  `hypermethodcalldynamic`), both in `src/vm/vm_hyper_method_ops.rs`, with
  the same step-1 counter functions — no new counter functions. Pure
  insertions, zero behavior change; `make test` (3018 files, 28265
  subtests) unchanged. Unlike `CallMethod`/`CallMethodDynamic`, a hyper
  opcode loops over every target element and dispatches once per element
  (design decision 4's "per-element probe"), so the verification identity
  here is element-level plausibility, not `sum(outcomes) ==
  opcode-histogram count` — confirmed directly that outcome sums exceed
  opcode counts on multi-element targets (`t/hyper-nested-itemize.t`: 12
  `HyperMethodCall` opcodes, 18 recorded outcomes). Full `t/` sweep (3018
  files, 50 hyper-active): `hypermethodcall` disjoint element dispatches
  `native=575`/`user=191`/`intercept=99` (native/user dominate ~75%/25%,
  same ordering conclusion as step 1 — E5c's plain-probe conversion is the
  highest-value single change); `hypermethodcalldynamic` recorded only
  `intercept=65` (mostly `callable-descend`/`callable-nodal`, i.e.
  `>>.&sub`) and **zero** `native`/`user` locally — real bug-adjacent
  finding, not dead code: `t/` never exercises `».method`/`».$name(...)`
  the string-dispatch branch, but three whitelisted roast files do, and
  running two directly confirmed real traffic (`roast/S03-metaops/hyper.t`
  `native=8`, `roast/S12-methods/parallel-dispatch.t` `user=12`, both still
  all-`ok`). Also re-confirmed inventory correction 4 from the design
  doc's "Facts that shape the cutover": `exec_hyper_method_call_dynamic_op`
  genuinely has no `skip_native`/`has_user_method` gate anywhere, unlike
  its static twin — V1 (raku-verify this gap) is still open, to be closed
  by the E6/E5c cutover per the doc. Full taxonomy tables (both entries,
  classes a-d) and the sweep detail are in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"Measurement slice results —
  hyper non-mut paths (E5 step 3)". Still to do: the
  `call_method_all_with_fallback` measurement slice (the last of the four),
  then E5b/E5c/E5d cutovers.
  **Progress 2026-08-11** (E5 step 4, measurement slice for
  `call_method_all_with_fallback`): instrumented the last of the four E5
  measurement entries, `vm_call_helpers.rs::call_method_all_with_fallback`
  (entry `callmethodallfallback`) — a shared helper (not an opcode handler)
  with a trivial 2-outcome body (`native`/`user`), called from 6 sites
  across 5 files: `CallMethod`'s own `.+`/`.*` modifier arms (already
  measured at the caller in step 1), `CallMethodMut` and
  `CallMethodDynamicMut` (2 sites each — E6 territory, not yet measured
  independently), and three sites unrelated to the `.+`/`.*` modifiers
  (`.cache`/`.Map` coercions, a cached scalar-accessor probe). Pure
  insertion, zero behavior change; `make test` (3018 files, 28265 subtests)
  unchanged. Full `t/` sweep: 7 files hit, `user=22`/`native=3`, all
  confirmed by inspection to be `.+`/`.*` MRO-walk tests on *variable*
  receivers (so routed through the Mut opcodes, not `CallMethod` itself) —
  sample too small to draw a sub-slice-ordering conclusion on its own;
  clearer once E6a's `CallMethodMut` sweep runs. **All four E5 measurement
  sub-slices are done** (steps 1-4). Full detail in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"Measurement slice results —
  call_method_all_with_fallback (E5 step 4)". Next: E5b, the `CallMethod`
  probe-section cutover to the E4 resolver decision.
  **Progress 2026-08-11** (E5b step 1, shadow-verify the `Native` candidate
  at `CallMethod` itself — **blocker finding, resolver decision NOT yet
  safe to build on it**): reused the existing E4b step 9 shadow-check
  function (`shadow_check_native_row_candidate`, unmodified, no new counter)
  at `CallMethod`'s own highest-traffic plain-probe arm, passing the
  already-computed `native_result.is_some()` — pure insertion, zero behavior
  change, `make test`-equivalent local suite green. Full `t/` sweep: 39558
  checks, ~965 mismatches (~2.4%), across 253 files, both directions
  (`real=false/shadow=true` ~545, `real=true/shadow=false` ~409), no single
  method dominant (`gist`/`raku` largest, but `join`/`sprintf`/`comb`/
  `DEFINITE`/`head`/`Int`/`substr`/... all contribute). **This contradicts
  E4b step 9's "essentially zero mismatches" only because that check ran at
  a low-traffic site (`call_method_with_values`, the interpreter slow path)
  — a sampling artifact, not evidence the underlying `native_row_servable`
  predicate is sound.** Two root causes identified by inspection: (1) the
  predicate checks only `(owner, method, arity, definite)`, blind to
  concrete-value-shape exceptions (`Sub.gist`/`.raku` decline the generic
  `"Any"`-owner row and use bespoke rendering instead — the same class of
  gap E4b step 2 already named for `should_bypass_native_fastpath`'s
  category-1 gates); (2) some methods the cascade genuinely serves
  (`DEFINITE` at 0 arity) have no row in `native_method_row_table.rs` at
  all. **Consequence: E5b must NOT build its "native or user" branch purely
  from the `Native` candidate** — either refine the predicate per-shape
  first, or keep the actual invocation as a direct, self-guarding
  `try_native_method` call (matching how `NativeCallBinding` was already
  found "no gain over a direct call" at E4b step 12) rather than a resolver
  decision that skips calling it. Open next question: does `CallMethod`'s
  pre-existing `skip_native`/`has_user_method` gate already make this moot
  for `CallMethod` specifically (by construction preventing `Native` from
  ever needing to outrank a matching `User` candidate at this arm)? Full
  detail, mismatch examples, and the two-option resolution in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"E5b step 1: shadow-verifying
  the Native candidate at CallMethod itself".
  **Progress 2026-08-11** (E5b step 2, answers step 1's open question --
  analysis only, no code change): the top-level `skip_native` gate does
  **not** by itself guarantee `User` outranks `Native` (it only extracts a
  `class_name` for `Instance`/`Package` receivers, missing e.g. a
  `Mixin`-shaped `"hello" but SomeRole` receiver). Yet raku-verified
  behavior is already correct (`$s.uc` on a `but`-mixed `Loud` role prints
  `MIXED-UC` in both raku and mutsu) because a *second*, independent bypass
  lives inside `try_native_method_raw` itself
  (`mixin_role_has_method(target, &method_name) => return None`,
  `vm_native_dispatch.rs:164-166`) -- one of 22 distinct per-shape
  `return None` bypass sites in that file alone. The augment-collision
  angle (`augment class Str { method uc {...} }`) isn't a real threat
  either, but because raku itself rejects it at compile time (redeclaring
  an already-declared core method without `multi` is a hard error; with
  `multi` it's an unimplemented multi-dispatch ambiguity) -- not an E5b
  ordering gap. **Conclusion: option (b) is confirmed as more than
  "cheaper" -- it is the only mechanism keeping today's dispatch correct**;
  option (a) is now actively discouraged, since making the `Native`
  candidate alone safe to route on would require reimplementing the same
  ~22 scattered shape-specific checks, ending up with two copies to keep in
  sync. **This generalizes past `CallMethod`: the `Native` candidate from
  `resolve_sequence` is measurement/hint-only at every E5/E6/E7 entry, not
  a routing decision** -- decision 1's "decision match" applies to the
  `User`/`NativeCallBinding` candidates only; the native probe stays a
  direct, self-guarding call in its existing cascade position through
  E5b-E7. Left open for the actual E5b cutover PR: how much of
  `try_compiled_method_or_interpret_sym`'s own pre-lookup interceptor
  cascade (default construction, Buf/Blob construction, Seq reification,
  ...) is safe to fold into a decision match, separate from the
  native-ordering question this step closes. Full detail in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"E5b step 2: the top-level
  `skip_native` gate does NOT settle the ordering question".
  **Progress 2026-08-11** (E5b step 3, shadow-verify the `User` candidate
  at `try_compiled_method_or_interpret`'s own resolution point): closes
  the "`User` candidate" half of step 2's open item 4. That function's
  Instance/Package resolution block turned out to be an inlined duplicate
  of `resolve_method_cached`'s exact three-tier cache and its two
  `resolve_method_with_owner_invocant` calls -- but reached only from the
  higher-traffic non-mut `CallMethod` opcode, which `resolve_method_cached`
  itself does not serve (only the Mut path does), so E4a's shadow probe had
  never actually run on this call site. Added the same
  `shadow_check_resolver` call `resolve_method_cached` already makes at its
  two resolve points, here too (sites `try_compiled_method_or_interpret:multi`/
  `:fresh`) -- pure instrumentation, zero behavior change. Full `t/` sweep
  (3022 files, `-P8`): 15085 total checks (both this box's new sites and
  `resolve_method_cached`'s existing ones), 25 mismatches (0.166%), every
  one decomposing to the single already-documented divergence class (a
  non-multi candidate whose signature doesn't match the call, e.g.
  `assign-rw($a is rw)` called with a literal) -- no new divergence class,
  confirming E4a's resolver is trustworthy at the busiest call site too, not
  just inferred from the Mut-path sweep. `make test`-equivalent (`prove -j8
  t/*.t`, 3022 files/28279 tests) green, unchanged. Still open: whether any
  of the ~430-line pre-lookup interceptor cascade ahead of this resolution
  block (Seq reification, the `.new`/`bless` native construction forks,
  IO::Handle/IO::Path native methods, MOP pseudo-methods, private methods,
  `^`-metamethods) can fold into a decision match, or must stay direct
  self-guarding pre-checks like the `Native` candidate -- most already gate
  on `has_user_method`/`is_native_method` internally, the same
  irreplaceable per-shape-check pattern step 2 found for `Native`. Full
  detail in `todo/deep/adr0019-e5-e7-entry-routing.md` §"E5b step 3".
  **Progress 2026-08-11** (E5b step 4, closing E5b at `CallMethod`): inventoried and
  classified the ~430-line pre-lookup interceptor cascade step 3 left open (Seq
  reification, ten `.new`/`bless`/class-method native construction forks, the
  IO::Handle/IO::Path Instance chain, MOP pseudo-methods, private methods,
  `^`-metamethods). None fold into a decision match -- each `.new`/`bless` fork is a
  self-contained construction routine with side effects beyond ordinary dispatch
  (registry mutation, deferred-iterator registration, real socket I/O), and the other
  four items are foldable-shape catalogs, class-(b) method-identity intercepts,
  a separate private-visibility tier (E7's job), or metamethod-specific invocation --
  all stay direct, self-guarding pre-checks, generalizing step 2's `Native`-candidate
  conclusion to this second cascade. Found and dispositioned a guard-completeness gap
  shared by five forks (IO::Path family/`Failure`/`Seq`/`IO::Socket::INET`/builtin
  class method: no `has_user_method` check, guarded only by exact class-name equality)
  as the same pre-existing `augment`-redeclaration-detection gap step 2 already found
  for `Str.uc` -- not a new ticket. **The resolution block itself did cut over**: since
  step 3 shadow-verified it an exact duplicate of `resolve_method_cached` reading/
  writing the same instance-level caches, replaced the ~90-line inlined duplicate with
  a direct `self.resolve_method_cached(..)` call -- a pure dedup, zero behavior change,
  closing step 2's open item 4 for the `User` candidate. `cargo test --lib` (779),
  full local suite (3022 files / 28,279 tests), `cargo clippy -- -D warnings` all
  green. **E5b is closed at `CallMethod`'s own entry point** -- native candidate stays
  a direct probe, user candidate resolution is shared/deduped, cascade stays direct.
  Full detail in `todo/deep/adr0019-e5-e7-entry-routing.md` §"E5b step 4". Next:
  E5c/E5d (`CallMethodDynamic` + the two hyper entries, already measured in E5 steps
  2-3).
  **Progress 2026-08-11** (E5c, both parts, closing E5c): classified
  `CallMethodDynamic`'s 14 named intercept arms and confirmed its general-case
  fallthrough already calls the shared `try_native_method`/`try_compiled_method_or_interpret`
  pair directly, with no inline cache or inlined resolution logic anywhere in the file
  -- unlike `CallMethod`, this entry never had a duplicate to converge, so it inherited
  E5b's closure automatically: no code change. For the hyper entries' per-element probe
  (part 2), raku-verified the gap E5 step 3 flagged (`HyperMethodCallDynamic` has no
  `skip_native`/`has_user_method` gate, unlike its static twin) against six collision
  attempts (Instance overrides + a `but`-mixin) -- zero divergence found, generalizing
  step 2's "the real safety net lives inside `try_native_method_raw` itself, not the
  caller's outer gate" conclusion to a second entry; downgrades that gap from "must
  fix" to "redundant defense-in-depth", no code change needed. The raku-verification
  pass did surface two real, unrelated bugs, both filed rather than fixed here: `.WHICH`/
  `.WHY` user overrides are silently ignored except via a compile-time-literal quoted
  call (two independent "skip native pseudo dispatch" mechanisms, neither aware these
  two of the eight MOP pseudo-methods are genuinely overridable --
  `todo/deep/pseudo-method-which-why-user-override-ignored-in-bareword-and-dynamic-form.md`),
  and unquoted `.$name` accepts a bare-string name where raku requires Callable/`CALL-ME`
  (`todo/tickets/dollar-dot-dynamic-method-name-should-require-callable.md`). Full detail
  in `todo/deep/adr0019-e5-e7-entry-routing.md` §"E5c, part 1"/"part 2". **E5c is closed.**
  Next: **E5d** (JIT-shim parity check, no code change expected).
  **Progress 2026-08-11** (E5d, closing all of E5): confirmed by inspection, not
  assumption -- of the two JIT shims in E5/E6's scope (`vm_jit_helpers.rs:314/367`),
  only `call_method` (`OpCode::CallMethod`) is E5's; it re-enters `exec_call_method_op`
  itself (the same entry point the non-JIT dispatch arm calls) with a byte-identical
  post-call tail, so every E5b/E5c change inside that function is covered under JIT
  automatically, no shim-side change needed. `CallMethodDynamic` and the two hyper
  opcodes have no JIT shim at all, so the check does not apply to them. **All of E5
  (steps 1-4, E5b, E5c parts 1-2, E5d) is closed.** Full detail in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"E5d". Next: **E6** (mutation-aware and
  container calls).
  **Checkbox note (2026-08-12):** this box's own text above already declared full closure
  the day it landed; the top-level checkbox was left `[ ]` by oversight while work moved
  straight on to E6 (which did get its checkbox flipped at closure, as did E7 later).
  Corrected here to `[x]` — no new content, matching the already-recorded closure.
- [x] **E6 — Route mutation-aware and container calls through the resolver.** Cover celled,
  lvalue/rw, Proxy, index/attribute writeback, and mutable aggregate entry points.
  **Design 2026-08-10** (same doc): includes `call_method_mut_with_values` (the second slow
  path), the dynamic-mut and hyper-dynamic gate gaps (raku-verified, closed by routing through
  the same decision), and `ArrayPush` — which keeps its container fast path behind a
  generation-refreshed `array_dispatch_pristine` bit (no user/wrap rows under `Array`/`List`),
  closing today's augmented-Array divergence with an O(1) check.
  **Progress 2026-08-11** (E6a, measurement slice for `CallMethodMut`): instrumented
  `exec_call_method_mut_op_impl` (`src/vm/vm_call_method_mut_ops.rs`) with the same
  `record_dispatch_entry_outcome`/`record_dispatch_entry_intercept` counters E5 step 1 introduced,
  entry key `"callmethodmut"` — no new counter functions, pure insertions (233 lines, 0
  deletions), zero behavior change. This slice covers `CallMethodMut` only, per design decision
  4's E6a scope, mirroring how E5 step 1 covered just `CallMethod`;
  `CallMethodDynamicMut`/`call_method_mut_with_values`/the Tier-A helpers are still to do as later
  E6a sub-slices. 33 named intercept arms added, structurally the mutation-aware twin of
  `CallMethod`'s own cascade (many shared arm names: `pair-freeze`, `proto`, `lock-protect`,
  `junction-invocant`/`junction-args`, the `lazy-*`/`hyperseq-*`/`modifier-*` families), plus a
  writeback-only sub-family with no `CallMethod` equivalent (`at-key`/`assign-key`/`delete-key`/
  `bind-key`/`bind-pos`, `shared-array-push-atomic`/`-legacy`/`-pop-shift`/`-splice`,
  `subst-mutate`/`match-make`). Verified via 5 individually-run representative files (the
  aggregate opcode histogram is unusable as a global cross-check here — its top-30-of-~340-opcode
  cap silently drops `CallMethodMut` from many single-file dumps, a 20% aggregate undercount vs
  the untruncated dispatch-entry sums), each an exact `sum(disjoint outcomes) ==
  CallMethodMut`-opcode-histogram match (5==5, 2==2, 38==38, 47==47, 13==13). Full `t/` sweep
  (3023 files, `prove -j8`, `MUTSU_VM_STATS=1`): disjoint total 89902 — `user=45097` (50.2%),
  `native=38640` (43.0%), `intercept=3830` (4.3%), `accessor=2335` (2.6%), overlay
  `notfound=28`; roughly 3.3x `CallMethod`'s E5-step-1 total, confirming bareword/variable
  receivers (which compile to `CallMethodMut`) carry the bulk of `t/`'s method-call traffic. Two
  arms confirm predictions E5 step 1 made for zero-count `CallMethod` arms explained by
  "compiles to `CallMethodMut` instead": `lock-protect` (2072, the largest single arm — variable
  `.protect` receivers) and `exception-concreteness` (7 vs 0). `shared-array-push-atomic` (1445,
  second-largest) has no `CallMethod` twin — mutation-only. 6 files
  (`say-env-roundtrip.t`/`slip-listop-args.t`/`sink-warning.t`/
  `undeclared-routine-compile-time.t`/`weird-errors-parse-forms.t`/
  `vendored-real-test-module.t`) fail under `MUTSU_VM_STATS=1` on exact-stderr assertions —
  confirmed pre-existing on `main` (the vm-stats dump itself writes to stderr at exit),
  reproduced identically before this change, not a regression. `make test`-equivalent
  (`prove -e target/debug/mutsu t/*.t`, 3023 files) otherwise green; `cargo clippy -- -D warnings`
  and `cargo fmt` clean. Full detail in `todo/deep/adr0019-e5-e7-entry-routing.md`
  §"Measurement slice results — CallMethodMut (E6a)". Still to do: `CallMethodDynamicMut` and
  `call_method_mut_with_values` measurement slices, the Tier-A helper survey, then E6b/E6c/E6d.
  **Progress 2026-08-11** (E6a, second slice, measurement for `CallMethodDynamicMut`):
  instrumented `exec_call_method_dynamic_mut_op` (`src/vm/vm_call_method_mut_ops.rs:347-433`, a
  small ~87-line function) with the same counters, entry key `"callmethoddynamicmut"` — 14 lines
  inserted, zero behavior change. Only four completion shapes exist: `.+`/`.*` modifiers
  (delegate to the already-measured `call_method_all_with_fallback`), a `$obj.$coderef(...)`
  call-sub-value form, a narrow `try_native_buf_mut` fast path (dynamic-name Buf mutating
  writes only), and the generic `vm_call_method_mut_with_values` interpreter fallback — no
  accessor probe, no distinct not-found completion, confirming the design doc's inventory
  correction that this entry has no general native/compiled probe. Verified via 5
  individually-run files (the same set E5 step 2 used for `CallMethodDynamic`), all exact
  `sum(disjoint outcomes) == CallMethodDynamicMut`-opcode-histogram matches. Full `t/` sweep:
  `user=29`, `call-sub-value=11` (== `intercept=11`), `native=1`, disjoint total 41 — low
  traffic, consistent with `CallMethodDynamic`'s own low-traffic finding in E5 step 2.
  `make test` (3023 files/28293 tests) green; clippy/fmt clean. Full detail in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"Measurement slice results — CallMethodDynamicMut
  (E6a, second slice)". Still to do: `call_method_mut_with_values` measurement, the Tier-A
  helper survey, then E6b/E6c/E6d.
  **Progress 2026-08-11** (E6a, third slice, measurement for `call_method_mut_with_values`):
  instrumented `call_method_mut_with_values` (`src/runtime/methods_mut_dispatch.rs:11-2748`,
  "the second slow path" per design decision 4's E6a scope), a ~2750-line function comparable in
  size to `CallMethodMut`'s own handler, entry key `"callmethodmutwithvalues"` — 182 lines
  inserted, zero behavior change. This is a plain `Interpreter` method (not an opcode handler)
  reached from ~10 call sites, dominated by but not limited to `CallMethodMut`'s own generic-fork
  tail. 41 named intercept arms added, including three near-duplicate method-match families for
  different receiver shapes (`@`-sigil array mutators, sigilless-array-binding mutators, `%`-sigil
  hash push/append). No opcode-histogram cross-check is available for a plain function; verified
  instead via 6 individually-run files checking `callmethodmutwithvalues`'s disjoint sum never
  exceeds `callmethodmut:user` in the same run (3 exact matches, 3 proper-subset). Full `t/`
  sweep: `native=14501` (52.9%), `user=11100` (40.5%), `intercept=1812` (6.6%), `accessor=0` (0%),
  disjoint total 27413 (~61% of `callmethodmut:user`'s own sweep total). Notable: `accessor=0`
  (the rw-accessor-write fast path never fires in `t/`) and `promise-channel-delegate=1011` (the
  single largest arm, pure pass-through to the non-mut sibling for `Promise`/`Channel` receivers —
  a concrete E6c/E6d cutover target). 20 files fail under `MUTSU_VM_STATS=1` in this sweep
  (broader than the 6-file list above; same root cause — vm-stats writes to stderr at process
  exit, and this sweep exercised more subprocess-spawning tests than the prior two slices'
  spot-checks did), confirmed pre-existing by reproducing 2 of them against the pre-slice base
  commit. `make test` (3023 files/28293 tests, no `MUTSU_VM_STATS`) green; clippy/fmt clean. Full
  detail in `todo/deep/adr0019-e5-e7-entry-routing.md` §"Measurement slice results —
  call_method_mut_with_values (E6a, third slice)". **All three E6a sub-slices are now measured.**
  Still to do: the Tier-A helper survey, then E6b/E6c/E6d.
  **Progress 2026-08-11** (E6a, Tier-A helper survey, closing E6a): docs-only, no dispatch behavior
  changed. Cross-checked `native_method_row.rs`'s `MUTATES_RECEIVER` flag (generated once by E2a's
  2026-08-10 probe) against the ~74 named intercept arms in the two instrumented files plus the 4
  unnamed Tier-A helpers (`try_native_array_mut`/`try_native_array_splice`/
  `try_native_hash_mut_bound`/`try_native_buf_mut`, `vm_call_method_mut_ops.rs`). All 41 current
  `MUTATES_RECEIVER` rows (`Str.subst-mutate`; `List`/`Array` `map`/`grep`/`rotate`/`push`/`pop`/
  `shift`/`unshift`/`splice`/`append`/`prepend`/`classify`/`categorize`/`rotor`/`produce`/`reduce`;
  `Hash.push`/`.append`; `Blob.new`/`push`/`pop`/`shift`/`unshift`/`append`/`prepend`/`splice`)
  also carry arity `N`, so the flag is currently redundant with the arity encoding for its one
  production reader (`native_row_servable`) — arity `N` alone already excludes them. Clean matches:
  `Str.subst-mutate`, `Hash.push`/`.append`, `List`/`Array` `push`/`pop`/`shift`/`unshift`/
  `append`/`prepend`/`splice`/`map`, `Blob` `push`/`pop`/`shift`/`unshift`/`append`/`prepend`/
  `splice` (plus `Hash.AT-KEY` and `List`/`Array.squish` confirmed correctly *un*flagged). Real
  gaps found: no row at all for `Pair.freeze`, `Match.make`, `ASSIGN-KEY`/`DELETE-KEY`/`BIND-KEY`/
  `BIND-POS`, `SetHash.set`/`.unset`/`.grab`/`.grabpairs`, `Collation.set`, the `Blob`
  `write-bits`/`write-num*`/`write-int*`/`read-*` family, and several owners E2a never probed at
  all (`Lock`, `Promise`, `Channel`, `LazyList`, `HyperSeq`, `RaceSeq`, `Proxy`, `Iterator`).
  `BagHash`/`MixHash.grab` are genuinely Tier-A-mutating but flagged `SPECIAL` not
  `MUTATES_RECEIVER` — traced to the probe only ever grepping `vm_call_method_mut_ops.rs`, never
  `runtime/methods_mut_dispatch.rs`, where `grab` actually lives. Conversely, `List`/`Array`'s
  `grep`/`rotate`/`classify`/`categorize`/`rotor`/`produce`/`reduce` rows over-claim
  `MUTATES_RECEIVER`: all eight names (map included) co-occur only in the `is_array_method`
  allow-list of the Array-subclass Instance-delegation branch, which routes them through
  non-mutating helpers — `map` alone turned out to have a *separate*, genuinely-mutating arm
  (`map-rw-writeback`) elsewhere. No `native_method_row_table.rs` edits landed (both candidate
  corrections recorded as open findings, not confidently resolvable without re-running the E2a
  probe methodology). None of this blocks E6b: `native_row_servable` is not consulted by real
  dispatch today, and E5b's "Native candidate is hint-only, never routing" conclusion already
  generalizes here. Full detail in `todo/deep/adr0019-e5-e7-entry-routing.md` §"Tier-A helper
  survey (E6a, final sub-slice)". **All of E6a is now closed.** Next: E6b.
  **Progress 2026-08-11** (E6b step 1, shadow-verify the `Native` candidate at `CallMethodMut`
  itself — mirrors E5b step 1): instrumented all ~10 `record_dispatch_entry_outcome("callmethodmut",
  "native"/"user")` sites inside `exec_call_method_mut_op_impl`
  (`src/vm/vm_call_method_mut_ops.rs`) — the five Tier-A helper completions, the
  `__mutsu_array_storage` delegation pair, and the generic-fork pair — with the same
  `shadow_check_native_row_candidate` call E5b step 1 used, skipping only the `skip_native` branch
  (no cascade-outcome to compare there). Pure insertion, 106 lines, 0 deletions, zero behavior
  change. Full `t/` sweep (3026 files, `-j8`, `MUTSU_VM_STATS=1`): 118117 checks, 5756 mismatches
  (4.88%, roughly double `CallMethod`'s 2.4%); the 20 stderr-assertion failures exactly match E6a
  third slice's pre-existing list, confirming no regression. Mismatch breakdown, three classes: (1)
  **2074 (36%), new to E6b** — Tier-A-served `push`/`shift`/`splice`/`append`/`unshift`/`pop`/
  `prepend` calls are *structurally* invisible to the `Native` candidate (`native_row_servable`
  excludes `MUTATES_RECEIVER` rows by construction, per the Tier-A helper survey's finding 4), not
  a predicate bug; (2) 2934 (51%) reproduces `CallMethod`'s own two root causes — missing rows
  (`DEFINITE`, 2066 alone, 36% of all mismatches) and shape-blind predicate exceptions
  (`gist`/`raku`/`FatRat`/`pull-one`); (3) 748 (13%) reproduces the reverse class (predicate
  over-claims by owner name, concrete shape declines — `raku`/`join`/`Int`/`sprintf`/`gist`/
  `comb`). **Confirms and sharpens E5b step 2's generalization**: the `Native` candidate stays
  hint-only at `CallMethodMut` too, and Tier-A traffic specifically could never route through it
  even with a shape-complete predicate, since `MUTATES_RECEIVER` rows are excluded by design — a
  future Tier-A-aware routing candidate would need to be a distinct kind, not attempted here.
  `cargo build`/`clippy -- -D warnings`/`fmt --check` clean. Full detail in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"E6b step 1: shadow-verifying the Native candidate at
  CallMethodMut itself". Next: E6b step 2 (does `try_compiled_method_mut_or_interpret_sym`'s
  `User`-candidate resolution duplicate `resolve_method_cached`, mirroring E5b step 3/4's dedup?).
  **Progress 2026-08-11** (E6b step 2, closing E6b): answered by inspection + git archaeology, no
  dedup needed — `try_compiled_method_mut_or_interpret_sym`'s resolution block already calls
  `resolve_method_cached` directly (that shared, `shadow_check_resolver`-instrumented function was
  introduced *for this exact call site* pre-ADR-0019, #4583; E5b step 4 later deduped `CallMethod`'s
  own separate inlined copy onto it, never touching the Mut path). Classifying the surrounding
  cascade (mirroring E5b step 4's table) found every pre-resolution item structurally identical to
  its non-mut twin (already annotated "mut path twin of the above" in source), but surfaced a real,
  independent dispatch-order bug in the *post*-resolution "lever A" native probes
  (`.sort`/`.map`/`.first`/coercions on a plain `Array`/`List`/`Str`/... receiver, `ValueView` not
  `Instance`/`Package` so the resolution block's `has_user_method` check never runs for them):
  `augment class Array { method sort {...} }` (legal raku — `Array` does not declare its own `sort`,
  unlike the already-known `Str.uc` redeclaration case) was silently shadowed by the native fast
  path, on three independent unguarded tiers (Tier-1 `try_native_method_raw`, the lever-A block, and
  the interpreter fallback's own by-name dispatch). Fixed with one shared predicate
  (`native_lever_a_user_override`) consulted at all three tiers plus both `call_method_with_values`
  entry points. That predicate's own correctness exposed a second, pre-existing gap:
  `class_mro`/`has_user_method` answered `false` for a bare unregistered builtin collection name
  (`"Array"`) even though `.^mro` reports its full chain via a different path — `class_mro_readonly`
  never consulted `builtin_type_catalog` for a *bare* name (only for the bracketed-parametrized
  case), so it fell to `compute_class_mro`, which had no `ClassDef` to read parents from. Fixed by
  teaching both `class_mro_readonly` and `compute_class_mro` to consult the catalog for a bare
  builtin name (ordered strictly *after* the existing bracketed-parametrized branch — an earlier
  version of this fix matched a bracketed name like `"Blob[uint8]"` too, dropping `Blob` from its
  own MRO since the catalog's parametrized rows track their base via `roles` not `mro`; caught by
  `t/digest-battery.t`'s SHA3 sub before landing). New regression test
  `t/augment-native-lever-a-methods.t` (raku-verified). Full local `t/` (3032 files/28,384 tests)
  green; a 192-file roast slice (S12-methods/S12-attributes/S14-roles/S02-types/S06-signature/
  S32-array/S03-metaops, release binary, `MUTSU_FUDGE=1`) green, 19,320 tests; clippy/fmt clean.
  Full detail in `todo/deep/adr0019-e5-e7-entry-routing.md` §"E6b step 2: the User-candidate
  resolution was already deduped -- no code change needed; classifying the surrounding cascade
  surfaced a real, unrelated dispatch-order bug instead". **All of E6b (steps 1-2) is now closed.**
  Next: E6c (the two dynamic gaps) or E6d (`ArrayPush`'s `array_dispatch_pristine` bit).
  **Progress 2026-08-11** (E6d, closing with no code change): ran V2's own raku baseline first —
  `augment class Array { method push(...) }` and its `multi method push` variant are both illegal
  in raku (`X::Redeclaration` / `X::Multi::Ambiguous`, on both `Array` and `List` — the same
  "already a legitimate program shape" exemption E5b step 2 established for `augment class Str {
  method uc {...} }`), so `ArrayPush`'s bypass of an illegal augment is not a new gap. The one
  legal override mechanism (a `does`-mixin: `@a does Loud` where `Loud` declares `push`) already
  dispatches correctly with zero code change — `exec_array_push_op`'s existing `is_simple_array`
  gate rebinds a mixed-in array away from `ValueView::Array`, so the fast path never runs and the
  call falls through to `call_method_with_values`, matching raku byte-for-byte. `Method.wrap` on a
  `.^lookup`-ed builtin (raku's own legal mechanism for intercepting `Array.push`) is out of scope
  — mutsu has no `Method.wrap` support at all, an unrelated missing-feature gap. **Conclusion: the
  `array_dispatch_pristine` generation-refreshed bit the design doc proposed is not needed — it
  would defend against a divergence that does not exist for any legal program.** Full detail in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"E6d: ArrayPush's augmented-Array divergence (V2) --
  raku-verified moot, array_dispatch_pristine not built". **E6a, E6b, and E6d are now closed; E6c
  (the two dynamic gaps) is the only remaining open box in E6.**
  **Progress 2026-08-12** (E6c, closing E6 outright): item 4 (`HyperMethodCallDynamic`'s missing
  `skip_native` gate) was already downgraded to "redundant gate, no code change" by E5c part 2
  before E6 began (`try_native_method_raw`'s own internal guards are the real safety net, not the
  caller's outer gate). Item 3 (`CallMethodDynamicMut`'s missing native/compiled probe) raku-
  verified real: `role Loud { method push($x) {...} }; @a does Loud; @a."push"(4)` silently ran
  the native array push instead of the role method (`[4]` instead of raku's `[1 2 3]` +
  `ROLE-PUSH: 4`). Root cause was one level deeper than the opcode handler, though: the shared mut
  slow path both `CallMethodDynamicMut` and `CallMethodMut`'s own generic fork bottom out into —
  `call_method_mut_with_values` (`runtime/methods_mut_dispatch.rs`) — special-cased
  push/append/unshift/prepend/pop/shift/splice purely by sigil (`target_var.starts_with('@')` /
  `('%')`), with no check that the value behind the sigil was still a plain `Array`/`Hash` and not
  a `does`-mixed `Mixin` — unlike the `ArrayPush` fast opcode's own `is_simple_array` gate (E6d)
  and the Tier-A `try_native_array_mut` helper (E6a), both of which already require
  `ValueView::Array`. So the *opcode-level* item-3 gap and a *deeper, more general* slow-path gap
  turned out to be the same bug wearing two faces: fixing the opcode probe alone would not have
  helped, since the fallback it reaches has the identical hole. Confirmed the same divergence on
  the **static** `CallMethodMut` path too, for any mutator without its own fast opcode (e.g.
  `@a.unshift(4)` with `@a does Loud`) — `ArrayPush` is the only mutator with a dedicated opcode,
  so `unshift`/`append`/`prepend`/`pop`/`shift`/`splice` always reach this same slow path even on
  a static receiver. Fixed by gating both the array-mutator and the hash-mutator blocks with
  `!self.mixin_role_has_method(&target, method)` (the exact guard `try_native_method_raw` already
  uses at `vm_native_dispatch.rs:165`), falling through to the function's own generic
  `call_method_with_values` tail on a hit — same "the shape check already is the safety net"
  pattern E5b step 2 and E6d established. Verified raku-byte-identical for `push`/`unshift`/
  `append` on both `Array`-mixin and `Hash`-mixin receivers, on both static and dynamic-name mut
  call forms; pinned as `t/mixin-array-hash-mutator-override.t` (8 assertions). Full `t/` suite
  (3034 files/28,400 tests) green; a 190-file roast slice (S14-roles, S32-array, S02-types,
  S06-signature, S03-metaops, S12-attributes, S12-methods — chosen for role/mixin/array/hash
  dispatch relevance) run against the release binary with `MUTSU_FUDGE=1`: the only two failures
  (`S02-types/quanthash.t`, `S12-attributes/trusts.t`) reproduce identically with this change
  reverted (confirmed by rebuilding from the pre-E6c commit), so both are pre-existing and
  unrelated. `cargo clippy -- -D warnings` / `cargo fmt` clean. **E6c is closed; all of E6 (E6a,
  E6b, E6c, E6d) is now closed.** Next: E7 (metaobject, qualified, and re-entrant calls).
- [x] **E7 — Route metaobject, qualified, and re-entrant calls through the resolver.** Cover HOW,
  `.^lookup`/`.^can`, qualified/private dispatch, EVAL carriers, and method objects.
  **Design 2026-08-10** (same doc): one consumer family per sub-PR (`run_instance_method`
  carrier sites, qualified, private-as-sequence-query, `.^lookup`/`.^can`/`.^methods` reading
  the call-path sequence, WALK, re-entrant carriers). The `.^can` dummy-`Value::NIL` probe is
  replaced by an E2 row lookup — a correctness fix as well as a routing change.
  **Progress 2026-08-12** (first consumer family, `run_instance_method` carrier sites, shadow-
  measurement only): `run_instance_method`/`run_instance_method_celled`
  (`runtime/class_dispatch.rs`) gained an optional `site: &'static str` tag, threaded through
  every one of their ~16 call sites as `""` (a no-op) except `vm_core_helpers::vm_run_instance_method`
  — the carrier's **only** two live callers, `CallDefined`'s user `.defined` and `SinkPop`'s user
  `.sink` in `vm_exec_dispatch.rs` — which now passes `"run_instance_method:vm-carrier"`. When
  tagged, `run_instance_method_celled` reuses E4a's `Interpreter::shadow_check_resolver` (the
  same probe already wired at `resolve_method_cached`'s two boundaries) to compare its own
  ad-hoc `resolve_method_with_owner_invocant` MRO walk against the E4 resolver's
  `resolve_sequence`, plus the E5/E6 generic `record_dispatch_entry_outcome`/`_intercept`
  counters under a new `"runinstancemethod"` entry key (arm name = the called method name, since
  this carrier's only two shapes are `.defined`/`.sink`). Pure insertion, zero behavior change
  (every added branch is `MUTSU_VM_STATS`-gated). Swept full `t/` (3040 files) plus a 124-file
  roast slice (`S12-methods`, `S12-attributes`, `S14-roles`, `S02-types` — chosen for
  metaobject/instance-dispatch relevance): the new site fired 102 times across 4 `t/` files
  (0 in the roast slice — those directories don't happen to exercise `.defined`/`.sink` on a
  user-overridable receiver) with **zero shadow mismatches at this site in either sweep** — this
  consumer family's ad-hoc resolution already agrees with the E4 resolver for all its actual
  traffic, so (unlike E6c) there is no gap to fix here. The sweep did surface 10 (`t/`) + 1
  (roast slice) mismatches overall, but every one is tagged `resolve_method_cached:fresh` — the
  pre-existing, unrelated E4a boundary, not this box's new site — confirmed byte-identical
  (same class/method/real/shadow detail strings) against the pre-E7 commit (10ecbd371) with this
  change reverted. All belong to E4a's already-documented explained bucket (the E8-deferred
  early-stopping rule: a non-multi method resolves by name in the real ad-hoc walk even when its
  typed signature does not bind the call, e.g. `method set(Int $x)` called via `$h.set("x")`) —
  this wider sweep just found more instances (10 vs. E4a's original 3) of the same bucket, not a
  new finding; out of scope for E7. `cargo clippy -- -D warnings` / `cargo fmt` clean; full `t/`
  (3040 files/28,437 tests) green. Full detail in `todo/deep/adr0019-e5-e7-entry-routing.md`
  §"E7 step 1: `run_instance_method` carrier sites — clean shadow-check, no cutover needed".
  **This consumer family needs no further work** (a shadow-only zero-mismatch result is itself
  the box's answer for this family — there is no ad-hoc-vs-resolver divergence to route around).
  Next E7 sub-slice: qualified dispatch / private-as-sequence-query, per the design's ordering.
  **Progress 2026-08-12** (second consumer family, qualified dispatch, shadow-measurement only):
  `dispatch_qualified_instance_method`'s (`runtime/methods_qualified.rs`) sole generic fallback —
  `self.resolve_method_with_owner(qualifier, actual_method, &args)`, reached for `self.Owner::method(...)`
  calls after the read-attribute/role-concretization/metamodel/qualified-`new`/native-ancestor
  special cases all miss — is now shadow-checked. Unlike E7 step 1, this callsite has no receiver
  *value* of the resolution target's type to derive an MRO chain from (the walk is rooted at the
  qualifier class NAME, not the instance), so `shadow_check_resolver` (E4a) was split: it is now a
  thin wrapper that derives `chain = self.dispatch_mro(invocant)` and delegates to a new
  `shadow_check_resolver_chain(site, class_name, method, method_sym, arg_values, invocant:
  Option<&Value>, chain: &[TypeId], real)` — the existing three callers (the two
  `resolve_method_cached` boundaries plus E7 step 1's `run_instance_method_celled`) are unchanged
  by the split. The new call site builds its own chain via
  `self.dispatch_mro(&Value::package(Symbol::intern(qualifier)))` and passes `invocant: None`
  (matching how `resolve_method_with_owner` itself calls `resolve_method_with_owner_impl(...,
  invocant: None)` for this exact case), recording outcomes under a new `"qualifieddispatch"`
  entry key (arm = the called method name). `dispatch_qualified_instance_method` has exactly one
  caller, so the probe is gated inline with `crate::vm::vm_stats::enabled()` rather than threaded
  through a `site` parameter — there is no second call site to tag differently yet.
  **Sweep**: full local `t/` (3047 files) plus the same 10-file roast slice used for the search
  (`S12-class/inheritance.t`, `S12-construction/new.t`, `S12-methods/{delegation,qualified,
  accessors,submethods}.t`, `S14-roles/{basic,conflicts,lexical,submethods-6e}.t`, all already
  whitelisted, found by grepping roast for `self.Owner::method`/`$obj.Owner::method` patterns).
  Unlike step 1, this consumer family sees real traffic: the new site fired 113 times across 13
  `t/` files and 40 times across 8 of the 10 roast files — **zero shadow mismatches at
  `qualifieddispatch` in either sweep**. The sweep's 10 total `resolver_shadow_mismatches` (all in
  the `t/` portion; the roast slice alone was 247 checks / 0 mismatches) are every one tagged
  `resolve_method_cached:fresh` in the mismatch detail, the same pre-existing E4a "non-multi method
  resolves by name independent of argument bind" bucket step 1 already root-caused and confirmed
  reproduces on the pre-E7 baseline — not a new finding, and not this box's site.
  `cargo clippy -- -D warnings` / `cargo fmt` clean; full `t/` (3047 files/28,481 tests) green; the
  10-file roast slice green (203 tests). Full detail in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"E7 step 2: qualified dispatch — clean shadow-check,
  no cutover needed". **This consumer family also needs no further work.** `dispatch_qualified_mixin_method`
  and `dispatch_qualified_non_instance_method` in the same file share the identical
  `resolve_method_with_owner` fallback shape but are deliberately left untagged for a later E7
  sub-slice (one consumer family per sub-PR). Next E7 sub-slice: private-as-sequence-query, per
  `todo/deep/adr0019-e5-e7-entry-routing.md`'s consumer list.
  **Progress 2026-08-12** (third consumer family, private-as-sequence-query, shadow-measurement
  only): `resolve_private_method_for_vm` (`runtime/resolution_private_method.rs`) — the single VM-
  facing entry point for both private-method call shapes, `$obj!m(...)` (unqualified,
  `resolve_private_method_any_owner`) and `$obj!Owner::m(...)` (owner-qualified,
  `resolve_private_method_with_owner`) — has exactly two callers in the whole codebase, both VM
  carrier sites (`vm_call_method_compiled_interpret.rs` / `vm_call_method_compiled_mut.rs`), the
  same "exactly two live callers" shape as step 1's carrier; the probe is gated inline like step 2
  (a single logical entry point, no second call site to tag differently). This step also does the
  "private-as-sequence-query" work the ADR's own E7 description names: `resolve_sequence`
  (`resolution_sequence.rs`) gained a `MethodVisibility` parameter (`Public`/`Private`) — `Public`
  is byte-for-byte the pre-existing filter (`is_private` skip, ancestor-submethod skip) and every
  existing caller (both `resolve_method_cached` boundaries, E7 step 1, E7 step 2, both
  `methods_native_bypass.rs` sites) now passes it explicitly, unchanged behavior; `Private` collects
  every `is_private` def at every chain level with no `is_my` exclusion (mirroring that neither
  ad-hoc private resolver ever checks `is_my`) and skips the `NativeCallBinding`/`Native` candidate
  blocks entirely (a private name can coincidentally collide with a public builtin/row name, and
  private dispatch can never reach either). `shadow_check_resolver_chain` gained the same
  `visibility` parameter threaded through from its existing callers (`Public`) plus the new private
  call site (`Private`). The owner-qualified chain is scoped to exactly `[owner]` — but ONLY when
  `owner` is actually present in the receiver's OWN MRO (`self.class_mro(class_name)`); the
  unqualified chain is the receiver's full `class_mro(class_name)`, both matching what the real
  ad-hoc walks consult. **The sweep found and fixed one real bug before landing**: the first cut
  built the qualified chain as `[TypeId::intern(owner)]` unconditionally, so a call like
  `$b!A::p()` where `$b`'s class does not inherit from `A` incorrectly found `A`'s own private `p`
  as a shadow candidate (`class=B method=p real=None shadow=Some("A")`, `t/private-owner-qualified-
  permission.t`) even though the real walk (rooted at `B`'s own MRO, which never contains `A`)
  correctly answers `None`. Guarding the chain to empty when `owner` is absent from the receiver's
  MRO fixed it — cheap and safe per the box's own "small, obviously safe" allowance, not deferred.
  **Sweep** (after the fix): full local `t/` (3047 files, debug binary, run both `-j2`/`-j4` and
  serially) plus an 11-file roast slice (found by grepping roast for `!\w+\(`/`self!`/`$_!Owner::`
  patterns under `S12-*`/`S14-*` and intersecting with `roast-whitelist.txt`): `S12-attributes/
  {class,instance}.t`, `S12-class/inheritance.t`, `S12-enums/thorough.t`, `S12-introspection/
  methods.t`, `S12-methods/{instance,private,trusts}.t`, `S14-roles/{basic,conflicts,stubs}.t` — all
  eleven already whitelisted; `S12-methods/private.t` in particular is the private-dispatch spec
  file itself and includes a `for ^10000` role-private-method-caching stress loop, so the slice
  exercises this dispatch shape heavily rather than getting a misleadingly clean zero-traffic
  result (`privatedispatch` fired ~260,600 times in that one file alone). **Zero shadow mismatches
  tagged `privatedispatch` in either sweep** post-fix: the roast slice recorded 260,566
  `resolver_shadow_checks` / 1 mismatch, and the `t/` sweep recorded 15,600 checks / 10 mismatches
  (246 of the checks were `privatedispatch`, fired across 30 distinct `t/` files) — every one of
  the 11 total mismatches is tagged `resolve_method_cached:fresh`, the same pre-existing E4a "non-
  multi method resolves by name independent of argument bind" bucket steps 1/2 already root-caused;
  none are tagged `privatedispatch`. The apparent `-j4`/`-j2` parallel-`prove` failures in ~20
  unrelated `t/` files (`proc-async.t`, `io-handle-*.t`, `is-run.t`, `quietly.t`, ...) were
  confirmed to be local concurrent-subprocess resource contention, not a regression: every one of
  those files passes individually, and the sequential `make test` run (the CI-matching invocation)
  is fully green. `cargo build`/`cargo test --lib` (804 tests, including 3 new `resolve_sequence`
  unit tests: two for the new `Private` tier plus one confirming the unchanged `Public` tier still
  excludes a private method) / `cargo clippy -- -D warnings` / `cargo fmt` clean; `make test` (3047
  files/28,481 tests) green; the 11-file roast slice green (491 tests, `MUTSU_FUDGE=1`).
  Full detail in `todo/deep/adr0019-e5-e7-entry-routing.md` §"E7 step 3: private-as-sequence-query
  — one real chain-scoping bug found and fixed". **This consumer family needed one small fix, now
  closed** (unlike steps 1/2's zero-mismatch results, but the fix was in the shadow probe's own
  chain construction, not in the real dispatch path — still zero real-behavior change). Next E7
  sub-slice: `.^lookup`/`.^can`/`.^methods` reading the call-path sequence, per the design's
  consumer ordering (the `.^can` dummy-`Value::NIL` probe replacement is called out specifically in
  the design paragraph above).
  **Progress 2026-08-12** (fourth consumer family, `.^can`, shadow-measurement — cutover
  DEFERRED): `Interpreter::collect_can_methods`'s (`runtime/methods_classhow_method_obj.rs`) last
  fallback tier is exactly the dummy-`Value::NIL`-arg probe this design paragraph names
  (`native_method_1arg(target, method_sym, &Value::NIL)`, invoking the real 1-arg cascade with a
  fake arg just to see if `Some(_)` comes back — no 2-arg check exists at all). A new existence
  predicate, `Interpreter::e2_native_method_exists` (`runtime/receiver_class.rs`), asks the E2
  catalog directly instead: it walks the receiver's full `dispatch_owner_chain` (sharing a new
  `chain_owner_probe` helper with `record_native_row_coverage`) and, at each level, checks a new
  `native_method_row::native_method_row_exists(owner, name)` — table PRESENCE, not
  `native_method_row`'s returned arity/flags, which cannot answer "does this method exist at all"
  by itself (a missing key and a genuinely-probed "exists but only via a special/mutating path" row
  like `List.push` share the identical `(N, ...)` bit pattern, confirmed by a unit test). Unlike
  E4b's `native_row_servable` (which `resolve_sequence`'s `Native` candidate uses to ask "is this
  row reachable for THIS call's shape"), this deliberately ignores `SPECIAL`/`MUTATES_RECEIVER`/
  arity/definedness — `.can` asks "does Raku consider this a method at all", confirmed against real
  Raku (`List.can("push")` is true on an indefinite type object). `collect_can_methods` computes
  both answers under `MUTSU_VM_STATS` and records agreement via a NEW dedicated counter pair
  (`record_can_shadow_check`, `vm/vm_stats.rs`) rather than the shared E4a/E4b/E7-steps-1-3
  `RESOLVER_SHADOW_*`/`NATIVE_ROW_SHADOW_*` infra, since this compares two existence predicates and
  never touches `resolve_sequence` — reusing the shared total would repeat the exact "false lead"
  step 1's progress note above describes. Pure insertion, zero behavior change: the dummy-arg probe
  alone still drives `.can`'s real answer. **Swept a 16-case hand-built probe script, all 16
  already-whitelisted roast files that call `.can`/`.^can` (found by grepping roast for
  `\.\^?can\(`), and the full local `t/` suite (3057 files/28,557 tests)**: 115 total
  `can_shadow_checks`, 3 `can_shadow_mismatches`, every one `real=true shadow=false` (the E2 lookup
  under-answers, never over-answers) — `IO::Path.can("e")` (`IO::Path` is one of the owners
  `native_method_row.rs`'s own module doc already names as never probed by E2a/E2b at all),
  `(1,2).^can('int8')` (`t/native-int-coerce-methods-are-cool-only.t:18` — `Cool` is an ABSTRACT
  owner the row-*generation* probe skipped for lack of a sample value, even though
  `builtin_type_method_names("Cool")` does list `int8`), and `$c.can("cancel")` on a scheduler
  `Cancellation` handle (`t/scheduler-cue-times.t:18` — a class entirely outside the 14-owner
  E2a/E2b campaign scope, answered by the dummy probe's separate `classhow_find_method` tier). Every
  mismatch traces to a documented, already-known catalog-coverage gap, not a real dispatch-path bug
  — so, per this box's own "if the catalog's incompleteness would make `.can` wrongly answer `false`
  for something genuinely callable, do NOT cut over" guidance, **the cutover is deferred, not
  landed**: `e2_native_method_exists` stays shadow-only, and `has_native`'s existing tiers keep
  driving `.can`/`.^can` unchanged. `cargo build`/`cargo test --lib` (810 tests, 6 new: 2 pin the
  `native_method_row_exists` table-presence-vs-return-value distinction, 4 exercise
  `e2_native_method_exists`'s chain walk including a 2-arg-only-method case the dummy probe's
  0/1-arg-only cascade calls cannot see) / `cargo clippy -- -D warnings` / `cargo fmt` clean; `make
  test` green. Because `.can`/`.^can` is a public API surface, this step's local verification went
  beyond the usual single-slice scope (full `t/`, every roast file that calls `.can` at all, not a
  sample). Full detail in `todo/deep/adr0019-e5-e7-entry-routing.md` §"E7 step 4: `.^can` —
  shadow-measured, cutover DEFERRED (E2 catalog coverage gap)". **This is the first E7 sub-slice
  that is not a clean zero-mismatch/zero-fix result — as this design paragraph itself predicted —
  but the correct response to a catalog-coverage gap (not a dispatch-path bug) is to defer the
  cutover, not force it.** Next E7 sub-slice: `.^lookup`/`.^methods` reading the call-path sequence
  (WALK and the EVAL/re-entrant carriers come after those, per the design's consumer ordering).
  **Progress 2026-08-12** (fifth consumer family, `.^lookup`, a real MRO-walk correctness fix — not
  a shadow-measurement box): unlike steps 1-4, this one started from a confirmed, already-reproduced
  bug rather than a shadow-check-driven discovery. `Interpreter::classhow_lookup`
  (`runtime/methods_classhow_lookup.rs`) — the sole implementation of `.^lookup`, one call site in
  `methods_classhow_dispatch.rs`'s `"lookup"` arm — only ever consulted the receiver's OWN
  registered class (`self.registry().classes.get(&class_name_str)`), never walking the MRO to an
  ancestor: `class A{method foo{...}}; class B is A{}; B.^lookup("foo")` returned `Nil` instead of
  the inherited method real Raku finds (confirmed: `raku`'s `B.^lookup("foo").defined` is `True`).
  Fixed by replacing the single-class lookup with a `self.class_mro(&class_name_str)` walk
  (most-derived first, same registry MRO primitive `resolve_method_with_owner`/
  `resolve_private_method_any_owner` already use elsewhere), taking the first class whose own
  `class_def.methods` has the name — the per-level `Value::make_sub` construction (callable-type/
  return-type/wrap-chain-index env tags) is otherwise unchanged from before this fix. Confirmed via
  `raku` that `.^lookup` deliberately does NOT filter by `is_my`/`is_multi`/visibility at all — it
  finds an ancestor's submethod (`class M{submethod boot{}}; class N is M{}; N.^lookup("boot")` is
  defined) and an inherited multi (returning just the first candidate, matching this fix's own
  per-level "first def wins" shape; true multi/proto candidate-sequence modeling is E8's job, out of
  scope here). **A second, more subtle bug surfaced mid-fix and was fixed in the same PR (small,
  obviously safe, not deferred — matching step 3's precedent)**: `Interpreter::classhow_find_method`
  (same file — the implementation of `.^find_method`, and indirectly of `.can` on a Package
  receiver via its `methods_instance_ops.rs` fallback) used to delegate its own "does this name
  exist" fallback straight to `classhow_lookup`. Once `classhow_lookup` became MRO-walking, that
  delegation started leaking `.^lookup`'s permissive ancestor-submethod visibility into
  `.^find_method`/`.can`, which real Raku keeps strict (`N.^find_method("boot").defined` and
  `N.can("boot").elems` are both false/0 — confirmed against `raku`; only the DECLARING class `M`
  finds it via either) — caught by a full `t/`-suite regression (`t/can-does.t` test 15, "`.can`
  does not inherit submethods into subclasses"). Fixed by extracting the shared per-level
  construction into `classhow_lookup_impl(invocant, method_name, include_ancestor_submethods: bool)`
  — `classhow_lookup` calls it with `true` (unchanged `.^lookup` behavior), `classhow_find_method`'s
  fallback with `false` (skips a level's def when `is_my` and the owning class is not the receiver's
  own). Two dead-end approaches tried and reverted before this one: (1) routing
  `classhow_find_method` through `collect_can_methods` directly recurses infinitely (that function's
  own native-method fallback tier calls `classhow_find_method`, confirmed by an actual stack
  overflow); (2) extracting `collect_can_methods`'s non-recursive tiers into a shared
  `collect_can_user_methods` helper avoided the recursion but dropped `.^find_method`'s
  `__mutsu_callable_type`/`__mutsu_lookup_*` env tagging for a non-multi method (regressing
  `t/declarator-trailing-wherefore.t` test 6, `.^name` reporting the generic `"Sub"` instead of
  `"Method"`) — and, when that tagging was added back unconditionally, broke `.wrap()` writeback for
  `.can`'s own OTHER callers instead (`t/method-wrap-writeback-only-mutations.t` test 3,
  `t/monitor-method-does-not-leak-topic-or-self.t` test 6, `t/exporthow-grammar-how.t`), because some
  wrap-chain-registration path apparently keys off that metadata's mere presence on a
  `.can`-obtained Sub. The `classhow_lookup_impl` split above avoids all of this: `classhow_find_method`
  never touches `collect_can_methods`/`collect_can_user_methods` at all, so `.can`'s own construction
  path (`methods_classhow_method_obj.rs`) is completely untouched by this PR. Two follow-up findings
  filed as their own tickets rather than expanding this PR's scope (per the "one bug per sub-PR"
  discipline): `todo/tickets/classhow-lookup-all-candidates-non-multi-mro-gap.md` (the sibling
  `classhow_lookup_all_candidates`, backing `.^find_method(name).candidates`, has the identical
  single-class-only bug in its non-multi branch) and
  `todo/tickets/classhow-lookup-surfaces-private-methods.md` (`.^lookup` surfaces a private method by
  its bare name, which real Raku's `.^lookup` does not — a separate, pre-existing visibility-
  filtering gap unrelated to the MRO-walk shape this fix targets). New `t/classhow-lookup-mro.t` (14
  assertions: the exact repro, own-class/2-levels-deep/role-composed/nonexistent/override-wins/
  inherited-multi/ancestor-submethod cases for `.^lookup`, plus 4 regression assertions pinning
  `.^find_method`/`.can`'s stricter ancestor-submethod exclusion). `cargo build`/`cargo clippy -- -D
  warnings`/`cargo fmt` clean; full local `make test` green (3059 files/28,585 tests, confirmed
  clean AFTER a `cargo clean -p mutsu` full rebuild ruled out stale-incremental-cache artifacts from
  an earlier concurrent-build mishap in this session); a 18-file roast slice (found by grepping
  roast for `\.\^lookup\(`/`HOW\.lookup` and intersecting with `roast-whitelist.txt`) green (898
  tests, `MUTSU_FUDGE=1`; `roast/S32-io/spurt.t`'s one failure on the first attempt was the known
  stale `temp-file-RT-126006-test` artifact `make roast` normally clears before starting, not a
  regression — confirmed clean after removing it). Since this changes a real `classhow_lookup`/
  `classhow_find_method` answer (not just a shadow-measurement probe), this counts as the "touched
  name/type resolution" case CLAUDE.md's testing rule names — the roast slice above is that
  targeted local check. Next E7 sub-slice: `.^methods` reading the call-path sequence (WALK and the
  EVAL/re-entrant carriers come after, per the design's consumer ordering).
  **Progress 2026-08-12** (sixth consumer family, `.^methods`, a real mixin-enumeration fix plus a
  shadow-measurement, not a clean zero-mismatch result like steps 1/2): scoping started from the
  hypothesis (per this design paragraph's own framing) that `dispatch_classhow_methods`'s
  (`runtime/methods_classhow_builtin_methods.rs`) main (non-`:local`) branch was already
  MRO-correct, since it already walks `self.class_mro(&class_name)` — unlike step 5's `.^lookup`,
  which had never walked the MRO at all before that fix. Direct `raku`-vs-`mutsu` comparison during
  scoping confirmed the MRO walk itself IS correct for plain inheritance (including submethods,
  private methods, and multi-method-as-single-dispatcher-entry, all already exercised end-to-end by
  the pinned `roast/S12-introspection/methods.t`, which passes cleanly both before and after this
  box), but surfaced a different, genuine gap: `(5 but R1).^methods()` (no `:local`) never included
  `R1`'s own mixed-in method (`zork`), even though `(5 but R1).zork` is directly callable and
  `(5 but R1).^methods(:local)` (a sibling branch) already collected it correctly. Confirmed against
  real `raku`: a `but`-mixin's `.^mro` puts an anonymous composite pun class FIRST — `(5 but
  R1).^mro` is `((Int+{R1}) (Int) (Cool) (Any) (Mu))` — and that pun class's own methods are exactly
  the mixed-in role's methods, so `zork` appears as the very first entry of `(5 but
  R1).^methods.map(*.name)` on real `raku`. **Fix**: the non-`:local` branch now extracts
  `mixin_role_names` (already computed once, above the `if local {...} else {...}` split, for the
  `:local` branch's own existing handling) and calls `self.collect_role_methods(role_name, private,
  &mut result)` for each BEFORE the base `class_name`'s own `class_mro` walk — mirroring the
  `:local` branch's pre-existing pattern verbatim, not a new mechanism, and matching the pun-class-
  first ordering confirmed against `raku` above. Two roles mixed onto the same value
  (`5 but R1 but R2`) both contribute their methods, pinned alongside the base repro. **Shadow-check
  added as well** (this box's other half, following the design paragraph's "reading the call-path
  sequence" framing): a new `MUTSU_VM_STATS`-gated comparison, `record_methods_shadow_check`
  (`vm/vm_stats.rs`, its own dedicated counter pair — not the shared `RESOLVER_SHADOW_*` family,
  since this compares two whole MRO CHAINS rather than a single dispatch-winner pick, the same
  reasoning step 4's `.^can` check already established for its own dedicated pair), comparing the
  chain the walk actually enumerates (`class_mro(class_name)`, the registry MRO primitive) against
  the E4 resolver's own canonical chain for the same receiver (`Interpreter::dispatch_owner_chain`,
  TypeId-based). Pure insertion, zero behavior change: `mro` alone still drives the enumeration.
  Swept a 10-case hand-built probe (plain classes, role composition, `but`-mixins, `List`/`Str`/
  `Int` builtins, an instance and a type-object receiver): 10 checks, 0 mismatches — the two chain
  computations already agree everywhere probed, once the mixin-methods fix above made the enumerated
  RESULT match `raku` too (the shadow check compares the underlying CHAIN, not the final method set,
  so it was unaffected by the mixin fix either way, but both are reported together as this sub-slice
  since they were found and fixed in the same investigation). `cargo build`/`cargo clippy -- -D
  warnings`/`cargo fmt` clean. New `t/classhow-methods-mixin-role.t` (9 assertions: the confirmed
  repro, two-roles-mixed-in, base-type-methods-still-present, plain-inheritance regression guard,
  and `:all` combined with a mixin). All 6 already-whitelisted roast files that call `.^methods(`
  (found by grepping roast for `\.\^methods\(`), including the two role-mixin files
  (`roast/6.c/S14-roles/mixin-6c.t`, `roast/S14-roles/mixin-6e.t`) and the dedicated introspection
  spec (`roast/S12-introspection/methods.t`, 57 assertions covering `:local`/`:all`/`:tree`/
  `:private`/multi-dispatcher-shape/attribute-order), green (`MUTSU_FUDGE=1`). Since this changes a
  real `.^methods()` answer (not just a shadow-only probe), this counts as CLAUDE.md's "touched
  name/type resolution" case requiring a local roast check before opening the PR — the 6-file sweep
  above is that check. Full detail in `todo/deep/adr0019-e5-e7-entry-routing.md` §"E7 step 6:
  `.^methods` — a real mixin-enumeration fix, plus a clean chain shadow-check". Next E7 sub-slice:
  WALK and the EVAL/`subtest` re-entrant carriers, per the design's consumer ordering.
  **Progress 2026-08-12** (seventh consumer family, `.WALK`, two real bugs plus a clean chain
  shadow-check — not a "just add a shadow check" box like steps 1/2): scoping
  (`src/runtime/methods_walk.rs`) found `try_walk_method` never recognized a runtime mixin
  (`ValueView::Mixin`) as a valid receiver at all — `(5 but R1).WALK("zork")` raised
  `X::Method::NotFound` where real `raku` returns `(R1::zork)`, confirmed by direct comparison
  during scoping the same way steps 5/6 found their gaps. **Fix 1**: `try_walk_method` now extracts
  `mixin_role_names` from a `Mixin` receiver (mirroring step 6's `dispatch_classhow_methods` fix)
  and prepends a new `WalkKind::MixinRole` target per role — unlike a statically-`does`-composed
  role (`WalkKind::Role`, submethods only, since regular methods are already composed into the
  consuming class's own method table), a *runtime* mixin's role is never composed anywhere, so its
  own regular (non-private) methods are the only place to find them; `lookup_own_walk_method` grew
  a matching arm. Investigating the fix surfaced a second, independent bug: `walk_list_invoke_direct`
  read a Mixin invocant's attributes via a direct `ValueView::Instance` match, which is `None` for a
  `Mixin`-wrapped instance — any WALK candidate on the *base* class of a mixin ran with an empty
  attribute map. **Fix 2**: reuse the existing general Mixin-unwrap helper
  (`Interpreter::self_instance_attrs`, already used by ordinary method dispatch for the same purpose)
  instead of the ad hoc match. Testing fix 1 surfaced a third, unrelated bug while checking `say`
  output: `$obj.WALK(...)().gist`/`.Str` printed `()`/empty on an otherwise-correct, already-working
  (pre-mixin, plain-class) WALK result. Root cause: `Interpreter::force_lazy_list`
  (`runtime/resolution_lazy.rs`), the forcer that `.gist`/`.Str`/`say` route through (via
  `call_method_with_values`'s "force LazyList and re-dispatch as Seq" branch), had a `cat_pull`
  branch but no `walk_pending` branch — and a WALK-produced `LazyList` starts with a **non-empty**
  `Some(Vec::new())` cache (`LazyList::new_cached`), so the function's cache short-circuit
  (`if let Some(cached) = list.cache... { return Ok(cached) }`) returned the still-empty initial
  cache before ever pulling a candidate, exactly the failure mode the existing `cat_pull` comment
  already warned about for its own case ("cache always starts non-empty, so this must run before the
  cache short-circuit"). **Fix 3**: added the missing `walk_pending` branch in the same position,
  delegating to the existing `force_walk_pending` (already used correctly by the VM's own opcode-level
  forcer, `force_lazy_list_vm` in `vm_helpers_lazy.rs`, which is why `for`-iteration and array
  coercion — a separate, already-correct forcing path — never showed this bug). **Shadow-check**
  added too, matching step 6's "the check is the other half of the fix" pattern: a new
  `MUTSU_VM_STATS`-gated `WALK_SHADOW_*` pair (`vm_stats.rs`) compares the CLASS-kind portion of the
  chain WALK's default (`:canonical`) ordering walks (`build_walk_targets`, ultimately
  `class_mro_readonly`) against the E4 resolver's own canonical chain for the same receiver
  (`dispatch_owner_chain`) — deliberately scoped to `:canonical` only, since WALK's other orderings
  (`:super`/`:breadth`/`:ascendant`/`:descendant`) are legitimate alternate traversals documented by
  raku's own WALK spec, not MRO restatements, so comparing them against the resolver's MRO chain
  would be a guaranteed, meaningless mismatch. Swept the existing `t/walk-lazy.t`/`t/walk-orderings.t`
  suite plus a hand-built mixin/two-role/ordering probe: 12 + 5 checks, 0 mismatches, once a
  same-name-as-a-builtin-type test class (`Sub`, colliding with the builtin `Sub`/`Routine` type) was
  renamed out of the probe — that collision is the SAME already-tracked owner-name-collision gap the
  E1a ledger already records (`multi_arg_type_keys`), not a new finding. New `t/walk-mixin-role.t` (11
  assertions: mixin-onto-builtin, mixin-onto-instance base-chain-still-walkable,
  mixin-role-method-found, absent-method-empty, two-stacked-mixin-roles, mixin-overriding-base,
  attribute-access-through-a-mixin-wrapped-candidate, plain-builtin-type-receiver-regression-guard)
  plus two new assertions appended to `t/walk-lazy.t` (`.gist`/`.Str` forcing). `cargo build`/`cargo
  clippy -- -D warnings`/`cargo fmt` clean; full local `make test` green. Since fixes 1-3 change real
  `.WALK` answers (not just a shadow-only probe), this counts as CLAUDE.md's "touched name/type
  resolution" case: `roast/S12-introspection/walk.t` and `roast/S14-roles/attributes-6e.t` (the two
  whitelisted files that call `.WALK(`) both green locally with `MUTSU_FUDGE=1`. `git grep -n "WALK"
  src/runtime/ src/vm/` confirmed WALK has no other internal caller in mutsu (no TWEAK/BUILDALL
  construction-phaser use, unlike the ADR's general phrasing might suggest) — the blast radius is
  exactly the `.WALK(...)`/`WalkList` dispatch sites this box touched, nothing wider. Full detail in
  `todo/deep/adr0019-e5-e7-entry-routing.md` §"E7 step 7: `.WALK` — two real bugs (mixin receivers,
  lazy-forcing gap) plus a clean chain shadow-check". **Next (and last) E7 sub-slice: the EVAL/
  `subtest` re-entrant carriers** — after that, E7 as a whole is closed and E8 is next.
  **Progress 2026-08-12** (eighth and final consumer family, EVAL/`subtest` re-entrant carriers — no
  distinct dispatch carrier exists, E7 closes): unlike steps 1-3 (each a standalone Rust function with
  its own ad-hoc MRO/candidate walk) and steps 5-7 (confirmed real dispatch-answer bugs), this step's
  finding is that EVAL/`subtest` do not have a distinct method-dispatch carrier at all. `subtest`
  (`test_fn_subtest`, `runtime/test_functions/tap_subtest.rs:91`) runs its block via a plain
  `self.call_sub_value(block, vec![], true)` — the same call path any ordinary Sub/Method value uses
  everywhere else; a `.method(...)` call inside a subtest body compiles to the same `CallMethod*`
  opcodes and dispatches through the same E1-E7-routed VM handlers as any other call. `EVAL`
  (`builtin_eval` → `eval_eval_string` → `parse_and_eval_with_operators` →
  `eval_block_value_opts(&stmts, true)`) genuinely re-parses and re-compiles a fresh AST, but does so
  with the SAME `compiler::Compiler` used for every compilation unit and runs the result through the
  SAME `run_nested`/`exec_one()` VM opcode loop as everything else — exactly what
  `vm_call_dispatch.rs`'s own `is_interpreter_carrier_function` doc comment already states ("EVAL...
  compile[s] source to bytecode and run[s] it on a sub-Interpreter... Neither tree-walks user code"),
  explicitly classifying this as a state-ownership (lever B) concern, not a dispatch-fallback (lever
  A) one. A method call inside `EVAL '...'` therefore hits an ordinary `CallMethod*` opcode with no
  separate ad-hoc resolution walk analogous to steps 1-7's targets. Sampled the dispatch-adjacent-
  looking subset of `eval_block_value`'s 141 non-`resolution_eval.rs` call sites (grammar `token`/
  `rule` bodies, `proto` dispatch bodies, attribute defaults, subscript index expressions): every one
  runs an ALREADY-SELECTED block/statement-list, never decides which method/candidate to call —
  candidate *selection* for multi/proto is explicitly E8's job per this ADR's own box description, a
  useful independent confirmation that E7's consumer list correctly excluded it. No code change, no
  test addition: this box's own honest output is the finding itself, per the assignment's explicit
  allowance that "checked, nothing to do, here's why" is as valuable as steps 1/2's clean shadow-
  checks. Full detail in `todo/deep/adr0019-e5-e7-entry-routing.md` §"E7 step 8: the EVAL/`subtest`
  re-entrant carriers — no distinct dispatch carrier exists; E7 closes" (includes the full per-step
  outcome table). **E7 is now closed as a whole.** Across all eight sub-slices: three clean shadow-
  checks with no gap (steps 1, 2, and this one, step 8 having no carrier at all to check); one
  shadow-measured-but-deferred catalog gap (step 4, `.^can`); four confirmed-and-fixed real
  `raku`-vs-`mutsu` behavioral bugs (steps 3, 5, 6, 7 — steps 6 and 7 in particular both independently
  found the same shape of gap, a runtime `but`-mixin's own role methods missing from an
  enumeration/walk that only ever traversed the registered class hierarchy, suggesting mixin receivers
  were systematically under-tested against introspection/reflection call paths before this box, even
  though ordinary method *invocation* on a mixin worked correctly throughout). **Next Phase E box: E8
  — model multi/proto/submethod ordering in the candidate sequence**, per this ADR's own box ordering.
- [x] **E8 — Model multi/proto/submethod ordering in the candidate sequence.** Remove parallel
  multi and submethod resolver entry points without changing tie-breaking or role conflicts.
  **Design 2026-08-10** (`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`):
  candidates carry `level`/`stored_idx` so winner selection (existing ladder, per call) and
  deferral order (sequence order + per-call signature filter) both derive from one sequence;
  submethod visibility and `drop_flattened_role_duplicates` apply at build time.
  `Registry::proto_methods` folds into `MethodEntry`. Unifying the method-vs-sub ranking
  ladders is explicitly out of scope.
  **Progress 2026-08-12** (E8a, sequence structural fields + shadow comparison): landed the
  slice plan's E8a exactly as scoped. `ResolvedCandidate::User` gained `level: u16`/
  `stored_idx: u16` (its position in the chain / within that level's stored declaration order —
  `resolution_sequence.rs`), set from `resolve_sequence`'s existing per-level, per-overload
  loop, so the sequence's own construction order already IS `(level, stored_idx)`-ascending
  with no extra sort needed. `drop_flattened_role_duplicate_candidates` (the build-time twin of
  `resolution_method.rs`'s post-match `drop_flattened_role_duplicates`) now runs inside
  `resolve_sequence` itself, before any per-call filtering — behavior-preserving, since the
  dedup removes candidates purely by owner identity and the flattened copy it keeps has the
  same signature as the raw one it drops. The "ranker extracted to consume a candidate slice"
  part of the slice plan became `Interpreter::match_sequence_candidates`, pulling the
  signature-match loop out of `shadow_check_resolver_chain` (E4a's winner probe) so a second
  probe could reuse it verbatim instead of copying the loop. That second probe,
  `Interpreter::shadow_check_deferral_sequence`, hooks the single real call site that builds
  the `nextsame`/`callsame` deferral list — `Interpreter::push_method_dispatch_frame`
  (`accessors_state.rs`) — and compares the sequence's own `(level, stored_idx)`-ordered,
  per-call-filtered, winner-fingerprint-removed candidate list against the real "remaining"
  list `resolve_all_methods_with_owner` + fingerprint dedup already computes there, under a new
  `DEFERRAL_SHADOW_CHECKS`/`_MISMATCHES` counter pair (list equality by fingerprint, exactly as
  the slice plan specifies) — a dedicated pair, not the shared `RESOLVER_SHADOW_*` infra, same
  reasoning as E7 steps 4/6/7's own dedicated pairs (comparing an ordered LIST, not a single
  winner pick). The winner side of "shadow-compare winner AND deferral list" needed no new
  code: per design decision 1 the winner ranking ladder is unchanged by `level`/`stored_idx`
  (deferral-order-only facts), so E4a's existing `shadow_check_resolver`/`_chain` at the
  `resolve_method_cached` boundaries already covers it, now exercising the enriched candidate
  shape for free. Two real findings surfaced building this, both fixed or bucketed honestly
  rather than force-fit to zero:
  1. **A real bug in the new probe itself, fixed**: the first version passed the call's
     invocant to the per-candidate signature match, but the REAL target
     (`resolve_all_methods_with_owner`) always calls `method_args_match_for_invocant` with
     `invocant: None` — the deferral list is invocant-BLIND (it never checks `:U:`/`:D:`
     smileys), matching raku's own `nextsame`/`callsame` walk. An invocant-aware shadow probe
     is stricter than its own target, not a shadow of it; every `::?ROLE:U:`/`::?ROLE:D:` multi
     pair in the sweep (`t/role-ud-multi-dispatch.t`, `t/multi-method-invocant-definedness.t`,
     `t/qualified-mu-coercion.t`) mismatched until the probe's `match_sequence_candidates` call
     switched to `invocant: None` to match.
  2. **A pre-existing, accepted divergence, documented not fixed**: `resolve_sequence`'s
     per-level lookup (`Registry::user_method_overloads`) silently returns nothing for a role
     owner that has never been *punned* — `Registry::method_entries` (the E1/E2 canonical
     table) is only ever populated from `self.classes`, and a role is not a key there unless a
     `RoleName.new` pun briefly registered (and later withdrew) a synthetic `ClassDef` for it.
     `resolve_all_methods_with_owner` has no such gap (it reads `self.registry().roles`
     directly), so it still finds a role's own un-flattened method the deferral probe misses.
     Root-caused and written up in full, including why this is not fixed inside E8a (it also
     feeds several REAL production dispatch paths — winner selection included — so populating
     it is a real-behavior change outside a shadow-only box's scope) and a suggested fix, in
     `todo/deep/method-entries-never-covers-unpunned-roles.md`.
  A `MUTSU_VM_STATS=1` sweep of the full local `t/` suite (3070 files) found 160 deferral-shadow
  checks across 46 files, 58 mismatches — every single one the shape `real_len` exactly one
  candidate ahead of `shadow_len`, confirmed by hand on every mismatching file
  (`t/anon-class-does-imported-role.t`, `t/builtin-distribution-role.t`,
  `t/callsame-punned-role-and-hyper-infix-sub.t`, `t/multi-udismiley-ambiguity-leak.t`,
  `t/qualified-method-call.t`, `t/role-conflict.t`, `t/role-required-method-name-based.t`,
  `t/role-required-universal-method.t`, `t/supply-nested-whenever-emitter.t`,
  `t/yaml-battery.t`) to be finding 2's gap, not a new bug. A roast slice touching multi/role/
  submethod/wrap dispatch (`S06-advanced/{callsame,dispatching,wrap}`, `S06-multi/{redispatch,
  type-based,syntax}`, `S12-methods/{defer-call,defer-next,lastcall,multi,parallel-dispatch}`,
  `S12-class/{mro-6c,inheritance,basic}`, `S14-roles/mixin-6c`, 16 files) found 37 checks, 0
  mismatches. Two new unit tests (`resolve_sequence_assigns_level_and_stored_idx`,
  `resolve_sequence_drops_a_flattened_role_duplicate_at_build_time`). `cargo build`/`cargo
  clippy -- -D warnings`/`cargo fmt --check` clean; `cargo test --lib` (812 tests) and full
  local `make test` (3070 files / 28652 tests) green. Zero real-behavior change: `resolve_
  all_methods_with_owner`, `push_method_dispatch_frame`'s own logic, and every real dispatch
  decision are untouched by this slice.
  **Progress 2026-08-12** (E8b, proto methods into `MethodEntry`, shadow mode): scoped down
  from the slice plan's full text ("`lookup_proto_method` deleted") to the same
  measure-first shape every prior Phase E box used before a cutover — E1a's `TypeId` column
  landing beside the still-authoritative string owner is the closest precedent. `MethodEntry`
  (`registry.rs`) gained a `proto: Option<FunctionDef>` column; `Registry::set_proto_method`
  (the single write site, called from `registration_class_body.rs`'s
  `class_body_proto_method_decl`) writes it alongside the still-standalone, still-sole-real-
  reader `proto_methods: HashMap<(String, String), FunctionDef>`. `Interpreter::
  lookup_proto_method`'s real MRO walk is untouched; it now also calls a new
  `MUTSU_VM_STATS`-gated probe, `shadow_check_proto_method`, which repeats the identical
  `class_mro` walk reading the new `method_entries`-backed `Registry::method_entry_proto`
  instead, and compares owner name + `FunctionDef::body_fingerprint()` against the real
  result under a dedicated `PROTO_METHOD_SHADOW_CHECKS`/`_MISMATCHES` counter pair
  (`vm_stats.rs`), following the same "own pair, not `RESOLVER_SHADOW_*`" reasoning as every
  prior probe family in this ADR.
  **One real bug found and fixed, in the registry's existing sync logic, not the new probe
  itself**: the first sweep found *majority* mismatches (e.g. 10/13, 12/19 checks), always
  `real=Some(owner) shadow=None` — the new column was silently losing its only entry.
  Root cause: `Registry::sync_user_method_entries` (pre-existing, called from every one of
  `registration_class_body.rs`'s own call sites *after* the proto decl already landed, plus
  composition/augmentation/redeclaration elsewhere) `retain`s a `(owner, name)` row only when
  `entry.builtin.is_some() || !entry.user_candidates.is_empty() || entry.accessor.is_some()` —
  a row holding only a freshly-written `.proto` (no builtin/user_candidates/accessor) matched
  none of those and was dropped from the map outright the next time anything synced that
  owner. Fixed by adding `entry.proto.is_some()` to the keep condition; `.proto` itself is
  deliberately NOT reset by the `key.owner == owner` clearing branch above it (unlike
  `user_candidates`/`accessor`, it has no `ClassDef`-backed source that branch re-derives
  from below — it is written once, directly, by `set_proto_method`). Confirmed zero real-
  behavior impact: nothing outside this box's own shadow probe read `.proto` before the fix,
  so the bug was entirely self-contained to the new, not-yet-consulted column. After the fix,
  a `MUTSU_VM_STATS=1` sweep of every `t/` file mentioning `proto method`/`proto submethod`
  (22 files) found 171 checks, 0 mismatches; a roast slice touching proto/multi/wrap dispatch
  (`S06-multi/{proto,type-based,syntax,redispatch}`, `S12-methods/{defer-next,defer-call,
  lastcall,multi,parallel-dispatch}`, `S06-advanced/{callsame,dispatching,wrap}`,
  `6.c/S12-class/mro-6c`, `S12-class/{inheritance,basic}`, `6.c/S14-roles/mixin-6c`, 16 files)
  found 24 checks (3 files actually exercise a proto method; the rest is coverage, not a
  gap), 0 mismatches. Two new unit tests (`set_proto_method_populates_both_the_legacy_table_
  and_method_entries`, `method_entry_proto_is_scoped_to_the_exact_owner`). `cargo build`/
  `cargo clippy -- -D warnings`/`cargo fmt` clean; `cargo test --lib` (814 tests) and full
  local `make test` (3071 files / 28661 tests) green. Zero real-behavior change:
  `lookup_proto_method`'s own return value, and every real proto-method dispatch decision,
  are untouched — `proto_methods` stays the sole table actually read for dispatch.
  **Progress 2026-08-12** (E8c, proto-method cutover): landed the deferred cutover exactly as
  E8b's own note scoped it, matching E1a→E1b's precedent — `Interpreter::lookup_proto_method`'s
  real MRO walk now reads `MethodEntry.proto` via `Registry::method_entry_proto` directly
  (`dispatch_proto.rs`), the same lookup E8b's shadow probe already proved agrees with the old
  walk everywhere tested (171+24 checks, 0 mismatches). `git grep -n "proto_methods" src/`
  confirmed the standalone `HashMap<(String, String), FunctionDef>` table had exactly the two
  readers E8b's own design already assumed (the real walk and its now-obsolete shadow probe,
  both in `dispatch_proto.rs`) and one writer (`Registry::set_proto_method`) — no `.^methods`/
  `.^lookup`/`.^find_method` introspection path or anything else touched it — so it is genuinely
  dead code, not merely superseded, and is deleted outright rather than kept as a secondary
  store. Its role as `lookup_proto_method`'s whole-program "skip the MRO walk entirely" fast path
  (`proto_methods.is_empty()`) is replaced by a new monotonic `Registry::has_proto_methods: bool`
  flag set once by `set_proto_method` (proto bodies are never unregistered, so a flag suffices —
  no count or set needed). The now-redundant `shadow_check_proto_method` probe and its
  `PROTO_METHOD_SHADOW_CHECKS`/`_MISMATCHES` counter pair (`vm_stats.rs`, including their
  `adr0019-e8b` stats-report lines) are deleted — once the shadow answer IS the real answer,
  there is nothing left to compare it against, the same reasoning E1b used to retire E1a's
  probes at its own cutover sites. Two registry unit tests updated for the new shape (one
  renamed and simplified to check `method_entries` + the fast-path flag instead of the retired
  table; the per-owner-scoping test unchanged). No new test added beyond that: this is a
  same-answer read-path swap with no new observable behavior to pin, and the existing proto-
  method suite (`t/proto-method-body.t`, `t/proto-method-rw-redispatch.t`,
  `t/proto-cross-module-invocant.t`, `t/handles-proto-dispatch-mut-invocant.t`,
  `t/proto-multi-captured-writeback-coherence.t`, `t/proto-new-no-match.t`,
  `t/proto-multi-method-role-composition.t`, `t/multi-udismiley-ambiguity-leak.t`,
  `t/role-ud-multi-dispatch.t`, `t/qualified-mu-coercion.t` — 72 assertions across plain-class,
  inherited, and role-composed proto shapes) already exercises the cut-over path end to end and
  stays green. Per CLAUDE.md's "touched name/type resolution" rule (this is a real dispatch
  read-path change, even though risk-free by measurement), ran a local roast slice touching
  proto/multi/wrap dispatch (`S06-multi/{proto,type-based,syntax,redispatch}`,
  `S12-methods/{defer-next,defer-call,lastcall,multi,parallel-dispatch}`,
  `S06-advanced/{callsame,dispatching,wrap}`, `6.c/S12-class/mro-6c`,
  `S12-class/{inheritance,basic}`, `6.c/S14-roles/mixin-6c`, 16 files, 524 assertions) — all
  green. `cargo build`/`cargo clippy -- -D warnings`/`cargo fmt --check` clean; `cargo test --lib`
  (814 tests, same count as E8b — one renamed, none added or removed) and full local `make test`
  (3074 files / 28683 tests) green. Zero real-behavior change confirmed by construction (E8b's
  own shadow measurement) and by the roast/`t/` sweeps above, not merely asserted.
  **E8 is now closed as a whole.** E8a (sequence structural fields: `level`/`stored_idx`,
  `drop_flattened_role_duplicate_candidates` at build time, the deferral-list shadow probe) +
  E8b (proto methods folded into `MethodEntry`, shadow mode, plus a real `sync_user_method_
  entries` bug fix) + E8c (this slice, the proto-method cutover) together cover everything the
  box's own text scoped: candidates carry `level`/`stored_idx` so winner selection and deferral
  order both derive from one sequence; `Registry::proto_methods` has folded into `MethodEntry`
  and is gone; unifying the method-vs-sub ranking ladders stayed explicitly out of scope, as
  designed. The one open item from E8a — `resolve_sequence`'s per-level lookup silently missing
  an un-punned role's own methods (`todo/deep/method-entries-never-covers-unpunned-roles.md`) —
  is a pre-existing, separately-tracked gap in a different lookup path (`user_method_overloads`,
  not anything E8c touched), not part of this box's scope. **Next Phase E box: E9-pre** — the
  mandatory raku verification campaign for `samewith`/`nextsame`/`callsame`/`nextwith` cursor
  semantics (design decision 3 in `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`),
  the highest-semantic-risk box of the whole phase (拙速厳禁) — it must run as its own dedicated
  session, not be attempted inline here, and is required before any E9a/b/c cursor cutover work
  starts.
- [ ] **E9 — Add resolver cursors for `samewith`/`nextsame`/`callsame`/`nextwith`.** Continue within
  the resolved sequence instead of re-entering name-based resolution.
  **Design 2026-08-10** (same doc): one `DispatchCursor {seq, next, invocant, args}` replaces
  the recomputed `MethodDispatchFrame.remaining` + fingerprint winner-removal; wrap chains
  become cursor-prefix entries (deleting the `sub_id == 0` sentinel, `wrap_skip_once`, and the
  by-name re-entries); the four synthesized native next-candidate fallbacks become ordinary
  sequence tail entries; proto `{*}` re-ranks the cursor's sequence instead of re-entering by
  name. **A mandatory raku verification campaign (E9-pre, 13 chain-order scenarios) lands as
  `t/` pins before any cursor cutover** — this is the highest-semantic-risk box of the phase.
  **Progress 2026-08-12 — E9-pre landed (docs + pins only, no cursor code).** Every scenario
  probed against Rakudo v2026.06 first; 12 new `t/` pins (38 assertions) each verified green
  under BOTH `prove -e raku` and mutsu, so the pins encode raku's answer; 8 divergences filed
  as tickets, none encoded into design. **Headline: the campaign falsified a design-2
  assumption** — when multi candidates span MRO levels, raku defers along the
  specificity-RANKED merged candidate list (implicit proto clones the nearest MRO proto;
  plain middle methods are later outer-chain entries that re-enter lower protos on deferral),
  not mutsu's `(level, decl-order)` walk. The cursor's sequence layout must be re-drawn
  against `todo/deep/defer-chain-ranked-multi-order.md` (design task) **before E9a starts**.
  Other divergence tickets: role-shadowed method wrongly in the chain, explicit child proto
  wrongly assuming parent candidates, `is Array` native-push fallback not pushing, method-wrap
  `unwrap`/`restore` no-op, `lastcall`-in-wrapper killing the dispatcher scope, callsame to
  native Mu methods (gist/Str/raku/new) yielding Nil/Any, and a cosmetic Signature.gist
  invocant format. Full scenario table in the E8-E11 design doc's E9-pre section.
  **Progress 2026-08-12 (same day) — decision 2 redrawn; E9a design-unblocked.** The re-draw
  landed in the same design doc ("E9 design decision 2 — REDRAWN"): the cursor sequence is a
  FLAT deferral expansion — per-MRO-class entries, each a plain method or that class's proto's
  specificity-ranked candidate block (implicit protos clone the nearest MRO proto and merge;
  explicit protos stand alone), with duplicate candidate occurrences across blocks being
  correct re-visit semantics. Confirmed by two exact-hit predictions against raku before any
  implementation. E9a is now a deliberate behavior-changing cutover (the old walker is wrong
  where the E9-pre tickets point) gated on new raku-valued pins + local `make roast`, with the
  matcher-strictness ticket (`multi-matcher-admits-int-for-num`) as prerequisite/co-requisite.
  That prerequisite was fixed the same day (the Int/Rat→Num "numeric widening" removed from the
  shared matcher and binder — `news/2026-08/multi-num-param-strictness.md`), so E9a's remaining
  blocker is only the cursor sequence-builder work itself.
  **Progress 2026-08-12 (same day) — E9a sequence-builder landed for the both-levels-multi-order
  shape.** `src/runtime/resolution_deferral.rs`'s `resolve_deferral_expansion` replaces
  `resolve_all_methods_with_owner` as the ordering source at both "remaining"-building call
  sites; both design-doc probes are now exact hits against Rakudo v2026.06 (not just
  predictions), pinned by `t/defer-multi-cross-level-proto-block.t`. Deliberately narrower than
  the full box: the winner-removal mechanism and `MethodDispatchFrame`'s `Vec`-based storage are
  unchanged (the `DispatchCursor{seq, next, invocant, args}` index-based rewrite is orthogonal
  perf/cleanliness work, left for a follow-up slice), and the role-shadow/explicit-proto-isolation
  divergence tickets remain open (distinct fixes, not implied by decision 2's redraw — see the
  design doc's 2026-08-12 progress note for detail). E9b (wrap-prefix) and E9c (proto `{*}`
  rewrite, `samewith`) are unstarted.
  **Progress 2026-08-13 — role-shadowed-method-in-defer-chain ticket fixed.** One of the two
  divergence tickets left open by the sequence-builder slice is resolved:
  `drop_flattened_role_duplicates` (`resolution_method.rs`) now also drops a `does`-composed
  role's raw MRO entry when a class-owned method of matching signature shadows it (not just when
  a flattened copy is present), so a role method the class overrides by name is fully excluded
  from the `nextsame`/`callsame` chain, matching raku. Distinguished from a role used as a
  *punned* class parent (`class Foo is R1`), which raku keeps as a genuine `.^mro` ancestor and
  which therefore must NOT be shadowed — caught immediately by the existing
  `t/callsame-punned-role-and-hyper-infix-sub.t` pin, which regressed on the first attempt before
  the punned-vs-composed distinction was added. New pin:
  `t/role-shadowed-method-in-defer-chain.t`. `explicit-child-proto-assumes-parent-candidates.md`
  and `native-array-push-defer-fallback-broken.md` remain open, as does the separate
  `method-entries-never-covers-unpunned-roles.md` production-dispatch gap.
  **Progress 2026-08-13 (same day) — native-array-push-defer-fallback-broken ticket fixed.** The
  second of the two divergence tickets is resolved: `nextsame`/`callsame` from a user-overridden
  `push`/`append`/`prepend`/`unshift`/`pop`/`shift` on an `is Array` subclass now reaches the real
  native array mutation instead of silently doing nothing. Two independent bugs in
  `native_array_storage_next_candidate` (`runtime/builtins_dispatch_next.rs`): (1) it routed
  through `try_native_method`, the PURE `&Value` native dispatch that has no entry for any
  mutating list method at all (the E6c sigil-only-routing precedent, again) — now routes the six
  mutators through `native_array_storage_mut` (promoted `pub(crate)` from
  `vm_call_method_mut_ops.rs`, the same helper the direct `$a.push(...)` fast path uses) via
  `with_attr_mut`'s `&mut Value` into the instance's SHARED attribute cell, so the mutation is
  visible to every other holder of the same instance; and (2) the common case — a single,
  non-multi, non-wrapped override — pushes NO `method_dispatch_stack` frame at all, so the
  original call args (`push(1)`'s `1`) had no carrier and silently defaulted to empty. Fixed by
  adding `samewith_call_args_stack`, a `Vec<Value>` stack pushed/popped in lockstep with
  `samewith_context_stack` by `push_method_samewith_context`/`pop_method_samewith_context`
  (`accessors_state.rs`), giving the fallback a place to recover the original args when no
  dispatch frame exists (also GC-rooted in `gc_roots.rs`). `push`/`append`/`prepend`/`unshift`
  additionally return the invocant itself (Raku's base `Array.push` semantics), not the raw
  backing array, so `callsame`'s return value now has correct identity (`===`) and subclass type.
  New pin: `t/native-array-push-defer-fallback.t` (16 assertions, raku-verified). Only
  `explicit-child-proto-assumes-parent-candidates.md` remains open from the E9-pre campaign;
  `method-entries-never-covers-unpunned-roles.md` stays a separate production-dispatch gap. Also
  noted, out of scope for this ticket: the DIRECT (non-deferred) `$x.push(1)` on a plain `is
  Array`-backed instance with no override has the SAME return-identity bug (returns the backing
  array, not `self`) — a pre-existing issue in `vm_call_method_mut_ops.rs`'s own fast path, filed
  separately.
  **Progress 2026-08-13 (same day) — explicit-child-proto-assumes-parent-candidates ticket
  fixed.** An EXPLICIT `proto method` declared on some class in the MRO now starts a fresh
  candidate set for its `{*}` redispatch: a new `Interpreter::proto_redispatch_boundary:
  Option<(Symbol, Symbol)>` field records `(method_name, owner_class)` for the proto currently
  governing a redispatch — `owner_class` is whichever class `lookup_proto_method`'s MRO walk
  actually found the explicit proto body on, set bracket-style (saved/restored, not a one-shot
  flag) around `call_method_with_values` in `dispatch_proto_call.rs` so a candidate that itself
  triggers a nested proto redispatch does not clobber the outer boundary. Both multi-candidate
  collection sites — `resolve_method_with_owner_impl`'s MRO walk (`resolution_method.rs`) and the
  `X::Multi::NoMatch` diagnostic's signature-listing walk (`class.rs`) — truncate the MRO at that
  owner when the boundary names the method being resolved, so an ancestor's candidates beyond the
  proto's own declaring class are invisible to the redispatch, matching raku exactly (verified:
  `class P { multi method m(Int $x) {...} } class C is P { proto method m($x) {*}; multi method
  m(Str $x) {...} }; C.new.m(5)` raises `X::Multi::NoMatch` in both raku and mutsu now, was
  silently resolving `P`'s candidate before). The inverse direction — an implicit child proto (no
  proto written in the child) inheriting and merging a parent's explicit proto's candidates — is
  untouched (the boundary is only set when `lookup_proto_method` names an owner, and a purely
  implicit case never enters this branch) and stays green on the existing
  `t/proto-star-cross-mro-candidates.t` pin. New pin: `t/proto-explicit-child-fresh-candidates.t`
  (4 assertions, including a mid-MRO case where the governing proto is neither the receiver's own
  class nor the ultimate ancestor, to confirm the boundary tracks the actual declaring class, not
  just "the receiver"). `cargo build`/`clippy -D warnings`/`fmt` clean; targeted `t/proto-*.t
  t/multi-*.t` (61 files, 541 assertions) and full local `make test` (3108 files, 28857 tests)
  green; local roast slice (`S06-multi/{proto,redispatch,syntax,type-based}.t`,
  `S12-methods/{defer-call,defer-next,multi}.t`, `S06-advanced/{callsame,dispatching,wrap}.t`, 10
  files, 334 assertions) green. **Both E9-pre divergence tickets are now closed**; the separate
  `method-entries-never-covers-unpunned-roles.md` production-dispatch gap remains open. E9b
  (wrap-prefix) and E9c (proto `{*}` cursor rewrite, `samewith`) are still unstarted.
- [ ] **E10 — Move wrap/unwrap mutation into canonical entries.** Bump the generation and remove
  wrap-specific cache-clearing paths.
  **Design 2026-08-10** (same doc): `method_wrap_chains` moves into the registry; every
  wrap/unwrap/restore path — including the two that currently invalidate nothing — bumps
  `method_generation`; the global `has_any_wrap_chains()` prefilter (which disables the fast
  cache program-wide once any wrap exists) is deleted after E3; the `.unwrap` method-wrap leak
  is fixed en route.
- [ ] **E11 — Retire arity-specific lookup entry points.** Keep native arity functions only as
  handler implementations selected by `MethodEntry`.
  **Design 2026-08-10** (same doc): grep-based completion criterion — no caller of
  `native_method_{0,1,2}arg` outside the resolver's native-invocation helper, `builtins/`
  internal recursion, and `#[cfg(test)]`; added to the G2 architectural guard test.

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
