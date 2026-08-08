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

**Current progress: 28/53 slices merged (C6, C7, C8, D1, and D2d complete; D2a and D2c-1/2/3 also
landed, 2026-08-07). Phase C is fully checked; the open box is D2 (attributes and generated
accessors), subdivided D2a-D2d — D2a, D2c-1/2/3, and D2d are done; D2b (typed attribute
descriptors) and the "compile `default`/`where_constraint` as actual bytecode chunks" remainder of
D2c are the only pieces still open, followed by D3 (class methods/submethods as compiled
candidates). D1 found most class structural data already typed-plan-driven
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
- [ ] **D4 — Compile class declaration-time expressions.** Cover computed names, traits, parent
  expressions, aliases, and deferred class bodies through re-entrant bytecode chunks. (Computed
  names and custom-trait arguments already landed with C5; parents, aliases, and deferred bodies
  remain.)
- [ ] **D5 — Drive user HOW operations from plan ops.** Execute `new_type`, `add_method`, trait
  interception, and `compose` without entering `register_class_decl`'s AST walker.
- [ ] **D6 — Remove `CompiledClassDeclPlan::legacy_body`.** Preserve augmentation, rollback,
  redeclaration errors, language revisions, nested types, and EVAL behavior. Excludes the
  token/rule arms (see the phase preamble). Start with the C6d-style instrumentation survey —
  C6's one box became nine PRs, and this field has the same shape.
- [ ] **D7 — Encode role structure and composition.** Put role parameters, attributes, methods,
  parent roles, conflicts, hides, and pun metadata into immutable plan operations.
- [ ] **D8 — Compile role declaration-time bodies and traits.** Run parameterized-role and composed
  ancestor bodies as bytecode child chunks with correct once-per-composition behavior. (Custom-trait
  arguments already landed with C5; the bodies remain.)
- [ ] **D9 — Remove `CompiledRoleDeclPlan::legacy_body`.** Preserve role puns, runtime mixins,
  conflicts, BUILD/TWEAK, custom HOWs, and EVAL. Same rule as D6: survey first, token/rule arms
  excluded.
- [ ] **D10 — Delete class/role AST registration walkers.** Keep only VM plan execution plus
  metadata helpers that do not inspect executable AST declarations. The token/rule arms of the
  body walk stay until their ADR-0009-scoped slice lands; D10 deletes everything else.

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
