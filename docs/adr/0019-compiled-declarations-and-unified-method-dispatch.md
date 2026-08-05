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

**Current progress: 17/53 slices merged. Next slice: C6d-4 (`call_sub_value`'s
`eval_block_value(&data.body)`), or C6d-3's Phase-C half — C6d-1 is complete (C6a, C6b and C6c
landed; C6 and C6d are both subdivided below, C6d from a measurement of where its sites are
actually reached).**

The count tallies top-level boxes only; sub-boxes (C6a–C6e, C6d-1..4, E1a/E1b) are that box's
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
- [ ] **C6 — Remove `CompiledSubDeclPlan::legacy_body`.** Make ordinary, multi, `our`, hoisted,
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
    `module_otf_needs_interpreter` / `declares_state` as `RoutineBodyFacts` on the def, behind the
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
  - [ ] **C6d — interpreter execution sites.** Route `eval_block_value(&def.body)` /
    `run_block(&def.body)` through `def.compiled`. Surveyed by instrumenting all six sites and
    running the whole `t/` suite once (1148 hits — see
    `todo/deep/c6d-interpreter-body-sites-are-mostly-token-bodies.md`), which corrects the
    earlier "widen OTF coverage to every routine" framing: a routine whose body declares a class
    never reaches these sites, and the sites do not tree-walk — `run_block` recompiles the body
    per call, so what C6d removes is a repeated compile. The mismatch to solve is that
    `compile_block_raw` compiles a *block* whose arguments the caller already bound, while
    `def.compiled` binds its own from `param_local_slots`. Subdivide:
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
    - [ ] **C6d-3 — the two sites dead across the whole suite**
      (`dispatch_proto_call.rs:call_proto_dispatch`, `types/roles.rs:run_role_submethod`); the
      latter's `def` is a `MethodDef`, so move it to Phase D.
    - [ ] **C6d-4 — `call_sub_value`'s `eval_block_value(&data.body)`**, which takes a *code
      object's* body rather than a def's: after C6c that is the one path left that still
      executes a routine code object's AST, reached when a `.wrap` chain routes dispatch through
      the interpreter carrier.
  - [ ] **C6e — redeclaration comparison and eager body facts, then drop the plan field.**
    Replace the two `body_debug_without_setline(&def.body)` comparisons (`registration_sub.rs`)
    with the plan's C4 redeclaration fingerprint, and fill `RoutineBodyFacts` eagerly at plan
    lowering — the C6b cache is lazy and still reads `def.body` on a miss, which a body-less def
    cannot serve. The proto `{*}` rewrite moved to C8: a proto def is built from `stmt_pool` by
    `RegisterProtoSub`, not from the sub plan, so it does not gate this field.
- [ ] **C7 — Remove the sub-registration AST adapter.** Delete dead sub-shaped walker branches and
  prove the routine registry never compiles a migrated declaration on demand.
- [ ] **C8 — Proto declarations register from typed plans.** Migrate `RegisterProtoSub` and
  `RegisterProtoToken` off `stmt_pool`, and compile the `{*}` dispatch instead of rewriting the
  proto's AST body per call (`rewrite_proto_dispatch_stmts` plus the `proto_def.body = rewritten`
  reassignment in `vm_call_func_ops.rs`). Independent of C6/C7; the slice exists because
  Decision §1's "only general declaration-registration opcode" end state is unreachable while
  these two opcodes still index the statement pool.

### Phase D — class and role plans become bytecode-native

Two facts about the current walkers shape this phase. First, `register_class_decl` is a single
~2,500-line function (`registration_class_decl.rs`) whose concerns share about eight mutable
locals, and `register_role_decl` (~920 lines, `registration_role.rs`) has the same shape — the
D1–D6 cut lines run *through* those function bodies, not between functions, so D0 exists to make
the later slices independently landable. Second, the class walker also registers grammar
`token`/`rule` declarations, whose bodies are regexes: like C6d-2 they are scoped against
ADR-0009, so D6, D9, and D10 exclude the token/rule arms until that work lands — deleting the
walkers wholesale is not possible before then.

- [ ] **D0 — Split the class/role walkers into named phases with no behavior change.** Extract
  `register_class_decl`'s sections (rollback snapshot, parent validation, role composition,
  punning, attribute pre-scan, body walk, custom-HOW install) and `register_role_decl`'s three
  passes into functions with explicit inputs, so D1–D9 replace one function at a time.
- [ ] **D1 — Encode class structural operations.** Put package open/reopen, parent edges, repr,
  visibility, lexical/package aliases, and source-order metadata in immutable plan operations.
- [ ] **D2 — Encode attributes and generated accessors.** Compile defaults/constraints as child
  chunks and publish generated methods through the canonical table.
- [ ] **D3 — Encode class methods and submethods as compiled candidates.** Install ordinary, multi,
  proto, private, rw, wrap, BUILD, and TWEAK metadata without walking `Stmt::MethodDecl`. That
  walk exists in three places, not one — the class walker (~508 lines), the role walker
  (~263 lines), and augmentation (`registration_class_augment.rs`) — plus a fourth reader in
  `registration.rs`; expect this box to subdivide per walker (class/role/augment).
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
the plan, and the VM no longer pattern-matches `Stmt::SubDecl`. The plan still carries
`legacy_body` for the existing routine-registry adapter. Source-order sub plans now also carry the
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
