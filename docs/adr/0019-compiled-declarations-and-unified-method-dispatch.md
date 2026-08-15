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

**Current progress:** Phases A, B, and C are fully closed. Phase D is closed except for the
optional, low-priority D2c-5. Phase E is closed except E2 (still-open cleanup, no longer gating —
E1, E3-E11 are all closed). Phase F has started: F5 is closed; F1-F4, F6, F7, and the completion
gates (G1-G4) remain open. See each box's entry below for its own status, and
`todo/deep/adr0019-*.md` for the underlying design docs — `d2-remainder-attr-plan-lowering.md`,
`d4-parent-expr-chunks.md`, `d5-plan-driven-how-ops.md`, `d6-d9-legacy-body-removal.md`,
`d7-d8-role-plan-encoding.md`, `d3-8-method-body-main-pass-compilation.md` for Phase D, and
`e1-typeid-receiver-owner.md`, `e2-e4-resolver-core.md`, `e5-e7-entry-routing.md`,
`e8-e11-candidate-sequence-semantics.md` for Phase E.

Sub-boxes (e.g. C6a-e, D2a-d, E1a-c) are that top-level box's own PRs; a box is checked when its
last sub-box merges. A box that turns out to need subdivision follows C6's precedent: measure
first, then split in place.

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
  The blocker was `FunctionDef.body` (58 readers when C6 started), so the box split into
  sub-slices C6a-e, each landing independently:
  - [x] **C6a-c** — memoized body-fingerprint hashing, `RoutineBodyFacts`/OTF-gate predicate
    precompute, and `SubData::compiled_routine` (code objects built from a registry routine now
    carry that routine's bytecode directly). `news/2026-08/routine-code-objects-are-bytecode-backed.md`.
  - [x] **C6d** — every interpreter execution site that ran a routine or code object's AST body
    now runs its compiled bytecode instead (ordinary routines, the proto `{*}` fallback, the
    `.wrap`-chain carrier, `call_function_fallback`); `call_function_def` was deleted in favor of
    one shared `call_routine_def` entry point. See `news/2026-08/multi-deferral-runs-the-compiled-candidate.md`,
    `news/2026-08/user-operators-run-their-compiled-body.md`,
    `news/2026-08/statement-calls-run-the-compiled-body.md`,
    `news/2026-08/routine-code-object-carrier-runs-bytecode.md`,
    `news/2026-08/proto-star-fallback-runs-compiled-candidate.md`,
    `news/2026-08/fallback-def-arm-runs-compiled-body.md`.
    - [ ] **C6d-2 — grammar token/rule bodies** stay interpreter-executed; that is ADR-0009's
      execution model, not this box's. Tracked until the token/rule work lands, then closed
      together with F7.
  - [x] **C6e** — redeclaration comparison switched to the C4 plan fingerprint, `RoutineBodyFacts`
    filled eagerly at plan lowering, and `legacy_body` itself dropped (2026-08-07) once every
    remaining consumer (class-walker method-nested subs, rw/raw scalar carriers, lvalue routines,
    NativeCall marshalling, runtime-resolved sub names, bare-block/closure `SubData`) had its own
    bytecode carrier — validated with a `MUTSU_FORCE_BODYLESS` instrument against the full `t/`
    suite and roast whitelist. `news/2026-08/legacy-body-field-dropped.md`.
  This closes ADR-0019's C6 box except for the token/rule carve-out (C6d-2).
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
  chunks and publish generated methods through the canonical table. Attributes had **no** existing
  typed-plan coverage before this box (unlike D1's structural fields): four independently-drifted
  `Stmt::HasDecl` destructuring sites, a special-cased runtime accessor lookup, and six raw
  `eval_block_value` default-evaluation sites, none touching `CompiledDeclExpr`. Subdivided:
  - [x] **D2a — Precompute body pre-scan facts.** `own_attribute_names` (class) and the combined
    attribute/`use`/declared-type scan (role) moved to the compiler as plan fields.
    `news/2026-08/d2a-attribute-prescan-plan-fields.md`.
  - [x] **D2b — Type full attribute descriptors.** `CompiledAttrDecl`, a typed mirror of
    `Stmt::HasDecl`'s 18-field surface, replaces the four independently-drifted destructures
    (`class_body_has_decl`/`role_body_has_decl`/the augment arm/`RuntimeHasDeclSpec`). D2b-2
    (2026-08-08) moved construction to plan-lowering time as a **name-keyed**
    `attr_decls: Vec<(Symbol, CompiledAttrDecl)>` on both class and role plans — name-keying
    sidesteps a genuine registration-vs-compiler traversal-order mismatch for nested-sub `has`
    declarations that blocks position-keying. `news/2026-08/d2b-compiled-attr-decl.md`.
  - [x] **D2c — Compile defaults/constraints as child chunks.** `is_default`/`default`/
    `where_constraint`, plus the three `Expr`-valued role registry tables, all run through
    `CompiledAttrDecl`/`DeclTraitArg` and `eval_decl_trait_arg` instead of raw `Expr` +
    `eval_block_value` (D2c-1..4, landed 2026-08-07/08). Verification found and fixed two real
    bugs: `run_decl_expr` was missing the topic (`$_`) save/restore the `Ast` path already had
    (#6071), and a `Compiled`-chunk "value block" fed through `call_compiled_closure` silently
    returned `Nil` — fixed with a `SubData::is_decl_expr_thunk` marker that routes such thunks
    through a shared `run_decl_code` helper instead of the ordinary closure-call convention.
    - [ ] **D2c-5 (optional)** — collapse the three near-duplicated default-evaluation env-setup
      shapes (class walker, default ctor, `dispatch_new`) into one; gated on raku-verifying shape
      B's `has_class_scoped_subs` special case first. Not started, low priority.
  - [x] **D2d — Publish generated accessors through the canonical table.** `MethodEntry` gained an
    `accessor` arm so the hot per-dispatch-call point lookups (`has_public_accessor`,
    `resolve_user_method_or_accessor`) probe the table instead of scanning `ClassDef::attributes`.
    **Deliberately closed without migrating `.^methods`/`.^can`/`.^attributes` synthesis**: those
    sites *enumerate* every method/attribute for full introspection metadata rather than doing a
    point lookup, and `class_def.methods`/`.attributes` vs. `MethodEntry` are already a single
    source of truth kept in lockstep by `sync_user_method_entries` — reading one over the other
    there is a lateral move, not a gain (see CLAUDE.md's "what gain and risk actually mean").
  Full slice-by-slice history and the D2b/D2c-remainder design are in
  `todo/deep/adr0019-d2c-attribute-default-chunks.md` and
  `todo/deep/adr0019-d2-remainder-attr-plan-lowering.md`.
- [x] **D3 — Encode class methods and submethods as compiled candidates.** Install ordinary, multi,
  proto, private, rw, wrap, BUILD, and TWEAK metadata without walking `Stmt::MethodDecl`. Three
  independent walkers (class ~508 lines, role ~263, `augment class` ~105) each hand-built a
  `MethodDef` from its own destructure, with confirmed drift: `augment_class` dropped
  `deprecated_message`, stored the raw `is_my` flag instead of `is_submethod`-gating, skipped
  privacy-aware duplicate detection, and had no `handles` forwarder synthesis, BUILD/TWEAK
  `:$!attr` validation, custom-trait dispatch, or `is export` handling.
  **D3-1..6 (2026-08-07/08)** unified all three walkers onto one `CompiledMethodDecl::from_stmt`
  constructor (mirroring D2b's `CompiledAttrDecl` precedent) and then fixed every confirmed drift
  point by porting the class walker's logic verbatim to `augment_class`. Two bugs found along the
  way turned out to be pre-existing in the class walker itself (not augment-specific), filed
  rather than fixed here: `todo/tickets/method-is-export-non-operator-name-does-nothing.md` and
  `todo/tickets/method-typed-trait-mod-is-dispatch-never-matches.md`; a shared `handles *`
  vs. built-in `Cool`/`Any` method precedence bug is
  `todo/tickets/wildcard-handles-loses-to-builtin-cool-methods.md`.
  **D3-7..9 (2026-08-08/09)** moved method *body* compilation itself into the compiler's main pass
  (mirroring C1-C4 for subs) instead of a throwaway `Compiler::new()` spun up at every
  registration — `compile_method_body` compiles once at plan-lowering time, keyed by a
  per-declaration `compiled_routine_key`, with a params-equality guard that degrades to the old
  runtime fallback on mismatch. `method_body_runtime_compiles` (a `MUTSU_VM_STATS` counter)
  dropped to zero for ordinary class/role methods. A real, general package-resolution bug for a
  class/role declared inside *any* closure (not just the narrower `subtest`-block case first
  caught) was found and fixed en route: `qualified_class_decl_name`/`qualified_role_decl_name` now
  resolve through `self.enclosing_package` instead of the synthetic STATE-SCOPE marker. The only
  residual cost is `subtest NAME => {...}` called as an ordinary function still recompiling its
  block from AST on every call — out of D3's scope, filed as
  `todo/tickets/subtest-recompiles-block-from-ast-every-call.md`.
  This closes ADR-0019's D3 box: no registration-time code matches a raw `Stmt::MethodDecl` (or
  re-scans its body) to decide what to install, outside the `stmt_pool`-fed augment walker's own
  one-shot construction and the private-access validator's generic body recursion.
- [x] **D4 — Compile class declaration-time expressions.** Cover computed names, traits, parent
  expressions, aliases, and deferred class bodies through re-entrant bytecode chunks. (Computed
  names and custom-trait arguments already landed with C5.)
  A 2026-08-08 scoping pass found the box's four named pieces are three different problems:
  **aliases** (`:ver`/`:auth`/`:api`) already compile as an ordinary sibling statement with no
  `legacy_body` walk, so folding them into a formal plan field would be a lateral move — closed
  as-is; **deferred class bodies** is not class-specific at all, it is `RoleDef::deferred_body_stmts`
  consumed once per composition — the same work D8 already scopes, so D4 needs no separate slice
  there; and **parent expressions** (`is Parent[Args]`/`does Role[Args]`/`hides Parent[Args]`) is
  the one real gap: bracket content was captured as raw balanced-bracket source text and
  re-parsed + re-evaluated on every registration (every loop iteration, every re-run of a
  `for`/`while`-declared class, every EVAL).
  **D4-1/2/3 (2026-08-08)** fixed the parent-expression gap: the parser now also captures bracket
  arguments as `Vec<Expr>` (D4-1, additive, string path unchanged), the compiler lowers them to
  `DeclTraitArg` chunks on the class plan (D4-2), and `resolve_role_candidate_with_args` consumes
  the pre-evaluated values at the class-header call site (D4-3) — verified against an 8-case raku
  table, byte-identical, and incidentally fixing a real `R["a,b"]`-comma-in-string parse failure
  the old string path had. Root-causing D4-3 also found and fixed a real, independent memory-
  aliasing parser bug (a pointer-keyed bracket-expression memo let two sibling `does R[X] does
  R[Y]` clauses on one header alias each other's parsed args after the first's buffer was freed) —
  fixed by returning a slice of the persistent source buffer instead of an owned copy, pinned by
  `role-double-parametric-args-distinct.t`. A same-role-composed-twice multi-dispatch bug found
  during the same investigation is unrelated and filed as
  `todo/tickets/same-role-composed-twice-multi-dispatch-picks-one-candidate.md`.
  A same-day, unrelated finding: `also does Role[Args];` *inside* a class body silently dropped
  its bracket arguments and skipped the ~200-line role-attribute-carryover machinery the
  header form performs — a plain correctness bug, independent of this migration, fixed by
  routing the body form through the shared composition path. `news/2026-08/also-does-parametric-role.md`.
  **Closed 2026-08-09.** Every named piece is accounted for: computed names/custom traits (C5),
  parent expressions (D4-1/2/3), aliases (closed as-is), deferred class bodies (D8). Two
  deliberate residuals stay open as their own tracked items: `methods_qualified.rs`'s
  string-only `resolve_role_candidate` call (genuinely dynamic runtime-built type names with no
  source `Expr` to carry — a permanent exception, same shape as D10's augment carve-out), and
  `registration_role_body.rs`'s `concretized_parent` lookup, which still double-evaluates a
  role-body `does`'s bracket arguments once for that lookup and once for composition (collapsing
  it is a real behavior change for a side-effecting argument, deliberately deferred).
- [x] **D5 — Drive user HOW operations from plan ops.** Execute `new_type`, `add_method`, trait
  interception, and `compose` without entering `register_class_decl`'s AST walker.
  **Design pass (2026-08-08)** found the user-HOW protocol (`new_type`/`add_method`/`compose`)
  runs entirely *after* native registration and reads the finished registry, never a raw `Stmt`
  — so D5 is not itself a migration. It shrinks to two things:
  **D5-1 (2026-08-09, docs only)** codifies the ordering invariant every plan-driven registration
  step (D1-D10) must preserve: shell publish → body registration (direct, per-statement registry
  writes — never batched into a private snapshot clobbered only at the end, since user code in a
  trait can `^add_method` mid-registration and observe the same walk still populating it) → HOW
  instantiate (before trait dispatch, so a custom HOW sees every trait) → `new_type` → `add_method`
  (reads the *finished* registry — automatically correct once the registry matches, which is why
  D5 needed no independent migration) → class `trait_mod:<is>` → `compose` (must observe every
  prior step's side effects). Every HOW install keys on the resolved storage name, never the
  plan's static declared name.
  **D5-2 (2026-08-09, verification only)** re-ran the box's completion criterion once D6/D9/D10
  had all landed: `scripts/battery-testsuite.sh` (OO::Monitors, the `EXPORTHOW::DECLARE`-based
  `monitor` declarator's own acceptance bar) and every whitelisted metamodel-adjacent roast file,
  all green — confirming empirically that the user-HOW protocol behaves identically with the
  registry populated by D1-D10's plan-op execution as it did under the retired AST walk. This
  closes ADR-0019's D5 box.
- [x] **D6 — Remove `CompiledClassDeclPlan::legacy_body`.** Preserve augmentation, rollback,
  redeclaration errors, language revisions, nested types, and EVAL behavior. Excludes the
  token/rule arms (see the phase preamble).
  **Survey + design (2026-08-08)**: a typed ordered `body_plan: Vec<ClassBodyOp>` lowered at
  compile time, whose `Other` arm carries a per-statement `CompiledDeclExpr` chunk replacing
  `class_body_other_stmt`'s per-registration OTF compile — the driver keeps its exact env-seeding
  / BEGIN-swallowing / writeback / re-publish structure, only the statement source changes. Two
  freebies found: the redundant `TrustsDecl` walk arm (already covered by D1's plan field) was
  deleted, and `persist_class_body_statics`'s body re-scan became a `declared_static_names` plan
  fact (D6-1).
  **D6-3a-e (2026-08-09)** built `body_plan` additively (each `ClassBodyOp` arm — `Attr`/`Method`/
  `Does`/`ClassSub`/`CodeAlias`/`ProtoMethod`/`LeavePhaser`/`Other` — precompiled its chunk),
  wired it in behind a `MUTSU_DROP_LEGACY_CLASS_BODY=1` instrument (D6-3d), then flipped the
  default (D6-3e). Wiring the instrument surfaced two real, previously-invisible compiler bugs in
  the "purely additive" chunks: **(1)** `LeavePhaser`'s chunk had compiled the *wrapping*
  `Stmt::Phaser` statement (which compiles to nothing) instead of the phaser's own inner body —
  fixed and pinned by `class_declarations_leave_phaser_chunk_compiles_inner_body`; **(2)** every
  D6-3b/c chunk qualified bare variable/sub names against the *enclosing* compiler's ambient
  package instead of the class's own name (unlike `compile_method_body`, which already sets it
  explicitly) — a top-level `no strict; class Foo { $foo = 42; }` wrote an unqualified global
  instead of `Foo::foo`, caught by the pre-existing `t/strict-use-and-eval.t` — fixed by threading
  `package_name` through and pinned by
  `class_declarations_other_chunk_qualifies_against_declaring_class`.
  **D6-4 (2026-08-09)** dropped `CompiledClassDeclPlan::legacy_body` itself: `run_class_body` now
  iterates `body_plan` directly with no separate raw-`Stmt` list to zip against. Verified via the
  full `t/` suite, the class/role/grammar roast files, `scripts/battery-testsuite.sh`, and a hand
  comparison against `raku` exercising every `ClassBodyOp` variant. This closes ADR-0019's D6 box.
- [x] **D7 — Encode role structure and composition.** Put role parameters, attributes, methods,
  parent roles, conflicts, hides, and pun metadata into immutable plan operations. Deliberately
  narrower than the box text sounds: candidate selection, trial binding, specificity, conflict and
  required-method detection, and pun materialization read the *registry*, not the AST, and stay
  runtime — only the declaration's own structure becomes plan data.
  **D7-1 (= D9-1, 2026-08-08)** gave the role plan the class side's missing `is_stub`/
  our-scope-violation twins. **D7-2** was already delivered by D2b-2 landing on both class and
  role plans at once. **D7-3 (2026-08-08)** replaced the `__mutsu_role_hides__`/
  `__mutsu_role_hidden__` string-marker encoding with a typed `parent_ops: Vec<RoleParentOp>`,
  read by position instead of string-matching the marker names, and threaded D4's argument chunks
  into the role-body `does` site's own parametric resolution. **D7-4 (2026-08-09)** added a
  `body_plan: Vec<RoleBodyOp>` (`Attr`/`Method`/`Parent`/`Deferred`) mirroring `walk_role_body`'s
  own dispatch, closing D7 in full.
- [x] **D8 — Compile role declaration-time bodies and traits.** Run parameterized-role and composed
  ancestor bodies as bytecode child chunks with correct once-per-composition behavior. Unit of
  compilation is **one chunk per deferred statement**, not per body, so the five consumer sites'
  per-statement package routing, lexical-persistence scan, and `X::Role::Instantiation` wrapping
  keep their exact env dance and only swap `run_block_raw(stmt)` for a compiled-chunk run.
  **D8-1 (2026-08-09)** added `deferred_body_ops: Vec<DeferredBodyOp>` (`{kind, chunk,
  declared_vars, raw}`), one op per `RoleBodyOp::Deferred` entry from D7-4, compiling a chunk for
  `TypeDecl`/`Plain` statements (package-known) but not `TokenRule` (ADR-0009 carve-out, same as
  D6/D9's class-body exclusion). **D8-2 (2026-08-09)** cut every consumer over to the precompiled
  chunk. Verification found and fixed three real bugs: a `Plain` statement's true package is
  ambient at the *composition* call site, not knowable at role-declaration compile time (so only
  `TypeDecl` gets a chunk, caught by `t/generics-nominalizable-class.t`); a role's `__hoisted`
  forward-reference shell is NOT a throwaway stub the way a class's is, so gating on
  `is_hoisted_shell` left `deferred_body_ops` permanently empty for every top-level role (caught
  by `t/indirect-declarator-names.t`); and `RoleBodyOp::Deferred`'s catch-all also matched
  `SetLine`/stub markers the runtime walk never defers, spuriously entering composition and
  corrupting a role type parameter (caught by `t/role-double-parametric-args-distinct.t`).
  **D8-3 (2026-08-09)** moved `run_role_submethod` (the C6d-3 leftover, BUILD/TWEAK on a
  `does`/`but`-composed non-`Instance` value) onto its precompiled chunk. Verification surfaced
  two pre-existing, unrelated bugs in this composition path, confirmed identical before/after so
  not regressions: `todo/tickets/role-submethod-array-hash-attr-key-mismatch.md` and
  `todo/tickets/role-submethod-runtime-does-parameterized-value.md`. **D8-4 (2026-08-09)** dropped
  `RoleDef::deferred_body_stmts` outright (write-only since D8-2). This closes ADR-0019's D8 box.
- [x] **D9 — Remove `CompiledRoleDeclPlan::legacy_body`.** Preserve role puns, runtime mixins,
  conflicts, BUILD/TWEAK, custom HOWs, and EVAL. Same rule as D6: token/rule arms excluded. D9-1
  (= D7-1) and D9-3 (= D7-3) were delivered by those same slices; D9-4 is D8's chunk work.
  **D9-5 (2026-08-09)** dropped `CompiledRoleDeclPlan::legacy_body` in one PR — unlike the class
  side, `body_plan: Vec<RoleBodyOp>` had sat purely additive with zero non-test consumers, so this
  went straight from "additive" to "sole driver, field dropped." Verified via the full `t/` suite,
  the whitelisted class/role/grammar roast files, `scripts/battery-testsuite.sh`, and a hand
  comparison against `raku`. Verification also surfaced a real, pre-existing, unrelated
  divergence — mutsu accepts an `our`-scoped role attribute (`our $.x` inside a role body) instead
  of raising raku's `X::Declaration::OurScopeInRole` — filed as
  `todo/tickets/role-our-scoped-attribute-not-rejected.md` rather than fixed here. This closes
  ADR-0019's D9 box.
- [x] **D10 — Delete class/role AST registration walkers.** Keep only VM plan execution plus
  metadata helpers that do not inspect executable AST declarations. The token/rule arms of the
  body walk stay until their ADR-0009-scoped slice lands; D10 deletes everything else.
  **Closed 2026-08-09 by amending the completion criterion.** After D6-4/D9-5 the walkers *are*
  the plan-op executors; the only remaining raw-`Stmt` reads are each typed `ClassBodyOp`/
  `RoleBodyOp` arm's own `raw: Stmt` field, used for one-shot payload extraction (e.g.
  `ProtoMethod`'s param defs, `LeavePhaser`'s inner body) once the op's kind is already known by
  the compiler-computed tag — not AST-shape *dispatch*. This is architecturally identical to C6's
  own accepted precedent (`FunctionDef.body` surviving as an opaque interpreter-fallback payload).
  The corrected completion criterion: **no AST-shape dispatch in the class/role registration
  path** — no code that pattern-matches a raw `Stmt` to decide what kind of declaration it is or
  what to do with it — outside token/rule routing and the `stmt_pool`-fed augment walker. Under
  that reading, D10 is satisfied: `run_class_body`/`walk_role_body` classify nothing themselves
  any more, dispatching entirely on the precomputed op tag. Two of the six named payload reads are
  cheap boolean decisions rather than pure extraction and could be precomputed to slightly harden
  the invariant further; filed as
  `todo/tickets/adr0019-d10-precompute-stub-and-swallow-flags.md` as optional follow-up, not a new
  ADR box.

### Phase E — one dispatch resolver and native handler table

Phase E depends on B and may proceed alongside C/D only where it does not touch their adapters.
The receiver-identity slice comes first because a reverted earlier handler-ID attempt showed that
`value_type_name()` is not a dispatch owner: type objects appeared as `Package`, user Array
subclasses as `Any`, and representation aliases such as `Map` need explicit handling.

The resolver must cover every entry: six opcodes (`CallMethod`, `CallMethodMut`,
`CallMethodDynamic`, `CallMethodDynamicMut`, `HyperMethodCall`, `HyperMethodCallDynamic`), the
non-opcode entries (`vm_call_method_with_values`/`vm_call_method_mut_with_values`, the
`vm_run_instance_method` carrier, the two JIT shims, three `vm_call_helpers` fallback entries),
and the `ArrayPush` fast-path opcode that bypasses method dispatch entirely. The detailed designs
for this phase are `todo/deep/adr0019-e1-typeid-receiver-owner.md` (E1),
`todo/deep/adr0019-e2-e4-resolver-core.md` (E2/E3/E4),
`todo/deep/adr0019-e5-e7-entry-routing.md` (E5/E6/E7), and
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md` (E8/E9/E10/E11) — consult these for
full slice-by-slice history; the checklist below keeps only the architectural outcome.

- [x] **E1 — Introduce stable `TypeId` and receiver-owner resolution.** Resolve concrete values,
  type objects, user classes, builtin subclasses, role mixins, and representation aliases to an
  ordered TypeId MRO without initialization probes or per-call string scans. The owner decision
  used to live at ~20 sites in 7 files, including 14 per-MOP-entry fallbacks.
  **Design (2026-08-10):** `TypeId` is a newtype over `Symbol`; one static `BuiltinTypeInfo`
  catalog — adjudicated against real `raku`, not the union of the old tables — replaces four
  divergent builtin MRO tables; one classifier (`receiver_dispatch_class`/`dispatch_mro`) produces
  the ordered chain plus definedness plus a dispatch-shape hint.
  - [x] **E1a — shadow mode (2026-08-10).** Landed the `TypeId`/catalog/classifier with zero
    behavior change, comparing its answer against every dispatch-critical owner site under a
    `MUTSU_VM_STATS` counter. A ~26k-check sweep found ~2.3% mismatches, all bucketed into three
    explained causes (no unexplained bucket): Enum receivers (the classifier is right, fixed in
    E1b), role Mixin/ParametricRole generic-collapse (exactly E1's target failure mode), and a
    `Package`-collision case filed as `todo/tickets/multi-arg-type-keys-package-collision.md`.
    Also filed `todo/tickets/mixin-role-order-not-tracked.md` (mutsu has no mixin-application-order
    field, so stacked `but` collisions resolve alphabetically instead of raku's "later wins" — the
    classifier deliberately mirrors the same wrong-but-deterministic order rather than diverging
    further).
  - [x] **E1b — switch (2026-08-10).** Made the classifier authoritative at the three sites safe
    to cut over unconditionally, and deleted the two remaining divergent MRO tables outright.
    Deliberately NOT cut over: `multi_arg_type_keys`, since its cutover is not a shadow-safe
    refactor but IS the fix for the ticket above — it stays on the shadow probe until that ticket
    is picked up on its own.
  - [x] **E1c — MOP fallback consolidation (2026-08-10).** One `mop_receiver_owner` helper
    replaces 22 duplicated per-MOP-entry owner-fallback arms across six files.
- [ ] **E2 — Give every native entry an exact handler ID.** Generate static type×method handler rows
  for pure arity handlers and stateful/special handlers; pin type-object, subclass, Map/Seq,
  Failure, and Rat-style cases that broke the reverted attempt.
  **Design (2026-08-10):** rows are *recognition metadata* (owner, name, arity mask,
  TYPE_OBJECT_OK/MUTATES/SPECIAL flags), not function pointers — invocation stays in the arity
  cascades until F3. Coverage is measured to (near) zero via a `native_call_unmodeled` counter
  before any read depends on rows.
  - [x] **E2a — row schema + instruments + pinned regression tests.** Zero behavior change.
  - [ ] **E2b — drive `native_call_unmodeled` toward zero** through the gate-classification table.
    Twelve slices (2026-08-10) hand-probed missing owner/method rows in clusters (`Pair`/`Seq`,
    `Match`, `Array`/`List`, `Str`/`Hash`/`Int` universals, `Buf`/`Blob`/`Set`/`Bag`/`Mix` family,
    `RakuAST::*` node accessors, `Date`/`Backtrace`/`Complex`), driving the counter from ~37904 to
    ~400 (**-99%**) on the `t/`+roast sweep corpus. Two slices were root-cause fixes rather than
    row additions and changed real dispatch answers, not just the coverage table: (1) ~20 built-in
    `X::*` exception types were never `register_x`'d, so their registry MRO dead-ended before
    reaching `Exception` — meaning `$exc ~~ Exception`/`.isa(Exception)` was silently `False` for
    any of them, not just a counter artifact; and (2) `catalog_chain_for_name`'s fallback for an
    uncataloged parametrized name (`Array[Int]`, `array[int32]`, `CArray[uint8]`) never spliced
    the base type's real ancestry, and a second latent bug in the splice logic silently truncated
    the chain even once fixed. Both raku-verified and pinned.
    **Gate renegotiation — adopted 2026-08-10 (decided with the user, not unilateral):** after
    twelve slices with no dominant cluster left (remaining hits are one-off RakuAST node
    accessors, NativeCall `CArray[T]` variants, and ad-hoc test-fixture class names), the design
    doc's original "must reach exactly zero before E3/E4b can land" precondition is **replaced**:
    E4b's resolver must fall back to the existing arity cascade on any row miss and keep
    incrementing the counter, so an incomplete table degrades to today's behavior instead of
    misdispatching. `native_call_unmodeled` is now a monitoring signal (kept low, reviewed
    periodically, new clusters still fixed at the root cause), not a hard precondition. E2b itself
    stays open for opportunistic root-cause fixes but no longer blocks E3/E4b.
- [x] **E3 — Add the generation-keyed resolved-call cache.** Key by receiver TypeId, method symbol,
  call shape, and method generation; cache the ordered candidate sequence, not a second resolver.
  **Design 2026-08-10** (same doc): lands after E4b. Key `(TypeId, Symbol, CallShape)` where
  CallShape packs arity bucket + has-named (named calls get sequence caching for the first
  time); joins `refresh_method_caches_for_generation`'s wholesale clear set; the two probe
  sites that today bypass the generation refresh gain it. `fast_method_cache` survives as the
  monomorphic IC in front until F5 — retiring it inside Phase E would be an unmeasured perf
  cliff. Bench-CI parity evidence is part of this box's exit (G3's dispatch clause).
  **Slice 1 (2026-08-14, #6395):** `pick_method_winner_from_sequence` models the non-multi
  early-stopping rule against `ResolvedSequence`, byte-for-byte verified against
  `resolve_method_with_owner_impl` with zero shadow-check mismatches across the full `t/` suite
  and the dispatch-heaviest roast directories; the same PR also closed two pre-existing
  generation-coverage gaps in the sound-multi-resolve and non-multi HashMap caches.
  **Slice 2 (2026-08-14, #6395):** confirmed both of those caches gain the generation key.
  **Cutover (2026-08-14, #6396):** added `resolved_seq_cache: FxHashMap<(TypeId, Symbol,
  CallShape), Arc<ResolvedSequence>>` and `resolve_via_sequence_cache`; `resolve_method_cached`'s
  two cache-miss paths (sound-multi-resolve miss, final "resolve fresh") now call it instead of
  `resolve_method_with_owner_invocant`'s live MRO walk, and the now-redundant
  `shadow_check_resolver` calls at those sites were removed. **Bench-CI parity (2026-08-14):** the
  next bench-CI point after the cutover (`fe4a650b0`, vs. the pre-cutover point `5b030c516`) shows
  `method-call` dropping from 0.185s to 0.146s raw (raku ratio 0.74→0.63) — but every other
  benchmark moved by a similar ~15-20% too, including `bench-tak` (a no-dispatch control,
  0.311s→0.265s), matching the PR's own local-A/B finding that shared-runner load noise dominates
  interleaved-vs-back-to-back comparisons on this box. Read against that uniform shift,
  `method-call`/`bench-class`/`poly-call` show no method-call-specific regression or outlier —
  satisfying this box's bench-CI parity exit criterion (no regression), though the data does not
  cleanly isolate a specific speedup either. **E3 is closed.**
- [x] **E4 — Resolve native and user candidates in one MRO walk.** Preserve user shadowing,
  visibility, invocant definedness, arity/signature ordering, and native fallback in one result.
  **Design (2026-08-10):** `resolve_sequence(chain, name, shape, definedness)` returns a
  `ResolvedSequence` — the shape-independent ordered candidate universe (user candidates in
  stored order per level, accessor arbitration, native rows at catalog levels, proto slot); the
  existing per-call ranking ladder is extracted to consume a candidate slice rather than
  reimplementing MRO walking itself.
  - [x] **E4a — sequence builder + shadow parity (user candidates only), 2026-08-10.** Landed
    `ResolvedSequence`/`ResolvedCandidate::User` and `resolve_sequence`, counter-verified against
    `resolve_method_with_owner_impl`'s real outcomes with zero behavior change.
  - [ ] **E4b — authoritative switch at `should_bypass_native_fastpath` (`call_method_with_values`'s
    one call site).** Thirteen investigation steps (2026-08-11) decomposed the ~110-line boolean
    chain into four disjoint categories and landed each at its locally optimal shape:
    - **Category 1 (receiver-shape safety gates)** mostly reduces to "the cascade itself already
      self-guards or never matches this name" — four confirmed reductions
      (`Supplier`/`Supplier::Preserving.Supply`, most of `Proc::Async`'s method family,
      `IO::Handle`'s `encoding`/`opened`/`DESTROY`, `Stash.AT-KEY`), each raku-verified and landed
      as an outright guard deletion; the remainder (`Supply`'s list vocabulary, lazy-`Match`
      forcing, `Hash.keys` with no args) are genuine receiver-state hazards that must stay
      explicit. Extracted into one `native_fastpath_receiver_state_guard` function.
    - **Category 2 (`is_native_method`, NativeCall class binding)** got its own
      `ResolvedCandidate::NativeCallBinding` candidate, shadow-verified safe for `Instance`
      receivers (99.2% mismatch reduction) — but found NOT to generalize to a `Package`
      (type-object) receiver: `Supply.interval`-style class-level factory methods share the same
      registry flag as genuine instance methods with no way to tell them apart, so the candidate
      stays gated by `is_instance` at its one real consumer.
    - **Category 3 (`resolve_user_method_or_accessor`)** landed live for the Instance branch
      (2026-08-11) — the exact substitution E4a already shadow-verified safe. The Package branch
      needs its own narrower `has_user_method`-only check (no accessor notion applies to a bare
      type object) and was NOT folded in.
    - **Category 4 (class-level attributes, `our $.x`/`my $.x`)** has no accessor at all
      (`registration_class_body_attr.rs` skips normal accessor registration for it entirely) —
      confirmed a genuine, permanently non-foldable fourth candidate kind with exactly one
      consumer; stays an explicit guard.
    A shadow-only `ResolvedCandidate::Native` (the E2 row-catalog candidate) also landed (step 9),
    verified `native_row_shadow_mismatches=0` against the real cascade result.
    **Conclusion (step 13, closing E4, 2026-08-11):** `Native`/`NativeCallBinding` are for E5-E7's
    *separate* VM-opcode dispatch mechanism (`CallMethod`/`CallMethodMut`/etc.), not for
    `should_bypass_native_fastpath` — gating this call site with either candidate would only add
    the cost of predicting an answer the existing direct call already computes for free. E4b is
    **functionally complete at its own call site**; the box stays open only pending E5-E7
    confirming the same conclusion holds at their entries too.
  **Progress (2026-08-12, closing E4):** E5, E6, and E7 have since all landed and independently
  confirmed the same answer: `Native`/`NativeCallBinding` stay measurement/hint-only at every
  entry, because the real safety net for native dispatch lives inside `try_native_method_raw`'s
  ~22 scattered per-shape guards, not a single candidate-presence fact a resolver could safely
  gate on without reimplementing all of them. E4's own box text ("resolve native and user
  candidates in one MRO walk... in one result") is satisfied by `ResolvedSequence`/
  `ResolvedCandidate` existing and being shadow-verified at its call sites — the box does not
  additionally require the VM opcode entries to *dispatch on* the `Native` candidate, since E5-E7
  independently and deliberately decided against that. **E4 is marked done.**
- [x] **E5 — Route ordinary VM method calls through the resolver.** Cover zero/n-arg and named-call
  opcodes while retaining mutation/writeback semantics at the caller boundary.
  **Design (2026-08-10):** the cutover shape is "resolver decides, existing arms execute" — each
  entry's dispatch-probe section becomes a match on the resolver decision while receiver
  normalization, method-identity intercepts, and writeback tails stay put. Per-entry
  `MUTSU_VM_STATS` measurement precedes each cutover.
  **E5, steps 1-3 (measurement, 2026-08-11):** instrumented `CallMethod`, `CallMethodDynamic`, and
  the two hyper opcodes with per-entry per-outcome counters. `CallMethod`'s traffic split:
  user=49%, native=44%, intercept=4%, accessor=3% (26,924 disjoint checks, full `t/`+roast S12/S14
  sweep).
  **E5b (`CallMethod` cutover, 2026-08-11):** the native candidate stays a direct probe (no gain
  from a resolver lookup); user-candidate resolution is shared/deduped into one
  `resolve_method_cached` call; the arity cascade stays a direct call. Closed at `CallMethod`'s
  own entry point.
  **E5c (`CallMethodDynamic` + hyper entries, 2026-08-11):** `CallMethodDynamic` inherited E5b's
  shared helper for free (it had no duplicate resolution logic to converge). Raku-verified that
  `HyperMethodCallDynamic`'s missing `skip_native`/`has_user_method` gate (an E5-step-3 finding)
  is redundant defense-in-depth, not a real gap — `try_native_method_raw`'s own internal guards
  are the real safety net. Two unrelated bugs found and filed, not fixed here:
  `todo/deep/pseudo-method-which-why-user-override-ignored-in-bareword-and-dynamic-form.md` and
  `todo/tickets/dollar-dot-dynamic-method-name-should-require-callable.md`.
  **E5d (JIT shim parity, 2026-08-11):** confirmed by inspection — the one JIT shim in E5's scope
  re-enters `exec_call_method_op` itself, so every E5b/E5c change is covered automatically; the
  other three opcodes have no JIT shim at all. **All of E5 is closed** (checkbox corrected
  2026-08-12 — the box's own text had already declared closure the day it landed).
- [x] **E6 — Route mutation-aware and container calls through the resolver.** Cover celled,
  lvalue/rw, Proxy, index/attribute writeback, and mutable aggregate entry points, including the
  second slow path `call_method_mut_with_values` and the `ArrayPush` fast opcode.
  **E6a/E6b (2026-08-11):** cut the celled/lvalue/writeback entries over the same way E5b did.
  Verification found and fixed a real, unrelated dispatch-order bug while classifying the
  surrounding cascade — pinned by `t/augment-native-lever-a-methods.t` — plus an MRO
  bracketed-vs-parametrized-name ordering bug caught by `t/digest-battery.t`'s SHA3 sub before
  landing.
  **E6d (`ArrayPush`'s augmented-Array divergence, 2026-08-11):** raku-verified moot — both
  `augment class Array { method push }` and its `multi` variant are illegal in raku
  (`X::Redeclaration`/`X::Multi::Ambiguous`), and the one legal override mechanism (a
  `does`-mixin) already dispatches correctly via the fast path's existing `is_simple_array` gate.
  The design doc's proposed `array_dispatch_pristine` generation-refreshed bit was **not built** —
  it would defend against a divergence that does not exist for any legal program.
  **E6c (2026-08-12, closing E6):** `CallMethodDynamicMut`'s missing native/compiled probe was a
  REAL bug, raku-verified: a `does`-mixed role's overriding `push` silently lost to the native
  array push. Root cause was one level deeper than the opcode handler — the shared mut slow path
  `call_method_mut_with_values` special-cased `push`/`append`/`unshift`/`prepend`/`pop`/`shift`/
  `splice` purely by sigil, with no check that the value behind the sigil was still a plain
  `Array`/`Hash` and not a `does`-mixed `Mixin` (unlike the `ArrayPush` fast opcode's own guard
  and the Tier-A native-array-mut helper). The same gap also affected the **static**
  `CallMethodMut` path for any mutator without its own fast opcode. Fixed by gating both the
  array- and hash-mutator blocks with the same `mixin_role_has_method` guard
  `try_native_method_raw` already uses. Pinned by `t/mixin-array-hash-mutator-override.t`. **All
  of E6 (E6a, E6b, E6c, E6d) is closed.**
- [x] **E7 — Route metaobject, qualified, and re-entrant calls through the resolver.** Cover HOW,
  `.^lookup`/`.^can`, qualified/private dispatch, EVAL carriers, and method objects.
  **Design (2026-08-10):** one consumer family per sub-slice: `run_instance_method` carrier sites,
  qualified dispatch, private-as-sequence-query, `.^lookup`/`.^can`/`.^methods`, `.WALK`, and the
  EVAL/`subtest` re-entrant carriers — eight sub-slices in total (2026-08-11/12).
  Outcome across all eight: three clean shadow-checks with no gap (qualified dispatch,
  `run_instance_method` carrier sites, and the EVAL/`subtest` step, which found those forms have
  no distinct dispatch carrier at all — both run through the same ordinary `CallMethod*` opcodes
  every other call uses); one shadow-measured-but-deferred catalog gap (`.^can`, closed later in
  E11 slice 2); and four confirmed-and-fixed real `raku`-vs-`mutsu` behavioral bugs. Two of those
  four (`.^methods` and `.WALK`) independently found the *same shape* of gap — a runtime
  `but`-mixin's own role methods missing from an enumeration/reflection path that only ever
  traversed the registered class hierarchy, even though ordinary method *invocation* on a mixin
  worked correctly throughout — suggesting mixin receivers were systematically under-tested
  against introspection call paths before this box. **All of E7 (eight sub-slices) is closed.**
  Full per-step detail in `todo/deep/adr0019-e5-e7-entry-routing.md`.
- [x] **E8 — Model multi/proto/submethod ordering in the candidate sequence.** Remove parallel
  multi and submethod resolver entry points without changing tie-breaking or role conflicts.
  Unifying the method-vs-sub ranking ladders is explicitly out of scope.
  **E8a (2026-08-12):** `ResolvedCandidate::User` gained `level`/`stored_idx` so winner selection
  and deferral order both derive from one sequence; `drop_flattened_role_duplicate_candidates`
  moved to build time. A shadow probe on the real `nextsame`/`callsame` deferral-list builder
  found and fixed a real bug in the probe itself (the deferral list is correctly invocant-BLIND,
  matching raku's own walk — an invocant-aware probe was stricter than its own target) and
  documented, without fixing, a pre-existing gap: `resolve_sequence`'s per-level lookup silently
  misses an un-punned role's own methods, since the canonical `method_entries` table is only ever
  populated from classes. Filed as `todo/deep/method-entries-never-covers-unpunned-roles.md`
  (also feeds several real production dispatch paths, so fixing it is out of this shadow-only
  box's scope).
  **E8b (2026-08-12):** `Registry::proto_methods` folded into `MethodEntry.proto`, shadow mode.
  Found and fixed a real bug in the *existing* sync logic (not the new probe): a row holding only
  a freshly-written `.proto` (no builtin/user_candidates/accessor) failed
  `sync_user_method_entries`'s keep condition and was silently dropped from the table.
  **E8c (2026-08-12):** the proto-method cutover — `lookup_proto_method`'s real MRO walk now
  reads `MethodEntry.proto` directly; the standalone `proto_methods` table (confirmed to have
  exactly the two readers E8b's design assumed) was deleted outright as genuinely dead code, not
  merely superseded. **All of E8 is closed.** Next Phase E box (per the design doc): **E9-pre**,
  the mandatory raku verification campaign for `samewith`/`nextsame`/`callsame`/`nextwith` cursor
  semantics — the highest-semantic-risk box of the phase, run as its own dedicated session.
- [x] **E9 — Add resolver cursors for `samewith`/`nextsame`/`callsame`/`nextwith`.** Continue within
  the resolved sequence instead of re-entering name-based resolution.
  **E9-pre (2026-08-12, docs + `t/` pins only, no cursor code):** every scenario probed against
  Rakudo v2026.06 first; 12 new pins (38 assertions), each verified green under both `prove -e
  raku` and mutsu. **The campaign falsified a design assumption**: when multi candidates span MRO
  levels, raku defers along the specificity-ranked *merged* candidate list, not mutsu's
  `(level, decl-order)` walk — the cursor's sequence layout was re-drawn (a flat deferral
  expansion: per-MRO-class entries, each a plain method or that class's proto's specificity-ranked
  candidate block) and confirmed by two exact-hit predictions against raku before any
  implementation. Eight divergence tickets were filed from the campaign; most were resolved in
  follow-up slices the same week (role-shadowed method wrongly in the defer chain, `is Array`
  native-push fallback not pushing/not carrying args, an explicit child proto wrongly assuming
  parent candidates — each raku-verified and pinned); `method-entries-never-covers-unpunned-roles.md`
  (a separate production-dispatch gap, not caused by this box) and
  `proto-method-body-skipped-for-type-object-invocant.md` remain open.
  **E9a (2026-08-12):** `resolve_deferral_expansion` replaced `resolve_all_methods_with_owner` as
  the ordering source at both "remaining"-building call sites, exact-hit verified against raku.
  The `DispatchCursor{seq, next, invocant, args}` index-based rewrite from the original design
  stayed explicitly out of scope as orthogonal perf/cleanliness work — every load-bearing piece of
  E9's scope was achieved through other means instead.
  **E9b (wrap-prefix, 2026-08-13, three slices):** method wraps fold into
  `MethodDispatchFrame.remaining` as `DeferralEntry::{Wrapper, Candidate}` prefix entries in one
  frame, replacing a separate `WrapDispatchFrame` plus by-name re-entry. Deleted the
  `sub_id == 0` sentinel, the `__mutsu_method_wrap_original` marker, the global
  `is_inside_wrap_dispatch()` guard, and the mid-MRO peek-and-intercept block. Fixed two real,
  raku-confirmed cross-stack/cross-wrap-chain bugs the probes found: a `callsame` inside a method
  picking the wrong dispatch frame when wrap and method stacks nest (fixed with a shared monotonic
  dispatch-token, innermost frame wins) and a wrapped method called from inside a *different*
  method's wrapper losing its own wrap chain to the (now-deleted) global guard.
  **E9c (proto `{*}` + `samewith`, 2026-08-13, two slices):** `{*}` now resolves the winner
  directly via a boundary-parameterized `resolve_method_within_boundary`, deleting the ambient
  `proto_redispatch_boundary` field and the double `lookup_proto_method` walk. **A probe falsified
  the original `samewith` design clause**: `samewith` re-runs the governing proto BODY (side
  effects observably repeat) in both raku and mutsu, so the by-name full-dispatcher restart is
  correct and stays — `samewith`'s own work shrank to carrier consolidation (merging two
  desync-prone parallel stacks into one `SamewithContext` stack).
  **All of E9 (E9-pre, E9a, E9b, E9c) is closed** (five PRs merged 2026-08-13: #6361/#6363/#6369/
  #6372/#6375). Full scenario tables and slice detail in
  `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`.
- [x] **E10 — Move wrap/unwrap mutation into canonical entries.** Bump the generation and remove
  wrap-specific cache-clearing paths.
  **Landed 2026-08-13.** `Registry::method_wrap_chains` replaces the interpreter-level map; every
  mutation path bumps `method_generation`, including two push sites that previously invalidated
  nothing at all. The one real "fast cache" gate — a `!self.has_any_wrap_chains()` guard that
  disabled `fast_method_cache` program-wide the instant ANY method anywhere was wrapped — is
  deleted, since a wrapped method was never cached there in the first place and a later wrap
  evicts any stale entry via the generation bump; the scan-guarding call sites (not caches) stay.
  Also fixed a real `.unwrap`/`.restore` method-wrap leak (confirmed against Rakudo first):
  neither ever actually removed the `method_wrap_chains` entry for a method-candidate `WrapHandle`
  before this fix, since it stored the wrong attribute shape. Pinned by
  `t/wrap-candidate-unwrap-restore.t`.
- [x] **E11 — Retire arity-specific lookup entry points.** Keep native arity functions only as
  handler implementations selected by `MethodEntry`. Completion criterion: no caller of
  `native_method_{0,1,2}arg` outside the resolver's two canonical invocation points
  (`call_method_with_values`'s by-arity match, `try_native_method`) and `builtins/` internal
  recursion.
  **Slice 1 (2026-08-13):** retired eight call sites that were pure invocation-context duplicates
  of what `call_method_with_values` already does internally (numeric-bridge coercion re-dispatch,
  `SetHash`/`BagHash`/`MixHash` Callable-arg sites) — zero behavior change, since the receiver at
  each site was always a native value already served by the same native-first path.
  **Slice 2 (2026-08-14):** closed a catalog-coverage gap (`builtin_sample_value` had no branch
  for seven owners including `Cool`/`Any`/`Mu`/`Code`/`Signature`/`IO::Path`/`IO::Handle`, so
  `native_method_row_exists` was unconditionally false for them) and cut the two deferred `.^can`
  sites over to `e2_native_method_exists`. Found and fixed a real pre-existing bug in the process:
  `can-ok`'s existence check only ever probed the 0-arg cascade, so any 1-arg-or-later native
  method was invisible to it even on its own concrete owner (`can-ok "abc", "substr"` wrongly
  failed). Pinned by `t/can-ok-cool-bridging-methods.t`.
  **Slices 3-5 (2026-08-14):** cut over the three remaining deferred sites —
  `builtins_collection.rs`'s `&self`-signature collection-method dispatcher (needed a `&mut self`
  conversion), the `is-deeply`/`is-eqv` diagnostic formatter (fixing a real gap: it never saw a
  user-defined `.raku` override, so the diagnostic silently fell back to generic stringification),
  and the REPL's last-value `.gist` display (same class of gap for user-defined `.gist`). Each
  guarded by `e2_native_method_exists()` to preserve its exact prior fallback shape, each
  raku-verified and pinned (`t/is-deeply-user-raku-diagnostic.t`, a new REPL display test).
  **E11 closes here.** The grep-based completion criterion is met exactly: `native_method_[012]arg`
  outside `src/builtins/` now resolves to only the two canonical invocation points plus doc
  comments.

### Phase F — derive introspection and remove compatibility state

- [ ] **F1 — Build `Method` objects from canonical entries.** Store ownership, visibility,
  signature, multi/submethod, wrap, and native metadata needed by introspection.
- [ ] **F2 — Derive `.^methods`, `.^can`, and method MRO views from the resolver/table.** Use the
  same TypeId MRO and visibility rules as calls.

  **Progress (2026-08-14):** the user-method half of F1/F2 is done, in the shadow-then-cutover
  style E1a set. All three MRO/table readers that used to walk `ClassDef::methods` directly now
  build every candidate list from the canonical `Registry::method_entries[(owner,
  name)].user_candidates` table: `.^methods`/`.^method_table` (#6399/#6400) and `.^can`/`.can`
  (#6402/#6406). Native/builtin method metadata (F1's "native metadata" clause) and full
  `Method`-object fidelity (F2's "visibility rules") remain open — see
  `todo/deep/adr0019-f1-f2-introspection-canonical-source.md` for the raku ground truth gathered so
  far and why it needs a dedicated verification pass before a design. That pass also surfaced a
  distinct bug, `todo/tickets/classhow-lookup-returns-sub-not-method-instance.md`: `.^lookup`
  builds a `Sub`-shaped value instead of the same `Method` `Instance` these readers now share.
  **Update (#6420):** the sharpest symptom of that bug — `.is_dispatcher`/`.multi` silently
  returning a bogus `<composed-method:NAME>` callable instead of a real answer — is fixed with
  targeted handling on the `Sub`-shaped value, verified against `raku` ground truth (pin:
  `t/classhow-lookup-method-is-dispatcher-multi.t`). The underlying representation mismatch (Sub
  vs. Method Instance) remains open; this was a scoped patch, not the unification.
  **Update (2026-08-14, F1 mechanism slice, `.package` only):** `make_native_method_object`/
  `make_method_object_with_owner` never set a `.package` attribute at all (always `Nil`, not just
  imprecise). Fixed: exact for user/role methods (declaring class/role, raku-verified); a multi
  dispatcher's own `.package` stays deliberately unset (real Rakudo answers a synthetic `(Dummy)`
  mutsu does not model) while its candidates get the correct owner; native methods default to the
  catalog owner (an accepted imperfect mechanism-slice default, e.g. `Str.uc` answers `(Str)` not
  Rakudo's true `(Cool)` — the fidelity slice closes that gap later). `.signature`'s synthesized
  default and the Sub-vs-Instance unification remain open. Pin: `t/classhow-methods-package.t`;
  design detail in `todo/deep/adr0019-f1-f2-introspection-canonical-source.md`'s "Progress
  (2026-08-14): F1 mechanism slice, `.package` only".
- [x] **F3 — Delete the per-type method-name lists and the test-only `METHOD_UNIVERSE`.** B1/B2
  already removed `METHOD_UNIVERSE` and runtime probing from the runtime path (both are
  `#[cfg(test)]`-only now); the live work is the fourteen per-type `&[&str]` name slices
  (~350 slots in `builtin_type_methods.rs`) that still feed `builtin_method_entries`. This is the
  explicit retirement of ANALYSIS §4-1's hand tables; retain only the generated native entry
  catalog that dispatch itself consumes.
  **Scoping (2026-08-14):** this box's "generated native entry catalog" target
  (`native_method_row.rs`'s `RAW_ROWS`) turns out to have drifted from the 14 arrays since its
  2026-08-10 generation — apparently missing an owner (`Sub`) entirely, with ~90+ extra
  dispatch-recognized names across other owners not vetted for `.^methods` inclusion, and at least
  one owner (`Signature`) in a different order. Not a mechanical cutover; needs its own
  raku-verification pass first. See `todo/deep/adr0019-f3-raw-rows-drift-from-introspection-arrays.md`.
  **Progress (2026-08-14, step 1):** the "`Sub` missing entirely" claim was a probe artifact (an
  owner-folding mismatch in the ad hoc diff, not a real gap — `RAW_ROWS` already carries all 10
  `Code`-folded rows `Sub` needs). Verified for real with a new permanent test,
  `raw_rows_cover_every_introspection_name_in_order`: **zero missing names for all 18 owners.**
  Fixed the two owners (`Signature`, `Any`) whose `RAW_ROWS` order genuinely diverged from their
  introspection array (rows were scattered into unrelated hand-added blocks); order now matches for
  all 18. The ~90+ extra dispatch-recognized names per owner (step 2's raku-verification triage) are
  still untouched — that remains the real blocker before the actual cutover (step 3).
  **Progress (2026-08-15, step 2, first name):** raku-verified the first of the ~90+ extra names —
  `Mu`'s single extra, `DEFINITE` (`RAW_ROWS` had it via E2b; `MU_METHODS` did not). Confirmed a
  genuine gap, not dispatch-only noise: `raku -e 'say 5.DEFINITE'` works and real Rakudo's
  `Mu.^methods` lists `DEFINITE` (mutsu's `Mu.DEFINITE` already dispatched correctly before this
  fix — only introspection was missing it). Added to `MU_METHODS` at the position matching its
  `RAW_ROWS`-relative order (first, ahead of `defined`), keeping
  `raw_rows_cover_every_introspection_name_in_order` green, and pinned in
  `t/can-methods-drift.t`. Step 2 remains open for the other ~89+ names across the other 17 owners —
  this is one triaged name, not a batch.
  **Progress (2026-08-15, step 2, `Any` and `Hash`):** triaged `Any`'s 7 extras and `Hash`'s 11.
  For `Any`: `serial` and `hash` are genuine `.^methods` gaps (real Rakudo's `Any.^methods` lists
  both, and mutsu already dispatches both correctly); `self`/`clone`/`WHICH`/`sink`/`item` are
  confirmed dispatch-only/internal (real Rakudo's `Any.^methods` does not list any of them —
  `WHICH` is a `Mu`-declared method appearing on `Any`'s inherited view, not `Any`'s own). Added
  `serial`/`hash` to `ANY_METHODS`. For `Hash`: `pick`/`EXISTS-KEY`/`AT-KEY`/`List`/`invert`/`flat`/
  `dynamic`/`roll` are genuine gaps (all confirmed present on real Rakudo's `Hash.^methods` and
  already dispatch correctly on mutsu); `Array`/`AT-POS`/`EXISTS-POS`/`perl` are confirmed
  dispatch-only (not on real Rakudo's `Hash.^methods`). Added the 8 genuine names to
  `HASH_METHODS`, in `RAW_ROWS`-relative order (both owners' rows arrive in two separate blocks in
  `native_method_row_table.rs`, an artifact of how E2b originally landed them; the newly-added
  names had to be appended after the array's existing tail to keep
  `raw_rows_cover_every_introspection_name_in_order` green, since that test only requires the
  *shared* names' relative order to match, not raku's true `.^methods` order). All raku-verified
  and pinned in `t/can-methods-drift.t`. Running total: 3 of 18 owners triaged (`Mu`, `Any`,
  `Hash`); `Str` (25 extras), `Int`/`Num`/`Rat`/`Complex` (25), `Cool` (11) remain the largest
  untriaged owners.
  **Progress (2026-08-15, step 2, `Cool`):** triaged `Cool`'s 11 extras — the native-sized-integer
  coercion methods (`int8`..`uint64`, `byte`, `int`, `uint`). All 11 raku-verified as genuine
  `Cool.^methods` entries and confirmed to already dispatch correctly on mutsu. Added a new
  `COOL_NATIVE_INT_COERCE_TAIL` array (appended after `NUMERIC_COERCIONS` in `builtin_type_
  method_names`'s `"Cool"` arm, matching the block's position in `RAW_ROWS`). **Found and fixed a
  real bug this exposed**, not just a list gap: `is_builtin_type_method`
  (`methods_classhow_lookup.rs`, feeding `.^find_method`/`.can` on a `Package` receiver) checked
  `["type_name", "Cool", "Any", "Mu"]` as a hardcoded ancestor list for *every* type regardless of
  whether `Cool` was actually an ancestor — harmless while `Cool`'s own list had no name likely to
  collide, but once `int8` etc. joined `Cool`'s list, `Pair.^can('int8')` (`Pair`'s real MRO is
  `[Pair, Any, Mu]`, no `Cool`) went from correctly `False` to a false-positive `True`. Fixed by
  reading the receiver type's real MRO from `registry().class_mro_readonly()` (the builtin type
  catalog's own authoritative source) instead of guessing, with the old hardcoded list kept only as
  a fallback for a type the catalog doesn't recognize. Regression-pinned (`Pair cannot int8`) in
  `t/can-methods-drift.t` alongside the new `Cool` names. Full local `t/` suite (3166 files) and the
  targeted `S12-introspection`/`S02-types/hash.t`/`S09-typed-arrays/hashes.t` roast files stay
  green. Running total: 4 of 18 owners triaged (`Mu`, `Any`, `Hash`, `Cool`); `Str` (25 extras) and
  `Int`/`Num`/`Rat`/`Complex` (25, likely shared) remain the largest untriaged owners.
  **Progress (2026-08-15, step 2, `Int`/`Num`/`Rat`/`Complex`):** the "25 extras, likely shared"
  guess above was wrong — checked `RAW_ROWS` directly per owner instead of assuming: only `Int` has
  a real 25-name extras block; `Num` has none; `Rat` has 2 (`FatRat`, `nude`); `Complex` has 8. Of
  `Int`'s 25, 7 are genuine `Int.^methods` gaps (`rand`, `uniprop`, `lsb`, `msb`, `int8`, `Real`,
  `Complex`), all raku-verified and already dispatching correctly; the other 18 are confirmed
  dispatch-only. `Rat`'s both extras are genuine (`FatRat`, `nude`). Of `Complex`'s 8, 6 are genuine
  (`isNaN`, `re`, `im`, `reals`, `conj`, `Complex`); `UInt`/`reverse` are dispatch-only. Since
  `NUMERIC_OWN` is one array shared by all four owners but these extras are NOT shared (e.g. `rand`
  is `Int`-only per `RAW_ROWS`, even though real Rakudo also has `Num`/`Rat` `.rand` — a separate,
  still-open gap `RAW_ROWS` itself doesn't cover, out of F3's own "match `RAW_ROWS`" scope), split
  the `"Int" | "Num" | "Rat" | "Complex"` match arm into four, each with its own optional extra tail
  (`INT_EXTRA_TAIL`, `RAT_EXTRA_TAIL`, `COMPLEX_EXTRA_TAIL`) appended after `NUMERIC_COERCIONS`,
  matching each block's `RAW_ROWS` position. All raku-verified and pinned in
  `t/can-methods-drift.t` (96 assertions total now). Full local `t/` suite (3167 files) and the
  targeted `S12-introspection/*`/`S32-num/*` roast files stay green. Running total: 8 of 18 owners
  now settled (`Mu`, `Any`, `Hash`, `Cool`, `Int`, `Num`, `Rat`, `Complex` — `Num` needed no
  changes, its extras block was empty). `Str` (25 extras) is now the only large owner left
  untriaged; the remaining 10 owners are the small-count ones the original survey table lists.
  **Progress (2026-08-15, step 2, `Str`):** triaged `Str`'s 24-name extras block (the survey's "25"
  count off by one). 11 genuine `Str.^methods` gaps (`uniprop`, `indent`, `ord`, `uniname`,
  `uninames`, `unival`, `univals`, `tclc`, `Version`, `Date`, `DateTime`), all raku-verified and
  already dispatching correctly (e.g. `'A'.ord`, `65.uniname`, `'1.2.3'.Version`). The other 13
  (`AST`, `list`, `UInt`, `FatRat`, `sprintf`, `chrs`, `bytes`, `Range`, `Complex`, `Real`,
  `reverse`, `byte`, `perl`) confirmed dispatch-only — real Rakudo's `Str.^methods` lists none of
  them. Added a new `STR_EXTRA_TAIL`, appended after the existing `&["elems", "fmt"]` tail in the
  `"Str"` match arm, matching the block's `RAW_ROWS` position. All raku-verified and pinned in
  `t/can-methods-drift.t` (129 assertions total now). Full local `t/` suite (3167 files) green;
  `roast/S12-introspection/*` and every `roast/S32-str/*.t` file green too (invoked via
  `scripts/run-roast-test.sh`, not a bare `prove` — the bare invocation spuriously "fails" 3 of the
  encoding-conversion files on missing fixture paths that only resolve inside that wrapper, an
  invocation artifact unrelated to this change, not a regression). **This closes F3 step 2's
  large-owner sweep**: all 5 owners the original survey flagged as having 7+ extras (`Str`, `Int`,
  `Cool`, `Complex`, `Any`) are now triaged. The remaining ~10 small owners (1-3 extras each per the
  original survey table) are still open for step 2 but are far smaller individually; step 3 (the
  actual `RAW_ROWS`-as-single-source cutover) can reasonably start once those are swept too.
  **Progress (2026-08-15, step 2, `List`/`Array`/`Range`/`Blob`):** the "1-3 extras each" estimate
  for the remaining owners above was also wrong for these four — a fresh `RAW_ROWS`-vs-introspection
  diff (not the original survey's rough read) found `List` has 18 extras, `Array` 19, `Range` 13,
  `Blob` 7; `Bool`/`Sub`/`Signature`/`IO::Path`/`IO::Handle` genuinely have zero (already fully
  covered). `List` gains 13 (`list`, `item`, `Slip`, `sink`, `invert`, `AT-POS`, `EXISTS-POS`,
  `is-lazy`, `Capture`, `hyper`, `race`, `Supply`, `fmt`); `Array` gains those same 13 plus two more
  real Rakudo answers only for `Array` specifically (`WHICH`, `dynamic`) — confirmed by raku, not
  assumed, since `LIST_METHODS` is one array shared by both owners but `RAW_ROWS` itself lists
  `WHICH` only under `Array`, not `List`. `Range` gains 7 (`hyper`, `lazy`, `int-bounds`, `AT-POS`,
  `race`, `in-range`, `EXISTS-POS`). `Blob`/`Buf` gain 5 (`read-uint8`, `read-int8`, `read-uint16`,
  `read-int16`, `read-uint32`). All 25 additions raku-verified and already dispatched correctly
  before this change. Since `List`/`Array` need different extra sets from the same shared base,
  split their match arm into two with separate `LIST_EXTRA_TAIL`/`ARRAY_EXTRA_TAIL` tails (same
  pattern as the `Int`/`Rat`/`Complex` split earlier in this box). All raku-verified and pinned in
  `t/can-methods-drift.t` (193 assertions total now). Full local `t/` suite (3167 files) green;
  `roast/S12-introspection/*`, `S02-types/{array,list,range}.t`, `S32-container/buf.t`,
  `S03-operators/buf.t`, and every `S03-buf/*.t` file green (via `scripts/run-roast-test.sh`).
  Running total: 13 of 18 owners now settled. Remaining 5 (`Sub`/`Signature`/`IO::Path`/
  `IO::Handle`/`Bool` per the fresh diff) all have **zero** extras — F3 step 2's owner-by-owner
  triage is therefore complete; step 3 (the actual cutover) is unblocked.
  **Progress (2026-08-15, step 3, the cutover):** deleted all fourteen hand-written per-type
  `&[&str]` name slices in `builtin_type_methods.rs` (`STR_OWN`, `NUMERIC_OWN`/`NUMERIC_COERCIONS`
  and their four owner-specific extra tails, `LIST_METHODS`/`ARRAY_EXTRA_TAIL`/`LIST_EXTRA_TAIL`,
  `HASH_METHODS`, `RANGE_METHODS`/`RANGE_EXTRA_TAIL`, `CODE_METHODS`, `SIGNATURE_METHODS`,
  `IO_PATH_METHODS`, `IO_HANDLE_METHODS`, `COOL_OWN`/`COOL_NATIVE_INT_COERCE_TAIL`, `ANY_METHODS`,
  `MU_METHODS`, `BUF_METHODS`/`BUF_EXTRA_TAIL`) plus the test-only `METHOD_UNIVERSE` and its now-
  moot `native_responds_to` helper (`builtin_sample_value`/`native_method_arities` stay: they still
  back `native_method_row.rs`'s inverse-probe tests, an unrelated concern). Mechanism: added a
  4th `NativeRowFlags` bit, `INTROSPECTABLE`, and set it on exactly the `RAW_ROWS` rows whose
  `(folded owner, name)` was a member of the OLD hand-written arrays -- computed once via a
  throwaway `#[test]` (comparing the live pre-deletion `builtin_type_method_names` output against
  every `RAW_ROWS` row and dumping the 652 matched pairs), then baked into
  `native_method_row_table.rs` with a small Python script (the table is `#[rustfmt::skip]`, hand-
  editing 652 of 1108 rows was not viable). `builtin_type_method_names` is now three lines:
  fold the owner, then read `native_method_row::introspectable_names_for_owner(folded)` -- RAW_ROWS
  order for the introspectable subset is guaranteed to match the old arrays' order (pinned by
  `raw_rows_cover_every_introspection_name_in_order`, which stays as a construction-time regression
  guard rather than an independent cross-check now). Zero behavior change intended: `t/can-methods-
  drift.t` (193 assertions), the full `t/` suite (3167 files, release), and every `S12-introspection`/
  `S02-types/{hash,array,list,range}.t`/`S09-typed-arrays/hashes.t`/`S32-{str,num}/*`/`S32-container/
  buf.t`/`S03-operators/buf.t` roast file stayed green. **F3 is now closed** -- the per-type name
  lists ANALYSIS §4-1 called out are gone; `RAW_ROWS` (already the dispatch-admission source since
  E4b) is also the sole `.^methods` source.
- [ ] **F4 — Remove `ClassDef::methods` as a dispatch/registration mirror.** Leave type structure
  metadata beside the canonical method table and update snapshots/rollback to copy one source.
- [x] **F5 — Remove superseded method caches and manual invalidation.** Keep only the
  generation-keyed resolved-call cache plus data caches that type mutation cannot invalidate.
  The inventory this box retires: ~72 manual clear sites across 12 files (the 32 in
  `vm_module_ops.rs` are four copies of one block and are a trivial first PR), the `String`-keyed
  `private_zeroarg_method_cache` with nine hand-clear sites of its own, and the *second*
  generation scheme `fn_resolve_cache_gen` that drives block-scope-exit clears in
  `accessors_misc.rs`. `native_ctor_plan_cache` is not "unrelated": it is cleared in lockstep
  with `fast_method_cache` at every one of those sites and must adopt the same generation.

  **Progress (#6422):** the trivial-first-PR slice this box called out is done — the duplicated
  clear block at all 7 non-generation-gated sites (module load/import/no/need, block-scope exit,
  sub registration, class/role/enum registration) is now one shared
  `Interpreter::invalidate_method_dispatch_caches()` (`src/vm/vm_dispatch_cache_invalidate.rs`).
  This is pure dedup, not the generation migration.
  **Progress (#6425):** `func_multi_resolve_cache`/`func_multi_type_cacheable` (plain multi *sub*
  dispatch, read by `resolve_function_multi_cached`) are now generation-guarded at their own read
  site (`refresh_func_multi_caches_for_generation`, keyed on `fn_resolve_gen`, mirroring
  `refresh_method_caches_for_generation`) — closing a real staleness gap the eager clear alone did
  not cover (`fn_resolve_gen` is bumped at ~15 sub/multi-registration sites that never called
  `invalidate_method_dispatch_caches`). Their `.clear()` calls in that function are now redundant
  for correctness and kept only to drop the maps' allocated capacity promptly.
  **Progress:** `private_zeroarg_method_cache` now also refreshes at its own read site
  (`resolve_private_method_any_owner`, via the existing `refresh_method_caches_for_generation`,
  keyed on `Registry::method_generation` — the same one `#[6420]`'s `.wrap`/`.is_dispatcher` work and
  the class/role/augment registration paths already bump). This closed a real gap: only one of the
  five call sites that read the cache (`resolve_private_method_for_vm`) went through the refreshing
  entry point; the other four (`methods_call_dispatch.rs`, `methods_signature_shaped.rs`,
  `methods_instance_ops.rs` ×2) called `resolve_private_method_any_owner` directly and depended
  entirely on the nine eager `clear_private_zeroarg_method_cache()` call sites, same generation-blind
  shape the `func_multi_*` pair had before #6425. Those nine eager clears are now redundant for
  correctness (kept only to drop the map's capacity promptly, same as `func_multi_*`).
  **Correction (verified 2026-08-14, do NOT act on the claim above without re-reading this):** the
  "redundant for correctness" claim two sentences up is WRONG for at least three of the nine sites.
  It silently assumed every site's enclosing operation reaches `sync_user_method_entries` (the only
  thing that bumps `Registry::method_generation`, which the read-site refresh keys on) the way
  class registration does. Read by hand: `register_role_decl` (`registration_role.rs:329`) never
  calls `sync_user_method_entries` anywhere in its body -- a role's methods are never synced into
  `method_entries` at declaration time, only later when a *class* composes/puns the role.
  `ensure_role_punned_to_class` (`registration_class_augment.rs:1028`) likewise never calls it.
  `augment_class` (`registration_class_augment.rs:71`) only reaches it conditionally, through
  `compose_role_into_augmented_class`, and only when `does_roles` is non-empty -- the common case,
  a plain `augment class C { method foo {...} }` with no `does`, never bumps the generation at all.
  For these sites the eager `clear_private_zeroarg_method_cache()` is the ONLY invalidation the
  private-zeroarg cache gets and is genuinely load-bearing; removing it would be a real staleness
  bug (a stale private-method resolution served after a role declaration/pun/plain-method augment),
  not a cleanup. The other sites in the original nine may or may not be safe -- `withdraw_role_pun`
  (`methods_object_dispatch_new.rs:202`) calls `sync_user_method_entries` on the line immediately
  before its clear, so that one specific call is provably redundant, but the remaining ones
  (`registration_class_body_attr.rs:66`, `registration_class_augment.rs:611,773`,
  `registration_class_decl.rs:114`, `types/role_mixin_class.rs:211`) still need the same per-site
  trace this correction just did before any of them are touched. **Do not remove any of these nine
  calls as a batch based on the earlier note.**
  **Correction:** the box text above describes `fn_resolve_cache_gen` as "the second generation
  scheme ... that drives block-scope-exit clears in `accessors_misc.rs`" — checked directly and this
  is stale/inaccurate. `fn_resolve_cache_gen` (`vm_call_resolve.rs`'s `find_compiled_function_inner`)
  is not a second scheme at all: it's compared against the *same* `fn_resolve_gen` counter
  `invalidate_method_dispatch_caches` bumps, `fn_resolve_cache` has zero eager manual clear sites
  anywhere in the codebase, and nothing named `fn_resolve_cache_gen` exists in `accessors_misc.rs`.
  This pair is already fully sound; nothing to migrate here.
  **Still open:** `method_resolve_cache`/`fast_method_cache`/`native_ctor_plan_cache` remain
  eager-cleared at `invalidate_method_dispatch_caches`'s 7 call sites, guarded by a *different*
  counter (`fn_resolve_gen`) than the one their own read-site refresh
  (`refresh_method_caches_for_generation`) uses (`Registry::method_generation`) — unlike the two
  pairs above, unifying these needs auditing whether `Registry::method_generation` is actually
  bumped at all 7 of those sites (some, e.g. plain sub/module registration, are function-registry
  events that may not touch the method registry), not just adding a read-site refresh call.
  **Progress:** `method_resolve_cache`/`fast_method_cache` already called
  `refresh_method_caches_for_generation()` at their own read sites (`resolve_method_cached`,
  `try_compiled_method_or_interpret_inner`'s fast-cache probe) — only `native_ctor_plan_cache`'s
  read site (`native_ctor_plan`, `methods_object.rs`) was missing it, the same gap
  `private_zeroarg_method_cache` had before #6420-adjacent work. Fixed: `native_ctor_plan` now
  self-refreshes on `Registry::method_generation` first, same as the other two. This closes one
  concrete staleness path (a `Registry::method_generation` bump — e.g. `.^add_method`'s
  `sync_user_method_entries` — reaching `native_ctor_plan_cache` even at a call site with no
  explicit `.clear()` of that cache) without yet answering the still-open audit question above:
  the 7 `invalidate_method_dispatch_caches` call sites were separately traced by hand — module
  load/import/no/need (`vm_module_ops.rs`) and class/role/enum registration
  (`vm_typedecl_ops.rs:116`) plausibly reach `sync_user_method_entries` transitively for any
  class/method content they carry, but plain `sub` registration (`vm_register_sub_ops.rs:316`) and
  block-scope-exit routine-registry restore (`accessors_misc.rs:351`) confirmed do **not** bump
  `Registry::method_generation` — and the latter is a genuine case where the eager clear is
  load-bearing today (it restores `token_defs`, and grammar `token`/`rule` bodies are methods, so a
  block-scoped grammar's token set changing must invalidate method caches, but nothing in that
  restore path touches `Registry::method_generation`). The 7 eager clears therefore stay; only the
  read-site gap closed above was safe to land without a wider generation-bump audit.
  **Progress (class registration traced):** confirmed (by reading, not a shadow-check sweep)
  that `vm_typedecl_ops.rs:116`'s preemptive clear — issued *before* `register_class_decl` runs,
  precisely so a same-named class redeclaration cannot serve a stale resolution from the old
  class — is followed, on every live code path through `register_class_decl`, by an unconditional
  `sync_user_method_entries` call that bumps `Registry::method_generation`: `publish_class_shell`
  calls it right after inserting the class (both the stub and non-stub branches), and
  `finalize_class_registration` calls it again after the composed body lands. The only early
  return that skips both (`is_stub_body` re-declaring an already-non-stub class) is a true no-op —
  the class is left completely unchanged, so no invalidation is needed for it either way. So by
  the time this opcode returns, `Registry::method_generation` has already advanced past whatever
  the preemptive clear was defending against, and the three read-site-refreshed caches
  (`method_resolve_cache`/`fast_method_cache`/`native_ctor_plan_cache`, now all three
  self-refreshing per the progress note above) would pick that up on their own. This is a strong
  signal the `vm_typedecl_ops.rs:116` clear is redundant for those three specifically — but it is
  *traced*, not shadow-verified against the full corpus the way E1a/E4a's cutovers were, and role
  registration (`exec_register_role_op`) and enum registration (`exec_register_enum_op`) were not
  re-traced here (neither currently calls `invalidate_method_dispatch_caches` at all, so they are
  out of scope for this specific box regardless). Removing the line is deliberately deferred to a
  dedicated shadow-check slice rather than done on trace evidence alone. `vm_module_ops.rs`'s four
  sites (module load/import/no/need) remain fully untraced.
  **Progress (shadow-check landed, #6448):** `exec_register_class_op` ran the actual verification
  the trace above called for, `MUTSU_VM_STATS`-gated and zero behavior change: it snapshotted
  `Registry::method_generation` before the eager `invalidate_method_dispatch_caches()` call and
  again right after a successful `register_class_decl` returns, and recorded a mismatch (via
  `record_class_reg_gen_shadow_check`, mirroring `record_deferral_shadow_check`) whenever the
  generation did *not* advance.
  **Progress (corpus evidence + cutover, #6452):** a `MUTSU_VM_STATS=1` sweep of the full `t/`
  suite (debug build, one process per file) recorded 1296 shadow checks with exactly **1**
  mismatch (`t/eval-private-and-stubs.t`, `class=Base is_stub=true`); a second sweep over the
  class/role/multi-heavy roast whitelist subset (`S12-*`, `S14-*`, `S06-multi`, `S02-types`,
  `S04-declarations`) recorded **5** mismatches, all `is_stub=true`. Every mismatch across both
  sweeps was the one known-benign shape traced above (a stub re-declaring an already-non-stub
  class of the same name — a true no-op, so no invalidation was needed for it either). With that
  corpus evidence in hand, the eager `invalidate_method_dispatch_caches()` call at
  `vm_typedecl_ops.rs:116` is removed for the class-registration path (`exec_register_class_op`
  only — role/enum registration and `vm_module_ops.rs`'s four module sites are untouched and
  still out of scope per the trace above). The shadow check itself is kept in place as a
  standing regression guard (same pattern as other ADR-0019 boxes' post-cutover assertions):
  it still fires on every class registration under `MUTSU_VM_STATS=1` and would flag any future
  change that lets a real class mutation through without bumping the generation.
  **Progress (`vm_module_ops.rs` shadow-check + cutover):** the four module-op sites left
  untraced above (`exec_use_module_op`/`exec_import_module_op`/`exec_no_module_op`/
  `exec_need_module_op`) got the same shadow-check-then-cutover treatment. Reasoning: a module
  load can install classes and subs, but each installation already invalidates dispatch caches at
  its OWN registration site regardless of the outer module op -- `exec_register_class_op` bumps
  `Registry::method_generation` unconditionally on every real change (established above), and
  `exec_register_sub_op` (`vm_register_sub_ops.rs:316`) calls `invalidate_method_dispatch_caches()`
  itself, including its unconditional `fn_resolve_gen` bump, on every actual install (skipped only
  for an idempotent re-registration of an already-installed identical sub -- confirmed by reading
  `SubRegisterOutcome::Installed` gating). So the module-op's own eager call was a second,
  redundant layer on top of per-declaration invalidation already happening deeper in the same call
  stack. A `MUTSU_VM_STATS`-gated shadow check (mirroring `record_class_reg_gen_shadow_check`)
  confirmed this: a full `t/` suite sweep (debug, one process per file) recorded 4049 checks with
  164 generation bumps, and a roast-whitelist sweep (release) recorded 2479 checks with 89 bumps --
  every bump traced to a `use`/`need` of a module that genuinely installs a class (e.g.
  `OO::Monitors`, `Cro::*`, `URI::DefaultPort`), and `import`/`no` (exercised in 6+9 and 2 files
  respectively across both corpora) never bumped, consistent with those ops not re-running a
  module's class declarations. With that evidence, the eager `invalidate_method_dispatch_caches()`
  call is removed from all four sites; the shadow check remains as a standing regression guard.
  `make test` (3164 files, all green) and `make roast` confirm no behavior change.
  **Progress (`exec_register_sub_op` cutover):** the remaining audited-but-unresolved call site,
  `vm_register_sub_ops.rs:316` (plain `sub` installation), is also cut over -- by construction, not
  by corpus sampling. Its eager `invalidate_method_dispatch_caches()` clears two disjoint cache
  families: the *function*-namespace ones (`func_multi_resolve_cache`/`func_multi_type_cacheable`
  and the light/otf/multi-candidates call caches), all guarded by `fn_resolve_gen`; and the
  *method*-namespace ones (`method_resolve_cache`/`fast_method_cache`/`native_ctor_plan_cache`/
  `multi_resolve_cache`/`multi_type_cacheable`/`resolved_seq_cache`/`dispatch_multi_candidate`),
  all keyed on `(owner type, method name)`. A bare `sub` is never a method-table entry under any
  key those caches use -- `register_compiled_sub_decl` is called only from this site and
  `run_prelude.rs`'s prelude-sub bootstrap, never from method/class registration -- so it can never
  make a method-namespace cache stale, unlike a class/method declaration. Direct precedent: the
  fast re-install path just above this call (the `prepared_fn_defs` branch, for a `my sub`
  re-entering its declaring block) already only bumps `fn_resolve_gen` for the identical "install a
  sub" event, with no method-cache clear at all. The call is replaced with a bare `fn_resolve_gen +=
  1`. `make test` (3164 files, all green) and `make roast` (1436 files, all green) confirm no
  behavior change.
  **Progress (`accessors_misc.rs:351` cutover, block-scope-exit routine-registry restore):** this
  was the one site earlier progress notes confirmed genuinely load-bearing -- it restores
  `token_defs`, and grammar `token`/`rule` bodies are methods, so a block-scoped grammar's token set
  changing must invalidate method caches, but the site never bumped `Registry::method_generation`.
  Rather than leave the eager `invalidate_method_dispatch_caches()` call as a permanent residual,
  the restore now calls the newly-`pub(crate)` `Registry::bump_method_generation()` unconditionally
  at the same point it already unconditionally bumps `regex_parse::TOKEN_DEFS_GEN` (both fire on
  every wholesale `token_defs` rewrite, changed or not) and separately bumps `fn_resolve_gen` for
  the function/sub-namespace half of the restore. This is a by-construction cutover, the same
  reasoning as `exec_register_sub_op`'s: `invalidate_method_dispatch_caches()` used to clear every
  cache in both namespaces unconditionally at this call site, and both namespaces' caches already
  self-refresh off their own generation counter at their own read sites (established earlier in
  this box for the method-namespace caches, and by `func_multi_resolve_cache`/
  `func_multi_type_cacheable`'s existing `fn_resolve_gen`-keyed refresh for the function-namespace
  ones) -- splitting one unconditional clear into two unconditional generation bumps cannot
  introduce staleness. `invalidate_method_dispatch_caches()` itself had no remaining callers after
  this cutover and was deleted outright (`src/vm/vm_dispatch_cache_invalidate.rs`), closing F5's
  entire eager-clear inventory. Verified with the full `t/` suite (3166 files) and a 169-file
  class/role/multi/grammar/eval roast subset, both green. **This closes ADR-0019's F5 box**: the
  box's own inventory (~72 duplicated manual clear sites, the `String`-keyed
  `private_zeroarg_method_cache`'s nine hand-clear sites, and the `native_ctor_plan_cache` lockstep
  gap) is fully accounted for -- every eager manual-invalidation call site this box named has either
  been proven redundant and removed, or converted to a generation bump consumed by a self-refreshing
  read site. No manual cache-clear call site remains anywhere in the method/function dispatch path.
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

This rejection is about a *second source of truth over which methods exist and how they dispatch* —
not about all hand-authored data everywhere. F1's native-metadata work (declaring `.package` and
`.signature.gist` fidelity) hits real Rakudo facts with no in-repo derivation, since Rakudo's own
native methods are hand-written Raku signatures mutsu reimplements in Rust with no signature to
read them from. That is declaration metadata, not a competing existence/dispatch catalog, and F1
attaches it as optional columns on the single already-generated `NativeMethodRow` catalog (one key,
one row) rather than a second `(owner, name)` structure — see
`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`'s "Decision (2026-08-14)" for the full
reasoning and sequencing.

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

The checklist above ("Execution plan and progress") is the authoritative, currently-maintained
record of what has landed. Phases A-D and E1, E3-E11, F5 are closed; E2 (open cleanup, no longer
gating), the rest of Phase F (F1-F4, F6, F7), and the completion gates are the remaining open work —
see their entries above for
current status and the linked `todo/deep/adr0019-*.md` design docs for full design and slice
history. Individual accomplishments
are additionally recorded per-PR under `news/2026-08/`.
