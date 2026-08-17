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
still index `stmt_pool` and are migrated by slice C8; the top-level `RegisterToken` opcode is
migrated by F7 (its own `CompiledTokenDeclPlan` keeps the *regex* body as an opaque payload — that
body's execution model is ADR-0009's, waived here the same way enum/subset are), while the
`ClassBodyOp`/`RoleBodyOp` `TokenRule` arm inside class/role/grammar bodies remains a raw-`Stmt`
carve-out, scoped together with the grammar-token work in C6d-2 and Phase D's token note.

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
E1, E3-E11 are all closed). Phase F has started: F3, F4 (all of F4a/F4b/F4c), and F5 are closed;
F1/F2 are done except a deliberately-parked fidelity slice; F6 is closed (with an amended
completion criterion — see its entry); F7 is closed (with a role-body permanent-exception carve-out —
see its entry); only the completion gates (G1-G4) remain open. See each
box's entry below for its
own status, and
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

  **Scoping (2026-08-17, read-only, no code): investigated whether `legacy_body` can now be
  dropped — conclusion is RETAIN, not a later box.** D9 already removed the same-shaped
  `CompiledRoleDeclPlan::legacy_body` field this bullet's "following its own precedent" language
  refers to, so `CompiledProtoDeclPlan::legacy_body` is now (`git grep` confirmed) the LAST
  `legacy_body`-shaped payload anywhere in the codebase — the obvious next target once F7 closed
  the token/rule carve-out. Traced every real reader, which turns out to be wider than this
  bullet's own two-name list:
  1. **Registration-time fact scans** (`register_proto_decl`'s `auto_signature_uses`/
     `is_stub_routine_body` over `body`) are movable to compile time (D2a's precompute-facts
     precedent), but alone that would not free the field's storage — three other consumers below
     still need the raw `Vec<Stmt>` kept on `FunctionDef` itself.
  2. **`Value::make_sub(...)` in `exec_register_proto_sub_op`** clones `body` into a `Sub` value
     when applying a custom `trait_mod:<is>`; that `Sub` is a first-class callable a user program
     can hold and invoke later through the ordinary Sub-calling convention, which tree-walks.
  3. **`call_proto_function`** (reached from `call_function_fallback`, one call site,
     `builtins_operators_fallback.rs`) is NOT dead or rare: the VM's opcode-level call dispatch
     (`vm_call_func_ops.rs`) always tries the bytecode-first proto paths
     (`vm_resolve_trivial_proto_candidate`, `vm_try_run_nontrivial_proto_body`) FIRST and falls
     through to `call_function_fallback` only for what those explicitly decline (a winning
     candidate that is not OTF-compilable — `where`/default/code-signature params, unsafe
     `state`, an `is_interpreter_handled_function` name, or a hand-built `FunctionDef` outside
     plan registration). `call_proto_function` IS the correct, necessary interpreter fallback for
     exactly the cases the VM fast path already filtered out — this bullet's original framing was
     right, not an unverified assumption this time.
  4. **`run_proto_method`** (two call sites: `.new` dispatch and general `proto method` calls in
     `methods_object_dispatch_new.rs`) is the ONLY dispatch mechanism for `proto method`/`proto
     submethod` bodies — there is no bytecode path to prefer it over. Unlike an ordinary proto,
     `is_method` protos are unconditionally excluded from `compile_sub_body` at plan-lowering time
     (`compiler/stmt.rs`'s `if !*is_method && !trivial` gate) — giving a proto method's `{*}` body
     its own compiled routine is a **capability that has never been built**, not a migration of an
     existing one (the code's own comment calls this "Phase D territory"). That is a fresh
     Phase-D/E-sized dispatch box on its own, not a cleanup of this one.

  **Conclusion, mirroring F6's qualified-dispatch retain finding: keep `legacy_body`
  permanently.** Every consumer traced one level deeper is either load-bearing (2/3/4) or
  insufficient alone to free the field (1). Revisit only if proto-method bytecode compilation is
  ever built as its own box; until then this is a permanent, justified compatibility payload, not
  deferred work.

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
  Full slice-by-slice history and the D2b/D2c-remainder design lived in
  `todo/deep/adr0019-d2c-attribute-default-chunks.md` and
  `todo/deep/adr0019-d2-remainder-attr-plan-lowering.md`; both retired once D2's own progress
  notes above and the linked `news/2026-08/` entries fully covered the same ground.
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
for this phase were `todo/deep/adr0019-e1-typeid-receiver-owner.md` (E1, retired once closed —
its one live spin-off, mixin composition order nondeterminism, was tracked separately as
`todo/tickets/mixin-role-order-not-tracked.md` and is now fixed, see
`news/2026-08/mixin-role-application-order-tracked.md`),
`todo/deep/adr0019-e2-e4-resolver-core.md` (E2/E3/E4),
`todo/deep/adr0019-e5-e7-entry-routing.md` (E5/E6/E7), and
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md` (E8/E9/E10/E11) — consult the ones that
still exist for full slice-by-slice history; the checklist below keeps only the architectural
outcome.

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
  - [x] **E4b — authoritative switch at `should_bypass_native_fastpath` (`call_method_with_values`'s
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
  **(2026-08-17): E4b's own checkbox corrected to `[x]`** — the 2026-08-12 "E4 is marked done"
  progress note above had already declared this sub-box closed (same pattern as E5's own
  checkbox-correction note), but the checkbox itself was left unchecked. Retired
  `todo/deep/adr0019-e4b-should-bypass-native-fastpath-decomposition.md`: its own "status update"
  section (an earlier snapshot from the same day) framed a few category-1 guard groups as "still
  open, same methodology applies" candidates for removal, but category 1's paragraph above (a
  later pass the same day) already confirms `Supplier`, most of `Proc::Async`, `IO::Handle`'s
  three-method group, and `Stash.AT-KEY` landed as outright deletions, with only `Supply`'s list
  vocabulary/lazy-`Match` forcing/`Hash.keys` staying as permanent hazards — superseding the
  design doc's older framing rather than leaving genuine unfiled work behind. Any remaining sliver
  (the unnamed rest of `Proc::Async`, `Stash.keys`/`.values`) is small enough to re-derive from
  `native_fastpath_receiver_state_guard` directly if ever revisited, not worth a standalone ticket.
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
  **Update (2026-08-15, F1 mechanism slice, `.signature` default):** `make_native_method_object`
  hardcoded every native `Method` Instance's `.signature` as an empty `Signature()` — `.^methods`/
  `.^method_table` on any built-in type answered zero params regardless of the method's real arity.
  A raku ground-truth sweep of ~280 introspectable (owner, name) pairs found no single shape
  dominates real Rakudo's native signatures (raw-capture `(Owner $:: |)`, generic named-catchall
  `(Owner:D $:: *%_)`, and fully-typed explicit params were all common, with no pattern derivable
  from `NativeArityMask` alone), so this slice synthesizes the plurality shape — `(Owner $:: |)`, an
  invocant plus a raw capture — as the generic default (`crate::value::signature::
  synthesize_native_signature`), replacing the hardcoded empty signature. Not exact parity by
  design; per-method overrides are the fidelity slice's job. Surfaced (but did not fix, filed
  separately as `todo/tickets/signature-arity-count-wrong-for-capture-params.md`) a pre-existing,
  general bug: `Signature.arity`/`.count` are wrong for any signature containing a raw-capture
  param, reproducing on plain user-declared subs too, not specific to this change. Pin:
  `t/classhow-native-method-signature-default.t`. The Sub-vs-Instance representation unification for
  `.^lookup`/`.^find_method` remains open — that surface still returns a `Sub`-shaped value with its
  own, separate `.signature` rendering path, untouched by this slice.
  **Update (2026-08-15, F1 mechanism slice, Sub-vs-Instance unification, closes the ticket):**
  `.^lookup`/`.^find_method` now return the same `Method`/`Submethod` `Instance`
  `.^methods`/`.^method_table`/`.^can` build, for all four cases `classhow_lookup_impl` handles (user
  class method, role method, attribute accessor, native/builtin method incl. grammar tokens).
  `.wrap`'s tag reuse needed no change (the `Instance` shape already carried the same
  `__mutsu_lookup_*` tags `.^methods(:local)` used); direct callability (`$m(invocant, args)`, real
  Raku's implicit `CALL-ME`) is preserved via a hidden `__mutsu_method_callable` attribute plus one
  new `CALL-ME` handler, not a general "make an Instance callable everywhere" capability — the two
  blockers the ticket worried about were both smaller than feared. `.multi`/`.rw`/`.readonly`
  (missing entirely from the `Instance` shape) were added as a byproduct. Found and fixed five real
  bugs along the way (missing invocant in `Method.signature`, `.candidates` on a non-multi method,
  cross-class multi-family `.candidates` combination plus a pre-existing per-candidate wrap-index
  bug, `.WHY` on a Method Instance, and dynamic hyper dispatch on a method value) — full detail in
  `news/2026-08/classhow-lookup-method-instance-unification.md`. Pin:
  `t/classhow-lookup-method-instance-callable.t`. F1's only remaining open piece is the fidelity
  slice (per-native-method `.signature`/`.package`/`.is_dispatcher` override columns), correctly idle
  until a real assertion demands a specific override (per the 2026-08-14 decision above).
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
- [x] **F4 — Remove `ClassDef::methods` as a dispatch/registration mirror.** Leave type structure
  metadata beside the canonical method table and update snapshots/rollback to copy one source.
  **Split in place (2026-08-15), following the C6/D2/E1-E11 precedent** — a read-site
  classification pass (`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`'s sibling
  survey, and an earlier scoping pass -- both now folded into this box's own progress notes below,
  which fully supersede that scoping's now-deleted file) found `Registry::sync_user_method_entries`
  currently writes the canonical table FROM
  `ClassDef::methods` (the opposite of what F4 wants), with ~15-20 files of live dispatch/MOP/
  BUILD-TWEAK read sites and ~10 files of write sites. That work does not fit one PR or one
  design decision, so it is now three sub-boxes:
  - [x] **F4a — Decide and implement the role-owner read policy.** `Registry::method_entries` has
    no row at all for a role that is never `.new`-punned (the common case — a role only ever
    `does`-composed into a class, never instantiated directly): see
    `todo/deep/method-entries-never-covers-unpunned-roles.md`. **Do NOT close this gap by
    populating role-owner rows into `Registry::method_entries` through the shared
    `sync_user_method_entries`/`get_method_overloads` write-and-read path** — tried exactly that
    on 2026-08-15 (mirror the class branch, populate on role-registration-finish) and it
    regressed composed-role multi-method `.*`/`.+` dispatch
    (`resolve_methods_per_mro_level`'s all-or-nothing `any_failed` gate, an existing landmine
    unrelated to this box, treats a newly-visible role-owned MRO level as a hard requirement even
    when its candidates are already fully represented via a more-derived flattened class level —
    reverted, PR #6478, repro and full trace in the ticket). The general lesson: **when a change
    writes through a function every dispatcher reads, the risk inventory is every transitive
    reader of that function, found by grep — not just the call sites a ticket happens to name.**
    The gap ticket's own "where this actually bites today" section named `ctor_phase_plan.rs:133`,
    `vm_call_method_compiled_cache.rs:97`, and `resolution_private_method.rs`'s three call sites as
    the production readers that need the fallback (`resolve_all_methods_with_owner`, the deferral/
    `nextsame`-`callsame` chain walker, was separately confirmed already correct — it reads
    `Registry::roles` directly today, bypassing `get_method_overloads` entirely, so it does not
    need migrating). It did NOT name `resolve_method_with_owner_impl`'s own per-level walk
    (reached transitively through `resolve_methods_per_mro_level`) — that is the one that broke.
    A separate same-day read-site survey (done for this split, informal, not yet raku-verified)
    additionally flagged `methods_qualified.rs`, `methods_classhow_lookup.rs`,
    `methods_classhow_dispatch.rs`, `accessors_state.rs`, `methods_walk.rs`, and
    `class_introspection.rs:262` as reading a similar class-vs-role fallback shape for `.^lookup`/
    `.WALK`/qualified dispatch/introspection — candidates for the same helper, but each needs its
    own confirmation of exactly what it reads and whether it is winner-selection-adjacent (like the
    site that broke) before being added to this box's scope, not assumed safe by pattern-match.
    Fix direction: add an explicit `role_method_overloads(owner, name)` helper reading
    `Registry::roles` directly (role method definitions are composition inputs, not dispatch
    entries — the dispatchable form is always the flattened copy on the composing class), and
    migrate confirmed-safe call sites to consult it as an explicit fallback, one consumer family
    per sub-PR, each raku-verified — the same discipline E1a/E4a/E7 already used successfully.
    Winner selection (`resolve_method_with_owner_impl`, `resolve_methods_per_mro_level`) must NOT
    call this helper.

    **Progress (private-method family, #TBD):** added `Registry::role_method_overloads(owner,
    name)` (reads `Registry::roles` directly, filtered non-empty like `user_method_overloads`)
    and `Registry::get_method_overloads_with_role_fallback` (`get_method_overloads(...).or_else(||
    role_method_overloads(...))`). Before wiring either into production, gathered corpus evidence
    with a `MUTSU_VM_STATS`-gated pure probe at the three named `resolution_private_method.rs`
    sites (`resolve_private_method_with_owner`, `resolve_private_method_any_owner`,
    `private_method_candidates_by_name`): each site's own MRO walk already probed
    `role_method_overloads` whenever its plain `get_method_overloads` call came back empty, purely
    to count how often the fallback would have found something extra — never consulted for a real
    answer. A full local `t/` sweep (3173 files) plus a full `roast-whitelist.txt` sweep (1436
    files), one process per file, `MUTSU_VM_STATS=1`, recorded a combined 41 opportunities (the
    plain lookup came back empty at some `cn` reached in an MRO walk) and **zero** hits — the role
    fallback never once found anything beyond what `get_method_overloads` alone already returned.
    This matches the theory: private methods are private specifically to their declaring
    package, so a role's own private method is only reachable either (a) already flattened onto
    the composing class (the common `does` case, where the class-level entry wins before the walk
    ever reaches the role's own MRO position) or (b) through an owner-qualified `self!R::m()` call,
    which requires an explicit `trusts` declaration Raku itself gates at compile time before
    dispatch is ever attempted — so the un-punned-role gap this box's ticket describes structurally
    cannot surface through these three sites' own call shape. With that evidence, cut the three
    sites over from `get_method_overloads` to `get_method_overloads_with_role_fallback` for real
    (removing the now-served-its-purpose probe): a documented no-op over the entire corpus, and a
    correctness fix for any case the corpus doesn't exercise. Verified with the full local `t/`
    suite (3173 files) and a 64-file `S04`/`S06`/`S09`/`S12`/`S14` roast subset, both green
    (`S14-roles/versioning.t` flaked once under `-j4` parallel load, passes individually and via
    `prove -e`, the same known shape other boxes' progress notes have already hit). Remaining sites
    from the ticket's confirmed list: `ctor_phase_plan.rs:133` needs no change — traced separately
    (see F4b's own progress note) to be structurally unreachable with a role receiver, since its
    only caller already filters to a real class first; `vm_call_method_compiled_cache.rs:97` is
    still open, a separate consumer family for its own sub-PR. The informally-flagged sites
    (`methods_qualified.rs`, `methods_classhow_lookup.rs`, `methods_classhow_dispatch.rs`,
    `accessors_state.rs`, `methods_walk.rs`, `class_introspection.rs:262`) remain unconfirmed and
    out of scope, per the box's own rule above.

    **Progress (`vm_call_method_compiled_cache.rs:97` family, closes F4a's named-site list):**
    unlike the private-method family, this site's `MUTSU_VM_STATS`-free corpus probe (a temporary
    `eprintln!` gated on a throwaway env var, not committed) found the fallback is NOT a no-op here:
    a full local `t/` sweep (3175 files, debug) recorded 162 opportunities where `multi_dispatch_
    type_cacheable`'s own `class_mro` walk reached a role's own MRO slot with no `method_entries`
    row (an un-punned role composed via `does`, e.g. diamond compositions in `S14-roles` fixtures),
    and a `roast-whitelist.txt` sweep (release, 1436 files) recorded 102 more — role names really do
    appear as their own `class_mro` entries in role-heavy code, unlike the rare private-method call
    shape. Reasoning for why this is safe to cut over despite the non-zero hit rate: the walk only
    *accumulates* `any_multi`/`value_dependent` across every MRO level (no early return on first
    match), so the role fallback can only ever ADD information the plain lookup missed, never
    remove any the class-level flattened copy already contributed — it can flip `any_multi`/
    `value_dependent` from false to true, never the reverse. A false-negative `value_dependent`
    (a `where`/literal/rw/signature-shaped candidate that lives only on the un-punned role, invisible
    to the old code) is the dangerous direction: it would let the type-keyed `multi_resolve_cache`
    memoize a resolution that is not actually type-deterministic. This box's own explicit
    prohibition on winner selection consulting the fallback is respected — `resolve_via_sequence_
    cache` (the actual resolver both cache paths 3 and 4 call) is untouched, so the resolved value
    for any given call is unaffected either way; only the caching *gate* changes, and only toward
    being more conservative about what it treats as type-cacheable. Cut over
    `get_method_overloads` -> `get_method_overloads_with_role_fallback` at this one site. Verified
    with the full local `t/` suite (3175 files, all green) and a 312-file `S04`/`S06`/`S09`/`S12`/
    `S14` roast subset (release, all green) — the multi/role-heaviest synopses, chosen to exercise
    this exact cacheability gate. `cargo clippy -- -D warnings` (the pre-commit hook's own
    invocation) is clean; a separate `--all-targets` clippy run surfaces 3 pre-existing lint
    failures in unrelated files (`match_lazy.rs`, `vm_jit_layout.rs`), confirmed present on `main`
    before this change and out of this box's scope. This closes every site F4a's own gap-ticket
    named; the informally-flagged sites above remain unconfirmed and out of scope per the box's own
    rule.

    **Progress (informally-flagged sites, triaged):** read all six: `methods_qualified.rs`,
    `methods_classhow_lookup.rs`, `accessors_state.rs`, `methods_walk.rs`, and
    `class_introspection.rs:262` (`has_user_method_including_role`) each ALREADY implement their own
    explicit class-then-role fallback (`registry().classes.get(cn)...methods.get(name)` followed by
    a separate `registry().roles.get(cn)...methods.get(name)` check, the same shape `.^lookup`'s
    unification already established) — none of them has the F4a gap; their remaining work is purely
    reading the `class_def.methods`/`role_def.methods` fields directly instead of the canonical
    table, which is F4c's "invert the write direction" scope, not F4a's. `methods_classhow_dispatch.
    rs`, the sixth, does NOT have its own role fallback at the one flagged line (750, inside
    `^add_method`'s "clone the whole multi candidate family when aliasing a `^find_method`/`^lookup`
    carrier" helper) — and unlike the other five, this is a REAL, raku-confirmed gap, not just an
    F4c-shaped field read: `role R { multi method m(Int $x){...}; multi method m(Str $x){...} };
    class C {}; C.^add_method('n', R.^find_method('m'))` (the role never punned, never composed
    anywhere) loses every multi candidate but the carrier's own on mutsu, while real Rakudo keeps
    the whole family — confirmed by direct `raku` comparison. Fixed by swapping the site's
    `classes.get(src_class).and_then(|cd| cd.methods.get(src_method))` to
    `get_method_overloads_with_role_fallback(src_class, src_method)` (also moves it onto the
    canonical table as a byproduct, matching the `vm_call_method_compiled_cache.rs` cutover above).
    Regression-pinned in `t/add-method-alias-unpunned-role-multi.t`. Verified with the full local
    `t/` suite (3176 files) and the same 312-file `S04`/`S06`/`S09`/`S12`/`S14` roast subset, both
    green. This closes F4a's entire informally-flagged list — one confirmed real gap fixed, five
    confirmed already-safe and reclassified into F4c.
  - [x] **F4b — Cutover the class-level-only read clusters.** The read sites that never touch a
    role at all (`methods_object.rs`'s six BUILD/TWEAK existence checks, `metamodel.rs`,
    `class_introspection.rs:39`, `ctor_phase_plan.rs:67,103`) move from `class_def.methods` to
    `MethodEntry.user_candidates` directly. The sites F4a confirms need a role fallback (`ctor_
    phase_plan.rs:133`, `vm_call_method_compiled_cache.rs:97`, `resolution_private_method.rs`, and
    whichever of the informally-flagged sites above F4a confirms in scope) move together with
    F4a's own migration, not as separate F4b work. All cutovers shadow-checked per the usual
    pattern. Skip
    `class_dispatch.rs:228` — it lives inside the `run_instance_method` carrier F6 deletes
    outright, so cutting it over here is throwaway work; let F6's carrier deletion remove it for
    free.

    **Progress (the class-only cluster, #TBD):** the class-level-only reads named above are
    migrated to `Registry::user_method_overloads` (a thin, already-existing wrapper over the
    canonical `MethodEntry` table): `methods_object.rs`'s `mro_has_build_or_tweak`,
    `native_ctor_plan`'s has_build/has_tweak probes, and `build_owning_attr_names`;
    `ctor_phase_plan.rs:67,103` (`build_construction_phase_steps`'s class_has_own /
    has_non_submethod probes); `metamodel.rs`'s three custom-HOW existence checks
    (`install_custom_grammar_how`'s `find_method`, `install_custom_class_how`'s `compose`,
    `declare_how_has_user_method`'s arbitrary method_name); `class_introspection.rs:39`
    (`class_has_new_accepting_positional`'s `new` lookup). Each site already bailed on a
    non-class MRO entry before this change (`registry.classes.get(cls)` returning `None`), so
    the swap is behavior-preserving by construction — no separate shadow-check instrumentation
    was needed on top of that existing bail. `metamodel.rs`'s `declare_drive_how_protocol`
    (~line 406-428, full-method-name enumeration for a class) is deliberately left on
    `class_def.methods`: it needs every method name a class owns, which the `(owner,
    name)`-keyed table has no index for yet — the same gap F4c's own text already calls out.
    `ctor_phase_plan.rs:133` (`get_method_overloads` inside `try_pin_phase_candidate`) already
    reads the canonical table and needs no change; its only caller passes `mro_class` after the
    loop's own role-skip (`roles.contains_key && !classes.contains_key` -> `continue`), so it is
    structurally never called with a role receiver today. Verified with the full local `t/`
    suite (3173 files) and a 64-file `S04`/`S06`/`S09`/`S12`/`S14` roast subset (class
    declarations, multi, typed arrays, introspection, roles), all green.
  - [x] **F4c — Invert the write direction and remove the field.** Make `MethodEntry`/the
    canonical table the write-side source (`sync_user_method_entries`'s write sites in
    `registration.rs`, `registration_class_body_attr.rs`, `registration_class_body_method.rs`,
    `registration_class_compose.rs`, `registration_role_*.rs`, `system.rs`,
    `methods_classhow_dispatch.rs:862,923` write `MethodEntry` directly instead of
    `class_def.methods`), solve the full-name-enumeration need
    (`methods_classhow_dispatch.rs:1324` needs "every method name owner X has," which the
    `(owner, name)`-keyed table has no index for today), then delete `ClassDef::methods` (see the
    design note below — `RoleDef::methods` is explicitly OUT of scope, not merely deferred) and
    update snapshot/rollback (class redeclaration, EVAL class restoration, and the other
    mechanisms the design note below identifies) to copy the one remaining source. Needs its own
    design note before code — this is the box's own original "field deletion" framing, now
    correctly scoped as the last step rather than the first.

    **Design note (2026-08-15, design-only — no code; this unblocks F4c implementation).**

    **(0) Ground-truth corrections to this box's own text, found by reading the code.** Four of
    F4c's own claims need amending before any slice starts. (i) The enumeration site is
    `methods_classhow_dispatch.rs:1331` (`^submethod_table`), not `:1324`; and it is one of
    *eight*, not one: `methods_classhow_method_obj.rs:29` (`.^methods`), `:93`
    (`class_method_table`, i.e. `^method_table`), `metamodel.rs:403-418`
    (`declare_drive_how_protocol`, the site F4b explicitly deferred here),
    `registration_class.rs:486` and `:488,493` (`collect_type_method_names`, backing
    `handles SomeType` — note it enumerates a **role** owner too), `registration.rs:207`
    (`resolve_class_stub_requirements`), `registration.rs:636`
    (`validate_private_method_existence`, which runs on *every* class registration), and
    `class.rs:174` (`detect_unresolved_role_method_conflicts`). (ii) The write-site list omits
    `registration_class_body.rs:251-252` (`our &alias ::= &m`), `registration_class.rs:401-405`
    (`apply_handle_specs`), `registration_class_compose_body.rs:390`,
    `types/role_mixin_class.rs:216-226`, `registration_class_augment.rs:1055-1132`
    (`ensure_role_pun_class`), `runtime_init.rs:2171-2198` (builtin seeding plus a startup
    `for class_name in classes.keys() { sync_user_method_entries }` loop), and
    `builtins_system_require.rs:227-240` (the `require` **class-alias** copy — it clones a whole
    `ClassDef`, methods included, under a second name, so after inversion it must also copy that
    owner's method rows; nothing in this box named it). (iii) "`sync_user_method_entries` only
    handles `ClassDef`" is right about its *source* but wrong about its *callers*: it is called
    with a role name at `methods_object_dispatch_new.rs:201` (`withdraw_role_pun`),
    `registration_class_compose_body.rs:45` (`rename_generic_composed_class`, old name), and
    transitively at `registration_class_augment.rs:1140` — in every such case the call is a pure
    **clear** (the `classes` lookup misses and the function returns after its `retain`). Those
    three become explicit `clear_user_methods_for_owner` calls, not accidents of a missing map
    entry. (iv) `methods_object.rs:95-96` is *not* a stale F4b hit: it is
    `is_native_default_constructible`, a different function from the three F4b migrated
    (`mro_has_build_or_tweak`, `native_ctor_plan`'s probes, `build_owning_attr_names`). It is a
    class-only read and belongs to F4c's reader cutover.

    **(1) Scope: `RoleDef::methods` is NOT deleted by F4c, and this is not re-litigating F4a.**
    F4c inherited "delete both fields" from the pre-split F4 bullet, written before F4a
    established the composition-input policy. Recommendation: **F4c deletes `ClassDef::methods`
    only; `RoleDef::methods` stays exactly where it is**, and a role-side sibling table
    (`Registry::role_method_entries`) is explicitly rejected, not deferred. Three reasons, all
    confirmed by reading. **First, the two representations hold genuinely different data under
    the same owner string and are live simultaneously.** When a role `R` is punned,
    `ensure_role_pun_class` (`registration_class_augment.rs:1041-1132`) builds a `ClassDef`
    under the key `R` whose every method is `role_origin`-tagged via its `tag_role_origin`
    closure, and registers it in `classes` — while `roles[R].methods` continues to hold the
    *untagged* originals (`registration_role_method.rs:243` sets `role_origin: None`). So
    `(R, m)` in `method_entries` and `(R, m)` in a hypothetical `role_method_entries` would carry
    different `MethodDef`s at the same instant. A single `(owner, name)` table therefore cannot
    hold both, and a sibling table keyed the same way buys uniformity of key type and nothing
    else. **Second, `RoleDef::methods` is not a mirror.** F4's whole thesis is eliminating drift
    between two copies of the same data; `ClassDef::methods` qualifies because
    `sync_user_method_entries` *derives* `method_entries` from it (`registry.rs:341-396`).
    `RoleDef::methods` is derived from nothing and nothing is derived from it — since F4a it has
    exactly one read helper (`Registry::role_method_overloads`, `registry.rs:1005-1015`). There
    is no drift to eliminate. **Third, moving it would cost real code for no invariant.** Its
    consumers are overwhelmingly *whole-map* iterations at composition time
    (`registration_class_compose.rs:315`, `registration_role_body.rs:368`,
    `registration_class_compose_body.rs:342`, `registration_class_augment.rs:633,644,1055,1074`,
    `types/role_mixin_class.rs:187`), so they would each need the role-side reverse index too;
    `RoleDef` is cloned wholesale in ~15 places (`roles.get(x).cloned()`), which gets methods for
    free today and would become a two-lookup dance; and `methods_qualified.rs:449-457` finds a
    parameterised role by **prefix-scanning `roles.keys()`**, so `Registry::roles` has to stay
    the role-name index regardless. The one argument *for* a sibling table — making "role methods
    are never dispatch entries" structural instead of by-convention — is already delivered by
    F4a's single-helper read path; a second table adds no enforcement the helper does not
    already give. This is not re-litigating F4a: F4a decided the **read policy** (role method
    definitions are composition inputs; the dispatchable form is always the flattened copy on the
    composing class; winner selection must never consult the role fallback). That policy is kept
    verbatim. What is being declined is a **storage relocation** the policy never required. The
    resulting class/role asymmetry is the point, not a defect: it makes it structurally
    impossible to feed a role-owned row to a dispatcher expecting a class-owned one, which is
    exactly the shape of the `resolve_methods_per_mro_level` `any_failed` regression (PR #6478,
    `todo/deep/method-entries-never-covers-unpunned-roles.md`). If a role-side drift is ever
    *observed*, file it as a new box then; do not pre-build the table.

    **(2) The full-name-enumeration index.** Add one private field to `Registry`:
    `owner_method_names: HashMap<Symbol, Vec<Symbol>>`, insertion-ordered, holding exactly the
    names for which `(owner, name)` has a **non-empty `user_candidates`** — not every row.
    Scoping it to the user column is load-bearing: rows also exist for `builtin` (1108 seeded
    rows, `seed_builtin_method_entries`), `accessor` (D2d), and `proto` (E8b), and indexing those
    would make `.^methods`/`^method_table` start reporting attribute and proto names that
    `class_def.methods.keys()` never contained. Rejected alternatives: a full `method_entries`
    scan per query (the precedent `builtin_method_names`, `registry.rs:326-339`, sets — but
    `validate_private_method_existence` runs once per class *registration*, making the program
    O(classes x total rows), i.e. quadratic); a `BTreeMap<(Symbol, Symbol), MethodEntry>` for
    free range queries (pays log n on `user_method_overloads`, which is on the dispatch
    cache-miss path E1/E2 deliberately made a flat hash); `FxHashSet<Symbol>` values (fine
    correctness-wise, but a `Vec` is cheaper at these sizes, keeps declaration order, and makes
    the snapshot/restore of an owner's rows deterministically ordered). Ordering is a free
    fidelity gain, not a risk: `ClassDef::methods` is a `std::collections::HashMap`
    (`decl_types.rs:15`) whose `RandomState` reseeds per instance, so today's `.^methods` /
    `^method_table` / `handles Type` order is nondeterministic **between runs**, and no consumer
    can be depending on it. Rakudo enumerates in declaration order, so an insertion-ordered `Vec`
    moves toward the reference implementation. **Maintenance is encapsulated, not distributed.**
    The field is private to the registry module and mutated only by a new mutator API — see (3);
    no write site touches it, so no write site can drift. The correctness trap to pin with unit
    tests is the interaction with the row-liveness predicate at `registry.rs:361-365`: a row
    whose `user_candidates` becomes empty must leave the index **even when the row itself
    survives** on `builtin`/`accessor`/`proto` (the `augment class Str { method chars {...} }`
    then-rolled-back shape, which the existing `user_override_shares_the_builtin_method_entry`
    test at `registry.rs:1288-1342` already exercises for the row half). Add
    `#[cfg(debug_assertions)]` + env-gated `MUTSU_CHECK_METHOD_INDEX=1` full-table verification
    (index <=> `{k : !method_entries[k].user_candidates.is_empty()}`), on the `MUTSU_VM_STATS`
    precedent; a full `t/` + whitelist sweep under it is this box's substitute for a read-side
    shadow check.

    **(3) Inverting the write direction: why it cannot be one-write-site-per-PR, and what
    replaces that.** F4a/F4b could ship one consumer family per PR because a *read* can be
    shadow-compared old-vs-new. A *write* has no such comparison, and worse: a site that starts
    writing only `method_entries` becomes invisible to every reader still on
    `class_def.methods`, so no single write site can move alone. The resolution is a **dual-write
    bridge**. A new `src/runtime/registry_method_table.rs` (a second `impl Registry` block —
    `registry.rs` is already 1402 lines, so the 500-line convention forbids growing it) provides
    the whole mutator surface: `set_user_methods(owner, name, defs)`,
    `push_user_method(owner, name, def)` (the `multi` case),
    `retain_user_methods(owner, name, pred)` (the privacy-preserving non-multi replace at
    `registration_class_body_method.rs:219-222`), `remove_user_methods(owner, name)`,
    `clear_user_methods_for_owner(owner)` (the redeclaration reset that `publish_class_shell`,
    `registration_class_validate.rs:406-409`, gets today for free from `sync`'s `retain` half),
    `rename_method_owner(old, new)`, `map_user_methods_in_place(owner, f)` (for
    `compile_class_methods`, `accessors_resolve.rs:116-122`, which mutates `compiled_code` on
    every `MethodDef` in place), `owner_method_names(owner)`,
    `user_method_rows_for_owner(owner)` / `restore_user_method_rows(owner, rows)` (for
    rollback), and `sync_accessor_entries(owner)` — the surviving half of
    `sync_user_method_entries`, which still derives the `accessor` column from
    `ClassDef::attributes` (F4 keeps type-structure metadata on `ClassDef` by design). Every
    mutator maintains the reverse index, drops rows that fall dead under the `registry.rs:361-365`
    predicate, refuses to store an empty candidate vec, and bumps `method_generation`. During the
    bridge each mutator writes **both** `method_entries` (+ index) and `classes[owner].methods`,
    and `sync_user_method_entries` degenerates to an assertion that the two agree. **The
    dual-write window is the shadow-check, and the consistency verifier is the comparison** —
    each write-site slice ships behind it, is independently revertible, and is verified by (a)
    the verifier over a full `t/` + roast-whitelist sweep, (b) `grep` showing that slice's file
    has zero remaining direct `.methods` mutations, and (c) the usual full local `t/` plus a
    targeted roast subset. Prefer *write-through* over *buffer-and-flush* at every site: keeping
    a per-declaration buffer in `ClassBodyCx` would just recreate the dual representation F4 is
    deleting. Write-through also **deletes two existing workarounds**, which is the box's real
    payoff and should be an explicit acceptance criterion:
    `registration_class_body_attr.rs:162-180` is a hand-written merge-back that exists *only*
    because the in-flight `cx.class_def` would clobber methods a user `trait_mod:<is>` installed
    via `.^add_method` mid-body (Attribute::Predicate's `is predicate`), and the
    `cx.class_def = updated` re-reads at `registration_class_body.rs:266-268` and `:398-402` are
    the same bug patched twice more.

    Ordered slices:

    - **F4c-1 — reverse index + the eight enumeration reads.** Add `owner_method_names`,
      maintained inside `sync_user_method_entries` only (nothing else writes the user column
      yet), plus the verifier. Cut the eight sites in (0)(i) from `class_def.methods.keys()` to
      `Registry::owner_method_names`. Invariant: each site's *set* of names is unchanged; order
      may change (justified above). Genuinely shadow-checkable, exactly like F4b — emit both
      lists under `MUTSU_VM_STATS` and require zero set-mismatches across `t/` + the whitelist
      before deleting the old read. This slice closes F4b's one deliberate deferral
      (`declare_drive_how_protocol`) and is valuable even if F4c goes no further. Run the
      batteries gate (`scripts/battery-testsuite.sh`) for this one — `declare_drive_how_protocol`
      and the custom-HOW paths are what OO::Monitors' `EXPORTHOW::DECLARE` support runs through.
    - **F4c-2 — mutator API + dual write.** No call-site changes beyond routing
      `sync_user_method_entries` through the mutators. Zero intended behavior change; verifier
      green.
    - **F4c-3 — the class-declaration family** (the in-flight `ClassBodyCx` writers):
      `registration_class_body_method.rs:192,205,221,344,351`, `registration_class_body.rs:251-252`,
      `registration_class_body_attr.rs:172-178` (delete the merge-back),
      `registration_class_compose.rs:352-356`, `registration_class_compose_body.rs:390-393`,
      `registration_class.rs:401-405`, `registration.rs:313,367,369`. Borrow-structure warning:
      `resolve_class_stub_requirements` (`registration.rs:202-373`) holds `&mut ClassDef` while
      calling `&mut self` helpers (`class_mro`, `collect_class_attributes`); once the map lives
      in the registry it must become snapshot -> compute -> write back, because the lock
      discipline at `registry.rs:21-26` forbids holding a guard across those calls. Getting this
      wrong yields a same-thread recursive `RwLock` acquisition — a **deadlock**, which surfaces
      as a `t/` timeout, i.e. the exact failure shape CLAUDE.md's triage protocol warns against
      dismissing as flaky.
    - **F4c-4 — the augment family:** `registration_class_augment.rs:294,310,322` (method decl),
      `:514,521` (method `handles`), `:574` (attribute `handles`), `:665-679`
      (`compose_role_into_augmented_class`), plus `types/role_mixin_class.rs:216-226`
      (`compose_mixin_role_submethods`). Separate from F4c-3 for two reasons: augment mutates the
      *registered* `ClassDef` in place rather than an in-flight one, and its publication into
      `method_entries` is **implicit** — it happens only because `compile_class_methods(name)`
      (`registration_class_augment.rs:602,699`; `types/role_mixin_class.rs:226`) incidentally
      calls `sync`. That implicit publication must become explicit in this slice or augment
      silently stops publishing.
    - **F4c-5 — role pun / synthesised mixin classes:** `ensure_role_pun_class`
      (`registration_class_augment.rs:1055-1132`), `withdraw_role_pun`
      (`methods_object_dispatch_new.rs:197-202`, becomes `clear_user_methods_for_owner`),
      `rename_generic_composed_class` (`registration_class_compose_body.rs:42-46`, becomes
      `rename_method_owner`), `types/role_mixin_class.rs:305-312`. Its own slice because this is
      the only place a role *name* legitimately owns class-side rows — the exact seam F4a's
      incident sits on. State and test the invariant: while `R` is punned,
      `user_method_overloads(R, m)` and `role_method_overloads(R, m)` may both be `Some` with
      **different** content (tagged vs untagged), and withdrawal clears only the former. Pin the
      "only the first `R.new` in a program worked" regression `withdraw_role_pun`'s own doc
      comment describes.
    - **F4c-6 — the runtime-reflective MOP family:** `methods_classhow_dispatch.rs:852-877`
      (`^add_method`, including its "create a stub `ClassDef` for a builtin type" branch),
      `:928-939` (`^add_multi_method`), `system.rs:349-360` (BEGIN-time method statements). Own
      slice because these fire at arbitrary times against an already-published class and are what
      the F4c-3 merge-back deletion depends on. Invariant: existence checks keep keying off
      `classes.contains_key`, not off the method table — `^add_method` must still auto-create a
      stub `ClassDef`, and `^add_multi_method` must still *error* for an unregistered class
      (its `inserted` flag, `:928-934`).
    - **F4c-7 — seeds and aliases:** `runtime_init.rs`'s builtin `classes`/`roles` seeding and
      its startup `for class_name in classes.keys() { sync }` loop (`:2195-2198`, which
      disappears), `builtins_system_require.rs:227-240` (the `require` class alias — must now
      copy method rows to the alias owner), and the `methods: HashMap::new()` initialisers at
      `methods_object_native_ctors_io.rs:20` and `methods_object_dispatch_new.rs:619` (these
      merely lose a field).
    - **F4c-8 — snapshot/rollback**, see (4).
    - **F4c-9a — reader cutover** (dual write still on, so every read is old-vs-new
      shadow-checkable exactly like F4b), then **F4c-9b — flip the mutators to single write,
      delete `ClassDef::methods`, delete `sync_user_method_entries` and the bridge's assertion
      half.**

    **(4) Snapshot/rollback — there are five mechanisms, not three.** **(a) `ClassRegSnapshot`**
    (`registration_class_validate.rs:19-72`): add `prev_method_rows: Vec<(Symbol, Vec<MethodDef>)>`
    captured with `user_method_rows_for_owner(name)` (O(names) via the index, and `MethodDef`
    clone is shallow — `body` is an `Arc`), restored with `restore_user_method_rows`. Two
    pre-existing gaps must be preserved deliberately, not silently inherited: the snapshot
    captures neither `MethodEntry::proto` (so a failed redeclaration that declared a
    `proto method` leaves it behind; today this survives because `sync`'s retain deliberately
    spares the `proto` column, `registry.rs:348-360` — after inversion the equivalent guarantee
    is that `restore_user_method_rows` touches only `user_candidates`) nor `method_wrap_chains`
    (cleared unconditionally at `publish_class_shell` via `clear_method_wrap_chains_for_class`,
    `registration_class_validate.rs:377`, and never restored). File the proto one as a
    `todo/tickets/` note rather than folding a behavior change into F4c. **(b) EVAL-string
    rollback** (`system_eval_string.rs:220-444`): read the merge carefully before redesigning it.
    `classes = snapshot; classes.extend(current)` means **current wins for every key present in
    current** — the snapshot's only net effect is to *resurrect* keys the EVAL removed
    (`withdraw_role_pun`, `__MUTSU_UNREGISTER_CLASS__`, `shadow_suppressed_type_with_package`,
    `rename_generic_composed_class`). So the `for class_name in ... { sync }` loop at `:441-444`
    is O(classes) x O(total rows) — quadratic — to repair at most a handful of owners.
    Replacement: take a shallow whole-table snapshot of the user column up front (strictly
    cheaper than the `classes_snapshot` deep `ClassDef` clone the function already pays at
    `:229`), then at restore install rows only for
    `resurrected = snapshot_class_keys - current_class_keys`. Linear, and scales with what the
    EVAL actually changed. To the explicit question: **no, this path never touches
    `RoleDef::methods` directly** — it clones and restores the whole `roles` map (`:222`,
    `:400`, `:412`) and role method data rides along inside `RoleDef`. That stays true under this
    design precisely because (1) keeps `RoleDef::methods` where it is; had we moved it to a
    sibling table, this merge would have needed a parallel union/merge for it. **(c) The fourth
    mechanism the box text does not name: `Registry::replace_method_entries_from`**
    (`registry.rs:459-462`), with five call sites — `test_functions/eval_exception.rs:256,362`
    (`eval-lives-ok`/`eval-dies-ok`), `test_functions/mod.rs:60-64`
    (`sync_eval_definition_state`, the write-back direction), `throws_like.rs:50`,
    `fails_like.rs:63` — which copies the whole table between a parent and a nested
    `Interpreter`. It must copy the reverse index too, or the nested interpreter runs the
    parent's table against `Interpreter::new()`'s empty index. Because it is a single function
    over a private field, encapsulation makes this a one-line fix rather than a landmine — which
    is itself the argument for the field being private. **(d) The fifth: `rename_generic_composed_class`**
    (`registration_class_compose_body.rs:42-46`), an owner *rename* currently expressed as
    "sync old (clears), sync new (re-derives)"; it needs a real `rename_method_owner`.
    **(e) Finally, three `classes.remove` sites call no sync at all** —
    `builtins.rs:511-517` (`__MUTSU_UNREGISTER_CLASS__`), `runtime_encoding.rs:258-266`
    (`shadow_suppressed_type_with_package`), `registration_role_decl.rs:99` — so they leave
    **permanently stale** `method_entries` rows today, masked only because every dispatcher
    checks `classes.contains_key` first. F4c-8 should give each an explicit
    `clear_user_methods_for_owner` rather than inherit a latent bug into the new world.

    **(5) Risk register.** **R1 — loss of the self-healing rebuild (the headline risk).** Today
    every `sync_user_method_entries` call is a full, idempotent re-derive for one owner, and
    `registration_class_body.rs:208` runs one after *every class-body statement* — so any missed
    or mis-ordered write is silently repaired by the next statement. After inversion there is no
    repair path at all; a missed write is a permanently missing method. Mitigation: the
    dual-write bridge, the mutator choke point, and full-corpus verifier sweeps per slice.
    **R2 — index/table drift.** Mitigation: private field, mutators only, debug assert +
    `MUTSU_CHECK_METHOD_INDEX`, and a unit test per mutator for the `registry.rs:361-365`
    liveness interaction. **R3 — implicit-publication loss** (augment and mixin composition
    publishing only via `compile_class_methods`' incidental sync). Mitigation: F4c-4 makes it
    explicit; detection is the verifier plus targeted `augment`/`does`-mixin `t/` tests.
    **R4 — the F4a landmine: accidentally widening a reader's role visibility while merely
    relocating storage.** The governing rule for F4c-9a is **preserve each site's existing
    role-visibility bit exactly**: a site reading only `classes` today moves to
    `user_method_overloads`; a site already doing class-then-role moves to
    `get_method_overloads_with_role_fallback`; a role-only site moves to
    `role_method_overloads`. Concretely, `resolution_method.rs` is the single highest-risk file
    in the refactor because it contains **both** kinds 400 lines apart:
    `resolve_method_with_owner_impl`'s per-level walk (`:140`) is class-only via
    `get_method_overloads` and must stay class-only, while `count_visible_method_candidates`
    (`:594-603`) and `resolve_all_methods_with_owner` (`:632-645`) do class-then-role today and
    must keep it. Doing the "obvious" thing and unifying the file on one helper reproduces the
    PR #6478 regression exactly. Apply the incident's own lesson — "the risk inventory is every
    transitive reader of that function, found by grep, not just the sites a ticket names" — as a
    per-site grep obligation for F4c-9a, not a per-file one. The same care applies to
    `accessors_state.rs:588,593` vs `:1116`, and `methods_classhow_lookup.rs:57` vs `:113`.
    **R5 — enumeration-order change.** Bounded by the `RandomState` argument in (2); still verify
    against the `.^methods`/`.^method_table`-touching roast files. **R6 — generation-bump
    volume.** Per-mutation bumps replace per-statement bumps; watch the `MUTSU_VM_STATS`
    `fast_method_cache` counters, and batch behind a `bump_once` guard if they move. Note the
    likely *win* in the other direction: `sync_user_method_entries` currently does a full
    `method_entries.retain()` (>1100 rows) **plus** a full clone of the class's method map after
    every class-body statement, making class registration O(statements x table size); F4c makes
    it O(declarations). Measure it via the bench CI, not locally. **R7 — empty rows.**
    `class_def.methods` can in principle hold `Some(vec![])` while `user_method_overloads`
    filters it to `None` (`registry.rs:403-406`); the mutator API must make empty rows
    unrepresentable and the verifier must assert it. **R8 — lock/borrow restructuring**, see
    F4c-3.

    **(6) Sequencing — definitive.** The scope decision (1) must be settled **first**, because it
    determines whether the role-only read sites are in F4c at all (they are not) and whether
    `role_method_overloads` remains the role read path (it does); that is this note, plus the
    one-line amendment to the F4c bullet above. The reverse index (2) must land **second, before
    any write site moves**, because `clear_user_methods_for_owner`, `rename_method_owner`, and
    both rollback redesigns all need an O(names) way to enumerate an owner's rows, and because it
    is independently valuable and independently shadow-checkable. Then the mutator/dual-write
    bridge, then the write sites in dependency order (class body -> augment -> pun/mixin -> MOP ->
    seeds/aliases), then rollback, then the read cutover, then the field deletion. So:
    **F4c-0 -> F4c-1 -> F4c-2 -> F4c-3 -> F4c-4 -> F4c-5 -> F4c-6 -> F4c-7 -> F4c-8 -> F4c-9a ->
    F4c-9b.** Standard verification per slice: full local `t/`, the 312-file
    `S04`/`S06`/`S09`/`S12`/`S14` roast subset the other F4 slices used, plus
    `scripts/battery-testsuite.sh` for F4c-1, F4c-4, and F4c-6 (the MOP/EXPORTHOW-adjacent ones),
    and a `MUTSU_CHECK_METHOD_INDEX=1` sweep for every slice from F4c-2 onward.

    Notes on where this design note corrects the box's own framing, beyond (0) above: the
    write/read inventories in the original bullet were incomplete — a full `grep -rn "\.methods"`
    over `src/` returns ~130 hits across ~45 files, including whole files neither list mentioned
    (`methods_call_dispatch.rs`, `methods_mixin_dispatch.rs`, `methods_signature_shaped.rs`,
    `resolution_deferral.rs`, `types/roles.rs`, `types/type_registry.rs`,
    `types/role_mixin_class.rs`, `methods_native_bypass.rs`, `compiler/helpers_method_body.rs`);
    most of the extras are role-only reads, which (1) puts out of F4c entirely. "Augment
    rollback" as a distinct named mechanism does not exist — `registration_class_augment.rs` has
    no snapshot/restore path; there are five mechanisms and they are the ones enumerated in (4).

    **Progress (F4c-1, #TBD):** added the reverse index (`Registry::owner_method_names`, a private
    `HashMap<Symbol, Vec<Symbol>>` maintained only inside `sync_user_method_entries`) plus its
    `#[cfg(debug_assertions)]` + `MUTSU_CHECK_METHOD_INDEX=1`-gated full-table verifier, per (2).
    `replace_method_entries_from` (item (4)(c)) now copies the index alongside the table, or a
    nested EVAL/`eval-lives-ok`/`throws-like`/`fails-like` interpreter would run the parent's
    `method_entries` against its own empty index. A shared `Registry::shadow_check_owner_method_names`
    helper (set comparison, `MUTSU_VM_STATS`-gated) was added and used to instrument all eight
    enumeration sites named in (0)(i); a full local `t/` sweep (3179 files, 15127 checks) came back
    clean at **seven** of the eight — `methods_classhow_dispatch::submethod_table`,
    `methods_classhow_method_obj::collect_class_methods`/`class_method_table`,
    `metamodel::declare_drive_how_protocol` (closing F4b's own deferral),
    `registration_class::collect_type_method_names`,
    `registration::resolve_class_stub_requirements`/`validate_private_method_existence` — which
    were cut over to `owner_method_names` and verified with the full local `t/` suite (3179 files),
    the 312-file `S04`/`S06`/`S09`/`S12`/`S14` roast subset, and `scripts/battery-testsuite.sh`
    (GATE PASSED), all green; `cargo clippy -- -D warnings` and `cargo fmt` clean.

    The eighth, `class::detect_unresolved_role_method_conflicts`, showed 12 mismatches (all under
    `Cro::HTTP`/`Cro::TCP`/`Cro::TLS`/`Log::Timeline`/local `t/` role fixtures) and was
    **deliberately left on the old `class_def.methods` read** — this is exactly the kind of
    ordering hazard R4 warns about, confirmed by reading, not assumed: `finalize_class_registration`
    calls `resolve_class_stub_requirements` (which can *remove* a resolved-away stub's entry from
    the in-flight `class_def.methods` in place) immediately before this site, and does not re-sync
    the registry until after both calls return — so at this exact point `class_def.methods` can be
    a proper subset of what `owner_method_names` (last synced before the stub-resolution mutation)
    still lists. Every one of the 12 mismatches was `new ⊇ old`, matching that theory exactly (no
    case of `old` containing a name `new` lacked). This is expected reader/writer skew from the
    dual-representation window, not a bug in either side; the shadow check stays in place
    (uninstrumented sites don't get silently dropped) and this site's cutover moves to F4c-9a,
    after F4c-3's write-through machinery makes `class_def.methods` and the registry agree at every
    point in `finalize_class_registration`, not just at statement boundaries.

    Along the way, the verifier itself caught a real index/table desync before it shipped: the
    early-return branch of `sync_user_method_entries` (taken when `classes.get(class_name)` misses
    — the "pure clear" shape `withdraw_role_pun`/`rename_generic_composed_class`'s old-name half
    use, per (0)(iii)) cleared the affected rows' `user_candidates` via the leading `retain` but
    returned before the index update, leaving `owner_method_names` pointing at now-dead rows.
    Caught by a `MUTSU_CHECK_METHOD_INDEX=1` sweep of the full local `t/` suite (6 crashes, all role-pun
    tests: `role-instantiation.t`, `punned-role-container-attribute.t`, `role-pun-private-attribute.t`,
    `role-body-composition-timing.t`, `role-diamond-stub-concrete.t`,
    `positional-role-attr-writeback-coherence.t`) before any cutover read it, so the coverage gap
    of (0)(iii)'s "pure clear" callers never reached production behavior. Fixed by clearing the
    index in that branch too; the same sweep is clean (0 crashes) after the fix.

    **Progress (F4c-2, #TBD):** added the full mutator surface to `registry_method_table.rs`
    per (3) -- `set_user_methods`, `push_user_method`, `retain_user_methods`,
    `remove_user_methods`, `clear_user_methods_for_owner`, `rename_method_owner`,
    `map_user_methods_in_place`, `user_method_rows_for_owner`, `restore_user_method_rows`, and
    `sync_accessor_entries` (the accessor-column "surviving half", deliberately left at its
    pre-existing O(total table) shape per (3)'s own framing -- accessor-only rows are not covered
    by `owner_method_names`, which stays scoped to the user-method column). `sync_user_method_
    entries` is rewritten to call these instead of inlining the retain/re-populate logic, per
    F4c-2's "no call-site changes beyond routing `sync_user_method_entries` through the mutators"
    scope -- the seven mutators no other production call site uses yet
    (`push_user_method`/`retain_user_methods`/`remove_user_methods`/`rename_method_owner`/
    `map_user_methods_in_place`/`user_method_rows_for_owner`/`restore_user_method_rows`) are
    `#[allow(dead_code)]` until their F4c-3/4/5/8 slice wires them in, and are exercised in the
    meantime by a unit test per mutator (10 tests) in a new `registry_method_table_tests.rs`,
    satisfying (5) R2's mitigation. Each mutator bumps `method_generation` on its own call rather
    than once per `sync_user_method_entries` invocation, a deliberately accepted (5) R6 behavior
    change -- not mitigated with a `bump_once` guard in this slice, since R6 frames that as
    reactive ("if they move"), not a prerequisite. Verified with the full local `t/` suite (3182
    files, 29634 tests) under `MUTSU_CHECK_METHOD_INDEX=1` (0 index/table-drift assertions), the
    312-file `S04`/`S06`/`S09`/`S12`/`S14` roast subset (only the pre-existing tracked
    `S12-attributes/trusts.t` failure), and `scripts/battery-testsuite.sh` (GATE PASSED); `cargo
    clippy -- -D warnings` and `cargo fmt` clean.

    **Progress (F4c-3, #TBD):** converted the class-declaration family's write sites to dual-write
    through the mutator API (`cx.class_def.methods`/`class_def.methods` unchanged, each site ALSO
    calls the matching registry mutator): `registration_class_body_method.rs`'s multi-candidate
    push, non-multi retain+push, and both `HandleSpec` delegation-forwarder pushes;
    `registration_class_body.rs`'s code-alias write; `registration_class_compose.rs` and
    `registration_class_compose_body.rs`'s role-method composition (both the direct-parent and
    grandparent-role-propagation paths); `registration_class.rs`'s `apply_handle_specs` (now
    `&mut self` plus a `class_name` parameter, its one call site updated); and
    `registration.rs`'s `resolve_class_stub_requirements` write-back (safe despite this function's
    own mid-loop `Err` returns: its only caller, `finalize_class_registration`, rolls back via
    `ClassRegSnapshot::restore`, which always ends with a full `sync_user_method_entries` re-derive
    from the restored pre-attempt `class_def` -- any registry state a failed attempt's mutator
    calls left behind is overwritten by that unconditional re-derive, exactly as it already
    overwrites `class_def` itself). The periodic per-statement `sync_user_method_entries` call
    (`registration_class_body.rs:208` and friends) is deliberately left untouched in this slice --
    per design note (3)'s framing it "degenerates to an assertion that the two agree" only once
    read cutover (F4c-9a) removes the last consumer of `class_def.methods`, so for now it stays the
    actual mechanism and these mutator calls are additive/shadow-provable, not a behavior change.

    **`registration_class_body_attr.rs:172-178`'s merge-back is deliberately NOT deleted in this
    slice**, contrary to this box's own bullet -- read carefully before assuming otherwise. Its
    root cause is that a user `trait_mod:<is>` (Attribute::Predicate's `is predicate` being the
    real-world case) can call `.^add_method`, which writes directly into the registry with no way
    to reach `cx.class_def` (the in-flight `ClassBodyCx` has no visibility into a MOP call). F4c-6
    (`^add_method`) has not landed yet, so `.^add_method`'s write still bypasses `class_def.methods`
    entirely; deleting the merge-back now would let the later unconditional `class_def.methods`
    re-publish (`registration_class_body.rs:208`'s per-statement sync, unchanged in this slice, per
    above) silently drop that method again -- reintroducing the exact bug the merge-back exists to
    fix. Revisit once F4c-6 makes `^add_method` dual-write-aware.

    **A real R8 lock-reentrancy panic was hit and fixed while implementing this slice** --
    `propagate_composed_role_parent_specs` (`registration_class_compose_body.rs`) had two
    `if let Some(x) = self.registry()....cloned() { <body> }` sites; Rust's temporary-lifetime-
    extension rule keeps the `RegistryReadGuard` produced by `self.registry()` alive for the
    *entire* `if let` body, not just the condition, so adding a `self.registry_mut()` call inside
    either body panics on a same-thread read -> write lock upgrade (caught by the debug-only
    `lock_reentry.rs` guard, not a silent deadlock -- see that module's own docs). Fixed by hoisting
    each clone into its own `let` statement first (ordinary `let` does not extend the temporary's
    lifetime past the statement), then branching on the *owned* result. This is exactly the R8
    hazard the design note names, just found in the sibling composition-propagation function
    rather than in `resolve_class_stub_requirements` itself, and just as easy to trip on any future
    F4c-4+ site that adds a `registry_mut()` call inside an existing `if let Some(x) =
    self.registry()... { }` -- grep for that shape before adding a write inside one.

    Verified with the full local `t/` suite (3182 files, 29634 tests) under
    `MUTSU_CHECK_METHOD_INDEX=1` (0 index/table-drift assertions, 0 lock-reentrancy panics after
    the fix above), the 312-file `S04`/`S06`/`S09`/`S12`/`S14` roast subset (release; only the
    pre-existing tracked `S12-attributes/trusts.t` failure), and `scripts/battery-testsuite.sh`
    (GATE PASSED); `cargo clippy -- -D warnings` and `cargo fmt` clean.

    **Progress (F4c-4, #TBD):** converted the augment family's write sites to the same dual-write
    shape as F4c-3 -- `registration_class_augment.rs`'s method-decl push, both `handles` sites
    (method-level and attribute-level), and `compose_role_into_augmented_class`'s role-method
    merge; plus `types/role_mixin_class.rs::compose_mixin_role_submethods`. Confirms this box's own
    reasoning for splitting F4c-4 from F4c-3: augment mutates the **already-registered** `ClassDef`
    via `self.registry_mut().classes.get_mut(name)`, so `class_def` here is *always* a live
    sub-borrow of a held write guard (not conditionally, the way F4c-3's `if let` temporaries
    sometimes were) -- every site in this file hits the R8 hazard the moment a second registry call
    is added inside the same block, not just the ones using the `if let Some(x) =
    self.registry()...` shape. Two different fixes were used depending on the existing structure:
    (a) most sites keep the original `class_def.methods` mutation block completely unchanged and
    add a **separate, subsequent, independently-short-lived** `self.registry()`/`self.registry_mut()`
    block after it closes (re-deriving the same decision from a fresh read where needed --
    `compose_role_into_augmented_class`'s per-name "does the class already have a local method"
    check re-queries `user_method_overloads` instead of reading the now-out-of-scope `class_def`);
    (b) `compose_mixin_role_submethods` already named its write guard (`let mut registry =
    self.registry_mut();`) rather than using an inline temporary, so its dual-write reuses that
    *same* guard for the mutator calls once `class_def`'s borrow ends (its last use), which is
    simpler and clearly not a new lock acquisition at all -- prefer this shape over (a) when a
    named guard is already in scope. Verified with the full local `t/` suite (3182 files, 29634
    tests) under `MUTSU_CHECK_METHOD_INDEX=1` (0 index/table-drift assertions, 0 lock-reentrancy
    panics), the 312-file `S04`/`S06`/`S09`/`S12`/`S14` roast subset (only the pre-existing tracked
    `S12-attributes/trusts.t` failure), and `scripts/battery-testsuite.sh` (GATE PASSED); `cargo
    clippy -- -D warnings` and `cargo fmt` clean.

    **Progress (F4c-5, #TBD):** converted the role-pun/mixin-class family. `ensure_role_punned_to_
    class`'s (formerly `ensure_role_pun_class` in this bullet's own text -- renamed since) fresh
    `ClassDef` insert now also dual-writes every composed method through `set_user_methods` (no
    R8 hazard here: it is a brand-new registry row, not a mutation of an already-borrowed one).
    `withdraw_role_pun` now calls `clear_user_methods_for_owner` + `sync_accessor_entries`
    directly instead of `sync_user_method_entries` -- behaviorally identical today (with the
    `classes.remove` immediately above, the old call would only ever take its "pure clear"
    early-return path, which already *is* exactly those two calls per the F4c-2 rewrite) and
    forward-looking for F4c-9b's eventual deletion of `sync_user_method_entries` itself.
    `rename_generic_composed_class` now calls `rename_method_owner` for the user-method column
    (replacing the old "sync old, sync new" idiom) plus the same `sync_accessor_entries` pair as
    before for the accessor column (no owner-rename mutator exists for that column by design --
    it stays keyed off `ClassDef::attributes`). `types/role_mixin_class.rs:305-312`, named in this
    bullet's own text, turned out to be stale -- that file's only `class_def.methods` write
    (`compose_mixin_role_submethods`) was already converted in F4c-4; grepping the whole file for
    `.methods` confirms nothing else touches it. This slice also retired the now-satisfied
    `#[allow(dead_code)]` markers on `push_user_method`, `retain_user_methods`,
    `remove_user_methods`, and `user_method_rows_for_owner` in `registry_method_table.rs` --
    `map_user_methods_in_place` and `restore_user_method_rows` (F4c-3's `compile_class_methods`
    site and F4c-8's rollback, respectively) remain the only unused mutators. Verified with the
    full local `t/` suite (3183 files, 29636 tests) under `MUTSU_CHECK_METHOD_INDEX=1` (0
    index/table-drift assertions, 0 lock-reentrancy panics), the 312-file
    `S04`/`S06`/`S09`/`S12`/`S14` roast subset (only the pre-existing tracked
    `S12-attributes/trusts.t` failure), and `scripts/battery-testsuite.sh` (GATE PASSED); `cargo
    clippy -- -D warnings` and `cargo fmt` clean.

    **Progress (F4c-6, #TBD):** converted the runtime-reflective MOP family --
    `methods_classhow_dispatch.rs`'s `^add_method` (including the "create a stub `ClassDef`"
    branch, which needed no separate handling: the dual-write call works purely against
    `method_entries`/`owner_method_names`, independent of whether `self.classes` already had a row)
    and `^add_multi_method`, plus `system.rs`'s BEGIN-time method-statement injection. Same
    separate-short-lived-guard shape as F4c-4/F4c-5's `if let Some(class_def) =
    self.registry_mut().classes.get_mut(...)` sites.

    **Correction to this box's own stated payoff:** the bullet frames F4c-6 as what "the F4c-3
    merge-back deletion depends on" -- landing this slice does NOT make deleting
    `registration_class_body_attr.rs`'s merge-back safe, and it stays in place. The merge-back's
    job is to pull a `.^add_method`-installed method back into `cx.class_def` before body
    processing's own periodic `sync_user_method_entries` call re-derives the registry from that
    local snapshot -- and that periodic full-clear-then-repopulate-from-`class_def.methods` call is
    *itself* untouched through the whole F4c-3..F4c-8 bridge (per F4c-3's own progress note: it
    stays the actual mechanism, not just an assertion, until F4c-9a). So even with `^add_method` now
    ALSO writing through `set_user_methods`, the very next per-statement sync (which knows nothing
    about that out-of-band write, only about `cx.class_def.methods`) unconditionally clears and
    re-derives the owner's rows from the still-unaware local snapshot, silently dropping it again --
    identically to before this slice. The merge-back can only go away once F4c-9b actually removes
    `class_def.methods`/`sync_user_method_entries`, leaving `method_entries` as the only place left
    to write. Verified this holds by re-running the merge-back's own regression coverage (the role
    pun / `is predicate`-flavored `t/` files already in the standard sweep below) with no change in
    outcome, confirming the guard is still load-bearing. Verified with the full local `t/` suite
    (3183 files, 29636 tests) under `MUTSU_CHECK_METHOD_INDEX=1` (0 index/table-drift assertions, 0
    lock-reentrancy panics), the 312-file `S04`/`S06`/`S09`/`S12`/`S14` roast subset (only the
    pre-existing tracked `S12-attributes/trusts.t` failure), and `scripts/battery-testsuite.sh`
    (GATE PASSED, including OO::Monitors which exercises `^add_method` via `EXPORTHOW::DECLARE`);
    `cargo clippy -- -D warnings` and `cargo fmt` clean.

    **Progress (F4c-7, #TBD):** of the three items this bullet names, only one needed a code
    change. `builtins_system_require.rs:227-240`'s `require`-driven class aliasing (a fresh
    `ClassDef` clone inserted under a new alias name) never called `sync_user_method_entries` for
    the alias at all -- the alias's `method_entries`/`owner_method_names` rows were simply never
    populated, so every F4c-1-cut-over enumeration site (`.^methods`, etc.) saw the alias as
    method-less even though `class_def.methods` (still read by the yet-to-be-cut-over dispatch
    paths) had them. Fixed by calling `sync_user_method_entries(&alias)` right after the insert,
    matching the pattern every other whole-class-insert site in the codebase already follows.
    `runtime_init.rs`'s startup seeding loop and the two `methods: HashMap::new()` initialisers
    needed no change -- the seeding loop already calls `sync_user_method_entries` per class (the
    established mechanism, already correct), and the two initialisers construct a `ClassDef` with
    zero methods, so there is nothing to dual-write; both are exactly the "disappears" /
    "merely loses a field" outcomes this bullet's own text predicts for F4c-9b, not something F4c-7
    itself needs to act on. Verified with the full local `t/` suite (3183 files, 29636 tests) under
    `MUTSU_CHECK_METHOD_INDEX=1` (0 index/table-drift assertions), the 312-file
    `S04`/`S06`/`S09`/`S11`/`S12`/`S14` roast subset (`S11-modules` added for this slice's own
    `require`-alias territory; only the pre-existing tracked `S11-modules/re-export.t` SORRY-abort
    and `S12-attributes/trusts.t` failures), and `scripts/battery-testsuite.sh` (GATE PASSED);
    `cargo clippy -- -D warnings` and `cargo fmt` clean.

    **Progress (F4c-8, #TBD):** of the five mechanisms (4) names, two were already done by earlier
    slices -- **(c)** `Registry::replace_method_entries_from` already copied the reverse index
    (fixed in F4c-1's own progress note) and **(d)** `rename_generic_composed_class` already uses
    `rename_method_owner` (F4c-5) -- so this slice covers the remaining three. **(a)
    `ClassRegSnapshot`** gained `prev_method_rows: Vec<(Symbol, Vec<MethodDef>)>`, captured via
    `user_method_rows_for_owner` and restored via `restore_user_method_rows`; the restore call sits
    before the pre-existing `sync_user_method_entries(name)` call, which stays authoritative during
    the bridge and will simply have nothing left to correct once F4c-9b arrives. Both pre-existing
    gaps preserved deliberately, not silently fixed: `restore_user_method_rows` only ever touches
    `user_candidates`, so `MethodEntry::proto` is untouched exactly as before; `method_wrap_chains`
    is untouched by either path, also exactly as before. Filed the proto gap as
    `todo/tickets/class-redeclaration-rollback-loses-proto-method.md` per this box's own
    instruction not to fold a behavior change into F4c. **(b) EVAL-string rollback**
    (`system_eval_string.rs`) keeps its `classes = snapshot; classes.extend(current)` merge exactly
    as-is (still correct, still needs no redesign) but the O(all classes) `for class_name in
    self.registry().classes.keys() { sync }` repair loop is now scoped to `resurrected_classes`
    (`classes_snapshot`'s keys minus `current_classes`'s, computed *before* the merge consumes
    `classes_snapshot`) -- a genuine algorithmic win (O(all classes) x O(total table) down to
    O(resurrected) x O(total table)), not just a mechanical dual-write, since every non-resurrected
    class's `method_entries` rows are already correct (kept live-synced by whatever ran during the
    EVAL). Confirmed this path still never touches `RoleDef::methods` -- unchanged, the `roles`
    field restore/extend pair is untouched by this slice. **(e)** all three no-sync `classes.remove`
    sites (`builtins.rs`'s `__MUTSU_UNREGISTER_CLASS__`, `runtime_encoding.rs`'s
    `shadow_suppressed_type_with_package`, `registration_role_decl.rs`'s stale-pun cleanup) now
    call `clear_user_methods_for_owner` + `sync_accessor_entries` explicitly, per the design note's
    own instruction not to inherit the latent permanently-stale-rows bug into the new world.
    Verified with the full local `t/` suite (3184 files, 29653 tests) under
    `MUTSU_CHECK_METHOD_INDEX=1` (0 index/table-drift assertions), the 312-file
    `S04`/`S06`/`S09`/`S11`/`S12`/`S14` roast subset plus the EVAL-specific
    `S29-context/eval.t`/`evalfile.t`, `S06-other/main-eval.t`, `S04-phasers/in-eval.t` (only the
    same two pre-existing tracked failures), and `scripts/battery-testsuite.sh` (GATE PASSED);
    `cargo clippy -- -D warnings` and `cargo fmt` clean.

    **Progress (F4c-9a-1, #TBD):** first slice of the read cutover (F4c-9a); not the whole box —
    see the remaining scope below. Migrated every `ClassDef::methods` direct-read site that is a
    genuine *downstream dispatch/introspection consumer* (not a write-family's own in-flight
    bookkeeping, and not a `RoleDef::methods` read, which (1) keeps in place) onto
    `Registry::user_method_overloads`/`get_method_overloads` (class-only sites) or
    `Registry::get_method_overloads_with_role_fallback` (the class-then-role sites R4 names).
    Seventeen files: `resolution_method.rs` (`count_visible_method_candidates`,
    `resolve_all_methods_with_owner` — the two sites R4 explicitly requires on the fallback
    helper; `resolve_method_with_owner_impl` at `:168` is untouched, confirmed still class-only
    via `get_method_overloads` per R4's own warning against unifying the file), `resolution_
    deferral.rs` (`own_overloads_at_level`, whose doc comment warned against the bare helper —
    now correctly reads as a pointer at the *with-fallback* one), `accessors_state.rs`
    (`has_multiple_dispatch_candidates` moved to the fallback helper, dropping its own manual
    class-then-role duplication; `find_method_candidate_index` at `:1116`, the R4-flagged
    "different from `:588,593`" site, moved to the class-only helper as R4 requires), `class.rs`
    (`registry_has_destroy_methods`'s class half, the per-level DESTROY fetch, and `format_
    method_candidate_signatures`; `detect_unresolved_role_method_conflicts` at `:169` is
    deliberately left untouched per its own comment — `class_def` there is not guaranteed to
    match the registry mid-call), `class_introspection.rs` (`has_user_method`; `has_user_method_
    including_role`'s role half is untouched, out of scope), `methods_walk.rs` (`WalkKind::Class`
    and `WalkKind::Role`'s receiver-class submethod fallback; `WalkKind::Role`'s own role-table
    probe and `WalkKind::MixinRole` are untouched, out of scope), `methods_classhow_lookup.rs`
    (the MRO-walk in `classhow_lookup`, both lookups in `classhow_lookup_all_candidates`; the
    role-only fallback outside the MRO loop is untouched), `methods_classhow_method_obj.rs`
    (the two public-attribute-shadow checks in `collect_class_methods`/`class_method_table` —
    the enumeration halves of these same two functions were already migrated in F4c-1),
    `methods_qualified.rs`, `methods_signature_shaped.rs` (both `method_exists` MRO scans),
    `methods_dispatch_new.rs` (`run_user_buildall_hook`'s `has_user` closure),
    `methods_object_dispatch_new.rs` (`any_build`/`any_tweak`), `metamodel.rs`
    (`declare_drive_how_protocol`'s per-name value fetch — its enumeration half was already
    migrated in F4c-1; this closes out the function's own leftover redundant class_def re-lookup),
    `registration.rs` (`inherited_matching_method_count`, `inherited_any_concrete_method`, and
    `resolve_class_stub_requirements`'s per-name value read — the last of these reads via
    `self.registry()` instead of the `&mut ClassDef` parameter, relying on the same "already
    published to the registry by this point" invariant the function's own F4c-1-era comment
    already documents for the enumeration half; verified safe because the parameter is a plain
    owned local, not borrowed from `self`, so no aliasing risk), `regex/regex_match_atom.rs`, and
    `compiler/helpers_method_body.rs` (a test-only fixture-comparison helper, moved to the
    fallback helper for symmetry with `resolve_all_methods_with_owner`). Every write-family site
    (registration_class_body*.rs, registration_class_augment.rs, registration_class_compose*.rs,
    methods_classhow_dispatch.rs's MOP writers, system.rs, builtins_system_require.rs,
    accessors_resolve.rs's `compile_class_methods`) is untouched — those still read/write
    `class_def.methods` as part of the dual-write bridge itself and are F4c-9b's job, not 9a's.
    `class_dispatch.rs:228` is untouched per the box's own instruction (F6's carrier deletes it
    for free). Verified with the full local `t/` suite (3185 files, 29665 tests) under
    `MUTSU_CHECK_METHOD_INDEX=1` (0 index/table-drift assertions); the same 243-file
    `S04`/`S06`/`S09`/`S12`/`S14` roast subset produced byte-identical failure output against a
    `main` baseline build (the same 7 pre-existing non-whitelisted failures, none newly broken —
    `S06-advanced/caller.t`, `S06-advanced/return_function.t`, `S12-attributes/trusts.t`,
    `S12-class/open_closed.t`, `S12-meta/exporthow.t`, `S12-traits/basic.t`, `S12-traits/
    parameterized.t`); `scripts/battery-testsuite.sh` (GATE PASSED, 245/271); `cargo clippy -- -D
    warnings` and `cargo fmt` clean. **Remaining F4c-9a scope** (deferred, not done by this
    slice): re-auditing the write-family files once F4c-9b is ready to retire their
    `class_def.methods` half.

    **Progress (F4c-9a-2, #TBD):** closes out `class.rs`'s `detect_unresolved_role_method_
    conflicts`, the one site F4c-9a-1 deliberately left alone because its own comment warned that
    `class_def` is not guaranteed to match the registry's `owner_method_names` mid-`finalize_
    class_registration` (the call runs right after `resolve_class_stub_requirements`, before the
    post-stub-resolution `class_def` is re-synced). That staleness gap turned out to already be
    closed: `resolve_class_stub_requirements`'s own mutation loop (F4c-3) dual-writes every
    `class_def.methods` add/remove straight to the registry via the mutator API, so the two are
    kept in lockstep even mid-call now. Confirmed empirically, not just by re-reading the code,
    before touching the site: this function's own pre-existing F4c-1 shadow check
    (`shadow_check_owner_method_names`) was run under `MUTSU_VM_STATS=1` across the full local
    `t/` suite (3185 files) and the 122-file `S12`/`S14` role-composition-conflict-heavy roast
    subset -- zero mismatches in either sweep, i.e. the shadow check itself supplied the
    confirmation its own comment demanded. Cut over to `Registry::owner_method_names` +
    `Registry::user_method_overloads`, matching the other seven F4c-1 sites; dropped the now-dead
    `class_def: &ClassDef` parameter (its one caller, `finalize_class_registration`, updated) and,
    since this was the shadow check's last remaining caller, retired the whole F4c-1 shadow-check
    apparatus it was the last user of: `Registry::shadow_check_owner_method_names`
    (`registry_method_table.rs`) and `vm_stats::record_owner_method_names_shadow_check` plus its
    two atomics, per-site mismatch map, and exit-time print block (`vm_stats.rs`) -- the box's own
    "write-through deletes existing workarounds" payoff pattern, applied to a verification
    scaffold instead of a runtime workaround. Verified with the full local `t/` suite (3185 files,
    29665 tests, green) and the same 243-file roast subset producing byte-identical failure output
    against the `main` baseline (same 7 pre-existing failures, zero new breakage); `scripts/
    battery-testsuite.sh` (GATE PASSED, 245/271); `cargo build`, `cargo clippy -- -D warnings`,
    `cargo fmt` all clean (confirming no other caller depended on the retired shadow-check API).
    **F4c-9a is now fully closed** apart from re-auditing the write-family files at F4c-9b time.

    **Progress (F4c-9b, #TBD): F4c is now fully closed.** Flipped every remaining dual-write site
    to single-write-through-the-mutator-API, deleted `ClassDef::methods`, and deleted
    `sync_user_method_entries` itself. This is the box's headline "invert the write direction and
    remove the field" step, done as one coherent PR (per CLAUDE.md's "prefer one coherent
    architectural PR over ten micro-PRs" -- the write sites are too interdependent to split
    safely: deleting the field is an all-or-nothing compile-time fact). Twenty-three files.

    **(1) Write-site conversions**, each dropping the `class_def.methods` half and keeping only
    the mutator call already dual-writing there since F4c-3/4/5/6: `registration_class_body_
    method.rs` (multi push, non-multi retain+push, `handles` delegation), `registration_class_
    body.rs` (`our &alias ::= &m`), `registration_class_compose.rs` and `_compose_body.rs` (role-
    method composition into the class), `registration_class.rs`'s `apply_handle_specs` (split the
    shared `apply_resolved_handles` helper so the class path only still uses it for the wildcard
    half -- the method half writes straight to the registry now; the role path,
    `apply_handle_specs_to_role`, is untouched, still using the full shared helper since `RoleDef::
    methods` stays), `registration_class_augment.rs` (five sites: method decl, both `handles`
    blocks, `compose_role_into_augmented_class`, and the role-pun class literal in `ensure_role_
    pun_class` -- this last one drops the `methods:` field from the `ClassDef` struct literal
    entirely and moves its content to a post-insert `set_user_methods` loop), `methods_classhow_
    dispatch.rs` (`^add_method`/`^add_multi_method` -- `^add_multi_method` keeps its `classes.
    contains_key` existence gate per design note (0)(iii), now checked directly instead of via an
    `Option<&mut ClassDef>` match), `system.rs` (BEGIN-time method statements), and `types/role_
    mixin_class.rs`'s `compose_mixin_role_submethods`.

    **(2) The redeclaration self-healing gap (R1, the headline risk) -- found and fixed, not just
    inherited.** Composition's dual-writes are *appends* (`push_user_method`), and pre-9b this was
    masked because `publish_class_shell`'s `sync_user_method_entries` call *rebuilt* the registry
    from the freshly-composed `class_def.methods` afterward, silently discarding whatever the
    dual-write had appended onto a stale prior declaration's rows. Deleting that rebuild step
    would have let a redeclared class accumulate duplicate role-composed methods forever. Fixed by
    moving the clear earlier: `register_class_decl` now calls `clear_user_methods_for_owner`
    immediately after `begin_class_def`, before composition runs, so composition always appends
    onto a clean slate. This also closes a *pre-existing*, independent gap: composition's `?`
    early-return on failure had no rollback at all before this change (a failed `does` composition
    could already leave dangling `method_entries` rows with no owning `ClassDef`, pre-9b too,
    just less consequential because `class_def.methods` was the actually-consulted copy at read
    time -- the field deletion makes the registry the sole record, so this pre-existing gap had to
    be closed in the same slice). Fixed by adding an explicit `snapshot.restore` on composition
    failure. `publish_class_shell`'s own trailing `sync_user_method_entries` calls (both the
    normal and `is_stub_body` paths) are now dead weight and deleted outright -- the owner's rows
    are already correct by the time it runs.

    **(3) The other four `sync_user_method_entries` calling contexts**, each replaced with the
    mechanism actually needed instead of a blanket rebuild: `finalize_class_registration`'s
    trailing call (`registration_class_body_exit.rs`) was a pure no-op by the time it ran (the
    per-statement loop and `resolve_class_stub_requirements` had already kept the registry
    correct) -- deleted with no replacement. `ClassRegSnapshot::restore` (`registration_class_
    validate.rs`, the F4c-8 rollback path) already had `restore_user_method_rows` doing the real
    work; only needed to add the accessor re-derive (`sync_accessor_entries`) it was implicitly
    getting as `sync_user_method_entries`'s surviving half. `compose_role_into_augmented_class`
    (`registration_class_augment.rs`) similarly only needed the accessor re-derive, not a method
    rebuild (its own method rows are already correct going in). The EVAL rollback path
    (`system_eval_string.rs`, F4c-8(b)) needed real new machinery: since `classes_snapshot` no
    longer carries method rows, a parallel `method_rows_snapshot: HashMap<String, Vec<(Symbol,
    Vec<MethodDef>)>>` is captured for every class alongside it (before the EVAL runs), and the
    resurrected-classes repair loop now calls `restore_user_method_rows` + `sync_accessor_entries`
    per resurrected owner instead of a rebuild. The `require`-alias copy (`builtins_system_
    require.rs`, F4c-7's territory) similarly needed a real replacement: `user_method_rows_for_
    owner` on the source name, `restore_user_method_rows` + `sync_accessor_entries` on the alias
    name -- copying between two different owners, which `restore_user_method_rows`'s signature
    (rows carry no owner of their own) supports directly. `runtime_init.rs`'s startup builtin-
    class seeding loop (`for class_name in classes.keys() { sync_user_method_entries }`) is now
    `sync_accessor_entries` only -- every seeded `ClassDef` has zero methods by construction, so
    only the accessor derive from `attributes` was ever doing anything there.

    **(4) Remaining readers migrated as a prerequisite for field deletion** (not previously caught
    by F4c-9a-1/2's downstream-consumer sweep, since these are write-adjacent files that sweep
    deliberately skipped): `resolve_class_stub_requirements` and `check_private_calls_exist_expr`
    (`registration.rs`, both now read `Registry::user_method_overloads` instead of the in-flight/
    fetched `ClassDef`; `resolve_class_stub_requirements` also lost its now-fully-unused
    `class_def: &mut ClassDef` parameter, and its caller `finalize_class_registration` stopped
    passing it), `methods_object.rs`'s `is_native_default_constructible` (the one F4c-9a-1 missed
    from the design note's own (0)(iv) ground-truth correction list), `accessors_resolve.rs`'s
    `check_class_native_readonly_param_errors` and `compile_class_methods` (the latter now uses
    the purpose-built `Registry::map_user_methods_in_place` mutator instead of mutating `class_def.
    methods` in place then rebuilding), and `class_dispatch.rs:228`
    (`instance_method_not_found`'s `has_visible_method` scan) -- the one site the F4c design note
    explicitly said to *skip* as "F6's carrier, cut over for free" is no longer skippable: the
    field is gone, so every remaining reference had to move regardless of which future box would
    otherwise have deleted it. F6's own text (this box, a few lines below) is stale on this one
    point now -- `class_dispatch.rs:228` reads `user_method_overloads` today, not `class_def.
    methods`, though the surrounding `run_instance_method` carrier F6 targets is otherwise
    unchanged.

    **(5) Registry unit test.** `user_override_shares_the_builtin_method_entry`
    (`registry.rs`) constructed a `ClassDef` with a `methods` field and called `sync_user_method_
    entries` to exercise the builtin/user-row-sharing invariant; rewritten to call `set_user_
    methods`/`clear_user_methods_for_owner` directly, same assertions.

    **(6) `ClassDef::methods` field deletion mechanics.** Beyond the write/read sites above, ~90
    `methods: HashMap::new()` struct-literal fields across `runtime_init.rs` (71 `ClassDef`
    literals, mechanically stripped with a script that tracked `ClassDef{`/`RoleDef{` brace depth
    so the 7 `RoleDef` literals in the same file -- which keep their `methods` field -- were left
    untouched) and five other files (`registration_class_validate.rs`, `registration_class_
    compose_body.rs`, `methods_object_native_ctors_io.rs`, `methods_classhow_dispatch.rs`,
    `methods_object_dispatch_new.rs`) needed the field dropped, per the box's own "these merely
    lose a field" framing for the seed initializers.

    Verified with the full local `t/` suite (3185 files, 29668 tests, green) under
    `MUTSU_CHECK_METHOD_INDEX=1` (0 index/table-drift assertions -- the index invariant this
    checks is unaffected by the write-direction inversion, since both sides of that invariant are
    registry-internal); the same 243-file `S04`/`S06`/`S09`/`S12`/`S14` roast subset producing
    byte-identical failure output against a `main`-baseline release build (same 7 pre-existing
    non-whitelisted failures, zero new breakage); `scripts/battery-testsuite.sh` (GATE PASSED,
    245/271, identical to the F4c-9a runs -- notably exercises `OO::Monitors`, the EXPORTHOW::
    DECLARE/MOP-heaviest battery and the one most likely to catch a `^add_method`/`^add_multi_
    method` regression from item (1)); `cargo build`, `cargo clippy -- -D warnings`, `cargo fmt`
    all clean.

    **Correction/fix found by CI, not local verification (R8, the lock-reentrancy hazard).**
    `class_body_code_alias` (item (1)'s `registration_class_body.rs` entry, `our &alias ::= &m`)
    originally read `if let Some(overloads) = self.registry().user_method_overloads(cx.name,
    source_name) { self.registry_mut().set_user_methods(...); }` -- exactly the R8 shape this
    ADR's own F4c-3/F4c-4/F4c-6 progress notes already warn about: a `self.registry()` temporary
    used directly as an `if let` scrutinee has its `RegistryReadGuard` lifetime-extended for the
    WHOLE if-let body by Rust's temporary-extension rule, so the nested `self.registry_mut()` call
    is a same-thread recursive `RwLock` acquisition -- a deadlock, not a panic, so it surfaced as a
    CI timeout (`roast/S13-syntax/aliasing.t`, exit 124) rather than a local test failure; none of
    this box's own local verification (full `t/`, the roast subset, the battery gate) exercises
    `our &alias ::= &method` inside a class body, so it passed clean locally and only CI's fuller
    roast run caught it. Fixed by hoisting the `Option<Vec<MethodDef>>` to an owned `let` binding
    before the `if let`, matching the established safe idiom this file and `registration_class_
    compose_body.rs` already use elsewhere (with their own explanatory comments predating this
    box). Audited every other `if let Some(...) = self.registry()...` site this box's diff touches
    or added for the same shape (`registration_class_body_method.rs`, `registration_class_augment.
    rs` — both already safe, the `registry_mut()` calls sit after the `if let` block closes, not
    inside it) -- no second instance found. Regression-pinned locally in `t/method-alias-decl-no-
    deadlock.t` (roast's own `S13-syntax/aliasing.t` already covers it, but is not always run
    locally). Re-verified after the fix: full local `t/` (3185 files, 29668 tests, green), the same
    243-file roast subset (byte-identical to `main` baseline), `scripts/battery-testsuite.sh`
    (GATE PASSED, 245/271).
  F6 does not have to wait on F4 as a whole: only `class_dispatch.rs:228` couples them, so F6's
  caller-reduction slices (migrating the ~40 `run_instance_method` references off the carrier,
  one family at a time) can proceed in parallel with F4a/b/c and simply pick up that one site
  last, right before the carrier itself is deleted.
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
- [x] **F6 — Delete compatibility call carriers and dead resolver modules.** Remove the
  `run_instance_method` family — three live functions plus two resolved-path helpers in
  `class_dispatch.rs` and the `vm_run_instance_method` carrier, ~700 lines with ~40 references —
  and the name/arity lookup facades once no caller remains. Also delete the eight stale doc
  comments that reference the already-removed `run_instance_method_resolved`.

  **Closed 2026-08-17 by amending the completion criterion, matching D10's own precedent.** The
  box's literal target — deleting the whole `run_instance_method`/`run_instance_method_at`/
  `run_instance_method_celled`/`instance_method_not_found`/`run_resolved_instance_method`/
  `forward_resolved_delegation` surface outright — turned out to be unreachable, not merely
  unfinished: every migration slice above found genuine, permanent reasons a caller must keep a
  fallback into this surface (an on-demand-compile/delegation-forwarder/writeback-merge
  orchestration layer with no duplicate elsewhere, a residual `COERCE`-no-match edge case,
  value-dependent multi-method resolution the cacheable-multi gate correctly declines to
  fast-path, an augmented-native-type `.new` shape), and — per this same session's correction
  above — `run_resolved_instance_method`/`instance_method_not_found` are independently load-bearing
  for proto-method `{*}` redispatch (`dispatch_proto_call.rs`), entirely unrelated to the
  `run_instance_method` carrier this box targets. **The corrected completion criterion:** every
  named caller family (coercion, mut-lvalue, qualified-dispatch, instance-ops, mut-dispatch,
  new-dispatch, general-call-dispatch) tries the modern direct-dispatch resolver
  (`try_dispatch_compiled_method_direct`/`_as`/`_with_attrs_cell`,
  `src/vm/vm_call_method_compiled_direct.rs`) FIRST and falls back to the carrier only for cases
  the direct resolver cannot serve — not "the carrier has zero remaining callers." Under that
  reading every named family is migrated; the carrier itself is retained as necessary cold-path/
  independently-used machinery, not further technical debt to chase. The eight stale doc comments
  were fixed in place (renamed to the function each actually describes — `forward_resolved_delegation`,
  `run_resolved_instance_method`, `run_resolved_method_celled`, or
  `run_resolved_method_compiled_or_treewalk`) rather than deleted, since each carries real,
  still-accurate information once correctly named. `news/2026-08/adr0019-f6-vm-level-dispatch-helper-landed-and-doc-cleanup.md`.

  **Scoping (2026-08-15, read-only, no code):** a full grep of every `self.run_instance_method(`
  call site (excluding the definitions themselves) found 15 sites across 7 caller families, close
  to the box's own "~14" estimate: **new-dispatch** (`methods_object_dispatch_new.rs:61,1418,1573`
  — direct user `.new`, a role-punned `.new`, and the general new-dispatch fallback);
  **general call-dispatch fallback** (`methods_call_dispatch.rs:70,581,3942` — the native-lever-A
  user-override branch inside `call_method_with_values` itself, the general by-name dispatch
  fallback, and a mixin/inner-instance dispatch); **instance-ops/pseudo-method** (`methods_instance_
  ops.rs:1308,1661,1699,1870` — accessor-vs-method resolution, Package/type-object dispatch,
  `Routine`/`Block`/`Code`/`Callable` ancestor dispatch, and a `.raku`-rendering coercion);
  **coercion** (`types/coercion.rs:195`, one site — a user-defined coercion method e.g. `method
  Str {...}`); **qualified dispatch** (`methods_qualified.rs:397`, one site — already E7-step-2
  shadow-checked at its OWN `resolve_method_with_owner` probe, which is separate from and prior to
  this call); **mut dispatch** (`methods_mut_dispatch.rs:28,2777` — the same native-lever-A branch
  mirrored for the mut path, and the general mut-dispatch fallback); **mut lvalue**
  (`methods_mut_method_lvalue.rs:1538`, one site — Proxy/STORE dispatch for lvalue method calls).
  An eighth, already-tagged family (`vm_core_helpers.rs`'s `vm_run_instance_method`, called from
  `vm_exec_dispatch.rs`'s `CallDefined`/`SinkPop` handling) is E7 step 1's own site and already
  shadow-checked; it is not re-scoped here.

  **Key finding — the carrier's own resolver is NOT the unified E3/E4 one.**
  `run_instance_method_celled` resolves via `resolve_method_with_owner_invocant` ->
  `resolve_method_with_owner_impl` (`resolution_method.rs`): a per-call ad-hoc MRO walk that reads
  `get_method_overloads` fresh at every level and matches candidates against live argument VALUES
  directly (`where`/type-constraint checks inline). This is textually distinct from the modern
  cached path `resolve_method_cached` -> `resolve_via_sequence_cache` -> `pick_method_winner_from_
  sequence` (`vm_call_method_compiled_cache.rs` / `resolution_sequence.rs`), which resolves against
  a cached, TypeId-keyed *candidate sequence* (`resolve_sequence`) and picks the winner from that
  cached shape. `resolve_sequence`'s own doc comment says it "mirrors the membership rules
  `resolve_method_with_owner_impl` applies per candidate ... but not its early-stopping MRO-walk
  control flow" — i.e. these are two independent implementations of the same semantics, not one
  resolver wearing two names. E7 step 1 already proved (corpus shadow-check, `vm_run_instance_
  method`'s one tagged site) that they agree in practice, but the other 7 families named above have
  never been shadow-checked against each other — `run_instance_method_at`'s own doc comment already
  flags this ("the ~14 other `run_instance_method` callers ... stay unmeasured until their own E7
  sub-slice tags them with a distinct `site` name"). The `site`/`shadow_check_resolver` plumbing
  already exists and needs no new mechanism — each family's first step is simply passing its own
  `site` tag through `run_instance_method_at` instead of the untagged `run_instance_method`, same as
  E7 step 1 did, before any cutover is attempted.

  **Key finding — carrier deletion is not a resolver swap, it is a per-site rewrite.** F6's target
  end-state removes the whole `run_instance_method`/`run_instance_method_at`/`run_instance_method_
  celled`/`instance_method_not_found`/`run_resolved_instance_method` API surface, not just its
  internal resolver call. That means each caller must stop calling into this `(receiver_class_name:
  &str, attributes: AttrMap, method_name, args, invocant: Option<Value>)`-shaped API entirely — most
  likely replaced by constructing/reusing a full `target: Value` (several call sites already have
  one, e.g. `types/coercion.rs`'s `value`, `methods_call_dispatch.rs:3942`'s `target.clone()`) and
  calling `call_method_with_values`/the VM's own compiled-dispatch entry point instead — NOT a
  mechanical find-replace, since `call_method_with_values` itself is one of the seven families
  (`methods_call_dispatch.rs:70`, its own native-lever-A fallback branch) and cannot be the
  migration target for its own caller without infinite regress; that site specifically needs the
  VM-level `resolve_method_cached`/`dispatch_compiled_method` pair directly. Each family also
  differs in what it does with the returned `(Value, AttrMap)` — some discard the map entirely
  (`Option::None` invocant, no cell to commit to), others thread it back into a live cell — so the
  post-migration commit logic needs individual review per family, not a shared helper assumed safe
  by pattern-match (the same discipline F4a's own box text insists on for its role-fallback
  candidates).

  **Recommended next step:** start with the smallest, most isolated family — `types/coercion.rs`'s
  single site — as F6's first sub-slice: tag it with its own `site` string, gather shadow-check
  corpus evidence (full `t/` + a coercion-heavy roast subset) that the ad-hoc walk and the sequence
  resolver agree, then migrate the call site itself off the `run_instance_method` API. Only after
  several families have independently proven the two resolvers agree does deleting the ad-hoc walker
  inside `run_instance_method_celled` itself (as opposed to deleting the whole carrier) become safe
  to consider as a separate, later step.

  **Progress (coercion family, step 1 — tag + gather evidence, #TBD):** `try_coerce_value_with_method`
  (`types/coercion.rs`, the sole call in the `ValueView::Instance` + `class_has_user_method` branch —
  the narrow "the source instance's own class declares a method literally named after the target
  type" coercion shape, e.g. `class HasInt { method Int { 42 } }` feeding `my Int() $x = HasInt.new`)
  switched from the untagged `run_instance_method` to `run_instance_method_at("coercion", ...)`.
  Purely additive — a no-op unless `MUTSU_VM_STATS` is set — so this alone changes no behavior; its
  purpose is corpus evidence before the actual call-site migration (still open, see below). Verified
  with a full local `t/` sweep (3187 files, one process per file, `MUTSU_VM_STATS=1`): the "coercion"
  site never once appears in the mismatch-by-site breakdown (2 total mismatches recorded corpus-wide,
  both pre-existing and tagged `privatedispatch`, unrelated to this site). Confirmed the site is
  genuinely exercised (not merely silent from zero traffic) via a targeted repro
  (`my Int() $x = HasInt.new` with `method Int {...}`) showing `resolver_shadow_checks=1
  resolver_shadow_mismatches=0`; the local `t/` corpus itself does not happen to isolate this one
  branch in an existing pinned test (the closer-named `t/any-type-object-int-coercion.t` /
  `t/stringy-numeric-object.t` exercise the general `.Int`/`.Numeric`/`.Str` conversion protocol via a
  different call path, not this branch — confirmed with a `rust-gdb -batch` breakpoint at the call
  site, which never fired for either file). `roast/S12-coercion/*.t` and `roast/S13-overloading/*.t`
  (release, via `scripts/run-roast-test.sh`) both fully green. **Remaining for this family:** the
  call site itself still goes through `run_instance_method_at` (not yet migrated to
  `call_method_with_values`/the VM-level resolved-dispatch entry point) — that migration, plus
  confirming which of `call_method_with_values`'s many `ValueView::Instance` branches a plain
  user-method-named-after-a-type call would actually reach, is deliberately deferred to its own
  follow-up slice rather than bundled here, matching this box's "post-migration commit logic needs
  individual review per family" caution.

  **Progress (mut-lvalue family, step 1 — tag + gather evidence, #TBD):**
  `assign_method_lvalue_with_values`'s sole call (`methods_mut_method_lvalue.rs`, the "method body
  doesn't directly expose an attribute — run it and check for Proxy" fallback used for `is rw`
  method/STORE-dispatch lvalue assignment, `invocant: None`) tagged with `run_instance_method_at
  ("mutlvalue", ...)`, same pattern as the coercion family's step 1 above. Verified with a full local
  `t/` sweep (3187 files, `MUTSU_VM_STATS=1`): the "mutlvalue" site never appears in the
  mismatch-by-site breakdown (the same 2 pre-existing `privatedispatch` mismatches as every other
  sweep in this box, unrelated to this site); `t/dot-assign-accessor.t` confirms the site is
  genuinely exercised (`resolver_shadow_checks=1 resolver_shadow_mismatches=0` for that file alone).
  `roast/S06-routine-modifiers/{lvalue-subroutines,proxy}.t`, `roast/S12-attributes/mutators.t`,
  `roast/S12-introspection/attributes.t`, and `roast/S12-class/attributes.t` (release, via
  `scripts/run-roast-test.sh`) all green. Call-site migration off the API itself remains open, same
  as the coercion family.

  **Progress (qualified-dispatch family, #TBD):** unlike coercion/mut-lvalue, this family
  (`dispatch_qualified_instance_method`, `methods_qualified.rs`, `self.Owner::method(...)`) already
  had its shadow-check evidence from Phase E box E7 step 2 (`shadow_check_resolver_chain("qualifieddispatch",
  ...)`, gated inline since the function has exactly one caller) — so this slice goes straight to the
  call-site migration itself rather than adding a tag. The old code discarded the `resolve_method_with_
  owner` result's `(owner, method_def)` after the shadow check and called `self.run_instance_method
  (qualifier, ...)`, which re-derived the identical resolution a second time via its own internal
  `resolve_method_with_owner_invocant` walk (the `let _ = method_def;` a few lines down was silencing
  the now-unused destructured binding — a tell that the resolved value was being thrown away). Switched
  to `self.run_resolved_method_compiled_or_treewalk(qualifier, &owner.resolve(), actual_method,
  method_def, attrs_map, args, Some(target.clone()))`, reusing the already-resolved `(owner, method_def)`
  directly — the same helper the sibling role-punned branch a few lines above already uses for the same
  reason. Eliminates the redundant double-resolve; `run_resolved_method_compiled_or_treewalk` remains
  part of the same `run_instance_method` family this box will eventually delete, so this is progress
  within F6, not F6's finish line for this site. Verified with the full local `t/` suite (3187 files,
  `MUTSU_CHECK_METHOD_INDEX=1`, all green, 0 index-drift assertions) and the standard 312-file
  `S04`/`S06`/`S09`/`S12`/`S14` roast subset (release, all green) plus every roast file with a bare
  `self.Owner::method` qualified call (`S12-class/inheritance.t`, `S12-construction/new.t`,
  `S12-methods/{delegation,qualified}.t`, `S14-roles/{basic,conflicts,lexical}.t`).

  **Progress (instance-ops/pseudo-method family, #TBD):** `methods_instance_ops.rs`'s four named
  sites. Three (accessor-vs-method resolution ~1308, Package/type-object dispatch ~1661,
  Routine/Block/Code/Callable ancestor dispatch ~1699) tagged `run_instance_method_at("instanceops",
  ...)`, same additive tag-and-gather pattern as coercion/mut-lvalue. The fourth (`.raku`/`.perl`
  rendering of a `Junction`'s member Instances, ~1891) already had a discarded pre-resolved
  `(owner, method_def)` from `resolve_method_with_owner` — same shape the qualified-dispatch slice
  above just fixed — so it was migrated straight to `run_resolved_method_compiled_or_treewalk`
  reusing that value, not merely tagged.

  **Real finding, not just a no-op:** the full local `t/` sweep (3187 files, `MUTSU_VM_STATS=1`) this
  time surfaced genuine mismatches — 9 of them, all `"instanceops"`, all shape `real=Some(owner)
  shadow=None` (the ad-hoc `run_instance_method` walk finds the method; the E4 `resolve_sequence`
  resolver does not; never the reverse). All 9 trace to the Package/type-object dispatch branch
  (~1661): `t/role-pun-dispatches-on-type-object.t` (6), `t/nested-type-short-name-owner-scope.t` (1,
  a qualified nested-package type object), `t/role-instantiation.t`'s `NotNewPun.x` (a role pun's
  non-`.new` method), and a `role R { multi method COERCE {...} }` type-object coercion call —
  i.e. every case is a **type-object receiver** (`Definedness::TypeObject`, not a live instance).
  Filed as `todo/tickets/adr0019-e4-sequence-resolver-misses-type-object-dispatch.md`: harmless today
  (shadow-only, nothing consumes the comparison to make a real dispatch decision) but it means the
  Package-dispatch branch specifically **cannot** be migrated off `run_instance_method` to the
  sequence resolver until that gap is closed — this box's own "gather evidence before migrating"
  discipline (F6's box text) working exactly as intended. The other two tagged sites (~1308, ~1699)
  and the fourth (migrated) site show zero mismatches.

  Verified with the full local `t/` suite (3187 files, `MUTSU_VM_STATS=1` for the shadow sweep,
  `cargo build`/`clippy`/`fmt` all clean) and the standard 312-file `S04`/`S06`/`S09`/`S12`/`S14`
  roast subset (release), both green — the 9 mismatches are diagnostic-only and change no observed
  behavior. `scripts/battery-testsuite.sh` GATE PASSED.

  **Progress (mut-dispatch family, #TBD) — closes the instanceops ticket with a real fix, not just a
  tag.** `methods_mut_dispatch.rs`'s two named sites (the native-lever-A mirror at
  `call_method_mut_with_values`'s top, and the general mut-dispatch fallback near its end) tagged
  `run_instance_method_at("mutdispatch", ...)`, same additive pattern as every prior family. The
  corpus-evidence sweep (full local `t/`, 3189 files, `MUTSU_VM_STATS=1`) found one mismatch,
  `t/role-bless-pun.t`'s `Service.bless(...).start`/`.running` — real=Some("Service") shadow=None,
  the same shape as the instance-ops family's already-filed
  `todo/tickets/adr0019-e4-sequence-resolver-misses-type-object-dispatch.md`. Root-caused this time:
  `resolve_sequence`'s `drop_flattened_role_duplicate_candidates` step drops a User candidate
  whenever ANY candidate's `role_origin` names its owner — meant to remove a role's raw MRO-level
  copy once a differently-owned class level already carries the role-flattened copy, but a role
  **pun** (`Service.bless`/`.new` on a bare role, `ensure_role_punned_to_class`) copies the role's
  methods into a synthetic class registered under the role's own name, self-tagging
  `role_origin = Some(role_name) == owner`. The filter didn't distinguish this self-reference from a
  genuine cross-owner duplicate, so a pun's sole MRO level deleted itself. Fixed by only adding a
  `role_origin` to the dedup set when it names a DIFFERENT owner than the candidate carrying it —
  matching `resolve_method_with_owner_impl` (the ad-hoc resolver this sequence is meant to
  reproduce), which never drops a pun's own single-level candidate. This is a real, in-scope fix
  (not deferred): it also closes all 9 of the instance-ops ticket's mismatches, which were the same
  self-referential-pun shape reached via a type-object receiver (`NotNewPun.x`) instead of a bless'd
  instance — confirmed by re-running that ticket's exact repro (now `resolver_shadow_mismatches=0`)
  and by the post-fix full sweep finding zero `"mutdispatch"`/`"instanceops"` mismatches corpus-wide
  (the only 2 remaining are the pre-existing, unrelated `"privatedispatch"` pair every prior sweep in
  this box has already recorded). `todo/tickets/adr0019-e4-sequence-resolver-misses-type-object-
  dispatch.md` retired to `news/2026-08/adr0019-f6-mut-dispatch-and-role-pun-dedup-bug.md`. As a side
  effect, the fix lets the VM's cached fast dispatch path (`resolve_method_cached` ->
  `resolve_via_sequence_cache`, which reads `resolve_sequence`) resolve a role pun's methods
  directly instead of always missing and falling through to the slow `run_instance_method` path —
  observed in `t/role-bless-pun.t`, where the newly-tagged `"mutdispatch"` sites stopped firing at
  all once the cached path started succeeding on its own. Verified with the full local `t/` suite
  (3189 files, `cargo build`/`clippy`/`fmt` all clean), the 309-file whitelisted subset of the
  standard `S04`/`S06`/`S09`/`S12`/`S14` roast slice (release), and `scripts/battery-testsuite.sh`
  (GATE PASSED).

  **Progress (new-dispatch family, #TBD):** `methods_object_dispatch_new.rs`'s three named
  `.new`-dispatch sites (the augmented-builtin-`.new` fallback, the role-punned `.new` branch, and
  the general new-dispatch fallback) tagged `run_instance_method_at("newdispatch", ...)`. The
  role-punned `.new` site is the same `ensure_role_punned_to_class` shape the mut-dispatch family's
  fix above targeted, so its evidence sweep (full local `t/`, 3190 files, before this branch was
  rebased onto that fix) found the identical self-referential-pun mismatch pattern at 4 call sites
  (`t/class-type-object-coercion-call.t`, `t/punned-role-user-new.t` x2,
  `t/role-pun-dispatches-on-type-object.t`'s `WithNew.new`) — no new root cause, just more corpus
  instances of the same bug. After rebasing onto the merged fix, the full sweep finds zero
  `"newdispatch"` mismatches (the only 2 remaining corpus-wide are the pre-existing, unrelated
  `"privatedispatch"` pair every prior sweep in this box has recorded). Verified with the full local
  `t/` suite (3190 files, `cargo build`/`clippy`/`fmt` all clean), the 309-file whitelisted subset of
  the standard `S04`/`S06`/`S09`/`S12`/`S14` roast slice (release), and
  `scripts/battery-testsuite.sh` (GATE PASSED).

  **Progress (general-call-dispatch family, #TBD):** `methods_call_dispatch.rs`'s three named sites
  — the native-lever-A user-override branch inside `call_method_with_values` itself (mirroring the
  mut-dispatch family's own native-lever-A site), the user-defined metamethod (`method ^foo(Mu)
  {...}`) dispatch branch, and the mixin/inner-instance class-method dispatch branch — tagged
  `run_instance_method_at("generalcalldispatch", ...)`, closing out this box's own 7-family scoping
  list (coercion, mut-lvalue, qualified-dispatch, instance-ops, mut-dispatch, new-dispatch, and now
  this one). A full local `t/` sweep (3190 files, `MUTSU_VM_STATS=1`) found zero new mismatches for
  this site (the only 2 remaining corpus-wide are the pre-existing, unrelated `"privatedispatch"`
  pair every prior sweep in this box has recorded) — every `run_instance_method` caller family is
  now tagged and shadow-checked. Verified with the full local `t/` suite (3190 files,
  `cargo build`/`clippy`/`fmt` all clean), the 309-file whitelisted subset of the standard
  `S04`/`S06`/`S09`/`S12`/`S14` roast slice (release), and `scripts/battery-testsuite.sh`
  (GATE PASSED).

  **All 15 `run_instance_method` call sites across all 7 scoped families now tagged.** With no
  caller left passing an empty `site`, `cargo clippy`'s dead-code lint caught the untagged
  `Self::run_instance_method` wrapper itself (the `run_instance_method_at("", ...)` thin shim every
  caller used before its own tagging slice) as unreachable — deleted it, folding its doc comment
  into `run_instance_method_at`'s and updating the one other stale "stays untagged" comment
  (`vm_core_helpers::vm_run_instance_method`). This is a real deletion inside F6's scope, not F7's
  bigger carrier removal: the celled core, `run_instance_method_at`, and the other compatibility
  surface (`run_instance_method_celled`, `instance_method_not_found`, `run_resolved_instance_method`)
  are still live — only the one now-orphaned entry point is gone. Verified with the same full local
  `t/` suite / shadow sweep / roast subset / battery gate as this family's own slice above.

  **Progress (coercion family, step 2 — first per-site carrier migration, #TBD):** following this
  box's own "Recommended next step" (the smallest, most isolated family first), `types/coercion.rs`'s
  single site migrated off `run_instance_method_at` onto `call_method_with_values(value.clone(),
  base_target, vec![])` — the invocant here is already a full `Value` (an `Instance`), so no
  `receiver_class_name`/`attributes` reconstruction is needed, and the returned `AttrMap` was already
  discarded (`let (coerced, _) = ...`) before this change, so nothing depended on it. This is F6's
  first actual carrier-caller migration (every prior slice only tagged for shadow-check evidence);
  the coercion family's own `"coercion"` `site` tag is now unused (no remaining caller) since the
  site itself no longer calls into the `run_instance_method` API at all. Verified with the full local
  `t/` suite (3190 files) plus the full local `t/`-coercion-named-file subset (67 files, all green),
  `cargo build`/`clippy`/`fmt` clean, the 314-file whitelisted `S04`/`S06`/`S09`/`S12`/`S14` +
  `S12-coercion`/`S13-overloading` roast subset (release), and `scripts/battery-testsuite.sh`
  (GATE PASSED).

  **Progress (mut-lvalue family, step 2 — carrier migration, #TBD):**
  `assign_method_lvalue_with_values`'s sole site (`methods_mut_method_lvalue.rs`, the "method body
  doesn't directly expose an attribute — run it and check for Proxy" fallback) migrated off
  `run_instance_method_at("mutlvalue", ...)` onto `call_method_with_values(target.clone(), method,
  method_args)`. Unlike the coercion family, the caller here still needed the post-call `AttrMap`
  snapshot (`proxy_store`'s `attributes` param, fed to the `STORE` callback's Proxy context) — but
  since `target` and the pre-extracted `attributes: Gc<InstanceAttrs>` share the same underlying
  cell, and self-mutations inside the called method already write through that cell in place (the
  interior-mutability guarantee ADR-0013 established), the old `run_instance_method`-returned
  `updated_attrs` snapshot was redundant: re-reading `attributes.to_map()` *after* the
  `call_method_with_values` call yields the identical post-mutation state. No functional change,
  one fewer `(Value, AttrMap)`-shaped return to thread through. Verified with the full local `t/`
  suite (3190 files, all green), `cargo build`/`clippy -- -D warnings`/`fmt` clean,
  `roast/S06-routine-modifiers/{lvalue-subroutines,proxy}.t`, `roast/S12-attributes/mutators.t`,
  `roast/S12-introspection/attributes.t`, and `roast/S12-class/attributes.t` (release, via
  `scripts/run-roast-test.sh`), and `scripts/battery-testsuite.sh` (GATE PASSED, 245/271 unchanged).
  The `"mutlvalue"` `site` tag is now unused (no remaining caller).

  **Negative result (instance-ops family, attempted and reverted, #TBD):** tried the same
  `call_method_with_values` swap on two of `methods_instance_ops.rs`'s three tagged sites (the
  accessor-vs-method resolution branch ~1308, and the Package/type-object dispatch branch ~1657 —
  the latter now that the mut-dispatch family's role-pun-dedup fix closed the E4 sequence-resolver
  gap that used to block it, see the mut-dispatch/new-dispatch progress notes above). Both caused an
  immediate stack overflow across a large swath of the local `t/` suite (dozens of files aborting
  with SIGABRT/SIGSEGV from unbounded recursion), not a subtle shadow mismatch. Root cause: **all
  three sites live inside `dispatch_instance_and_fallback`
  (`methods_instance_ops.rs:42`), which is itself called FROM `methods_call_dispatch.rs` at three
  sites reachable through `call_method_with_values`'s own call chain** — so a call site inside it
  calling back into `call_method_with_values` recurses into itself whenever the modern resolver
  falls through to the same fallback again for the same `(target, method)` (a condition that,
  unlike the already-migrated coercion/mut-lvalue sites, recurs unboundedly rather than terminating,
  since `has_user_method` keeps evaluating true). This differs from the coercion and mut-lvalue call
  sites, which live in leaf functions never reached from `call_method_with_values`'s own resolution
  chain. **Lesson: before applying the `call_method_with_values` swap to any remaining F6 site,
  first confirm the containing function is not itself in `call_method_with_values`'s call graph**
  (grep its own name as a callee inside `methods_call_dispatch.rs`) — a shadow-check-clean corpus
  sweep (as the instance-ops family's own tag-and-gather step ran) does NOT catch this, since the
  shadow check only compares resolved values, never actually invokes the swapped call path. All
  three instance-ops sites remain on `run_instance_method_at` pending a fix that goes through the
  VM-level `resolve_method_cached`/`dispatch_compiled_method` pair directly instead (the same
  direction this box's own "general-call-dispatch fallback" family note already flagged as required
  for `call_method_with_values`'s OWN native-lever-A site, for the identical infinite-regress
  reason). `methods_instance_ops.rs` is reverted to its pre-attempt state, byte-identical to before
  this slice; no functional change landed from this attempt.

  **Progress (mut-dispatch family, step 2 — one carrier call site removed, #TBD):** the instance-ops
  finding above generalizes across every remaining family — see
  `todo/deep/adr0019-f6-vm-level-dispatch-helper-needed.md` for the full call-graph survey. One site
  turned out safe despite that survey's caution: `call_method_mut_with_values`'s own native-lever-A
  branch (`methods_mut_dispatch.rs`, the "augmented native-typed receiver" mirror of
  `call_method_with_values`'s identically-shaped top branch). Its own doc comment already establishes
  that a native-typed receiver here carries no attribute cell, so "a plain value dispatch (like the
  non-mut path) is the correct shape" — meaning this branch never needed the mut-specific machinery
  at all. Migrated `run_instance_method_at("mutdispatch", ...)` to
  `self.call_method_with_values(target, method, args)` (the plain non-mut sibling, NOT a self-call —
  `call_method_with_values` does not call back into `call_method_mut_with_values` for this shape, so
  none of the instance-ops recursion risk applies here). This does not remove the carrier dependency
  itself (`call_method_with_values`'s own identically-shaped branch still calls
  `run_instance_method_at("generalcalldispatch", ...)` internally, per the general-call-dispatch
  family's own known self-reference blocker) — it centralizes what used to be two independent
  carrier call sites into the one the general-call-dispatch family will eventually fix, removing one
  more distinct `run_instance_method` reference and one more duplicated implementation of the same
  dispatch shape. Verified with the full local `t/` suite (3190 files, all green, no recursion),
  `t/augment-native-lever-a-methods.t`, `cargo build`/`clippy -- -D warnings`/`fmt` clean, and
  `scripts/battery-testsuite.sh` (GATE PASSED). `mut-dispatch`'s remaining site
  (`methods_mut_dispatch.rs:2777`, the general mut-dispatch fallback) stays on `run_instance_method_at`
  — it is the mut-path's own analog of `dispatch_instance_and_fallback`/`dispatch_new` and is
  presumed blocked by the same recursion shape (not yet attempted).

  **Progress (VM-level direct-dispatch helper, #TBD) — unblocks the recursion-hazard sites.**
  Implemented `Interpreter::try_dispatch_compiled_method_direct`
  (`src/vm/vm_call_method_compiled_direct.rs`, new file), the helper
  `todo/deep/adr0019-f6-vm-level-dispatch-helper-needed.md` scoped: it extracts the
  `resolve_method_cached` -> `check_method_wrap_chain` -> on-demand-compile ->
  `dispatch_compiled_method` sequence that `try_compiled_method_or_interpret_inner`
  (`vm_call_method_compiled_interpret.rs`) already runs for the VM's own `CallMethod` opcode path,
  minus that function's accessor-vs-method arbitration branch (which recurses into
  `call_method_with_values` and is therefore exactly the hazard this helper exists to avoid). Confirmed
  by direct call-graph inspection that neither `try_compiled_method_or_interpret_inner` nor any function
  it calls (`resolve_method_cached`, `check_method_wrap_chain`, `populate_uncompiled_method`,
  `dispatch_compiled_method`) is reachable from `call_method_with_values`/`call_method_mut_with_values`'s
  own bodies (`grep` for `try_compiled_method_or_interpret` in `methods_call_dispatch.rs`/
  `methods_mut_dispatch.rs` finds nothing) — so a call site inside either function's call graph can call
  this new helper without risking the self-reentry that broke the naive `call_method_with_values` swap.
  Returns `None` (caller falls back to `run_instance_method_at`) when no compiled resolution exists, when
  the resolved method still lacks bytecode after an on-demand compile attempt, or — implicitly, since the
  helper does no accessor arbitration of its own — the caller is responsible for excluding the
  accessor-should-win case before calling it (every currently-known blocked site already does this itself,
  e.g. instance-ops's `!accessor_wins &&` guard below).

  **First application (instance-ops family, accessor-vs-method resolution site,
  `methods_instance_ops.rs:~1307`):** the `!accessor_wins && self.has_user_method(...)` branch now tries
  `try_dispatch_compiled_method_direct` first and only falls back to `run_instance_method_at("instanceops",
  ...)` when it returns `None`. Per the mut-lvalue family's own finding, `target` and `attributes` share
  the same underlying cell (ADR-0013) and `dispatch_compiled_method` already commits any reconciled
  attribute map back through that cell, so re-reading `attributes.to_map()` after the direct-dispatch call
  reflects the post-mutation state without needing the carrier's own returned snapshot. Verified with the
  full local `t/` suite (3190 files, all green — no recursion, no regression), the exact `t/` files that
  exercise this branch's accessor-vs-method priority logic (`t/accessor-mro-shadowing.t`,
  `t/role-class-prioritization.t`, `t/method-table.t`, all green with `MUTSU_VM_STATS=1` confirming zero
  `"instanceops"` fallback traffic for those runs), `cargo build`/`clippy -- -D warnings`/`fmt` clean, the
  314-file whitelisted `S04`/`S06`/`S09`/`S12`/`S14` roast subset (release — the same 7 non-whitelisted
  files fail identically before and after this change, confirmed by an A/B run via `git stash`), and
  `scripts/battery-testsuite.sh` (GATE PASSED, 245/271 unchanged).

  **Progress (instance-ops family, remaining two sites, #TBD) — family closed.** Migrated the
  Package (type-object) dispatch branch (`~line 1687`, invocant `None`) onto
  `try_dispatch_compiled_method_direct` directly (target is already a `Package` view, matching the
  helper's own derivation), and the value-type dispatch branch (`~line 1732`, the
  `augment class Array`/`Routine`/`Block`/`Code`/`Callable` fallback for a bare non-Instance/
  non-Package receiver) onto the new `try_dispatch_compiled_method_direct_as` variant, which takes
  an explicit `dispatch_class` symbol instead of deriving the owner class from `target`'s own
  `ValueView` — needed here because `dispatch_class` (e.g. `"Array"`, `"Routine"`) is deliberately
  *not* the receiver's own runtime type (a bare `Sub` dispatching against the `Routine`/`Block`/
  `Code`/`Callable` MRO chain it doesn't literally carry as its `ValueView` tag). Both still fall
  back to `run_instance_method_at("instanceops", ...)` when the direct path returns `None`.
  Evidence gathered with a temporary env-gated probe (`MUTSU_DEBUG_F6_PROBE`, removed before
  commit — see the debugging guidelines' "temporary instrumented build" allowance) run across the
  full local `t/` corpus: the package-dispatch site hit the direct path 1527/1529 times, falling
  back only for a `COERCE` call with no matching candidate (`t/class-type-object-coercion-call.t`
  test 13, "a non-matching COERCE falls back to new" — the fallback's own ad-hoc `new`-redirect
  logic, not a resolver bug); the value-type dispatch site hit the direct path 14/14 times (no
  fallback observed in-corpus). A dedicated `use MONKEY-TYPING; augment class Routine {...}` /
  `augment class Block {...}` repro (raku-compared) confirmed correct output, and an `rust-gdb`
  breakpoint on each site's fallback call line confirmed it never fires for that repro (both sites
  serve the call directly). Verified with the full local `t/` suite (3191 files, all green — no
  recursion, no regression), `cargo build`/`clippy -- -D warnings`/`fmt` clean, the 314-file
  whitelisted `S04`/`S06`/`S09`/`S12`/`S14` roast subset (release — the same 7 non-whitelisted
  files fail identically before and after), and `scripts/battery-testsuite.sh` (GATE PASSED,
  245/271 unchanged). **This closes the instance-ops family**: all three of its named sites are now
  migrated off the carrier (falling back to it only for the residual COERCE edge case above). Every
  other blocked family (new-dispatch, mut-dispatch's remaining site, general-call-dispatch,
  qualified-dispatch's shared helper) remains open — each needs its own per-site review of what it
  does with the returned value beyond the resolved method (same "no shared-helper-by-pattern-match"
  discipline this box has followed throughout), but now has the same concrete, verified-safe
  direct-dispatch path (`try_dispatch_compiled_method_direct`/`_as`) to migrate onto.

  **Progress (mut-dispatch family, remaining site — family closed, #TBD).** Migrated the general
  mut-dispatch fallback (`call_method_mut_with_values`'s own `has_user_method(...)` branch,
  `methods_mut_dispatch.rs:~2763`) onto `try_dispatch_compiled_method_direct`, falling back to
  `run_instance_method_at("mutdispatch", ...)` when it returns `None`. Same write-back reasoning as
  the instance-ops and mut-lvalue families: `target` and `attributes` share the cell
  (`Value::instance_sharing_cell`/ADR-0013), so a fresh `attributes.to_map()` read after the direct
  call reflects the post-mutation state without the carrier's own returned snapshot. Gathered
  evidence with the same temporary env-gated probe technique as the instance-ops slice (removed
  before commit): across the full local `t/` corpus the direct path hit 107/111 times, falling back
  only for value-dependent multi-method resolution the modern resolver's cacheable-multi gate
  correctly declines to fast-path (`t/multi-method.t`, `t/multi-new-default-fallback.t`,
  `t/multi-num-param-strictness.t` — all pass either way). Verified with the full local `t/` suite
  (3191 files; one run hit `t/supply-done-in-tap-callback-is-not-a-failure.t` test 3, a
  thread-emit-timing Supply test unrelated to method dispatch — 5/5 direct reruns and a full
  suite re-run were clean, consistent with load-sensitive flakiness rather than a regression),
  `cargo build`/`clippy -- -D warnings`/`fmt` clean, the 314-file whitelisted
  `S04`/`S06`/`S09`/`S12`/`S14` roast subset (release — the same 7 non-whitelisted files fail
  identically before/after), and `scripts/battery-testsuite.sh` (GATE PASSED, 245/271 unchanged).
  **This closes the mut-dispatch family**: both its named sites (the native-lever-A branch, migrated
  in the prior mut-dispatch slice, and this general fallback) are now off the carrier. Remaining open
  families: new-dispatch, general-call-dispatch, qualified-dispatch's shared helper.

  **Progress (new-dispatch family, all three sites — family closed, #TBD).** Migrated all three
  `methods_object_dispatch_new.rs` sites onto `try_dispatch_compiled_method_direct`: the
  augmented-builtin-`.new` fallback (`try_augmented_builtin_new`, invocant `None`, dispatches
  against a freshly-constructed `Value::package(Symbol::intern(class_key))`), the role-punned
  `.new` branch (dispatches against `target`, already the role's own type object freshly punned to
  a class by the preceding `ensure_role_punned_to_class` call), and the general new-dispatch
  fallback (dispatches against `target`, which is `Package(class_name)` throughout `dispatch_new`
  by construction — every `Instance` receiver is redirected to `Value::package(class_name)` before
  reaching this point). Each site wraps the direct-dispatch attempt to produce the exact same
  `Result<(Value, AttrMap), RuntimeError>` shape the site's own (unmodified) downstream
  error-handling logic already expects — e.g. the general fallback's proto-method/positional-arg/
  default-constructor fallthrough chain, and `try_augmented_builtin_new`'s
  `is_multi_no_match -> Ok(None)` translation — so none of that existing logic needed to change,
  only the single `run_instance_method_at` call each site made.

  Gathered evidence with the same temporary env-gated probe technique (removed before commit)
  across the full local `t/` corpus: the augmented-builtin-`.new` site fell back 100% of the time
  (4/4, all in `t/augment-builtin-datetime.t`, which fully passes either way — the fast resolver
  path apparently doesn't cover this augmented-native-type shape, so this site gets no speed
  benefit yet but is still correctly carrier-free-by-default with a safe fallback); the role-pun
  site hit the direct path 4/7 times (`t/role-instantiation.t` et al., all pass); the general
  fallback hit 13/58 times, with the majority of fallbacks tracing to value-dependent `multi method
  new` candidates (`t/multi-new-default-fallback.t`, `t/proto-new-no-match.t`, ...) — the same
  cacheable-multi-gate shape already validated safe by the mut-dispatch family's own evidence, not
  a new risk. Verified with the full local `t/` suite (3191 files, all green — no recursion, no
  regression), `cargo build`/`clippy -- -D warnings`/`fmt` clean, the 314-file whitelisted
  `S04`/`S06`/`S09`/`S12`/`S14` roast subset (release — the same 7 non-whitelisted files fail
  identically before/after), and `scripts/battery-testsuite.sh` (GATE PASSED, 245/271 unchanged —
  notably exercises real `.new` construction across every bundled library, e.g. Cro/DBIish/
  JSON::Tiny). **This closes the new-dispatch family.** Remaining open families:
  general-call-dispatch, qualified-dispatch's shared helper.

  **Progress (general-call-dispatch family, partial — 1 of 3 named sites, #TBD).** Migrated the
  native-lever-A user-override branch at the very top of `call_method_with_values`
  (`methods_call_dispatch.rs:~65`) onto `try_dispatch_compiled_method_direct_as`, the same explicit-
  class-name variant the instance-ops value-type dispatch site used: `target` here is a plain native
  value (Array/Str/Range/...), not `Instance`/`Package`, so the dispatch class comes from
  `value_type_name(&target)` rather than `target.view()`. No attrs-cell propagation concern (native
  values carry no attribute cell; the original code already discarded the carrier's `updated` map).
  Verified with the full local `t/` suite (3191 files, all green), `t/augment-native-lever-a-methods.t`
  plus an `rust-gdb` breakpoint on the fallback call confirming it never fires for that file, `cargo
  build`/`clippy -- -D warnings`/`fmt` clean, the 314-file whitelisted
  `S04`/`S06`/`S09`/`S12`/`S14` roast subset (release — same 7 non-whitelisted failures before/after),
  and `scripts/battery-testsuite.sh` (GATE PASSED, 245/271 unchanged).

  **Progress (general-call-dispatch family, `^metamethod` site migrated, #TBD).** Wrote
  `t/metamethod-dispatch.t` first (6 cases: type-object receiver, inherited metamethod, extra
  positional args after the prepended type object, calling through a live instance, and repeated
  calls proving the body actually executes with retained `my`-scoped class state) — this shape had
  **zero coverage in the local `t/` corpus** before this slice (`grep -rl 'method \^' t/*.t` found
  nothing), only `roast/S14-traits/routines.t`'s indirect `is foo`-trait/`wrap` interaction test.
  Confirmed against `raku` (all 6 cases match). Re-reading the calling convention this box's prior
  note flagged as the blocker: `run_instance_method_celled`'s own `inv_value` derivation resolves
  `invocant: None` + this site's hardcoded-empty `AttrMap::new()` to `Value::package(receiver_class_
  name)` — i.e. even on today's carrier, `self` inside a metamethod is NEVER the real receiver, only
  a synthesized type-object value (matching `raku`'s own `self.WHO` returning an empty/type-like
  value there, not the instance). `dispatch_compiled_method` performs the identical `attrs_empty ->
  Value::package(cn)` computation when its own `target` carries no attribute cell. So passing that
  SAME synthesized `Value::package(class_sym)` as `try_dispatch_compiled_method_direct_as`'s
  `target` reproduces bit-identical invocant semantics — no calling-convention shift after all; the
  type object the metamethod body actually receives as its own first declared param continues to
  arrive via `args[0]` (still prepended by the VM caller), entirely independent of the invocant
  value threaded into `dispatch_compiled_method`, since positional-param binding and `self`-binding
  are separate code paths (`call_compiled_method`'s `base`/`invocant` computation vs. its
  `bind_function_args_values` call over `args`). Migrated the branch to try
  `try_dispatch_compiled_method_direct_as(class_sym, &Value::package(class_sym), method, &args)`
  first, falling back to `run_instance_method_at("generalcalldispatch", ...)` on `None`, same
  pattern as every other migrated site. Verified with the full local `t/` suite (3193 files, all
  green), the new `t/metamethod-dispatch.t`, `roast/S14-traits/routines.t` (release), the standard
  314-file whitelisted `S04`/`S06`/`S09`/`S12`/`S14` roast subset (release — the same 7
  non-whitelisted files fail identically before/after, matching every prior slice in this box),
  `cargo build`/`clippy -- -D warnings`/`fmt` clean, and `scripts/battery-testsuite.sh` (GATE
  PASSED, 245/271 unchanged).

  **Progress (general-call-dispatch family, mixin fallback site — family closed, #TBD).** Built the
  "new helper shape" this box's own prior note called for: `dispatch_compiled_method_with_attrs_cell`
  (`vm_call_method_compiled_cache.rs`) and its resolve-and-dispatch wrapper
  `try_dispatch_compiled_method_direct_with_attrs_cell` (`vm_call_method_compiled_direct.rs`), a
  sibling of `dispatch_compiled_method`/`try_dispatch_compiled_method_direct_as` that takes an
  EXPLICIT `attrs_cell: &Gc<InstanceAttrs>` separate from the dispatch invocant `target`, instead of
  deriving the cell from `target.view()`. Always takes the slow (`call_compiled_method`) path, never
  the fast (`call_compiled_method_fast`) one: the fast path's live-cell optimization reads attributes
  directly off `self`'s own `ValueView`, which requires `self` to literally be `ValueView::Instance` —
  not true for a `Mixin` wrapper, so there is no live cell for it to find; an acceptable trade for
  this cold path (role-mixin class-method dispatch is not hot-loop code). Confirmed the existing
  carrier path itself (`run_resolved_instance_method`, `class_dispatch.rs:593`) never performs the
  compiled VM opcode path's eager `ValueView::Proxy` auto-fetch either (that logic lives only in
  `dispatch_compiled_method`'s own tail, called from the VM's `CallMethod` opcode handler and
  `try_dispatch_compiled_method_direct[_as]`, never from the tree-walk-carrier's own
  `call_compiled_method` call) — so omitting it from the new helper is not a NEW behavior gap, it
  matches the site's pre-migration behavior exactly (no roast/local-`t/` file combines a mixin class
  method with a `Proxy`-returning `is rw` accessor, confirmed by `grep -l Proxy t/*.t | xargs grep -l
  'mixin\|but role'` finding nothing).

  Migrated the mixin fallback (`ValueView::Mixin(inner, mixins)` class-method branch,
  `methods_call_dispatch.rs:~3990`) to try `try_dispatch_compiled_method_direct_with_attrs_cell`
  first — passing `class_name`/`target` (the Mixin wrapper, so nested `self.foo` still redispatches
  through the mixin's role overrides) and `attributes` (the `GcRef` destructured from `inner`'s own
  `ValueView::Instance`, deref-coercing to the `&Gc<InstanceAttrs>` the helper wants) — falling back
  to `run_instance_method_at("generalcalldispatch", ...)` on `None`, same pattern as every other F6
  migration in this box. This site already had solid pre-existing local `t/` coverage (unlike the
  `^metamethod` site above): `t/mixin-inherited-method-self-dispatch.t` (8 cases: direct/inherited
  override via both mixin forms, attribute-mutation persistence through an inherited method,
  multi-hop self-dispatch, args), `t/mixin-private-method-self-dispatch.t` (5 cases, the sibling
  private-method branch just above this one — unchanged by this slice), and
  `t/mixin-compiled-attr-writeback.t` (9 cases: scalar/array/hash attribute mutation through a
  compiled mixin method, multi `.*` dispatch, nested self-dispatch back to a class method) — all
  re-verified green after the migration, including every attribute-mutation-persistence case. Per
  this box's own "gather evidence before migrating" discipline, no NEW test file was needed here
  (contrast the `^metamethod` site, which had zero coverage and got `t/metamethod-dispatch.t`
  first). Verified with the full local `t/` suite (3193 files, all green), the three named mixin
  test files individually, `cargo build`/`clippy -- -D warnings`/`fmt` clean, the 314-file
  whitelisted `S04`/`S06`/`S09`/`S12`/`S14` roast subset (release — the same 7 non-whitelisted files
  fail identically before/after, matching every prior slice in this box), and
  `scripts/battery-testsuite.sh` (GATE PASSED, 245/271 unchanged). **This closes the
  general-call-dispatch family: all 3 of its named sites are now migrated off the carrier.** The only
  remaining open F6 item is qualified-dispatch's shared helper (a different, larger problem — see
  this box's own note above, "qualified-dispatch's shared helper is a DIFFERENT, bigger problem").

  **Scoping (2026-08-17, read-only, no code) — qualified-dispatch's shared helper.** A full grep of
  every `run_resolved_method_compiled_or_treewalk`/`run_resolved_method_celled` call (excluding the
  two definitions themselves) found **22 call sites across 14 files** — noticeably more than this
  box's earlier "8+" estimate. By file: `methods_qualified.rs` (7 sites — the qualified-dispatch
  module itself: role-concretization dispatch, the by-name qualifier dispatch main path, a
  role-origin fallback, a role-definition fallback, a Mixin-wrapped role-applied branch, a
  Mixin-wrapped by-MRO branch, and the non-Instance/type-object qualified path), `class.rs` (2,
  BUILD-phase attribute-default method calls), `ctor_phase_plan.rs` (2, via the `_celled` core
  directly — BUILD/TWEAK construction-phase running), `methods_instance_ops.rs` (3),
  `methods_signature_shaped.rs` (3, `where`/type-constrained signature-shaped multi dispatch),
  `methods_mixin_dispatch.rs`, `dispatch_proto.rs` (proto `{*}` redispatch, its own
  `proto_dispatch_stack` push/pop wrapped around the call), `methods_call_dispatch.rs` (1, the mixin
  private-method branch adjacent to the mixin fallback site this box just migrated),
  `methods_walk.rs`, `types/role_mixin_class.rs` — one site each.

  **Key finding — this is NOT a "wrong ad-hoc resolver" problem like `run_instance_method`'s was.**
  Unlike that carrier (whose `resolve_method_with_owner_invocant` walk duplicated the E4 sequence
  resolver and could disagree with it — the actual defect F6 exists to eliminate),
  `run_resolved_method_celled` does no resolution of its own at all: every call site above has
  ALREADY resolved `(owner_class, method_def)` through its own specific-purpose walk (a qualifier-
  rooted MRO walk, a role-origin match, a proto candidate, a BUILD-phase attribute default, ...) —
  correctly bypassing the general override-resolution rules on purpose, since a qualified call's
  entire point is to skip them. `run_resolved_method_celled`'s actual job is "run this exact
  already-resolved candidate as compiled bytecode, with on-demand compile-if-needed, delegation-
  forwarder fallback, and `pending_rw_writeback_sources` MERGE (not restore) semantics for a nested
  call's captured-outer writes" — three pieces of real, load-bearing logic that `dispatch_compiled_
  method` (the fast VM-opcode path's own resolution-free core, and the ONLY part of the direct-
  dispatch family that is actually resolution-free) does **not** provide:
  1. **On-demand compile-in-place** (`compile_method_def_in_place_with_dist`) for a candidate reached
     before its owner's registration compile pass, or added at runtime (a role method punned via
     `does`, a custom-HOW method) — `dispatch_compiled_method` requires an already-compiled `cc`
     handed in by its caller; `try_dispatch_compiled_method_direct[_as]`'s own `populate_uncompiled_
     method` step does this, but re-resolves first (the exact thing qualified dispatch must avoid).
  2. **Delegation-forwarder fallback** (`forward_resolved_delegation`) when the resolved candidate is
     a synthesized `handles`-delegation forwarder (`compiled_code: None`, no body to compile) —
     `dispatch_compiled_method` has no equivalent branch at all.
  3. **`pending_rw_writeback_sources` merge-not-restore** around the call, so a sibling BUILD's
     queued captured-outer write survives a nested `.new` (#3620) while the body's own writes also
     propagate — this is normally the VM `CallMethod`/`CallMethodMut` opcode handler's job (drained
     right after `dispatch_compiled_method` returns, per that function's own doc comment), which
     none of these call sites go through since they are all reached from Rust-side dispatch logic,
     not the bytecode loop.

  So a real fix is not "swap the call" (the `try_dispatch_compiled_method_direct*` family is
  categorically wrong here — it would re-resolve and risk picking the general override instead of
  the qualifier-specific candidate, exactly the correctness bug this box's earlier note already
  flagged) and not a rename either (`run_resolved_method_celled` already IS the resolution-free
  helper this family needs — deleting it outright would mean re-inventing the three pieces above
  inside `dispatch_compiled_method` first). The actual convergence work, should it be pursued, is
  extending `dispatch_compiled_method` (or a new sibling next to it, mirroring this session's
  `dispatch_compiled_method_with_attrs_cell` pattern) to cover on-demand-compile +
  delegation-forwarder-fallback + writeback-merge, THEN retargeting these 22 call sites onto it and
  deleting `run_resolved_method_celled`/`run_resolved_method_compiled_or_treewalk`. That is a
  materially larger unit of work than any single-site F6 slice landed so far in this box (it touches
  the VM's own compiled-dispatch core, not just one caller), genuinely deserving its own dedicated
  design pass (up to and including an ADR update if the chosen shape changes `dispatch_compiled_
  method`'s public contract) rather than a same-session extension of this slice. Per this box's own
  "gather evidence before migrating" discipline: reconciliation-shape heterogeneity across the 22
  sites is real and already visible even from a partial read — `methods_qualified.rs`'s 7 sites alone
  split into "commits the returned attrs map back to the receiver" (3 sites), "discards the returned
  map entirely, relying on `self`'s own live cell" (3 sites, all Mixin-invocant branches, matching
  this session's own mixin-fallback finding that a Mixin invocant needs its OWN attrs-cell handling),
  and "commits the map AND does an eager `Proxy` auto-fetch" (1 site) — so no single shared wrapper
  will fit all 22 by pattern-match; each needs the same individual review this box's text already
  calls for. Left un-started; this note is the scoping only.

  **Design conclusion (2026-08-17) — revising the "extend `dispatch_compiled_method`" plan above:
  do NOT pursue it.** Traced `dispatch_compiled_method`'s own attribute source down one more level
  (`call_compiled_method`, `vm_method_dispatch.rs`) before starting the extension the prior note
  proposed, and found the proposed convergence target is unsound for a real subset of the 22 sites —
  not merely more work than expected, but the wrong foundation:

  `dispatch_compiled_method` derives its `attributes: AttrMap` **exclusively from `target.view()`**
  (`ValueView::Instance{attributes,..} => Some(...)`, else empty) — this is what made the mixin
  fallback slice above need the `_with_attrs_cell` sibling instead of a plain reuse. But
  `call_compiled_method` itself (the shared primitive BOTH `dispatch_compiled_method` and
  `run_resolved_method_celled` are built on) takes `attributes: &AttrMap` as an **explicit,
  independent parameter** — confirmed by reading its body: the has-attr-alias scan, the
  `$!x`/attributive-param setup, and (deeper, `reconcile_attrs`) the post-call attribute snapshot
  all key off this parameter, not off `base`'s (the computed self value's) own `ValueView`. This
  parameter is what actually seeds a method body's attribute reads/writes — `base`/`invocant` only
  supplies `self`'s term identity and re-dispatch behavior, a separate concern.

  `methods_qualified.rs:735` (the Mixin role-applied branch) proves this distinction is load-bearing,
  not incidental: it builds `role_attrs` by layering the mixin's own `__mutsu_attr__` entries on top
  of the inner instance's attribute snapshot — a value that exists **only as a plain `AttrMap`**,
  with no backing `Gc<InstanceAttrs>` cell of its own (unlike the general mixin-fallback site this
  session already migrated, where the cell genuinely was `inner`'s real cell). Even THIS session's
  own new `dispatch_compiled_method_with_attrs_cell` helper cannot serve this site — it still requires
  a real `&Gc<InstanceAttrs>`, and there isn't one to point to here. Only `call_compiled_method`'s raw
  `&AttrMap` parameter admits a synthesized, cell-less map at all.

  So `run_resolved_method_celled` is not a duplicate, diverging implementation of what
  `dispatch_compiled_method` already provides (the actual defect that motivated deleting the
  `run_instance_method` ad-hoc-resolver family) — it is a **correctly-scoped, necessary orchestration
  layer built on the SAME shared core** (`call_compiled_method`), adding exactly the cold-path
  concerns the hot VM-opcode path's own caller (`try_compiled_method_or_interpret_inner`) doesn't need
  handled the same way: on-demand compile-in-place, delegation-forwarder fallback,
  `pending_rw_writeback_sources` merge, and (one more piece found while re-reading its tail this
  session — `class_dispatch.rs:621-626` — not previously called out) converting an unhandled
  submethod `Failure` return into an `Err`, since a submethod's own call sites never reach the VM's
  `CallMethod` opcode path directly. `dispatch_compiled_method` is the specialized, hot-path variant
  (auto cell-derivation from `target`, an optional live-cell fast path, eager `Proxy` auto-fetch) —
  not a superset `run_resolved_method_celled` duplicates, but a sibling built for a narrower case
  (target IS the real receiving `Instance` with its own cell) that most calls hit but not all.

  Generalizing `dispatch_compiled_method`/`call_compiled_method`'s return-value identity-preservation
  logic (`vm_method_dispatch.rs:977-996`, itself keyed on `base.view()` being `Instance`) to also
  cover a synthesized/cell-less `attributes` map would mean redesigning the single hottest dispatch
  path in the interpreter for a purely internal-architecture win with no Raku-compatibility or
  roast-count gain — failing this file's own gain/risk framing ("Gain = moving toward the correct
  architecture... Risk = making the codebase worse... A temporary CI/roast failure is NOT a risk" —
  the converse also holds: a change to the hottest path with no compatibility upside is not
  automatically a "gain" just because it deletes a few hundred lines of a DIFFERENT, non-duplicate
  file). This does not fit the pattern that made every other F6 slice in this box a clean win (a
  genuinely duplicate/diverging ad-hoc resolver, replaced with the same modern resolver already used
  elsewhere, at low risk to an already-cold or already-migrated call path).

  **Recommendation: close this F6 sub-item as "investigated, revised" rather than "blocked pending
  design."** `run_resolved_method_celled`/`run_resolved_method_compiled_or_treewalk` should be
  RETAINED as-is — they are sound, necessary machinery, not the kind of technical debt F6 exists to
  remove.

  **Correction (2026-08-17) to this paragraph's own claim about `run_resolved_instance_method`.**
  This paragraph originally asserted `run_resolved_instance_method` (`class_dispatch.rs:289` — the
  OTHER "resolved-path helper" the box's original text bundled together with the two above under
  one phrase) "is called only from the ad-hoc `run_instance_method_celled` walker's own
  found-a-candidate tail, so it is removed for free once that walker's own callers are gone." That
  is wrong: a grep for its call sites (excluding the definition) finds it also called directly from
  `dispatch_proto_call.rs:150` — `call_proto_dispatch`'s proto-method `{*}` redispatch, the
  ADR-0019 E9c-2 hot path (its own doc comment there names it explicitly: "the same resolved-method
  run path ordinary dispatch uses (`run_resolved_instance_method`)"), independent of
  `run_instance_method_celled` entirely. `instance_method_not_found` (`class_dispatch.rs:193`) has
  the identical shape: two independent callers, `run_instance_method_celled`'s not-found tail and
  `call_proto_dispatch`'s own not-found branch (`dispatch_proto_call.rs:163`). So neither function
  is removable "for free" once the ad-hoc walker's carrier callers are migrated away — both are
  independently load-bearing for proto-method `{*}` redispatch, a distinct, unrelated-to-F6 feature.
  They stay retained alongside `run_resolved_method_celled`/`run_resolved_method_compiled_or_treewalk`
  for that reason, not merely pending walker cleanup. **This closes out F6's qualified-dispatch item;
  F6 has no further open code-migration slices.**
- [x] **F7 — Delete obsolete declaration payloads and generic statement-pool entries.** Remove old
  `Register*` compatibility code and assert that migrated sub/class/role declarations retain no
  executable source AST.

  **Scoping (2026-08-17, read-only, no code).** Read literally, this box's own completion criterion
  is already satisfied: `RegisterSub`/`RegisterClass`/`RegisterRole` were consolidated into
  `RegisterDecl(CompiledDeclPlanRef)` in Phase A4, and no sub/class/role declaration reads a raw
  `Stmt` to decide what to register. The remaining work is what C6d-2, D6, and D9 each deferred
  "until the token/rule work lands, then closed together with F7": grammar `token`/`rule`
  declarations are the only `Register*` path left reading a raw `Stmt` end to end (top-level
  `RegisterToken(idx)` and the `ClassBodyOp`/`RoleBodyOp` `TokenRule` arm inside class/role/grammar
  bodies) that is not one of Phase A's/D10's own already-accepted permanent exceptions
  (`AugmentClass`'s one-shot `stmt_pool`-fed walker; `RegisterEnum`/`RegisterSubset`, explicitly
  deferred as non-blocking by Phase A). This is narrower and lower-risk than it sounds — the actual
  registration path (`register_token_decl`/`insert_token_def`) is materially flatter than a
  `SubDecl`'s (no redeclaration check, no attribute pre-scan, `compiled: None` always, since a
  token/rule body is never bytecode-compiled — that stays exactly as ADR-0009 decided) — but it is
  real, unstarted design/implementation work, not a trivial rename. Full survey, the concrete
  `CompiledTokenDeclPlan` shape recommendation (mirroring C8's own `CompiledProtoDeclPlan`
  precedent), and a found-while-scoping `is_my`/`is_our`-dropped-silently observation (spot-checked
  against `raku` and found benign, but worth a proper table before this code is touched) are in
  `todo/deep/adr0019-f7-token-rule-declaration-typed-plan.md`. Not started.

  **Slice 1 — top-level `token`/`rule` declarations (2026-08-17).** Landed the recommended shape:
  `CompiledTokenDeclPlan` (`name`/`params`/`param_defs`/`multi`/`raw_body`, `raw_body` kept opaque
  per ADR-0009 — a token/rule body is never bytecode-compiled) and a new
  `CompiledDeclPlanRef::Token(u32)` variant, mirroring `CompiledProtoDeclPlan`'s own shape. The
  top-level `Stmt::TokenDecl`/`RuleDecl` compile arm now calls `add_token_decl_plan` + emits
  `RegisterDecl(idx)` instead of cloning into `stmt_pool` + emitting the old dedicated
  `RegisterToken(idx)` opcode, which is deleted outright (its own `exec_register_token_op`
  handler replaced by `exec_register_token_decl_op`, reading the typed plan). `is_my`/`is_our`
  are deliberately NOT carried onto the plan — the pre-existing path never read them either (the
  old match arm dropped them via `..`), so the plan preserves exact fidelity rather than inventing
  unread fields; the "Found while scoping" `is_my`/`is_our` question stays open as its own,
  separate, not-yet-verified item. Verified with the full local `t/` suite (3194 files, all
  green), the full local grammar/token/rule-named `t/` subset (68 files) plus every whitelisted
  grammar/regex roast file (`S05-*`/`S12-*` whitelist subset, 190+19 files including
  `integration/advent2013-day18.t`, ADR-0009's own pin test) — all release, all green —
  `cargo test --lib` (835 tests, including `opcode_stays_small`, confirming `OpCode` shrank rather
  than grew), `cargo build`/`clippy -- -D warnings`/`fmt` clean, and a hand-built raku-verified
  table (`proto token`/`multi token :sym<>` variants, `rule` with a `+`-quantified subrule, a
  `my token` lexical-scope leak check) byte-identical to `raku` including the exit code. The
  `ClassBodyOp`/`RoleBodyOp` `TokenRule` carve-out (declarations inside class/role/grammar bodies)
  is a separate, second slice, not bundled here per this box's own "no shared-helper by
  pattern-match" discipline.

  **Slice 2 — class-body `token`/`rule` declarations (2026-08-17).** A class body's own package is
  fixed and known at class-declaration compile time (unlike a role body's, whose composing package
  is not known until composition), so a class-body `token`/`rule` statement can skip the
  registration-time `run_block_raw` OTF recompile the same way slice 1 removed it at the top level.
  `ClassBodyOp` gained a `TokenRule { plan: CompiledTokenDeclPlan }` variant (the plan-building
  logic factored out of `add_token_decl_plan` into a shared `build_token_decl_plan(stmt)` free
  function, reused by `classify_class_body_stmt`); `run_class_body` calls `register_token_decl`
  straight from the plan's fields instead of falling into `ClassBodyOp::Other`'s raw-`Stmt` +
  `run_block_raw` path. The regex body itself (`raw_body`) stays interpreter-executed, unchanged —
  ADR-0009's own execution model. **Role-body `token`/`rule` declarations are deliberately left
  as-is** (still `RoleBodyOp::Deferred`/`DeferredBodyOpKind::TokenRule`, carrying a raw `Stmt`): a
  role's composing package genuinely is not known until composition (the same reason `Plain`
  deferred statements also keep the raw-`Stmt` fallback, per D8-1/D8-2), so there is no compile-time
  package to precompute a plan against — this is a permanent constraint, not a deferred slice 3.
  Verified with `cargo test --lib` (835 tests, plus a new
  `class_declarations_body_plan_types_token_rule_declarations` unit test replacing the now-stale
  "excludes token/rule chunks" one), the full local `t/` suite (3197 files, 29769 tests, all
  green), the full local grammar/token/rule-named `t/` subset (68 files, 462 tests) plus every
  whitelisted grammar/regex roast file (`S05-*`/`S12-*` subset, 191 files, 7573 tests, including
  `integration/advent2013-day18.t`) — all release, all green — `cargo build`/`clippy -- -D
  warnings`/`fmt` clean, and a hand-built raku-verified table (grammar `token`/`rule` bodies, a
  role-declared token composed into two different grammars, a grammar redeclared inside a loop
  body — exercising registration running repeatedly) byte-identical to `raku` including the exit
  code. This closes ADR-0019's F7 box's own scoped remaining work in full.

### Completion gates

- [ ] **G1 — Full compatibility gate.** `make test`, `make roast`, GC stress, JIT stress, WASM, and
  bundled-library suites pass with no new quarantine.
- [ ] **G2 — Architectural guard tests.** Tests fail if a migrated declaration enters
  `stmt_pool`, retains `legacy_body`, dispatch bypasses `MethodEntry`, or introspection reads a hand
  name table.

  **Progress (2026-08-17):** the `stmt_pool` clause already has scattered per-declaration-kind
  coverage, one test per migrated kind, all in `src/compiler/mod.rs`'s `declaration_plan_tests`
  module: `sub_declarations_leave_the_generic_statement_pool` (A2),
  `type_declarations_leave_the_generic_statement_pool` (A3, class/role),
  `nontrivial_proto_declarations_compile_their_dispatch_body` (C8, proto), and the new
  `token_rule_declarations_leave_the_generic_statement_pool` (F7, covering both the top-level and
  class-body registration paths), and now
  `every_migrated_declaration_kind_together_leaves_the_generic_statement_pool` — one program
  declaring `sub`/`proto sub`+`multi sub`/`token`/`rule`/`class` together (plus a class body's own
  nested `method`/`token`/`rule`), checked against the top-level `stmt_pool`, every nested compiled
  function's own `stmt_pool`, and each class body op's `Other`/`ClassSub` raw payload — closing the
  "isolated per-kind tests could miss a kind-vs-kind interaction" gap the per-kind tests alone left
  open. Deliberately excludes role bodies: `RoleBodyOp::Deferred` keeps a raw `Stmt` for ANY
  deferred statement kind by design (a role's composing package is unknown until composition, ADR
  D8-1/D8-2), so it is an accepted carve-out bucket, not a regression surface this sweep should
  police. The `legacy_body`/`dispatch bypasses MethodEntry`/`introspection
  reads a hand name table` clauses still have no dedicated guard tests, though each was verified
  ad hoc at its own closing box (e.g. D6-4/D9-5's `legacy_body` field removal, E4-E7's dispatch
  entry-routing verification, F1/F2's `.^methods`/`.^can` canonical-table cutover) — formalizing
  those as permanent regression tests is
  still open, unscoped work.
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
record of what has landed. Phases A-D and E1, E3-E11, F5, F7 are closed; E2 (open cleanup, no longer
gating), the rest of Phase F (F1-F4, F6), and the completion gates are the remaining open work —
see their entries above for
current status and the linked `todo/deep/adr0019-*.md` design docs for full design and slice
history. Individual accomplishments
are additionally recorded per-PR under `news/2026-08/`.
