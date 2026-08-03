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
tree-walking paths named in ANALYSIS §1.1.

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
- The migration touches broad compatibility-sensitive behavior; stacked PRs keep each ordered layer
  independently testable.
- Native handlers may remain split across source modules for maintainability. Their *registration*
  and lookup entry is singular even when their implementation is not.

## Implementation status

Stage 1 is in progress. `RegisterSub` now indexes a typed `CompiledSubDeclPlan` pool rather than
`CompiledCode::stmt_pool`; all normal, hoisted, nested-`our`, and top-level-method lowering sites use
the plan, and the VM no longer pattern-matches `Stmt::SubDecl`. The plan still carries
`legacy_body` for the existing routine-registry adapter. Replacing that field with the compiled
routine reference is the next declaration slice.

`RegisterClass` and `RegisterRole` now likewise index typed class/role declaration-plan pools for
both source-order declarations and hoisted forward-reference shells. The VM no longer discovers
these declaration kinds by inspecting the generic statement pool. Their `legacy_body` adapters
remain until structural registration and declaration-time expressions become plan operations and
compiled child chunks.

The three declaration entry opcodes have been consolidated into `RegisterDecl`, whose compact
operand selects a tagged `CompiledDeclPlanRef`. Sub/class/role plans retain their typed cold-data
pools, while VM opcode dispatch and declaration-sensitive compiler analysis now share one bytecode
entry.

The dispatch layer now has a canonical `BuiltinMethodEntry` keyed by owner type and method name.
Built-in introspection derives its name list from those entries. The
runtime `Registry` owns those entries in a `(owner, method)` map, seeds them from static data at
construction without invoking native handlers, and serves built-in introspection from that map.
Each built-in entry now carries a static `NativeMethodHandlerId`, and the VM pure-native dispatch
entry admits modeled built-in calls through the registry row before invoking the arity-specific
implementation behind that ID. Remaining compatibility call paths still invoke the arity helpers
directly and must be routed through the shared entry.

`MethodEntry` now carries both an optional built-in descriptor and ordered user candidates, so a
user override and its built-in fallback share one `(owner, method)` row. Class declaration,
rollback, augmentation, role composition, MOP `add_method`, partial registration, and `EVAL`
restoration synchronize user candidates into the table. Dispatch still reads the compatibility
`ClassDef::methods` mirror until compiled-candidate replacement and generation invalidation move
with it in the next stage.

Every built-in seed and user-candidate synchronization advances the registry's monotonic method
generation. Resolver and fast-dispatch entry points compare it with the interpreter's observed
generation and invalidate the resolve, fast, multi, constructor, and private-method caches as one
unit when it changes. This removes correctness dependence on registration sites remembering every
individual cache before the read side switches to `MethodEntry`.

User-method presence checks and overload lookup now read every candidate from
`MethodEntry.user_candidates`, including uncompiled EVAL/MOP candidates. The on-demand compiler
updates `ClassDef::methods` and immediately republishes the compiled candidates through the table,
so dispatch no longer falls back to the class method mirror when reading overloads.
