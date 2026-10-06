# ADR-12026: A precompilation hit loads a module from recorded facts, decodes routine bodies on first use, and keeps registration as executed code

- **Status**: Proposed (2026-10-06)
- Date: 2026-10-06
- Deciders: tokuhirom, Claude
- Addresses: [#12026](https://github.com/tokuhirom/mutsu/issues/12026) (phase 2 of
  [#11756](https://github.com/tokuhirom/mutsu/issues/11756): `use Test` costs 43.6M
  instructions to load on a hit, and the goal is 20.6M or less)
- Builds on: [ADR-11756](11756-compiled-bytecode-precompilation.md). Its §2.6 defers
  registration to this ADR.
- Related: [ADR-0134](0134-begin-time-prologue.md) (BEGIN prologue),
  [ADR-0066](0066-call-dispatch-inline-cache.md) (inline caches key on a
  `CompiledFunction`'s address)

## 1. Context

Phase 1 made a module's mainline compile a cache hit, and that is the default since
#11868. What a hit still costs was measured on main `dfbc8e3a0` (profiling build, warm
cache, callgrind Ir of `use Test; ok 1;` minus an empty script = **43.6M**):

| Part | Ir | Notes |
|---|---:|---|
| Mainline execution | 12.7M | registration is 9.7M of it: 122 `RegisterDecl`, ~80k each |
| Decode of the compiled section | 8.2M | routine bodies (`CompiledFns`) 3.9M; decl plans and metadata 2.8M, mostly serde `ParamDef`s |
| AST read (`load_cached_unit`) | 2.9M | |
| BEGIN-prologue ordering (`order_unit`) | 1.8M | the cached AST is stored before the reorder, so it runs on every load |
| `stable_hash` of the compiled AST (cache key) | ~1.4M | |
| `$=pod` and declarator entries | 1.1M | |
| Other AST scans in `load_module_inner` | ~2-3M | undeclared-call, exported-type (×2), unit-scope names (×2), dynamics |
| Import and export bookkeeping | ~2.5M | |

A read-only inventory of the load path found the following:

- **No runtime structure holds the module's `stmts`.** The Vec is a local of
  `load_module_inner`. The AST fragments the runtime does keep (`stmt_pool`, class-plan
  method bodies, `ParamDef`s, the proto `legacy_body`) already travel inside the
  cached `CompiledCode`.
- **Most AST passes on the load path are pure functions of the AST or the source.**
  This covers:
  - `order_unit`;
  - the undeclared-call scan;
  - `detect_unit_package_name`;
  - the unit-scope, dynamic, our-name and exported-operator lists;
  - `collect_exported_type_names`;
  - `module_has_state_sub`;
  - the dispatcher-mention flag;
  - the pod blocks.

  Their *uses* depend on load-time state (the registry, env, module stacks), but the
  lists themselves do not.
- **The exceptions are:**
  - *The undeclared-routine guards.* The verdict on each unexplained call depends on
    the registry and env, and the guard statements are spliced into the AST before
    the compile. That is why phase 1 keys the entry on the stable hash of the AST
    actually compiled.
  - *Module-level phaser bodies.* These are compiled on the fly.
  - *`capture_module_compiled_fns`.* For a module with a `state`-declaring sub it
    runs a second, uncached full compile.
  - *`$=pod` declarants.* They clone routine bodies.
- **Registration reads only the compiled code** (decl plans plus `CompiledFns`). Every
  write it makes, though, is keyed by or mixes in load-time state:
  - the current package and unit, and `$?FILE`;
  - the module, unit and load stacks, and `suppress_exports`;
  - the existing registry and env contents, and the imported-alias tables;
  - the process counters (`decl_order`, callable ids);
  - user `trait_mod:<is>` handlers, which are arbitrary code.
- **Registration also materializes every routine body.** `adapt_compiled_to_def`
  clones each `CompiledFunction` into its `FunctionDef` and re-runs three precomputes.
  So a lazy decode of `CompiledFns` alone would save nothing.

## 2. Decision

### 2.1 A hit is served from a load-facts record, not from the AST

At cache-write time, the entry gains a `ModuleLoadFacts` record. It holds the result of
every pure AST pass that `load_module_inner` and `run_block_with` perform:

- the unexplained-call scan;
- the unit package name;
- `prologue_len`;
- the name lists (unit-scope, our, dynamic, exported-operator, exported-type,
  our-routine);
- `module_has_state_sub`;
- whether the mainline has phasers;
- the dispatcher-mention flag;
- whether the unit has pod or declarator docs.

The record is computed from the same AST and source that the compile saw, at the moment
the compiled section is written. So it can never disagree with the compiled code.

On a hit whose facts are valid, `load_module_inner` does not read the `.bin` AST, does
not run `order_unit`, and does not run any of the scans. It reads the facts instead.

**The guards move out of the cache key.** Each unexplained call's verdict is a pure
function of the call record and the load-time state. The guard statements are a pure
function of the verdicts. So the entry is keyed on the *verdict vector* (one bit per
recorded call) instead of the stable hash of the compiled AST:

- a hit judges the recorded calls against the current state;
- it compares the verdicts with the recorded ones;
- it uses the compiled section only if they are equal.

This is the same soundness argument as ADR-11756 §2.2: a recorded question is re-asked,
not assumed. The AST content itself is pinned by the source hash and the
content-addressed parse session the entry already records.

**The AST stays available on demand.** A consumer that cannot be served from facts asks
for the AST, which is read once per load exactly as today. The consumers are:

- module-level phaser bodies;
- `$=pod` declarants;
- `capture_module_compiled_fns`;
- verify mode.

These are rare in modules. The `Test` load needs only the pod one, and §2.4 removes it.
Correctness never depends on the facts covering a case. Coverage only decides how often
the AST is read.

### 2.2 A routine body is decoded on first use

`CompiledFns` entries are encoded length-prefixed. The decoded table maps each key to a
lazy slot that holds the raw bytes and a shared handle to the entry's symbol table.

`FunctionDef.compiled` becomes a handle with the same laziness. The first call, or the
first reader that needs the body, materializes the `CompiledFunction` and runs the
`adapt_compiled_to_def` precomputes on it. The result is cached in the slot, so it is
created exactly once per `FunctionDef`, as today.

ADR-0066 inline caches key on the materialized `Arc` address. That address is stable
from materialization on, so nothing about cache validity changes. The ~32 readers of
`.compiled` go through one accessor, and verify mode materializes every slot before it
compares.

### 2.3 Registration stays executed code, and becomes cheap

**Rejected: storing the registry delta of a load and replaying it.** The inventory shows
why. Each registration write depends on load-time state, and user `trait_mod:<is>`
handlers run arbitrary code during it. A replay would have to:

- record and re-validate all of that state;
- re-key the delta on the importer's package and stacks;
- give up the moment a trait handler is present.

That is a second, parallel implementation of registration, which is exactly the kind of
dual mechanism AGENTS.md calls a risk. It would also fall out of date with every change
to `register_sub_decl_with_metadata`.

Instead, the `RegisterDecl` ops keep running, and their per-routine cost is cut to the
writes they must make:

1. **Plan-time precomputes.** Whatever `adapt_compiled_to_def` derives from the plan
   alone is computed once, at compile time, and stored in the compiled code. With
   §2.2 that work disappears from the load path entirely: param local slots, the named
   call plan and the param-name symbols, given the plan's own params, return type and
   flags.
2. **Shared parameter data.** `params` and `param_defs` are shared (`Arc`) between the
   plan, the `FunctionDef` and the `CompiledFunction`. They are not cloned per
   registration.
3. **No clone of the builtin registry on the first write.** The function table is
   layered: a shared, immutable builtin layer and a per-interpreter overlay. Today the
   whole builtin registry is copied on the first write (1.58M).
4. **The in-place `RegisterDecl` of a hoisted sub is O(1)** when the hoisted
   registration already installed the same site fingerprint.

### 2.4 `$=pod` is recorded, not rebuilt from the AST

The pod blocks parsed from the source are stored in the facts record. That is plain
data: the parse of `collect_pod_blocks` before it becomes `Value`s. On a hit, `$=pod` is
built from that data.

Declarator entries come from `ParseEffects.decl_docs` (already cached). The declarant
values that clone routine bodies are created lazily, on first `.WHY` / `$=pod` access
that needs them, or through the AST-on-demand path of §2.1.

### 2.5 The decl-plan codec is native

ADR-11756 §2.4 chose hand-written codecs "by measurement". The measurement is now in:
`CompiledSubDeclPlan` and `CompiledRoutineMetadata` decode at 2.8M, mostly through serde
for `ParamDef`. They get a native bincode codec with the same symbol-table context, so
decoding no longer goes through serde.

## 3. Consequences

- **Expected gains** against the 43.6M, each measured and recorded per step:

  | Step | Expected gain |
  |---|---:|
  | §2.1 | ~8M (AST read, `order_unit`, scans, `stable_hash`) |
  | §2.2 | ~3.5M of the 3.9M body decode, plus most of `adapt_compiled_to_def` |
  | §2.3 | ~4-5M of the 9.7M registration |
  | §2.4 | ~1M |
  | §2.5 | ~1.5M |

  The total, ~18M, would put `use Test` near 25M. The rest of the gap to 20.6M (import
  and export bookkeeping, the remaining registration writes) is measured after these
  land, and a slice that does not pay for itself is recorded here as a negative result.
- **The cache entry grows** by the facts record and by per-routine length prefixes.
  `CACHE_FORMAT_VERSION` is bumped.
- **New maintenance rule.** A new pure AST pass on the module load path must either
  record its result in `ModuleLoadFacts` or call the AST-on-demand accessor. A new
  registration precompute that depends only on the plan belongs at compile time. Verify
  mode is extended to check that the facts recorded at write time equal facts computed
  afresh from the AST.
- **Every module benefits**, not only `Test`.

## 4. Alternatives considered

- **Replay a recorded registry delta** (§2.3). Rejected as a dual mechanism with an
  unbounded validation surface.
- **Snapshot the whole interpreter after `use Test`.** This is a process-image approach
  like Rakudo's serialized setting. It is rejected because it conflicts with
  per-importer state (packages, stacks, tags) and would need a memory-image format.
- **Keep the AST and only cache the scan results.** This saves the scans but not the
  2.9M AST read and the 1.4M hash, and it still needs the guard redesign to drop the
  hash.
- **Lazy-decode `CompiledFns` but keep `FunctionDef.compiled` eager.** It saves nothing,
  because registration materializes every body (§1).

## 5. Implementation plan

Each step is its own PR. Before landing, each step needs:

- the `t/` and roast verify sweeps with `MUTSU_PRECOMP_VERIFY=1` clean;
- the measured `use Test` load cost recorded in §6.

The steps:

0. **Cheap wins with no design.**
   - Read the source once per load (it is read three times today).
   - Run the eligibility scans once (twice today).
   - Compute `collect_exported_type_names`, `collect_unit_package_scope_names` and
     `collect_unit_our_var_names` once each (twice each today).
1. **§2.1 load facts and the verdict-vector key**, with AST on demand. Verify mode
   compares the recorded facts with fresh ones.
2. **§2.4 recorded pod.**
3. **§2.2 lazy routine bodies.**
4. **§2.3 registration**, one sub-step per item: plan-time precomputes, shared params,
   layered builtin registry, O(1) hoisted re-registration.
5. **§2.5 native decl-plan codec.**

## 6. Implementation status

- **Steps 0 and 1** (§2.1). Step 0 measured at ~0.3M on `use Test` (the
  duplicated source reads and scans), so it rode with step 1.
  - `src/runtime/module_load_facts.rs` holds `ModuleLoadFacts` and the lazily
    decoded `ModuleAst`.
  - `precomp::bytecode` makes the entry self-contained: metadata, parse effects,
    facts, AST, then payload.
  - `undeclared_routines::RecordedCalls` is the recorded half of the check. The
    parser's import table and the registry are re-asked on every load.
  - The cache key's `ast_fingerprint` became `guards_fingerprint`.
  - The AST is decoded on demand for declarator docs, a `state` sub, a
    module-level block phaser, a compile the cache cannot serve, and verify
    mode, which also recomputes the facts.
  - `use Test` load: **43.6M → 35.1M** Ir (AST read, `order_unit`, scans and
    `stable_hash` gone from a hit).
  - The verify sweeps over `t/` (6626 files) and the roast whitelist pass.
