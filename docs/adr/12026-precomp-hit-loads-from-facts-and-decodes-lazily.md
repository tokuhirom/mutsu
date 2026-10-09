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

**The seam, refined by an inventory of the readers (2026-10-06).**

Laziness does not go inside `CompiledFunction`. About 157 sites read `x.code.`, and
`adapt_compiled_to_def`, the precomputes and `make_sub_for_routine` all touch it. The
lazy unit is a whole body, behind an eager head:

- **Codec.** Each `CompiledFns` entry is encoded as `key, head, body length, body`.
  - The *head* carries what registration and fingerprint probes read: fingerprint,
    package, source file (including `code.source_file`, which `source_file_sym`
    falls back to), `code.source_line`, params and flags.
  - The *body* carries `code`, `trir`, the derived parameter vectors and the nested
    `compiled_fns`.
  - Today an entry has no length prefix and `code` is encoded first, so skipping a
    body means decoding it.
  - A nested body is also stored in the enclosing table (`import_compiled_functions`
    inserts the same `Arc` into both). The codec should encode it once and refer to
    it by key.
- **Slot.** A table value holds the head, the raw body bytes (an `Arc<[u8]>` range)
  and the symbol table as `Arc<[Symbol]>`. It does not use `Rc`, because tables cross
  threads (`vm_hyper_race_parallel`). The body sits in a `OnceLock`.
  - A deferred decode must reinstall `DECODE_SYMBOLS` around itself, because serde
    `ParamDef`s and `ParamCode` read that thread-local.
  - Fingerprint probes read the head: `vm_call_resolve`, the light, named and fast
    call caches, and `TrLink::current_in`. Call sites materialize the body.
- **`FunctionDef.compiled`.** The per-module table is dropped after the load, so a
  body survives only through `FunctionDef.compiled` (and `MethodDef.compiled_code`).
  The handle there must be lazy too.
  - Its first use runs `adapt_compiled_to_def` and `stamp_source_file`.
  - Both are deferred together. Today `stamp_source_file` makes every nested body
    unique on every registration, and `adapt` deep-clones each body before overwriting
    the fields it just cloned (0.44M of its 1.06M on `use Test`).
- **Registration needs three new eager facts** to stay off the body:
  - the nested-export plan indices, which today come from a scan of every installed
    sub's `code.sub_decl_plans` during a module load
    (`register_nested_exported_subs`);
  - `code.source_file`, for `lexsub_plan_owner`;
  - the precompute inputs, unless the precomputes move to materialization time.
- **Class and role methods.** These take `cf.code` into `MethodDef.compiled_code` at
  class registration. They either get the same handle, or they materialize at that
  boundary. The latter is acceptable, since few classes are declared per module.
- **Readers that need every body.** Verify mode, roundtrip mode,
  `inherit_frame_lexical_routines` (only for a split mainline) and the disassembly
  dump force every slot before they run. JIT has no warm-up pass, and
  `clone_for_thread` copies no table.

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
- **Step 4, item 3** (§2.3). This is not a layered builtin registry but its
  cheaper equivalent:
  - `runtime::registry_cow_table::CowTable` shares the large registry tables
    copy-on-write (classes, method entries, role/class relations), and each
    class definition as well;
  - a role declaration no longer removes names it never registered, which
    would copy the table.

  `use Test` load: **−1.2M** Ir. The price is +0.3M at startup, because the
  367 builtin class definitions are each allocated behind a reference count
  once per process.
- **Step 2** (§2.4). Recording the Pod `Value`s turned out unnecessary. The load
  facts hold the byte ranges of the source that Pod entries were read from
  (`pod_ranges`), and a hit runs the same entry scanner over just those ranges.
  A heredoc body inside a range, or a Pod error, records no ranges and the
  whole source is scanned as before. Verify mode recomputes the ranges.
  `use Test` load: **33.09M → 32.09M** Ir (−1.0M, same-session paired A/B).
- **Step 3** (§2.2). A routine body is decoded on first use. `src/compiled_lazy.rs` holds
  the two handles:
  - `LazyFn` is one slot of a `CompiledFns` table. The codec writes each entry as
    `key, nested-export flag, body bytes`, and a decoded table keeps the bytes
    (with the entry's symbol table as an `Arc<[Symbol]>`) until `get` is called.
    `precomp_codec::decode_body` reinstalls the thread-local symbol table around the
    deferred decode. The map no longer derefs to its `HashMap`: `get` decodes,
    `get_lazy` does not, and `iter`/`values` decode.
  - `RoutineBody` is `FunctionDef.compiled`. Registration stores the slot and an
    `AdaptInputs` snapshot of the def's signature; the first `FunctionDef::compiled_fn()`
    runs the former `adapt_compiled_to_def` (clone, signature fields, source-file stamp,
    three precomputes) once.
  - The nested-export scan of `register_nested_exported_subs` is the one registration
    read of a body that was not avoidable, so its answer (`has_nested_export_plans`) is
    recorded beside the bytes. Reading it needs no decode.
  - A body that fails to decode panics with a message naming the cache directory: the
    entry was already accepted as a whole, so only damage to the file after that can
    cause it.
  - A `use Test; ok 1;` run decodes 2 of its 62 routine bodies. Debug-build load
    (callgrind Ir of the script minus an empty one, paired in one session):
    **282.7M → 245.5M** (−13%).
  - Not done here: the head/body split for fingerprint probes (a probe decodes the
    body today), sharing `params`/`param_defs` instead of the one `AdaptInputs` clone
    (§2.3 item 2), and the nested `compiled_fns` of a body, which stamps its nested
    routines (decoding them) on the body's first use.
