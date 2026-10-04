# ADR-11756: The precompilation cache stores a module's compiled bytecode, validated by recorded compile inputs

- **Status**: Proposed (2026-10-04)
- Date: 2026-10-04
- Deciders: tokuhirom, Claude
- Addresses: [#11756](https://github.com/tokuhirom/mutsu/issues/11756)
  (`use Test` recompiles and re-registers the whole module on every load)
- Related: [ADR-0134](0134-begin-time-prologue.md) (module BEGINs re-run on every
  load "until precompiled bytecode exists"), [ADR-0110](0110-typed-resolved-ir-for-statically-typed-routines.md)
  (assumes TRIR chunks serialize with `CompiledCode`), [ADR-0107](0107-compilation-unit-runtime-identity.md)
  (compunit identity), [ADR-0133](0133-no-per-call-ast-compile-at-runtime.md),
  [#11761](https://github.com/tokuhirom/mutsu/issues/11761) (export re-registration)

## 1. Context

`src/precomp.rs` caches a module's **parsed AST** (`Vec<Stmt>`) plus the
parser state a parse would have left behind (`ParseEffects`). Every load then
compiles the whole module again and runs its registration ops.

Every `t/` and roast file starts with `use Test`. After the cleanups in
perf/use-test-load-cost, a release build spends **82.5M instructions** loading
`Test` (`use Test; ok 1;` 91.1M minus an empty script 8.6M, warm cache,
2026-10-04). Callgrind attributes that to:

| inclusive | what |
| ---: | --- |
| ~34M | `Compiler::compile` of the module unit. ~19.6M of it compiles routine bodies (`compile_sub_body_with_deprecation`): every routine in the module, every load |
| ~23M | registration ops (`exec_register_sub_op_in_registry`: `register_sub_decl_with_metadata`, export aliases) |
| ~5.5M | `add_sub_decl_plan` → `compiled_routine_metadata` (body fingerprints) |
| ~3.7M | deserializing the cached AST |
| ~3.2M | the importer's type-name pre-scan of the module source |

Rakudo's precompilation stores the compiled compunit together with its
serialized BEGIN-time state. mutsu stores neither, so it behaves like an
uncached rakudo load, every time. ADR-0134 §1 already records one consequence
(module BEGINs run on every load) and defers it until "precompiled bytecode
exists". ADR-0110 assumes a bytecode cache that does not exist yet.

### What the compiler depends on besides the AST

A survey of `Compiler::compile` for a module mainline (`compile_block_raw`,
`src/runtime/run.rs`) found these inputs that do not come from `stmts`:

- **Fields copied from the interpreter**: `current_package` (forced to
  `GLOBAL` for a module load), `enclosing_package`, `is_routine` /
  `is_mainline`, `current_distribution` (a `Distribution` instance, baked into
  constants by `$?DISTRIBUTION`).
- **Thread-locals**:
  - the ambient unit source file (`unit_source_file`), stamped into every chunk;
  - the parser's `SCOPES` queries (`is_imported_function`,
    `is_user_declared_type`, `is_user_declared_enum_value`).
    `is_imported_function` is **not** part of today's `ParseEffects`;
  - `CURRENT_LANGUAGE_VERSION`;
  - parser state reached through compile-time re-parses (`parse_fragment`,
    heredoc interpolation).
- **Environment `OnceLock`s**: `MUTSU_NO_SHADOW_SLOTS`, `MUTSU_CONST_FOLD`,
  `MUTSU_SLOT_READ_FILTER`.
- **The AST passed to the compiler is not the cached AST**:
  `load_module_inner` reorders the BEGIN prologue and splices in
  `check_undeclared_routines_with_guards`' guards. That check reads interpreter
  state.

It also has **side effects** beyond the returned `CompiledCode`/`CompiledFns`:

- **Process-global latches**: `REFLECTIVE_NAME_ACCESS_SEEN`, `DISPATCHER_SEEN`.
- **Process-local counters whose values are embedded in the bytecode**:
  - `STATE_COUNTER`, which feeds closure state-scope names
    (`Pkg::&<closure>/N`), `__do_decl_init_N`, `__mutsu_xx_target_N`, and
    `PhaserEnd`'s `site_id`;
  - `begin_site_id` / `augment_site_id`, which hash a package name built from
    that counter;
  - `LEXSUB_ALIAS_SERIAL`, which mints `__mutsu_lexsub_*`;
  - `INSTANCE_ID_COUNTER`, through exception objects built as constants.
- **Fresh per-process identity**: `CompiledFns::next_id`.

Nothing reachable from `CompiledCode` derives serde today. The pieces are:
`Symbol`, the AST and `Value` (through `SerValue`) already serialize. Most
`OpCode` payloads are plain data. A handful of fields are runtime caches
(`handler_snapshot`, the `*_sites` caches, `jit`, the `OnceLock` indexes,
`ClassOperandSite`'s `Mutex`) that should be rebuilt rather than stored.
Constants are overwhelmingly literals. The exceptions are exception instances
and the `Distribution` instance.

## 2. Decision

### 2.1 Cache the compile, keep the AST entry

A precomp entry gains an optional **compiled section**. It holds the serialized
`CompiledCode` and `CompiledFns` of the module mainline, exactly as
`load_module_inner`'s compile produced them, next to the AST it already stores.
On a hit, the load skips `Compiler::compile` for the mainline and runs the
deserialized code.

The AST stays in the entry. Registration, the BEGIN prologue and every
analysis that reads `stmts` keep working unchanged, and every load the
compiled section cannot serve falls back to today's path.

### 2.2 Soundness: a compiled section is used only if its recorded inputs still hold

Serving stale bytecode is a silent wrong answer, which is worse than any
slowdown. So validity is not inferred from the source hash alone.

- **Recorded inputs.** Every read the compiler makes that does not come from
  `stmts` goes through one recorder (`CompileInputs`). The recorder logs the
  question and the answer, e.g. `is_imported_function("foo") -> false`,
  `current_package -> GLOBAL`, `language_version -> 6.d`, `env
  MUTSU_CONST_FOLD -> unset`. The log is stored in the entry. A hit re-asks
  every recorded question against the current state, and any different answer
  rejects the compiled section (the AST part is still used). This is
  dependency tracking, not a hand-maintained key. A newly added compiler input
  that bypasses the recorder is the one way to break it, which is what §2.5
  catches.
- **The exact compiler input is hashed.** The key includes a structural hash of
  the `stmts` actually handed to `Compiler::compile`, after prologue
  reordering and guard splicing, and not just the source hash. A different
  guard set therefore cannot reuse the entry.
- **Replayed effects.** The global latches the compile set
  (`REFLECTIVE_NAME_ACCESS_SEEN`, `DISPATCHER_SEEN`) are stored as effects and
  re-applied on a hit, the way `ParseEffects` are replayed today.

### 2.3 No process-local identity in serialized bytecode

A value minted from a process-local counter means nothing in another process,
and it can collide with a value minted freshly in the loading one. Before
anything is cached:

- **Counter-minted names and site ids come from a compile session.** Every
  value the compiler mints today from a process-global counter is minted
  instead as (session, ordinal). The ordinal is a counter local to one
  top-level compile, shared by its sub-compilers. The session is chosen per
  top-level compile. That covers `STATE_COUNTER`'s `Pkg::&<closure>/N`,
  `__do_decl_init_N`, `__mutsu_xx_target_N`, `__mutsu_cas_seen_N` and the
  `site_id`s of `PhaserEnd`; `LEXSUB_ALIAS_SERIAL`'s `__mutsu_lexsub_*`; and,
  through the package name they hash, `begin_site_id` / `augment_site_id`.
  - For a **cacheable compile** (a module mainline), the session is
    **content-addressed**: a hash of the module's source identity (canonical
    path and content hash) and of how many times this process has already
    compiled that same unit. The first compile of a module in any process gets
    the same session, so a cached chunk is valid as is and needs no rewriting
    on load. Loading it claims that occurrence, so a later recompile of the
    same unit in the same process gets the next occurrence and fresh values,
    exactly as today.
  - **Every other compile** (EVAL, OTF, the main program) keeps drawing its
    session from a process-global counter, as today. The two session spaces are
    kept disjoint by one reserved bit.
  - This preserves today's guarantee: each compile mints values no other
    compile in the process shares. Making the values merely
    compunit-relative would not preserve it. A recompile of the same source
    (a block handed to `reduce`/`classify`, an OTF recompile) would then reuse
    its predecessor's `state` scope, which is a behaviour change. It is a
    prerequisite slice that changes no behaviour on its own.
  - The parser's `ANON_STATE_COUNTER` names (`__ANON_STATE_N__`) are already
    embedded in today's cached AST. This slice audits whether a collision
    between a cached and a freshly parsed name is observable, and moves them
    onto the same session scheme if it is.
- **Constants that carry identity are not serialized.** The serializer accepts
  only identity-free constants. A chunk whose constant pool holds an object
  with an instance id (the compiler-built exception objects) or the
  `Distribution` instance is re-materialized on load from a recorded recipe,
  where that is simple. Otherwise the module is marked uncacheable and keeps
  today's path. Uncacheable is always a correct answer.
- **Runtime caches are rebuilt.** `handler_snapshot`, the `*_sites` caches,
  `jit`, the derived `OnceLock` indexes and `ClassOperandSite`'s cache are
  skipped by the serializer and start empty, as they do after a fresh compile.
  `CompiledFns` gets a fresh `next_id` on load.

### 2.4 The serialized form: derived encoding, hand-written boundaries

The format is private to one build: the existing binary-mtime stamp already
discards every entry when mutsu is rebuilt. So it needs no schema evolution, and
the choice of encoder is about speed and maintenance only.

- **Plain data is encoded by derive.** That covers most `OpCode` variants, the
  decl plans and the small spec structs. `OpCode` has hundreds of variants, and
  a hand-written encoder per type would drift every time one is added. A
  derive follows automatically. The encoder does not have to be serde:
  bincode 2's own `Encode`/`Decode` derive is preferred, because its `Decode`
  takes a context argument. The context carries the entry's symbol table
  without a thread-local.
- **The boundaries are hand-written**, because they are where the policy lives:
  - `Symbol`: an index into a per-entry string table, so each distinct name is
    interned once per load, not once per occurrence;
  - `Value` constants: identity-free variants only, per §2.3;
  - runtime caches: skipped, and rebuilt empty;
  - `Arc`-shared nested code: encoded once, shared again on decode.
- **Decision criterion, measured in slice 2.** Today's AST cache decodes
  `Test.rakumod` in ~3M instructions against a 209M parse (serde + bincode). So
  the derived approach is expected to decode the compiled form in a few million
  instructions against the ~34M compile it replaces. If the measurement
  contradicts that, the types that dominate the decode profile get
  hand-written codecs. A wholesale hand-written format is adopted only if
  per-type replacement cannot close the gap.

### 2.5 A verify mode proves the cache transparent

`MUTSU_PRECOMP_VERIFY=1` makes a hit compile anyway and compare the fresh
result with the cached one, serialized to bytes, failing loudly on any
difference. Iteration orders that would make serialization nondeterministic are
sorted. The full `t/` suite and the roast whitelist must pass with verify on
before the compiled section is enabled by default, and the nightly stress
workflow keeps running it afterwards. A recorder bypass, a missed effect or a
leaked process-local id all show up here as a byte difference, not as a
user-visible wrong answer.

### 2.6 Out of scope here (later decisions)

- **Registration.** Phase 1 still runs the registration ops: about 23M of the
  82.5M. Reaching #11756's goal needs a second phase that stores the
  registration *result* and replays it, or makes it cheap (#11761 is part of
  that). That is a separate ADR, because it means serializing registry deltas,
  which is a larger and different contract than serializing code.
- **BEGIN-time state** (ADR-0134 §1's accepted divergence). Running a module's
  BEGINs once at precompilation needs its BEGIN-time state serialized too.
  Phase 1 keeps re-running them on every load, which is what happens today.
- **OTF and EVAL compiles** are not cached.

## 3. Consequences

- **Expected gain (phase 1):** the mainline compile, ~34M of `Test`'s 82.5M,
  is replaced by deserialization of the compiled section. The net gain depends
  on how fast that deserialization is, and it is measured per slice.
  `CompiledCode` deserialization must stay well under the compile it replaces.
  Otherwise the slice is not worth landing, and that is a valid negative
  result to record here.
- **Every module benefits**, not only `Test`. That includes zef's own modules
  and every vendored battery.
- **Cache entries grow** by the serialized bytecode, and
  `CACHE_FORMAT_VERSION` is bumped whenever the serialized shape changes. The
  existing binary-mtime stamp already invalidates every entry on rebuild.
- **New maintenance rule:** a compiler read of state that does not come from
  `stmts` must go through `CompileInputs`. The verify mode is the net that
  enforces it.
- ADR-0110's "TRIR chunks serialize with `CompiledCode`" becomes true once
  `TrChunk` gets the same treatment.

## 4. Alternatives considered

- **Compile routine bodies lazily, on first call.** This also saves the ~19.6M
  of body compiles a test never calls, and it helps EVAL too. But
  `compile_sub_body_with_deprecation` inherits compiler state from the
  enclosing scope (`inherit_enclosing_scopes`, fold context, outer code-var
  names, listop shadows, static class), and all of that would have to be
  captured at declaration. It does not touch the mainline or registration
  costs. It is complementary, not a substitute, and can follow later.
- **Key the cache on the source hash and binary stamp only.** That is what the
  AST cache does, and for an AST it is sound, because the parse depends only on
  the source plus `ParseEffects`. The compile is not like that (§1). The
  missing `is_imported_function` input alone would let a module compiled under
  one importer's imports serve another importer.
- **Snapshot the whole interpreter after loading the module.** That is closer
  to Rakudo's serialization context, but it is far larger than needed for
  phase 1. Phase 2 can grow toward it in steps.

## 5. Implementation plan

1. **Compile sessions for counter-minted names and site ids** (§2.3). No
   caching yet, and no behaviour change. Checked by the existing suites.
2. **Encoding for the compiled shape** (§2.4). `OpCode`, `CompiledCode`,
   `CompiledFns` and the plan types, with runtime caches skipped. Measure
   decode cost against the compile it replaces, plus a round-trip test
   (`compile → serialize → deserialize → serialize` is byte-identical) over
   every `t/` and roast module.
3. **`CompileInputs` recorder and effect replay** (§2.2), still writing nothing
   to disk.
4. **Compiled section in the precomp entry**, behind `MUTSU_PRECOMP_BYTECODE=1`,
   with `MUTSU_PRECOMP_VERIFY=1` (§2.5). Run the whole `t/` + roast with verify
   on. Measure `use Test` and the TAP suite.
5. **Default on**, once 4 is clean. Verify mode moves into the nightly stress
   workflow.

Phase 2 (registration) gets its own ADR after step 5 lands and is measured.

## 6. Implementation status

- **Step 1** landed in [#11768](https://github.com/tokuhirom/mutsu/pull/11768):
  `src/compiler/compile_session.rs`. The content-addressed session is not
  wired yet; every session is still a counter session.
- **Step 2**: `src/precomp_codec/`. It provides bincode 2 derives for plain
  data and hand-written codecs for `CompiledCode`, `CompiledFunction`,
  `CompiledFns`, `TrChunk`, `LexScopeChain` and `ClassOperandSite`.
  `PortableValue` guards the constants, and `StaticStr` stands in for the two
  `&'static str` fields. `MUTSU_PRECOMP_ROUNDTRIP=1` round-trips every compile
  and runs the decoded copy. All of `t/` (6430 files) passes in that mode.
  Measured on `use Test; ok 1;` (profiling build, warm cache), decoding every
  compile of the process costs **6.7M instructions against 30.0M of
  compiling**. Most of the decode is the serde-encoded AST fragments
  (`stmt_pool`, the plans' `ParamDef`s), whose `Symbol`s are interned per
  occurrence. Per §2.4, they are the first candidate for a native codec if
  step 4's net measurement asks for more.
