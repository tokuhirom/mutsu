# mutsu codebase analysis

This document is an **architecture and soundness review** of the mutsu codebase —
"how much of the design is in order, and what debt remains" — not a bug tracker.

**Verified against HEAD on 2026-09-12.** Method: subsystem-level close reading, each claim
re-checked on the live tree, every finding carrying a `file:line` or a file reference. This
document describes the codebase as it stands; earlier editions live in git history and are
not summarized here.

Where the moving parts are tracked:

| what | where |
|---|---|
| open findings | GitHub issues on `tokuhirom/mutsu`, labelled `todo:ticket` / `todo:deep` / `todo:perf` — [docs/issue-workflow.md](docs/issue-workflow.md) |
| which finding to pick up next | the `tier:*` labels on the issues themselves — `tier:S` first, then `tier:B`, `tier:N`, with `tier:icebox` out of the queue |
| architectural decisions | [docs/adr/](docs/adr/) — 95 ADRs |
| completed work | [news/](news/) — one file per accomplishment |
| roast failure analysis | [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) |
| ecosystem compatibility | the nightly parity sweep (ADR-0085), [docs/ecosystem-parity.md](docs/ecosystem-parity.md) |

Findings are cited here **by GitHub issue number**. The in-repo `todo/` directory this
document used to cite was migrated to GitHub issues on 2026-09-08 and deleted; a file path
stopped resolving the moment a finding was fixed and `git mv`d into `news/` (108 of the 122
`todo/` paths cited from `src/` were already dangling at migration time), whereas an issue
number keeps resolving after the issue closes.
[docs/todo-issue-map.md](docs/todo-issue-map.md) is the frozen path→issue map for the 57
files that moved, and citations of the old paths are left as they are (§6 says why).

---

## 0. Summary

mutsu is a Rust implementation of a minimal Raku-compatible interpreter. The assessment:

- **The execution stack is complete and has no known provenance-UB hole.** One bytecode VM
  (no tree-walking interpreter); cycle-collecting GC default-on (ADR-0003); 8-byte NaN-boxed
  `Value` (ADR-0005); Cranelift JIT default-on (ADR-0004). Every `Gc<T>` payload lives in an
  `UnsafeCell` inside `GcBox` (`gc/gc_ptr.rs:166`), so aliased container writes derive their
  `&mut` with valid interior-mutable provenance (ADR-0013, closed with a required Miri gate).
- **roast is no longer the productive compatibility axis.** The whitelist stands at
  **1437 / 1465 (98.1%)** and is at its ceiling. The strict oracles now are (a) the vendored
  upstream test suites of the bundled batteries and (b) the ecosystem parity sweep — ADR-0085
  made per-distribution test-suite parity against rakudo a committed, nightly-measured KPI,
  currently ≈41% of distributions, ≈53% of test files, ≈63% of assertions.
- **The batteries layer is the product, and its exception list is nearly empty.** `modules/`
  holds **40 vendored upstream dists** run verbatim, plus `vendor/zef`. The policy — grow the
  interpreter until the real module runs (rung 2), never reimplement it natively (rung 3) —
  now has only **two** standing native providers: `NativeCall` (measured non-vendorable,
  [#7560](https://github.com/tokuhirom/mutsu/issues/7560)) and the native `JSON::Fast` provider
  — a last-resort provider for a module mutsu does not ship, consulted only after the
  resolution ladder comes up empty. The module-name-keyed *interception* it used to be was the
  exception [ADR-0096](docs/adr/0096-batteries-adoption-policy.md) §E2 scheduled for retirement,
  and it was retired on 2026-09-12 ([#8183](https://github.com/tokuhirom/mutsu/issues/8183)),
  taking `JSON::Tiny` out of the mechanism entirely: a measured performance gap is a reason to
  optimize, not to substitute a semantically divergent implementation under the module's own
  name (§1.8). The native `Test` provider was deleted outright on 2026-09-10
  (~3,300 lines); a bare `use Test` loads rakudo's own `Test.rakumod`.
- **The active architectural thread is the call and closure path**, and it is nearly closed:
  a per-callsite inline cache (ADR-0066), locals and the five per-call bookkeeping stacks as
  windows into shared stacks rather than moved `Vec`s (ADR-0077/0078), built-in dynamics
  evicted from capture material (ADR-0086), and closure capture as a chained fallback tier
  instead of a per-call merged copy (ADR-0092), with kept-set narrowing rejected on
  measurement (ADR-0094). Only ADR-0084 ("the frame `Env` is not the program's symbol table")
  remains design-only.
- **There is no known open soundness hole.** The last one — cross-thread aliased container
  writes — was closed on 2026-09-08: ADR-0068's steps 1-3 are all implemented, all five
  lane-decline reasons are classified, every route is measured, and the read-side guard's cost
  was measured afterwards (§2.2).
- **Performance is a surplus, not a problem**, and is therefore not used as a ranking
  criterion below.
- **Repository hygiene is the worst-trending axis by a wide margin.** `src/` is ~577k lines
  across 376 files over 500 lines and **138 over 1000**; `opcode.rs` is 9,819 lines and
  `runtime/mod.rs` 4,802. `.clone()` sits at 11,476, the
  `unwrap`/`expect`/`panic!`/`unreachable!` family at 2,440, and `#[allow(` at 243 — all three
  rise at every measurement, and the second is in direct tension with PLAN §8.3's "mutsu must
  never Rust-panic on any input".
- **One load-bearing decision is still unwritten**: the batteries adoption policy exists only
  as prose in `BATTERIES.md`/`CLAUDE.md`, not as an ADR (§8).

Nothing found is of the "the basic design is broken" kind. The debt is concentrated in file
size, panic surface, and one substitution mechanism the batteries policy should not have
allowed.

---

## 1. Architecture

### 1.1 Declaration registration and dispatch entries

User-code bodies (subs, methods, blocks) execute exclusively as bytecode. Declarations
compile to immutable typed plans (`RegisterDecl`); user and native methods share one
registry-owned `MethodEntry` write side and one generation invalidation boundary; and a single
TypeId/MRO resolver serves every dispatch read entry, with generation-checked O(1) cache hits
(`runtime/resolution_sequence.rs:327` `resolve_sequence`, `:273` `resolve_via_sequence_cache`).
That resolver now has its own module rather than living inside the dispatch file — ADR-0019 is
Accepted/Implemented and closed.

The class/role registration walker is not a tree-walk over raw statements:
`runtime/registration_class_decl.rs` is 430 lines and the body walk
(`registration_class_body*.rs`, 6 files, ~2,000 lines) reads a compile-time
`body_plan: Vec<ClassBodyOp>`. `declare_drive_how_protocol` (`runtime/metamodel.rs:377`,
invoked from `vm/vm_typedecl_ops.rs:583`) runs the user-HOW MOP protocol *after* native
registration, reading the finished registry.

Deliberately non-gating residue:

- **The exact-handler-ID catalog** (a static type×method row for every native entry, replacing
  the arity-cascade fallback) is open cleanup;
  `native_call_unmodeled` is a monitoring signal, not a precondition
  ([#7540](https://github.com/tokuhirom/mutsu/issues/7540)).
- **Per-native-method introspection fidelity** — `.package` on multi dispatchers, an exact
  rather than synthesized `.signature` — is a reactive per-case slice, not an upfront sweep
  (§4).
- **The module-sub on-the-fly compile gate** (`def_is_otf_compilable_module_single`,
  `vm/vm_call_func_ops.rs:1909`) still excludes `state`, sigilless `\x` params,
  `is encoded(...)` and `start`, each with a documented mechanism-level blocker (PLAN §3).

### 1.2 Closure capture — a chained tier, not a merged copy

`compute_upvalues` (`opcode.rs:7739`) promotes proven immutable scalar reads to indexed
`GetUpvalue`; escaping mutable lexicals use shared `ContainerRef` cells addressed by creator
slot, so a capture always tracks later mutation rather than snapshotting it.

The *lookup* side was reworked by ADR-0092: a closure's captured environment is no longer
merged key-by-key into the callee frame's overlay at every call. It is a **fallback tier**
consulted after the caller chain — `overlay → chain → GLOBAL_BASE → fallback`
(`env.rs:1398` `capture_tier()`, `:1721` `get_sym`, `:1771` `get_sym_with_fallback`) — which
removes a per-call merge loop from `call_compiled_closure_in_unit`
(`vm/vm_closure_dispatch.rs`). ADR-0086 moved the built-in dynamics into a per-interpreter
never-copied base tier so they stop riding along as capture material, and ADR-0094 recorded
the measurement that narrowing the kept set further does *not* pay, so it is deliberately not
done. Reflective and dynamic names still use the explicit env boundary.

### 1.3 Lexical slots and call frames

Shadow slots are default-on, whole-`locals` per-block clones are gone, and `needs_env_sync` is
a per-slot `Vec<bool>` computed by `compute_needs_env_sync` (`opcode.rs:5937`): a slot is
marked only if some op reads or writes that name by name, plus the closure-free-var fold.
`EnvConsumerSlots` records exact slots for `ForLoop`, `BlockScope`, `BlockLocalScope`,
`MakeGather` and `WheneverScope`; block restore is slot-authoritative.

Underneath that, the frame representation itself changed: locals are a **window into one
contiguous `locals_stack`** addressed by a per-frame base rather than a per-call `Vec`
borrowed from a pool (ADR-0077), and the five per-call bookkeeping stacks
(`block_declared_vars`, `loop_local_vars` and siblings) became base/truncate windows over
shared vectors instead of moved-and-dropped `Vec`s (ADR-0078). Both are measured wins on the
call path (≈3-6% retired instructions on `fib(22)`).

The open end of this thread is ADR-0084: the frame `Env` is still asked to behave like a
symbol table in places where a compiled slot table should serve. It is Proposed, design
complete, unimplemented — and it is the last structural piece of the call-path campaign.

### 1.4 Optimizer and opcode set

Constant folding, constant-pool dedup, `constant` inlining with constant-condition DCE,
declaration-marker fusion and `SetSourceLine` removal all landed. The representation guards
hold: `OpCode` ≤ 48 bytes (`opcode.rs:2784`), `Value` ≤ 8 bytes (`value/mod.rs:2755`), and the
`CapNode` leaf is size-guarded too (`runtime/regex_types.rs:672`).

Everything remaining here is gated on ADR-0006's measurement protocol: the surviving
administrative ops (`SetVarDynamic`, `CheckReadOnly`), inline `Option<String>` payloads moving
to constant-pool `Option<u32>`, `Jump(i32)` carrying an absolute index, and histogram-driven
consolidation of syntax-shaped specialized ops. The "opcode count ≠ time" lesson still
governs: do not consolidate on aesthetics.

### 1.5 JIT

Tier A translates hot opcode chunks into helper-call sequences; Tier B inlines NaN-box
tag-dispatched Int/Num arithmetic (`vm/vm_jit.rs`, `vm_jit_tier_b*.rs`). ADR-0004 is closed and
the JIT is the default configuration.

The structural limit still matters for roadmap purposes: **the JIT bails at the call
boundary**, so a loop that calls a sub runs the interpreter call path. No amount of JIT
coverage subsumes §1.3 — which is exactly why the ADR-0066/0077/0078/0092 cluster, not more
JIT tiers, is where the call-path work went.

### 1.6 Parser and slangs

Hand-written scannerless recursive descent; precedence handling is textbook-clean
(`parser/expr/precedence.rs`) and `parser/memo.rs` gives packrat-style backtracking relief.
There is still **no true slang stack**: regex bodies are scanned as raw text at parse time
(`parser/primary/regex/scan.rs`) and structurally parsed at runtime; Pod is skipped by the
parser and rebuilt from raw source at runtime.

Three narrower, working mechanisms sit where a slang stack would be, and all three read
declarative facts out of user code rather than executing Rakudo-internal token bodies:

1. A **unit-scoped declarator registry** (`parser/stmt/simple/registry.rs`, 310 lines) fed by a
   module's `EXPORTHOW::DECLARE` block, consulted by both the bare and scope-prefixed
   declaration forms. It extends the *declarator* table, not the grammar.
2. **Slang activation** (ADR-0026): a compile-time `use` effect that recognizes specific
   grammar-mixin overrides and maps them onto hand-implemented parser modes. This is what
   allowed `Slangify`/`Slang-Tuxic` to be bundled verbatim, and Text::CSV downstream of them.
3. **Slang package declarators** (ADR-0091): a slang that *adds* a `package_declarator:sym<…>`
   candidate has its keyword, package kind and metaclass read out of the candidate
   declaratively and routed through the same `EXPORTHOW::DECLARE` machinery. Driven by
   Test::Async ([#8005](https://github.com/tokuhirom/mutsu/issues/8005)).

Both ADRs explicitly reaffirm the refusal to execute token bodies, so the verdict is unchanged
in kind: user-defined grammar/token/rule slang switching remains future work.

### 1.7 RakuAST model layer

`src/rakuast/` (6 files, ~8,250 lines) converts the internal `Stmt`/`Expr` tree to a RakuAST
node tree (`Q[…].AST`) and lowers it back through the **same compiler** (`EVAL($tree)`), so
there is still no second execution engine. Remaining gaps — read-direction representation
gaps where the parser desugars before conversion, construction of advanced parameter forms, a
blocked lowering list, and Phase 6 macros — are inventoried in
[#7564](https://github.com/tokuhirom/mutsu/issues/7564).

**This layer has acquired a downstream consumer.** ADR-0088 makes RakuAST and execution share
one source-level regex tree, and its static-tree, RakuAST, execution-lowering and provenance
slices have landed; dynamic contents and the full migration remain. Until then RakuAST work
was demand-free — that is no longer true for the regex boundary, and the roadmap below reflects
it.

### 1.8 The batteries layer

`modules/` holds **40 vendored upstream dists** run verbatim (Base64, CBOR-Simple, Cro-Core,
Cro-HTTP, Cro-TLS, Crypt-Random, DBIish, DateTime-Parse, Digest, Digest-HMAC, Encode,
File-Directory-Tree, File-Temp, HTTP-HPACK, HTTP-Status, HTTP-UserAgent, IO-Path-ChildSecure,
IO-Socket-Async-SSL, IO-Socket-SSL, JSON-JWT, JSON-Tiny, Log-Async, Log-Timeline, MIME-Base64,
NativeHelpers-Blob, NativeLibs, OO-Monitors, OpenSSL, Rakudo-Core, Slang-Tuxic, Slangify,
Template-Mustache, Terminal-ANSI, Test-Util-ServerPort, Text-CSV, TinyFloats, URI, UUID, XML,
YAMLish), plus `vendor/zef`. Module resolution has a documented precedence chain (`use lib` →
`-I` → `MUTSULIB` → the `mzef` site repo → bundled batteries), and a release-time gate runs
each battery's **upstream** test suite (`scripts/battery-testsuite.sh`, `batteries.lock`).

Architecturally this is a policy with teeth (BATTERIES.md): **a module is grown into by fixing
the interpreter (rung 2), never reimplemented natively (rung 3)**. The consequences are visible
in the code, and the exception list has shrunk to two entries:

- `NativeCall` — measured non-vendorable (it needs `use QAST:from<NQP>`, MoarVM dispatch
  programs, and 61 missing `nqp::` ops), [#7560](https://github.com/tokuhirom/mutsu/issues/7560).
  The op-count half of that measurement moves as the `nqp::` op layer grows (below), but the
  verdict does not depend on it: `use QAST:from<NQP>` and the MoarVM dispatch-program surface
  are structural, and the record's own reopening condition requires all three to fall together.
  This is a justified rung-3 use, recorded as such.
- **The native `JSON::Fast` provider** (`runtime/json.rs`, 759 lines, plus
  `vm/vm_native_json.rs`) — a last-resort provider for the one JSON module mutsu does not
  ship. The module-name-keyed *interception* this entry used to describe was the exception
  [ADR-0096](docs/adr/0096-batteries-adoption-policy.md) §D4/E2 scheduled for retirement, and it
  was retired on 2026-09-12 ([#8183](https://github.com/tokuhirom/mutsu/issues/8183)); what is
  left is narrower on every axis:

  - `JSON::Tiny` is gone from it entirely. It is a vendored battery, so `use JSON::Tiny`
    resolves through the ordinary ladder and runs the module's own Raku source — `to-json([1,2])`
    answers `[ 1, 2 ]`, the module's spelling, where the interception answered a pretty-printed
    block.
  - `JSON::Fast` is vendored too as of 2026-09-13 (#8226), and its last-resort native provider
    is **deleted**: the name resolves through the ladder like any other module. The "~50 missing
    `nqp::` ops" once recorded here was never measured — 42 of its 51 ops already worked. The
    only JSON mutsu still answers from Rust is `Rakudo::Internals::JSON`, a core class that no
    `use` gates.
  - `json_tiny_exception_style()` — the "best-effort guess" that read the *set* of loaded
    module names to pick `from-json`'s exception type — is deleted. Each module owns its own
    error again: `JSON::Tiny` throws its own `X::JSON::Tiny::Invalid` from its own source, and
    the native provider throws JSON::Fast's plain `X::AdHoc`, whatever else is loaded.

  The measurement that had been cited as making this permanent has also moved: the real grammar
  parsed 200 META-shaped documents in >600s when the split was recorded, and does it in **12.6s**
  today (raku: 0.84s, so mutsu's grammar engine is ~15x off rakudo rather than unusable). That
  remaining 15x is the honest bill, and `Rakudo::Internals::JSON` — a *core* Rakudo class, which
  is what zef's metadata path actually calls — is unaffected by any of this, so the sequencing
  worry that kept the mechanism unexamined did not apply.

  **The line the project holds** is between an **optimization** and a **substitution**: a fast
  path selected transparently and semantically indistinguishable from the code it replaces is
  legitimate (that is what the JIT does to bytecode); one that changes which exception type a
  program sees, depending on which module names were `use`d, is a different implementation
  wearing the module's name. Being slow was not a licence for the second kind, and a
  performance measurement is not a justification for a substitution.

`Test` left that list entirely on 2026-09-10: the native TAP provider, its `tap_state`
bookkeeping, the native subtest machinery, `Stmt::Subtest`/`OpCode::SubtestScope`, the
`MUTSU_REAL_TEST` gate and both comparison-sweep scripts were **deleted** (~3,300 lines), with
0 regressions across `t/` and the roast whitelist. A small parse-time `TEST_EXPORTS` list
survives for a measured startup-latency reason (it keeps `use Test` at 7ms instead of 93ms) and is self-validated
against the vendored module by `test_exports_match_the_vendored_module`.

**Why this belongs in an architecture review**: the vendored suites are the strictest
correctness oracle the project has, and ADR-0085 turned "how compatible are we, really" into a
committed number — per-distribution test-suite parity against rakudo, measured nightly over a
corpus of ~1,600 distributions, currently ≈41% dist / ≈53% file / ≈63% assertion. That metric,
not roast, is now what should choose interpreter work. The native-provider exception list is a
first-class piece of the architecture and should shrink monotonically or be justified in
writing — the discipline [ADR-0096](docs/adr/0096-batteries-adoption-policy.md) §D4 now states.

### 1.9 Metaobject protocol

`runtime/metamodel.rs` (455 lines) plus the registration path implement enough MOP for a real
ecosystem module to install its own declarator and metaobject: `EXPORTHOW::DECLARE::<keyword>`
registration, `Metamodel::ClassHOW` subclassing with fully-qualified
`self.Metamodel::ClassHOW::<meth>` dispatch and `callsame` base candidates, user
`new_type`/`add_method`/`compose`, a user `BUILDALL`/`POPULATE` hook at construction, and a
user `clone` reaching the native attribute-copying clone. `OO::Monitors` runs verbatim on this,
and ADR-0091 (§1.6) extends the same protocol to slang-registered package declarators.

The structural observation: the user protocol is driven from inside a plan-driven,
mostly-compiled registration path whose `body_plan` walk is still imperative Rust rather than
bytecode, so MOP breadth couples to that walker's shape. What remains unbuilt for a module like
Test::Async is the QAST and dispatch-program layer *above* the `nqp::` op set (which does now
exist, §1.8) — a different and much larger claim than "HOW subclassing is unbuilt".

### 1.10 Representation campaigns

- **ADR-0016 (span-based captures, lazy `Match`) is complete.** Absolute positions, the
  `CapNode`/`RegexCaptures` split, a shared `MatchTarget` with span reads, the one-list-per-axis
  collapse, and a lazy `ValueRepr::Match(Gc<MatchNode>)`. Residue is deliberate and small
  (`CodeBlockContext`'s text snapshot, eager `Match` construction in the reduce/failed-replay
  paths). **One standing constraint the whole codebase inherits**: a `view()`-based "is it an
  X?" probe materializes a lazy `Match`, so variant probes on paths a `Match` can reach must be
  tag probes. `MUTSU_VM_STATS=1` reports `match_materializations`, so an accidental forcing path
  is observable rather than prose-only.
- **ADR-0015 (native-backed container storage) is complete through P3b.** CStruct bodies,
  native-backed `Buf`/`Blob` and `CArray[T]`, and numeric `array[T]` storage synchronized with
  the native payload behind the `ArrayData::items` chokepoint. Only optional P3c
  (reference-element `CArray`) remains, and it should start when a real NativeCall consumer
  needs it — not before. ADR-0090 has since added `HAS`-embedded CStruct members on the same
  foundation.

### 1.11 Tooling surface

A language server ships as part of the interpreter (ADR-0065), deliberately scoped to the
protocol surface an **AI agent** actually consumes rather than to full IDE parity; phases S0
through S5a have landed, with `references` (S5b) next. It is called out here because it is a
new consumer of the parser and symbol data with no roast or battery coverage behind it —
whatever it reads from the compiler is a de facto API.

---

## 2. Correctness and soundness

### 2.1 In-process aliased writes — closed and gated

`GcBox` stores its payload as `value: UnsafeCell<T>` (`gc/gc_ptr.rs:166`) and `Gc::as_ptr`
projects through it, so `gc_contents_mut` (`gc/gc_ptr.rs:794`) derives its `&mut` from a
pointer carrying interior-mutable provenance while shared `&` reads are live. The required
`miri` CI job runs both primitive GC/container tests and interpreter-level soundness shapes;
`src/gc/borrow_shapes.rs` pins the handle, raw-pointer and shared-read orderings that callers
actually use. ADR-0013 is closed.

ADR-0095 hardens the same area one layer up: a mutable native instance method now **publishes
its store before it wakes another thread**, closing a visibility window and a lost-update in
`call_native_instance_method_mut_in_place`'s read-modify-write over a shared attribute cell.
That is follow-on work built on the closed primitive, not a reopening of it.

### 2.2 Cross-thread aliased container writes — closed

ADR-0068 established that a cross-thread aliased container write needs a synchronized store
rather than a name-keyed lane, and **all three of its steps are implemented** (closed
2026-09-08). An element store reaching its container through a shared `ContainerRef` cell takes
a cell-keyed stripe lock (`value/container_lock.rs`), and so do the read chokepoints
(`Value::with_deref` / `into_deref`) — the dominant race was writer-versus-reader, so guarding
only writes left failures standing. All five reasons the name-keyed lane declines are
classified, every route is measured with an oracle-classified probe and a stress acceptance,
and the one unexplained historical SIGSEGV was shown not to belong to this class. Pins live in
`t/concurrent-*.t`.

Two results from that campaign are worth carrying forward, because both contradict the obvious
reasoning:

- **Count the funnels, not the call sites.** `gc_contents_mut` has 167 call sites, and citing
  that number as exposure is exactly the mistake the ADR warns against: the class has **three**
  funnels — the named element store, the attribute-rooted element store, and the mutating
  method — and every route tried arrived at one of them. Probe a suspected new route rather
  than reasoning from its shape.
- **The shared thing is the cell, not the node.** Twenty threads writing one celled array reach
  thirteen distinct `Gc<ArrayData>` addresses, because `Gc::make_mut` copies an aliased node, so
  a node-keyed lock excludes nothing.

What remained afterwards was a performance question, not a correctness one, and it has been
measured: the read-side guard is free when no VM mutator thread ever spawns, costs +5-7% on the
shape nobody predicted (a program that spawns a worker early and then does heavy
*single-threaded* work through a bound container, since the gate is sticky), and is a **net win
of 12-13% on genuinely concurrent programs**, because a stripe is cheaper than several cores
contending on the cell's own `Mutex<Value>` and the inner node's refcount atomics. No change
was made.

### 2.3 `RuntimeError` as a control channel

`RuntimeError` still carries `return`/`last`/`next`/`take`/`emit` through `Result::Err`. The
size problem is gone (control bools folded into `enum Control`, cold routing fields boxed,
`result_large_err` allows 0). Channel separation remains unstarted and low priority. Worth
noting: **there is no `size_of` guard test for `RuntimeError`** — only `Value`, `OpCode` and
the `CapNode` leaf are pinned — so a size regression here would be silent.

### 2.4 Process-level robustness

- **Deep recursion** — the interpreter runs on a 256 MB-stack thread (`main.rs:149`) and the
  pure-recursion integration tests pass. Pathologically deep recursion can still overflow; a
  larger fixed stack is a blunt instrument, not heap frames.
- **Supply detached-worker panics are swallowed** — QUIT propagation is unimplemented
  (`PLAN.md` §5, unchecked; no propagation path in `src/runtime/native_supply_methods.rs`).
  This is the most concrete unfixed correctness gap in the concurrency area.
- **Thread pool** — ADR-0020's shared worker pool is landed. `start`, one-shot `cue`,
  `cue(:every)`, supply emitters, promise-waiter dispatch and hyper/race batch workers all go
  through `worker_pool::submit`/`submit_joinable`, which fixed a 52-thread/+16.4 GB
  `cue(:every)` blowup (down to 4 threads / 761 MB). A small number of raw `spawn_user_thread`
  sites remain outside the pool, including `runtime/slang_activation.rs`.
- **Channel-backed Supply** now broadcasts to every tap (ADR-0074), and scheduled delivery
  belongs to the tapped Supply (ADR-0028/0043) — the deferred-tap and quit-ownership questions
  that used to sit here are decided.

### 2.5 How names resolve now

The env-writeback correctness cluster is resolved as one mechanism change: locals are
synchronized by slot (ADR-0018) and escaping captures share cells instead of being copied
through a name-keyed env. A reader tracing "how does a name resolve at runtime" needs ADR-0018
for the slot side and ADR-0092 (§1.2) for the capture side; the process-global name-keyed side
tables that used to shadow both are being retired one at a time — `var_type_constraints`, an
unscoped process-global `HashMap`, was deleted under ADR-0042.

---

## 3. Duplicate implementations

### 3.1 Statement/expression dual compilation — resolved

`for`, `if`/`elsif` and the bare `{ … }` block each have **one** lowering now, shared by both
source positions (`compiler/control_for.rs`, `compiler/control_if.rs`, and the bare-block
unification under ADR-0076). `ForLoopSpec` is constructed in exactly one place;
`compiler/helpers_do_expr.rs` is down to 284 lines. The value-position divergences the merge
exposed — dropped loop phasers, a mirrored `.reverse` rw writeback, missing junction
autothreading, missing `for_loop_param_syms` immunity, unmarked read-only multi-params, missing
`&?BLOCK`, and a value-position `if`'s bare-regex condition — are fixed and pinned by
`t/control-construct-value-position.t`.

**Deliberate residue**: the two block forms still emit different opcodes (`BlockScope` vs
`DoBlockExpr` — 22 files vs 6 reference them). ADR-0076 shares the lowering and defers the
opcode merge; that merge is an optimizer question, gated on the same measurement protocol as
§1.4, not a correctness one.

### 3.2 Sub declaration registered twice

`SubDecl` both registers an AST body (`RegisterSub`) and compiles the body
(`compile_sub_body`). This is the last unmerged duplicate of the declaration campaign and
collapses when the registration path finishes compiling.

### 3.3 Method dispatch — one resolver, many doors

There is one candidate-resolution mechanism: `resolve_sequence`
(`runtime/resolution_sequence.rs:327`) builds the shape-independent ordered candidate universe
(user candidates, accessor arbitration, native catalog rows, proto slot) from the same
TypeId/MRO chain calls use, and `resolve_via_sequence_cache` (`:273`, keyed
`(TypeId, Symbol, CallShape)`) makes a hit generation-checked O(1). ADR-0066's per-callsite
inline cache sits in front of it.

What is *not* unified is the set of entry points into that resolver:
`call_method_with_values` (`runtime/methods_call_dispatch.rs:164`),
`dispatch_method_by_name_{1,2,3}` (`runtime/methods_dispatch_match{,2,3}.rs`),
`run_instance_method_at`/`run_instance_method_celled` (`runtime/class_dispatch.rs:85,125`), and
the arity-keyed native entries in `builtins/`. They all resolve through the same
candidate-sequence logic rather than independently re-walking the MRO, so this is a surface
issue, not a second dispatch mechanism. Same-name string matches stay scattered — `"elems"`
appears in **40 files** — and `runtime/methods_call_dispatch.rs` is now **4,695 lines**, the
fifth-largest file in the tree.

---

## 4. Hardcode and drift risks

No test-specific hardcoded outputs found. Two derivation shortcuts remain:

1. **Native-method introspection data is hand-maintained one layer down.** User-declared types
   derive `.^methods`/`.^can`/method MRO from the canonical
   `Registry::method_entries[(owner, name)].user_candidates` table, and
   `.^lookup`/`.^find_method` return the same `Method` `Instance` shape those readers build —
   that half is sound. But the *candidate-name universe for natives* lives in a literal table,
   `builtins/native_method_row_table.rs` (1,549 lines, ~340 rows), with
   `builtins/builtin_type_methods.rs` (407 lines) reading names off its `INTROSPECTABLE`-flagged
   rows. Structural tests plus `t/can-methods-drift.t` guard it. Per-method fidelity gaps —
   native `.package` on multi dispatchers, a synthesized rather than exact `.signature` —
   remain open as a reactive per-case slice. The growth rate matters because §1.9 lets user code
   introspect through this same surface.
2. **Module names hardcoded into dispatch** — *resolved 2026-09-12*
   ([#8183](https://github.com/tokuhirom/mutsu/issues/8183)). The JSON fast path used to key off
   the literal strings `"JSON::Fast"`/`"JSON::Tiny"` in the parser's export list, in `use`-time
   gating, and at two call sites, with one decision (which exception type `from-json` throws)
   reading the *set* of loaded module names rather than anything about the call. `JSON::Tiny`
   left the mechanism entirely, the load-order-sensitive exception guess is deleted, and the
   dispatch sites no longer beat a resolved def — the one remaining name is a last-resort
   provider consulted only when the ladder resolves nothing (§1.8).
3. **Parser grammar relaxations for roast** (minor): `is List` type-ish traits, the
   Test::Assuming colonpair, and the `throws-like` trailing-`)` special form.

---

## 5. Value model, performance, robustness

- **State outside the value**: Failure handled/pending registries are `thread_local!` and lose
  registration across thread boundaries; pending DESTROY queues likewise. Seq consumed/cached/
  lazy state is O(n) linear scans of `OnceLock<Mutex<Vec<Weak>>>` statics — fragile and slow.
- **Env**: COW `Arc<FxHashMap<Symbol,Value>>` with a scoped parent-overlay chain capped at
  `MAX_OVERLAY_DEPTH = 16` (`env.rs:639`), now with a capture tier behind it (§1.2).
- **`.clone()` ≈ 11,476**: each is an 8-byte NaN-box copy plus a refcount for container tags,
  so unit cost is low, but the count rises at every measurement and is a code-shape signal.
- **`unwrap`/`expect`/`panic!`/`unreachable!` ≈ 2,440** and **`#[allow(` 243**, both rising.
  PLAN §8.3's "mutsu must never Rust-panic on any input" is a goal the trend is moving away
  from, and no mechanism currently pushes back on it.
- Allocation-failure aborts on user-sized allocations remain guarded via `try_reserve`.

---

## 6. Repository hygiene

The 500-line rule is not being followed, and the gap is widening:

- **376 files over 500 lines, 138 over 1000**, in ~577k lines of `src/`.
- Largest files: `opcode.rs` 9,819 · `vm/vm_exec_dispatch.rs` 5,917 ·
  `vm/vm_var_assign_index_named.rs` 5,094 · `runtime/mod.rs` 4,802 ·
  `runtime/methods_call_dispatch.rs` 4,695 · `compiler/stmt.rs` 4,563 ·
  `runtime/regex_parse_core.rs` 4,343 · `compiler/mod.rs` 4,272 · `ast.rs` 3,730 ·
  `vm/vm_call_method_mut_ops.rs` 3,665.
- Giant dispatch matches (`opcode.rs`, `vm_exec_dispatch.rs`) are intentional exceptions. The
  rest are not, and `runtime/mod.rs` — a grab-bag, not a dispatch match — has grown at every
  single review this document has run.

Splitting does happen when a campaign forces a file open: the registration walker was split
from ~2,500 lines to 430, and `resolve_sequence` moved into its own module. That is the pattern
that works. A standalone line-moving campaign is not proposed; what is proposed (§7) is making
the split a completion gate on whatever campaign next opens the file.

**Citations of the deleted `todo/` directory survive — and are deliberately left alone.**
There are 279 `todo/…` path references in `src/`, 257 across 56 files in `docs/adr/`, and 61 in
`PLAN.md` and top-level `docs/`. They are not dangling in the sense the migration was about:
`docs/todo-issue-map.md` is frozen, so each resolves in one lookup, and the rot the migration
ended was prospective — a path breaking when its finding is fixed and moved into `news/`.
Rewriting them in bulk would edit 56 decision documents to change how they cite their own
history, churn `git blame` across hundreds of files, and conflict with everything in flight,
for one saved indirection. The exception is a **live** pointer that reads as current state —
ADR-0073's Status line names a remaining case as "tracked in
`todo/deep/ordered-alternation-eager-candidate-enumeration.md`" — which is ride-along cleanup
for whoever next edits that ADR, not a campaign. A smaller, older instance of the same shape:
comment references to the retired `MUTSU_SHADOW_SLOTS` opt-in gate.

---

## 7. Recommended roadmap

Ordering rule, stated so it can be argued with:

1. **Design prerequisites before data sweeps.** A costly-to-reverse policy that exists only as
   prose is the highest-leverage thing to fix, because everything downstream cites it.
2. **Soundness outranks breadth**, but only where the task is actionable — a narrowed route
   with a known shape qualifies; a blind stress loop does not.
3. **Hygiene is a completion gate on the subsystem being replaced**, never a standalone
   campaign.
4. **Feature breadth with no downstream consumer ranks last** — and "downstream consumer" now
   has a number attached to it (ADR-0085's parity sweep).

| # | Item | Kind | Why here |
|---|------|------|----------|
| 1 | **Follow the parity frontier** (§1.8) — the adoption-policy ADR half of this row is **done**: [ADR-0096](docs/adr/0096-batteries-adoption-policy.md), [#8184](https://github.com/tokuhirom/mutsu/issues/8184) | policy / product architecture | The project's main goal rests on "vendor upstream verbatim; grow mutsu; no new native providers," which is now a decision document rather than prose in `BATTERIES.md`/`CLAUDE.md`: the rejected alternative, the two named exceptions, the retirement precedent, and the corrected `nqp::` framing all live in ADR-0096. What remains of this row is the follow-on work, and with ADR-0085 shipping a nightly parity number it can be chosen by measurement instead of by anecdote. |
| ~~1b~~ | ~~**Retire the JSON `use`-time interception**~~ (§1.8, §4, [#8183](https://github.com/tokuhirom/mutsu/issues/8183)) | design cleanup | **Done 2026-09-12** (ADR-0096 §E2 closed). `JSON::Tiny` left the mechanism entirely (it is a vendored battery and now loads like any other module); the exception-shape guess keyed on the set of loaded module names is deleted; the dispatch sites no longer beat a resolved def; and the surviving `JSON::Fast` name is a last-resort provider consulted only when the ladder resolves nothing, so `-I`/`MUTSULIB`/site-repo copies win. The remaining bill — mutsu's grammar engine is ~15x rakudo on the real module, down from the >1000x that had made this look permanent — stands on its own. |
| 2 | **Supply panic propagation ([#8185](https://github.com/tokuhirom/mutsu/issues/8185)), and a mechanism against the panic-surface trend ([#8186](https://github.com/tokuhirom/mutsu/issues/8186))** (§2.4, §5) | correctness debt | Detached-worker panics are silently swallowed instead of reaching QUIT. Separately, the panic-family count rises at every measurement against an explicit "never Rust-panic" goal — a goal with no enforcement mechanism is a wish, so either add one (a budget test, a lint) or amend the goal. |
| 3 | **Finish the call-path thread: ADR-0084** (§1.3, [#7817](https://github.com/tokuhirom/mutsu/issues/7817)) | design cleanup | ADR-0066/0077/0078/0086/0092/0094 all landed; ADR-0084 ("the frame `Env` is not the program's symbol table") is the one piece still design-only, and it is what the others' remaining overhead funnels into. |
| 4 | **Pay hygiene debt through the work above** (§6) | completion discipline | 138 files over 1000 lines, `opcode.rs` approaching 10k, `runtime/mod.rs` at 4,802 and growing at every review. Split when a campaign opens the file and the ownership boundary is visible; a standalone line-moving campaign is not proposed, and neither is a bulk rewrite of the surviving `todo/` citations (§6 says why). This row is therefore a discipline, not a queue item — deliberately the one row with no issue behind it. |
| 5 | **RakuAST, now demand-driven** (§1.7, [#7564](https://github.com/tokuhirom/mutsu/issues/7564)) | demand-driven feature | ADR-0088's shared regex tree gives this layer its first real execution-side consumer, so the remaining slices can be chosen by what that migration needs rather than by inventory completeness. Phase 6 macros still have no consumer. |

Explicitly **not** ranked as current architecture work: the completed ADR-0013/0015-P3b/0016/
0018/0019/0020/0029/0058/0085 campaigns; optional ADR-0015 P3c; ADR-0016's deliberately
deferred eager replay carriers; ADR-0076's deferred opcode merge; roast whitelist chasing; and
perf levers with no goal-item consumer. They become candidates when a real downstream
dependency changes that premise.

---

## 8. ADR ledger review

95 ADRs, reviewed against the tree. **The decisions themselves hold up — none was found to be
wrong**, and the one supersession in the set (ADR-0082 → ADR-0083, on whether a collecting
`for` gathers containers or snapshots) is cleanly recorded on both sides.

### The systemic problem is status drift, in both directions

An ADR's recorded status drifts from what shipped, because implementation progress is reported
in `news/` and PLAN.md and not folded back into the ADR that owns the decision. That defeats the
ADR's purpose for anyone who reads the ADR first. This is **recurrent, not a one-off**: the
index was swept clean at an earlier review and has re-accumulated stale rows within weeks.

Current drift, as of this review:

**`docs/adr/README.md`'s index was behind the ADR bodies on 14 rows** — 0058, 0059, 0065, 0066,
0068, 0070, 0071, 0073, 0080, 0085, 0086, 0088, 0089, 0092. Every one was *more conservative*
than the ADR itself (typically "Proposed" against a body that says Accepted/implemented);
ADR-0085 was the worst, indexed as "P1 implemented; P2-P5 open" when all five phases had landed
and its driving issue closed. These rows were re-synced to each ADR's own Status line in the
same pass as this review.

**Two ADR bodies are behind reality**, and are left for their owning campaigns to update rather
than edited here:

- **ADR-0070** (native methods declare accepted nameds) reads `Proposed` with slices 1-3
  implemented.
- **ADR-0081** (compunit-scoped module import aliases) reads `Proposed`, but the decision
  shipped on 2026-09-11.

### The decisions, grouped by thread

Reading 95 ADRs as a list is not useful; they fall into a small number of campaigns:

| thread | ADRs | state |
|---|---|---|
| Substrate: GC, NaN-boxing, JIT, interior mutability | 0001, 0003, 0004, 0005, 0013, 0095 | closed, with ADR-0095 as active hardening |
| Cross-thread container writes | 0068 (+ ADR-0001 layer 3c) | closed 2026-09-08, cost measured after |
| Compiled declarations and unified dispatch | 0019, 0029, 0047, 0051, 0066, 0070, 0071, 0093 | mechanism closed; catalog/fidelity residue |
| Lexical slots, capture, call frames | 0018, 0023, 0024, 0025, 0027, 0055, 0061, 0077, 0078, 0084, 0086, 0092, 0094 | landed except ADR-0084 |
| Containers, itemization, container identity | 0015, 0036, 0039, 0040, 0042, 0045, 0049, 0059, 0064, 0067, 0079, 0080, 0083, 0089, 0090 | mostly landed; 0079/0040/0045 design-only |
| Seq and deferral | 0034, 0038, 0058 | closed |
| Regex, LTM, captures | 0007, 0016, 0022, 0046, 0073, 0088 | landed; ADR-0088's migration in progress |
| Concurrency: pool, supplies, taps | 0020, 0028, 0031, 0043, 0062, 0074 | landed |
| Parser, slangs, declarators, modules | 0026, 0044, 0050, 0052, 0053, 0054, 0081, 0087, 0091 | mechanisms landed; several design-only |
| RakuAST | 0011, 0088 | phases 1-5 landed; regex boundary in progress |
| Product and process | 0002, 0075, 0085, 0096 | ADR-0085 is the new KPI; 0096 records the batteries policy |

### The missing ADR — written 2026-09-12

**The batteries adoption policy** was the one load-bearing decision this review found recorded
nowhere but in `BATTERIES.md` and `CLAUDE.md` prose. It is now
[ADR-0096](docs/adr/0096-batteries-adoption-policy.md) ([#8184](https://github.com/tokuhirom/mutsu/issues/8184)),
carrying the five things the prose could not: the rejected alternative (native reimplementation,
and what it costs — the compatibility signal, silent divergence, a maintenance tail, the
resolution ladder); the exception list as an auditable two-entry list with the discipline that
keeps an entry on it (§D4); the retirement precedent set by `Pod::To::Text` and by deleting the
native `Test` provider (§D6); the accurate form of the `nqp::` measurement (§D5 — the op set is
a *threshold* function, not "do not build an op layer", which is a claim 111 shipped ops have
outgrown); and the clause the JSON carve-out was allowed to skip (§D3):

> **A performance measurement justifies an optimization, never a substitution.** Rung 3 is
> banned because a module that only looks like the upstream one is a private dialect, and that
> argument does not weaken when the divergence is bought with speed rather than convenience.
> What speed justifies is a transparent, semantics-preserving change to the real module's own
> code path.

The ADR recorded the JSON interception as an exception **scheduled for retirement**
([#8183](https://github.com/tokuhirom/mutsu/issues/8183)), not as policy, and
`docs/batteries/json-tiny.md` withdrew its own "permanent policy" claim accordingly — a
battery's selection record does not get to make a policy decision. That retirement landed the
same day: `JSON::Tiny` left the mechanism entirely, the exception-shape guess is deleted, and
what survives under the `JSON::Fast` name is a last-resort provider the resolution ladder
outranks (§1.8). §D3 is therefore the first clause the ADR has already been used to settle
rather than merely to state.

---

*Based on static close reading plus live verification against HEAD, 2026-09-12.*
