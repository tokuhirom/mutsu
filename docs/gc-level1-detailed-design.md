# GC Level 1 Detailed Design Notes

Status: Draft  
Related: [ADR-0001](adr/0001-gc-strategy-and-phasing.md), [PLAN.md](../PLAN.md), [ANALYSIS.md](../ANALYSIS.md)

This document is a set of detailed notes for turning ADR-0001's "level 1 = cycle collector on Arc" into an implementable design.
It does not change the ADR itself; it concretizes what was previously undecided: the **activation mechanism / root boundary / interface with Track B**.

## 1. Decisions fixed up front

### 1.1 The initial implementation is **synchronous, cooperative cycle collection**

The first collector **does not use a background thread**.

- collect runs only at the VM's **explicit safepoints**
- the mutator side goes only as far as "candidate registration"
- trial-deletion / reclaim is performed by **the currently executing thread**

Rationale:

- in current mutsu, `stack` / `locals` / `upvalues` / `call_frames.saved_*` are held directly by the `Interpreter`
- introducing a background collector makes root-snapshot consistency, lock ordering, and coherence with `clone_for_thread` drastically harder all at once
- the ADR's main point is "avoid moving, avoid implementation explosion", so starting with a concurrent collector would run counter to that

Conclusion:

- ship **Level 1a = synchronous collector** first
- backgrounding is split off as a follow-up optimization, Level 1b

### 1.2 Safepoints are restricted to "re-entry boundaries"

collect is permitted only at points that hold no long-lived borrows like `arc_contents_mut` and no locks.

Initial safepoint candidates:

- backward edges of the bytecode dispatch loop
- call / return / invoke boundaries
- join / merge boundaries of `await` / `start` / `hyper` / `race`
- entry/exit of lazy force / gather drain / map-grep nested-run

Conversely, do NOT collect:

- inside mutation helpers while container internals are borrowed
- during write-through of `ContainerRef` / array/hash slots
- while holding locks such as `shared_vars` / registry / attrs

This is the same problem as Track B's "re-entry-safe chokepoint design", so design them as one.

## 2. Root model

### 2.1 No Rust stack scanning

Level 1 is a cycle collector, not a tracing GC, so **raw `Value`s on the Rust stack are not scan targets**.

Instead:

- the collector follows only **edges between GC-managed objects**
- roots are enumerated from **the persistent registers / saved frames / shared tables owned by the `Interpreter`**

In other words, "a value that momentarily sits in a Rust local the collector doesn't know about" is not a problem. Since the collector is non-moving, no address updates are needed;
refcounting guarantees ordinary liveness, and the cycle collector's sole role is "cutting isolated cycles".

### 2.2 Initial root set

The roots traversed in one collect are fixed to the following.

- `Interpreter.stack`
- `Interpreter.locals`
- `Interpreter.upvalues`
- `Interpreter.env`
- `Interpreter.call_frames[*].saved_env`
- `Interpreter.call_frames[*].saved_locals`
- `Interpreter.call_frames[*].saved_upvalues`
- `Interpreter.block_stack`
- `Interpreter.topic_save_stack`
- `Interpreter.last_topic_value`
- `Interpreter.element_source` / `container_ref_var` / any bind-related temporary state that holds `Value`s
- `shared_vars`
- `closure_env_overrides`
- pending queues that hold `Value`s, such as pending destroy / pending writeback / lazy-force / gather-resume
- among the process-global async/supply registries, those that hold `Value` / `SharedPromise` / `SharedChannel`

Excluded:

- anything holding only `CompiledCode` / opcodes / function metadata and no `Value`
- string sets such as `readonly_vars`
- Rust temporary locals

### 2.3 Define the completion criteria of A' weakly

The ADR's A' is groundwork to make root consolidation easier, but the entry condition for Level 1a should not be made too strict.

Entry conditions:

- roots can be enumerated from `stack` / `locals` / `upvalues` / `saved_*`
- the `Value`s remaining in `env` / `shared_vars` / pending queues can be explicitly enumerated

May remain incomplete:

- complete removal of the env path
- unifying all lexicals onto upvalue indices

Rationale:

- Level 1 does not move, so "complete root relocation information" is unnecessary
- however, root-enumeration code scattered across the `Interpreter` must be avoided, so before starting on the collector,
  consolidate `visit_roots(&mut impl RootVisitor)` into a single place

## 3. Managed objects

### 3.1 Do not GC-ify all of `Value`

`Value` stays as the current plain enum. Making everything `Gc<Value>` is a **level-2** idea and out of scope for Level 1.

What Level 1 puts under GC management is only "heap nodes that can form cycles".

Initial targets:

- `Array`
- `Hash`
- `Set`
- `Bag`
- `Mix`
- `Sub`
- `Instance`
- `ContainerRef`
- `Pair` / `ValuePair`

Targets included in the initial scope:

- `Promise`
- `Channel`
- `LazyList`

Rationale:

- the ADR's problem statement includes the Promise/Supply mutual references
- excluding these would produce a state where "we added a GC yet async cycles still leak", diverging from the design goal

However, **"included in the initial scope" and "implemented simultaneously in the first wave" must be kept distinct**.

- `Promise` / `Channel` are the heart of async cycles, and their internal state is relatively self-contained
- `LazyList` is necessary, but it holds `env` / `cache` / coroutine state / lazy pipe / closure sequence and has a wide trace surface

Therefore the implementation waves are fixed as:

- **first wave**: `Array` / `Hash` / `ContainerRef` / `Promise` / `Channel`
- **second wave**: `Sub` / `Instance`
- **third wave**: `LazyList` (and, if needed, the rest of the supply graph)

In short, **`LazyList` is not removed from Level 1's scope, but it is left out of the first migration wave**.

Conversely, out of scope:

- `Int` / `Num` / `Rat` / `Complex` / `Bool`
- `Str`
- `Nil`
- the `Range` family
- metadata-only type objects

### 3.2 Representation policy

The basic form for Level 1 is:

- `Value::Array(Arc<ArrayData>, ...)` → `Value::Array(Gc<ArrayData>, ...)`
- `Value::Hash(Arc<HashData>)` → `Value::Hash(Gc<HashData>)`
- `Value::Sub(Arc<SubData>)` → `Value::Sub(Gc<SubData>)`
- for `Value::Instance { attributes: Arc<InstanceAttrs>, ... }`,
  consider separately whether the `Instance` body or the `attributes` cell becomes the GC-managed object

Key points:

- **the outer `Value` does not move**
- **inner nodes do not move either**
- most existing APIs get by with just replacing `Arc<T>` with `Gc<T>`

### 3.3 `ContainerRef` moves to a GC cell rather than staying `Arc<Mutex<Value>>`

Given the integration with Track B, leaving only `ContainerRef` as `Arc<Mutex<Value>>` would be half-baked.

Initial proposal:

- replace `ContainerRef(Arc<Mutex<Value>>)` with `ContainerRef(Gc<CellValue>)`
- `CellValue` must be able to hold typed-scalar constraints
- cross-thread cases that need a lock are confined inside `CellValue`

With this:

- scalar bind cells
- array/hash element leaf cells
- captured outer lexical cells

all ride on the same GC management model.

### 3.4 The Supply graph includes not just the "Supplier value" but the process-global registries

The Supply subsystem is not closed within the attributes of `Value::Instance("Supplier")`.
The actual live graph is spread across process-global registries.

Supply-side registries that should be included in the initial scope:

- `supplier_state_map`
  - `emitted: Vec<Value>`
  - `quit_reason: Option<Value>`
  - `pending_promises: Vec<SharedPromise>`
- `supplier_subscriptions_map`
  - tap callback `Value`
  - done/quit/whenever-quit/close callback `Value`s
  - `channel_sink: SharedChannel`
  - `Value`s held by unique/classify/produce/start/batch/zip state
- `promise_combinator_map`
  - edges from combinator promise → source promises
- `supply_taps_map`
  - `Value`s for replay/deferred taps

Treatment:

- these are regarded as **root containers, not GC-managed nodes**
- there is no need to make the registries themselves `Gc<T>`
- it suffices for the visitor to enumerate the `Value` / `SharedPromise` / `SharedChannel` inside them at collect time

Rationale:

- many Supply cycles are formed by callback closures or pending promises remaining in a global registry
- if these are not included in the root visitor, we end up with "Promise/Channel are managed, yet cycles via Supply remain"

Conversely, things NOT immediately targeted in the first wave:

- registries that are primarily OS resources, such as socket/listener/udp
- the `mpsc::Receiver<SupplyEvent>` itself
- auxiliary state that holds no `Value`s, such as collected text buffers

However, when any of these directly hold `Value` callbacks or `SharedPromise`s, add them to the root visitor.

## 4. Interface with Track B

### 4.1 Do not "cell-ify all elements"

Even within Track B, avoid making every array/hash element a cell at all times.

Policy:

- ordinary elements are plain `Value`
- only leaves that come to need alias / bind / captured-write / cross-thread sharing become cells
- structural mutation of arrays/hashes goes through a **single chokepoint**

This preserves the direction of the existing `assign_element_slot` / `hash_insert_through` / `array_slot_ref` / `hash_slot_ref`.

### 4.2 Use the mutation chokepoints as the collector's stand-in for write barriers

Level 1a is not a generational GC, so a full-blown write barrier is not needed.
However, cycle-candidate registration is performed from the mutation chokepoints.

Candidate chokepoints:

- array element writes
- hash insert/replace
- instance attr writes
- container cell stores
- closure env capture / rebinding
- promise/channel wiring
- lazy-pipe / coroutine / closure-sequence state updates

Rules:

- scalar-only writes do not register candidates
- push to the candidate buffer only when the node may have a GC-managed node as a child

## 5. Collector internals

### 5.1 Initial algorithm

Following the ADR, adopt Bacon-Rajan-style trial deletion, but start with a minimal configuration.

Each GC node holds:

- the equivalent of a strong refcount
- color / state
- a buffered flag
- the equivalent of a vtable for tracing children

Each node implements `trace(&mut Visitor)` and enumerates **only its GC-managed children**.

### 5.2 Collect flow

1. mutation chokepoints push nodes onto the candidate buffer
2. at a safepoint, if the budget is exceeded, a collect starts
3. rather than marking nodes reachable from roots,
   trial deletion is applied to the candidate subgraph
4. nodes with remaining external references are colored back to black
5. only cycles are reclaimed

Conservative is fine at first:

- candidate duplicates are allowed
- the buffer can be a `Vec<GcId>`
- correctness takes priority over shortening pause times

**2026-07-11 refinement (implemented)**: the candidate buffer holds **`Weak` handles**
(`ErasedWeakGc`) to nodes. If it held strong `Arc` clones, a buffered node that dies by
refcount would keep the entire allocation (including its child `Value` tree) alive until
the next collect, and workloads that generate lots of acyclic garbage (`Match` trees from
grammar parses, etc.) would balloon RSS severalfold (`docs/grammar-parse-gc-churn.md`).
With weak holding:

- refcount death frees on the spot, same as `MUTSU_GC=off` (the dead sweep all but disappears)
- true cyclic garbage keeps itself alive, so the upgrade at drain time always succeeds and
  the soundness of trial deletion is unchanged
- dead entries whose upgrade fails are simply skipped (the header is gone along with the node,
  so it is not touched — not touching it is itself a condition of correctness)

## 6. `clone_for_thread` and cross-thread

### 6.1 The initial implementation assumes "live shared nodes exist in shared_vars"

The current `clone_for_thread` is a mixed snapshot-plus-shared model, and this is the most dangerous part.

Level 1a rule:

- when a GC-managed node is shared cross-thread, the node itself may be shared
- however, a collect does **not start until every interpreter that could reference that node has entered a safepoint**

For simplicity, in the initial implementation:

- the global collector state holds "registered interpreters"
- the side initiating a collect raises a stop request to all interpreters
- each interpreter cooperatively stops at a safepoint and enumerates its own roots

That is, **stop-the-world, but cooperative**.

This handles cross-thread cycles correctly even without a background collector.

### 6.2 Global collect is mandatory because async objects and supply registries are in the initial scope

If `Promise` / `Channel` / the supply graph are in scope, a thread-local collector is not enough.
Lean into **process-global collector + per-interpreter root enumeration** from the start.

The supply graph here includes not only the `Supplier` value but the process-global registries of §3.4.

## 7. Auxiliary APIs to build first

Preparing the following APIs before implementation keeps the design from scattering.

- `Interpreter::visit_roots(visitor)`
- `Value::visit_gc_children(visitor)`
- `ArrayData::visit_gc_children(visitor)`
- `HashData::visit_gc_children(visitor)`
- `SubData::visit_gc_children(visitor)`
- `InstanceAttrs::visit_gc_children(visitor)`

Prohibited:

- fallbacks where the collector directly pokes at `self.interpreter.*`
- duplicated per-call-site implementations of root-enumeration logic

## 8. Measurement

Adopt the ADR's `MUTSU_VM_STATS` proposal as-is. The counters wanted first are:

- `gc_candidate_pushes`
- `gc_candidate_dedup_hits`
- `gc_collections`
- `gc_reclaimed_nodes`
- `gc_reclaimed_cycles`
- `gc_pause_ns_total`
- `gc_pause_ns_max`
- `gc_root_nodes_scanned`

Success criteria:

- `gc_candidate_pushes = 0` on the `fib` / int-heavy benchmarks
- on array/hash-heavy benchmarks, pauses are observable and nothing hangs
- reclaimed counts increase on the pin test reproducing the async cycle regression

## 9. Debugging operations

### 9.1 Initial rollout is `compiled in, default off`

> **Update (2026-07-05, after ADR-0003 acceptance)**: the "default off" in this section was the initial-rollout policy.
> Upon passing ADR-0003's acceptance gate, the **production default was switched to `MUTSU_GC=on`**
> (trigger = candidate buffer size threshold + adaptive backoff). `MUTSU_GC=off` is the explicit opt-out.
> Exception: only the crate's own unit-test builds (`cfg!(test)`) default to off — `cargo test` shares
> process-global collector state across parallel threads, and in-process safepoint collects would
> interfere across tests (see `gc::test_support`). GC-on unit testing is covered by the CI gc-stress job
> with explicit `MUTSU_GC=on` + single-thread.

In the first stage of Level 1a, the GC code is compiled into the binary but **cycle collection does not run by default**.

Policy:

- normal execution does not reclaim cyclic references
- root visitor / child visitor / candidate push / verify hooks are implemented
- collect is opt-in only

Rationale:

- first we want to confirm that "the presence of GC code does not break normal execution"
- we want to separate bugs in candidate registration / root enumeration from bugs in the actual reclamation

Recommended env:

- `MUTSU_GC=off|on`
- initial default is `off`
- even with `on`, at first collect runs only via `collect_now()` / stress modes

Precise meaning in the initial implementation:

- `MUTSU_GC=off`
  - candidate registration may still happen
  - no reclamation whatsoever
  - the manual hook (`gc_debug_collect_now`) need not be a no-op; "log the collect attempt and skip" is acceptable
- `MUTSU_GC=on`
  - collect is permitted
  - but does not run without a trigger

### 9.1a Basic rules for env vars

All GC env vars **default to unset**.

- booleans use `0|1` as canonical
- modes are fixed lowercase (`off|on`, `summary|detail|trace`)
- on numeric parse failure, emit a warning to `stderr` once and fall back to the default
- unknown enum values likewise get a warning + the default

Priority:

1. if `MUTSU_GC=off`, all other `MUTSU_GC_*` triggers are **disabled entirely**
2. on top of `MUTSU_GC=on`, interpret manual / deterministic / random
3. deterministic triggers and random triggers **may be combined**
4. if multiple conditions hold at the same safepoint, the reason is normalized to a single one by this priority:
   `manual > every_safepoint > every_candidate > explicit_safepoint > random > threshold`

This "reason normalization" is for logs and stats; the collect itself executes only once.

### 9.2 Stress is primarily deterministic, not random

Random collect is useful, but it is not the first battleground.
**Build reproducible deterministic stress first.**

Modes wanted first:

- `MUTSU_GC_EVERY_CANDIDATE=N`
  - collect on every N candidate pushes
- `MUTSU_GC_EVERY_SAFEPOINT=1`
  - collect at every safepoint
- `MUTSU_GC_AT=call,return,await,...`
  - collect only at specific safepoint kinds
- `MUTSU_GC_COLLECT_NOW=1`
  - collect exactly once right after startup or at a specific timing

The minimal implementation set for the first cut:

- `MUTSU_GC`
- `MUTSU_GC_EVERY_SAFEPOINT`
- `MUTSU_GC_EVERY_CANDIDATE`
- `MUTSU_GC_LOG`
- `MUTSU_GC_VERIFY`

Things that may be deferred from the first cut:

- `MUTSU_GC_AT`
- `MUTSU_GC_COLLECT_NOW`
- `MUTSU_GC_RANDOM_SEED`
- `MUTSU_GC_RANDOM_RATE`

Rationale:

- what we want first is deterministic stress plus logging/verify
- safepoint-kind selection and random can wait until the first cut is stable

Random comes afterward:

- `MUTSU_GC_RANDOM_SEED=<u64>`
- `MUTSU_GC_RANDOM_RATE=<0.0..1.0>`

Interpretation of random:

- `MUTSU_GC_RANDOM_RATE=0` disables random
- if `MUTSU_GC_RANDOM_RATE>0` and no seed is specified, do not auto-assign a fixed seed;
  **adopt a seed derived from the startup time and always log it**
- reproduction runs must always pin the seed

Interpretation of `MUTSU_GC_EVERY_CANDIDATE`:

- `0` or unset = disabled
- `1` = every candidate push
- `N>1` = collect at the safepoint where the push counter reaches a multiple of N

Interpretation of `MUTSU_GC_EVERY_SAFEPOINT`:

- `0` or unset = disabled
- `1` = collect at every collectable safepoint
- `>1` is not accepted: warning + treated as `1`

### 9.2a Fix the safepoint kinds as an enum

Before introducing `MUTSU_GC_AT`, fix the internal safepoint kinds first.

Level 1a collectable safepoints:

- `backedge`
  - the backward edge of the bytecode dispatch loop
- `call`
  - immediately before or after a call frame push
- `return`
  - immediately before or after a call frame pop / return merge
- `await`
  - poll / resume boundaries of promise/channel await
- `react_poll`
  - one poll unit of the react/supply drive loop
- `lazy_force`
  - pull / resume boundaries of `force_lazy_list*`
- `nested_run`
  - entry/exit of nested VM execution entered via `with_nested_registers`
- `thread_join`
  - join / merge boundaries of `start` / `hyper` / `race`
- `manual`
  - debug hook / explicit collect

NOT collectable in Level 1a:

- internal helpers while holding locks
- while a borrow equivalent to `arc_contents_mut` is active
- during write-through of attrs/arrays/hashes

Implementation rules:

- the collector's trigger decision always receives a safepoint kind
- `MUTSU_GC_EVERY_SAFEPOINT=1` reacts to all of the collectable safepoints above
- when `MUTSU_GC_AT=call,await,...` is introduced later, these enum names are stringified as-is

Rules:

- random runs must **always log the seed**
- CI's main line is deterministic stress
- random is for nightly / local reproduction / long-running fuzz

### 9.3 How to run roast / tests

Three assumed stages:

1. `GC=off`
2. `GC=on + deterministic stress`
3. `GC=on + random (pinned seed)`

Initial production operation:

- normal `make test` / roast run with `GC=off`
- an additional job with `GC=on + every safepoint`
- an even heavier job with `GC=on + every candidate`

Random is not required in the always-on CI, but we want it at least before releases and in nightly runs.

### 9.3a Failure logs observed in the gc-stress job

The gc-stress job (`gc-stress` in `ci.yml`: `GC=on` + `MUTSU_GC_EVERY_CANDIDATE=1024`
+ `MUTSU_GC_VERIFY=1` + `MUTSU_ROAST_TIMEOUT_SCALE=2`) is a BLOCKING gate, so when it
fails, never settle for "flaky, just re-run" — always classify the cause and record it here.

Classification guide:

- **`VERIFY FAIL` appeared** → heap corruption. A genuine GC bug. Fix with top priority.
- **`exit 124` + `Failed: 0` (Bad plan, planned N ran M<N)** → per-file timeout.
  Not corruption. The typical case is a thread-heavy test that passes in a standard
  standalone run but exceeds its budget under `prove -j4` parallel CPU contention. After
  confirming that no `VERIFY FAIL` appeared, measure the test's standalone runtime with the
  release + GC-on settings, and if it is a load timeout,
  raise the per-file timeout in `run-roast-test.sh`.
- **A concrete subtest's `not ok`** → very likely a GC logic regression. Reproduce standalone and investigate.

Observation record:

- **2026-07-08, PR #4325 (fix-typed-array-gather-init):** `roast/S17-promise/start.t`
  timed out with `exit 124` (planned 65 / ran 49). Unrelated to the PR content (typed
  array gather init). No `VERIFY FAIL`. In standalone runs with the release + GC-on settings
  it passes stably at 65/65 in ~7.5s (measured 3 times). The cause was that spawning `^300`
  `start` blocks exceeded the 60s (=30s×2) budget under `prove -j4` parallel load. As a
  countermeasure, the per-file timeout for `start.t` in `run-roast-test.sh` was raised to
  60 (120 after scaling). Not a GC regression.

### 9.4 GC logs are mandatory

Without logs, GC becomes untraceable when it breaks. Include tiered logging from the start.

Recommended env:

- `MUTSU_GC_LOG=summary|detail|trace`

Defaults:

- unset = no GC log
- `summary` = for production / CI
- `detail` = for local reproduction
- `trace` = dedicated to one test / one reproduction

`summary`:

- collect start/end
- cycle id
- reason (`manual`, `threshold`, `safepoint-stress`, `random`)
- candidates / roots / traced / reclaimed
- pause time

`detail`:

- in addition to `summary`, per-phase counts
- number revived
- reclaimed counts by type

`trace`:

- per-node events
- `candidate_push`, `scan`, `revive`, `reclaim`
- dedicated to standalone reproduction / unit tests

Output policy:

- emit to `stderr`
- use fixed wording that is easy to grep, assuming it ends up in `tmp/make-test.log` / `tmp/make-roast.log`
- keep summary to about 2 lines per collect

summary example:

```text
gc: start cycle=42 reason=safepoint candidates=18 roots=241
gc: end cycle=42 traced=55 revived=31 reclaimed=24 pause_ms=0.37
```

trace example:

```text
gc: candidate_push id=123 kind=Array cause=hash_insert
gc: revive id=123 kind=Array ext_refs=1
gc: reclaim id=456 kind=Promise
```

### 9.4a Output format of `MUTSU_VM_STATS` and the GC summary

The existing `MUTSU_VM_STATS` culture is

- one-line summary
- process-global counters
- `stderr`
- a fixed, grep-friendly prefix

so the GC aligns with that.

Policy for the added summary lines:

- unify the prefix to either `[mutsu gc]` or `[mutsu vm-stats] gc:`
- for the first cut, **`[mutsu gc]`** is recommended
- with `MUTSU_VM_STATS=1`, emit one per-run aggregate summary at the end
- with `MUTSU_GC_LOG=summary`, emit a streaming log per collect

That is:

- **streaming per-collect log** = `MUTSU_GC_LOG`
- **end-of-run aggregate summary** = `MUTSU_VM_STATS`

Items to include in the first-cut aggregate summary:

- `collections`
- `candidate_pushes`
- `candidate_dedup_hits`
- `reclaimed_nodes`
- `reclaimed_cycles`
- `pause_ns_total`
- `pause_ns_max`
- `roots_scanned`

Recommended format:

```text
[mutsu vm-stats] gc: collections=12 candidate_pushes=48 dedup_hits=7 reclaimed_nodes=19 reclaimed_cycles=6 pause_ns_total=812345 pause_ns_max=220111 roots_scanned=1440
```

Recommended streaming summary format:

```text
[mutsu gc] start cycle=42 reason=await safepoint=await candidates=18
[mutsu gc] end cycle=42 traced=55 revived=31 reclaimed=24 roots=241 pause_ns=370000
```

Recommended `trace` format:

```text
[mutsu gc] candidate_push cycle=42 id=123 kind=Array cause=hash_insert
[mutsu gc] revive cycle=42 id=123 kind=Array ext_refs=1
[mutsu gc] reclaim cycle=42 id=456 kind=Promise
```

Logging design rules:

- keys are `key=value`, following the existing `vm-stats` style, not snake_case
- 1 event = 1 line
- keys that are grepped often (`cycle`, `reason`, `kind`, `id`) appear in a fixed order
- random runs must emit the seed on the first log line

Example:

```text
[mutsu gc] config mode=on random_seed=12345 random_rate=0.050000 every_safepoint=1 verify=1
```

### 9.5 verify hook

In the early GC-introduction period, **verify is weighted roughly as heavily as the collector itself**.

Recommended env / hooks:

- `MUTSU_GC_VERIFY=1`
- `gc_debug_collect_now()`
- `gc_debug_stats()`
- `gc_debug_verify_heap()`

Interpretation of `MUTSU_GC_VERIFY`:

- `0` or unset = verify disabled
- `1` = verify before and after each collect
- a `2` could be introduced later to add per-phase verify, but the first cut has only `0|1`

Minimum things verify checks:

- no node reachable from a root has been erroneously reclaimed
- no reclaimed node is double-freed
- child edges are not corrupted
- duplicates or stale ids in the candidate buffer do not corrupt the heap

Logs on verify failure:

- failing node id
- node kind
- parent/child edges
- reachability from roots
- current cycle id / seed / mode

### 9.6 Relationship with `MUTSU_VM_STATS`

Put the GC counters on the existing `MUTSU_VM_STATS`.

Aims:

- see, in numbers, that GC does not sit on the hot path
- confirm `gc_candidate_pushes = 0` on the `fib` / int-heavy benchmarks
- make the process-global execution reality, including worker threads, visible in a one-line summary

## 10. Testing policy

### 10.1 roast alone is not enough

roast is needed to observe the final interactions, but guaranteeing GC correctness with roast alone is dangerous.
**Thicken the heap-graph unit tests first.**

### 10.2 Unit test layers

Minimum required layers:

- graph unit tests
  - self-cycle
  - 2-node / 3-node cycles
  - with roots / without roots
  - cycle + one external reference
- root enumeration tests
  - `stack`
  - `locals`
  - `upvalues`
  - `call_frames.saved_*`
  - supply registries
- collector invariant tests
  - candidate duplicates
  - revive
  - reclaim
  - never erroneously collecting a node that has an external ref
- stress unit tests
  - every safepoint
  - nested call / await / lazy force
- cross-thread tests
  - `Promise`
  - `Channel`
  - supply registry callbacks

### 10.3 Pin tests

Create dedicated pins when introducing the GC.

Examples:

- a promise-channel cycle remains with `GC=off` and is reclaimed with `GC=on`
- a supply callback closure cycle is reproducible under deterministic stress
- a `LazyList` closure/env cycle is reclaimed once the third wave lands

### 10.4 Collect in test mode

In unit / integration tests, do not depend on random; use the **explicit collect hook**.

Principles:

- tests can call `gc_debug_collect_now()` directly
- `MUTSU_GC_EVERY_SAFEPOINT=1` is for integration stress
- random mode is for flushing out regressions and is not used in tests that have expected values

## 11. Implementation order

1. ✅ introduce the root visitor
2. ✅ introduce the child visitor
3. ✅ add just the GC counter slots to `MUTSU_VM_STATS` ahead of time
4. ✅ minimal implementation of `Gc<T>` / node header / candidate buffer (`src/gc/gc_ptr.rs`, #4110)
5. migrate `Array` / `Hash` / `ContainerRef` as the first wave ← **we are here**
   - **5a ✅ prerequisite: add COW mutation APIs to `Gc<T>`** (`get_mut` / `make_mut` [`T: Clone`] /
     `ptr_eq`). Container migration makes heavy use of `Arc::make_mut` (COW) and `Arc::ptr_eq`
     (container identity), so migration is impossible without these on `Gc`. When shared, `make_mut`
     COWs to a fresh single-handle node and adjusts the GC-visible strong count correctly
     (-1 from the old node, new node = 1).
     Verified under Miri (Stacked Borrows). `Value` variants not yet migrated (dead code).
   - **5b ✅ prerequisite: finish `Gc<T>` as a drop-in for `Arc<T>`** (implement `Debug`/`PartialEq`/`Eq`/`Hash`
     by delegation to the pointee — so swapping the field in the `#[derive]`d `Value` is all that's needed; `as_ptr`;
     `clone_erased` [conversion to ErasedGc, no GC-strong increment]; `gc_contents_mut` [the Gc version of `arc_contents_mut`]).
     Confirmed via UFCS that `Arc::make_mut/clone/strong_count/ptr_eq/get_mut(x)` → `Gc::…(x)` passes as a mechanical substitution.
     Miri-verified. Dead code.
     **★Measured flip size (2026-07-03)**: an experimental flip of `Value::Array` to `Gc<ArrayData>` produced
     **479 compile errors** (mostly `Arc<ArrayData>` ripple through function signatures = 407 E0308 / 72 type-annotation E0282).
     With no way to create intermediate checkpoints, safe completion in one session was judged impossible and the flip was reverted. → **5c requires
     a dedicated session per type**. Additional requirements discovered: (a) `Gc: Debug/PartialEq/Eq/Hash` (covered by this 5b)
     (b) using a `Gc`-backed `Value` as a key in an object hash triggers clippy `mutable_key_type` (due to the header's atomics;
     Hash/Eq delegate to the value, so no real harm) → an `#[allow]` is needed at the use sites.
   - 5c onward: mechanically replace `Value::Array(Arc<ArrayData>)` → `Value::Array(Gc<ArrayData>)` one type at a time
     (~479 sites/type). Add the `ArrayData: Trace` + `Value::trace_gc_edges` bridge (enumerate Gc children as ErasedGc and
     recurse into not-yet-migrated containers) at the same time (design validated in this session; to be revived when the flip completes).
     One split PR and a dedicated session per type.
6. migrate `Promise` / `Channel` as the first wave's async nodes
7. introduce the supply registry root visitor (`supplier_state_map` / `supplier_subscriptions_map` / `promise_combinator_map` / `supply_taps_map`) in the first wave
8. synchronous collect at safepoints
9. migrate `Sub` / `Instance` / captured env / attrs in the second wave
10. migrate `LazyList` as the third wave
11. ✅ follow up as needed on the OS-resource-centric async registries that hold `Value` edges
    — **closed by the 2026-07-05 investigation as "no code change required (sound-by-refcount)"**. The implemented
    collector is trial deletion driven from the candidate buffer, and liveness is judged solely by the GC strong refcount
    (the root visitor is not consumed in production — it is test-only). If a registry holds `Value`/`SharedPromise`/
    `SharedChannel`, that holding is itself an external strong ref that revives the node during the scan phase, so
    visitor non-coverage causes no unsoundness, no leaks, and no false VERIFY hits.
    Uncovered registries with edges (the hardening list for when a root-consuming mechanism — full-root VERIFY / a tracing collector —
    is introduced): `AsyncSocketConnMap` (deferred_accept_*) /
    `AsyncSocketListenerMap` (callback) / `SupplyChannelMap` (in-flight SupplyEvent) /
    `PipeProcMap` / `ProcByPidMap` (Proc instance) / `SignalRegistry` (value, queued Emit) /
    `uncaught_handler_store` / `FakeSchedulerMap` (callback) / `LockStateMap.async_waiters`.

## 12. What this design gives up

- a background collector in Level 1a
- generational-ization in Level 1a
- precise scanning of the Rust stack
- turning all of `Value` into handles
- doing NaN-boxing first

## 13. Remaining open questions

The remaining items are narrowed down to three.

1. whether `Instance` splits the "body node" and the "attributes cell" into separate nodes
2. ~~among the OS-resource-centric async registries (socket/proc/udp etc.), how much to include simultaneously in the supply first-wave root visitor~~
   → **resolved (2026-07-05, §11 step 11)**: inclusion unnecessary. The collector is refcount-driven and does not
   consume the root visitor, so registry holdings are automatically sound as external strong refs. Visitor expansion is
   deferred to the §11 step 11 list as hardening for when a root-consuming mechanism is introduced.
3. how the cooperative STW stop protocol coexists with the existing `shared_vars`-family synchronization

Beyond these three, nothing should be left undecided in Level 1a.
