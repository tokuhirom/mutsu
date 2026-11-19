# ADR-17335: AttrX::Mooish t/090's wall time is per-round CPU, not waiting — what structural work closes it

- **Status**: Proposed (2026-11-19; for the maintainer's decision, nothing implemented)
- **Issue**: [#17335](https://github.com/tokuhirom/mutsu/issues/17335) (`todo:perf`, AttrX::Mooish
  `t/090-concurrent{,.custom}.rakutest`); side finding filed as
  [#18242](https://github.com/tokuhirom/mutsu/issues/18242)
- **Related**: [ADR-15737](15737-raku-level-call-path-footprint.md) (call-path footprint, D1-D2),
  [ADR-0084](0084-the-frame-env-is-not-the-programs-symbol-table.md) and
  [ADR-12529](12529-frames-resolve-names-through-the-static-link.md) (the frame `Env`),
  [ADR-17216](17216-one-program-world-shared-by-every-thread.md) (`WorldCell`),
  [ADR-15967](15967-start-blocks-submission-order.md) (thread model),
  [ADR-12814](12814-concurrency-memory-safety-follows-rakudo.md) (memory safety under threads),
  [ADR-0096](0096-batteries-adoption-policy.md) (the module runs verbatim)

## 1. Context

`t/090-concurrent.custom.rakutest` is one of the last two in-scope ecosystem files over the
sweep's budget. It runs 20 workers × 10,000 rounds of `Foo.new` / `has-foo` / `foo` /
`clear-foo` on `has Int $.foo is mooish(:lazy, :clearer, :predicate)`. mutsu takes ~290 s and
raku takes ~16 s. The sweep allows 240 s per file; #17335's close condition is ≤ 3x raku.

Two slices cut the per-round instruction count: #18218 by 9.2% and #18235 by 1.8%. Neither moved
the measured wall time. The working hypothesis was therefore that waiting dominates: the
per-Attribute `Lock` that `attr-var` takes (`nqp::lock($!lock)`), lock handoff, or the
scheduler. This ADR measures that hypothesis, refutes it, and ranks the structural options by
what they can actually buy.

## 2. Measurements (2026-11-19)

Setup:

- Release build of main @ 0ce9c1e1, on a 4-vCPU container with nothing else running.
- raku 2026.11 and AttrX::Mooish 1.0.10, both from `checkout-dist.py`.
- The probe is t/090's subtest 1 as a script with knobs `W` (workers) and `R` (rounds per
  worker). It takes a loop-only wall clock, and the loop's CPU time comes from
  `/proc/self/stat`.
- Every cell is three repetitions. Ranges are the spread over those repetitions.
- `nolock` is a copy of the module's `lib/` with the `nqp::lock`/`nqp::unlock` pair in
  `attr-var` removed. It is used only to measure an upper bound. Subtest 1 gives each thread its
  own instance, so the lock is not needed for correctness there.

### 2.1 Throughput, CPU per round, busy cores

| | W | rounds/s | CPU ms/round | busy cores (of 4) |
|---|---:|---:|---:|---:|
| mutsu | 1 | 231-241 | 4.13-4.32 | 1.0 |
| mutsu | 4 | 642-734 | 4.95-5.48 | 3.5-3.6 |
| mutsu | 20 | 630-716 | 4.95-5.47 | 3.4-3.5 |
| mutsu, `nolock` | 20 | 714-825 | 4.63-5.32 | 3.7-3.8 |
| mutsu, `MUTSU_GC=off` | 20 | 678-734 | 4.81-5.26 | 3.5-3.6 |
| raku | 1 | 6,416-7,760 | 0.13-0.16 | 1.0 |
| raku | 20 | 11,479-12,539 | 0.21-0.23 | 2.5-2.6 |
| raku, `nolock` | 20 | 16,633-19,212 | 0.17-0.19 | 3.1-3.2 |

At 200,000 rounds, mutsu's W=20 rate gives 280-317 s and raku's gives 16-17 s. Both match the
real file.

### 2.2 What the numbers say

1. **The file is CPU-bound.** mutsu keeps 3.4-3.6 of 4 cores busy at W=20. That is more than
   raku's 2.5-2.6: raku is the one held back by the module's lock (`nolock` gains it +40%).
   Wall time is `rounds × CPU-per-round / busy-cores`. The single-thread CPU per round is 4.3 ms
   against raku's 0.145 ms, a ~29x gap. That ratio is the whole story.
2. **The lock costs mutsu 8-15%, not a multiple.** At W=20, `nolock` raises throughput by
   8-15%. The critical section (`attr-var` with `AttrProxy.new` and the two FETCH/STORE
   closures) is ~19% of a round's CPU. At 4 cores it is not the binding constraint: it would cap
   throughput at ~5.3 rounds per round-time, above the 4 the cores allow. It causes idle cores
   in handoff bursts, not serialisation.
3. **Lock handoff is cheap enough.** In a microbenchmark of a contended `nqp::lock`/`nqp::unlock`
   loop, mutsu pays ~8-13 µs extra per contended acquisition and raku ~6 µs. Mooish takes the
   lock twice per round, so the handoff costs ≤ 25 µs of a 4,300 µs round (<1%).
4. **GC stop-the-world costs ~3-8%.** `MUTSU_GC=off` changes W=20 throughput by about that,
   within the noise band.
5. **The scheduler is not a factor.** W=4 and W=20 give the same throughput. mutsu runs one OS
   thread per `start` here, and so does rakudo, whose `Lock` also blocks its thread.
6. **CPU per round rises 15-25% from W=1 to W≥4, but not because of in-process sharing.** Four
   *separate* mutsu processes at W=1 inflate the same way (4.4-5.2 ms against 4.0-4.4 alone) as
   one process at W=4 (4.7-5.1 ms). raku inflates similarly (0.14 → 0.21 ms; its plain
   allocation loop 0.007 → 0.009 ms). This is the host's memory system, not a cache line mutsu
   bounces between threads. `WorldCell`'s read path is already one atomic load, and
   `Symbol::intern` already has a per-thread memo.
7. **World writes in the loop:** 3 per round. All three are a compiler temporary,
   `__with_tmp_N` from the `with` desugaring, which `SetGlobal` persists into the shared `our`
   table. It costs ~7 µs per `with` statement under contention: <1% here, but a world write per
   statement everywhere (#18242).
8. **Why the instruction cuts did not show.** The same binary's W=20 throughput spans 630-734
   rounds/s across three runs (±8%). A 9% cut is inside that band for any single A/B. CPU per
   round (`loop-cpu / rounds`) is the less noisy metric. It still needs ≥ 5 interleaved
   repetitions per side.

### 2.3 Where a round's CPU goes

The probe was run under callgrind at W=1, for 20 and 80 rounds; the figures below are the slope
between the two, 8.07M Ir per round. The profile is flat. Grouped:

| bucket | self Ir/round | share |
|---|---:|---:|
| `Symbol::intern` (thread-local `&str` memo), `Symbol::as_str` | 754k | 9.3% |
| `malloc`/`free`/`realloc` | 624k | 7.7% |
| `Env::*` (name-keyed frame env: `walk_with_fallback`, `get_sym`, inserts) | 540k | 6.7% |
| `memcmp`/`memchr` (string keys, `::` scans) | 197k | 2.4% |
| SipHash (`std` `HashMap<String, _>`) | 60k | 0.7% |

| subtree | inclusive Ir/round | share |
|---|---:|---:|
| general binder (20 calls/round: `is raw`, named, slurpy, defaults) | 667k | 8.3% |
| `merge_method_env` | 435k | 5.4% |
| `reconcile_attrs` | 153k | 1.9% |
| `acquire_lock` | ~0 | 0% |

About a quarter of a round goes to keeping a **name-keyed** frame environment coherent: method
entry and exit merge an `Env` by name, attributes are reconciled by name, and the interning and
string comparison that this needs. Another tenth is per-call allocation. The rest is many
medium-sized pieces (type checks and smartmatch ~7%, `:=` binds, dispatch), as in the #17335
comments.

## 3. The gap

| goal | rounds/s needed | CPU/round needed (at today's 3.5 busy cores and the host inflation) | factor |
|---|---:|---:|---:|
| sweep budget with margin (≤ 180 s) | ~1,100 | ≤ ~2.6 ms | ~1.6x |
| #17335 close condition (≤ 3x raku, ~≤ 50 s) | ~4,000 | ≤ ~0.75 ms | ~5.5x |
| the issue's 30 s target | ~6,700 | ≤ ~0.45 ms | ~10x |

Only per-round CPU can close these. Every waiting-side lever together is bounded by the
8-15% (lock) plus the 3-8% (GC) measured above.

## 4. Options

Each option lists its expected gain on this file (measured share or prototype bound), what else
it helps, its cost and risk, its ADR dependencies, and how it keeps the memory-safety rules of
`docs/security.md` / ADR-12814.

### O1. Method frames without a name-keyed `Env` round trip

**Change.** Finish ADR-0084 / ADR-12529 for method bodies. A method's locals, `self` and
attribute cells live in its slot frame. Entry and exit no longer build, merge (`merge_method_env`)
or reconcile (`reconcile_attrs`) a name-keyed `Env`. Names an operation still needs (`EVAL`,
`CALLER::`, `::('$x')`, closures that escape) get a lazily materialised view, the shape ADR-0084
already prescribes for subs.

**Gain.** Bounded by the buckets in §2.3 that exist only for that round trip: ~20-27% of round
CPU, so 1.25-1.35x. This is the largest single structural lever measured.

**Also helps.** Every method-heavy program: Red, ParaSeq, DEPARSE, roast `S12`. Interning and
string-key traffic shrink everywhere it lands.

**Cost and risk.** Large. Method bodies have the most name-reaching edge cases (phasers, `CATCH`
with `.rethrow`, closures captured into Proxies, `callwith`/`nextsame`, private methods on a
Proxy subclass's `.VAR`). Roast `S12`/`S06` and the battery are the guard. The risk is
compatibility, not flakiness: it removes a dual mechanism, which is the gain the "Gain and risk"
section of `AGENTS.md` asks for.

**Dependencies.** ADR-0084, ADR-12529 (phase 2 for methods), ADR-0077 (contiguous locals),
ADR-0121 (attribute slots).

**Memory safety.** Frames are per-thread execution state (ADR-17216 §2). No new shared state,
no `unsafe`.

### O2. ADR-15737 D1 + D2: a per-site resolved call plan, per-call allocation removed, the general binder's remaining shapes

**Change.** Already Accepted. Extend it with the binder shapes Mooish uses on 20 calls per
round: sigilless/`is raw` with a *variable* argument, named parameters, `*%slurpy`, defaults
that are barewords.

**Gain.** ADR-15737 estimates ~1.3x for the call floor. Allocation is 7.7% self and the binder
8.3% inclusive here, so 1.15-1.3x on this file.

**Also helps.** Every call-heavy program; this is the call floor.

**Cost and risk.** Medium, sliced; the ADR's slice plan stands.

**Dependencies.** ADR-15737.

**Memory safety.** ADR-15737 already keeps plans in per-interpreter `caches`, not in the shared
`CompiledCode`. Kept as is, nothing shared becomes mutable.

### O3. Shrink the `attr-var` critical section

**Change.** Give the operations inside it a compiled lane:

- constructing a Proxy subclass (`AttrProxy.new` → `callwith(:$FETCH, :$STORE)`, today through
  the by-name terminal and a method-table `walk_mro`);
- a private method on a Proxy subclass's `.VAR` (`.VAR!SET-SELF`, today through
  `dispatch_private_method_on_proxy_subclass` → `run_resolved_method_celled`);
- a lean `:=` bind (#17874).

The module's lock stays. The module runs verbatim (ADR-0096), and rakudo takes the same lock.

**Gain.** At most the `nolock` bound: +8-15% throughput at W=20 today. It matters more after O1
and O2: if they shrink the rest of the round faster than the section, the section's share grows
and the lock starts to bind.

**Also helps.** Any Proxy-subclass user (Mooish is the main one in the ledger).

**Cost and risk.** Medium. Three lanes, each with its own guards.

**Dependencies.** None new; #17874, #14855.

**Memory safety.** Per-thread; the lock itself is untouched.

### O4. Thread-runtime tuning: GC stop-the-world and lock handoff

**Change.** Two parts:

- the cycle collector's stop-the-world triggers less often while many mutators are live
  (per-thread candidate thresholds);
- `acquire_lock` spins briefly before it enters `gc::wait_until` → `worker_pool::enter_blocking`
  (helper registration, pool lock, park bookkeeping).

**Gain.** ≤ 3-8% for the GC part and ≤ 1% for the handoff (§2.2).

**Cost and risk.** The GC change touches ADR-0003's adaptive threshold and the STW protocol,
where a soundness bug is a memory-safety bug.

**Dependencies.** ADR-0001 §6.1, ADR-0003, ADR-0123.

**Memory safety.** The spin must stay inside the existing `Mutex<LockState>`. The GC change must
keep the quiescence protocol as it is.

### O5. Keep compiler temporaries out of the world table (#18242)

**Change.** A `with`/`without` temporary is compiled to a frame slot and never reaches
`SetGlobal`'s `our` persistence.

**Gain.** <1% here, but it removes a world write per statement from every threaded program,
along with the cross-thread aliasing of a temporary.

**Cost and risk.** Small, a `todo:ticket`.

**Memory safety.** Removes shared writes; adds nothing.

### Ruled out by the measurements

- **Scheduler or thread-pool changes.** W=4 and W=20 are equal (§2.2 item 5).
- **Lock-free or per-thread `WorldCell` reads and per-thread caches.** Reads are already one
  atomic load, and the CPU inflation is the same in separate processes (§2.2 item 6).
- **Changing or bypassing the module's lock.** The module runs verbatim (ADR-0096), and rakudo
  holds the same lock. A native `AttrProxy` would be a BATTERIES rung-3 substitute, which is
  banned.

## 5. Decision (proposed)

1. **Recommend O1 as the structural carrier for #17335**, with O2 continuing in parallel under
   ADR-15737. These are the only two levers that scale with the gap. O1 needs its own
   design pass in `lane:env` (#12529) before code. This ADR proposes the direction, not the
   design.
2. **O3 after O1/O2's first slices.** Re-measure the `nolock` bound each time. Do O3 when the
   bound exceeds ~15%.
3. **O5 now, as a ticket (#18242).** It is independent and cheap.
4. **O4 is not pursued** unless, after O1-O3, the `MUTSU_GC=off` or `nolock` bound alone exceeds
   ~10%.
5. **Measure with CPU per round, not single wall A/Bs.** Use `loop-cpu / rounds` at W=1 and
   W=20, with ≥ 5 interleaved repetitions per side. The probe is t/090 subtest 1 with `W`/`R`
   knobs and a `/proc/self/stat` CPU reading around the loop, as in §2.

**Expected gain against the gap.** O1 (1.25-1.35x) × O2 (1.15-1.3x) × O3 (≤ 1.1x) × O5
(~1.01x) comes to ~1.6-1.9x. That is ~150-180 s on this box: inside the sweep budget with
margin. It is **not** the close condition (5.5x) or 30 s (10x). Those need the per-statement
cost of method bodies to fall roughly to raku's JIT-compiled level, which no option here
reaches. The maintainer may keep #17335 open as the long-term target, or restate its close
condition as "passes the sweep budget with ≥ 25% margin" once O1/O2 land.

## 6. Consequences

- The waiting-side hypothesis is retired for this file. Future slices quote CPU per round.
- O1 becomes the next large `lane:env` work item and needs a design ADR of its own.
- The ecosystem ledger can count `.custom` as passing only after O1/O2. Until then its result
  depends on the runner's load.

## 7. Open questions

- Is the per-statement penalty inside Mooish's methods (5-15 µs against 0.3-2 µs for the same
  statement isolated, #17335) fully explained by the `Env` round trip of O1, or partly by
  methods leaving the compiled tier (`CATCH`, `LEAVE`, closures)? A per-shape microbenchmark
  should be O1's first slice.
- Should the 3x-of-raku close condition hold for a file whose module serialises on one lock per
  Attribute? raku itself uses only 2.5 of 4 cores here.
