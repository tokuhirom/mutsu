# bench-ctor: close the remaining gap vs rakudo (construction residual costs)

bench-ctor is the only benchmark where mutsu is slower than raku on the bench CI
(ratio 1.17-1.35x across 2026-07-30 rows; every other bench is at or below
parity). The bench is the Zef::Distribution shape: a 20-attribute class with a
parent, `method new(*%_)` delegating to `self.bless(|%_, :meta(%_))`, and TWEAK
submethods at two MRO levels. History: 35x → ~3x (#4557/#4558/#4561) → ~1.3x
today.

## Where the time goes (measured 2026-07-30)

A perf profile (profiling build, 40k iterations) is completely flat — no
dominant function. The cost is the sum of many small per-construction heap and
hash operations: malloc/free family ~17%, NaN-box payload/arc/gc ops ~16%,
`Gc::drop` + cycle-collector `buffer_candidate` ~7%, hashbrown ~5%,
thread-local access ~5%.

Wall-clock A/B with bench variants (min of 5, startup subtracted, per
construction, local machine):

| variant                          | mutsu  | raku   |
|----------------------------------|--------|--------|
| baseline (new + bless + TWEAK x2)| 58.6us | 61.8us |
| TWEAK submethods removed         | 28.4us | 51.2us |
| plain default ctor only          | 13.0us | 50.2us |

So the plain constructor is already ~4x faster than raku; the entire deficit
lives in the phases layered on top:

1. **TWEAK phase: ~30us/construction — half the total.**
   `run_tweak_phase` (src/runtime/methods_dispatch_new.rs) re-derives
   everything on every construction: full MRO walk with per-level registry
   probes, `ordered_role_submethods_for_class`, language-revision metadata
   string building, `cell.to_map()` probe materialization (21 attrs), and then
   `run_instance_method_celled` does a full uncached
   `resolve_method_with_owner_invocant` (multi-candidate matching) per TWEAK
   call. `MUTSU_VM_STATS` confirms 2 resolver-path dispatches per construction
   (TWEAK=10000 for 5000 iterations). None of this is cached, although it is a
   pure function of the class shape — the same shape data that
   `NativeCtorPlan` already memoizes for the default-ctor path.
2. **Custom `new` -> `bless` plumbing: ~15us/construction.**
   `*%_` slurpy -> `|%_, :meta(%_)` -> bless rebuilds pairs/hashes each time
   (MakePair x7 + MakeHash + MakeSlip + MakeRealArray per construction), plus
   3 O(env) `env_deep_copies` per construction (dual-store `cow_mut` — the
   known Slice-F debt) on the dispatch frames.
3. **GC candidate buffer churn:** candidate_pushes=105670,
   dedup_hits=1384390 for 5000 constructions — ~280 dedup hits per object.
   Container temporaries (the `%_` hash, the `:meta` copy, @/% attribute
   defaults, dispatch frames) hammer the cycle-collector buffer thread-local.

Rakudo, by contrast, compiles construction to a BUILDPLAN over flat P6opaque
slots, passes named args as a capture without rebuilding hashes, and spesh
specializes the whole new -> bless -> BUILDALL chain.

## Slices

- [ ] **S1: cache a per-class construction phase plan.** Extend
  `NativeCtorPlan` (or add a sibling cache with the same invalidation sites)
  with the ordered, already-resolved BUILD/TWEAK submethod list: (owner class,
  role origin, method def Arc, 6.c/6.e skip decisions). `run_build_phase` /
  `run_tweak_phase` then iterate the cached list instead of re-walking
  MRO/registry/metadata, and skip `resolve_method_with_owner_invocant` when
  the plan already pins the candidate (single non-multi TWEAK is the common
  case). Also avoid the unconditional `cell.to_map()` probe when no alias
  refresh is needed and the resolved candidate doesn't consume it.
  Expected: removes most of the 30us/construction TWEAK overhead.
- [ ] **S2: trim the custom-new -> bless argument plumbing** — avoid
  rebuilding the named-arg hash/pair vector twice (`%_` slurpy then
  `:meta(%_)` slip flatten), and chase the 3 env_deep_copies/construction on
  this path (dual-store; coordinate with the Slice F campaign, PLAN §6).
- [ ] **S3: GC candidate churn** — ~280 dedup hits per construction is pure
  overhead; investigate suppressing candidate buffering for
  freshly-constructed containers whose refcount never exceeded 1, or batching
  the thread-local lookups.
- [ ] **S4 (ADR territory, long term):** flat attribute slots instead of the
  per-instance hash map (the ADR-0016 "span + shared subject" analog for
  objects). Only if S1-S3 leave a measurable gap.

## Measurement notes

- Iterate counters with the debug build (`MUTSU_VM_STATS=1`, identical to
  release); wall-clock with release. Document numbers come from the bench CI
  (`bench-history.tsv` on `bench-data`), not local runs.
- Local A/B on this machine flatters mutsu (P-core, large caches): local
  baseline already beats raku while the 4c CI runner shows 1.35x — the
  allocation-heavy construction path degrades more on the smaller runner, so
  always confirm on bench CI.
