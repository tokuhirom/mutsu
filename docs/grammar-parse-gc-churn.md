# Grammar-parse GC churn: acyclic Match garbage pushed as cycle candidates

**Status:** finding / not-yet-actioned. Owner: GC campaign (PLAN.md §2).
**Recorded:** 2026-07-11, from profiling zef's grammar dist-identity parse.

## TL;DR

Grammar parsing produces a large volume of **acyclic** garbage — `Match`
objects and their capture trees, whose payload is `InstanceAttrs`
(`HashMap<String, Value>`). Plain `Arc` refcounting already frees all of it
(it is tree-shaped, no back-edges). Yet every container-kind `Value` is pushed
into the GC cycle-candidate buffer on drop and processed by the trial-deletion
collector, which then reclaims **zero cycles**. The cycle collector is doing
substantial work for no reclaimed cycles on this workload.

## Evidence

`MUTSU_VM_STATS=1` on the zef-shape microbench (`tmp/zbench.raku`, 200 parses of
`Foo::Bar::Baz:ver<1.2.3>` via a `grammar REQUIRE { token ... } :actions`),
debug build, **after** the char-class-token static-resolve fix (PR #4412):

```
gc: collections=9 candidate_pushes=365806 dedup_hits=242807
    reclaimed_nodes=177544 reclaimed_cycles=0
    pause_ns_total=135334720 (135ms) pause_ns_max=30ms roots_scanned=188262
```

Before that fix the numbers were ~2x larger (candidate_pushes=725129,
reclaimed_cycles=0). In both cases **`reclaimed_cycles=0`** across every
collection — the collector never finds an actual cycle here.

`perf --call-graph fp` (debug + `-Cforce-frame-pointers=yes`) on the same
microbench showed **~54% of CPU in `drop_in_place<HashMap<String, Value>>`**,
reached via `collect_cycles_at` -> drop `Arc<GcBox<dyn Trace>>` ->
`InstanceAttrs` -> the map. So the collector is spending its time dropping
`Match`/capture attribute maps that refcounting would have freed anyway.

## Why it happens

Per ADR-0001 the GC uses a scalar/container **type filter** so cycle-free scalar
values (Int/Num/Str/...) pay zero GC cost. But `Match`/`Instance` are
container-kind (`InstanceAttrs`), so they are always buffered as potential cycle
roots when dropped with survivors — even though a `Match` capture tree is a pure
tree (parent holds children; children never point back to the parent), so it can
never form a cycle in this workload.

## Possible directions (for the GC owner to weigh)

- **Skip buffering tree-shaped / never-cyclic instance kinds.** `Match` (and
  likely other parser-internal instances) are structurally acyclic; if they can
  be marked so, their drops need not enter the candidate buffer at all. Needs a
  sound criterion (a `Value`/class flag) so nothing that *could* cycle is
  excluded.
- **Threshold / batching tuning for buffer churn.** `candidate_pushes` and
  `dedup_hits` are hundreds of thousands per 200 parses; even without a
  collection the push+dedup hashing is real cost. A cheaper candidate set (or
  not re-pushing an already-buffered node) may help.
- **Measure with `MUTSU_GC=0`** to bound the ceiling: how much of the residual
  ~9s/200-parses is GC vs. the regex engine itself.

## Repro

```
# microbench (create if absent) — see the "NEXT SESSION" repro in memory
#   grammar REQUIRE { token TOP {<name><verpart>?} token name {<-restricted>+ % '::'} ... }
#   class Actions { method TOP($/){make ~$<name>} ... }
#   for ^200 { REQUIRE.parse("Foo::Bar::Baz:ver<1.2.3>", :actions(Actions)) }
MUTSU_VM_STATS=1 target/debug/mutsu tmp/zbench.raku 2>&1 | grep 'gc:'

# perf (NOPASSWD at the exact path; build debug with frame pointers first):
RUSTFLAGS="-Cforce-frame-pointers=yes" cargo build
sudo -n /usr/lib/linux-tools/6.8.0-100-generic/perf record -g --call-graph fp \
  -F 999 -o /tmp/p.data -- target/debug/mutsu tmp/zbench.raku
sudo -n /usr/lib/linux-tools/6.8.0-100-generic/perf report -i /tmp/p.data --stdio -g none | head
```
