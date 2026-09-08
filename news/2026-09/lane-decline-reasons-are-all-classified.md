# ADR-0068 step 3 is complete: all five lane-decline reasons are classified

ADR-0068 §2 lists five reasons the name-keyed cross-thread lane declines a
container element store, and the campaign's remaining scope was exactly "the set
of ways a container becomes cross-thread reachable while the lane declines".
Three of the five had never been exercised by a probe: the name is not a plain
lexical `@`/`%` (a twigil), the name is masked as re-declared, and the container
was never in a spawning frame's env. Each now has its own oracle-classified
probe and its own stress acceptance, and all three are **covered**.

## First, a correction to the oracle recipe

ADR-0068 §1.2's `rust-gdb` ignore-counter oracle says to break on "the
unsynchronized aliased element store" and read `already hit N times` there, with
nothing on the lane, as **exposed**. That was true when it was written and it is
**not true now**: §7 put `ContainerStructGuard::acquire_for` at
`vm_var_assign_index_named.rs:2379`, and that guard is a *scope* guard whose
region lexically contains both the hash store and the array store further down
the same function. The store site therefore fires on covered writes too, and
following the old recipe verbatim produces a false positive.

The discriminator today is **the store site hit without the guard**. Break on
three things, not two:

| guard | lane | store site | verdict |
|---|---|---|---|
| N | 0 | N | covered by the cell-keyed guard |
| 0 | N | 0 | covered by the name-keyed lane |
| 0 | 0 | N | **exposed** |
| 0 | 0 | 0 | the workload does no aliased container write at all |

## The probes

Each writes 1000 elements from 20 concurrent threads, reaching the container
through exactly one §2 reason. The "lane entry" column counts
`assign_array_elem_to_shared_var` calls including *declines*, which is how you
confirm the probe really put the intended reason in front of the lane instead of
missing it.

| probe (§2 reason) | guard | lane entry | lane accept | array store | hash store | verdict |
|---|---|---|---|---|---|---|
| dynamic `@*log` via a named sub | 1000 | 1000 | 0 | 1000 | — | covered, cell |
| dynamic `%*reg` via a named sub | 1000 | — | — | — | 1000 | covered, cell |
| `@seen` masked by a slurpy `*@seen` parameter | 0 | 1000 | **1000** | 0 | — | covered, lane |
| `@a` reaching the writer only as a parameter | 1000 | 1000 | 0 | 1000 | — | covered, cell |

The twigil rows are covered because `is_plain_lexical_name` requires an
alphabetic second byte, so a dynamic's `*` declines the atomic lane and the
named sub cells the container instead — straight onto the guard.

The re-declared row is the interesting one. `thread_redeclared_vars` is keyed by
**name, not by scope**, so a slurpy `*@seen` parameter anywhere in the program
masks the *outer* `@seen`'s writes too. That decline does not drop the write off
the edge; it lands it on the atomic `__mutsu_atomic_arr::` store.

## Acceptance

Debug build, `MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`,
24-way on 12 cores: **0 / 96 on each of the four probes, 0 / 384 in total.**

Pinned as `t/concurrent-lane-decline-routes.t`. Worth being clear about what
that file pins: rakudo gives concurrent `@a[$i] = ...` no atomicity at all and
loses updates on three of the four blocks (1 of 4 passing, on each of three
runs). This is **mutsu's exclusion invariant**, not a Raku guarantee — which is
exactly what ADR-0068 set out to buy.

## Step 3 is complete

With §11 (route 5), §12 (the `S17-procasync/stress.t` SIGSEGV, ruled out of this
class) and §13, all five §2 reasons are classified, every §3 route is measured,
and the one unexplained crash is off the ledger. The evidence across §8, §10,
§11 and §13 is consistent: this class has **three funnels** — the named element
store, the attribute-rooted element store, and the mutating method — and every
route tried since has arrived at one of them.

`#7543` closes. What is left of ADR-0068 is not exposure but a measurement, and
a perf one at that: §7's read-side guard sits in `Value::with_deref` /
`into_deref`, which are hot, and a threaded program now takes a mutex on every
celled-container read. Nothing has measured it, and it is tracked as
[#7613](https://github.com/tokuhirom/mutsu/issues/7613).
