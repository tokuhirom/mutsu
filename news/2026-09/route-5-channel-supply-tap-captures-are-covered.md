# ADR-0068 route 5 is unblocked, and it turned out to be covered already

Route 5 of the ADR-0068 cross-thread container-write audit — `Channel.Supply`
tap captures — was the last route the audit left **Exposed** on the path
oracle. §6 question 3 blocked it behind "a separate, deterministic
Channel-supply delivery bug (values dropped/misordered on a single, unloaded
run)", which had to be fixed before the route's race rate could be measured at
all. Both halves are now settled, and neither needed a lock.

## The blocker was mostly a measurement artifact

The delivery bug does not reproduce. A `Channel.Supply` tap delivers every sent
value, in order, on a single unloaded run — 1000/1000 elements through a named
sub, 100/100 values through the day05 three-writer idiom — and matches `raku`
on the same program.

What the original probe hit is a **semantic** divergence rather than a delivery
one. Two taps on one `Channel`'s Supply are *competing consumers* in rakudo:
each sent value goes to exactly one of them (`a=1,3,5 b=2,4,6`). mutsu
broadcasts to all of them instead (`a=1,2,3,4,5,6` twice). The three-tap-writer
FizzBuzz idiom the audit used therefore cannot fill the array at all under
rakudo, and reading its mutsu output as "values dropped/misordered" was reading
that divergence from the other side. A `Supplier` is a genuine broadcaster and
mutsu agrees with rakudo there, so the fault is specific to the channel-backed
Supply. Filed as [#7604](https://github.com/tokuhirom/mutsu/issues/7604) — a
compatibility bug, not a concurrency one, and it does not gate the route.

## With the idiom corrected, the oracle says "covered"

Re-run with one tap (the shape that means the same thing in both
implementations), ADR-0068 §1.2's `rust-gdb` ignore-counter oracle answers:

| probe | cell-keyed guard | name-keyed lane | unsynchronized store |
|---|---|---|---|
| tap body writes captured `%seen` / `@log` / `$sum` | **100** | 0 | **0** |
| tap body calls a named sub writing `@a[$i]` | 0 | **1000** | **0** |

So the capture shape sits on the cell-keyed `ContainerStructGuard` that step 1/2
added, and the named-sub shape on the `shared_array_elem_set` lane — the same
two outcomes routes 1 and 4 reached after that step. The unsynchronized aliased
store is not reached at all. This is what §8 predicted: a new route arrives at
one of the three known funnels rather than needing a fourth.

## Acceptance

Debug build, `MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`,
24-way on 12 cores:

| probe | result |
|---|---|
| day05 three-writer idiom over one `Channel.Supply` tap, 20 emitters | **0 / 96** |
| FizzBuzz three-writer idiom in one tap callback, 20 emitters | **0 / 96** |
| named-sub element store driven from a `Channel.Supply` tap, 1000 writes | **0 / 48** |

Pinned as two new rows in `t/concurrent-celled-container-store.t`, which passes
in full under real rakudo as well as under mutsu.

## What is left of ADR-0068 step 3

None of the audit's routes is now Exposed-and-unmeasured.
[#7543](https://github.com/tokuhirom/mutsu/issues/7543) stays open for the two
remaining loose ends: §3.1's `roast/S17-procasync/stress.t` SIGSEGV, still
unexplained and never reproduced, and the §2 lane-decline reasons that no route
has yet exercised — twigil'd names, a name masked as re-declared, and a
container that was never in a spawning frame's env.
