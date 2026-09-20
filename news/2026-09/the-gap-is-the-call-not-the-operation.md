# The gap is the call, not the operation

mutsu runs `JSON::Fast` about 59x slower than rakudo, and the working
assumption behind the last several perf campaigns was that this is a broad tax
— that every value operation goes through NaN-box decode, enum dispatch and a
generic path, and is therefore uniformly some tens of times too expensive.

Measured per primitive operation, that assumption is **wrong**, and usefully so.

| operation | mutsu ns | raku ns | ratio |
| --- | ---: | ---: | ---: |
| `$t = $t + 1` | 86.5 | 21.2 | **4x** |
| `1 < 2 ?? 1 !! 0` | 102.5 | 13.4 | 8x |
| `$a ~ $b` | 398.4 | 47.5 | 8x |
| `@a[3]` (read) | 152.5 | 17.3 | 9x |
| `@a[3] = 1` | 177.0 | 18.6 | 10x |
| `@a.push(1); @a.pop` | 980.1 | 111.7 | 9x |
| `%h<k>` (read) | 182.4 | 76.5 | 2x |
| `%h<k> = 1` | 746.7 | 17.7 | 42x |
| **`f()`** | **794.2** | **5.8** | **137x** |
| **`$o.m()`** | **3052.8** | **9.7** | **315x** |
| **`$o.v`** (accessor) | **1054.0** | **11.4** | **93x** |
| `C.new(v => 1)` | 3119.9 | 154.0 | 20x |
| the `while` loop itself | 146.5 | 2.1 | 70x |

Arithmetic, strings and containers are **already at 4-10x**. Nothing about
them needs type specialization to get under 10x; they are under 10x. The gap
is concentrated in one mechanism — **the call** — and it is an order of
magnitude worse than everything around it.

Measured as instructions rather than time, on the same 100,000-iteration loops
with the bare loop skeleton subtracted:

| | instructions per call |
| --- | ---: |
| `$o.m()` | **22,528** |
| `f()` | **7,212** |

rakudo does a method call in roughly 30 cycles.

## What those 22,528 instructions are

From the callgrind caller tree of the method-call loop, **per single call**:

| | per call |
| --- | ---: |
| `Symbol::intern` | **10** |
| `Symbol::resolve` (each one a heap allocation) | 3 |
| `String::clone` | 2 |
| `resolve_user_method` — a full by-name method resolution | **1** |
| `HashMap` clone | 1 |
| heap allocations, all sources | **~11** |

plus an MRO walk (`Registry::class_mro_readonly`, 3.9% of the run), an
`is_native_method` probe, and `LocalKey::with` at 9.6% of the whole program —
2,360 thread-local accesses per call.

Put plainly: **mutsu resolves the method by name, from scratch, on every
call.** The receiver's class, the method name and the MRO are the same on
iteration 100,000 as they were on iteration 1, and none of that is
remembered. MoarVM caches `(receiver type -> resolved method)` at the call
site and spends a type-id comparison.

## Why this reframes the perf program

Three things follow, and two of them contradict what this repository was
about to do.

**1. `MetaNs`/env name resolution is not the ceiling — it is a symptom.**
[#8830](https://github.com/tokuhirom/mutsu/issues/8830)'s thesis was right
that runtime string-keyed resolution dominates, but the profile category it
was measured in ("~22% of `JSON::Fast`") was reading the *consequences* of
the call protocol from the outside. Six slices landed against that issue for
a cumulative 2,975,774,664 -> 1,843,258,057 Ir on the benchmark; the last one
removed **65.8%** of the single largest entry in the profile for **4.3%**
overall. That is what grinding a symptom looks like.

**2. `JSON::Fast` is a recursive-descent parser, i.e. a program made of
calls.** Its 59x is not a mystery any more; it is the 137x/315x rows above
multiplied by how often it calls.

**3. The JIT loop work would not have helped it.** The `while` skeleton row
(70x) is real and is exactly what a fused native loop region would attack —
but it is the *cheapest* row in the table in absolute terms, and `JSON::Fast`
spends its time in calls, not in loop headers. Three slices landed this month
confirm it empirically: the typed-store fast path (-43% on a numeric loop),
the nqp inline path (-44%) and the DESTROY sweep (-3.9%) moved the JSON parse
by 0%, -0.1% and 0% respectively.

## How it was measured

`benchmarks/micro/primitive-ops.py`, added with this entry. Each operation
runs inside the same loop skeleton at N and at 2N iterations, and
`(t(2N) - t(N)) / N` is its per-iteration cost with startup, module load and
compile time cancelled exactly rather than estimated; the empty loop's own
per-iteration cost is then subtracted to leave the operation alone. N is
doubled per engine and per operation until the difference is large enough to
measure, which is what lets one table hold operations three orders of
magnitude apart.

Two rows are worth not over-reading. `%h<k>` reads at 2x and stores at 42x,
while rakudo's own read (76.5 ns) is four times its store (17.7 ns) — the
shape of that pair has not been explained and may be an artefact of what each
side does with the unused result. Everything else in the table is consistent
across runs.

## What to do about it

Filed as [#8880](https://github.com/tokuhirom/mutsu/issues/8880): a
per-call-site inline cache for method dispatch, keyed on the receiver's type
id. It needs no JIT, no register
residency and no change to the value representation — the three things the
previous plan had queued ahead of it.
