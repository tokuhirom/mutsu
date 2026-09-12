# The bench suite gets a metric that can actually see a regression

`bench.yml` records wall clock on a shared GitHub runner and warns when a
benchmark's median slows more than 1.5x against the previous recorded run.
Measured against the history that job has itself produced, **that alarm sits
below the series' own noise floor**, so it cannot do the job it was added for.

## How noisy the wall-clock series actually is

For each of the 48 series in `bench-history.tsv`, the median and p90 of
`|change|` between *consecutive recorded main commits* — the step a reader has
to learn to ignore before a real regression becomes visible. Last 40 commits:

| series | median \|Δ\| | p90 \|Δ\| | median \|Δ\| of the ratio |
| --- | ---: | ---: | ---: |
| `bench-ctor` | 31.7% | 96.1% | 25.5% |
| `method-call` | 30.8% | 57.7% | 14.5% |
| `bench-mandelbrot` | 28.1% | 43.8% | 7.7% |
| `bench-fib` | 23.3% | 43.7% | 8.1% |
| `bench-array` | 16.4% | 32.8% | 8.3% |
| `bench-threads` | 15.2% | 40.9% | 7.6% |

Every series measures a median step of 16-33% and a p90 of 29-96%. A 1.5x alarm
is *inside* that p90 for most rows: it rings on noise, and a genuine 30%
regression — a very large regression — never trips it. The mutsu/raku ratio,
which the workflow header offers as the runner-speed normalizer, is better but
still a 5-25% median step.

## What replaces it — alongside, not instead

`scripts/bench-det.sh` runs every benchmark under
`valgrind --tool=callgrind` and records the simulated instruction count.
Three back-to-back runs of one binary:

```
bench-hash             269,436,183 / 269,406,388 / 269,437,654    spread 0.012%
bench-grammar-parse     50,299,105 /  50,328,170 /  50,282,570    spread 0.09%
bench-fib (JIT on)   1,230,716,337 / 1,230,716,715                spread 0.00003%
```

The detectable change goes from 16-33% to about **0.1%**, so the new series
alarms at 2% — twenty times its own noise, and twenty-five times tighter than
anything wall clock can support. Because the count is *simulated*, it does not
depend on the runner's CPU, its load, or its neighbours: the entire class of
variance the wall-clock series fights does not exist here.

Cost is four minutes per configuration for the whole suite on a four-core box,
against the job's (now 45-minute) budget. The JIT-on pass is cheaper than the
JIT-off one, because there are fewer instructions to simulate.

The wall-clock series stays exactly as it was. Instruction counts are blind to
cache behaviour, memory boundness, lock contention and real thread parallelism,
and they step whenever the toolchain changes — a legible one-time step, unlike
the random walk they complement, but a step. Read the two together: the
deterministic series localizes a change, the wall-clock one confirms it
mattered.

## It paid for itself in the same session

The `MetaNs` conversion landed beside this (see
`news/2026-09/magic-key-construction-has-one-constructor.md`) is exactly the
size of change the old series cannot resolve. On the deterministic metric:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| 500000 `@c[$i] = $i` stores | 10,249,295,393 | 9,719,805,922 | **−5.17%** |
| `bench-index-store` | 13,275,156,905 | 12,743,882,240 | **−4.00%** |
| `bench-threads-serial` | 6,880,126,866 | 6,805,773,324 | **−1.08%** |

1,059 instructions removed per element store, measured to three significant
figures. The wall-clock numbers for the same pair of binaries in the same
session moved by +4% on one benchmark and −55% on another, in directions that
were pure noise. Either could have been reported as anything.

Stored in `bench-det-history.tsv` on the `bench-data` branch, keyed by commit,
with the rustc version recorded instead of the runner — the instruction count
does not depend on the runner's CPU, but the toolchain that produced the binary
is what steps it.
