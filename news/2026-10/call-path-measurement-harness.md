# A call-path measurement harness for the ADR-15737 slices

`scripts/call-path-ab.sh` prints, per input under `benchmarks/call-path/`, the four
numbers every slice of ADR-15737 quotes: instructions per iteration, allocations per
iteration, the simulated I1 miss rate and the branch mispredict rate (one callgrind
run per iteration count, so all three share a profile), and user-CPU nanoseconds per
iteration from native runs. Every figure is a slope between two iteration counts, so
startup, module load and warm-up cancel.

Inputs: `plain-call` (a one-argument and a zero-argument method call on a fixed object)
and `mooish-lazy` (`Lazy.new; $i.foo` with the real AttrX::Mooish 1.0.10, fetched once
into `tmp/call-path/`, #15688). `scripts/bench-det.sh --alloc-awk` exposes the allocator
edge extractor so the harness and the bench CI count allocations with one implementation.

First reading, profiling build of main@3e94525f, 4 cores:

| input | Ir/iter | allocs/iter | I1 miss | mispredict | ns/iter |
| --- | ---: | ---: | ---: | ---: | ---: |
| plain-call | 33,964 | 9.0 | 8.78% | 6.16% | 5,750 |
| mooish-lazy | 4,125,199 | 2,388 | 3.97% | 16.18% | 1,950,000 |

Not measured here: Red `t/38-equal`'s share of the method-call path (installing Red needs
`mzef` to fetch and run external code, which this session was not permitted to do).
