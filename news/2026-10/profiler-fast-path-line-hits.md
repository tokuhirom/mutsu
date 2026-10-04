# The profiler counts lines in routine bodies run on a call fast path

With the JIT off, `mutsu --profile` left the bodies of ordinary subs and
methods out of the line table: `sub f() { my $x = 1; $x }` called 20 times
showed its routine entries but no line hits. The call fast paths (and the
lazy-pull and `given` loops) run a body through
`exec_one_backedge_polled`, which polled only after a backward transfer even
with the profiler armed, so `record_line` never saw those lines. Native code
counted them through its `profile_line` hooks, so the JIT-on and JIT-off
profiles disagreed (an ADR-0106 §8 gate 4 break).

The helper now takes the caller's hoisted `DispatchPolls`, like the main
dispatch loops, and polls before every op while the profiler is armed. A
disarmed run pays a register test per op and two relaxed loads per call:
+0.40% instructions on bench-fib, +0.25% on bench-tak, unchanged on
bench-mandelbrot (callgrind, deterministic across repeats), within gate 1's
0.5%.
