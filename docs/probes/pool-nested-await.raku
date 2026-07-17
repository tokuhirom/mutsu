# ★The probe that decides the worker-pool ADR.
# Nested `start` + `await` far deeper than raku's max_threads (96 here).
# raku completes depth 500 in 0.12s WITHOUT deadlocking on 96 workers, because
# its `await` yields a MoarVM continuation ($*AWAITER) and hands the worker
# back. mutsu has no continuations: it survives only because it spawns a real
# thread per task (0.99s, 500 OS threads). A BOUNDED pool in mutsu would
# deadlock on this shape -- every worker parked in a blocking `await`.
# Usage: <impl> docs/probes/pool-nested-await.raku [DEPTH]
my $depth = (@*ARGS[0] // 500).Int;
sub f($n) { start { $n <= 0 ?? 0 !! await(f($n - 1)) + 1 } }
my $t0 = now;
my $r = await f($depth);
say "nested await depth=$depth -> $r, wall={(now - $t0).Num.round(0.01)}s";
