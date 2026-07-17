# Per-task thread-spawn cost: N trivial `start {}` blocks, awaited together.
# mutsu spawns an OS thread per task (19 spawn_user_thread sites, no pool);
# raku hands the task to its ThreadPoolScheduler.
# Usage: <impl> docs/probes/pool-spawn.raku [N]
# 2026-07-17, release, 12 cores: mutsu 0.232-0.262s | raku 0.051-0.07s (N=500)
my $n = (@*ARGS[0] // 500).Int;
my $t0 = now;
my @p = (^$n).map({ start { 1 } });
await @p;
say "start x$n: {(now - $t0).Num.round(0.001)}s";
