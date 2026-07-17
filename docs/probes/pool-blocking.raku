# Does the implementation bound real concurrency for genuinely-blocking tasks?
# raku serializes against max_threads (= 96 here, 8 x cpu-cores): 200 x sleep(2)
# takes ~6.1s (three batches), NOT 2s. mutsu is thread-per-task and unbounded,
# so it finishes in ~2.09s -- faster, but at 200 x 256 MiB of reserved stack.
# Usage: <impl> docs/probes/pool-blocking.raku [N]
my $n = (@*ARGS[0] // 200).Int;
my $t0 = now;
await (^$n).map({ start { sleep 2 } });
say "$n x sleep(2): wall={(now - $t0).Num.round(0.01)}s";
