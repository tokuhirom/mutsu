# Floating-point (Num) counterpart of int-arith: iterate the logistic map
# (a classic chaotic-dynamics kernel) in a tight loop — pure Num
# multiply/subtract with no allocation.
sub workload() {
    my $x = 0.5e0;
    for ^200000 {
        $x = 3.9e0 * $x * (1e0 - $x);
    }
    "{$x}";
}

# Warm timed section (convention: benchmarks/bench-regex-match.raku, docs/benchmarks.md):
# the workload runs twice untimed and a third time timed, and that third run is printed
# as `bench-section-seconds:`, so scripts/bench-ci.sh records a `@section` series free of
# interpreter startup. Under scripts/bench-det.sh (BENCH_DET=1) it runs once.
my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;
