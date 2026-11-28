# Recursive fibonacci - tests function call overhead and recursion
sub fib(Int $n --> Int) {
    if $n <= 1 {
        return $n;
    }
    return fib($n - 1) + fib($n - 2);
}

sub workload() {
    my $result = 0;
    for 1..25 -> $i {
        $result = fib($i);
    }
    "fib(25) = $result";
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
