# Hot function with a compile-time-constant debug guard (common logging pattern).
constant DEBUG = False;

sub step(Int $x) {
    if DEBUG {
        note "step($x)";
    }
    my $y = $x * 2 + 1;
    if DEBUG {
        note "  -> $y";
    }
    $y
}

sub workload() {
    my $sum = 0;
    for ^100000 -> $i {
        $sum += step($i) mod 1000;
    }
    "{$sum}";
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
