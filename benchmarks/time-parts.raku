# Decompose timestamps into days/hours/minutes/seconds.
sub workload() {
    my $total = 0;
    for ^350000 -> $i {
        my $ts      = $i * 97 + 13;
        my $days    = $ts div (60 * 60 * 24);
        my $hours   = ($ts mod (60 * 60 * 24)) div (60 * 60);
        my $minutes = ($ts mod (60 * 60)) div 60;
        my $seconds = $ts mod 60;
        $total += $days + $hours * 2 + $minutes * 3 + $seconds * 5;
    }
    "{$total}";
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
