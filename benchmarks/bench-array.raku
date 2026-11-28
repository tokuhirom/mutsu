# Array operations - push, map, grep, sort
sub workload() {
    my $checksum = 0;

    # Push
    my @arr;
    for 1..10000 -> $i {
        @arr.push($i);
    }
    $checksum += @arr.elems;

    # Map
    my @doubled = @arr.map(* * 2);
    $checksum += @doubled[0] + @doubled[*-1];

    # Grep
    my @evens = @arr.grep(* %% 2);
    $checksum += @evens.elems;

    # Sort (descending)
    my @sorted = @arr.sort({ $^b <=> $^a });
    $checksum += @sorted[0];

    # Reverse
    my @rev = @arr.reverse;
    $checksum += @rev[0];

    "checksum = $checksum";
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
