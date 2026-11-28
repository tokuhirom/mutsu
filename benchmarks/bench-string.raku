# String operations - concat, split, join, regex matching
sub workload() {
    my $checksum = 0;

    # String concatenation
    my $s = "";
    for 1..10000 -> $i {
        $s = $s ~ "x";
    }
    $checksum += $s.chars;

    # Split and join
    for 1..1000 -> $i {
        my @parts = "the quick brown fox jumps over the lazy dog".split(" ");
        my $joined = @parts.join("-");
        $checksum += $joined.chars;
    }

    # Regex matching
    for 1..5000 -> $i {
        if "hello world 42 foo" ~~ /\d+/ {
            $checksum += 1;
        }
    }

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
