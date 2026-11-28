class Point {
    has $.x;
    has $.y;
    method distance-to(Point $other) {
        (($!x - $other.x) ** 2 + ($!y - $other.y) ** 2).sqrt
    }
}

sub workload() {
    my $p1 = Point.new(x => 0, y => 0);
    my $sum = 0e0;
    for ^10000 {
        my $p2 = Point.new(x => $_, y => $_ * 2);
        $sum += $p1.distance-to($p2);
    }
    "{$sum.Int}";
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
