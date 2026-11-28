# Takeuchi function (the classic Gabriel/LISP call benchmark): three
# recursive calls per level with three arguments each — measures function
# call and argument binding overhead more heavily than fib.
sub tak($x, $y, $z) {
    if $y < $x {
        return tak(tak($x - 1, $y, $z), tak($y - 1, $z, $x), tak($z - 1, $x, $y));
    }
    return $z;
}
sub workload() {
    "{tak(21, 14, 7)}";
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
