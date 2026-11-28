# Mandelbrot escape-time on a 60x60 grid (classic benchmarks-game kernel):
# nested loops around a data-dependent while loop of Num multiply/add/compare.
sub workload() {
    my $total = 0;
    for ^60 -> $py {
        my $ci = $py.Num * 0.05e0 - 1.5e0;
        for ^60 -> $px {
            my $cr = $px.Num * 0.05e0 - 2.2e0;
            my $zr = 0e0;
            my $zi = 0e0;
            my $iter = 0;
            while $zr * $zr + $zi * $zi <= 4e0 && $iter < 100 {
                my $t = $zr * $zr - $zi * $zi + $cr;
                $zi = 2e0 * $zr * $zi + $ci;
                $zr = $t;
                $iter = $iter + 1;
            }
            $total = $total + $iter;
        }
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
