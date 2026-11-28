# Polymorphic method dispatch: one call site, several receiver classes.
class Circle {
    has $.r;
    method area { 355/113 * $!r * $!r }
}
class Rect {
    has $.w;
    has $.h;
    method area { $!w * $!h }
}
class Triangle {
    has $.b;
    has $.h;
    method area { $!b * $!h / 2 }
}

sub workload() {
    my @shapes;
    for ^100 -> $i {
        @shapes.push(Circle.new(r => $i % 7 + 1));
        @shapes.push(Rect.new(w => $i % 5 + 1, h => $i % 3 + 1));
        @shapes.push(Triangle.new(b => $i % 4 + 1, h => $i % 6 + 1));
    }

    my $total = 0;
    for ^40 {
        for @shapes -> $s {
            $total += $s.area;
        }
    }
    "{$total.Int}";
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
