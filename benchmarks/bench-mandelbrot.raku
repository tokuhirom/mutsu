# Mandelbrot escape-time on a 60x60 grid (classic benchmarks-game kernel):
# nested loops around a data-dependent while loop of Num multiply/add/compare.
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
say $total;
