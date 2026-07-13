# Floating-point (Num) counterpart of int-arith: iterate the logistic map
# (a classic chaotic-dynamics kernel) in a tight loop — pure Num
# multiply/subtract with no allocation.
my $x = 0.5e0;
for ^200000 {
    $x = 3.9e0 * $x * (1e0 - $x);
}
say $x;
