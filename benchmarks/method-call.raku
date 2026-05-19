class Point {
    has $.x;
    has $.y;
    method distance-to(Point $other) {
        (($!x - $other.x) ** 2 + ($!y - $other.y) ** 2).sqrt
    }
}

my $p1 = Point.new(x => 0, y => 0);
my $sum = 0e0;
for ^10000 {
    my $p2 = Point.new(x => $_, y => $_ * 2);
    $sum += $p1.distance-to($p2);
}
say $sum.Int;
