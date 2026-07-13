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
say $total.Int;
