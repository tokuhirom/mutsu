unit module BlockUseTermShadowsType;

class Point {
    has $.x;
    method scale($k) { Point.new(x => $!x * $k) }
}

our constant G is export = Point.new(x => 7);
