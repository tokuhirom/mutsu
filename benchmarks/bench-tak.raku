# Takeuchi function (the classic Gabriel/LISP call benchmark): three
# recursive calls per level with three arguments each — measures function
# call and argument binding overhead more heavily than fib.
sub tak($x, $y, $z) {
    if $y < $x {
        return tak(tak($x - 1, $y, $z), tak($y - 1, $z, $x), tak($z - 1, $x, $y));
    }
    return $z;
}
say tak(21, 14, 7);
