unit module WithTailHelper;

sub tail-helper-named($p, Int :$elems, :$type = Str) is export {
    "helper($p,$elems)"
}

sub tail-helper-pos($p) is export {
    "helper-pos($p)"
}
