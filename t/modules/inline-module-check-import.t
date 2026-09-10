module FiniteField {
    multi infix:<+>(UInt $a, UInt $b) is export { $*modulus Rmod callsame }
    multi infix:<*>(UInt $a, UInt $b) is export { $*modulus Rmod callsame }
    multi infix:<**>(UInt $a, Int $b) is export { expmod $a, $b, $*modulus }
    multi infix:</>(UInt $a, UInt $b) is export { $a * $b**-1 }
}

CHECK {
    use Test;
    plan 1;

    sub f($_) { $_**100 + $_ + 1 }

    import FiniteField;
    my $*modulus = 13;
    is f(10), 1, 'an inline module can be imported from CHECK';
}
