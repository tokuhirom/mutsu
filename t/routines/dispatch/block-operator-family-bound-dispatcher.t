use Test;

# A block's own `multi infix:<op>` runs its winner with the family's
# dispatcher bound as `&infix:<op>`; that bound code object is reused per
# (candidate, dispatcher) pair across calls (ADR-18580 D3). Two routines with
# their own family of the same operator, called alternately, must each keep
# their own candidates, and `callsame` must reach the built-in operator.
# Expected values are rakudo's.
plan 5;

sub run-a($n) {
    my $*modulus = 7;
    multi infix:<+>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    (^$n).map({ $_ + 5 }).join(',')
}
sub run-b($n) {
    multi infix:<+>(UInt $a, UInt $b) { "b" ~ callsame() }
    (^$n).map({ $_ + 1 }).join(',')
}
is (run-a(4), run-b(3), run-a(3), run-b(2)).join(' '), '5,6,0,1 b1,b2,b3 5,6,0 b1,b2',
    'two routines keep their own operator family';

{
    my $*modulus = 1_000_003;
    multi infix:<*>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    multi infix:<+>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    my $acc = 1;
    for 1..2000 -> $i { $acc = $acc * $i + $i }
    is $acc, 221928, 'modular arithmetic over many operator calls';
    is (-3) + 4, 1, 'a non-UInt operand takes the built-in operator';
    is 5 + 6, 11, 'a UInt pair takes the block candidate';
}
is 3 + 4, 7, 'outside the block the built-in operator answers';
