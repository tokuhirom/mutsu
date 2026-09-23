use Test;

plan 2;

multi sub infix:<(&)> (Range:D $a, Range:D $b --> Set) {
    Set([$( $a.min .. $b.max )])
}

is (1..5) (&) (3..10), Set([$(1..10)]), 'a user-defined set operator dispatches in binary form';
my $reduced = [(&)] (1..5), (3..10);
is $reduced, Set([$(1..10)]), 'a user-defined set operator dispatches in reduction form';
