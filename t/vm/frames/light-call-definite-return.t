use Test;

# A definite constant return spec (`--> Nil`, `--> True`, `--> False`,
# `--> 42`) discards the body's value and returns the constant. Such routines
# are served by the positional-light call path (#9074); every call is repeated
# so the cached dispatch is exercised as well as the first, uncached one.

plan 11;

my sub nil-rw(str $t, int $p is rw --> Nil) { $p = $p + 1; 42 }
my str $w = "x";
my int $p = 0;
my @got;
for ^3 { @got.push: nil-rw($w, $p) }
is-deeply @got, [Nil, Nil, Nil], '--> Nil discards the body value on every call';
is $p, 3, '--> Nil routine still writes through its is rw parameter';

my sub t($x --> True) { 0 }
my sub f($x --> False) { 1 }
my sub n($x --> 42) { "no" }
for ^2 {
    is-deeply (t(1), f(1), n(1)), (True, False, 42), "constant returns (pass $_)";
}

my sub bare($x --> Nil) { return }
is-deeply (bare(1), bare(2)), (Nil, Nil), 'bare return in a --> Nil routine';

my $seen = 0;
my sub sinks($x --> Nil) { (1..3).map({ $seen++ }) }
is-deeply sinks(1), Nil, 'lazy tail value is discarded';
is $seen, 3, 'lazy tail value is sunk (map callback ran)';
sinks(1);
is $seen, 6, 'lazy tail value is sunk on the cached call too';

my sub fl($x --> Nil) { fail "boom" }
my $r = fl(1);
isa-ok $r, Failure, 'fail bypasses the definite return value';
$r = fl(2);
isa-ok $r, Failure, 'fail bypasses the definite return value (cached call)';
is $r.exception.message, 'boom', 'the Failure carries the failure message';

# vim: expandtab shiftwidth=4
