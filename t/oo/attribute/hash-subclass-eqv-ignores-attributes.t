use Test;

# `eqv` on `is Hash` / `is Array` subclass instances compares the contents
# (and the type), not the extra attributes the subclass declares (found via
# PDF::COS::Stream's `is-deeply $x.COERCE($x), $x`, #12907).
plan 7;

class H is Hash { has $.x; }
my $a = H.new; $a<k> = 1;
my $b = H.new; $b<k> = 1;
ok $a eqv $b, 'same contents are eqv';
my $c = H.new; $c<k> = 2;
nok $a eqv $c, 'different contents are not eqv';

class H2 is Hash { has $.s is rw; has @.l; }
my $d = H2.new; $d<k> = 1;
my $e = H2.new; $e<k> = 1;
$e.s = 3;
ok $d eqv $e, 'a differing declared attribute does not matter';
is-deeply $d, $e, 'is-deeply agrees';
nok $a eqv $d, 'different classes are not eqv';

class A is Array { has $.s is rw; }
my $x = A.new(1, 2); my $y = A.new(1, 2);
$y.s = 9;
ok $x eqv $y, 'Array subclass: attributes ignored';
nok $x eqv A.new(1, 3), 'Array subclass: contents differ';
