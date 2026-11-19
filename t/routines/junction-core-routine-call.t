use Test;

# A core routine called in its sub form with a Junction argument threads over
# the Junction when none of its candidates takes one whole, as on rakudo
# (#18098): `abs(any(-1, -2))` is `any(1, 2)`.

plan 7;

is abs(any(-1, -2)).raku, 'any(1, 2)', 'abs';
is chr(any(65, 66)).raku, 'any("A", "B")', 'chr';
is uc(any('a', 'b')).raku, 'any("A", "B")', 'uc';
is sqrt(any(4, 9)).raku, 'any(2e0, 3e0)', 'sqrt';
is flip(any('ab', 'cd')).raku, 'any("ba", "dc")', 'flip';

sub f($x) { abs($x) }
my @warm = (-5..5).map({ f($_) });
is f(any(-1, -2)).raku, 'any(1, 2)', 'a user sub handing its parameter on, after a warm-up';

ok defined(any(1, Any)), 'defined takes the Junction whole';
