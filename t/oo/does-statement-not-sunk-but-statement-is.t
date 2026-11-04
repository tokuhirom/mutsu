use v6;
use Test;

# rakudo 2026.09: a `but` statement is sunk (the composed `sink` runs); a `does`
# statement is not, however the target is spelled. A routine's result is sunk
# either way (#16511).

plan 11;

my $n = 0;
role Named { method sink { $n++ } }

(5) but role { method sink { $n += 1 } };
is $n, 1, 'but with an anonymous role is sunk';
5 but Named;
is $n, 2, 'but with a named role is sunk';

(5) does role { method sink { $n += 10 } };
is $n, 2, 'does with an anonymous role is not sunk';
(5) does Named;
is $n, 2, 'does with a named role is not sunk';
my $x = 5;
$x does Named;
is $n, 2, 'does on a variable is not sunk';
[<a b c>] does role { method sink { $n += 100 } };
is $n, 2, 'does on an array is not sunk';

my $b = 0;
($b + 1) does role { method sink { $b++ } };
is $b, 0, 'the documentation idiom as a bare statement leaves $b alone';

sub f { ($b + 1) does role { method sink { $b += 10 } } }
f;
is $b, 10, 'a routine returning a does mixin sinks it';

sub g { return [<a b c>] does role { method sink { $n += 1000 } } }
g;
is $n, 1002, 'return of a does mixin is sunk';

sub h { my $r = 5 but role { method sink { $n += 10000 } }; $r }
h;
is $n, 11002, 'a variable holding a but mixin returned from a routine is sunk';

class S { method sink { $n += 100000 } }
S.new does role { };
is $n, 11002, 'does on a fresh object is not sunk';
