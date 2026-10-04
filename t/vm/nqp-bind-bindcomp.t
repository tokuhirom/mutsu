use Test;
use nqp;

# nqp::bind is `:=`, and nqp::bindcomp registers a compiler object that
# nqp::getcomp then answers (#11499). Expected answers are Rakudo 2026.09's.

plan 6;

my $x;
is nqp::bind($x, 5), 5, 'bind answers the value';
is $x, 5, 'bind binds the variable';

my $y = 1;
nqp::bind($x, $y);
$y = 7;
is $x, 7, 'binding to a variable shares its container';

my $comp = [1];
ok nqp::bindcomp('zork', $comp) =:= $comp, 'bindcomp answers the compiler object';
ok nqp::getcomp('zork') =:= $comp, 'getcomp answers the bound compiler object';
ok nqp::isnull(nqp::getcomp('nope')), 'an unbound language is still null';
