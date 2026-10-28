use Test;

plan 11;

my \x = 5;
is x, 5, 'my \x = value reads back';
throws-like { EVAL 'my \y = 1; y = 2' }, X::Assignment::RO, 'a sigilless term is readonly';

my $c = 1;
my \z := $c;
z = 4;
is $c, 4, 'my \z := $c aliases the container';

my @a = 1, 2;
my \r = @a;
is r.elems, 2, 'a sigilless term of an array';

my @seen;
for 1, 2 -> \k { @seen.push: k }
is @seen, [1, 2], 'for with a sigilless pointy parameter';

is (-> \v { v + 1 })(4), 5, 'sigilless pointy block parameter';
is (-> (\i, \j) { i + j })((1, 2)), 3, 'sigilless destructured sub-parameters shadow the imaginary unit';

given 7 -> \g { is g, 7, 'given with a sigilless pointy parameter' }
with 3 -> \w { is w, 3, 'with a sigilless pointy parameter' }
if my \q = 9 { is q, 9, 'a sigilless declaration as a condition' }

for 1..3 { my \loop = $_; $_ == 3 and is loop, 3, 're-declaration in a loop' }
