use Test;

# #15659: a declared `multi infix:<eq>` (or lt/ne/...) keeps the core word's
# pairwise chaining: `a eq b eq c` is `(a eq b) && (b eq c)`, each pair
# dispatching through the declared candidates and the core body.

plan 10;

class T { has $.v }
multi infix:<eq>(T \a, T \b) { a.v == b.v }
multi infix:<lt>(T \a, T \b) { a.v < b.v }

is ("a" eq "a" eq "a"), True, 'three equal strings chain';
is ("a" eq "a" ne "b"), True, 'a mixed eq/ne chain';
is ("a" eq "b" eq "b"), False, 'the first pair failing fails the chain';
is ("a" eq "a" eq "b"), False, 'the last pair failing fails the chain';

my $x = T.new(:v(1));
my $y = T.new(:v(1));
my $z = T.new(:v(2));
is ($x eq $y eq $y), True, 'the declared candidate answers each pair';
is ($x eq $y eq $z), False, 'and the chain stops at a failing pair';
is ($x lt $z lt T.new(:v(3))), True, 'a declared lt chains too';

is (1 lt 2 lt 3), True, 'the core lt chains with the declaration in scope';
is ("x" eq "x"), True, 'a single comparison is unchanged';

my $calls = 0;
sub mid { $calls++; "m" }
"m" eq mid() eq "m";
is $calls, 1, 'the shared middle operand is evaluated once';
