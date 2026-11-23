use Test;

# An assignment expression yields the scalar it assigned to, so an increment
# or decrement applied to it writes that scalar: `($x = 6)++` leaves 7 in
# `$x` (#18840). The postfix form returns the assigned value, the prefix form
# the incremented one.
plan 11;

my $x = 5;
($x = 6)++;
is $x, 7, '($x = 6)++ increments $x';

$x = 5;
is ($x = 6)++, 6, 'the postfix form returns the assigned value';
is $x, 7, '...and leaves the incremented one';

$x = 5;
is ++($x = 6), 7, 'the prefix form returns the incremented value';

my $y;
($y = 6)--;
is $y, 5, '($y = 6)-- decrements';

is ($ = 1)++, 1, 'an anonymous scalar';

my $c = 0;
my $z;
($z = $c++)++;
is "$z $c", '1 1', 'the right-hand side runs once';

my Int $t;
(($t = 3))++;
is $t, 4, 'nested parentheses and a typed scalar';

my $s = "a9";
($s = "Az")++;
is $s, 'Ba', 'a string successor';

class A { has $!a; method m { ($!a = 1)++; $!a } }
is A.new.m, 2, 'an attribute';

sub f { state $st; ($st = 10)++; $st }
is f(), 11, 'a state variable';
