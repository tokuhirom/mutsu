use Test;

plan 6;

my $block = { 1 };
my $sub = sub { 7 };
throws-like { 5 - $block }, X::Multi::NoMatch,
    'plain subtraction rejects a Block operand';
throws-like { 5 R- $block }, X::Multi::NoMatch,
    'reversed subtraction rejects a Block operand';
throws-like { 5 R* $block }, X::Multi::NoMatch,
    'reversed multiplication rejects a Block operand';
throws-like { 5 R- $sub }, X::Multi::NoMatch,
    'reversed subtraction rejects a Sub operand';
is 5 R- 3, -2, 'numeric reversed subtraction still works';

{
    multi sub infix:<->(Code:D $a, Int:D $b) { 99 }
    is 5 R- $block, 99, 'a user infix candidate may accept the Code operand';
}
