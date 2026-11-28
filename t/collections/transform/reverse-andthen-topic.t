use v6;
use Test;

plan 1;

my Mu $side-effect = 0;
$side-effect = $_ Randthen 23;
is $side-effect, 23, 'Randthen supplies the right value as the left thunk topic';
