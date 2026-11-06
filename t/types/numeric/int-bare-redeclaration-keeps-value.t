use Test;

# A bare redeclaration (`my int $x;`) of a same-scope variable keeps its value,
# for native types as it does for `my Int $x;` (Language/numerics.rakudoc).
plan 5;

my int $i = 7;
{ my int $i; }
my int $i;
is $i, 7, 'native int keeps its value';

my num $n = 2.5e0;
my num $n;
is $n, 2.5e0, 'native num keeps its value';

my str $s = "abc";
my str $s;
is $s, "abc", 'native str keeps its value';

my uint8 $u = 200;
my uint8 $u;
is $u, 200, 'sized native keeps its value';

my int $j = 3;
my int $j = 4;
is $j, 4, 'a redeclaration with an initializer still assigns';
