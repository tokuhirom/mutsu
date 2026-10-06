use Test;

plan 5;

# A parameterised type object bound or assigned into a lexical declared with
# the same parameterised type (#12146).
my Positional[Int] $x;
my Positional[Int] $z := $x;
is $z.raku, 'Positional[Int]', 'Positional[Int] type object binds';

my Associative[Int] $h;
my Associative[Int] $h2 := $h;
is $h2.raku, 'Associative[Int]', 'Associative[Int] type object binds';

my Array[Int] $a;
my Array[Int] $a2 := $a;
is $a2.raku, 'Array[Int]', 'Array[Int] type object binds';

my Positional[Int] $y = $x;
is $y.raku, 'Positional[Int]', 'Positional[Int] type object assigns';

my Positional $bare := $x;
is $bare.raku, 'Positional[Int]', 'binds to the unparameterised role too';
