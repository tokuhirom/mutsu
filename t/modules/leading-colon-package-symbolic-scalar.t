use Test;

# From the Date::Names distribution (t/5-en-class.t): `$::Pkg::($name)`.
plan 4;

package Foo::en {
    our $abc = 5;
    our $def = 'x';
}

my $n = 'abc';
is $::Foo::en::($n), 5, '$::Pkg::Sub::($name) looks up an our scalar';
is $::Foo::en::('def'), 'x', 'literal key';
is $Foo::en::($n), 5, 'same as the form without the leading ::';
my $v = $::Foo::en::($n);
is $v, 5, 'usable as an initializer';
