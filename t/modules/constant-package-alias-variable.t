use Test;

# A `constant` bound to a package type object is a package qualifier for its
# sigiled variables too: `$E::v` is `$A::B::v` (#11315).

plan 12;

class A::B { our $v = 3; our @a = 1, 2; our %h = x => 1 }
constant E = A::B;

is $E::v, 3, 'a scalar reads through the alias';
is @E::a.join(','), '1,2', 'an array reads through the alias';
is %E::h<x>, 1, 'a hash reads through the alias';
is $E::v.VAR.^name, 'Scalar', 'the aliased scalar is the variable\'s own container';

$E::v = 5;
is $A::B::v, 5, 'a scalar assignment writes the real variable';

module M::N { our $count = 2; our @list = 1; our %map; }
my constant Al = M::N;

$Al::count++;
is $M::N::count, 3, '++ through a lexical constant alias';
$Al::count += 10;
is $M::N::count, 13, 'a compound assignment through the alias';
@Al::list.push(3);
is @M::N::list.join(','), '1,3', 'a mutating method through the alias';
%E::h.push: (y => 2);
is %A::B::h<y>, 2, 'a mutating hash method through the alias';

is $Al::missing.raku, 'Any', 'an undeclared variable under the alias is Any';

constant N = 5;
is $N::x.raku, 'Any', 'a constant that is not a package is no alias';

{
    my constant Inner = A::B;
    is $Inner::v, 5, 'an alias declared in a block';
}
