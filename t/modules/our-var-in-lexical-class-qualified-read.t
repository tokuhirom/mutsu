use Test;

# An `our` variable of a lexical class (`my class X { our $v }`) is reachable
# through the package-qualified spelling `$X::v` / `@X::a` / `%X::h` (#16620).

plan 8;

my class X { our $v = 3; our @a = 1, 2; our %h = a => 1; };

is $X::v, 3, 'scalar read';
is-deeply @X::a, [1, 2], 'array read';
is-deeply %X::h, {a => 1}, 'hash read';

$X::v = 10;
is $X::v, 10, 'a write through the qualified name';
is X::<$v>, 10, 'and the stash sees it';

{
    my class Y { our $w = 5 }
    is $Y::w, 5, 'a class in an inner block';
}

class Z { our $u = 9 }
is $Z::u, 9, 'a package class is unchanged';
is X::.keys.sort.join(','), '$v,%h,@a', 'the stash lists each variable once';

done-testing;
