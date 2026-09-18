use Test;

plan 6;

my $values = Array[Int]([45, 47]);

is-deeply $values, [45, 47],
    'a parameterized Array type can be called as a constructor';
is $values.^name, 'Array[Int]',
    'the callable constructor preserves the parameterized type';
is $values.of.^name, 'Int',
    'the callable constructor preserves the element type';

my $type = Int;
my $dynamic = Array[$type](11, 13);

is-deeply $dynamic, [11, 13],
    'a dynamic parameterized Array type can be called as a constructor';
is $dynamic.^name, 'Array[Int]',
    'the dynamic callable constructor preserves the parameterized type';
is $dynamic.of.^name, 'Int',
    'the dynamic callable constructor preserves the element type';
