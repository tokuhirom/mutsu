use Test;

# `has $.k is built(:bind) handles <...>` holds the caller's container; the
# delegation must reach the value in it (from Array::Unsorted::Map).

plan 9;

class C { has $.k is built(:bind) handles <Numeric elems push Int Bool list>; }

my @a = 1, 2;
my $o = C.new(k => @a);

is +$o, 2, 'prefix + goes through the delegated Numeric';
is $o.elems, 2, 'elems';
is $o.Int, 2, 'Int';
ok ?$o, 'Bool';
$o.push(3);
is-deeply @a, [1, 2, 3], 'a mutating delegated method reaches the caller container';
is +$o, 3, 'prefix + sees the push';
is $o.list.join(','), '1,2,3', 'list';

class D { has $.k is built(:bind) is required handles <Numeric>; }
is +D.new(k => [1, 2, 3, 4]), 4, 'is required, literal array';

class E does Associative { has $.keys is built(:bind) handles <elems Numeric>; }
is +E.new(keys => <a b>), 2, 'a class doing Associative';

done-testing;
