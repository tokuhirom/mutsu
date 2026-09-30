use Test;

plan 3;

constant p = 7;
my $seen;
CHECK $seen = p.is-prime;
constant q = p * 2;
CHECK $seen = $seen && q == 14;

ok $seen, 'a mainline CHECK sees the constants declared before it';

class Foo { method v { 5 } }
constant f = Foo.new;
is f.v, 5, 'a constant after a class still runs in source order';

constant e = BEGIN 5;
is e, 5, 'constant with BEGIN initializer unaffected';
