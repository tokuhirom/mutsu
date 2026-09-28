use Test;

plan 10;

my %h = foo => 42;
ok %h{'foo' | 'meow'}:exists, 'any Junction finds an existing key';
ok %h{one('foo', 'meow')}:exists, 'one Junction finds exactly one existing key';
nok %h{all('foo', 'meow')}:exists, 'all Junction requires every key';
nok %h{none('foo', 'meow')}:exists, 'none Junction rejects an existing key';
ok %h{all('foo', 'foo')}:exists, 'all Junction succeeds when every key exists';
nok %h{'foo' | 'meow'}:!exists, 'negation applies after Junction collapse';
is (%h{'foo' | 'meow'}:exists).^name, 'Bool', 'Junction existence returns Bool';
ok %h{'foo'}:exists, 'ordinary key existence still succeeds';

my %objects{Any};
%objects{'foo' | 'meow'} = 1;
ok %objects{'foo' | 'meow'}:exists, 'object hash also threads Junction keys';
nok %objects{'other' | 'missing'}:exists, 'object hash reports missing keys';
