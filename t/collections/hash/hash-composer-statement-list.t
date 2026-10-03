use Test;

# A statement that starts with a hash composer and continues with a comma is a
# list, not two statements: `method hashes { {result => 1}, {result => 2} }`
# returns both hashes. From Badger's test doubles.

plan 4;

class C { method hashes { {result => 1}, {result => 2} } }
is-deeply C.hashes, ({result => 1}, {result => 2}), 'method returns both hashes';

sub f { {a => 1}, 5 }
is-deeply f(), ({a => 1}, 5), 'a hash then a scalar';

sub g { {a => 1} }
is-deeply g(), {a => 1}, 'a lone hash is still the value';

my $seen = 0;
{a => 1}.keys.map({ $seen++ }).eager;
is $seen, 1, 'a statement-leading hash with a postfix';
