use v6;
use lib 't/lib/NamedAliasArray/lib';
use Test;

plan 4;

# From Math::NumberTheory: a scalar variable holding an Array, passed as `:$k`
# through an exported proto, binds a renamed `:offset(:@k)!` parameter.
use NamedAliasArray;

my $k = [3, 4, 9];
is-deeply np(33, :$k), (36, 37, 42), ':$k with a scalar holding an Array';
is-deeply np(33, :k($k)), (36, 37, 42), ':k($k)';
my @a = 3, 4, 9;
is-deeply np(33, :k(@a)), (36, 37, 42), 'an @ variable';
is np(33, :k(2)), 35, 'an Int still picks the scalar candidate';
