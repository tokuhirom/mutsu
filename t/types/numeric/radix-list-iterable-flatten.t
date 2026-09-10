use v6;
use Test;

# :N[...] radix-list notation: the bracket body is an ordinary list
# constructor, so an embedded iterable flattens into the digit list.
# Crypt::Random::Extra's UUID builder relies on `:256[$buf.values]`.

plan 6;

my @a = 1, 2, 3;
is :256[@a], 66051, ':256[@a] uses the array ELEMENTS as digits';
is :256[1, 2, 3], 66051, 'literal digit list unchanged';

my $buf = Buf.new(0xde, 0xad);
is :256[$buf.values], 0xdead, ':256[Seq] flattens the sequence';
is :256[$buf.values].fmt("%x"), 'dead', 'flattened value formats correctly';

my @hex = 10, 11;
is :16[@hex], 171, ':16[@a] flattens too';

is :16[1, ".", 8], 1.5, 'fractional radix list still works';
