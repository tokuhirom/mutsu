use Test;

# `.roll($n)` on a Range with non-integer endpoints sampled the enumeration only
# when the start was numeric; a Str range (`'a'..'z'`) fell through to "return
# the start element", so `("a".."z").roll(8).join` was always 'aaaaaaaa'.
# Found via `Digest::MD5`'s "hash 100 random strings" subtest, which hashed the
# same string a hundred times.

plan 8;

my @letters = ('a' .. 'z').roll(200);
is @letters.elems, 200, 'roll(n) on a Str range returns n elements';
ok @letters.all ~~ /^ <[a..z]> $/, '... all drawn from the range';
ok @letters.unique.elems > 10, '... and actually varied, not the start element';

my @pair = ('a' .. 'e').roll(*)[^50];
is @pair.elems, 50, 'roll(*) on a Str range is pullable';
ok @pair.unique.elems > 1, '... and varied too';

isa-ok ('a' .. 'z').roll, Str, 'the no-argument form still yields one element';

# The numeric paths keep the types they had.
ok (1 .. 26).roll(50).all ~~ Int, 'an Int range still rolls Ints';
ok (1.1 .. 3.1).roll(20).all ~~ Rat, 'a Rat range still rolls Rats';
