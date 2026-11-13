use Test;

# A finite `...` sequence with a built-in generator (arithmetic, geometric,
# string successor) keeps every term, however many: mutsu computes an eager
# prefix and used to cut the sequence off at 10000 terms (`1 ... 20000` had
# 10001 elements, `"a" ... "zzz"` stopped at "ntq"). The rest is generated on
# demand, so a huge sequence is still only computed as far as it is read.

plan 14;

is ("a" ... "zzz").elems, 18278, 'a string successor sequence keeps every term';
is ("a" ... "zzz").tail, 'zzz', 'and ends at its endpoint';
is (1 ... 20000).elems, 20000, 'an Int sequence keeps every term';
is (1, 3 ... 20000).tail, 19999, 'a stepped sequence stops before passing its endpoint';
is (1 ...^ 20000).elems, 19999, 'an exclusive endpoint is left out';
is (20000 ... 1).tail(2), (2, 1), 'a descending sequence keeps every term';
is (1.5 ... 30000).elems, 29999, 'a Rat sequence keeps every term';
is (1 ... * > 25000).tail, 25001, 'a closure endpoint is tested past the prefix';
is (1, 2 ... 30000, 'x', 'y').tail(3), (30000, 'x', 'y'), 'values after a list endpoint follow';
nok (1 ... 20000).is-lazy, 'the sequence is not lazy';

my @a = 1 ... 20000;
is @a.elems, 20000, 'assigning it to an array keeps every term';
is @a[19999], 20000, 'and the last term is there';

is (1 ... 10**9).head(3), (1, 2, 3), 'a huge sequence is computed only as far as it is read';
my $n = 0;
for 1 ... 10**9 { last if ++$n > 15000 }
is $n, 15001, 'a loop over a huge sequence reads past the prefix and stops';
