use Test;

# From List::MoreUtils (pairwise.rakutest): `@a.AT-POS($i)` as a call argument
# is the element container, so an `is rw` parameter binds it.
plan 2;

my @a = 1, 2;
sub f($x is rw) { $x++ }
f(@a.AT-POS(1));
is-deeply @a, [1, 3], 'AT-POS argument binds an is-rw parameter';

sub pw(&code, @p, @q) { code(@p.AT-POS(0), @q.AT-POS(0)) }
my @p = 1; my @q = 5;
pw(-> $m is rw, $n is rw { $m++; $n *= 2 }, @p, @q);
is-deeply (@p, @q), ([2], [10]), 'pairwise-style rw aliasing';
