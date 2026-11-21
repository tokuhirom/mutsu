use Test;

# A feed is looser than the comma: the source of a leftward feed is the whole
# comma list on its right, not just its first item.

plan 5;

my @p;
@p <== grep { $_ } <== 3, 4;
is-deeply @p, [3, 4], 'a chained leftward feed takes the whole list';

my @r;
@r <== 5, 6,;
is-deeply @r, [5, 6], 'a trailing comma ends the list';

my @t;
@t <== map { $_ + 1 } <== grep { $_ > 1 } <== 1, 2, 3;
is-deeply @t, [3, 4], 'three feeds over a list';

my @q <== map { $_ * 2 } <== 1, 2;
is-deeply @q, [2, 4], 'a declaration fed a list';

my @u;
@u <== 7, 8 if True;
is-deeply @u, [7, 8], 'a statement modifier after the list';
