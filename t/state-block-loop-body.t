use v6;
use Test;

# A block used as a loop body via the statement-modifier form is cloned once
# per loop statement, so its `state` persists across iterations; a bare block
# NESTED in a prefix loop body re-clones per iteration, so its `state`
# restarts. The modifier form used to lose every write (1 1 1) because the
# block-exit cleanup treated the state slot as a block-local `my` and the
# loop's state sync then wrote Nil through the shared cell. Expected values
# verified against raku.

plan 4;

my @a;
{ state $n = 0; $n = $n + 1; @a.push($n); } for 1..3;
is @a.join(','), '1,2,3', 'statement-modifier block body: plain = accumulates';

my @b;
{ state $n = 0; $n++; @b.push($n); } for 1..3;
is @b.join(','), '1,2,3', 'statement-modifier block body: ++ accumulates';

my @c;
for 1..3 { { state $n = 0; $n++; @c.push($n); } }
is @c.join(','), '1,1,1', 'a nested bare block re-clones per iteration';

my @d;
{ my $m = 10; state $n = 0; $n++; @d.push($n + $m); } for 1..2;
is @d.join(','), '11,12', 'state accumulates while a sibling my stays block-local';

done-testing;
