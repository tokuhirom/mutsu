use v6;
use Test;

# A bare block nested as the sole statement of a non-`for` loop body is
# re-cloned per iteration, so its `state` restarts every time (raku: 1 1 1).
# The #5959 sole-source-block ResetStateLocals suppression used to apply to
# these bodies too, conflating them with the `{...} for @xs` modifier form —
# but `while`/`until`/C-style `loop`/`repeat` have no state-persisting
# modifier twin (raku never calls a bare `{...} while COND` block at all),
# so the suppression is simply wrong there. Expected values verified
# against raku.

plan 6;

my @a;
my $c1 = 0;
while $c1++ < 3 { { state $n = 0; $n++; @a.push($n); } }
is @a.join(','), '1,1,1', 'nested bare block in a while body restarts per iteration';

my @b;
my $c2 = 0;
until $c2++ >= 3 { { state $n = 0; $n++; @b.push($n); } }
is @b.join(','), '1,1,1', 'nested bare block in an until body restarts per iteration';

my @c;
loop (my $i = 0; $i < 3; $i++) { { state $n = 0; $n++; @c.push($n); } }
is @c.join(','), '1,1,1', 'nested bare block in a C-style loop body restarts per iteration';

my @d;
my $c3 = 0;
repeat { { state $n = 0; $n++; @d.push($n); } } while $c3++ < 2;
is @d.join(','), '1,1,1', 'nested bare block in a repeat body restarts per iteration';

# Direct (non-nested) loop-body state still accumulates: the loop statement
# clones its body once and the iterations share that clone.
my @e;
my $c4 = 0;
repeat { state $n = 0; $n++; @e.push($n); } while $c4++ < 2;
is @e.join(','), '1,2,3', 'state directly in a repeat body accumulates';

my @f;
my $c5 = 0;
until $c5++ >= 3 { state $n = 0; $n++; @f.push($n); }
is @f.join(','), '1,2,3', 'state directly in an until body accumulates';

done-testing;
