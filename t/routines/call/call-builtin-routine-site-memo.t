use Test;

# A call site of a built-in routine remembers that it reached the builtin
# (#18028). Whatever can make the same site answer differently later must
# still be seen: the remembered route is only a shortcut.

plan 8;

sub through-abs($x) { abs($x) }
sub through-max($a, $b) { max($a, $b) }

# Warm both sites, so later calls take the remembered route.
my @warm = (-5..5).map({ through-abs($_) });
is @warm.join(','), '5,4,3,2,1,0,1,2,3,4,5', 'abs over a range';
is (^20).map({ through-max($_, 7) }).join(','),
    '7,7,7,7,7,7,7,7,8,9,10,11,12,13,14,15,16,17,18,19', 'max over a range';

# (A Junction argument is left to the general path's autothreading, which
# does not thread a builtin's Junction yet: #18098.)

# Wrapping the builtin reaches a warmed site, and unwrapping restores it.
my $handle = &abs.wrap(-> |c { 'wrapped ' ~ callsame });
is through-abs(-3), 'wrapped 3', 'a wrap of the builtin is seen by a warmed site';
$handle.restore;
is through-abs(-3), 3, 'and its restore too';

# A warmed site keeps answering the builtin when other code shadows the
# name in its own scope.
{
    my sub abs($x) { "mine $x" }
    is abs(-4), 'mine -4', 'a lexical sub shadows the builtin in its scope';
}
is through-abs(-4), 4, 'but not at a site outside that scope';

# The builtin's own errors still reach the caller.
sub through-chr($x) { chr($x) }
is through-chr(65), 'A', 'chr warms';
dies-ok { through-chr(-1) }, 'a failing builtin call still dies';
